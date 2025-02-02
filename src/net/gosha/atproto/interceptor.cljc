(ns net.gosha.atproto.interceptor
  "Simplified, callback-based implementation of the interceptor pattern.

  Interceptors are maps with ::enter and/or ::leave keys, with _interceptor
  functions_ as values. An interceptor function takes a context as its
  argument, and either (a) returns an updated context, or (b) calls
  `continue` with an updated context (possibly asynchronously.) If it calls
  continue, it should return nil to avoid double-processing the context.

  Execution flow is as described in the Pedestal documentation
  (http://pedestal.io/pedestal/0.6/reference/interceptors.html):

  ```
  Logically speaking, interceptors form a queue. During the :enter phase,
  the next interceptor is popped off the queue, pushed onto the leave stack,
  and it’s :enter function, if any, is executed.

  Once the handler (or other interceptor) adds a ::response to the context,
  the chain logic switches to ::leave mode: it pops interceptors off the leave
  stack and invokes the :leave function, if any.

  Because it’s the leave stack the :leave functions are invoked in the opposite
  order from the :enter functions.

  Both the queue and the stack reside in the context map. Since interceptors can
  modify the context map, that means they can change the plan of execution for
  the rest of the request! Interceptors are allowed to enqueue more interceptors
  to be called, or they can terminate the request.

  This process, of running all the interceptor ::enter functions,
  then running the interceptor ::leave functions, is called executing the
  interceptor chain.
  ```

  Context maps may have the following keys:

  - ::queue - Interceptor queue
  - ::stack - Interceptor stack
  - ::request - The request object
  - ::response - The response object (present only for `leave` phase.)

  Errors are represented as response objects with an `:error` key indicating
  the error type, and a `:message` key with a human-readable error message.")

(declare continue)

(defn- try-invoke
  "Helper function to invoke the given phase on an interceptor with appropriate
   error handling."
  [i phase ctx]
  (try
    (let [f (get i phase)]
      (if (not f)
        ctx
        (let [ret (f ctx)]
          (if (and ret (not (and (map? ret) (contains? ret ::request))))
            {::request ::missing
             ::response {:error "Invalid Interceptor Context"
                         :message (str "Phase " phase " of " (::name i)
                                    " returned a non-context value.")
                         :return-value ret}}
            ret))))
    #?(:clj (catch Throwable t
              (assoc ctx ::response
                {:error (.getName (.getClass t))
                 :message (.getMessage t)
                 :exception t
                 :phase phase
                 :interceptor (::name i)})))))

(defn- leave
  "Execute the leave phase of an interceptor context."
  [{:keys [::queue ::stack] :as ctx}]
  (if (empty? stack)
    ctx
    (let [current (first stack)
          ctx' (assoc ctx ::stack (rest stack))
          ctx'' (try-invoke current ::leave ctx')]
      (when ctx'' (continue ctx'')))))

(defn- enter
  "Execute the enter phase of an interceptor context."
  [{:keys [::queue ::stack] :as ctx}]
  (if (empty? queue)
    (leave ctx)
    (let [current (first queue)
          ctx' (assoc ctx ::stack (cons current stack) ::queue (rest queue))
          ctx'' (try-invoke current ::enter ctx')]
      (when ctx'' (continue ctx'')))))

(defn- first-index-of
  [seq pred]
  (first (keep-indexed (fn [i v]
                         (when (pred v) i)) seq)))

(defn insert-after
  "Given a context, insert the given interceptor into the queue immediately
  before the interceptor with the provided name. Throws an exception if no such
  interceptor exists"
  [ctx interceptor name]
  (let [[before after] (split-at (inc (first-index-of (::stack ctx)
                                        #(= name (::name %))))
                         (::stack ctx))]
    (assoc ctx ::stack (concat before [interceptor] after))))

(defn continue
  "Continue processing the interceptor chain of the provided context.

  This function may be called rather than returning an updated context from an
  interceptor."
  [{:keys [::response] :as ctx}]
  (if response (leave ctx) (enter ctx)))

(defn execute
  "Execute an interceptor chain starting with the context, passing the
  final context to the provided callback."
  [ctx callback]
  (let [final {::name ::execute ::leave callback}
        ctx (update ctx ::queue #(cons final %))]
    (enter ctx)))
