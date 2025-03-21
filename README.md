# atproto Clojure SDK

Work very much in progress

Multi-platform codebase designed to work in Clojure, ClojureScript and ClojureDart.

## Usage

### ATProto Client

The workflow for utilizing the client is to:

1. Obtain a session by specifying the ATProto endpoint and (optionally) authentication credentials.
2. Use the session to make query or procedure calls to ATProto or Bluesky endpoints.

The SDK supports three types of session:
1. Unauthenticated sessions to make API calls to public atproto application endpoints like [Bluesky](https://docs.bsky.app/docs/category/http-reference).
2. Credentials sessions to use with your own username/password for CLI tools.
3. OAuth sessions to connect to your users' Personal Data Servers and make API calls on their behalf.

`query` and `procedure` calls use the "NSID" of the query or procedure, and a Clojure map of parameters.

All calls (including the call to `create`) are asynchronous, and return immediately. The return value depends on platform:

- Clojure: a Clojure promise.
- ClojureScript: a core.async channel.
- ClojureDart: a Dart Watchable.

You can also provide a `:channel`, `:callback` or `:promise` keyword option to recieve the return value. Not all options are supported on all platforms.

```clojure
(require '[atproto.client :as client])

;; Unauthenticated client to public endpoint
(def client @(client/create {:service "https://public.api.bsky.app"}))

;; Bluesky endpoints and their query params can be found here:
;; https://docs.bsky.app/docs/category/http-reference

;; Credentials-based authenticated client
(def client @(client/create {:credentials {:identifier "<me.bsky.social>"
                                           :password "SECRET"}}))

;; Issue a query with the client
@(client/query client {:op :app.bsky.actor.getProfile
                   :params {:actor "<me.bsky.social>"}})

;; => {:handle "<me.bsky.social>" ... }

;; Using core.async
(def result (async/chan))
(at/query client
          {:op :app.bsky.actor.getProfile
           :params {:actor "<me.bsky.social>"}}
          :channel result)
(async/<!! result)
```

### Jetstream

Connect to Bluesky's [Jetstream service](https://docs.bsky.app/blog/jetstream) to get real-time updates of public network data. Jetstream provides a JSON-based alternative to the binary CBOR firehose, making it easier to work with post streams, likes, follows, and other events.

```clojure
(require '[atproto.jetstream :as jetstream]
         '[clojure.core.async          :as async]
         '[examples.jetstream-analysis :as analysis]))

;; Connect with default settings (subscribes to posts)
(def conn (jetstream/connect-jetstream (async/chan 1024)))

;; Print out a single post (with 5 second timeout)
(let [event (async/alt!!
             (:events conn)    ([v] v)
             (async/timeout 5000) :timeout)]
  (clojure.pprint/pprint event))

;; Start analyzing the stream
(def analysis (analysis/start-analysis conn))

;; Get current statistics about post rates, sizes, etc
(analysis/get-summary @(:state analysis))

;; Save sample messages for offline analysis
(analysis/collect-samples conn
                        {:count    10
                         :filename "samples/my-samples.json"})

;; Cleanup
(analysis/stop-analysis analysis)
(jetstream/disconnect conn)
```

Check out the `examples.jetstream-analysis` namespace for a complete example of stream processing and analysis.

## References

- [Existing SDKs](https://atproto.com/sdks)
- [What goes in to a Bluesky or atproto SDK?](https://github.com/bluesky-social/atproto/discussions/2415)
- [atproto Interop Test Files](https://github.com/bluesky-social/atproto-interop-tests)

## Contribute

Help is very much welcomed!

Before submitting a pull request, please take a look at the [Issues](https://github.com/goshatch/atproto-clojure/issues) to see if the topic you are interested in is already being discussed, and if it is not, please create an Issue to discuss it before making a PR.

## License

MIT, see LICENSE file
