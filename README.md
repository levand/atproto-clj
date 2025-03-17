# atproto Clojure SDK

Work very much in progress

Multi-platform codebase designed to work in Clojure, ClojureScript and
ClojureDart.

## Progress

| Feature      | Status |
| ------------ | ------ |
| http client  | 🟡     |
| identifiers  | 🔴     |
| bsky         | 🔴     |
| crypto       | 🔴     |
| mst          | 🔴     |
| lexicon      | 🔴     |
| identity     | 🔴     |
| streaming    | 🟡     |
| service auth | 🔴     |
| plc          | 🔴     |
| oauth server | 🔴     |

## Usage

### ATProto client

The workflow for utilizing the client is to:

1. Obtain a session by specifying the ATProto endpoint and (optionally) authentication credentials.
2. Use the session to make `query` or `procedure` calls to [ATProto](https://atproto.com/specs/xrpc#lexicon-http-endpoints) or [Bluesky](https://docs.bsky.app/docs/category/http-reference) endpoints.

A session is a thread-safe, stateful object containing the information required to make ATProto HTTP requests.

`query` and `procedure` calls use the "NSID" of the query or procedure, and a Clojure map of parameters/input.

All calls (including the call to `init`) are asynchronous, and return immediately. The return value depends on platform:

- Clojure: a Clojure promise.
- ClojureScript: a core.async channel.
- ClojureDart: a Dart Watchable.

You can also provide a `:channel`, `:callback` or `:promise` keyword option to recieve the return value. Not all options are supported on all platforms.


```clojure
(require '[atproto.client :as at])

;; Unauthenticated client to default endpoint
(def session @(at/init))

;; Unauthenticated client to a particular server
(def session @(at/init :endpoint "https://public.api.bsky.app"))

;; Password-based authenticated client
;; Defaults to looking up and using the identifier's PD server
(def session @(at/init :identifier "me.bsky.social"
                      :password "SECRET"))

;; Bluesky endpoints and their query params can be found here:
;; https://docs.bsky.app/docs/category/http-reference

@(at/query session :app.bsky.actor.getProfile {:actor "gosha.net"})
;; => {:handle "gosha.net",
;;     :displayName "Gosha ⚡",
;;     :did "did:plc:ypjjs7u7owjb7xmueb2iw37u",
;;     ......}

;; Using core.async
(def result (async/chan))
(at/query session :app.bsky.actor.getProfile {:actor "gosha.net"} :channel result)
(async/<!! result)
```

### Jetstream

Connect to Bluesky's [Jetstream service](https://docs.bsky.app/blog/jetstream) to get real-time updates of public network data. Jetstream provides a JSON-based alternative to the binary CBOR firehose, making it easier to work with post streams, likes, follows, and other events.

The Jetstream implementation is currently only supported for JVM Clojure.

```clojure
(require '[atproto.jetstream :as jet])
(require '[clojure.core.async :as a]))

;; Define a channel to recieve events
(def events-ch (a/chan))

;; Subscribe to the jetstream
(def control-ch (jet/consume events-ch :wanted-collections ["app.bsky.feed.post"]))

;; Consume events
(a/go-loop [count 0]
  (if-let [event (a/<! events-ch)]
    (do
      (when (zero? (rem count 100)) (println (format "Got %s posts" count)))
      (recur (inc count)))
    (println "event channel closed")))

;; Stop processing
(a/close! control-ch)
```

## References

- [Existing SDKs](https://atproto.com/sdks)
- [What goes in to a Bluesky or atproto SDK?](https://github.com/bluesky-social/atproto/discussions/2415)
- [atproto Interop Test Files](https://github.com/bluesky-social/atproto-interop-tests)

## Contribute

Help is very much welcomed!

Before submitting a pull request, please take a look at the [Issues](https://github.com/goshatch/atproto-clojure/issues) to see if the topic you are interested in is already being discussed, and if it is not, please create an Issue to discuss it before making a PR.

## License

MIT, see LICENSE file
