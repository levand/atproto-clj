# AT Protocol "Statusphere" Example App in Clojure

[Original example app in typescript](https://github.com/bluesky-social/statusphere-example-app/tree/main).

An example application covering:

* Signin via OAuth
* Fetch information about users (profiles)
* ~~Listen to the network firehose for new data~~ (not yet)
* Publish data on the user's account using a custom schema

To configure:

```
cp config-sample.edn config.edn
```

Update `config.edn` with the appropriate configuration values.

To start:

```clojure
clojure -M -m statusphere config.edn
```
