(ns atproto.did-test
  (:require #?(:cljs [cljs.test :refer :all]
               :clj  [clojure.test :refer :all])
            [atproto.did :as did]))

(deftest valid-atproto-did-test
  (are [did] (did/valid? did)
    "did:plc:l3rouwludahu3ui3bt66mfvj"
    "did:plc:aaaaaaaaaaaaaaaaaaaaaaaa"

    "did:web:example.com"
    "did:web:sub.example.com"
    "did:web:localhost%3A8080"              ;; port allowed for localhost
    ))

(deftest invalid-atproto-did-test
  (are [did] (not (did/valid? did))
    nil
    ""
    "random-string"
    "did:"
    "did:foo"
    "did:foo:"
    "did:foo:bar"
    "did:as4ff:asd"
    "did:foo:&@"

    "did:plc:foo"                          ; too short
    "did:plc:toolongtoolongtoolongto"      ; too short
    "did:plc:toolongtoolongtoolongtool"    ; too long
    "did:plc:l3rouwludahu3ui3bt66mfv1"     ; non base-32 char
    "did:plc:l3rouwludahu3ui3bt66mfv8"     ; non base-32 char
    "did:plc:l3rouwludahu3ui3bt66mfvA"     ; non base-32 char
    "did:plc:l3rouwludahu3ui3bt66mfvZ"     ; non base-32 char

    "did:web:foo@example.com"              ; unallowed character
    "did:web::example.com"                 ; cannot start with colon
    "did:web:example.com:"                 ; cannot end with colon
    "did:web:example.com:path:to:resource" ; no path allowed
    "did:web:example.com%3A8080"           ; no port allowed outside localhost
    ))

(deftest web-did-url-mapping-test
  (let [web-did->url {"did:web:example.com"      "https://example.com/"
                      "did:web:sub.example.com"  "https://sub.example.com/"
                      "did:web:localhost%3A8080" "http://localhost:8080/"}]
    (doseq [[web-did url] web-did->url]
      (is (= url (did/web-did->url web-did)) "The URL is correctly generated from the Web DID.")
      (is (= web-did (did/url->web-did url)) "The Web DID is correctly generated from the URL."))))
