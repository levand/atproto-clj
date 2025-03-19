(ns statusphere.view
  (:require [hiccup2.core :as h])
  (:import [java.time LocalDateTime LocalDate ZoneId]
           [java.time.format DateTimeFormatter]))

(def status-options
  ["👍",
   "👎",
   "💙",
   "🥹",
   "😧",
   "😤",
   "🙃",
   "😉",
   "😎",
   "🤓",
   "🤨",
   "🥳",
   "😭",
   "😤",
   "🤯",
   "🫡",
   "💀",
   "✊",
   "🤘",
   "👀",
   "🧠",
   "👩‍💻",
   "🧑‍💻",
   "🥷",
   "🧌",
   "🦋",
   "🚀"])

;; helpers

(defn- status-instant
  [{:keys [created-at indexed-at]}]
  (first (sort [created-at indexed-at])))

(defn- format-instant
  [instant]
  (.format (LocalDateTime/ofInstant instant (ZoneId/systemDefault))
           (DateTimeFormatter/ofPattern "EEE MMM d uuuu")))

(defn- today?
  [instant]
  (= (LocalDate/now)
     (LocalDate/ofInstant instant (ZoneId/systemDefault))))

(defn- bsky-link
  [handle]
  (str "https://bsky.app/profile/" handle))

(defn- shell
  [{:keys [error title content]}]
  (str
   (h/html
    [:html
     [:head
      [:title title]
      [:link {:rel "stylesheet" :href "/style.css"}]]
     [:body
      (when error
        [:div.error.visible
         error])
      content]])))

;; Views

(defn login
  [{:keys [error]}]
  (shell
   {:error error
    :title "Log in"
    :content [:div#root
              [:div#header
               [:h1 "Statusphere"]
               [:p "Set your status on the Atmosphere."]]
              [:div.container
               [:form {:action "/login" :method "post" :class "login-form"}
                [:input {:type "text"
                         :name "handle"
                         :placeholder "Enter your handle (eg alice.bsky.social)"
                         :required "required"}]
                [:button {:type "submit"} "Log in"]]
               [:div {:class "signup-cta"}
                "Don't have an account on the Atmosphere?"
                [:a {:href "https://bsky.app"}
                 "Sign up for Bluesky"]
                " to create one now!"]]]}))

(defn home
  [{:keys [error statuses did-handle-map profile my-status] :as props}]
  (shell
   {:error error
    :title "Home"
    :content [:div#root
              [:div#header
               [:h1 "Statusphere"]
               [:p "Set your status on the Atmosphere."]]
              [:div.container
               [:div.card
                (if profile
                  [:form {:action "/logout" :method "post" :class "session-form"}
                   [:div
                    "Hi, " [:strong (get profile :displayName "friend")] ". What's your status today?"]
                   [:div
                    [:button {:type "submit"} "Log out"]]]
                  [:div.session-form
                   [:div [:a {:href "/login"} "Log in"] " to set your status!"]
                   [:div [:a {:href "/login" :class "button"} "Log in"]]])]
               [:form {:action "/status" :method "post" :class "status-options"}
                (for [option status-options]
                  [:button {:class (str "status-option" (when (= option (:status my-status))
                                                          " selected"))
                            :name "status"
                            :value option}
                   option])]
               (for [i (range (count statuses))]
                 (let [status (nth statuses i)
                       handle (get did-handle-map (:author-did status))
                       instant (status-instant status)]
                   [:div {:class (str "status-line" (when (zero? i) " no-line"))}
                    [:div
                     [:div {:class "status"} (:status status)]]
                    [:div {:class "desc"}
                     [:a {:class "author"
                          :href (bsky-link handle)}
                      (str "@" handle)]
                     (if (today? instant)
                       (str " is feeling " (:status status) " today.")
                       (str " was feeling " (:status status) " on " (format-instant instant)))]]))]]}))

(defn not-found
  []
  (shell
   {:title "Page not found"
    :content [:h1 "Page not found"]}))
