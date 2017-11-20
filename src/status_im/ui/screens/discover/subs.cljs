(ns status-im.ui.screens.discover.subs
  (:require [re-frame.core :refer [reg-sub]]
            [status-im.utils.datetime :as time]))

(defn- calculate-priority [chats current-public-key contacts
                           {:keys [whisper-id created-at]}]
  (let [contact               (get contacts whisper-id)
        chat                  (get chats whisper-id)
        seen-online-recently? (< (- (time/now-ms) (get contact :last-online))
                                 time/hour)
        me?                   (= current-public-key whisper-id)]
    (+ created-at                                         ; message is newer => priority is higher
       (if (or me? contact) time/day 0)                   ; user exists in contact list => increase priority
       (if (or me? chat) time/day 0)                      ; chat with this user exists => increase priority
       (if (or me? seen-online-recently?) time/hour 0)))) ; the user was online recently => increase priority


(defn- get-discoveries-by-tags [discoveries search-tags]
  ;;todo check
  (filter #(some (:tags %) search-tags)
          (vals discoveries)))

(reg-sub :discover/discoveries :discoveries)

(reg-sub :discover/search-tags :discover-search-tags)

(reg-sub :discover/tags
  :<- [:discover/discoveries]
  (fn [discoveries]
    (reduce (fn [acc {:keys [tags]}]
              (into acc tags))
            #{}
            (vals discoveries))))

(reg-sub :discover/top-discovery-per-tag
  :<- [:discover/discoveries]
  :<- [:discover/tags]
  (fn [[discoveries tags] [_ limit]]
    (for [tag tags]
      (let [results (get-discoveries-by-tags discoveries #{tag})]
        [tag {:discovery (first results)
              :total     (count results)}]))))

;; TODO(yenda) this is not really the most recent discoveries
;; it's just all off them
(reg-sub :discover/recent-discoveries
  :<- [:discover/discoveries]
  (fn [discoveries]
    (sort-by :created-at > (vals discoveries))))

;; TODO(yenda) this is not really the most popular tags
;; it's just the first ones that are in the tags set
(reg-sub :discover/popular-tags
  :<- [:discover/tags]
  (fn [tags [_ limit]]
    (take limit tags)))

(reg-sub :discover/search-results
  :<- [:discover/discoveries]
  :<- [:discover/search-tags]
  :<- [:chats]
  :<- [:get-contacts]
  :<- [:get :current-public-key]
  (fn [[discoveries search-tags chats contacts current-public-key] [_ limit]]
    (let [discoveries (->> (get-discoveries-by-tags discoveries search-tags)
                           (map #(assoc % :priority (calculate-priority chats current-public-key contacts %)))
                           (sort-by :priority >))]
      {:discoveries (take limit discoveries)
       :tags        search-tags
       :total       (count discoveries)})))

(reg-sub :discover/all-dapps
  (fn [db]
    (let [dapp? (->> (get-in db [:group/contact-groups "dapps" :contacts])
                     (map :identity)
                     set)]
      (->> (:contacts/contacts db)
           (filter #(-> % key dapp?))
           (into {})))))

