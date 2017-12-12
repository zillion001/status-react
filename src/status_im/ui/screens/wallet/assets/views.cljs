(ns status-im.ui.screens.wallet.assets.views
  (:require-macros [status-im.utils.views :refer [defview letsubs]])
  (:require [status-im.ui.components.react :as react]
            [status-im.ui.components.status-bar :as status-bar]
            [status-im.ui.components.toolbar.view :as toolbar]
            [status-im.ui.components.button.view :as button]
            [status-im.i18n :as i18n]
            [status-im.ui.components.styles :as component.styles]
            [status-im.ui.components.button.styles :as button.styles]
            [status-im.ui.screens.wallet.components.views :as components]
            [status-im.ui.screens.wallet.assets.styles :as assets.styles]
            [status-im.ui.screens.wallet.main.styles :as main.styles]
            [status-im.ui.components.tabs.views :as tabs]
            [status-im.ui.components.styles :as components.styles]
            [status-im.ui.screens.wallet.transactions.views :as transactions]
            [status-im.utils.utils :as utils]
            [status-im.ui.components.chat-icon.styles :as chat-icon.styles]
            [status-im.utils.money :as money]
            [status-im.ui.components.icons.vector-icons :as vector-icons]
            [clojure.string :as string]
            [status-im.utils.datetime :as datetime]))

(defn balance-display [style
                       main-value-amount
                       main-value-currency
                       change-description change]
  [react/view style
   [react/view {:style assets.styles/main-value}
    [react/text {:style assets.styles/main-value-amount} main-value-amount]
    [react/text {:style assets.styles/main-value-currency} main-value-currency]]
   [react/view {:style assets.styles/change}
    [react/text {:style assets.styles/change-description}
     change-description]
    [components/change-display change]]])


(defview my-token-tab-title [active?]
  (letsubs [ {:keys [symbol]} [:token-balance]]
    [react/text {:uppercase? true
                 :style      (assets.styles/tab-title active?)}
     (i18n/label :t/wallet-my-token {:symbol symbol})]))

(defn- format-amount [amount decimals]
  (money/to-fixed (money/token->unit (or amount (money/bignumber 0)) decimals)))

(defview my-token-tab-content []
  (letsubs [syncing? [:syncing?]
            {:keys [symbol amount decimals usd-value]} [:token-balance]]
    [react/view components.styles/flex
     [react/view {:style assets.styles/total-balance-container}
      [balance-display {}
       (format-amount amount decimals) symbol (str usd-value " " (i18n/label :t/usd-currency)) 0.05]
      [react/view {:style (merge button.styles/buttons-container main.styles/buttons)}
       [button/button {:disabled?  syncing?
                       :on-press   #(utils/show-popup "TODO" "Not implemented yet!")
                       :style      (assets.styles/button-bar :first)
                       :text-style assets.styles/main-button-text}
        (i18n/label :t/wallet-send-token {:symbol symbol})]
       [button/button {:disabled?  true
                       :on-press   #(utils/show-popup "TODO" "Not implemented yet!")
                       :style      (assets.styles/button-bar :last)
                       :text-style assets.styles/main-button-text}
        (i18n/label :t/wallet-exchange)]]]
     [react/view
      [react/text {:style assets.styles/transactions-title} (i18n/label :t/transactions)]
      [react/view {:flex-direction :row}
        [transactions/history-list]]]]))

(defn market-value-tab-title [active?]
  [react/text {:uppercase? true
               :style      (assets.styles/tab-title active?)}
   (i18n/label :t/wallet-market-value)])

(defn invert-y [height data]
  (map #(- height %) data))

(defn normalize-values [height data]
  (let [highest (reduce max data)]
    (map #(/ (* % height) highest) data)))

(defn add-x-values [data]
  (map vector (iterate #(+ % 10) 0) data))

(defn add-closing-point [height data]
  (let [last-x (-> data last first)]
   (concat data [[last-x height]])))

(defn serialize-points [data]
  (string/join " " (map #(string/join "," %) data)))

(defn line-chart [{:keys [width height data x-labels y-labels]}]
  (let [prepared (->> data
                      (normalize-values height)
                      (invert-y height)
                      add-x-values)]
    [vector-icons/svg {:width  (+ width  50)
                       :height (+ height 30)}
     [vector-icons/defs
      [vector-icons/linear-gradient {:id "line-chart-gradient" :x1 0 :y1 0 :x2 0 :y2 height}
       [vector-icons/stop {:offset "0%" :stop-color "#8397dd"}]
       [vector-icons/stop {:offset "100%" :stop-color "#eaeefb"}]]]
     [vector-icons/polyline {:width        width
                             :height       height
                             :fill         :none
                             :stroke       "#4360df"
                             :stroke-width 3
                             :points       (serialize-points prepared)}]
     [vector-icons/polygon {:width  width
                            :height height
                            :fill   "url(#line-chart-gradient)"
                            :stroke :none
                            :points (->> prepared
                                         (add-closing-point height)
                                         serialize-points)}]
     (map-indexed (fn [index x-axis-legend-item]
                    ^{:key index}
                    [vector-icons/text {:x           (* index (/ (- width 10) (dec (count x-labels))))
                                        :y           (+ height 10)
                                        :font-size   12
                                        :fill        "#8a95bc"
                                        :text-anchor (if (zero? index) :start :middle)} x-axis-legend-item])
                  x-labels)
     (map-indexed (fn [index y-axis-legend-item]
                    ^{:key index}
                    [vector-icons/text {:x                  (+ width 10)
                                        :y                  (- height (* index (/ height (dec (count y-labels)))))
                                        :font-size          12
                                        :fill               "#8a95bc"
                                        :alignment-baseline :middle} y-axis-legend-item])
                  y-labels)]))

(defn create-y-labels [num-of-labels data]
  (let [lowest  (reduce min data)
        highest (reduce max data)
        step    (/ (- highest lowest) num-of-labels)]
    (map #(if (zero? %) "" (str %))
         (range lowest (+ step highest) step))))

(defn create-x-labels []
  (map #(datetime/format-date "MMM dd" %) (datetime/last-n-days 30 6)))

(defn double-row [title1 content1
                  title2 content2]
  [react/view {:flex-direction :row}
   [components/section :half title1 content1]
   [components/section :half title2 content2]])

(defn wide-row [title content]
  [react/view {:flex-direction :row}
   [components/section :full title content]])

(defn info-sheet []
  [react/view
   [double-row "Market cap" "230,019,822 USD"
               "Volume 24h" "3,567,910 USD"]
   [double-row "Circulating Supply" "1,104,590 STT"
               "Total Supply"       "10,000,000 STT"]
   [wide-row "Concept" "Status is a free (libre) and open source mobile client targeting Android & iOS built, entirely on Ethereum technologies."]
   [double-row "Crowdsale opening date" "20. Jun 2017" nil nil]
   [double-row "Crowdsale closing date" "22. Jun 2017" nil nil]])

(defview market-value-tab-content []
  (letsubs [{:keys [usd-value]} [:token-balance]]
    (let [data [0 40 32 50 120 20 35 17 48 60
                90 80 71 35 20 60 150 63 85 36
                0 40 32 50 120 20 35 17 48 60]]
      [react/view {:flex             1
                   :padding-top      20
                   :background-color "#e8ecf8"}
       [react/scroll-view {}
        [balance-display {:padding-left 16} usd-value (i18n/label :t/usd-currency) (i18n/label :t/value) 0.05]
        [line-chart {:width    300
                     :height   100
                     :x-labels (create-x-labels)
                     :y-labels (create-y-labels 5 data)
                     :data     data}]
        [info-sheet]]])))

(def tabs-list
  [{:view-id :wallet-my-token
    :content my-token-tab-title
    :screen  my-token-tab-content}
   {:view-id :wallet-market-value
    :content market-value-tab-title
    :screen  market-value-tab-content}])

(defn token-toolbar [name symbol icon]
  [react/view assets.styles/token-toolbar
   [react/image (assoc icon :style (chat-icon.styles/image-style 64))]
   [react/text {:style assets.styles/token-name-title}
    name]
   [react/text {:style assets.styles/token-symbol-title}
    symbol]])

(defview my-token-main []
  (letsubs [current-tab                [:get :view-id]
            {:keys [symbol name icon]} [:token-balance]]
    [react/view {:style component.styles/flex}
     [status-bar/status-bar]
     [toolbar/toolbar {:style assets.styles/token-toolbar-container}
      toolbar/default-nav-back
      [token-toolbar name symbol icon]]
     [tabs/swipable-tabs tabs-list current-tab true
      {:navigation-event     :navigation-replace
       :tab-style            assets.styles/tab
       :tabs-container-style assets.styles/tabs-container}]]))