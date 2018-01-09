(ns status-im.ui.components.list.views
  "
  Wrapper for react-native list components.

  (defn render [{:keys [title subtitle]}]
    [item
     [item-icon {:icon :dots_vertical_white}]
     [item-content 
      [item-primary title] 
      [item-secondary subtitle]]
     [item-icon {:icon :arrow_right_gray}]])

  [flat-list {:data [{:title  \"\" :subtitle \"\"}] :render-fn render}]

  [section-list {:sections [{:title :key :unik :data {:title  \"\" :subtitle \"\"}}] :render-fn render}]

  or with a per-section `render-fn`

  [section-list {:sections [{:title \"\" :key :unik :render-fn render :data {:title  \"\" :subtitle \"\"}}]}]
  "
  (:require [reagent.core :as reagent]
            [reagent.impl.template :as temp]
            [status-im.ui.components.icons.vector-icons :as vector-icons]
            [status-im.ui.components.checkbox.view :as checkbox]
            [status-im.ui.components.list.styles :as styles]
            [status-im.ui.components.react :as react]
            [status-im.utils.platform :as platform]))

(def flat-list-class (react/get-class "FlatList"))
(def section-list-class (react/get-class "SectionList"))

(defn item
  ([content] (item nil content))
  ([left-action content] (item left-action content nil))
  ([left-action content right-action]
   [react/view {:style styles/item}
    [react/view {:style styles/left-item-wrapper}
     left-action]
    [react/view {:style styles/content-item-wrapper}
     content]
    (when right-action
      [react/view {:style styles/right-item-wrapper}
       right-action])]))

(defn touchable-item [handler item]
  [react/touchable-highlight {:on-press handler}
   item])

(defn item-icon
  [{:keys [icon style icon-opts]}]
  {:pre [(not (nil? icon))]}
  [react/view {:style style}
   [vector-icons/icon icon (merge icon-opts {:style styles/item-icon})]])

(defn item-image
  [{:keys[source style image-style]}]
  [react/view {:style style}
   [react/image {:source source
                 :style  (merge styles/item-image image-style)}]])

(defn item-primary
  [primary]
  [react/text {:style styles/primary-text} primary])

(defn item-primary-only
  [primary]
  [react/text {:style styles/primary-text-only} primary])

(defn item-secondary
  [secondary]
  [react/text {:style styles/secondary-text :ellipsize-mode :middle :number-of-lines 1} secondary])

(defn item-content
  [& children]
  (into [react/view {:style styles/item-content-view}] (keep identity children)))

(defn item-checkbox
  [{:keys [style] :as props}]
  [react/view {:style (merge style styles/item-checkbox)}
   [checkbox/checkbox props]])

(defn- wrap-render-fn [f]
  (fn [data]
    (reagent/as-element (f (.-item data) (.-index data) (.-separators data)))))

(def default-separator [react/view styles/separator])

(def default-header [react/view styles/list-header-footer-spacing])

(def default-footer [react/view styles/list-header-footer-spacing])

(def section-separator [react/view styles/section-separator])

(defn- base-list-props
  [{:keys [render-fn empty-component header separator default-separator?]}]
  (let [separator (or separator (when (and platform/ios? default-separator?) default-separator))]
    (merge {:keyExtractor (fn [_ i] i)}
           (when render-fn               {:renderItem (wrap-render-fn render-fn)})
           (when separator               {:ItemSeparatorComponent (fn [] (reagent/as-element [separator]))})
           (when empty-component         {:ListEmptyComponent (fn [] (reagent/as-element empty-component))})
           (when header                  {:ListHeaderComponent (fn [] (reagent/as-element header))}))))

;; Workaround an issue in reagent that does not consider JS array as JS value
;; This forces clj <-> js serialization and breaks clj semantic
;; See https://github.com/reagent-project/reagent/issues/335

(deftype Item [value]
  IEncodeJS
  (-clj->js [x] (.-value x))
  (-key->js [x] (.-value x))
  IEncodeClojure
  (-js->clj [x options] (.-value x)))

(defn- to-js-array
  "Converts a collection to a JS array (but leave content as is)"
  [coll]
  (let [arr (array)]
    (doseq [x coll]
      (.push arr x))
    arr))

(defn- wrap-data [o]
  (Item. (to-js-array o)))

(defn flat-list
  "A wrapper for FlatList.
   See https://facebook.github.io/react-native/docs/flatlist.html"
  [{:keys [data empty-component] :as props}]
  {:pre [(or (nil? data)
             (sequential? data))]}
  [flat-list-class
   (merge (base-list-props props)
          props
          {:data (wrap-data data)})])

(defn- wrap-render-section-header-fn [f]
  (fn [data]
    (reagent/as-element (f (.-section data)))))

(defn- default-render-section-header [{:keys [title data]}]
  (when (seq data)
    [react/text {:style styles/section-header}
     title]))

(defn- wrap-per-section-render-fn [props]
  (update
    (if-let [f (:render-fn props)]
      (assoc (dissoc props :render-fn) :renderItem (wrap-render-fn f))
      props)
    :data wrap-data))

(defn section-list
  "A wrapper for SectionList.
   See https://facebook.github.io/react-native/docs/sectionlist.html"
  [{:keys [sections empty-component render-section-header-fn] :as props
    :or {render-section-header-fn default-render-section-header}}]
  (if (and (every? #(empty? (:data %)) sections) empty-component)
    empty-component
    [section-list-class
     (merge (base-list-props props)
            props
            {:sections            (clj->js (map wrap-per-section-render-fn sections))
             :renderSectionHeader (wrap-render-section-header-fn render-section-header-fn)}
            (when platform/ios? {:SectionSeparatorComponent (fn [] (reagent/as-element [section-separator]))}))]))

