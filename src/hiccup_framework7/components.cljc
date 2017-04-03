(ns hiccup-framework7.components
  (:require [clojure.string :as string]))

(defn- class-names [m]
  (->> m
       (filter val)
       (map (comp #(if (vector? %)
                     (->> %
                          (remove nil?)
                          (map name)
                          (string/join "-"))
                     (name %))
                  key))
       (string/join " ")))

(defn- select-icon-props [props]
  (->> props
       (filter (comp second (partial re-find #"^icon") name key))
       (map (fn [x]
              (update x 0 (comp keyword last #(string/split % #"-") name))))
       (into {})))

(def ^:private merge-attrs
  (partial merge-with
           (comp (partial string/join " ")
                 vector)))

(defn view [_ & content]
  [:div.view content])

(defn icon [{:keys [f7 icon material fa ion color]}]
  [:i {:class (class-names {:f7-icons f7
                            :fa fa
                            :icon true
                            :material-icons material
                            [:color color] color
                            [:fa fa] fa
                            [:ion ion] ion
                            icon icon})}
   (or f7 material)])

(defn link [{:keys [close-popup back? external? href open-popup text color attrs]
             :as props} & content]
  (let [icon-props (not-empty (select-icon-props props))]
    [:a (merge-attrs {:href (or href "#")
                      :data-popup (or close-popup open-popup)
                      :class (class-names
                               {:link true
                                :icon-only (and icon-props (not text) (empty? content))
                                :external external?
                                :back back?
                                [:color color] color
                                :close-popup close-popup
                                :open-popup open-popup})}
                     attrs)
     (when icon-props (icon icon-props))
     (when text [:span text])
     content]))

(defn list [{:keys [form media-list?]} & content]
  [(if form :form :div)
   {:class (class-names {:list-block true
                         :media-list media-list?})}
   [:ul content]])

(defn list-group [& content]
  [:ul.list-group content])

(defn list-item [{:keys [after link media subtitle text title media-item?
                         badge group-title? radio? checkbox? name value checked?
                         disabled?]} & content]
  [:li (if group-title?
         [:div.list-group-title title]
         (conj (when link [:a.item-link {:href link}])
               [(cond radio? :label.label-radio
                      checkbox? :label.label-checkbox
                      :else :div)
                {:class "item-content"}
                (when (or radio? checkbox?)
                  [:input {:type (cond radio? :radio
                                       checkbox? :checkbox)
                           :name name
                           :value value
                           :disabled disabled?
                           :checked checked?}])
                (when media [:div.item-media media])
                [:div.item-inner

                 (conj (when media-item? [:div.item-title-row])
                       (clojure.core/list
                         (when title [:div.item-title title])

                         (when (or after badge)
                           [:div.item-after
                            (if badge [:span.badge badge] after)])))
                 (when subtitle [:div.item-subtitle subtitle])
                 (when text [:div.item-text text])
                 content]]))])

(defn list-label [& content]
  [:div.list-block-label content])

(defn label [_ & content]
  [:div.item-title.label content])

(defn input [{:keys [type] :as props} & content]
  [:div.item-input
   (conj (case type
           :range [:div.range-slider]
           nil)
         [(if (#{:select :textarea} type) type :input) props content])])

(defn block [{:keys [inner?]} & content]
  [:div.content-block
   (conj (when inner? [:div.content-block-inner])
         content)])

(defn block-title [& content]
  [:div.content-block-title content])

(defn nav-left [& content]
  [:div.left content])

(defn nav-center [& content]
  [:div.center content])

(defn nav-right [& content]
  [:div.right content])

(defn navbar [{:keys [title back-link back-link-url]} & content]
  [:div.navbar
   [:div.navbar-inner
    (when back-link
      (nav-left (link {:back? true
                       :href back-link-url
                       :icon :icon-back
                       :text back-link})))
    (when title (nav-center title))
    content]])

(defn popup [{:keys [id]} & content]
  [:div.popup {:id id} content])

(defn page [_ & content]
  [:div.page
   [:div.page-content
    content]])
