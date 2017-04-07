(ns hiccup-framework7.components
  (:require [clojure.string :as string])
  (:refer-clojure :exclude [list]))

(defn- class-names [m]
  (->> m
       (filter second)
       (map (comp #(cond->> %
                     (not (vector? %)) vector
                     true (remove nil?)
                     true (map (fn [x]
                                 (try (name x)
                                      (catch Exception e x))))
                     true (string/join "-"))
                  first))
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

(defn list-item [{:keys [after badge badge-color checkbox? checked?
                         disabled? divider? group-title? link
                         media media-item? name radio? subtitle
                         text title value]} & content]
  (let [badge-el (when badge
                   [:span {:class (class-names
                                    {:badge true
                                     [:bg badge-color] badge-color})}
                    badge])
        after-el (when after [:span after])
        input-el (when (or radio? checkbox?)
                   [:input {:type (cond radio? :radio checkbox? :checkbox)
                            :name name
                            :value value
                            :disabled disabled?
                            :checked checked?}])
        input-icon-el nil
        media-el (when media [:div.item-media media])
        title-el (when title [:div.item-title title])
        subtitle-el (when subtitle [:div.item-subtitle subtitle])
        text-el (when text [:div.item-text text])
        title-row-el (when media-item? [:div.item-title-row title-el])
        after-wrap-el (when (or after-el badge-el)
                        [:div.item-after after-el badge-el])
        inner-el [:div.item-inner
                  (seq (if media-item?
                         [title-row-el subtitle-el text-el]
                         [title-el after-wrap-el]))
                  content]
        item-content-el [(if (or radio? checkbox?) :label :div)
                         {:class (class-names {:item-content true
                                               [:label :radio] radio?
                                               [:label :checkbox] checkbox?})}
                         input-el media-el inner-el]
        link-el (when link [:a.item-link {:href link} item-content-el])]
    [:li {:class (class-names {:item-divider divider?
                               :list-group-title group-title?})}
     (if (or divider? group-title?)
       [:span title]
       (or link-el item-content-el))]))

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

(defn block [{:keys [inner? inset?]} & content]
  [:div {:class (class-names {:content-block true
                              :inset inset?})}
   (if inner?
     [:div.content-block-inner content]
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

(defn grid [{:keys [no-gutter?]} & content]
  [:div {:class (class-names {:row true
                              :no-gutter no-gutter?})}
   content])

(defn col [{:keys [width tablet-width]} & content]
  [:div {:class (class-names [[:col-auto (not (or width tablet-width))]
                              [[:col width] width]
                              [[:col tablet-width] tablet-width]])}
   content])
