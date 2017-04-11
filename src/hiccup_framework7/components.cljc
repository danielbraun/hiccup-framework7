(ns hiccup-framework7.components
  (:require [clojure.string :as string])
  (:refer-clojure :exclude [list]))

(defn- wrap-component [f]
  (fn [& args]
    (if (map? (first args))
      (f (first args) (rest args))
      (f {} args))))

(defmacro defcomponent
  [name & fdecl]
  `(do (defn ~name ~@fdecl)
      (alter-var-root (var ~name) wrap-component)))

;TODO Add doc-strings to all components so that vim-fireplace shows their previews

(defn- classes [m]
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

(defcomponent view [_ content]
  [:div.view content])

(defcomponent icon [{:keys [f7 icon material fa ion color]} _]
  [:i {:class (classes {:f7-icons f7
                        :fa fa
                        :icon true
                        :material-icons material
                        [:color color] color
                        [:fa fa] fa
                        [:ion ion] ion
                        icon icon})}
   (or f7 material)])

(defcomponent link [{:keys [close-popup back? external? href open-popup text color attrs]
             :as props} content]
  (let [icon-props (not-empty (select-icon-props props))]
    [:a (merge-attrs {:href (or href "#")
                      :data-popup (or close-popup open-popup)
                      :class (classes
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

(defcomponent list [{:keys [form? media-list? attrs]} content]
  [(if form? :form :div)
   (merge-attrs {:class (classes {:list-block true
                                  :media-list media-list?})}
                attrs)
   [:ul content]])

(defcomponent list-group [_ content]
  [:ul.list-group content])

(defcomponent list-item [{:keys [after badge badge-color checkbox? checked?
                         disabled? divider? group-title? link
                         media media-item? name radio? subtitle
                         text title value]} content]
  (let [badge-el (when badge
                   [:span {:class (classes
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
                         {:class (classes {:item-content true
                                           [:label :radio] radio?
                                           [:label :checkbox] checkbox?})}
                         input-el media-el inner-el]
        link-el (when link [:a.item-link {:href link} item-content-el])]
    [:li {:class (classes {:item-divider divider?
                           :list-group-title group-title?})}
     (if (or divider? group-title?)
       [:span title]
       (or link-el item-content-el))]))

(defcomponent list-label [_ content]
  [:div.list-block-label content])

(defcomponent label [_ content]
  [:div.item-title.label content])

(defcomponent input [{:keys [type] :as props} content]
  [:div.item-input
   (conj (case type
           :range [:div.range-slider]
           nil)
         [(if (#{:select :textarea} type) type :input) props content])])

(defcomponent block [{:keys [inner? inset?]} content]
  [:div {:class (classes {:content-block true
                          :inset inset?})}
   (if inner?
     [:div.content-block-inner content]
     content)])

(defcomponent block-title [_ content]
  [:div.content-block-title content])

(defcomponent nav-left [_ content]
  [:div.left content])

(defcomponent nav-center [_ content]
  [:div.center content])

(defcomponent nav-right [_ content]
  [:div.right content])

(defcomponent navbar [{:keys [title back-link back-link-url]} content]
  [:div.navbar
   [:div.navbar-inner
    (when back-link
      (nav-left (link {:back? true
                       :href back-link-url
                       :icon :icon-back
                       :text back-link})))
    (when title (nav-center title))
    content]])

(defcomponent popup [{:keys [id]} content]
  [:div.popup {:id id} content])

(defcomponent page [_ content]
  [:div.page
   [:div.page-content
    content]])

(defcomponent grid [{:keys [no-gutter?]} content]
  [:div {:class (classes {:row true
                          :no-gutter no-gutter?})}
   content])

(defcomponent col [{:keys [width tablet-width]} content]
  [:div {:class (classes [[:col-auto (not (or width tablet-width))]
                          [[:col width] width]
                          [[:col tablet-width] tablet-width]])}
   content])
