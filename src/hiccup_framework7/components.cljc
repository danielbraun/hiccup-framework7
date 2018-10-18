(ns hiccup-framework7.components
  (:require [clojure.string :as string]
            [hiccup.def :refer [defelem]]
            [hiccup-framework7.def :refer [defcomponent classes]])
  (:refer-clojure :exclude [list]))

;TODO Add doc-strings to all components so that vim-fireplace shows their previews

(defn- select-icon-props [props]
  (->> props
       (filter (comp second (partial re-find #"^icon") name key))
       (map (fn [x]
              (update x 0 (comp keyword last #(string/split % #"-") name))))
       (into {})))

(defelem view [& content]
  (into [:div.view] content))

(defcomponent icon [{:keys [f7 icon material fa ion color size]} content]
  [:i {:class {:f7-icons f7
               :fa fa
               :icon true
               :material-icons material
               [:color color] color
               [:fa fa] fa
               [:ion ion] ion
               icon icon}
       :style (when size (str "font-size:" size "px"))}
   (name (or f7 material))
   content])

(defcomponent link [{:keys [close-popup back? external? href open-popup text color ]
                     :as props} content]
  (let [icon-props (not-empty (select-icon-props props))]
    [:a {:href (or href "#")
         :data-popup (or close-popup open-popup)
         :class {:link true
                 :icon-only (and icon-props (not text) (empty? content))
                 :external external?
                 :back back?
                 [:color color] color
                 :close-popup close-popup
                 :open-popup open-popup}}
     (when icon-props (icon icon-props))
     (when text [:span text])
     content]))

(defcomponent list [{:keys [form? media-list?]} content]
  [(if form? :form :div)
   {:class {:list-block true
            :media-list media-list?}}
   [:ul content]])

(defcomponent list-group [_ content]
  [:ul.list-group content])

(defcomponent list-item [{:keys [after badge badge-color checkbox? checked?
                                 disabled? divider? group-title? link
                                 media media-item? name radio? smart-select
                                 subtitle text title value]} content]
  (let [badge-el (when badge
                   [:span {:class {:badge true
                                   [:bg badge-color] badge-color}}
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
        after-wrap-el (when (or after-el badge-el)
                        [:div.item-after after-el badge-el])
        title-row-el (when media-item?
                       [:div.item-title-row title-el after-wrap-el])
        inner-el [:div.item-inner
                  (seq (if media-item?
                         [title-row-el subtitle-el text-el]
                         [title-el after-wrap-el]))
                  content]
        item-content-el [(if (or radio? checkbox?) :label :div)
                         {:class {:item-content true
                                  [:label :radio] radio?
                                  [:label :checkbox] checkbox?}}
                         input-el media-el inner-el]
        link-el (when (or smart-select link)
                  [:a {:href (or link "#")
                       :class {:item-link true
                               :smart-select smart-select}}
                   item-content-el])]
    [:li {:class {:item-divider divider?
                  :list-group-title group-title?}}
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
  [:div {:class {:content-block true
                 :inset inset?}}
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

(defn navbar [{:keys [title back-link back-link-url sliding?]
               :or {back-link "Back"}} & content]
  ;TODO: sliding
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

#_(defn page [{:keys [layout navbar toolbar fixed id with-subnavbar?]
             :or {layout :fixed}} & content]
  (let [page-attrs {:data-page id
                    :class (when with-subnavbar? "with-subnavbar")}]
    (case layout
      :static [:div.pages
               [:div.page page-attrs
                fixed
                [:div.page-content navbar content toolbar]]]
      :fixed [:div.pages.navbar-fixed.toolbar-fixed
              [:div.page page-attrs
               navbar fixed [:div.page-content content] toolbar]]
      :through (clojure.core/list
                 navbar
                 [:div.pages.navbar-through.toolbar-through
                  [:div.page page-attrs
                   fixed [:div.page-content content]]]
                 toolbar))))

(defcomponent grid [{:keys [no-gutter?]} content]
  [:div {:class {:row true
                 :no-gutter no-gutter?}}
   content])

(defcomponent col [{:keys [width tablet-width]} content]
  [:div {:class [[:col-auto (not (or width tablet-width))]
                 [[:col width] width]
                 [[:col tablet-width] tablet-width]]}
   content])

(defcomponent button [{:keys [round? big? raised? fill?]} content]
  [:button {:class {:button true
                    :button-fill fill?
                    :button-round round?
                    :button-raised raised?
                    :button-big big?}} content])

(defcomponent search-bar [{:keys [form? placeholder cancel-link
                                  clear-button? init? params]
                           :or {form? true
                                clear-button? true}}
                          content]
  [(if form? :form :div)
   {:class {:searchbar true}}
   [:div {:class {:searchbar-input true}}
    [:input {:type :search, :placeholder placeholder}]
    (when clear-button? [:a.searchbar-clear {:href "#"}])]
   (when cancel-link [:div.searchbar-cancel cancel-link])])

(defcomponent tabs [{:keys [animated? swipeable?]} content]
  (let [tabs-el [:div.tabs content]]
    (if animated?
      [:div {:class {:tabs-animated-wrap animated?
                     :tabs-swipeable-wrap swipeable?}} tabs-el]
      tabs-el)))

(defcomponent tab [{:keys [active? id]} content]
  [:div {:id id
         :class {:tab true
                 :active active?}}
   content])

(defcomponent buttons [_ content]
  [:div {:class {:buttons-row true}} content])

(defcomponent subnavbar [{:keys []} content]
  [:div.subnavbar content])

(defelem toolbar [& content]
  [:div.toolbar
   (into [:div.toolbar-inner] content)])

(defelem tabbar-label [& content]
  (into [:span.tabbar-label] content))

(defelem swiper [& content]
  [:div.swiper-container
   (into [:div.swiper-wrapper] content)
   [:div.swiper-pagination]])

(defelem swiper-slide [& content]
  (into [:div.swiper-slide] content))

(defelem pages [& content]
  (into [:div.pages] content))

(defelem page [& content]
  [:div.page
   (into [:div.page-content] content)])

(defn search-bar [{:keys [cancel-link init?]
                   :or {cancel-link "Cancel"
                        init? true}
                   :as attrs}]
  [:div.searchbar
   {:class (classes {:searchbar-init init?})}
   [:div.searchbar-input
    [:input (merge {:type :search
                    :placeholder "Search"
                    :name :q} attrs)]
    [:a.searchbar-clear {:href "#"}]]
   [:a.searchbar-cancel {:href "#"} cancel-link]])

(defelem views [& content]
  (into [:div.views] content))
