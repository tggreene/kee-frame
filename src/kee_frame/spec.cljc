(ns ^:no-doc kee-frame.spec
  (:require [re-frame.interceptor :refer [->interceptor get-effect get-coeffect assoc-coeffect assoc-effect]]
            [re-frame.core :as rf]
            [clojure.spec.alpha :as s]
            [expound.alpha :as e]
            [kee-frame.api :as api]
            [kee-frame.core :as core]
            [kee-frame.router :as router]
            [kee-frame.controller :as controller]))

(s/def ::params (s/or :path-vector vector? :fn fn?))
(s/def ::start (s/or :vector ::event-vector :fn fn?))
(s/def ::stop (s/or :vector ::event-vector :fn fn?))

(s/def ::controller (s/keys :req-un [::params ::start]
                            :opt-un [::stop]))

(s/def ::event-vector (s/cat :event-key keyword? :event-args (s/* any?)))

(s/def ::routes any?)
(s/def ::router #(satisfies? api/Router %))
(s/def ::hash-routing? (s/nilable boolean?))
(s/def ::root-component (s/nilable vector?))
(s/def ::global-interceptors (s/nilable vector?))
(s/def ::initial-db (s/nilable map?))
(s/def ::app-db-spec (s/nilable keyword?))
(s/def ::blacklist (s/coll-of keyword? :kind set?))
(s/def ::debug?  boolean?)
(s/def ::debug-config (s/nilable (s/keys :opt-un [::blacklist ::events? ::controllers? ::routes? ::overwrites?])))
(s/def ::link (s/keys :req-un [::effect-present? ::get-dispatch ::set-dispatch]))
(s/def ::chain-links (s/nilable (s/coll-of ::link)))
(s/def ::breakpoints vector?)
(s/def ::debounce-ms number?)
(s/def ::log-spec-error (s/nilable fn?))
(s/def ::scroll (s/nilable (s/or :boolean boolean?
                                 :config (s/keys :opt-un [:scroll/timeout]))))
(s/def ::screen (s/nilable (s/or :boolean boolean?
                                 :config (s/keys :req-un [::breakpoints ::debounce-ms]))))

(s/def ::start-options (s/keys :opt-un [::routes ::router ::hash-routing? ::root-component ::initial-db ::log ::log-spec-error
                                        ::app-db-spec ::debug? ::debug-config ::chain-links ::screen ::scroll ::global-interceptors]))

(s/def ::reitit-route-data (s/cat :route-name keyword? :path-params (s/* (s/map-of keyword? any?))))

(defn default-log-spec-error [new-db spec event]
  (rf/console :group "*** Spec error when updating DB, rolling back event " event " ***")
  (e/expound spec new-db)
  (rf/console :groupEnd "*****************************"))

(defn rollback [context new-db db-spec log-spec-error]
  ((or log-spec-error
       default-log-spec-error) new-db db-spec (get-coeffect context :event))
  (assoc-effect context :db (get-coeffect context :db)))

(defn spec-interceptor [db-spec log-spec-error]
  (->interceptor
   :id :spec
   :after (fn [context]
            (let [new-db (get-effect context :db)]
              (if (and new-db (not (s/valid? db-spec new-db)))
                (rollback context new-db db-spec log-spec-error)
                context)))))

(defn validate-start-options!
  [options]
  (when-not (s/valid? ::start-options options)
    (e/expound ::start-options options)
    (throw (ex-info "Invalid start options" (s/explain-data ::start-options options)))))

(defn validate-controller!
  [controller]
  (when-not (s/valid? ::controller controller)
    (e/expound ::controller controller)
    (throw (ex-info "Invalid controller" (s/explain-data ::controller controller)))))

(defn validate-dispatch!
  [dispatch]
  (when-not (s/valid? ::event-vector dispatch)
    (e/expound ::event-vector dispatch)
    (throw (ex-info "Invalid dispatch value" (s/explain-data ::event-vector dispatch)))))

(defn validate-route-data!
  [data]
  (when-not (s/valid? ::reitit-route-data data)
    (e/expound ::reitit-route-data data)
    (throw (ex-info "Bad route data input" (s/explain-data ::reitit-route-data data)))))

(defn start-spec-interceptor!
  [app-db-spec log-spec-error]
  (rf/reg-global-interceptor (spec-interceptor app-db-spec log-spec-error)))

(defn initialize-spec-validation!
  []
  #?@(:cljs
      [(set! core/validate-start-options! validate-start-options!)
       (set! core/validate-controller! validate-controller!)
       (set! router/validate-route-data! validate-route-data!)
       (set! router/start-spec-interceptor! start-spec-interceptor!)
       (set! controller/validate-dispatch! validate-dispatch!)]
      :clj
      [(alter-var-root #'core/validate-start-options! (constantly validate-start-options!))
       (alter-var-root #'core/validate-controller! (constantly validate-controller!))
       (alter-var-root #'router/validate-route-data! (constantly validate-route-data!))
       (alter-var-root #'router/start-spec-interceptor! (constantly start-spec-interceptor!))
       (alter-var-root #'controller/validate-dispatch! (constantly validate-dispatch!))]))

(defn reset-spec-validation!
  []
  (let [do-nothing (constantly nil)]
    #?@(:cljs
        [(set! core/validate-start-options! do-nothing)
         (set! core/validate-controller! do-nothing)
         (set! router/validate-route-data! do-nothing)
         (set! router/start-spec-interceptor! do-nothing)
         (set! controller/validate-dispatch! do-nothing)]
        :clj
        [(alter-var-root #'core/validate-start-options! (constantly do-nothing))
         (alter-var-root #'core/validate-controller! (constantly do-nothing))
         (alter-var-root #'router/validate-route-data! (constantly do-nothing))
         (alter-var-root #'router/start-spec-interceptor! (constantly do-nothing))
         (alter-var-root #'controller/validate-dispatch! (constantly do-nothing))])))

(comment
  (reset-spec-validation!))
