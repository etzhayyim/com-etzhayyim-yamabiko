(ns yamabiko.murakumo
  "Pure cljc actor boundary generated from manifest migration scaffold."
  (:require [clojure.string :as str]))

(def actor-did
  "did:web:etzhayyim.com:yamabiko")

(def common-gates
  [:council-charter-attestation
   :no-platform-held-key-baseline
   :no-probing-baseline
   :murakumo-only-inference-baseline
   :did-primary-baseline
   :append-only-gate-baseline
   :kotoba-only-substrate-baseline])

(defn collection
  [name]
  (str "com.etzhayyim.yamabiko." name))

(def cell-specs {
  :carbody_fabrication {:legacy-cell "carbody-fabrication"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "carbody_fabrication")]
     :required-gates common-gates
     :trigger "manifest cell carbody_fabrication"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :bogie_assembly {:legacy-cell "bogie-assembly"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "bogie_assembly")]
     :required-gates common-gates
     :trigger "manifest cell bogie_assembly"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :interior_hvac {:legacy-cell "interior-hvac"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "interior_hvac")]
     :required-gates common-gates
     :trigger "manifest cell interior_hvac"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :traction_electrical {:legacy-cell "traction-electrical"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "traction_electrical")]
     :required-gates common-gates
     :trigger "manifest cell traction_electrical"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :final_assembly {:legacy-cell "final-assembly"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "final_assembly")]
     :required-gates common-gates
     :trigger "manifest cell final_assembly"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :dynamic_test {:legacy-cell "dynamic-test"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "dynamic_test")]
     :required-gates common-gates
     :trigger "manifest cell dynamic_test"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :homologation_binder {:legacy-cell "homologation-binder"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "homologation_binder")]
     :required-gates common-gates
     :trigger "manifest cell homologation_binder"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :emissions_acoustic_audit {:legacy-cell "emissions-acoustic-audit"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "emissions_acoustic_audit")]
     :required-gates common-gates
     :trigger "manifest cell emissions_acoustic_audit"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :silen_rail_review {:legacy-cell "silen-rail-review"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "silen_rail_review")]
     :required-gates common-gates
     :trigger "manifest cell silen_rail_review"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
})

(defn safe-rkey
  [s]
  (let [clean (-> (str s)
                  (str/replace #"^did:web:" "")
                  (str/replace #"[^A-Za-z0-9._~-]" "-"))]
    (if (str/blank? clean) "unknown" clean)))

(defn gate-value
  [attestations gate]
  (or (get attestations gate)
      (get attestations (name gate))
      (when (set? attestations) (attestations gate))
      (when (set? attestations) (attestations (name gate)))))

(defn missing-gates
  [spec attestations]
  (->> (:required-gates spec)
       (remove #(boolean (gate-value attestations %)))
       vec))

(defn put-record-effect
  [collection rkey record]
  {:op :mst/put-record
   :actor actor-did
   :collection collection
   :rkey rkey
   :record record})

(defn records-for
  [spec {:keys [records record computed-at request-id]
         :as input}]
  (let [input-records (cond
                        (map? records) records
                        (some? record) {0 record}
                        :else {})
        base {:actorDid actor-did
              :computedAt computed-at
              :legacyCell (:legacy-cell spec)
              :phase (:phase spec)
              :requestId request-id
              :actorBoundary "cljc-migration-scaffold"
              :scaffold true
              :constitutionalStatus "attested-plan"}]
    (map-indexed
     (fn [idx coll]
       (let [record* (merge {:$type coll}
                            base
                            (or (get input-records coll)
                                (get input-records idx)
                                {}))
             rkey (safe-rkey (or (:rkey record*)
                                 (get record* "rkey")
                                 (:tid record*)
                                 request-id
                                 (str (:legacy-cell spec) "-" idx)))]
         {:collection coll
          :record record*
          :rkey rkey}))
     (:collections spec))))

(defn cell-plan
  [cell-key {:keys [attestations] :as input}]
  (let [spec (get cell-specs cell-key)]
    (when-not spec
      (throw (ex-info "unknown cell" {:cell cell-key})))
    (let [missing (missing-gates spec attestations)]
      (merge
       {:cell cell-key
        :legacy-cell (:legacy-cell spec)
        :actor actor-did
        :phase (:phase spec)
        :murakumo-node (:murakumo-node spec)
        :trigger (:trigger spec)
        :ceiling (:ceiling spec)
        :required-gates (:required-gates spec)
        :missing-gates missing}
       (if (seq missing)
         {:status :blocked
          :effects []}
         (let [planned-records (records-for spec input)]
           {:status :ready
            :records (vec planned-records)
            :effects (mapv (fn [{:keys [collection record rkey]}]
                             (put-record-effect collection rkey record))
                           planned-records)}))))))

(defn all-cell-plans
  [input]
  (into {}
        (map (fn [cell-key] [cell-key (cell-plan cell-key input)]))
        (keys cell-specs)))
