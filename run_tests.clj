(require '[clojure.test :as test])
(def test-namespaces
  '[yamabiko.cells.bogie-assembly.test-state-machine
    yamabiko.cells.carbody-fabrication.test-state-machine
    yamabiko.cells.dynamic-test.test-state-machine
    yamabiko.cells.emissions-acoustic-audit.test-state-machine
    yamabiko.cells.final-assembly.test-state-machine
    yamabiko.cells.homologation-binder.test-state-machine
    yamabiko.cells.interior-hvac.test-state-machine
    yamabiko.cells.silen-rail-review.test-state-machine
    yamabiko.cells.traction-electrical.test-state-machine
    yamabiko.methods.test-agent yamabiko.methods.test-charter-gates
    yamabiko.murakumo-test yamabiko.repository-contract-test])
(doseq [namespace test-namespaces] (require namespace))
(let [result (apply test/run-tests test-namespaces)]
  (println "==> yamabiko:" (select-keys result [:test :pass :fail :error]))
  (when (pos? (+ (:fail result) (:error result))) (System/exit 1)))
