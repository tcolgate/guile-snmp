(use-modules (snmp net-snmp)
             (unit-test)
             (oop goops))

 (slot-ref (slot-ref (slot-ref (get-tree (oid val) (get-tree-head)) 'enums) 'next) 'label)

(exit-with-summary (run-all-defined-test-cases))

