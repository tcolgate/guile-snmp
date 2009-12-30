(use-modules (asn1-parser)
             (unit-test)
             (oop goops))

(display (read-asn1 (current-input-port)))

(exit-with-summary (run-all-defined-test-cases))

