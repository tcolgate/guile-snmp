#!/usr/local/bin/snmp-shell
!#

(let ((tds (netsnmp-create-table-data-set "stuff")))
  (netsnmp-table-dataset-add-index tds ASN-OCTET-STR)
  (netsnmp-table-set-add-default-row tds 2 ASN-OCTET-STR 0 (nullOid) 0)
  (netsnmp-table-set-add-default-row tds 3 ASN-INTEGER 0 (nullOid) 0)

  (netsnmp-register-table-data-set
    (netsnmp-create-handler-registration "test" 
                                         nullOid 
                                         .1.2.3.4.5.6.7 
                                         HANDLER-CAN-RONLY) 
    tds 
    nullOid))

