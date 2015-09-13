# Guile SNMP

  Guile SNMP is a wrapper for the Net-SNMP libraries. it is intended
to provide a basic wrapper for direct useage of the Net-SNMP libraries
from Guile and an easy to use dialect of Scheme targeted at easier
SNMP reporting.

  Simple things are nice and simple

```scheme
; Get sysName from a device
(get sysName.0)

; Get the list of ifNames
(walk ifName)

; Get the list of ifNames using a getbulk
(bulk-walk ifName)

; Walk the ifName tables using a function
(let loop ((name (walk-func ifName)))
  (display name)(newline))

; Walk the ifName tables using a function
(let loop ((name (bulk-walk-func ifName)))
  (display name)(newline))
```

  Far more interesting things are possible, like visualizing
OSPF or Spanning tree, or making your [[Cisco devices dump
their config|cisco-config-dump]]

