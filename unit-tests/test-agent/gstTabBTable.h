/*
 * Note: this file originally auto-generated by mib2c using
 *       version : 14170 $ of $
 *
 * $Id:$
 */
#ifndef GSTTABBTABLE_H
#define GSTTABBTABLE_H

#ifdef __cplusplus
extern          "C" {
#endif


/** @addtogroup misc misc: Miscellaneous routines
 *
 * @{
 */
#include <net-snmp/library/asn1.h>

    /*
     * other required module components 
     */
    /* *INDENT-OFF*  */
config_add_mib(GUILE-SNMP-TEST-MIB)
config_require(GUILE-SNMP-TEST-MIB/gstTabBTable/gstTabBTable_interface)
config_require(GUILE-SNMP-TEST-MIB/gstTabBTable/gstTabBTable_data_access)
config_require(GUILE-SNMP-TEST-MIB/gstTabBTable/gstTabBTable_data_get)
config_require(GUILE-SNMP-TEST-MIB/gstTabBTable/gstTabBTable_data_set)
    /* *INDENT-ON*  */

    /*
     * OID and column number definitions for gstTabBTable 
     */
#include "gstTabBTable_oids.h"

    /*
     * enum definions 
     */
#include "gstTabBTable_enums.h"

    /*
     *********************************************************************
     * function declarations
     */
    void            init_gstTabBTable(void);
    void            shutdown_gstTabBTable(void);

    /*
     *********************************************************************
     * Table declarations
     */
/**********************************************************************
 **********************************************************************
 ***
 *** Table gstTabBTable
 ***
 **********************************************************************
 **********************************************************************/
    /*
     * GUILE-SNMP-TEST-MIB::gstTabBTable is subid 4 of gstMibObjects.
     * Its status is Current.
     * OID: .1.3.6.1.3.1977.1.4, length: 8
     */
    /*
     *********************************************************************
     * When you register your mib, you get to provide a generic
     * pointer that will be passed back to you for most of the
     * functions calls.
     *
     * TODO:100:r: Review all context structures
     */
    /*
     * TODO:101:o: |-> Review gstTabBTable registration context.
     */
    typedef netsnmp_data_list gstTabBTable_registration;

/**********************************************************************/
    /*
     * TODO:110:r: |-> Review gstTabBTable data context structure.
     * This structure is used to represent the data for gstTabBTable.
     */
    /*
     * This structure contains storage for all the columns defined in the
     * gstTabBTable.
     */
    typedef struct gstTabBTable_data_s {

        /*
         * gstTabBData(3)/DisplayString/ASN_OCTET_STR/char(char)//L/A/w/e/R/d/H
         */
        char            gstTabBData[255];
        size_t          gstTabBData_len;        /* # of char elements, not bytes */

    } gstTabBTable_data;


    /*
     * TODO:120:r: |-> Review gstTabBTable mib index.
     * This structure is used to represent the index for gstTabBTable.
     */
    typedef struct gstTabBTable_mib_index_s {

        /*
         * gstTabBIndex(1)/INTEGER32/ASN_INTEGER/long(long)//l/A/w/e/R/d/h
         */
        long            gstTabBIndex;

        /*
         * gstTabBIndex2(2)/DisplayString/ASN_OCTET_STR/char(char)//L/A/w/e/R/d/H
         */
        /** 128 - 1(other indexes) - oid length(10) = 116 */
        char            gstTabBIndex2[116];
        size_t          gstTabBIndex2_len;


    } gstTabBTable_mib_index;

    /*
     * TODO:121:r: |   |-> Review gstTabBTable max index length.
     * If you KNOW that your indexes will never exceed a certain
     * length, update this macro to that length.
     *
     * BE VERY CAREFUL TO TAKE INTO ACCOUNT THE MAXIMUM
     * POSSIBLE LENGHT FOR EVERY VARIABLE LENGTH INDEX!
     * Guessing 128 - col/entry(2)  - oid len(8)
     */
#define MAX_gstTabBTable_IDX_LEN     118


    /*
     *********************************************************************
     * TODO:130:o: |-> Review gstTabBTable Row request (rowreq) context.
     * When your functions are called, you will be passed a
     * gstTabBTable_rowreq_ctx pointer.
     */
    typedef struct gstTabBTable_rowreq_ctx_s {

    /** this must be first for container compare to work */
        netsnmp_index   oid_idx;
        oid             oid_tmp[MAX_gstTabBTable_IDX_LEN];

        gstTabBTable_mib_index tbl_idx;

        gstTabBTable_data data;

        /*
         * flags per row. Currently, the first (lower) 8 bits are reserved
         * for the user. See mfd.h for other flags.
         */
        u_int           rowreq_flags;

        /*
         * TODO:131:o: |   |-> Add useful data to gstTabBTable rowreq context.
         */

        /*
         * storage for future expansion
         */
        netsnmp_data_list *gstTabBTable_data_list;

    } gstTabBTable_rowreq_ctx;

    typedef struct gstTabBTable_ref_rowreq_ctx_s {
        gstTabBTable_rowreq_ctx *rowreq_ctx;
    } gstTabBTable_ref_rowreq_ctx;

    /*
     *********************************************************************
     * function prototypes
     */
    int             gstTabBTable_pre_request(gstTabBTable_registration *
                                             user_context);
    int             gstTabBTable_post_request(gstTabBTable_registration *
                                              user_context, int rc);

    int             gstTabBTable_rowreq_ctx_init(gstTabBTable_rowreq_ctx *
                                                 rowreq_ctx,
                                                 void *user_init_ctx);
    void            gstTabBTable_rowreq_ctx_cleanup(gstTabBTable_rowreq_ctx
                                                    * rowreq_ctx);


    gstTabBTable_rowreq_ctx
        *gstTabBTable_row_find_by_mib_index(gstTabBTable_mib_index *
                                            mib_idx);

    extern oid      gstTabBTable_oid[];
    extern int      gstTabBTable_oid_size;


#include "gstTabBTable_interface.h"
#include "gstTabBTable_data_access.h"
#include "gstTabBTable_data_get.h"
#include "gstTabBTable_data_set.h"

    /*
     * DUMMY markers, ignore
     *
     * TODO:099:x: *************************************************************
     * TODO:199:x: *************************************************************
     * TODO:299:x: *************************************************************
     * TODO:399:x: *************************************************************
     * TODO:499:x: *************************************************************
     */

#ifdef __cplusplus
}
#endif
#endif                          /* GSTTABBTABLE_H */
/** @} */
