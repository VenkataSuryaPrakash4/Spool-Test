*&---------------------------------------------------------------------*
*& Report ZSPOOL_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zspool_test.

TYPE-POOLS: slis. " SLIS contains all the ALV data types
TABLES:acdoca.

TYPES: BEGIN OF ty_acdoca,
         rbukrs TYPE bukrs,
         gjahr  TYPE gjahr,
         belnr  TYPE belnr_d,
         tsl    TYPE fins_vtcur12,
         budat  TYPE budat,
         bldat  TYPE bldat,
         kunnr  TYPE kunnr,
       END OF ty_acdoca.

DATA:wa_screen    TYPE screen,
     gt_acdoca    TYPE TABLE OF ty_acdoca,
     gt_items     TYPE TABLE OF bapi3007_2,
     gt_items_det TYPE TABLE OF bapi3007_2,
     gt_fieldcat  TYPE slis_t_fieldcat_alv,
     gt_sort      TYPE slis_t_sortinfo_alv,
     gt_filter    TYPE slis_t_filter_alv,

     wa_layout    TYPE slis_layout_alv,

     gv_bukrs     TYPE bapi3007_1-comp_code,
     gv_kunnr     TYPE bapi3007_1-customer,
     gv_budat     TYPE bapi3007-key_date,
     gt_final     TYPE TABLE OF zsttst,
     wa_final     TYPE zsttst,
     gv_count     TYPE n LENGTH 2.
*INCLUDE ZSPOOL_TEST_gd.  "Global Declarations
*INCLUDE ZSPOOL_TEST_ss.  "Selection Screen
*INCLUDE ZSPOOL_TEST_cd.  "Business Logic


*INITIALIZATION.
*PERFORM clear.
*
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM screen_validation.


START-OF-SELECTION.
*PERFORM get_data.
  SELECT rbukrs,gjahr,belnr,tsl,budat,bldat,kunnr
    FROM acdoca
    INTO TABLE @gt_acdoca
    UP TO 5000 ROWS.
*    WHERE rbukrs = @p_bukrs AND
*          gjahr = @p_gjahr AND
*          belnr IN @s_belnr AND
*          koart = 'D'.
  IF sy-subrc = 0.
    SORT gt_acdoca BY rbukrs kunnr budat.
  ENDIF.

*&—– Fetch data from the database —–*
  LOOP AT gt_acdoca INTO DATA(lwa_acdoca).
    gv_bukrs = lwa_acdoca-rbukrs.
    gv_kunnr = lwa_acdoca-kunnr.
    gv_budat = lwa_acdoca-budat.

    CALL FUNCTION 'BAPI_AR_ACC_GETOPENITEMS'
      EXPORTING
        companycode = gv_bukrs
        customer    = gv_kunnr
        keydate     = gv_budat
        noteditems  = 'X'
        secindex    = 'X'
      TABLES
        lineitems   = gt_items.

    READ TABLE gt_items INTO DATA(lwa_items) WITH KEY  comp_code = lwa_acdoca-rbukrs
                                                 fisc_year = lwa_acdoca-gjahr
                                                 doc_no = lwa_acdoca-belnr
                                                 customer = lwa_acdoca-kunnr.
    IF sy-subrc = 0.
      wa_final-bukrs = lwa_acdoca-rbukrs.
      wa_final-gjahr = lwa_acdoca-gjahr.
      wa_final-belnr = lwa_acdoca-belnr.
      wa_final-budat = lwa_acdoca-budat.
      wa_final-bldat = lwa_acdoca-bldat.
      wa_final-dmbtr = lwa_acdoca-tsl.
      APPEND wa_final TO gt_final.
    ENDIF.
    CLEAR:lwa_acdoca,gv_bukrs,gv_kunnr,gv_budat,wa_final,lwa_items.
    REFRESH:gt_items.
  ENDLOOP.

*&—– Field Catalog —–*
  ADD 1 TO gv_count.
  APPEND VALUE #( col_pos = gv_count fieldname = 'BUKRS'
                  key = 'X' outputlen = 5
                  seltext_s = 'Comp Code' ) TO gt_fieldcat.
  ADD 1 TO gv_count.
  APPEND VALUE #( col_pos = gv_count fieldname = 'GJAHR'
                  key = 'X' outputlen = 5
                  seltext_s = 'Fisc Year' ) TO gt_fieldcat.

  ADD 1 TO gv_count.
  APPEND VALUE #( col_pos = gv_count fieldname = 'BELNR'
                  key = 'X' outputlen = 10
                  seltext_s = 'Doc num'
                  no_zero = 'X') TO gt_fieldcat.

  ADD 1 TO gv_count.
  APPEND VALUE #( col_pos = gv_count fieldname = 'BUDAT'
                  outputlen = 10
                  seltext_s = 'Post Date' ) TO gt_fieldcat.

  ADD 1 TO gv_count.
  APPEND VALUE #( col_pos = gv_count fieldname = 'BLDAT'
                  outputlen = 10
                  seltext_s = 'Doc Date' ) TO gt_fieldcat.

  ADD 1 TO gv_count.
  APPEND VALUE #( col_pos = gv_count fieldname = 'DMBTR'
                    do_sum = 'X' outputlen = 15
                  seltext_s = 'Amount'
                    edit = 'X' ) TO gt_fieldcat.

*--- Layout
*    wa_layout-lights_fieldname = 'ICON'.
  wa_layout-zebra = 'X'.
  wa_layout-colwidth_optimize = 'X'.

*--- Sort
    APPEND VALUE #( fieldname = 'BELNR'
                    tabname = 'LT_FINAL' up = 'X'
                    subtot = 'X' ) TO gt_sort.

*--- Filter
  gt_filter = VALUE #( ( fieldname = 'BELNR' tabname = 'LT_FINAL'
                       sign0 = 'I' optio = 'BT'
                       valuf_int = '0000001406'
                       valut_int = '0000001803' ) ).

  DATA:lt_events   TYPE slis_t_event,
       lv_list_typ TYPE slis_list_type.

  CONSTANTS: c_4 TYPE c VALUE '4'.
  lv_list_typ = c_4.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = lv_list_typ
    IMPORTING
      et_events       = lt_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.

  IF lt_events IS NOT INITIAL.
    DATA(lwa_event) = lt_events[ 3 ].
    lwa_event-form = 'TOP_OF_PAGE'.
    MODIFY lt_events FROM lwa_event INDEX 3."sy-tabix.
    CLEAR:lwa_event.

    lwa_event = lt_events[ 12 ].
    lwa_event-form = 'END_OF_LIST'.
    MODIFY lt_events FROM lwa_event INDEX 12.
    CLEAR:lwa_event.
  ENDIF.

** Display ALV
  DATA: alv_print TYPE slis_print_alv.
  alv_print-no_print_listinfos = 'X'. "avoid process list on SPOOL

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'ZPF_STAT_CUST'
        i_callback_user_command  = 'USR_CMD'
        is_layout                = wa_layout
        it_fieldcat              = gt_fieldcat
        is_print                 = alv_print
       it_sort                  = gt_sort
       it_filter                = gt_filter
       i_save                   = 'A'
       it_events                = lt_events
      TABLES
        t_outtab                 = gt_final
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.

FORM top_of_page.
  DATA:lt_head TYPE slis_t_listheader.
*  DATA(lv_inp) = |{ p_bukrs }| & |/| & |{ p_gjahr }|.
    DATA(lv_inp) = | 5000 | & |/| & |2021|.
  lt_head = VALUE #( ( typ = 'H' info = 'Customer ageing report' )
                     ( typ = 'S' key = 'Date' info = sy-datum )
                     ( typ = 'S' key = 'User' info = sy-uname )
                     ( typ = 'S' key = 'Inputs' info = lv_inp ) ).

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_head
      i_logo             = 'SAPABAP_IMG'.
ENDFORM.

FORM end_of_list.
  DATA:lt_foot TYPE slis_t_listheader.
  lt_foot = VALUE #( ( typ = 'A' info = 'TEXT-005' ) ).

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_foot.
ENDFORM.

*&———————————————————————*
*& Form FORM_MENU
*&———————————————————————*
* SET Screen
*———————————————————————-*
FORM ZPF_STAT_CUST USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'ZPF_STAT_CUST'.

ENDFORM. "FORM_MENU
*&———————————————————————*
*& Form USER_COMMAND
*&———————————————————————*
* for to handle user command
*———————————————————————-*

FORM USR_CMD USING r_ucomm LIKE sy-ucomm
rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&DOW'. "Function code for export which we created in MENU
**submit the same program in background and store
      TYPES: BEGIN OF ty_tsp01,
               rqident   TYPE tsp01-rqident, " spool number
               rq2name   TYPE tsp01-rq2name, " Spool request: Suffix 2
               rqcretime TYPE tsp01-rqcretime, " User name
             END OF ty_tsp01.
      DATA: lv_jobcnt  TYPE tbtcjob-jobcount, " job number
            lv_jobname TYPE tbtcjob-jobname. " job name
      DATA: lv_len   TYPE i,
            ls_param TYPE rsparams, " selection work area
            ls_tsp01 TYPE ty_tsp01, " Spool Requests work area
            lt_tsp01 TYPE STANDARD TABLE OF ty_tsp01, " Spool Requests internal table
            lt_param TYPE rsparams_tt. " Selection table

********** Derive job counter
      lv_jobname = 'ZALV2PDF'. "Background job name
      CALL FUNCTION 'JOB_OPEN' "open a job
        EXPORTING
          jobname          = lv_jobname
        IMPORTING
          jobcount         = lv_jobcnt
        EXCEPTIONS
          cant_create_job  = 1
          invalid_job_data = 2
          jobname_missing  = 3
          OTHERS           = 4.
      IF sy-subrc EQ 0.

        DATA : lv_rqdest TYPE tsp01-rqdest VALUE 'LP01',
               lv_linsz  TYPE sylinsz VALUE '9999999'.

        SUBMIT (sy-repid) "submit the same program
        TO SAP-SPOOL DESTINATION lv_rqdest
        LINE-SIZE lv_linsz
        IMMEDIATELY 'X'
        KEEP IN SPOOL 'X'
        USER sy-uname VIA JOB lv_jobname NUMBER lv_jobcnt
        WITHOUT SPOOL DYNPRO
        WITH SELECTION-TABLE lt_param
        AND RETURN.

        CALL FUNCTION 'JOB_CLOSE' "job close
          EXPORTING
            jobcount             = lv_jobcnt
            jobname              = lv_jobname
            strtimmed            = 'X'
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            invalid_target       = 8
            OTHERS               = 9.
        IF sy-subrc <> 0.
          RAISE job_cannot_be_closed. " Raise exception
        ENDIF.
      ELSE.
        RAISE job_cannot_be_submitted.
      ENDIF.

*********** confirm job status if finished
      DATA: lv_job TYPE tbtcv-fin. " Job status
      DO 120 TIMES.
        CALL FUNCTION 'BDL_READ_JOB_STATUS' "get job status
          EXPORTING
            jobname       = lv_jobname
            jobnumber     = lv_jobcnt
          IMPORTING
            jobstatus     = lv_job
          EXCEPTIONS
            job_not_found = 1
            OTHERS        = 2.
        IF lv_job NE 'F'. "job finished
          WAIT UP TO 1 SECONDS.
          CONTINUE.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

**calculate report name in Spool table
      DATA: lv_rq2name TYPE tsp01-rq2name. " Spool request
      DATA: lv_tmp(14) TYPE c, " temp
            lv_string  TYPE string, " store program name
            lv_temp    TYPE tsp01-rq2name, " Spool request
            lv_spool   TYPE tsp01-rqident. " spool number

      IF lv_job EQ 'F'. "job finished
        lv_len = strlen( sy-repid ) .
        IF lv_len >= 9 .
          CONCATENATE sy-repid+0(9)
          sy-uname+0(3) INTO lv_rq2name .
        ELSE.
          lv_len = 9 - lv_len.
          DO lv_len TIMES .
            CONCATENATE lv_temp '_' INTO lv_temp .
          ENDDO.
          CONCATENATE sy-repid lv_temp
          sy-uname INTO lv_rq2name .
        ENDIF.

*Get spool request from SPOOL table TSP01
        SELECT rqident rq2name rqcretime
        FROM tsp01
        INTO TABLE lt_tsp01
        WHERE rq2name = lv_rq2name
        AND rqowner = sy-uname .

*sort table to find latest spool no
        SORT lt_tsp01 BY rqcretime DESCENDING .

*read table to find latest spool no
        READ TABLE lt_tsp01 INTO ls_tsp01 INDEX 1.

*get user desktop
        CALL METHOD cl_gui_frontend_services=>get_desktop_directory
          CHANGING
            desktop_directory    = lv_string
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            OTHERS               = 4.
        IF sy-subrc <> 0.
* Implement suitable error handling here
          lv_string = sy-repid.
        ENDIF.
        CALL METHOD cl_gui_cfw=>update_view.
        CONCATENATE lv_string TEXT-001 INTO lv_string.

*Prepare selection table for PDF download
        ls_param-selname = 'SPOOLNO'.
        ls_param-sign = 'I'.
        ls_param-option = 'EQ'.
        ls_param-low = ls_tsp01-rqident.
        ls_param-high = ' '.
        APPEND ls_param TO lt_param.

        ls_param-selname = 'P_FILE'.
        ls_param-sign = 'I'.
        ls_param-option = 'EQ'.
        ls_param-low = lv_string.
        ls_param-high = ' '.
        APPEND ls_param TO lt_param.

* Submit to PDF converted and download
        SUBMIT rstxpdft4 WITH SELECTION-TABLE lt_param
        AND RETURN.
        IF sy-subrc <> 0.

          MESSAGE 'PDF convert not possible' TYPE 'I'.
        ENDIF.
      ELSE.
        MESSAGE 'PDF Can not be downloaded, you have selected huge data. Reduce data and try again' TYPE 'I'.
      ENDIF.
  ENDCASE.
ENDFORM. "user command
