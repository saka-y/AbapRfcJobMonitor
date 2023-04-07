REPORT yrfcjobmonitor.

CLASS lcx_sendmailerror DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .
ENDCLASS.

CLASS lcl_tbtco DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      tt_jobname TYPE RANGE OF tbtco-jobname.

    TYPES:
      BEGIN OF ts_tbtco,
        jobname   TYPE tbtco-jobname,
        jobcount  TYPE tbtco-jobcount,
        strtdate  TYPE tbtco-strtdate,
        strttime  TYPE tbtco-strttime,
        enddate   TYPE tbtco-enddate,
        endtime   TYPE tbtco-endtime,
        authckman TYPE tbtco-authckman,
      END OF ts_tbtco,
      tt_tbtco TYPE STANDARD TABLE OF ts_tbtco WITH DEFAULT KEY.

    CLASS-METHODS:
      main
        IMPORTING
          iv_dest   TYPE rfcdest
          iv_strtdt TYPE tbtco-strtdate
          iv_strttm TYPE tbtco-strttime
          iv_enddt  TYPE tbtco-enddate
          iv_endtm  TYPE tbtco-endtime
          iv_recv   TYPE so_recname
          it_jobnam TYPE tt_jobname.

  PRIVATE SECTION.
    CLASS-METHODS:

      get_tbtco
        IMPORTING
          iv_dest         TYPE rfcdest
          iv_strtdt       TYPE tbtco-strtdate
          iv_strttm       TYPE tbtco-strttime
          iv_enddt        TYPE tbtco-enddate
          iv_endtm        TYPE tbtco-endtime
          it_jobnam       TYPE tt_jobname
        RETURNING
          VALUE(lt_tbtco) TYPE tt_tbtco,

      get_options_jobname
        IMPORTING
          it_jobnam         TYPE tt_jobname
        RETURNING
          VALUE(rt_options) TYPE esh_t_co_rfcrt_options,

      mailtitle
        IMPORTING
          iv_mandt        TYPE sy-mandt
          iv_strtdt       TYPE tbtco-strtdate
          iv_strttm       TYPE tbtco-strttime
          iv_enddt        TYPE tbtco-enddate
          iv_endtm        TYPE tbtco-endtime
        RETURNING
          VALUE(rv_title) TYPE so_obj_des,

      mailcontents
        IMPORTING
          it_tbtco           TYPE tt_tbtco
        RETURNING
          VALUE(rt_contents) TYPE swuoconttab,

      sendmail
        IMPORTING
          iv_title   TYPE so_obj_des
          it_content TYPE swuoconttab
          iv_recv    TYPE so_recname
        RAISING
          lcx_sendmailerror.

ENDCLASS.

CLASS lcx_sendmailerror IMPLEMENTATION.
ENDCLASS.

CLASS lcl_tbtco IMPLEMENTATION.
  METHOD main.

    DATA(lt_tbtco) = get_tbtco( iv_dest   = iv_dest
                                iv_strtdt = iv_strtdt
                                iv_strttm = iv_strttm
                                iv_enddt  = iv_enddt
                                iv_endtm  = iv_endtm
                                it_jobnam = it_jobnam ).
    IF lt_tbtco IS INITIAL.
      MESSAGE s145(bt).
      LEAVE LIST-PROCESSING.
    ENDIF.

    TRY.
        sendmail( iv_title   = mailtitle( iv_mandt  = lt_tbtco[ 1 ]-authckman
                                          iv_strtdt = iv_strtdt
                                          iv_strttm = iv_strttm
                                          iv_enddt  = iv_enddt
                                          iv_endtm  = iv_endtm )
                  it_content = mailcontents( it_tbtco = lt_tbtco )
                  iv_recv    = iv_recv ).
      CATCH lcx_sendmailerror INTO DATA(lo_msg).
        MESSAGE ID     lo_msg->if_t100_message~t100key-msgid
                TYPE   lo_msg->if_t100_dyn_msg~msgty
                NUMBER lo_msg->if_t100_message~t100key-msgno
                WITH   lo_msg->if_t100_dyn_msg~msgv1
                       lo_msg->if_t100_dyn_msg~msgv2
                       lo_msg->if_t100_dyn_msg~msgv3
                       lo_msg->if_t100_dyn_msg~msgv4.
    ENDTRY.

  ENDMETHOD.

  METHOD get_tbtco.

    DATA:
      lt_options TYPE STANDARD TABLE OF rfc_db_opt,
      lt_fields  TYPE STANDARD TABLE OF rfc_db_fld,
      lt_data    TYPE STANDARD TABLE OF tab512.

    lt_options = VALUE #( ( text = 'STATUS = ''A''')
                          ( text = 'AND STRTDATE >= ''' && iv_strtdt && '''' )
                          ( text = 'AND ENDDATE <= ''' && iv_enddt && '''' ) ).
    IF it_jobnam IS NOT INITIAL.
      lt_options = VALUE #( BASE lt_options
                            ( text = 'AND' )
                            ( LINES OF get_options_jobname( it_jobnam = it_jobnam ) ) ).
    ENDIF.

    lt_fields = VALUE #( ( fieldname = 'JOBNAME' )
                         ( fieldname = 'JOBCOUNT' )
                         ( fieldname = 'STRTDATE' )
                         ( fieldname = 'STRTTIME' )
                         ( fieldname = 'ENDDATE' )
                         ( fieldname = 'ENDTIME' )
                         ( fieldname = 'AUTHCKMAN' ) ).
    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION iv_dest
      EXPORTING
        query_table          = 'TBTCO'
      TABLES
        options              = lt_options
        fields               = lt_fields
        data                 = lt_data
      EXCEPTIONS
        table_not_available  = 0
        table_without_data   = 0
        option_not_valid     = 0
        field_not_valid      = 0
        not_authorized       = 0
        data_buffer_exceeded = 0
        OTHERS               = 0.

    CHECK lt_data IS NOT INITIAL.

    lt_tbtco = VALUE #( BASE lt_tbtco
                        FOR ls_data IN lt_data
                        ( jobname   = ls_data+00(32)
                          jobcount  = ls_data+32(08)
                          strtdate  = ls_data+40(08)
                          strttime  = ls_data+48(06)
                          enddate   = ls_data+54(08)
                          endtime   = ls_data+62(06)
                          authckman = ls_data+68(03) ) ).

    DELETE lt_tbtco WHERE strtdate = iv_strtdt AND strttime < iv_strttm.
    DELETE lt_tbtco WHERE enddate = iv_enddt AND endtime > iv_endtm.

  ENDMETHOD.

  METHOD get_options_jobname.
    DATA:
      lt_sellist TYPE STANDARD TABLE OF vimsellist,
      lt_jobnam  TYPE tt_jobname.
    lt_jobnam = it_jobnam. "汎用モジュール内で変更されるためImportパラメータの値を待避して使用
    CALL FUNCTION 'VIEW_RANGETAB_TO_SELLIST'
      EXPORTING
        fieldname          = 'JOBNAME'
        append_conjunction = 'AND'
      TABLES
        sellist            = lt_sellist
        rangetab           = lt_jobnam.
    IF lt_sellist[] IS NOT INITIAL.
      rt_options = VALUE #( BASE rt_options
                            FOR ls_sellist IN lt_sellist
                            ( text = |{ ls_sellist-negation } { COND #( WHEN ls_sellist-leftpar = 0 THEN '' ELSE '(' ) }| &&
                                     |{ ls_sellist-viewfield } { ls_sellist-operator } '{ ls_sellist-value }' { COND #( WHEN ls_sellist-rightpar = 0 THEN '' ELSE ')' ) } { ls_sellist-and_or }| ) ).
    ENDIF.
  ENDMETHOD.

  METHOD mailtitle.
    rv_title = `CL` && |{ iv_mandt }| && ` (` &&
               |{ iv_strtdt DATE = USER } { iv_strttm TIME = USER }| && ` - ` &&
               |{ iv_enddt DATE = USER } { iv_endtm TIME = USER }| && `)`.
  ENDMETHOD.

  METHOD mailcontents.
    rt_contents = VALUE #( ( line = |{ lines( it_tbtco ) } | && '件のジョブでエラーが発生しています' ) ).
    rt_contents = VALUE #( BASE rt_contents
                          FOR ls_tbtco IN it_tbtco
                          ( line = ls_tbtco-jobname && `(` && ls_tbtco-jobcount && `) ` &&
                                   |{ ls_tbtco-strtdate DATE = USER } { ls_tbtco-strttime TIME = USER }| && ` - ` &&
                                   |{ ls_tbtco-enddate DATE = USER } { ls_tbtco-endtime TIME = USER }| ) ).
  ENDMETHOD.

  METHOD sendmail.

    DATA:
      ls_docdata   TYPE sodocchgi1,
      lt_receivers TYPE STANDARD TABLE OF somlreci1.

    ls_docdata = VALUE #( obj_descr = iv_title ).

    lt_receivers = VALUE #( ( receiver = iv_recv rec_type = 'U' ) ).

    CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
      EXPORTING
        document_data              = ls_docdata
        document_type              = 'RAW'
*       put_in_outbox              = 'X'
*       commit_work                = 'X'
      TABLES
        object_content             = it_content
        receivers                  = lt_receivers
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_sendmailerror
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    COMMIT WORK AND WAIT.
    IF line_exists( lt_receivers[ com_type = 'INT' ] ).
      WAIT UP TO 5 SECONDS.
      SUBMIT rsconn01 AND RETURN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

DATA:
  gs_tbtco TYPE tbtco.

PARAMETERS:
  p_dest   TYPE rfcdest OBLIGATORY,
  p_strtdt TYPE tbtco-strtdate OBLIGATORY,
  p_strttm TYPE tbtco-strttime OBLIGATORY,
  p_enddt  TYPE tbtco-enddate OBLIGATORY,
  p_endtm  TYPE tbtco-endtime OBLIGATORY,
  p_recv   TYPE so_recname OBLIGATORY.
SELECT-OPTIONS:
  s_jobnam FOR gs_tbtco-jobname.

INITIALIZATION.

  DATA(lv_span) = CONV t('010000').
  p_strtdt = COND #( WHEN sy-uzeit < lv_span THEN sy-datum - 1
                                             ELSE sy-datum ).
  p_enddt  = sy-datum.
  p_endtm  = ( sy-uzeit  DIV lv_span ) * lv_span.
  p_strttm = p_endtm - lv_span.


START-OF-SELECTION.
  lcl_tbtco=>main( iv_dest   = p_dest
                   iv_strtdt = p_strtdt
                   iv_strttm = p_strttm
                   iv_enddt  = p_enddt
                   iv_endtm  = p_endtm
                   iv_recv   = p_recv
                   it_jobnam = s_jobnam[]
  ).
