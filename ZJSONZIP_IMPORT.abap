REPORT zjsonzip_import.

**********************************************************************
* SCREEN
**********************************************************************
PARAMETERS: p_folder TYPE string LOWER CASE.

**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.
**********************************************************************
  PERFORM f4_folder.

**********************************************************************
START-OF-SELECTION.
**********************************************************************
  PERFORM execute.


**********************************************************************
* class
**********************************************************************
CLASS zcl_abap2json_mini DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_log,
        table TYPE tabname,
        file  TYPE string,
        text  TYPE string,
        count TYPE i,
      END OF ts_log .
    TYPES:
      tt_log TYPE TABLE OF ts_log .

    CONSTANTS gc_encoding TYPE abap_encod VALUE 'UTF-8' ##NO_TEXT.

    CLASS-METHODS json2abap
      IMPORTING
        !iv_json_zip TYPE xstring
      EXPORTING
        !et_data     TYPE data
        !ev_where    TYPE string .
    CLASS-METHODS import_json_zip
      IMPORTING
        !iv_folder            TYPE clike OPTIONAL
        !iv_del               TYPE flag OPTIONAL
        !iv_simulate          TYPE flag OPTIONAL
        !iv_show_progress_bar TYPE flag OPTIONAL
        !iv_show_confirm      TYPE flag OPTIONAL
      EXPORTING
        !et_log               TYPE tt_log
        !ev_error_text        TYPE char255 .
    CLASS-METHODS get_http_response
      IMPORTING
        !iv_url            TYPE string
      RETURNING
        VALUE(ro_response) TYPE REF TO if_http_response
      RAISING
        cx_demo_exception .
    CLASS-METHODS get_json_zip_file_list
      IMPORTING
        !iv_folder         TYPE string
        !iv_sub_folder     TYPE flag DEFAULT abap_true
      RETURNING
        VALUE(rt_filename) TYPE stringtab
      RAISING
        cx_demo_exception .
    CLASS-METHODS get_table_name
      IMPORTING
        !iv_filename    TYPE clike
      RETURNING
        VALUE(rv_table) TYPE tabname .
    CLASS-METHODS file_upload
      IMPORTING
        !iv_filename   TYPE string
      EXPORTING
        !ev_xstring    TYPE xstring
        !ev_error_text TYPE char255 .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abap2json_mini IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP2JSON_MINI=>FILE_UPLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILENAME                    TYPE        STRING
* | [<---] EV_XSTRING                     TYPE        XSTRING
* | [<---] EV_ERROR_TEXT                  TYPE        CHAR255
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD file_upload.
    DATA: lv_filelength TYPE i,
          lt_temptable  TYPE w3mimetabtype,
          lo_cx         TYPE REF TO cx_demo_exception.

    CLEAR: ev_xstring, ev_error_text.

    IF iv_filename CP 'http*'.
      TRY.
          ev_xstring = get_http_response( iv_filename )->get_data( ).
        CATCH cx_demo_exception INTO lo_cx.
          ev_error_text = lo_cx->exception_text.
      ENDTRY.
    ELSE.
      cl_gui_frontend_services=>gui_upload(
        EXPORTING
          filename                = iv_filename              " Name of file
          filetype                = 'BIN'              " File Type (ASCII, Binary)
        IMPORTING
          filelength              = lv_filelength         " File Length
        CHANGING
          data_tab                = lt_temptable           " Transfer table for file contents
        EXCEPTIONS
          file_open_error         = 1                  " File does not exist and cannot be opened
          file_read_error         = 2                  " Error when reading file
          no_batch                = 3                  " Cannot execute front-end function in background
          gui_refuse_filetransfer = 4                  " Incorrect front end or error on front end
          invalid_type            = 5                  " Incorrect parameter FILETYPE
          no_authority            = 6                  " No upload authorization
          unknown_error           = 7                  " Unknown error
          bad_data_format         = 8                  " Cannot Interpret Data in File
          header_not_allowed      = 9                  " Invalid header
          separator_not_allowed   = 10                 " Invalid separator
          header_too_long         = 11                 " Header information currently restricted to 1023 bytes
          unknown_dp_error        = 12                 " Error when calling data provider
          access_denied           = 13                 " Access to file denied.
          dp_out_of_memory        = 14                 " Not enough memory in data provider
          disk_full               = 15                 " Storage medium is full.
          dp_timeout              = 16                 " Data provider timeout
          not_supported_by_gui    = 17                 " GUI does not support this
          error_no_gui            = 18                 " GUI not available
          OTHERS                  = 19
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
        RETURN.
      ENDIF.

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_filelength
        IMPORTING
          buffer       = ev_xstring
        TABLES
          binary_tab   = lt_temptable
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP2JSON_MINI=>GET_HTTP_RESPONSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_URL                         TYPE        STRING
* | [<-()] RO_RESPONSE                    TYPE REF TO IF_HTTP_RESPONSE
* | [!CX!] CX_DEMO_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_http_response.
    DATA: lo_http    TYPE REF TO if_http_client,
          lv_message TYPE string.

    cl_http_client=>create_by_url(
      EXPORTING
        url                    = iv_url
        do_not_use_client_cert = abap_true
      IMPORTING
        client                 = lo_http
      EXCEPTIONS
        OTHERS                 = 1
    ).
    CHECK: sy-subrc EQ 0.

    lo_http->send(
      EXCEPTIONS
        OTHERS                 = 1
    ).
    IF sy-subrc <> 0.
      lo_http->get_last_error(
        IMPORTING
          message        = lv_message
      ).
      RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = CONV #( lv_message ).
    ENDIF.
    CHECK: sy-subrc EQ 0.

    lo_http->receive(
      EXCEPTIONS
        OTHERS                 = 1
    ).
    IF sy-subrc <> 0.
      lo_http->get_last_error(
        IMPORTING
          message        = lv_message
      ).
      RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = CONV #( lv_message ).
    ENDIF.
    CHECK: sy-subrc EQ 0.

    ro_response = lo_http->response.
    IF ro_response IS INITIAL.
      RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = CONV #( iv_url ).
    ENDIF.

*    rv_response = lo_http->response->get_cdata( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP2JSON_MINI=>GET_JSON_ZIP_FILE_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FOLDER                      TYPE        STRING
* | [--->] IV_SUB_FOLDER                  TYPE        FLAG (default =ABAP_TRUE)
* | [<-()] RT_FILENAME                    TYPE        STRINGTAB
* | [!CX!] CX_DEMO_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_json_zip_file_list.
    CONSTANTS: lc_github_url  TYPE string VALUE 'https://github.com/',
               lc_rawfile_url TYPE string VALUE 'https://raw.githubusercontent.com/'.
    DATA: lv_base_url       TYPE string,
          lv_html           TYPE string,
          lv_regex          TYPE string,
          lt_match_result   TYPE match_result_tab,
          ls_match_result   TYPE match_result,
          lv_filename       TYPE string,
          lv_file_separator TYPE c,
          lt_file_info      TYPE TABLE OF file_info,
          ls_file_info      TYPE file_info,
          lv_count          TYPE i,
          lv_error_text     TYPE text255.



    IF iv_folder CP 'http*'.

      lv_base_url = iv_folder.
      IF lv_base_url CP 'https://github.com/*'.
        " github.com
        IF lv_base_url NS '/tree/'.
          lv_base_url = lv_base_url && '/tree/main/'.
        ENDIF.

        lv_html = get_http_response( lv_base_url )->get_cdata( ).
        CHECK: lv_html IS NOT INITIAL.
        REPLACE lc_github_url IN lv_base_url WITH ''.

        " file
        lv_regex = lv_base_url && '[^"]*\.json\.zip'.
        REPLACE '/tree/' IN lv_regex WITH '/blob/'.
        FIND ALL OCCURRENCES OF REGEX lv_regex IN lv_html RESULTS lt_match_result.
        LOOP AT lt_match_result INTO ls_match_result.
          lv_filename = lc_rawfile_url && lv_html+ls_match_result-offset(ls_match_result-length).
          REPLACE '/blob/' IN lv_filename WITH '/'.
          APPEND lv_filename TO rt_filename.
        ENDLOOP.

        " folder
        IF iv_sub_folder EQ abap_true.
          lv_regex = lv_base_url && '[^"]*'.
          FIND ALL OCCURRENCES OF REGEX lv_regex IN lv_html RESULTS lt_match_result.
          LOOP AT lt_match_result INTO ls_match_result.
            lv_filename = lc_github_url && lv_html+ls_match_result-offset(ls_match_result-length).
            APPEND LINES OF get_json_zip_file_list( lv_filename ) TO rt_filename.
          ENDLOOP.
        ENDIF.

      ELSE.
        " others
        IF lv_base_url CS '.json'.
          APPEND lv_base_url TO rt_filename.
        ENDIF.
      ENDIF.

    ELSE.

      CHECK: iv_folder IS NOT INITIAL.

      cl_gui_frontend_services=>get_file_separator(
        CHANGING
          file_separator       = lv_file_separator
        EXCEPTIONS
          not_supported_by_gui = 1
          error_no_gui         = 2
          cntl_error           = 3
          OTHERS               = 4
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_error_text.
        RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = lv_error_text.
      ENDIF.

      " file
      CLEAR: lt_file_info, lv_count.
      cl_gui_frontend_services=>directory_list_files(
        EXPORTING
          directory                   = iv_folder        " Directory To Search
          filter                      = '*.json.zip'            " File filter
          files_only                  = abap_true       " Return only Files, no Directories
        CHANGING
          file_table                  = lt_file_info       " Return Table for the Found Files
          count                       = lv_count            " Number of Files/Dir Found
        EXCEPTIONS
          cntl_error                  = 1                " Control error
          directory_list_files_failed = 2                " Could not list files in the directory
          wrong_parameter             = 3                " Incorrect parameter combination
          error_no_gui                = 4                " No GUI available
          not_supported_by_gui        = 5                " GUI does not support this
          OTHERS                      = 6
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_error_text.
        RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = lv_error_text.
      ENDIF.

      LOOP AT lt_file_info INTO ls_file_info.
        lv_filename = iv_folder && lv_file_separator && ls_file_info-filename.
        APPEND lv_filename TO rt_filename.
      ENDLOOP.

      " folder
      IF iv_sub_folder EQ abap_true.
        CLEAR: lt_file_info, lv_count.
        cl_gui_frontend_services=>directory_list_files(
          EXPORTING
            directory                   = iv_folder        " Directory To Search
            directories_only            = abap_true " Return only Directories, no Files
          CHANGING
            file_table                  = lt_file_info       " Return Table for the Found Files
            count                       = lv_count            " Number of Files/Dir Found
          EXCEPTIONS
            cntl_error                  = 1                " Control error
            directory_list_files_failed = 2                " Could not list files in the directory
            wrong_parameter             = 3                " Incorrect parameter combination
            error_no_gui                = 4                " No GUI available
            not_supported_by_gui        = 5                " GUI does not support this
            OTHERS                      = 6
        ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_error_text.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = lv_error_text.
        ENDIF.

        LOOP AT lt_file_info INTO ls_file_info.
          lv_filename = iv_folder && lv_file_separator && ls_file_info-filename.
          APPEND LINES OF get_json_zip_file_list( lv_filename ) TO rt_filename.
        ENDLOOP.
      ENDIF.

      IF rt_filename IS INITIAL AND iv_folder CS '.json'.
        " if iv_folder is file (not folder), return it.
        IF cl_gui_frontend_services=>file_exist( iv_folder ) EQ abap_true.
          APPEND iv_folder TO rt_filename.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP2JSON_MINI=>GET_TABLE_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILENAME                    TYPE        CLIKE
* | [<-()] RV_TABLE                       TYPE        TABNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_table_name.
    DATA: lv_name   TYPE string,
          lv_offset TYPE i.

    " reemove before /
    FIND ALL OCCURRENCES OF REGEX '[\\/]' IN iv_filename MATCH OFFSET lv_offset.
    IF sy-subrc EQ 0.
      lv_offset = lv_offset + 1.
    ENDIF.
    lv_name = iv_filename+lv_offset.

    " remove after .
    FIND '.' IN lv_name MATCH OFFSET lv_offset.
    rv_table = lv_name(lv_offset).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP2JSON_MINI=>IMPORT_JSON_ZIP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FOLDER                      TYPE        CLIKE(optional)
* | [--->] IV_DEL                         TYPE        FLAG(optional)
* | [--->] IV_SIMULATE                    TYPE        FLAG(optional)
* | [--->] IV_SHOW_PROGRESS_BAR           TYPE        FLAG(optional)
* | [--->] IV_SHOW_CONFIRM                TYPE        FLAG(optional)
* | [<---] ET_LOG                         TYPE        TT_LOG
* | [<---] EV_ERROR_TEXT                  TYPE        CHAR255
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD import_json_zip.
    DATA: lv_folder       TYPE string,
          lt_filename     TYPE TABLE OF string,
          lv_filename     TYPE string,
          lv_xstring      TYPE xstring,
          lt_table        TYPE TABLE OF tabname,
          lv_table        TYPE tabname,
          lt_confirmtable TYPE sesf_string_tab,
          lv_count        TYPE i,
          lv_total        TYPE i,
          lv_index        TYPE i,
          lv_answer       TYPE c,
          ltr_data        TYPE REF TO data,
          lo_cx           TYPE REF TO cx_demo_exception.
    FIELD-SYMBOLS: <lt_data> TYPE table.

    CLEAR: et_log, ev_error_text.

    TRY.

        IF iv_folder IS NOT INITIAL.
          lv_folder = iv_folder.
        ELSE.
          cl_gui_frontend_services=>directory_browse(
            CHANGING
              selected_folder      = lv_folder
            EXCEPTIONS
              cntl_error           = 1               " Control error
              error_no_gui         = 2               " No GUI available
              not_supported_by_gui = 3               " GUI does not support this
              OTHERS               = 4
          ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.
        ENDIF.

        CHECK: lv_folder IS NOT INITIAL.
        lt_filename = get_json_zip_file_list( lv_folder ).

        IF lt_filename IS INITIAL.
          ev_error_text = '*.josn.zip does not exist'.
          RETURN.
        ENDIF.
        SORT lt_filename.

        LOOP AT lt_filename INTO lv_filename.
          APPEND get_table_name( lv_filename ) TO lt_table.
        ENDLOOP.
        SORT lt_table.
        DELETE ADJACENT DUPLICATES FROM lt_table.

        IF iv_show_confirm EQ abap_true.
          MOVE-CORRESPONDING lt_table TO lt_confirmtable.
          CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_TABLE'
            EXPORTING
              titlebar        = 'Confirm'
*             start_column    = 25
*             start_row       = 6
*             end_column      = 90
*             end_row         = 20
              columnname      = 'Table'
            IMPORTING
              answer          = lv_answer
            CHANGING
              ct_displaytable = lt_confirmtable.
          IF lv_answer <> 'J'.
            RETURN.
          ENDIF.
        ENDIF.

        IF iv_del EQ abap_true.
          LOOP AT lt_table INTO lv_table.
            DELETE FROM (lv_table).
            APPEND VALUE #( table = lv_table text = 'deleted' count = sy-dbcnt ) TO et_log.
          ENDLOOP.
        ENDIF.

        lv_total = lines( lt_filename ).
        LOOP AT lt_filename INTO lv_filename.
          CLEAR: lv_table.

          lv_index = sy-tabix.

          IF iv_show_progress_bar EQ abap_true.
            CALL FUNCTION 'PROGRESS_INDICATOR'
              EXPORTING
                i_text               = |{ CONV f( 100 * lv_index / lv_total ) DECIMALS = 2 }% { lv_table }|
                i_output_immediately = abap_true
                i_processed          = lv_index
                i_total              = lv_total.
          ENDIF.

          file_upload(
            EXPORTING
              iv_filename  = lv_filename
            IMPORTING
              ev_xstring    = lv_xstring
              ev_error_text = ev_error_text
          ).
          IF ev_error_text IS NOT INITIAL.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.

          lv_table = get_table_name( lv_filename ).

          CREATE DATA ltr_data TYPE TABLE OF (lv_table).
          ASSIGN ltr_data->* TO <lt_data>.
          json2abap(
            EXPORTING
              iv_json_zip = lv_xstring
            IMPORTING
              et_data     = <lt_data>
          ).
          MODIFY (lv_table) FROM TABLE <lt_data>.
          lv_count = lines( <lt_data> ).

          APPEND VALUE #( table = lv_table file = lv_filename count = lv_count ) TO et_log.
        ENDLOOP.


        IF iv_simulate EQ abap_false.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.

      CATCH cx_demo_exception INTO lo_cx.
        ev_error_text = lo_cx->exception_text.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP2JSON_MINI=>JSON2ABAP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON_ZIP                    TYPE        XSTRING
* | [<---] ET_DATA                        TYPE        DATA
* | [<---] EV_WHERE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD json2abap.
    DATA: lv_xstring TYPE xstring,
          lv_json    TYPE string,
          lo_zip     TYPE REF TO cl_abap_zip,
          ls_file    TYPE cl_abap_zip=>t_file,
          lv_index   TYPE i.
    CLEAR: et_data, ev_where.

    CREATE OBJECT lo_zip.
    lo_zip->load(
      EXPORTING
        zip             = iv_json_zip
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2
    ).
*    CHECK: sy-subrc EQ 0.
    IF sy-subrc <> 0 OR lo_zip->files IS INITIAL.
      cl_abap_conv_in_ce=>create( encoding = gc_encoding input = iv_json_zip )->read(
        IMPORTING
          data = lv_json
      ).

      /ui2/cl_json=>deserialize(
        EXPORTING
          json             = lv_json
        CHANGING
          data             = et_data
      ).
      RETURN.
    ENDIF.

    IF ev_where IS REQUESTED.
      lo_zip->get(
        EXPORTING
          name                    = 'SQL_WHERE.txt'
        IMPORTING
          content                 = lv_xstring
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3
      ).
      IF sy-subrc EQ 0.
        cl_abap_conv_in_ce=>create( encoding = gc_encoding input = lv_xstring )->read(
          IMPORTING
            data = ev_where
        ).
      ENDIF.
    ENDIF.

    LOOP AT lo_zip->files INTO ls_file WHERE name CP '*.json'.
      lv_index = sy-tabix.
      EXIT.
    ENDLOOP.
    CHECK: lv_index IS NOT INITIAL.

    lo_zip->get(
      EXPORTING
        index                   = lv_index
      IMPORTING
        content                 = lv_xstring
      EXCEPTIONS
        zip_index_error         = 1
        zip_decompression_error = 2
        OTHERS                  = 3
    ).
    CHECK: sy-subrc EQ 0.

    cl_abap_conv_in_ce=>create( encoding = gc_encoding input = lv_xstring )->read(
      IMPORTING
        data = lv_json
    ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = lv_json
      CHANGING
        data             = et_data
    ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
* form
**********************************************************************
FORM f4_folder.
  DATA: lt_df TYPE TABLE OF dynpread,
        ls_df TYPE dynpread.

  ls_df-fieldname = 'P_FOLDER'.
  APPEND ls_df TO lt_df.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_df.
  READ TABLE lt_df INTO ls_df INDEX 1.
  p_folder = ls_df-fieldvalue.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      initial_folder       = p_folder
    CHANGING
      selected_folder      = p_folder " Folder Selected By User
    EXCEPTIONS
      cntl_error           = 1               " Control error
      error_no_gui         = 2               " No GUI available
      not_supported_by_gui = 3               " GUI does not support this
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
FORM execute.
  DATA: lt_log        TYPE zcl_abap2json_mini=>tt_log,
        lv_error_text TYPE text255.


  zcl_abap2json_mini=>import_json_zip(
    EXPORTING
      iv_folder            = p_folder
      iv_show_progress_bar = abap_true
      iv_show_confirm      = abap_true
    IMPORTING
      et_log               = lt_log
      ev_error_text        = lv_error_text
  ).

  IF lv_error_text IS NOT INITIAL.
    MESSAGE lv_error_text TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF lt_log IS NOT INITIAL.
    cl_demo_output=>display( lt_log ).
  ENDIF.

ENDFORM.
