

SPAN {
font-family: "Courier New";
font-size: 10pt;
color: #000000;
background: #FFFFFF;
}
.L0S31 {
font-style: italic;
color: #808080;
}
.L0S32 {
color: #3399FF;
}
.L0S33 {
color: #4DA619;
}
.L0S52 {
color: #0000FF;
}
.L0S55 {
color: #800080;
}
.L0S70 {
color: #808080;
}

REPORT y8test71.



*----------------------------------------------------------------------*

*  TABLES - DATABASES

*----------------------------------------------------------------------*



TABLES: rs38m, tadir, trdir, dd01t, yappl_prog,

        trdire, t100, tlibv, enlfdir, usr02,

        dd02l, dd03l, dd04t, v_fdir, tfdir, tftit, d010inc, dd02t.



*-----------------------------------------------------------------------

*  TYPES

*-----------------------------------------------------------------------

*----- Text element structure

TYPES: t_texttab LIKE textpool.



*--- Message classes



TYPES: BEGIN OF t_messages,

         msgid LIKE trdire-msgid,

         msgnr LIKE t100-msgnr,

         text  LIKE t100-text,

       END OF t_messages.



*--- screen flow.

TYPES: BEGIN OF t_screen_flow,

         screen LIKE d020s-dnum,

         code   LIKE d022s-line,

       END OF t_screen_flow.



*--- Data dictionary objects - tables, structures.

TYPES: BEGIN OF t_dict_struct,

         tabname   LIKE dd03l-tabname,

         tabtext   LIKE dd02t-ddtext,

         fieldname TYPE char100,

         position  LIKE dd03l-position,

         keyflag   LIKE dd03l-keyflag,

         rollname  TYPE char100, "LIKE dd03l-rollname,

         domname   LIKE dd03l-domname,

         datatype  LIKE dd03l-datatype,

         leng      LIKE dd03l-leng,

         ddtext    LIKE dd04t-ddtext,

       END OF t_dict_struct.



*--- Function Modules

TYPES: BEGIN OF t_functions,

         funcname LIKE tfdir-funcname,

         include  LIKE tfdir-include,

         pname    LIKE tfdir-pname,

         stext    LIKE tftit-stext,

       END OF t_functions.



*--- Include program names

TYPES: BEGIN OF t_includes,

         prog LIKE trdir-name,

         text(255),

       END OF t_includes.



*----- ABAP program list

TYPES: BEGIN OF t_programmes,

         devclass        TYPE devclass,

         prog            LIKE trdir-name,

         text(255),

         subc            LIKE yappl_prog-subc,

         functions       TYPE t_functions  OCCURS 0,

       END OF t_programmes.



*-ALL related Declarations

DATA:

  t_header    TYPE STANDARD TABLE OF w3head   WITH HEADER LINE,   "Header

  t_fields    TYPE STANDARD TABLE OF w3fields WITH HEADER LINE,   "Fields

  t_html      TYPE STANDARD TABLE OF w3html,                      "Html

  wa_header   TYPE w3head,

  w_head      TYPE w3head.



TYPES : BEGIN OF ty_ht_all,

          tabname TYPE char50,

          line    TYPE w3_html,

        END OF ty_ht_all.



DATA  t_html_all  TYPE STANDARD TABLE OF ty_ht_all WITH HEADER LINE.

FIELD-SYMBOLS <fs_html_all> TYPE ty_ht_all.



FIELD-SYMBOLS <fs_html> TYPE w3html.



*----------------------------------------------------------------------*

*  DATA - INTERNAL TABLES

*----------------------------------------------------------------------*

*---- Program texts - declaration only not used

DATA : i_texttab         TYPE t_texttab  OCCURS 0 WITH HEADER LINE.

DATA : i_messages        TYPE t_messages OCCURS 0 WITH HEADER LINE.

DATA : i_screen_flow     TYPE t_screen_flow.

DATA : lt_includes_all   TYPE seop_methods_w_include.



FIELD-SYMBOLS <fs_includes_all> TYPE seop_method_w_include.



*----- Program content for text download

DATA: BEGIN OF content OCCURS 0,

        line(255),

      END OF content.



*--- Programme texts.

DATA: i_programme_texts TYPE t_texttab OCCURS 0 WITH HEADER LINE.



*--- dictionary object

DATA: i_dictionary TYPE t_dict_struct OCCURS 0 WITH HEADER LINE.



*--- Allows HTML routines to create an HTML without the table name on

*      each line.

DATA: BEGIN OF i_dict_minus_tabname OCCURS 0,

        fieldname TYPE char100,

        position  LIKE dd03l-position,

        keyflag   LIKE dd03l-keyflag,

        rollname  TYPE char100, "dd03l-rollname,

        domname   LIKE dd03l-domname,

        datatype  LIKE dd03l-datatype,

        leng      LIKE dd03l-leng,

        ddtext    LIKE dd04t-ddtext,

      END OF i_dict_minus_tabname.



*--- Table names of customer tables, used for searching for tables

DATA: BEGIN OF table_names OCCURS 0,

        tabname   LIKE i_dictionary-tabname,

        tabtext   LIKE dd02t-ddtext,

      END OF table_names.



DATA: BEGIN OF old_table_names OCCURS 0,

        tabname   LIKE i_dictionary-tabname,

        tabtext   LIKE dd02t-ddtext,

END OF old_table_names.



DATA: BEGIN OF new_table_names OCCURS 0,

        tabname   LIKE i_dictionary-tabname,

        tabtext   LIKE dd02t-ddtext,

END OF new_table_names.



*--- Function Modules.

DATA: i_functions   TYPE t_functions OCCURS 0 WITH HEADER LINE.

DATA: i_functions_2 TYPE t_functions OCCURS 0 WITH HEADER LINE.



*--- Customer function names, used for searching for functions

DATA: BEGIN OF function_names OCCURS 0,

        funcname        LIKE i_functions-funcname,

      END OF function_names.



DATA: BEGIN OF i_programmes OCCURS 0,

        devclass        LIKE yappl_prog-devclass,

        prog            LIKE trdir-name,

        text(255),

        subc            LIKE yappl_prog-subc,

        messages        TYPE t_messages OCCURS 0,

        text_elements   TYPE t_texttab OCCURS 0,

        selection_texts TYPE t_texttab OCCURS 0,

        screen_flow     TYPE t_screen_flow OCCURS 0,

        includes        TYPE t_includes OCCURS 0,

        functions       TYPE t_functions OCCURS 0,

        dict_struct     TYPE t_dict_struct OCCURS 0,

      END OF i_programmes.



*--- Names of function modules used within programmes

DATA: i_prog_includes TYPE t_includes OCCURS 0 WITH HEADER LINE.



*--- Includes to download

DATA: i_includes LIKE i_programmes OCCURS 0 WITH HEADER LINE.



*--- Tree display structure.

DATA: i_node LIKE snodetext OCCURS 0 WITH HEADER LINE.



*--- Temp table of downloaded objects.

DATA: BEGIN OF i_downloaded OCCURS 0,

        object(30),

      END OF i_downloaded.

*----------------------------------------------------------------------*

*  DATA - WORKING FIELDS

*----------------------------------------------------------------------*

DATA: footer_message LIKE content-line.

DATA: mess(100).

DATA: temp_func_name LIKE i_functions-funcname.

DATA: forced_exit TYPE i VALUE 0.

DATA: start_time LIKE sy-uzeit.

DATA: run_time LIKE sy-uzeit.

DATA: run_time_char(8).

DATA: l_upd      TYPE rs_bool.



RANGES: s_prog   FOR trdir-name.

RANGES: s_dev    FOR tadir-devclass.

RANGES: s_auth   FOR usr02-bname.

RANGES: s_table  FOR dd02l-tabname.

RANGES: s_fname  FOR tfdir-funcname.

RANGES: s_fgroup FOR enlfdir-area.



*-----------------------------------------------------------------------

*  CONSTANTS

*-----------------------------------------------------------------------

CONSTANTS: c_tables(7)          VALUE 'TABLES '.

CONSTANTS: c_like(5)            VALUE 'LIKE '.

CONSTANTS: c_type(5)            VALUE 'TYPE '.

CONSTANTS: c_from(5)            VALUE 'FROM '.

CONSTANTS: c_from_db(14)        VALUE 'FROM DATABASE '.

CONSTANTS: c_to_db(12)          VALUE 'TO DATABASE '.

CONSTANTS: c_structure(10)      VALUE 'STRUCTURE '.

CONSTANTS: c_perform(8)         VALUE 'PERFORM '.

CONSTANTS: c_form(5)            VALUE 'FORM '.

CONSTANTS: c_comma(1)           VALUE ','.

CONSTANTS: c_period(1)          VALUE '.'.

*-----------------------------------------------------------------------

*  SELECTION SCREEN

*-----------------------------------------------------------------------

*--- Author

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE t_b1.

SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 5(23) t_auth.

SELECT-OPTIONS p_auth FOR usr02-bname.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 5(36) t_pmod.

PARAMETERS: p_mod AS CHECKBOX.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b1.



SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t_b2.

*--- Tables

SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS: r_table RADIOBUTTON GROUP r1.

SELECTION-SCREEN COMMENT 5(20) t_rtable.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 10(18) t_ptable.

SELECT-OPTIONS : p_table FOR dd02l-tabname.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 10(69) t_tnote.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 14(61) t_tnote1.

SELECTION-SCREEN END OF LINE.



*--- Function Modules

SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS: r_func RADIOBUTTON GROUP r1.

SELECTION-SCREEN COMMENT 5(30) t_rfunc.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 10(18) t_pfname.

SELECT-OPTIONS : p_fname FOR tfdir-funcname.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 10(18) t_fgroup.

SELECT-OPTIONS : p_fgroup FOR enlfdir-area.

SELECTION-SCREEN END OF LINE.



*--- Programs / Includes

SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS: r_prog RADIOBUTTON GROUP r1.

SELECTION-SCREEN COMMENT 5(18) t_rprog.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 10(18) t_rpname.



SELECT-OPTIONS p_prog FOR trdir-name.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 10(18) t_sdev.

SELECT-OPTIONS : p_dev FOR tadir-devclass.

SELECTION-SCREEN END OF LINE.



*--- Local objects

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(27) t_$tmp.

PARAMETERS: p_$tmp AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN: END OF BLOCK b2.



*-----  Additional things to download.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE t_b3.

SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(30) t_ptext.

PARAMETERS: p_text AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(30) t_pmes.

PARAMETERS: p_mes AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(30) t_pinc.

PARAMETERS: p_inc AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN COMMENT 40(20) t_recu.

PARAMETERS: p_reci AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(30) t_pfunc.

PARAMETERS: p_func AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN COMMENT 40(20) t_recf.

PARAMETERS: p_recf AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(30) t_doc.

PARAMETERS: p_doc AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(30) t_pscr.

PARAMETERS: p_scr AS CHECKBOX.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(30) t_pdict.

PARAMETERS: p_dict AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN: END OF BLOCK b3.



*-----  File details

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE t_b4.

SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(20) t_phtml.

PARAMETERS: p_html RADIOBUTTON GROUP g1 DEFAULT 'X'.

SELECTION-SCREEN COMMENT 30(20) t_phext.

PARAMETERS: p_hex(4) TYPE c DEFAULT 'html' LOWER CASE.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(20) t_ptxt.

PARAMETERS: p_txt RADIOBUTTON GROUP g1.

SELECTION-SCREEN COMMENT 30(20) t_pext.

PARAMETERS: p_tex(4) TYPE c DEFAULT 'txt' LOWER CASE.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN SKIP.



SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(20) t_ppath.

PARAMETERS: p_path LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\temp\DOWN'.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b4.



*-----------------------------------------------------------------------

*   INITIALIZATION

*-----------------------------------------------------------------------

INITIALIZATION.

*--- parameter screen texts.

  t_b1     = 'Author (Optional)'.

  t_b2     = 'Objects to download'.

  t_b3     = 'Additional downloads'.

  t_b4     = 'Download parameters'.

  t_auth   = 'Author name'.

  t_pmod   = 'Include programs modified by author'.

  t_rtable = 'Tables'.

  t_ptable = 'Table name'.

  t_tnote  = 'Please note: tables are stored under the username of'.

  t_tnote1 = '             the last person who modified them.'.

  t_rfunc  = 'Function modules'.

  t_pfname = 'Function name'.

  t_fgroup = 'Function group'.

  t_rprog  = 'Programs'.

  t_rpname = 'Program name'.

  t_sdev   = 'Development class'.

  t_ptxt   = 'Text document'.

  t_phtml  = 'HTML document'.

  t_ptext  = 'Text elements'.

  t_pinc   = 'Include programs'.

  t_recu   = 'Recursive search'.

  t_phext  = 'File extension'.

  t_pext   = 'File extension'.

  t_ppath  = 'File path'.

  t_pmes   = 'Message classes'.

  t_pfunc  = 'Function modules'.

  t_doc    = 'Function module documentation'.

  t_recf   = 'Recursive search'.

  t_pscr   = 'Screens'.

  t_pdict  = 'Dictionary structures'.

  t_$tmp   = 'Include local objects'.



*-----------------------------------------------------------------------

*  START-OF-SELECTION.

*-----------------------------------------------------------------------

START-OF-SELECTION.



  PERFORM check_combo_boxes.

  PERFORM fill_ranges.



  start_time = sy-uzeit.



  TRANSLATE p_hex TO LOWER CASE.

  TRANSLATE p_tex TO LOWER CASE.



*--- Main program flow.

  CASE 'X'.

*--- Select tables

    WHEN r_table.



* this sets up the tables but downlads later

      PERFORM retrieve_tables TABLES i_dictionary

                                     table_names

                                     s_table.



    WHEN r_func.

*--- Select function modules

      PERFORM retrieve_functions TABLES s_fname

                                        s_fgroup

                                        i_programmes

                                        i_functions

                                 USING 1.



      LOOP AT i_functions.

        PERFORM func_include_name USING i_functions-pname

                                        i_functions-include

                                        temp_func_name

                                        0.



        PERFORM find_include_programs USING temp_func_name.



        PERFORM find_custom_functions TABLES i_functions

                                      USING temp_func_name.

      ENDLOOP.



      SORT i_prog_includes ASCENDING BY prog.

      DELETE ADJACENT DUPLICATES FROM i_prog_includes COMPARING prog.



      PERFORM retrieve_functions TABLES s_fname

                                        s_fgroup

                                        i_functions

                                        i_functions_2

                                 USING 0.



      i_functions[] = i_functions_2[].



*--- Select programs

    WHEN r_prog.

      mess = 'Processing Please Wait...'.



      PERFORM display_status USING mess 0.

      PERFORM retrieve_programs TABLES i_programmes

                                       s_prog

                                       s_dev

                                       s_auth.

  ENDCASE.



*-----------------------------------------------------------------------

** END-OF-SELECTION

*-----------------------------------------------------------------------

END-OF-SELECTION.



  IF forced_exit = 0.



    CASE 'X'.

      WHEN r_table.

        IF NOT ( i_dictionary[] IS INITIAL ).

          PERFORM download_dd_structures TABLES i_dictionary

                                         USING p_path.

*        PERFORM fill_tree_node_tables TABLES i_dictionary.

        ENDIF.



      WHEN r_func.

        IF NOT ( i_functions[] IS INITIAL ).



          PERFORM download_functions TABLES i_functions

                                     USING p_path.



*         PERFORM fill_tree_node_functions TABLES i_functions.

        ENDIF.



      WHEN r_prog.

        IF NOT ( i_programmes[] IS INITIAL ).



          PERFORM download_programs TABLES i_programmes

                                    USING p_path.



*         PERFORM fill_tree_node_programs TABLES i_programmes.

        ENDIF.

    ENDCASE.



    IF NOT ( i_node[] IS INITIAL ).

      PERFORM display_tree TABLES i_node.

    ELSE.

      mess = 'No Items Found Matching Selection Criteria'.

      PERFORM display_status USING mess 2.

    ENDIF.



  ENDIF.



************************************************************************

*****************************SUBROUTINES********************************

************************************************************************



*-----------------------------------------------------------------------

*  CHECK_COMBO_BOXES...

*-----------------------------------------------------------------------

FORM check_combo_boxes.



  IF p_auth IS INITIAL.

    CASE 'X'.

      WHEN r_table.

        IF p_table IS INITIAL.

          mess = 'You Must Enter A Table Name Or Author'.

        ENDIF.

      WHEN r_func.

        IF ( p_fname IS INITIAL AND p_fgroup IS INITIAL ).

          CONCATENATE 'You Must Enter A Function Name,'

                      'Function Group Or Author'

                      INTO mess SEPARATED BY space.

        ENDIF.



      WHEN r_prog.

        IF p_prog IS INITIAL.

          CONCATENATE 'You Must Enter A Program Name'

                      'Development Class Or Author'

                      INTO mess SEPARATED BY space.

        ENDIF.

    ENDCASE.

  ELSE.

    IF r_func = 'X'.

      IF ( ( p_auth <> '' ) AND

         ( ( p_fname <> '' ) OR ( p_fgroup <> '' ) ) ).

        CONCATENATE 'You Cannnot Enter An Author As Well As'

                    'A Func Name Or Func Group'

                    INTO mess SEPARATED BY space.

      ENDIF.

    ENDIF.

  ENDIF.



  IF NOT mess IS INITIAL.

    PERFORM display_status USING mess 3.

    forced_exit = 1.

    STOP.

  ENDIF.



ENDFORM.                    " CHECK_COMBO_BOXES



*-----------------------------------------------------------------------

* FILL_RANGES...      for selection routines

*-----------------------------------------------------------------------

FORM fill_ranges.



  IF NOT p_auth IS INITIAL.

    s_auth[] = p_auth[].

  ENDIF.



  IF NOT p_table IS INITIAL.

    s_table[] = p_table[].

  ENDIF.



  IF NOT p_fname IS INITIAL.

    s_fname[] = p_fname[].

  ENDIF.



  IF NOT p_fgroup IS INITIAL.

    s_fgroup[] = p_fgroup[].

  ENDIF.



  IF NOT p_prog IS INITIAL.

    s_prog[] = p_prog[].

  ENDIF.



  IF NOT p_dev IS INITIAL.

    s_dev[] = p_dev[].

  ENDIF.



  IF p_$tmp IS INITIAL.

    s_dev-sign   = 'E'.

    s_dev-option = 'EQ'.

    s_dev-low    = '$TMP'.

    APPEND s_dev.

  ENDIF.



ENDFORM.                    "FILL_RANGES



*-----------------------------------------------------------------------

*  FIND_TABLES...             Search for tables in dictionary

*-----------------------------------------------------------------------

FORM retrieve_tables TABLES i_dictionary STRUCTURE i_dictionary

                            table_names STRUCTURE table_names

                            range_table STRUCTURE s_table.



* Get Tables

  SELECT tabname FROM dd02l

                 INTO table_names-tabname

                 WHERE tabname IN range_table

                 AND as4user IN s_auth

                 AND as4local = 'A'.



    SELECT SINGLE ddtext FROM dd02t

                         INTO table_names-tabtext

                         WHERE tabname = table_names-tabname

                         AND ddlanguage = sy-langu

                         AND as4local = 'A'.



    APPEND table_names.

  ENDSELECT.



* Get DD Objects

  SELECT rollname FROM dd04l

                  INTO table_names-tabname

                  WHERE rollname IN range_table

                  AND as4user IN s_auth

                  AND as4local = 'A'.



    SELECT SINGLE ddtext FROM dd04t

                         INTO table_names-tabtext

                         WHERE rollname = table_names-tabname

                         AND ddlanguage = sy-langu

                         AND as4local = 'A'.



    APPEND table_names.

  ENDSELECT.



  IF NOT ( table_names[] IS INITIAL ).



    REFRESH new_table_names.



    PERFORM find_table_definition TABLES i_dictionary

                                         table_names.



* Download any new DD objects

    IF NOT new_table_names[] IS INITIAL.



      PERFORM find_table_definition TABLES i_dictionary

                                           new_table_names.



    ENDIF.



  ENDIF.



ENDFORM.                    "RETRIEVE_TABLES



*-----------------------------------------------------------------------

*  find_table_definition... from sap database.

*-----------------------------------------------------------------------

FORM find_table_definition TABLES i_dict     STRUCTURE i_dictionary

                                  tablenames STRUCTURE table_names.



  DATA gotstate   LIKE dcobjif-gotstate.

  DATA dd02v_wa   LIKE dd02v.

  DATA dd09l_wa   LIKE dd09l.

  DATA definition LIKE dd03p OCCURS 0 WITH HEADER LINE.

  DATA dd01v_wa   TYPE dd01v.

  DATA dd07v_tab  TYPE TABLE OF dd07v WITH HEADER LINE.

  DATA dd04v_wa   LIKE dd04v.

  DATA tpara_wa   LIKE tpara.

  DATA l_namein   TYPE ddobjname.

  DATA lt_dd01v   TYPE TABLE OF dd01v.



  LOOP AT tablenames.



    l_namein = tablenames-tabname.

    TRANSLATE l_namein TO UPPER CASE.



* Check its a table

    CALL FUNCTION 'DDIF_TABL_GET'

      EXPORTING

        name          = tablenames-tabname

        state         = 'A'

        langu         = 'E'

      IMPORTING

        gotstate      = gotstate

        dd02v_wa      = dd02v_wa

        dd09l_wa      = dd09l_wa

      TABLES

        dd03p_tab     = definition

      EXCEPTIONS

        illegal_input = 1

        OTHERS        = 2.



    IF sy-subrc = 0

      AND gotstate = 'A'.



      LOOP AT definition.

        MOVE-CORRESPONDING definition TO i_dict.

        MOVE table_names-tabtext TO i_dict-tabtext.



        DATA l_len TYPE i.

        l_len = strlen( i_dict-rollname ).



        IF   i_dict-rollname(1) = 'Z'

          OR i_dict-rollname(1) = 'Y'

          OR i_dict-rollname(1) = 'z'

          OR i_dict-rollname(1) = 'y'



        OR ( l_len GE 5

          AND ( i_dict-rollname(5) = '/GLB/'

             OR i_dict-rollname(5) = '/glb/' ) ).



          APPEND i_dict-rollname TO new_table_names.





* Use odd characters as function module table to html removes <>

          CONCATENATE '¬a href="./Dictionary-'

                      i_dict-rollname

                      '.html"¬¬'

                      i_dict-rollname

                      '¬/a¬¬'

                 INTO i_dict-rollname.



        ENDIF.



        APPEND i_dict.

      ENDLOOP.



    ELSE.



* Check its a DD object

      CALL FUNCTION 'DDIF_DTEL_GET'

        EXPORTING

          name          = tablenames-tabname

          state         = 'A'

          langu         = sy-langu

        IMPORTING

          gotstate      = gotstate

          dd04v_wa      = dd04v_wa

          tpara_wa      = tpara_wa

        EXCEPTIONS

          illegal_input = 1

          OTHERS        = 2.



      IF sy-subrc = 0

        AND gotstate = 'A'.



        CLEAR i_dict.

        CLEAR dd01v_wa.



        i_dict-tabname   = tablenames-tabname.

        i_dict-fieldname = dd04v_wa-rollname.

        i_dict-rollname  = dd04v_wa-domname.



* Data Element has Y or Z DMN

        l_len = strlen( i_dict-rollname ).



        IF   i_dict-rollname(1) = 'Z'

          OR i_dict-rollname(1) = 'Y'

          OR i_dict-rollname(1) = 'z'

          OR i_dict-rollname(1) = 'y'



        OR ( l_len GE 5

          AND ( i_dict-rollname(5) = '/GLB/'

             OR i_dict-rollname(5) = '/glb/' ) ).



          l_namein  = i_dict-rollname.



          CALL FUNCTION 'DDIF_DOMA_GET'

            EXPORTING

              name          = l_namein

              state         = 'A'

              langu         = sy-langu

            IMPORTING

              gotstate      = gotstate

              dd01v_wa      = dd01v_wa

            TABLES

              dd07v_tab     = dd07v_tab

            EXCEPTIONS

              illegal_input = 1

              OTHERS        = 2.



          CONCATENATE '¬a href="./Dictionary-'

                      i_dict-rollname

                      '_DMN'

                      '.html"¬¬'

                      i_dict-rollname

                      '¬/a¬¬'

                 INTO i_dict-rollname.



        ENDIF.



        i_dict-keyflag   = ''.

        i_dict-tabtext   = dd04v_wa-scrtext_l.

        i_dict-datatype  = dd04v_wa-datatype.

        i_dict-leng      = dd04v_wa-leng.

        i_dict-ddtext    = dd04v_wa-scrtext_l.

        APPEND i_dict.



        IF NOT dd01v_wa IS INITIAL.

          CLEAR i_dict.



          CONCATENATE dd01v_wa-domname '_DMN' INTO i_dict-tabname.



          PERFORM add_tab TABLES dd07v_tab

                          USING  'DD07V'

                                 i_dict-tabname.



          i_dict-fieldname = dd01v_wa-domname.

          i_dict-keyflag   = ''.

          i_dict-datatype  = dd01v_wa-datatype.

          i_dict-leng      = dd01v_wa-leng.

          i_dict-ddtext    = dd01v_wa-ddtext.

          i_dict-tabtext   = dd01v_wa-ddtext .



          APPEND i_dict.



        ENDIF.



      ENDIF.



    ENDIF.



  ENDLOOP.



ENDFORM.                    "FIND_TABLE_DEFINITION



*-----------------------------------------------------------------------

*  RETRIEVE_FUNCTIONS...   Retrieve function modules from SAP DB

*-----------------------------------------------------------------------

FORM retrieve_functions TABLES s_fname STRUCTURE s_fname

                               s_fgroup STRUCTURE s_fgroup

                               func_names STRUCTURE i_functions

                               found_func STRUCTURE i_functions

                        USING main_scan.



  RANGES: sel_fname  FOR tfdir-funcname.

  RANGES: sel_fgroup FOR enlfdir-area.



  sel_fname[]  = s_fname[].

  sel_fgroup[] = s_fgroup[].



  IF  main_scan = 1.

    IF NOT p_auth IS INITIAL.

*---  select all function groups by author

      SELECT area FROM tlibv INTO sel_fgroup-low

                       WHERE uname = p_auth.



        sel_fgroup-sign   = 'I'.

        sel_fgroup-option = 'EQ'.

        APPEND sel_fgroup.

      ENDSELECT.

    ENDIF.



*--- Select by function name and/or function group.

    SELECT * FROM v_fdir

             WHERE funcname IN sel_fname

               AND area IN sel_fgroup

               AND generated = ''.



      SELECT SINGLE funcname

                    pname

                    include  FROM tfdir

                             INTO (found_func-funcname,

                                   found_func-pname,

                                   found_func-include)

                             WHERE funcname = v_fdir-funcname.



      SELECT SINGLE stext FROM tftit

                          INTO found_func-stext

                          WHERE spras = sy-langu

                            AND funcname = v_fdir-funcname.



      APPEND i_functions.

    ENDSELECT.

  ELSE.

    LOOP AT func_names.

      SELECT SINGLE funcname

                    pname

                    include  FROM tfdir

                             INTO (found_func-funcname,

                                   found_func-pname,

                                   found_func-include)

                             WHERE funcname = func_names-funcname.



      SELECT SINGLE stext FROM tftit

                          INTO found_func-stext

                          WHERE spras = sy-langu

                            AND funcname = func_names-funcname.



      APPEND found_func.

    ENDLOOP.

  ENDIF.



ENDFORM.                    "RETRIEVE_FUNCTIONS



*-----------------------------------------------------------------------

* RETRIEVE_PROGRAMS...    find programs and sub objects from SAP DB

*-----------------------------------------------------------------------

FORM retrieve_programs TABLES i_prog   STRUCTURE i_programmes

                              sel_prog STRUCTURE s_prog

                              sel_dev  STRUCTURE s_dev

                              sel_auth STRUCTURE s_auth.



  DATA : counter       TYPE i VALUE 1.

  DATA : wa_includes   TYPE t_includes.

  DATA : lt_tadiv      TYPE TABLE OF tadiv.

  DATA : l_clskey      TYPE seoclskey.

  DATA : lt_includes   TYPE seop_methods_w_include.

  DATA : l_includes    TYPE seop_method_w_include.



  FIELD-SYMBOLS <fs_tadiv> TYPE tadiv.



*----- Select by name, development class and author

  IF p_mod IS INITIAL.

    SELECT devclass prog subc FROM yappl_prog

                              INTO (i_prog-devclass,

                                    i_prog-prog,

                                    i_prog-subc)

                              WHERE prog     IN sel_prog

                                AND devclass IN sel_dev

                                AND cnam     IN sel_auth.

      "                                AND (    subc  = '1'

      "                                      OR subc  = 'M'

      "                                      OR subc  = 'S' ).



      APPEND i_prog.

    ENDSELECT.

  ELSE.

    SELECT devclass prog subc FROM yappl_prog

                              INTO (i_prog-devclass,

                                    i_prog-prog,

                                    i_prog-subc)

                              WHERE prog     IN sel_prog

                                AND devclass IN sel_dev

"                                AND ( subc     = '1'

"                                     OR subc   = 'S' )

                                AND ( cnam     IN sel_auth

                                 OR   unam     IN sel_auth ).

      APPEND i_prog.

    ENDSELECT.

  ENDIF.



* Add in Classes

  SELECT * FROM tadiv

    INTO CORRESPONDING FIELDS OF TABLE lt_tadiv

    WHERE obj_name IN sel_prog

      AND devclass IN sel_dev.



  IF NOT lt_tadiv[] IS INITIAL.



    LOOP AT lt_tadiv ASSIGNING <fs_tadiv>.



      REFRESH lt_includes.



      l_clskey = <fs_tadiv>-obj_name.



      CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'

        EXPORTING

          clskey                       = l_clskey

        IMPORTING

          includes                     = lt_includes

        EXCEPTIONS

          _internal_class_not_existing = 1

          OTHERS                       = 2.



      IF sy-subrc = 0.



        APPEND LINES OF lt_includes TO lt_includes_all.

        SORT lt_includes_all.

        DELETE ADJACENT DUPLICATES FROM lt_includes_all.



        LOOP AT lt_includes INTO l_includes.

          MOVE l_includes-incname TO i_prog-prog.

          APPEND i_prog.

        ENDLOOP.



      ENDIF.



    ENDLOOP.



  ENDIF.



*----- Find extra items

  LOOP AT i_prog.

    PERFORM find_program_name USING i_prog-prog

                              CHANGING i_prog-text.



    IF p_text = 'X'.

      PERFORM find_program_texts TABLES i_prog.

    ENDIF.



    IF p_mes = 'X'.

      PERFORM find_messages TABLES i_prog USING i_prog-prog.

    ENDIF.



    IF p_scr = 'X'.

      PERFORM find_screen_flow TABLES i_prog USING i_prog-prog.

    ENDIF.



    IF p_dict = 'X'.



      l_upd = rs_c_false.



      PERFORM find_custom_dict_structures TABLES i_prog

                                                 table_names

                                          USING  i_prog-prog

                                                 l_upd.

    ENDIF.



    IF p_func = 'X'.



      PERFORM find_custom_functions TABLES function_names

                                    USING i_prog-prog.

    ENDIF.



    IF p_inc = 'X'.

      PERFORM find_include_programs USING i_prog-prog.

      PERFORM sort_includes TABLES i_prog.



*---   find all relevant data for the includes table.

      IF NOT ( i_includes[] IS INITIAL ).



        LOOP AT i_prog-includes INTO wa_includes.



          IF p_dict = 'X'.



            l_upd = rs_c_false.



            PERFORM find_custom_dict_structures TABLES i_prog

                                                       table_names

                                                USING wa_includes-prog

                                                      l_upd.

          ENDIF.



          IF p_func = 'X'.

            PERFORM find_custom_functions TABLES function_names

                                          USING wa_includes-prog.

          ENDIF.

        ENDLOOP.



      ENDIF.



    ENDIF.



    PERFORM sort_dict_structures TABLES i_prog table_names.

    PERFORM sort_functions TABLES i_prog function_names.



    MODIFY i_prog INDEX counter.

    counter = counter + 1.

  ENDLOOP.



ENDFORM.                    "RETRIEVE_PROGRAMS



*----------------------------------------------------------------------

*  FIND_PROGRAM_NAME... find programme name

*----------------------------------------------------------------------

FORM find_program_name USING programme_name

                       CHANGING programme_text.





  DATA xxx     LIKE trdir-name.



  READ TEXTPOOL xxx INTO i_programme_texts LANGUAGE sy-langu.



  READ TEXTPOOL programme_name INTO i_programme_texts LANGUAGE sy-langu.



  READ TABLE i_programme_texts WITH KEY 'R'.



  IF sy-subrc EQ 0.

    programme_text = i_programme_texts-entry.

    DELETE i_programme_texts INDEX sy-tabix.



  ELSE.

* Get them from somewhere else







  ENDIF.



ENDFORM.                               " FIND_PROGRAMME_NAME



*----------------------------------------------------------------------

*   FIND_PROGRAM_TEXTS...  Messages and text elements

*----------------------------------------------------------------------

FORM find_program_texts TABLES i_prog STRUCTURE i_programmes.



  DATA: temp_selection TYPE t_texttab.



*--- selection texts.

  LOOP AT i_programme_texts WHERE id = 'S'.

    APPEND i_programme_texts TO i_prog-selection_texts.

    DELETE i_programme_texts INDEX sy-tabix.

  ENDLOOP.



*--- Text elements.

  LOOP AT i_programme_texts WHERE id = 'I'.

    APPEND i_programme_texts TO i_prog-text_elements.

  ENDLOOP.



ENDFORM.                    "FIND_PROGRAM_TEXTS



*----------------------------------------------------------------------

*   FIND_MESSAGES... finds all program messages including dynamically

*                      called messages - providing they have been

*                      declared on one complete line.

*----------------------------------------------------------------------

FORM find_messages TABLES i_prog STRUCTURE i_programmes

                   USING progname.



*--- lines for main program

  DATA: i_report_lines LIKE content OCCURS 0 WITH HEADER LINE.



*-- Separate working area for internal table

  DATA: wa_messages TYPE t_messages.



  DATA: msgid      LIKE trdire-msgid.

  DATA: head       LIKE i_report_lines-line.

  DATA: tail       LIKE i_report_lines-line.

  DATA: headlength TYPE i VALUE 0.

  DATA: taillength TYPE i VALUE 0.



*--- Read the program contents into memory

  READ REPORT progname INTO i_report_lines.



*--- Read the report content looking for message calls.

  LOOP AT i_report_lines.

    TRANSLATE i_report_lines TO UPPER CASE.



    IF NOT ( i_report_lines IS INITIAL ) AND i_report_lines(1) <> '*'.

*     Find the main message definition.

      IF i_report_lines CS 'MESSAGE-ID'.

        SHIFT i_report_lines LEFT DELETING LEADING space.

        SPLIT i_report_lines AT 'MESSAGE-ID' INTO head tail.

        SPLIT tail AT '.' INTO head tail.

        SHIFT head LEFT DELETING LEADING space.

        msgid = head.

      ELSE.

*     There are three different ways of calling a message to display

*     this routine looks for all three of them and strips the message

*     class and number out of the code

        IF i_report_lines CS 'MESSAGE'.



          SHIFT i_report_lines-line UP TO 'MESSAGE'.

          IF i_report_lines-line CS '('.

            SPLIT i_report_lines-line AT '(' INTO head tail.

            headlength = strlen( head ).

            headlength = headlength - 3.

            wa_messages-msgnr = head+headlength(3).



            SPLIT tail AT ')' INTO head tail.

            wa_messages-msgid = head.



          ELSEIF i_report_lines-line CS 'ID'.

            SHIFT i_report_lines UP TO 'ID'.

            SPLIT i_report_lines AT space INTO head tail.

            SHIFT tail LEFT DELETING LEADING space.

            head = tail.

            SPLIT head AT space INTO head tail.

            wa_messages-msgid = head.



            SPLIT tail AT 'NUMBER' INTO head tail.

            SHIFT tail LEFT DELETING LEADING space.



            taillength = strlen( tail ).

            IF taillength = 3.

              wa_messages-msgnr = tail+0(3).

            ELSE.

              CONTINUE.

            ENDIF.

          ELSE.

*---        use message class from main program

            SPLIT i_report_lines-line AT space INTO head tail.

            SHIFT tail LEFT DELETING LEADING space.

            wa_messages-msgid = msgid.

            wa_messages-msgnr = tail+1(3).

          ENDIF.



          APPEND wa_messages TO i_prog-messages.

          CLEAR wa_messages.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.



*--- Sort the messages and delete multiple occurrences from the

*    internal table.



  SORT i_prog-messages ASCENDING BY msgid msgnr.



  DELETE i_prog-messages WHERE msgid(1) <> 'Y'

                           AND msgid(1) <> 'Z'.



  DELETE ADJACENT DUPLICATES FROM i_prog-messages.



  DELETE i_prog-messages WHERE msgid IS INITIAL.

  DELETE i_prog-messages WHERE msgnr IS INITIAL.

  DELETE i_prog-messages WHERE msgnr CN '0123456789'.



  LOOP AT i_prog-messages INTO wa_messages.



    SELECT SINGLE text FROM t100 INTO wa_messages-text

                       WHERE sprsl = sy-langu

                         AND arbgb = wa_messages-msgid

                         AND msgnr = wa_messages-msgnr.



    MODIFY i_prog-messages FROM wa_messages INDEX sy-tabix.

  ENDLOOP.



ENDFORM.                    "FIND_MESSAGES



*----------------------------------------------------------------------

*  FIND_SCREEN_FLOW...

*----------------------------------------------------------------------

FORM find_screen_flow TABLES i_prog STRUCTURE i_programmes

                      USING progname.



  DATA: flow TYPE t_screen_flow OCCURS 0 WITH HEADER LINE.



  CALL FUNCTION 'DYNPRO_PROCESSINGLOGIC'

    EXPORTING

      rep_name  = progname

    TABLES

      scr_logic = flow.



  SORT flow ASCENDING BY screen.



  DELETE ADJACENT DUPLICATES FROM flow COMPARING screen.



  IF i_prog-subc <> 'M'.

    DELETE flow WHERE screen = '1000'.

  ENDIF.



  LOOP AT flow.

    APPEND flow TO i_prog-screen_flow.

  ENDLOOP.



ENDFORM.                    " FIND_SCREEN_FLOW



*-----------------------------------------------------------------------

* FIND_INCLUDE_PROGRAMS... Search each program for INCLUDE programs

*----------------------------------------------------------------------

FORM find_include_programs USING value(program).



  DATA: fip_prog(255),

        tail(255).



  DATA : lt_split_tab   TYPE TABLE OF string.



*--- Lines for include

  DATA: i_inc_lines LIKE content OCCURS 0 WITH HEADER LINE.



*----- Read ABAP

  READ REPORT program INTO i_inc_lines.



*----- Examine each line of ABAP

  LOOP AT i_inc_lines.



*--- find include programs.

    IF i_inc_lines(1) = '*'

      OR i_inc_lines(1) = '"'

      OR i_inc_lines IS INITIAL.

      CONTINUE.

    ENDIF.



    TRANSLATE i_inc_lines-line TO UPPER CASE.

    SHIFT i_inc_lines-line UP TO 'INCLUDE'.



    IF   ( i_inc_lines-line(9) EQ 'INCLUDE Z' )

      OR ( i_inc_lines-line(9) EQ 'INCLUDE Y' )

      AND i_inc_lines-line+8(9) NE space

      AND sy-tabix <> 1.



      fip_prog = i_inc_lines-line+8(64).



      SPLIT fip_prog AT '.' INTO fip_prog tail.



* Append program name to list of include programs

      SELECT SINGLE * FROM trdir WHERE name EQ fip_prog.



      CHECK sy-subrc EQ 0.



      i_prog_includes-prog = fip_prog.

      APPEND i_prog_includes.



*--- Recursively look for other includes.

      IF p_reci = 'X'.

        PERFORM find_include_programs USING fip_prog.

      ENDIF.



    ENDIF.



  ENDLOOP.



*----- Examine each line of ABAP

  LOOP AT i_inc_lines.

*--- find include programs.

    IF i_inc_lines(1) = '*' OR i_inc_lines IS INITIAL.

      CONTINUE.

    ENDIF.



    TRANSLATE i_inc_lines-line TO UPPER CASE.

    SHIFT i_inc_lines-line UP TO 'SUBMIT'.



    IF   ( i_inc_lines-line(8) EQ 'SUBMIT Z' )

      OR ( i_inc_lines-line(8) EQ 'SUBMIT Y' )

      AND i_inc_lines-line+7(9) NE space

      AND sy-tabix <> 1.



      fip_prog = i_inc_lines-line+7(64).



      CONDENSE fip_prog.



      SPLIT  fip_prog AT space INTO TABLE lt_split_tab.

      READ TABLE lt_split_tab INTO fip_prog INDEX 1.



      SPLIT fip_prog AT '.' INTO fip_prog tail.

*       Append program name to list of include programs



      SELECT SINGLE * FROM trdir

        WHERE name EQ fip_prog.



      CHECK sy-subrc EQ 0.



      i_prog_includes-prog = fip_prog.

      APPEND i_prog_includes.



    ENDIF.



  ENDLOOP.



ENDFORM.                               " FIND_INCLUDE_PROGRAMS



*----------------------------------------------------------------------

*  SORT_INCLUDES.. Remove any duplicates from include table.

*----------------------------------------------------------------------

FORM sort_includes TABLES i_prog STRUCTURE i_programmes.



  SORT i_prog_includes.

  DELETE ADJACENT DUPLICATES FROM i_prog_includes COMPARING prog.



  LOOP AT i_prog_includes.

    PERFORM find_program_name USING    i_prog_includes-prog

                              CHANGING i_prog_includes-text.

    MODIFY i_prog_includes.

    MOVE-CORRESPONDING i_prog_includes TO i_includes.

    APPEND i_includes.

  ENDLOOP.



  APPEND LINES OF i_prog_includes TO i_prog-includes.

  CLEAR i_prog_includes. REFRESH i_prog_includes.



ENDFORM.                    "SORT_INCLUDES





*&---------------------------------------------------------------------*

*&      Form  find_custom_dict_structures

*&---------------------------------------------------------------------*

*       text

*----------------------------------------------------------------------*

*      -->I_PROG       text

*      -->TABLE_NAMES  text

*      -->VALUE        text

*      -->(PROGRAM)    text

*      -->L_UPD        text

*----------------------------------------------------------------------*

FORM find_custom_dict_structures TABLES i_prog STRUCTURE i_programmes

                                        table_names

                                        STRUCTURE table_names

                                 USING value(program)

                                       l_upd.



  DATA : i_lines     LIKE content OCCURS 0 WITH HEADER LINE.



*--- read abap

  READ REPORT program INTO i_lines.



  PERFORM find_custom_dict_structures2 TABLES i_prog

                                              table_names

                                              i_lines

                                        USING i_prog-prog

                                              l_upd.



ENDFORM.                    "FIND_CUSTOM_DICT_STRUCTURES





*&---------------------------------------------------------------------*

*&      Form  find_custom_dict_structures2

*&---------------------------------------------------------------------*

*       text

*----------------------------------------------------------------------*

*      -->I_PROG       text

*      -->TABLE_NAMES  text

*      -->VALUE        text

*      -->(PROGRAM)    text

*----------------------------------------------------------------------*

FORM find_custom_dict_structures2 TABLES i_prog      STRUCTURE i_programmes

                                         table_names STRUCTURE table_names

                                         i_lines_in

                                  USING value(program)

                                         l_upd.



  DATA : head(76).

  DATA : tail(76).

  DATA : linetype(9).

  DATA : i_lines        LIKE content OCCURS 0 WITH HEADER LINE.

  DATA : end_of_line    TYPE i VALUE 1.

  DATA : l_line         LIKE content.

  DATA : l_line3        LIKE content.

  DATA : l_htlm         TYPE char255.

  DATA : l_len          TYPE i.

  DATA : l_line2        TYPE char255.

  DATA : l_line5        TYPE char255.

  DATA : lt_split_tab   TYPE TABLE OF string.

  DATA : lt_split_tab2   TYPE TABLE OF string.

  DATA : l_table_found  TYPE rs_bool.

  DATA : no_lines       TYPE i.



  DATA : l_tabix        TYPE sy-tabix.

  DATA : l_tabix2       TYPE sy-tabix.

  DATA : l_tabix3       TYPE sy-tabix.



  FIELD-SYMBOLS : <fs_split_tab>   TYPE string.

  FIELD-SYMBOLS : <fs_split_tab2>  TYPE string.

  FIELD-SYMBOLS : <fs_split_tab3>  TYPE string.

  FIELD-SYMBOLS : <fs_split_tab4>  TYPE string.

  DATA l_add_dot TYPE rs_bool VALUE rs_c_false.



  i_lines[] = i_lines_in[].



  LOOP AT i_lines.



    l_line = i_lines.



*--- find custom tables.

    IF i_lines-line(1) = '*' OR i_lines IS INITIAL.

      CONTINUE.

    ENDIF.



    TRANSLATE i_lines-line TO UPPER CASE.



* Determine the linetype.

    IF end_of_line = 1.

      SHIFT i_lines-line UP TO c_tables.

      IF sy-subrc = 0.

        linetype = c_tables.

      ELSE.

        SHIFT i_lines-line UP TO c_like.

        IF sy-subrc = 0.

          linetype = c_like.

        ELSE.

          SHIFT i_lines-line UP TO c_type.

          IF sy-subrc = 0.

            linetype = c_type.

          ELSE.

            SHIFT i_lines-line UP TO c_from.

            IF sy-subrc = 0.

              linetype = c_from.

            ELSE.

              SHIFT i_lines-line UP TO c_structure.

              IF sy-subrc = 0.

                linetype = c_structure.

              ELSE.

                SHIFT i_lines-line UP TO c_perform.

                IF sy-subrc = 0.

                  linetype = c_perform.

                ELSE.

                  SHIFT i_lines-line UP TO c_form.

                  IF sy-subrc = 0.

                    linetype = c_form.

                  ELSE.

                    CONTINUE.

                  ENDIF.

                ENDIF.

              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ELSE.

      linetype = c_comma.

    ENDIF.



* Work on the appropriate linetype

    CASE linetype.



      WHEN c_tables.

        SHIFT i_lines-line UP TO space.



        old_table_names[] = table_names[].



        PERFORM find_tables_on_one_line TABLES table_names

                                        USING  i_lines-line

                                               end_of_line.



        IF l_upd = rs_c_true

          AND table_names[] NE old_table_names[].



          CONDENSE i_lines-line NO-GAPS.



          l_line2  = i_lines-line.

          l_len    = strlen( l_line2 ).

          l_len    = l_len - 1.



          IF l_len GT 1.

            l_line2 = l_line2(l_len).

          ENDIF.



          CONCATENATE '<a href="./Dictionary-'

                      l_line2

                      '.html">'

                      l_line2

                      '</a>'

                 INTO l_line2.



          TRANSLATE l_line  TO UPPER CASE.

          TRANSLATE l_line2 TO UPPER CASE.



          REPLACE ALL OCCURRENCES OF i_lines-line IN l_line WITH l_line2.



          IF sy-subrc = 0.

            REFRESH i_lines_in.

            APPEND l_line TO i_lines_in.

          ELSE.

            i_lines_in[] = i_lines[].

          ENDIF.



        ENDIF.



      WHEN c_comma.



        old_table_names[] = table_names[].



        PERFORM find_tables_on_new_line TABLES table_names

                                        USING i_lines-line end_of_line.



        IF l_upd = rs_c_true

          AND table_names[] NE old_table_names[].



          CONDENSE i_lines-line NO-GAPS.



          l_line2  = i_lines-line.

          l_len    = strlen( l_line2 ).

          l_len    = l_len - 1.



          IF l_len GT 1.

            l_line2 = l_line2(l_len).

          ENDIF.



          CONCATENATE '<a href="./Dictionary-'

                      l_line2

                      '.html">'

                      l_line2

                      '</a>'

                 INTO l_line2.



          TRANSLATE l_line  TO UPPER CASE.

          TRANSLATE l_line2 TO UPPER CASE.



          REPLACE ALL OCCURRENCES OF i_lines-line IN l_line WITH l_line2.



          IF sy-subrc = 0.

            REFRESH i_lines_in.

            APPEND l_line TO i_lines_in.

          ELSE.

            i_lines_in[] = i_lines[].

          ENDIF.



        ENDIF.



      WHEN c_form.



        l_tabix = 1.



        l_line3 = l_line.

        CONDENSE l_line3.



        l_len    = strlen( l_line3 ).

        l_len    = l_len - 1.

        l_add_dot = rs_c_false.

        IF l_len GT 1.

          IF l_line3+l_len(1) = '.'

            OR l_line3+l_len(1) = ','.

            l_line3 = l_line3(l_len).

            l_add_dot = rs_c_true.

          ENDIF.

        ENDIF.



        SPLIT l_line3 AT space INTO TABLE lt_split_tab.



        LOOP AT lt_split_tab ASSIGNING <fs_split_tab>.



          TRANSLATE <fs_split_tab> TO UPPER CASE.



          IF <fs_split_tab> = c_form(4).

            l_tabix = sy-tabix + 1.

            EXIT.

          ENDIF.

        ENDLOOP.



        l_line = ''.



        LOOP AT lt_split_tab ASSIGNING <fs_split_tab> FROM l_tabix.



          IF l_line = ''.

            l_line = '<A NAME="'.



            CONCATENATE l_line

                        <fs_split_tab>

                        '">'

                    INTO l_line.

          ENDIF.



          EXIT.

        ENDLOOP.



        LOOP AT lt_split_tab ASSIGNING <fs_split_tab2>.

          IF <fs_split_tab2> = '' AND sy-tabix NE 1.

            <fs_split_tab2> = '¬'.

          ENDIF.

          CONCATENATE l_line <fs_split_tab2> INTO l_line SEPARATED BY space.

        ENDLOOP.



        REPLACE ALL OCCURRENCES OF '¬' IN l_line WITH ''.

        IF l_add_dot = rs_c_true.

          CONCATENATE l_line '.' INTO l_line.

        ENDIF.

        APPEND l_line TO i_lines_in.



      WHEN c_perform.



        l_add_dot = rs_c_false.

        l_line3 = l_line.

        CONDENSE l_line3.



        l_len    = strlen( l_line3 ).

        l_len    = l_len - 1.



        IF l_len GT 1.

          IF l_line3+l_len(1) = '.'

            OR l_line3+l_len(1) = ','.

            l_line3 = l_line3(l_len).



            l_add_dot = rs_c_true.



          ENDIF.

        ENDIF.



        SPLIT l_line3 AT space INTO TABLE lt_split_tab.



        LOOP AT lt_split_tab ASSIGNING <fs_split_tab>.



          TRANSLATE <fs_split_tab> TO UPPER CASE.



          IF <fs_split_tab> = c_perform(7).

            l_tabix = sy-tabix + 1.

            EXIT.

          ENDIF.

        ENDLOOP.



        LOOP AT lt_split_tab ASSIGNING <fs_split_tab> FROM l_tabix.



          l_line2 = <fs_split_tab>.



          CONCATENATE '<a href="#'

                      l_line2

                      '">'

                      l_line2

                      '</a>'

                 INTO l_htlm.



          TRANSLATE l_line  TO UPPER CASE.

          TRANSLATE l_line2 TO UPPER CASE.



          <fs_split_tab> = l_htlm.

          EXIT.

        ENDLOOP.



        LOOP AT lt_split_tab ASSIGNING <fs_split_tab> FROM l_tabix.



          l_line = ''.



          LOOP AT lt_split_tab ASSIGNING <fs_split_tab2>.

            IF <fs_split_tab2> = '' AND sy-tabix NE 1.

              <fs_split_tab2> = '¬'.

            ENDIF.

            CONCATENATE l_line <fs_split_tab2> INTO l_line SEPARATED BY space.

          ENDLOOP.



          REPLACE ALL OCCURRENCES OF '¬' IN l_line WITH ''.



          IF l_add_dot = rs_c_true.

            CONCATENATE l_line '.' INTO l_line.

          ENDIF.

          APPEND l_line TO i_lines_in.



        ENDLOOP.



      WHEN c_type

        OR c_structure

        OR c_from

        OR c_like.



        l_line3 = l_line.

        CONDENSE l_line3.



        SPLIT l_line3 AT space INTO TABLE lt_split_tab.



        DESCRIBE TABLE lt_split_tab LINES no_lines.

        READ TABLE lt_split_tab ASSIGNING <fs_split_tab> INDEX no_lines.



        LOOP AT lt_split_tab ASSIGNING <fs_split_tab>.



          IF   <fs_split_tab> = c_type

            OR <fs_split_tab> = c_structure

            OR <fs_split_tab> = c_like.



            l_table_found = rs_c_false.

            l_tabix       = sy-tabix.



            LOOP AT lt_split_tab ASSIGNING <fs_split_tab2> FROM l_tabix.



              IF <fs_split_tab2> = 'TABLE'.

                l_table_found = rs_c_true.

                l_tabix2      = sy-tabix.



                LOOP AT lt_split_tab ASSIGNING <fs_split_tab3> FROM l_tabix2.



                  IF <fs_split_tab3> = 'OF'.

* found the OF so next line is the TABLE or STRUCTURE



                    l_tabix3 = sy-tabix + 1.

                    READ TABLE lt_split_tab ASSIGNING <fs_split_tab4> INDEX l_tabix3.

                    EXIT.



                  ENDIF.



                ENDLOOP.



              ENDIF.



            ENDLOOP.



            IF l_table_found = rs_c_false.



            ENDIF.



          ENDIF.



        ENDLOOP.



        IF <fs_split_tab4> IS ASSIGNED.

          l_line5 = <fs_split_tab4>.

        ENDIF.



        SPLIT l_line AT space INTO TABLE lt_split_tab.



* find position of LIKE or TYPE

        l_tabix = 1.



        LOOP AT lt_split_tab ASSIGNING <fs_split_tab>.



          TRANSLATE <fs_split_tab> TO UPPER CASE.



          IF   <fs_split_tab> = c_type(4)

            OR <fs_split_tab> = c_structure(9)

            OR <fs_split_tab> = c_like(4).



            l_tabix       = sy-tabix.

            EXIT.

          ENDIF.



        ENDLOOP.



        LOOP AT lt_split_tab ASSIGNING <fs_split_tab> FROM l_tabix.



          l_line2 = <fs_split_tab>.

          l_len   = strlen( l_line2 ).



          IF   l_line2(1) = 'Z'

            OR l_line2(1) = 'Y'

            OR l_line2(1) = 'z'

            OR l_line2(1) = 'y'

          OR ( l_len GE 5

            AND ( l_line2(5) = '/GLB/'

               OR l_line2(5) = '/glb/' ) )

          AND l_line2 = l_line5.



            l_len    = strlen( l_line2 ).

            l_len    = l_len - 1.



            IF l_len GT 1.

              IF l_line2+l_len(1) = '.'

                OR l_line2+l_len(1) = ','.

                l_line2 = l_line2(l_len).



              ENDIF.

            ENDIF.



* Left part of the '-'

            IF l_line2 CS '-'.

              SPLIT l_line2 AT '-' INTO TABLE lt_split_tab2.



              READ TABLE lt_split_tab2 ASSIGNING <fs_split_tab2> INDEX 1.

              IF sy-subrc = 0.

*              l_line2 = <fs_split_tab2>.

              ENDIF.



* <a href="Dictionary-ygttxtlg.html#Dictionary-ygttxltg_doc_num">DOC_NUM</a>



              CONCATENATE '<a href="./Dictionary-'

                 <fs_split_tab2>

                 '.html#Dictionary-'

                 l_line2

                 '">'

                 l_line2

                 '</a>'

            INTO l_htlm.



            ELSE.



              CONCATENATE '<a href="./Dictionary-'

                          l_line2

                          '.html">'

                          <fs_split_tab>

                          '</a>'

                     INTO l_htlm.

            ENDIF.



            TRANSLATE l_line  TO UPPER CASE.

            TRANSLATE l_line2 TO UPPER CASE.



            <fs_split_tab> = l_htlm.



            IF l_upd = rs_c_true.



              IF sy-subrc = 0.

                l_line = ''.



                LOOP AT lt_split_tab ASSIGNING <fs_split_tab2>.

                  IF <fs_split_tab2> = '' AND sy-tabix NE 1.

                    <fs_split_tab2> = '¬'.

                  ENDIF.

                  CONCATENATE l_line <fs_split_tab2> INTO l_line SEPARATED BY space.

                ENDLOOP.



                REPLACE ALL OCCURRENCES OF '¬' IN l_line WITH ''.



                APPEND l_line TO i_lines_in.

              ELSE.

                i_lines_in[] = i_lines[].

              ENDIF.



            ELSE.



              IF NOT l_line2 IS INITIAL.

                APPEND l_line2 TO table_names.

              ENDIF.



            ENDIF.



          ENDIF.

        ENDLOOP.



      WHEN c_from_db

        OR c_to_db.



        l_line3 = l_line.

        CONDENSE l_line3.



        SPLIT l_line3 AT space INTO TABLE lt_split_tab.



        DESCRIBE TABLE lt_split_tab LINES no_lines.

        READ TABLE lt_split_tab ASSIGNING <fs_split_tab> INDEX no_lines.



        LOOP AT lt_split_tab ASSIGNING <fs_split_tab>.



          IF   <fs_split_tab> = c_type

            OR <fs_split_tab> = c_structure

            OR <fs_split_tab> = c_like.



            l_table_found = rs_c_false.

            l_tabix       = sy-tabix.



            LOOP AT lt_split_tab ASSIGNING <fs_split_tab2> FROM l_tabix.



              IF <fs_split_tab2> = 'TABLE'.

                l_table_found = rs_c_true.

                l_tabix2      = sy-tabix.



                LOOP AT lt_split_tab ASSIGNING <fs_split_tab3> FROM l_tabix2.



                  IF <fs_split_tab3> = 'OF'.

* found the OF so next line is the TABLE or STRUCTURE



                    l_tabix3 = sy-tabix + 1.

                    READ TABLE lt_split_tab ASSIGNING <fs_split_tab4> INDEX l_tabix3.

                    EXIT.



                  ENDIF.



                ENDLOOP.



              ENDIF.



            ENDLOOP.



            IF l_table_found = rs_c_false.



            ENDIF.



          ENDIF.



        ENDLOOP.



        IF <fs_split_tab4> IS ASSIGNED.

          l_line5 = <fs_split_tab4>.

        ENDIF.



        SPLIT l_line AT space INTO TABLE lt_split_tab.



* find position of LIKE or TYPE

        l_tabix = 1.



        LOOP AT lt_split_tab ASSIGNING <fs_split_tab>.



          TRANSLATE <fs_split_tab> TO UPPER CASE.



          IF   <fs_split_tab> = c_type(4)

            OR <fs_split_tab> = c_structure(9)

            OR <fs_split_tab> = c_like(4).



            l_tabix       = sy-tabix.

            EXIT.

          ENDIF.



        ENDLOOP.



        LOOP AT lt_split_tab ASSIGNING <fs_split_tab> FROM l_tabix.



          l_line2 = <fs_split_tab>.

          l_len   = strlen( l_line2 ).



          IF   l_line2(1) = 'Z'

            OR l_line2(1) = 'Y'

            OR l_line2(1) = 'z'

            OR l_line2(1) = 'y'

          OR ( l_len GE 5

            AND ( l_line2(5) = '/GLB/'

               OR l_line2(5) = '/glb/' ) )

          AND l_line2 = l_line5.



            l_len    = strlen( l_line2 ).

            l_len    = l_len - 1.



            IF l_len GT 1.

              IF l_line2+l_len(1) = '.'

                OR l_line2+l_len(1) = ','.

                l_line2 = l_line2(l_len).



              ENDIF.

            ENDIF.



* Left part of the '-'

            IF l_line2 CS '-'.

              SPLIT l_line2 AT '-' INTO TABLE lt_split_tab2.



              READ TABLE lt_split_tab2 ASSIGNING <fs_split_tab2> INDEX 1.

              IF sy-subrc = 0.

                l_line2 = <fs_split_tab2>.

              ENDIF.

            ENDIF.



            CONCATENATE '<a href="./Dictionary-'

                        l_line2

                        '.html">'

                        <fs_split_tab>

                        '</a>'

                   INTO l_htlm.



            TRANSLATE l_line  TO UPPER CASE.

            TRANSLATE l_line2 TO UPPER CASE.



            <fs_split_tab> = l_htlm.



            IF l_upd = rs_c_true.



              IF sy-subrc = 0.

                l_line = ''.



                LOOP AT lt_split_tab ASSIGNING <fs_split_tab2>.

                  IF <fs_split_tab2> = '' AND sy-tabix NE 1.

                    <fs_split_tab2> = '¬'.

                  ENDIF.

                  CONCATENATE l_line <fs_split_tab2> INTO l_line SEPARATED BY space.

                ENDLOOP.



                REPLACE ALL OCCURRENCES OF '¬' IN l_line WITH ''.



                APPEND l_line TO i_lines_in.

              ELSE.

                i_lines_in[] = i_lines[].

              ENDIF.



            ELSE.



              IF NOT l_line2 IS INITIAL.

                APPEND l_line2 TO table_names.

              ENDIF.



            ENDIF.



          ENDIF.

        ENDLOOP.







*      WHEN c_like OR c_type OR c_structure.

*

*        SHIFT i_lines-line UP TO space.

*        SHIFT i_lines-line LEFT DELETING LEADING space.

*

**        IF i_lines-line(1) = 'Y'

**          OR i_lines-line(1) = 'Z'.

*

*        IF i_lines-line CS c_comma.

*          SPLIT i_lines-line AT c_comma INTO head tail.

*          IF i_lines-line CS '-'.

*            SPLIT head AT '-' INTO head tail.

*          ENDIF.

*          IF i_lines-line CS 'OCCURS'.

*            SPLIT i_lines-line AT space INTO head tail.

*          ENDIF.

*        ELSE.

*          IF i_lines-line CS c_period.

*            SPLIT i_lines-line AT c_period INTO head tail.

*            IF i_lines-line CS '-'.

*              SPLIT head AT '-' INTO head tail.

*            ENDIF.

*            IF i_lines-line CS 'OCCURS'.

*              SPLIT i_lines-line AT space INTO head tail.

*            ENDIF.

*          ELSE.

*            SPLIT i_lines-line AT space INTO head tail.

*          ENDIF.

*        ENDIF.

*

*        table_names-tabname = head.

*

*        IF l_upd = rs_c_false.

*          APPEND table_names.

*        ENDIF.



*      ENDIF.



        SHIFT i_lines-line UP TO space.

        SHIFT i_lines-line LEFT DELETING LEADING space.



*        IF i_lines-line(1) = 'Y'

*          OR i_lines-line(1) = 'Z'.



        IF i_lines-line CS c_comma.

          SPLIT i_lines-line AT c_comma INTO head tail.

          IF i_lines-line CS '-'.

            SPLIT head AT '-' INTO head tail.

          ENDIF.

          IF i_lines-line CS 'OCCURS'.

            SPLIT i_lines-line AT space INTO head tail.

          ENDIF.

        ELSE.

          IF i_lines-line CS c_period.

            SPLIT i_lines-line AT c_period INTO head tail.

            IF i_lines-line CS '-'.

              SPLIT head AT '-' INTO head tail.

            ENDIF.

            IF i_lines-line CS 'OCCURS'.

              SPLIT i_lines-line AT space INTO head tail.

            ENDIF.

          ELSE.

            SPLIT i_lines-line AT space INTO head tail.

          ENDIF.

        ENDIF.



        l_len = strlen( head ).



        IF   head(1) = 'Z'

          OR head(1) = 'Y'

          OR head(1) = 'z'

          OR head(1) = 'y'



        OR ( l_len GE 5

          AND ( head(5) = '/GLB/'

             OR head(5) = '/glb/' ) ).



          table_names-tabname = head.



          IF l_upd = rs_c_false.

            APPEND table_names.

          ENDIF.

        ENDIF.



    ENDCASE.



  ENDLOOP.



ENDFORM.                    "find_custom_dict_structures2





*-----------------------------------------------------------------------

*  SORT_DICT_STRUCTURES... don't allow muliples in prog structure

*-----------------------------------------------------------------------

FORM sort_dict_structures TABLES i_prog STRUCTURE i_programmes

                                 tab_names STRUCTURE table_names.



  DATA: wa_dict_struct TYPE t_dict_struct.



  SORT tab_names ASCENDING BY tabname.

  DELETE ADJACENT DUPLICATES FROM tab_names.



  IF NOT tab_names[] IS INITIAL.

    LOOP AT table_names.

      MOVE-CORRESPONDING table_names TO wa_dict_struct.

      APPEND wa_dict_struct TO i_prog-dict_struct.

    ENDLOOP.

    CLEAR tab_names. REFRESH tab_names.

  ENDIF.



ENDFORM.                    "SORT_DICT_STRUCTURES



*----------------------------------------------------------------------

*  FIND_TABLES_ON_NEW_LINE...  Find custom tables declared with a

*                              tables statement but have extended onto

*                              multiple lines.

*----------------------------------------------------------------------

FORM find_tables_on_new_line TABLES cust_tables STRUCTURE table_names

                             USING line eol.



  DATA: temp_line(100).

  DATA: head(76).

  DATA: tail(76).

  DATA: strlength TYPE i VALUE 0.



  temp_line = line.

  SHIFT temp_line LEFT DELETING LEADING space.



  DATA l_len TYPE i.

  l_len = strlen( temp_line ).



  IF   temp_line(1) = 'Z'

    OR temp_line(1) = 'Y'



  OR ( l_len GE 5

    AND ( temp_line(5) = '/GLB/'

       OR temp_line(5) = '/glb/' ) ).



    IF temp_line CS c_comma.

      SPLIT temp_line AT ',' INTO head tail.

      table_names-tabname = head.

      APPEND table_names.

      SHIFT tail LEFT BY 1 PLACES.

      PERFORM find_tables_on_new_line TABLES table_names

                                      USING tail eol.

    ELSE.

      SPLIT temp_line AT '.' INTO head tail.

      eol = 1.

      table_names-tabname = head.

      APPEND table_names.

    ENDIF.

  ELSE.

    strlength = strlen( temp_line ).

    IF strlength > 0.

      IF temp_line CS c_comma.

        SHIFT temp_line UP TO space.

        PERFORM find_tables_on_new_line TABLES table_names

                                        USING temp_line eol.

      ELSE.

        IF temp_line(1) = '"'.

          eol = 0.

        ELSE.

          eol = 1.

        ENDIF.

      ENDIF.

    ELSE.

      eol = 0.

    ENDIF.

  ENDIF.



ENDFORM.                    "FIND_TABLES_ON_NEW_LINE



*----------------------------------------------------------------------

*  FIND_TABLES_ON_ONE_LINE...  Find custom tables declared with a table

*                              statement whereby the tables are declare

*                              on one line

*----------------------------------------------------------------------

FORM find_tables_on_one_line TABLES cust_tables STRUCTURE table_names

                             USING line eol.



  DATA: temp_line(100).

  DATA: head(76).

  DATA: tail(76).

  DATA: strlength TYPE i VALUE 0.



  temp_line = line.

  SHIFT temp_line LEFT DELETING LEADING space.



  DATA l_len TYPE i.

  l_len = strlen( temp_line ).



  IF   temp_line(1) = 'Z'

    OR temp_line(1) = 'Y'



  OR ( l_len GE 5

    AND ( temp_line(5) = '/GLB/'

       OR temp_line(5) = '/glb/' ) ).



    IF temp_line CS c_comma.

      SPLIT temp_line AT ',' INTO head tail.

      table_names-tabname = head.

      APPEND table_names.

      SHIFT tail LEFT BY 1 PLACES.

      PERFORM find_tables_on_one_line TABLES table_names

                                       USING tail eol.

    ELSE.

      SPLIT temp_line AT '.' INTO head tail.

      eol = 1.

      table_names-tabname = head.

      APPEND table_names.

    ENDIF.



  ELSE.



    strlength = strlen( temp_line ).

    IF strlength > 0.

      SHIFT temp_line UP TO space.

      PERFORM find_tables_on_one_line TABLES table_names

                                      USING temp_line eol.

    ELSE.

      eol = 0.

    ENDIF.



  ENDIF.



ENDFORM.                    "FIND_TABLES_ON_ONE_LINE



*----------------------------------------------------------------------

* FIND_CUSTOM_FUNCTIONS... Look for any functions not created by SAP

*----------------------------------------------------------------------

FORM find_custom_functions TABLES i_func_names STRUCTURE function_names

                           USING value(program).



  DATA : i_lines            LIKE content OCCURS 0 WITH HEADER LINE.

  DATA : head(76).

  DATA : tail(76).

  DATA : i_recursive_func   LIKE i_functions OCCURS 0 WITH HEADER LINE.

  DATA : i_found_recursive  LIKE i_functions OCCURS 0 WITH HEADER LINE.

  DATA : rec_func_name      LIKE trdir-name.



*--- read abap

  READ REPORT program INTO i_lines.



  LOOP AT i_lines.



*--- find custom tables.

    IF   i_lines-line(1) = '*'

      OR i_lines-line(1) = '"'

      OR i_lines IS INITIAL.

      CONTINUE.

    ENDIF.



    TRANSLATE i_lines-line TO UPPER CASE.



    IF i_lines-line CS 'CALL FUNCTION'.



      SHIFT i_lines-line UP TO 'FUNCTION'.

      SHIFT i_lines-line UP TO space.

      SHIFT i_lines-line LEFT DELETING LEADING space.

      SHIFT i_lines-line LEFT DELETING LEADING ''''.

      SPLIT i_lines-line AT '''' INTO head tail.



      DATA l_len TYPE i.

      l_len = strlen( head ).



      IF    head(1) = 'Z'

        OR  head(1) = 'Y'

        OR  head(1) = 'z'

        OR  head(1) = 'y'



      OR ( l_len GE 5

        AND (  head(5) = '/GLB/'

           OR  head(5) = '/glb/' ) ).



*      IF head(1) = 'Y'

*        OR head(1) = 'Z'

*        OR head(5) = '/GLB/'.



        i_func_names-funcname = head.

        APPEND i_func_names TO i_recursive_func.



        READ TABLE i_func_names WITH KEY funcname = head.



        IF sy-subrc <> 0.

          APPEND i_func_names.



          IF p_recf = 'X'.

            PERFORM retrieve_functions TABLES s_fname

                                              s_fgroup

                                              i_recursive_func

                                              i_found_recursive

                                        USING 0.



            READ TABLE i_found_recursive INDEX 1.



            PERFORM func_include_name USING i_found_recursive-pname

                                            i_found_recursive-include

                                            rec_func_name

                                            0.



            PERFORM find_include_programs USING rec_func_name.



            PERFORM find_custom_functions TABLES i_func_names

                                          USING rec_func_name.

          ENDIF.



          CLEAR i_found_recursive.

          REFRESH i_found_recursive.

          CLEAR i_recursive_func.

          REFRESH i_recursive_func.

        ENDIF.



      ENDIF.



    ENDIF.



  ENDLOOP.



ENDFORM.                    "FIND_CUSTOM_FUNCTIONS



*-----------------------------------------------------------------------

*  FUNC_INCLUDE_NAME...

*-----------------------------------------------------------------------

FORM func_include_name USING value(prog_name)

                             value(include_no)

                             internal_name

                             value(want_top).



  DATA: inc_number(4).



  CONCATENATE '%U' include_no INTO inc_number.



  IF want_top = 0.

    SELECT SINGLE include FROM d010inc INTO internal_name

                          WHERE master = prog_name

                            AND include LIKE inc_number.

  ELSE.

    SELECT SINGLE include FROM d010inc INTO internal_name

                          WHERE master = prog_name

                            AND ( include LIKE '%TOP'

                             OR  ( include LIKE inc_number

                               AND include NOT LIKE '%$%' ) ).

  ENDIF.

ENDFORM.                    "FUNC_INCLUDE_NAME



*-----------------------------------------------------------------------

*  SORT_FUNCTIONS... don't allow duplicates in prog structure.

*-----------------------------------------------------------------------

FORM sort_functions TABLES i_prog STRUCTURE i_programmes

                           i_func_names STRUCTURE function_names.



  DATA: wa_func_struct TYPE t_functions.



  SORT i_func_names ASCENDING BY funcname.

  DELETE ADJACENT DUPLICATES FROM i_func_names.



  IF NOT i_func_names[] IS INITIAL.

    LOOP AT i_func_names.

      MOVE-CORRESPONDING i_func_names TO wa_func_struct.

      APPEND wa_func_struct TO i_prog-functions.

    ENDLOOP.

    CLEAR i_func_names. REFRESH i_func_names.

  ENDIF.



ENDFORM.                    "SORT_FUNCTIONS



*----------------------------------------------------------------------

*  DISPLAY_STATUS...

*----------------------------------------------------------------------

FORM display_status USING message delay.



  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'

    EXPORTING

      percentage = 0

      text       = message

    EXCEPTIONS

      OTHERS     = 1.



  IF delay > 0.

    CALL FUNCTION 'RZL_SLEEP'

      EXPORTING

        seconds        = delay

      EXCEPTIONS

        argument_error = 1

        OTHERS         = 2.

  ENDIF.



ENDFORM.                    " DISPLAY_STATUS





***********************************************************************

************************HTML ROUTINES***********************************

************************************************************************



*-----------------------------------------------------------------------

*  CONVERT_DD_TO_HTML...   Convert text description to HTML

*-----------------------------------------------------------------------

FORM convert_dd_to_html TABLES i_dict      STRUCTURE i_dict_minus_tabname

                               i_dict_html STRUCTURE w3html

                        USING  l_title_text.



  DATA : column_captions LIKE w3head   OCCURS 0 WITH HEADER LINE.

  DATA : row_attributes  LIKE w3fields OCCURS 0 WITH HEADER LINE.

  DATA : l_w3html        TYPE w3html.

  DATA : wa_header       TYPE w3head.



  PERFORM set_column_headers TABLES column_captions.

  PERFORM set_row_attributes TABLES row_attributes.



  l_w3html = 'BORDER=1'.



  wa_header-text = l_title_text .

  wa_header-font = 'Arial'.

  wa_header-size = '4'.



  CALL FUNCTION 'WWW_ITAB_TO_HTML'

    EXPORTING

      table_header     = wa_header

      table_attributes = l_w3html

      all_fields       = 'X'

    TABLES

      html             = i_dict_html

      fields           = row_attributes

      row_header       = column_captions

      itable           = i_dict.



  PERFORM re_format_html TABLES i_dict_html.



ENDFORM.                    "CONVERT_DD_TO_HTML



*----------------------------------------------------------------------

*  SET_COLUMN_HEADERS... For DD structures converted to HTML

*----------------------------------------------------------------------

FORM set_column_headers TABLES column_captions STRUCTURE w3head.



  DATA field_no LIKE w3head-nr   VALUE 1.

  DATA text     LIKE w3head-text VALUE 'Field name'.



  DO 8 TIMES.         "number of fields in structure i_dictionary.



    CALL FUNCTION 'WWW_ITAB_TO_HTML_HEADERS'

      EXPORTING

        field_nr  = field_no

        text      = text

        justified = '"Left"'

        fgcolor   = '#000000'

        bgcolor   = '#FFFFFF'

        size      = '2'

        font      = '"Arial"'

      TABLES

        header    = column_captions.



    field_no = field_no + 1.



    CASE field_no.

      WHEN 2.

        text = 'Position'.

      WHEN 3.

        text = 'Key'.

      WHEN 4.

        text = 'Data Element'.

      WHEN 5.

        text = 'DMN'.

      WHEN 6.

        text = 'Datatype'.

      WHEN 7.

        text = 'Length'.

      WHEN 8.

        text = 'DMN Text'.

    ENDCASE.



  ENDDO.



ENDFORM.                    "SET_COLUMN_HEADERS



*----------------------------------------------------------------------



*  SET_ROW_ATTRIBUTES... For DD structures converted to HTML



*----------------------------------------------------------------------

FORM set_row_attributes TABLES row_attributes STRUCTURE w3fields.



  DATA field_no LIKE w3fields-nr VALUE 1.



  DO 8 TIMES.         "number of fields in structure i_dict.

    CALL FUNCTION 'WWW_ITAB_TO_HTML_LAYOUT'

      EXPORTING

        field_nr = field_no

        size     = '1'

        fgcolor  = 'black'

        font     = '"Arial"'

      TABLES

        fields   = row_attributes.



    field_no = field_no + 1.

  ENDDO.



ENDFORM.                    "SET_ROW_ATTRIBUTES



*----------------------------------------------------------------------

*  RE_FORMAT_HTML... Splits table lines at EOL marker an places line on

*                    a new line.

*----------------------------------------------------------------------

FORM re_format_html TABLES html_table STRUCTURE w3html.



  DATA : new_html     LIKE w3html OCCURS 0 WITH HEADER LINE.

  DATA : lt_split_tab TYPE TABLE OF string.

  DATA : l_string     TYPE string.



  FIELD-SYMBOLS <fs_string> TYPE string.



  LOOP AT html_table.



    REPLACE ALL OCCURRENCES OF '¬¬' IN html_table WITH '>'.

    REPLACE ALL OCCURRENCES OF '¬'  IN html_table WITH '<'.



    CONCATENATE l_string html_table INTO l_string.

  ENDLOOP.



  SPLIT l_string AT '</' INTO TABLE lt_split_tab.



  LOOP AT lt_split_tab ASSIGNING <fs_string>.



    IF <fs_string> NE ''.

      IF <fs_string>(1) NE '<'.

        CONCATENATE '</' <fs_string> INTO <fs_string>.

      ENDIF.

    ENDIF.



    DATA l_len TYPE i.



    l_len =  strlen( <fs_string> ).



    IF l_len GT 250.

      APPEND <fs_string>(250)       TO new_html.

      l_len = l_len - 250.

      APPEND <fs_string>+250(l_len) TO new_html.

    ELSE.



      APPEND <fs_string> TO new_html.



    ENDIF.

  ENDLOOP.



  html_table[] = new_html[].



ENDFORM.                    "RE_FORMAT_HTML





*----------------------------------------------------------------------

*  BUILD_HTML_TABLE... Builds an HTML table based upon a text table

*                      Replaces signs '<>' with HTML versions.

*----------------------------------------------------------------------

FORM convert_table_to_html TABLES contents STRUCTURE content

                           USING value(program_name).



  DATA : l_fname          LIKE content.

  DATA : i_lines_in       LIKE content      OCCURS 0 WITH HEADER LINE.

  DATA : html_table       LIKE w3html       OCCURS 0 WITH HEADER LINE.

  DATA : i_prog           LIKE i_programmes OCCURS 0 WITH HEADER LINE.

  DATA : lt_split_tab     TYPE TABLE OF string.

  DATA : l_tabix          TYPE sy-tabix.

  DATA : l_line2          TYPE char255.

  DATA : l_line1          TYPE char255.

  DATA : listing_name     TYPE char100 VALUE 'Program listing for:'.



  FIELD-SYMBOLS : <fs_split_tab1>  TYPE string.

  FIELD-SYMBOLS : <fs_split_tab2>  TYPE string.



  CONSTANTS: br(4) VALUE '<br>'.

  CONSTANTS: cr(4) VALUE '<cr>'.

  CONSTANTS: hr(4) VALUE '<hr>'.

  CONSTANTS: lt(4) VALUE '&lt;'.

  CONSTANTS: gt(4) VALUE '&gt;'.



  html_table = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">'.

  APPEND html_table.



  html_table = '<html>'.

  APPEND html_table.



  html_table = '<head>'.

  APPEND html_table.



  CONCATENATE '<title>' program_name '</title>' INTO html_table.

  APPEND html_table.



  html_table = '</head>'.

  APPEND html_table.



  html_table = '<body bgcolor=#FFFFFF>'.

  APPEND html_table.



  IF program_name CS '======'.



    READ TABLE lt_includes_all ASSIGNING <fs_includes_all>

      WITH KEY incname = program_name.



    IF <fs_includes_all> IS ASSIGNED.



      CONCATENATE listing_name

                  <fs_includes_all>-cpdkey-cpdname

                   INTO listing_name  SEPARATED BY space.

    ELSE.



      CONCATENATE listing_name program_name INTO listing_name

                                            SEPARATED BY space.

    ENDIF.

  ELSE.



    CONCATENATE listing_name program_name INTO listing_name

                                          SEPARATED BY space.



  ENDIF.



  CONCATENATE '<font size=3 face = "Arial" color=#000000><b>'

                listing_name '</b></font>' INTO html_table.

  APPEND html_table.



  html_table = hr.

  APPEND html_table.



  html_table = '<font size=2 face = "Sans Serif">'.

  APPEND html_table.



  html_table = '<pre width=100>'.

  APPEND html_table.



  LOOP AT contents.



    IF NOT ( contents IS INITIAL ).

      WHILE ( contents CS '<' OR contents CS '>' ).

        REPLACE '<' WITH lt INTO contents.

        REPLACE '>' WITH gt INTO contents.

      ENDWHILE.



      CONCATENATE contents cr INTO html_table.

    ELSE.

      html_table = br.

    ENDIF.



* Add In Links

    l_fname = html_table.

    TRANSLATE l_fname TO UPPER CASE.



    IF l_fname CS 'CALL FUNCTION'

      AND NOT l_fname(1) = '*'

      AND NOT l_fname(1) = '"'.



* CALL FUNCTION <a href="./YRSZ_I_COPY_QRY_TO_CUBE_RFC.html">YRSZ_I_COPY_QRY_TO_CUBE_RFC</a>



      l_fname = html_table.

      REPLACE ALL OCCURRENCES OF `'`              IN l_fname WITH ''.

      REPLACE ALL OCCURRENCES OF 'CALL FUNCTION ' IN l_fname WITH ''.

      REPLACE ALL OCCURRENCES OF 'call function ' IN l_fname WITH ''.

      REPLACE ALL OCCURRENCES OF '.'              IN l_fname WITH ''.

      REPLACE ALL OCCURRENCES OF cr               IN l_fname WITH ''.



      CONDENSE l_fname NO-GAPS.

      TRANSLATE l_fname TO UPPER CASE.





      DATA l_len TYPE i.

      l_len = strlen( l_fname ).



      IF   l_fname(1) = 'Z'

        OR l_fname(1) = 'Y'

        OR l_fname(1) = 'z'

        OR l_fname(1) = 'y'



      OR ( l_len GE 5

        AND ( l_fname(5) = '/GLB/'

           OR l_fname(5) = '/glb/' ) ).



        CONCATENATE '<a href="./'

                    l_fname

                    '.html'

                    '">'

                    l_fname

                    '</a>'

               INTO l_line2.



        REPLACE ALL OCCURRENCES OF l_fname IN html_table WITH l_line2.

      ENDIF.



    ELSEIF l_fname CS 'CALL METHOD'

      AND  ( l_fname CS 'ME-'

        OR   l_fname CS 'LCL_ALV-' ).



*     CALL METHOD me-&gt;type_verification<cr>

      l_line1 = html_table.



      CONDENSE l_line1.

      REPLACE ALL OCCURRENCES OF 'CALL METHOD me-&gt;'      IN l_line1 WITH ''.

      REPLACE ALL OCCURRENCES OF 'CALL METHOD lcl_alv-&gt;' IN l_line1 WITH ''.

      REPLACE ALL OCCURRENCES OF '.<cr>'                    IN l_line1 WITH ''.

      REPLACE ALL OCCURRENCES OF '<cr>'                     IN l_line1 WITH ''.

      REPLACE ALL OCCURRENCES OF 'call method me-&gt;'      IN l_line1 WITH ''.

      REPLACE ALL OCCURRENCES OF 'call method lcl_alv-&gt;' IN l_line1 WITH ''.



* Get the internal ref of the methof

      SPLIT program_name AT '=' INTO TABLE lt_split_tab.



      TRANSLATE l_line1 TO UPPER CASE.



      READ TABLE lt_includes_all ASSIGNING <fs_includes_all>

        WITH KEY cpdkey-cpdname = l_line1.



      IF <fs_includes_all> IS ASSIGNED.





        l_len = strlen( <fs_includes_all> ).



        IF   <fs_includes_all>(1) = 'Z'

          OR <fs_includes_all>(1) = 'Y'

          OR <fs_includes_all>(1) = 'z'

          OR <fs_includes_all>(1) = 'y'



        OR ( l_len GE 5

          AND ( <fs_includes_all>(5) = '/GLB/'

             OR <fs_includes_all>(5) = '/glb/' ) ).



* Add HTML Link Code

          CONCATENATE '<a href="./'

                      <fs_includes_all>-incname

                      '.html">'

                      l_line1

                      '</a>'

                 INTO l_line2.



          REPLACE ALL OCCURRENCES OF l_line1 IN html_table WITH l_line2.



          IF sy-subrc NE 0.

            TRANSLATE l_line1 TO LOWER CASE.

            REPLACE ALL OCCURRENCES OF l_line1 IN html_table WITH l_line2.

          ENDIF.



        ENDIF.

      ENDIF.



    ELSEIF ( l_fname CS 'INCLUDE'

      OR  l_fname CS 'SUBMIT' )

      AND ( l_fname(1) NE '*'

        AND l_fname(1) NE '"' ).



      l_fname = html_table.

      CONDENSE l_fname.



      SPLIT l_fname AT space INTO TABLE lt_split_tab.



      LOOP AT lt_split_tab ASSIGNING <fs_split_tab1>.



        IF   <fs_split_tab1> = 'INCLUDE'

          OR <fs_split_tab1> = 'SUBMIT'

          OR <fs_split_tab1> = 'include'

          OR <fs_split_tab1> = 'submit'.



* Found so next line is the INCLUDE or SUBMIT program

          l_tabix = sy-tabix + 1.

          READ TABLE lt_split_tab ASSIGNING <fs_split_tab2> INDEX l_tabix.



          l_line1 = <fs_split_tab2>.



          REPLACE ALL OCCURRENCES OF '.' IN l_line1 WITH ''.

          REPLACE ALL OCCURRENCES OF cr  IN l_line1 WITH ''.





          l_len = strlen( <fs_split_tab2> ).



          IF   <fs_split_tab2>(1) = 'Z'

            OR <fs_split_tab2>(1) = 'Y'

            OR <fs_split_tab2>(1) = 'z'

            OR <fs_split_tab2>(1) = 'y'



          OR ( l_len GE 5

            AND ( <fs_split_tab2>(5) = '/GLB/'

               OR <fs_split_tab2>(5) = '/glb/' ) ).



* Add HTML Link Code

            CONCATENATE '<a href="./'

                        l_line1

                        '.html">'

                        <fs_split_tab2>

                        '</a>'

                   INTO l_line2.



            REPLACE ALL OCCURRENCES OF l_line1 IN html_table WITH l_line2.



            IF sy-subrc NE 0.

              TRANSLATE l_line1 TO LOWER CASE.

              REPLACE ALL OCCURRENCES OF l_line1 IN html_table WITH l_line2.

            ENDIF.



            EXIT.



          ENDIF.



        ENDIF.



      ENDLOOP.



    ELSE.



* Change the ABAP to add the links to the tables

      REFRESH i_lines_in.

      APPEND contents TO i_lines_in.



      l_upd = rs_c_true.



      PERFORM find_custom_dict_structures2 TABLES i_prog

                                                  table_names

                                                  i_lines_in

                                            USING i_prog-prog

                                                  l_upd.



      LOOP AT i_lines_in.

        html_table = i_lines_in.

      ENDLOOP.



    ENDIF.



    APPEND html_table.



  ENDLOOP.



  html_table = '</pre>'.

  APPEND html_table.



  html_table = hr.

  APPEND html_table.



  CONCATENATE footer_message br INTO html_table.

  APPEND html_table.



  html_table = '</body>'.

  APPEND html_table.



  html_table = '</html>'.

  APPEND html_table.



  contents[] = html_table[].



ENDFORM.                    "CONVERT_TABLE_TO_HTML



************************************************************************

*********************DOWNLOAD ROUTINES**********************************

************************************************************************



*-----------------------------------------------------------------------

* DOWNLOAD_DD_STRUCTURES... download database objects to file

*-----------------------------------------------------------------------

FORM download_dd_structures TABLES i_dict STRUCTURE i_dictionary

                            USING value(pathname).



  DATA: filename    LIKE rlgrap-filename.

  DATA: i_dict_html LIKE w3html OCCURS 0 WITH HEADER LINE.

  DATA: l_title     TYPE char100.

  DATA : l_ddtx TYPE char100.



  LOOP AT i_dict.



* <a id="Dictionary-ygttxltg_doc_num">DOC_NUM</a>

    CONCATENATE '¬a id="./Dictionary-'

              i_dict-tabname

              '-'

              i_dict-fieldname

              '"¬¬'

              INTO l_ddtx.

    TRANSLATE l_ddtx TO LOWER CASE.





    CONCATENATE l_ddtx

            i_dict-fieldname

            '¬/a¬¬'

       INTO i_dict-fieldname.



    MODIFY i_dict.



  ENDLOOP.



  LOOP AT i_dict.



    l_title = i_dict-tabtext.



    MOVE-CORRESPONDING i_dict TO i_dict_minus_tabname.

    APPEND i_dict_minus_tabname.



    AT END OF tabname.



      CLEAR   i_dict_html.

      REFRESH i_dict_html.



      CONCATENATE 'Converting table'

                  i_dict-tabname

                  'to html'

             INTO mess SEPARATED BY space.



      PERFORM display_status USING mess 0.



      TRANSLATE i_dict-tabname TO LOWER CASE.

      TRANSLATE p_hex          TO LOWER CASE.



      CONCATENATE pathname

                  '\Dictionary-'

                  i_dict-tabname

                  '.'

                  p_hex

             INTO filename.



      CONCATENATE i_dict-tabname

                  '-'

                  l_title

             INTO l_title SEPARATED BY space.



      PERFORM convert_dd_to_html TABLES i_dict_minus_tabname

                                        i_dict_html

                                 USING  l_title.



* Views

      IF i_dict-tabname CS '_DMN'.

* do nothings



      ELSE.

        PERFORM do_views  USING i_dict.

      ENDIF.



      DATA l_fnma TYPE char50.

      l_fnma = i_dict-tabname.

      TRANSLATE l_fnma TO UPPER CASE.



      LOOP AT t_html_all ASSIGNING <fs_html_all>

        WHERE tabname = l_fnma.

        MOVE-CORRESPONDING <fs_html_all> TO i_dict_html.

        APPEND i_dict_html.

      ENDLOOP.



      PERFORM ws_download TABLES i_dict_html

                          USING  filename.



      CLEAR   i_dict_html.

      REFRESH i_dict_html.



      CLEAR   i_dict_minus_tabname.

      REFRESH i_dict_minus_tabname.



    ENDAT.



  ENDLOOP.



ENDFORM.                    "DOWNLOAD_DD_STRUCTURES



*-----------------------------------------------------------------------

*  DOWNLOAD_FUNCTIONS...       Download function modules to file.

*-----------------------------------------------------------------------

FORM download_functions TABLES i_func STRUCTURE i_functions

                               USING value(p_path).



  DATA: inc_number(4).

  DATA: file_ext(4).

  DATA: func_path LIKE rlgrap-filename.

  DATA: html_name(110).



  DATA: BEGIN OF i_loc_includes OCCURS 0,

          include LIKE d010inc-include,

        END OF i_loc_includes.



  LOOP AT i_func.



    CLEAR i_loc_includes.

    REFRESH i_loc_includes.



*--- Function module

    PERFORM func_include_name USING i_func-pname

                                    i_func-include

                                    i_loc_includes-include

                                    0.



    APPEND i_loc_includes.



*--- Global declarations

    PERFORM func_include_name USING i_func-pname

                                    i_func-include

                                    i_loc_includes-include

                                    1.



    APPEND i_loc_includes.



    LOOP AT i_loc_includes.



      READ REPORT i_loc_includes-include INTO content.



      TRANSLATE i_functions-stext TO LOWER CASE.



*--- Create filename and convert contents to HTML if applicable

      IF p_html = 'X'.

        IF i_loc_includes-include CS 'TOP'.

          CONCATENATE 'Global-'

                      i_func-funcname

                  INTO html_name SEPARATED BY space.

        ELSE.

          html_name = i_func-funcname.

        ENDIF.



        PERFORM convert_table_to_html TABLES content

                                      USING html_name.

        file_ext = p_hex.

      ELSE.

        file_ext = p_tex.

      ENDIF.



      IF i_loc_includes-include CS 'TOP'.

        CONCATENATE p_path

                    '\Global-'

                    i_func-funcname '.'

                    file_ext

               INTO func_path.

      ELSE.

        CONCATENATE p_path

                    '\'

                    i_func-funcname

                    '.'

                    file_ext

               INTO func_path.

      ENDIF.



      TRANSLATE func_path TO LOWER CASE.



      PERFORM ws_download TABLES content

                          USING func_path.



      CLEAR content. REFRESH content.

    ENDLOOP.



    IF p_doc = 'X'.

      PERFORM download_func_documentation USING i_func-funcname

                                                i_func-stext.

    ENDIF.

  ENDLOOP.



  LOOP AT i_prog_includes.

    PERFORM read_main_code_and_download TABLES i_texttab

                                               i_texttab

                                               i_messages

                                         USING i_prog_includes-prog.

  ENDLOOP.

ENDFORM.                    "DOWNLOAD_FUNCTIONS



*-----------------------------------------------------------------------

* DOWNLOAD_FUNCTION_DOCUMENTATION...

*-----------------------------------------------------------------------

FORM download_func_documentation USING function_name

                                       description.



  DATA: i_lines LIKE content OCCURS 0 WITH HEADER LINE.

  DATA: i_script_lines LIKE tline OCCURS 0 WITH HEADER LINE.

  DATA: html_page_name(100).

  DATA: doc_path LIKE rlgrap-filename.

  DATA: object LIKE dokhl-object.



  MOVE function_name TO object.



  CALL FUNCTION 'DOCU_GET'

    EXPORTING

      id                     = 'FU'

      langu                  = sy-langu

      object                 = object

      typ                    = 'T'

      version_active_or_last = 'L'

    TABLES

      line                   = i_script_lines

    EXCEPTIONS

      no_docu_on_screen      = 1

      no_docu_self_def       = 2

      no_docu_temp           = 3

      ret_code               = 4

      OTHERS                 = 5.



  IF sy-subrc = 0

    AND NOT ( i_script_lines[] IS INITIAL ).



    APPEND 'SHORT TEXT' TO i_lines.

    APPEND description  TO i_lines.

    APPEND space        TO i_lines.



    LOOP AT i_script_lines.

      MOVE i_script_lines-tdline TO i_lines-line.



      WHILE i_lines-line CP '&*'

        OR i_lines-line CP '*&'.

        REPLACE '&' WITH '' INTO i_lines-line.

      ENDWHILE.



      APPEND i_lines.

    ENDLOOP.



    CONCATENATE 'Documentation -'

                function_name

           INTO html_page_name SEPARATED BY space.



    PERFORM convert_table_to_html TABLES i_lines

                                  USING  html_page_name.



    CONCATENATE p_path '\Docs-' function_name '.'

                                      p_hex

                                      INTO doc_path.

    TRANSLATE doc_path TO LOWER CASE.



    PERFORM ws_download TABLES i_lines

                        USING doc_path.



  ENDIF.



ENDFORM.                    "DOWNLOAD_FUNC_DOCUMENTATION



*-----------------------------------------------------------------------

*  DOWNLOAD_PROGRAMS..

*-----------------------------------------------------------------------

FORM download_programs TABLES i_prog STRUCTURE i_programmes

                                     USING value(p_path).



  DATA : wa_dict_struct  LIKE i_dictionary.

  DATA : i_loc_tab_names LIKE table_names   OCCURS 0 WITH HEADER LINE.

  DATA : i_loc_dict      TYPE t_dict_struct OCCURS 0 WITH HEADER LINE.



  DATA : wa_includes     TYPE t_includes.

  DATA : i_loc_functions TYPE t_functions   OCCURS 0 WITH HEADER LINE.

  DATA : wa_functions    TYPE t_functions.



  SORT i_prog ASCENDING BY prog.

  DELETE ADJACENT DUPLICATES FROM i_prog COMPARING prog.



  LOOP AT i_prog.



*-- Download screens.

    PERFORM download_screens TABLES i_prog-screen_flow

                             USING  i_prog-prog.



*-- Download dictionary objects

    LOOP AT i_prog-dict_struct INTO wa_dict_struct.

      MOVE wa_dict_struct-tabname TO i_loc_tab_names-tabname.

      APPEND i_loc_tab_names.

    ENDLOOP.



*--- Temporary stops multiple objects with the same name being

*      downloaded to the same directory.

    LOOP AT i_loc_tab_names.

      SORT i_downloaded ASCENDING BY object.



      READ TABLE i_downloaded

        WITH KEY object = i_loc_tab_names-tabname.



      IF sy-subrc = 0.

        DELETE i_loc_tab_names.

      ENDIF.

    ENDLOOP.



    IF NOT ( i_loc_tab_names[] IS INITIAL ).



      REFRESH new_table_names.



* Tables

      PERFORM find_table_definition TABLES i_loc_dict

                                           i_loc_tab_names.



      PERFORM download_dd_structures TABLES i_loc_dict

                                     USING  p_path.



* Download any new DD objects

      IF NOT new_table_names[] IS INITIAL.



        PERFORM find_table_definition TABLES i_loc_dict

                                             new_table_names.



        PERFORM download_dd_structures TABLES i_loc_dict

                                       USING  p_path.



      ENDIF.



    ENDIF.



*--- Temporary

    LOOP AT i_loc_tab_names.

      APPEND i_loc_tab_names TO i_downloaded.

    ENDLOOP.



*-- Download function modules



*--- Temporary stops multiple objects with the same name being

*      downloaded to the same directory.



    LOOP AT i_prog-functions INTO wa_functions.

      SORT i_downloaded ASCENDING BY object.

      READ TABLE i_downloaded

        WITH KEY object = wa_functions-funcname.



      IF sy-subrc = 0.

        DELETE i_prog-functions.

      ENDIF.

    ENDLOOP.



    IF NOT ( i_prog-functions[] IS INITIAL ).

      PERFORM retrieve_functions TABLES s_fname

                                        s_fgroup

                                        i_prog-functions

                                        i_loc_functions

                                 USING 0.



      PERFORM download_functions TABLES i_loc_functions

                                 USING p_path.

    ENDIF.



*--- Temporary

    LOOP AT i_loc_functions.

      MOVE i_loc_functions-funcname TO i_downloaded.

      APPEND i_downloaded.

    ENDLOOP.



*-- Download includes

    LOOP AT i_prog-includes INTO wa_includes.

      PERFORM read_main_code_and_download TABLES i_texttab

                                                 i_texttab

                                                 i_messages

                                          USING wa_includes-prog.

    ENDLOOP.



*-- Main program

    PERFORM read_main_code_and_download TABLES i_prog-text_elements

                                               i_prog-selection_texts

                                                 i_prog-messages

                                        USING i_prog-prog.



    CLEAR content.

    REFRESH content.



    CLEAR i_loc_dict.

    REFRESH i_loc_dict.



    CLEAR i_loc_tab_names.

    REFRESH i_loc_tab_names.



    CLEAR i_loc_functions.

    REFRESH i_loc_functions.



  ENDLOOP.



ENDFORM.                    "DOWNLOAD_PROGRAMS



*-----------------------------------------------------------------------

*   FIND_MAIN_CODE_AND_DOWNLOAD...

*-----------------------------------------------------------------------

FORM read_main_code_and_download TABLES text_elements   STRUCTURE i_texttab

                                        selection_texts STRUCTURE i_texttab

                                        messages        STRUCTURE i_messages

                                 USING  progname.



  DATA: i_lines    LIKE content OCCURS 0 WITH HEADER LINE.

  DATA: w_filename LIKE rlgrap-filename.



  READ REPORT progname INTO i_lines.



*-- download text elements and selection texts for main program

  PERFORM append_text_elements TABLES text_elements

                                      selection_texts

                                      i_lines.



*-- download messages classes for main program.

  PERFORM append_messages_to_file TABLES messages

                                         i_lines.



  IF ( p_txt = 'X' ).



    CONCATENATE p_path

                '\'

                progname

                '.'

                p_tex

           INTO w_filename.



    TRANSLATE w_filename TO LOWER CASE.

    PERFORM ws_download TABLES i_lines USING w_filename.



  ELSE.



    PERFORM convert_table_to_html TABLES i_lines

                                  USING progname.



    CONCATENATE p_path

                '\'

                progname

                '.'

                p_hex

           INTO w_filename.



    TRANSLATE w_filename TO LOWER CASE.



    PERFORM ws_download TABLES i_lines USING w_filename.



  ENDIF.



ENDFORM.                    "READ_MAIN_CODE_AND_DOWNLOAD



*-----------------------------------------------------------------------

* APPEND_TEXTS_TO_FILE...

*-----------------------------------------------------------------------

FORM append_text_elements TABLES text_elements   STRUCTURE i_texttab

                                 selection_texts STRUCTURE i_texttab

                                 i_lines         STRUCTURE content.



  DATA: w_lines TYPE i VALUE 0.



  DESCRIBE TABLE text_elements LINES w_lines.



  IF w_lines > 0.



    DO 3 TIMES.

      i_lines-line = ''.

      APPEND i_lines.

    ENDDO.



    i_lines-line = '*Text elements'.

    APPEND i_lines.



    i_lines-line = '*-------------'.

    APPEND i_lines.



    LOOP AT text_elements.

      i_lines-line+0(2)  = '* '.

      i_lines-line+2(74) = text_elements.

      APPEND i_lines.

    ENDLOOP.



  ENDIF.



  DESCRIBE TABLE selection_texts LINES w_lines.



  IF w_lines > 0.



    DO 3 TIMES.

      i_lines-line = ''.

      APPEND i_lines.

    ENDDO.



    i_lines-line = '*Selection texts'.

    APPEND i_lines.



    i_lines-line = '*---------------'.

    APPEND i_lines.



    LOOP AT selection_texts.

      i_lines-line+0(1)  = '*'.

      i_lines-line+1(75) = selection_texts.

      APPEND i_lines.

    ENDLOOP.



  ENDIF.



ENDFORM.                    "APPEND_TEXT_ELEMENTS



*----------------------------------------------------------------------

*  APPEND_MESSAGES_TO_FILE

*----------------------------------------------------------------------

FORM append_messages_to_file TABLES messages STRUCTURE i_messages

                                    i_lines STRUCTURE content.



  DATA: w_lines    TYPE i VALUE 0,

        prev_msgid LIKE messages-msgid.



  DESCRIBE TABLE messages LINES w_lines.



  IF w_lines > 0.



    DO 2 TIMES.

      i_lines-line = ''.

      APPEND i_lines.

    ENDDO.



    i_lines-line = '*Messages'.

    APPEND i_lines.



    i_lines-line = '*-------------'.

    APPEND i_lines.



    LOOP AT messages.

      IF ( messages-msgid <> prev_msgid ).

        CONCATENATE '*'

                    'Message class:'

                    messages-msgid

               INTO i_lines-line SEPARATED BY space.



        prev_msgid = messages-msgid.

        APPEND i_lines.

      ENDIF.



      CONCATENATE '*'

                  messages-msgnr

                  messages-text

             INTO i_lines-line SEPARATED BY space.



      APPEND i_lines.

    ENDLOOP.



  ENDIF.

ENDFORM.                    " APPEND_MESSAGES_TO_FILE



*-----------------------------------------------------------------------

*  DOWNLOAD_SCREENS...

*-----------------------------------------------------------------------

FORM download_screens TABLES screen_flow STRUCTURE i_screen_flow

                      USING value(progname).



  DATA: header     LIKE d020s.

  DATA: fields     LIKE d021s OCCURS 0 WITH HEADER LINE.

  DATA: flow       LIKE d022s OCCURS 0 WITH HEADER LINE.

  DATA: w_filename LIKE rlgrap-filename.



  LOOP AT screen_flow.



    CLEAR header.

    CLEAR fields[].

    CLEAR flow[].



    CALL FUNCTION 'RS_IMPORT_DYNPRO'

      EXPORTING

        dylang = sy-langu

        dyname = progname

        dynumb = screen_flow-screen

      IMPORTING

        header = header

      TABLES

        ftab   = fields

        pltab  = flow.



    CONCATENATE progname

                'screen'

                screen_flow-screen

           INTO w_filename SEPARATED BY space.



    CONCATENATE p_path

                '\'

                w_filename

                '.txt'

           INTO w_filename.



    CALL FUNCTION 'RS_DYNPRO_DOWNLOAD'

      EXPORTING

        header    = header

        descript  = ''

        file      = w_filename

      TABLES

        fields    = fields

        flowlogic = flow.



  ENDLOOP.



ENDFORM.                    "DOWNLOAD_SCREENS



*-----------------------------------------------------------------------

*  WS_DOWNLOAD...    Write an internal table to file

*-----------------------------------------------------------------------

FORM ws_download TABLES i_download

                 USING filename.



  DATA l_filename TYPE localfile.





  DATA: BEGIN OF content OCCURS 0,

          line(255),

        END OF content.



  content[] = i_download[].

  l_filename = filename.





  FIELD-SYMBOLS <fs_w3_html> TYPE ANY.



  IF l_filename CA '/'.

    REPLACE ALL OCCURRENCES OF '/GLB/' IN l_filename WITH '_GLB_'.

    REPLACE ALL OCCURRENCES OF '/glb/' IN l_filename WITH '_glb_'.

  ENDIF.



  LOOP AT content ASSIGNING <fs_w3_html>.

    REPLACE ALL OCCURRENCES OF '/GLB/' IN <fs_w3_html> WITH '_GLB_'.

    REPLACE ALL OCCURRENCES OF '/glb/' IN <fs_w3_html> WITH '_glb_'.

  ENDLOOP.



  i_download[] = content[].





  CALL FUNCTION 'WS_DOWNLOAD'

    EXPORTING

      filename = l_filename

      filetype = 'ASC'

    TABLES

      data_tab = i_download.



ENDFORM.                    "WS_DOWNLOAD



************************************************************************

**************************DISPLAY ROUTINES******************************

************************************************************************



*-----------------------------------------------------------------------

*  FILL_TREE_NODE_PROGRAMS

*-----------------------------------------------------------------------

FORM fill_tree_node_programs TABLES i_prog STRUCTURE i_programmes.



  DATA: w_prog         TYPE t_programmes.

  DATA: w_node         LIKE snodetext.

  DATA: wa_screens     TYPE t_screen_flow.

  DATA: wa_messages    TYPE t_messages.

  DATA: wa_includes    TYPE t_includes.

  DATA: wa_dict_struct TYPE t_dict_struct.

  DATA: wa_func_struct TYPE t_functions.

  DATA: strlength      TYPE i.

  DATA: text(255).

  DATA: w_lines(4)     TYPE c.



  DESCRIBE TABLE i_prog LINES w_lines.



  IF w_lines = 1.

    CONCATENATE w_lines 'programs downloaded' INTO w_node-text2

                                             SEPARATED BY space.

  ELSE.

    CONCATENATE w_lines 'programs downloaded' INTO w_node-text2

                                             SEPARATED BY space.

  ENDIF.



  run_time = sy-uzeit - start_time.

  WRITE run_time TO run_time_char.

  CONCATENATE w_node-text2 '- runtime' run_time_char INTO w_node-text2

                                         SEPARATED BY space.



*--- Include header display record.

  w_node-tlevel    = '1'.

  w_node-tlength2  = 45.

  w_node-tcolor2   = 1.

  APPEND w_node TO i_node.



  LOOP AT i_prog.



*--- Main programs.

    w_node-tlevel     = '2'.

    CONCATENATE 'Program -' i_prog-text INTO text SEPARATED BY space.



    w_node-text2      = i_prog-prog.

*    Description

    strlength         =  strlen( text ).

    w_node-tlength3   = strlength.

    w_node-tcolor3    = 4.

    w_node-tpos3      = 60.

    w_node-text3      = text.

*     write 'Main Program' to w_node-text3.

    APPEND w_node TO i_node.



*--- Screens.

    LOOP AT i_prog-screen_flow INTO wa_screens.

      w_node-tlevel     = '3'.

      w_node-text2      = wa_screens-screen.

      w_node-tlength3   = 6.

      w_node-tcolor3    = 4.

      w_node-tpos3      = 60.

      w_node-text3      = 'Screen'.

      APPEND w_node TO i_node.

    ENDLOOP.



*--- Message Classes.

    LOOP AT i_prog-messages INTO wa_messages.

      AT NEW msgid.

        w_node-tlevel     = '3'.

        w_node-text2      = wa_messages-msgid.

        w_node-tlength3   = 14.

        w_node-tcolor3    = 4.

        w_node-tpos3      = 60.

        w_node-text3      = 'Message class'.

        APPEND w_node TO i_node.

      ENDAT.

    ENDLOOP.



*--- Includes

    LOOP AT i_prog-includes INTO wa_includes.

      w_node-tlevel     = '3'.

      w_node-text2      = wa_includes-prog.

      w_node-tlength3   = 8.

      w_node-tcolor3    = 4.

      w_node-tpos3      = 60.

      w_node-text3      = 'Include'.

      APPEND w_node TO i_node.

    ENDLOOP.



*--- Dictionary structures

    LOOP AT i_prog-dict_struct INTO wa_dict_struct.

      w_node-tlevel     = '3'.

      w_node-text2      = wa_dict_struct-tabname.

      w_node-tlength3   = 17.

      w_node-tcolor3    = 4.

      w_node-tpos3      = 60.

      w_node-text3      = 'Dictionary object'.

      APPEND w_node TO i_node.

    ENDLOOP.



*--- Function Modules

    LOOP AT i_prog-functions INTO wa_func_struct.

      w_node-tlevel     = '3'.

      w_node-text2      = wa_func_struct-funcname.

      w_node-tlength3   = 17.

      w_node-tcolor3    = 4.

      w_node-tpos3      = 60.

      w_node-text3      = 'Function Module'.

      APPEND w_node TO i_node.

    ENDLOOP.



  ENDLOOP.



ENDFORM.                    " FILL_TREE_NODE



*-----------------------------------------------------------------------

*  FILL_TREE_NODE_TABLES...

*-----------------------------------------------------------------------

FORM fill_tree_node_tables TABLES i_dict STRUCTURE i_dictionary.



  DATA: w_lines(4)     TYPE c VALUE '0'.

  DATA: w_node         LIKE snodetext.

  DATA: wa_dict_struct TYPE t_dict_struct.



  LOOP AT i_dict.

    AT NEW tabname.

      w_lines = w_lines + 1.

    ENDAT.

  ENDLOOP.



  IF w_lines = 1.

    CONCATENATE w_lines 'table downloaded' INTO w_node-text2

                                           SEPARATED BY space.

  ELSE.

    CONCATENATE w_lines 'tables downloaded' INTO w_node-text2

                                            SEPARATED BY space.

  ENDIF.



  run_time = sy-uzeit - start_time.

  WRITE run_time TO run_time_char.

  CONCATENATE w_node-text2 '- runtime' run_time_char INTO w_node-text2

                                                     SEPARATED BY space.



*--- include header display record.

  w_node-tlevel     = '1'.

  w_node-tlength2   = 45.

  w_node-tcolor2    = 1.

  APPEND w_node TO i_node.



  LOOP AT i_dict.



    MOVE-CORRESPONDING i_dict TO wa_dict_struct.



    AT NEW tabname.

      w_node-tlevel     = '2'.

      w_node-text2      = wa_dict_struct-tabname.



      w_node-tlength3   = 60.

      w_node-tcolor3    = 4.

      w_node-tpos3      = 60.

      w_node-text3 = wa_dict_struct-tabtext.



      APPEND w_node TO i_node.

    ENDAT.



  ENDLOOP.



ENDFORM.                    "FILL_TREE_NODE_TABLES



*-----------------------------------------------------------------------

*  FILL_TREE_NODE_FUNCTIONS...

*-----------------------------------------------------------------------

FORM fill_tree_node_functions TABLES i_func STRUCTURE i_functions.



  DATA: w_lines(4) TYPE c VALUE '0'.

  DATA: w_node     LIKE snodetext.



  DESCRIBE TABLE i_func LINES w_lines.

  IF w_lines = 1.

    CONCATENATE w_lines 'function downloaded' INTO w_node-text2

                                              SEPARATED BY space.

  ELSE.

    CONCATENATE w_lines 'functions downloaded' INTO w_node-text2

                                               SEPARATED BY space.

  ENDIF.



  run_time = sy-uzeit - start_time.

  WRITE run_time TO run_time_char.

  CONCATENATE w_node-text2 '- runtime' run_time_char INTO w_node-text2

                                                     SEPARATED BY space.

*--- include header display record.

  w_node-tlevel = '1'.

  w_node-tlength2  = 45.

  w_node-tcolor2    = 1.

  APPEND w_node TO i_node.



  SORT i_func ASCENDING BY funcname.



  LOOP AT i_func.

    w_node-tlevel     = '2'.

    w_node-text2      = i_func-funcname.

    w_node-tlength3   = 74.

    w_node-tcolor3    = 4.

    w_node-tpos3      = 60.

    w_node-text3      = i_func-stext.



    APPEND w_node TO i_node.

  ENDLOOP.



  LOOP AT i_prog_includes.

    w_node-tlevel     = '2'.

    w_node-text2      = i_prog_includes-prog.

    w_node-tlength3   = 74.

    w_node-tcolor3    = 4.

    w_node-tpos3      = 60.

    w_node-text3      = 'Include'.



    APPEND w_node TO i_node.

  ENDLOOP.



ENDFORM.                    "FILL_TREE_NODE_FUNCTIONS



*-----------------------------------------------------------------------



* DISPLAY_TREE...



*-----------------------------------------------------------------------



FORM display_tree TABLES i_nodes STRUCTURE snodetext.



  DATA: w_node LIKE snodetext.



* build up the tree from the internal table node

  CALL FUNCTION 'RS_TREE_CONSTRUCT'

    TABLES

      nodetab            = i_node

    EXCEPTIONS

      tree_failure       = 1

      id_not_found       = 2

      wrong_relationship = 3

      OTHERS             = 4.



* get the first index and expand the whole tree

  READ TABLE i_node INTO w_node INDEX 1.



  CALL FUNCTION 'RS_TREE_EXPAND'

    EXPORTING

      node_id   = w_node-id

      all       = 'X'

    EXCEPTIONS

      not_found = 1

      OTHERS    = 2.



* now display the tree



  CALL FUNCTION 'RS_TREE_LIST_DISPLAY'

    EXPORTING

      callback_program      = sy-cprog

      callback_user_command = 'CB_USER_COMMAND'

      callback_text_display = 'CB_TEXT_DISPLAY'

      callback_top_of_page  = 'TOP_OF_PAGE'

    EXCEPTIONS

      OTHERS                = 1.



ENDFORM.                    " DISPLAY_TREE





*&---------------------------------------------------------------------*

*&      Form  do_views

*&---------------------------------------------------------------------*

*       text

*----------------------------------------------------------------------*

*      -->I_DICT     text

*      -->TABLENAMES text

*      -->P_PATH     text

*----------------------------------------------------------------------*

FORM do_views   USING i_dict     STRUCTURE i_dictionary.



  DATA : gotstate   LIKE dcobjif-gotstate.

  DATA : dd02v_wa   LIKE dd02v.

  DATA : dd09l_wa   LIKE dd09l.

  DATA : definition LIKE dd03p OCCURS 0 WITH HEADER LINE.

  DATA : dd01v_wa   TYPE dd01v.

  DATA : dd07v_tab  TYPE TABLE OF dd07v WITH HEADER LINE.

  DATA : dd04v_wa   LIKE dd04v.

  DATA : tpara_wa   LIKE tpara.

  DATA : l_namein   TYPE ddobjname.



  DATA : lt_dd26v   TYPE TABLE OF dd26v.

  DATA : lt_dd27p   TYPE TABLE OF dd27p.

  DATA : lt_dd28j   TYPE TABLE OF dd28j.

  DATA : lt_dd28v   TYPE TABLE OF dd28v.

  DATA : l_dd25v    TYPE dd25v.

  DATA : l_dd09l    TYPE dd09l.



  TYPES: BEGIN OF ty_dd27p_mini,

     viewname    TYPE viewname,

     objpos      TYPE mcpos,

     ddlanguage  TYPE ddlanguage,

     viewfield   TYPE fieldname,

     tabname     TYPE tabname,

     fieldname   TYPE fieldname,

     keyflag     TYPE mckeyflag,

     mcfieldid   TYPE mcfieldid,

     eform       TYPE formrout,

     rollname    TYPE rollname,

     rollnamevi  TYPE rollnamevi,

  END OF ty_dd27p_mini.



  DATA l_dd27p_mini  TYPE ty_dd27p_mini.

  DATA lt_dd27p_mini TYPE TABLE OF ty_dd27p_mini.



  FIELD-SYMBOLS <fs_dd27p> TYPE dd27p.



  l_namein = i_dict-tabname.

  TRANSLATE l_namein TO UPPER CASE.



  CALL FUNCTION 'DDIF_VIEW_GET'

    EXPORTING

      name          = l_namein

      state         = 'A'

      langu         = 'E'

    IMPORTING

      gotstate      = gotstate

      dd25v_wa      = l_dd25v

      dd09l_wa      = l_dd09l

    TABLES

      dd26v_tab     = lt_dd26v " V-VIEWTABLE: View of Base Tables of a View

      dd27p_tab     = lt_dd27p " View of Fields in an SAP Table View

      dd28j_tab     = lt_dd28j " Internal structure for joins of views

      dd28v_tab     = lt_dd28v " View on a Selection Condition

    EXCEPTIONS

      illegal_input = 1

      OTHERS        = 2.



  IF sy-subrc = 0

    AND gotstate = 'A'.



* Shrink 'DD27P'

    REFRESH lt_dd27p_mini.



    LOOP AT lt_dd27p ASSIGNING <fs_dd27p>.

      MOVE-CORRESPONDING <fs_dd27p> TO l_dd27p_mini.

      APPEND l_dd27p_mini TO lt_dd27p_mini.

    ENDLOOP.



    PERFORM add_tab TABLES lt_dd26v

                    USING  'DD26V'

                           l_namein.



    PERFORM add_tab TABLES lt_dd27p_mini

                    USING  'DD27P'

                           l_namein.



    PERFORM add_tab TABLES lt_dd28j

                    USING  'DD28J'

                           l_namein.



    PERFORM add_tab TABLES lt_dd28v

                    USING  'DD28V'

                           l_namein.



  ENDIF.



ENDFORM.                    " DO_VIEWS

*&---------------------------------------------------------------------*

*&      Form  ADD_TAB

*&---------------------------------------------------------------------*

*       text

*----------------------------------------------------------------------*

*      -->P_7299   text

*----------------------------------------------------------------------*

FORM add_tab  TABLES   lt_tab

              USING    value(p_tab)

                       l_tabname.



  FIELD-SYMBOLS <fs_dd03l> TYPE dd03l.

  DATA : lt_dd03l TYPE TABLE OF dd03l.



  REFRESH lt_dd03l.

  REFRESH t_header.

  REFRESH t_fields.



  SELECT * FROM dd03l

    INTO CORRESPONDING FIELDS OF TABLE lt_dd03l

    WHERE tabname  = p_tab

      AND as4local = 'A'.



  SORT lt_dd03l BY position.



  IF p_tab = 'DD27P'.

    DELETE lt_dd03l WHERE position GT 11.

  ENDIF.



  LOOP AT lt_dd03l ASSIGNING <fs_dd03l>.



    w_head-text = <fs_dd03l>-fieldname.



*-Populate the Column Headings

    CALL FUNCTION 'WWW_ITAB_TO_HTML_HEADERS'

      EXPORTING

        field_nr  = sy-tabix

        text      = w_head-text

        justified = '"Left"'

        fgcolor   = 'black'

        bgcolor   = 'white'

        size      = '2'

        font      = '"Arial"'

      TABLES

        header    = t_header.



*-Populate Column Properties

    CALL FUNCTION 'WWW_ITAB_TO_HTML_LAYOUT'

      EXPORTING

        field_nr = sy-tabix

        fgcolor  = 'black'

        size     = '1'

      TABLES

        fields   = t_fields.



  ENDLOOP.



*-title of the display

  CONCATENATE p_tab

              ''

         INTO wa_header-text SEPARATED BY space.



  wa_header-font = 'Arial'.

  wa_header-size = '4'.



*-preparing the html from intenal table

  REFRESH t_html.



  CALL FUNCTION 'WWW_ITAB_TO_HTML'

    EXPORTING

      table_header = wa_header

    TABLES

      html         = t_html

      fields       = t_fields

      row_header   = t_header

      itable       = lt_tab.



  PERFORM re_format_html TABLES t_html.



  LOOP AT t_html ASSIGNING <fs_html>.



    MOVE-CORRESPONDING <fs_html> TO t_html_all.

    t_html_all-tabname = l_tabname.

    APPEND t_html_all.



  ENDLOOP.



ENDFORM.                    " ADD_TAB


