*----------------------------------------------------------------------*
***INCLUDE MFCJ0F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  WRITE_TFBUF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_F_COMP_CODE  text
*      -->P_F_CAJO_NUMBER  text
*      -->P_F_DISPLAY_PERIOD_LO  text
*      -->P_F_DISPLAY_PERIOD_HI  text
*      -->P_GD_DEFAULTS  text
*----------------------------------------------------------------------*
FORM write_tfbuf USING    p_f_comp_code TYPE bukrs
                          p_f_cajo_number TYPE cjnr
                          p_f_display_period_lo LIKE sy-datum
                          p_f_display_period_hi LIKE sy-datum.

* Write mandatory fields into persistent defaults buffer tfbuf
* tfbuf-buffr is structured as follows (only for the Cash Journal):
* cccc____nnnn____llllllll__hhhhhhhh
* where 'c' is a digit of comp_code, 'n' a digit of cajo_number,
* 'l' display period lo and 'h' display period hi.
  CLEAR gd_defaults.
  gd_defaults    = p_f_comp_code.
  gd_defaults+8  = p_f_cajo_number.
  gd_defaults+16 = p_f_display_period_lo.
  gd_defaults+26 = p_f_display_period_hi.

  tfbuf-buffr = gd_defaults.
  tfbuf-usnam = sy-uname.
  tfbuf-applk = 'CJ'.
  tfbuf-lfdnr = '001'.
  tfbuf-datum = sy-datlo.
  MODIFY tfbuf.


ENDFORM.                               " WRITE_TFBUF

*&---------------------------------------------------------------------*
*&      Form  PROCESS_FB00
*&---------------------------------------------------------------------*
*    User specific Customizing for Cash Journal
*----------------------------------------------------------------------*
FORM process_fb00.

  DATA: tab_fb00(10) TYPE c VALUE 'T1106',    " visible tab
        xfb00(1) TYPE c.               " FB00 has saved its values

*--------------- Information which tab will be visible -----------------
  EXPORT tab_fb00 TO MEMORY ID '%TAB_FB00%'.

*--------- Authority Check for FB00
  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = 'FB00'
    EXCEPTIONS
      ok     = 0
      OTHERS = 4.
  IF sy-subrc NE 0.
    MESSAGE s172(00) WITH 'FB00'.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

  CALL TRANSACTION 'FB00'.

* ----Read changes from FB00
  IMPORT xfb00 FROM MEMORY ID '%FB00%'.
  IF xfb00 = 'X'.
    GET PARAMETER ID 'FCJ' FIELD fcjopt.
  ENDIF.

ENDFORM.                               " PROCESS_FB00
*&---------------------------------------------------------------------*
*&      Form CHECK_VEND_CUST_CHANGED
*&---------------------------------------------------------------------*
*    Created by Note 840792
*    Check if customer/vendor has been changed from saved entry
*----------------------------------------------------------------------*
FORM CHECK_VEND_CUST_CHANGED.

CLEAR  ld_change.

SELECT COUNT( * ) FROM TCJ_POSITIONS
             WHERE COMP_CODE       = itcj_e_postings-COMP_CODE
             and   CAJO_NUMBER     = itcj_e_postings-CAJO_NUMBER
             and   FISC_YEAR       = itcj_e_postings-FISC_YEAR
             and   POSTING_NUMBER  = itcj_e_postings-POSTING_NUMBER
             and   POSITION_NUMBER = itcj_e_postings-POSITION_NUMBER
             and   CUSTOMER        = itcj_e_postings-CUSTOMER
             and   VENDOR_NO       = itcj_e_postings-VENDOR_NO.
IF sy-dbcnt EQ 0.
   ld_change = 'X'.
ENDIF.

ENDFORM.                               "CHECK_VEND_CUST_CHANGED

*&---------------------------------------------------------------------*
*&      Form CHECK_WT_RELEVANT
*&---------------------------------------------------------------------*
*    Created by Note 840792
*    Check if customer/vendor is a subjekt of withholding tax
*----------------------------------------------------------------------*
FORM CHECK_WT_RELEVANT.

IF itcj_e_postings-customer NE SPACE.
   CALL FUNCTION 'FI_WT_DETERM_RELEVANT_TYPES'
     EXPORTING
       I_BUKRS              = itcj_e_postings-comp_code
       I_ACCT               = itcj_e_postings-customer
       I_KOART              = 'D'
       I_BUDAT              = itcj_e_postings-posting_date
       I_POSTINGTIME        = '2'
   TABLES
       T_WITH               = lt_accit_wt
   EXCEPTIONS
       NOTHING_SELECTED     = 1
*       OTHERS           = 2
               .
   IF SY-SUBRC = 0.
      SET CURSOR
      FIELD 'ISCJ_E_POSTINGS-CUSTOMER' LINE gd_loop_index.
      MESSAGE E008(FCJ).
*Geben Sie in gesichertem Beleg einen Debitor ohne Quellensteuertypen
   ENDIF.
ELSEIF itcj_e_postings-vendor_no NE SPACE.
   CALL FUNCTION 'FI_WT_DETERM_RELEVANT_TYPES'
     EXPORTING
       I_BUKRS              = itcj_e_postings-comp_code
       I_ACCT               = itcj_e_postings-vendor_no
       I_KOART              = 'K'
       I_BUDAT              = itcj_e_postings-posting_date
       I_POSTINGTIME        = '2'
     TABLES
       T_WITH               = lt_accit_wt
     EXCEPTIONS
       NOTHING_SELECTED     = 1
*       OTHERS           = 2
                  .
   IF SY-SUBRC = 0.
      SET CURSOR
      FIELD 'ISCJ_E_POSTINGS-VENDOR_NO' LINE gd_loop_index.
      MESSAGE E009(FCJ).
*Geben Sie in gesichertem Beleg einen Kreditor ohne Quellensteuertypen .
   ENDIF.
ENDIF.

ENDFORM.                               "CHECK_WT_RELEVANT
*&---------------------------------------------------------------------*
*&      Form  auth_check_view
*&---------------------------------------------------------------------*
*    Created by Note 841876
*    Check authorizations: only view?
*----------------------------------------------------------------------*
*      -->ld_comp_code   TYPE bukrs
*      -->ld_cajo_number TYPE cjnr
*      <--gd_display_cj  TYPE boolean
*----------------------------------------------------------------------*
FORM auth_check_view.

* -------------------------------------------------------------------- *

* Check saving authorization for Cash Journal
  IF NOT ( ls_tcj_c_journals-begru IS INITIAL ). "Usergroup
    AUTHORITY-CHECK OBJECT 'F_FBCJ'
      ID 'ACTVT' FIELD '02'                      "Change
      ID 'BEGRU' FIELD ls_tcj_c_journals-begru.
    IF sy-subrc NE 0.                                      "1805616
    AUTHORITY-CHECK OBJECT 'F_FBCJ'
      ID 'ACTVT' FIELD '06'                      "Delete/Store
      ID 'BEGRU' FIELD ls_tcj_c_journals-begru.
       IF sy-subrc NE 0.                                   "1805616
    AUTHORITY-CHECK OBJECT 'F_FBCJ'
      ID 'ACTVT' FIELD '10'                      "Post
      ID 'BEGRU' FIELD ls_tcj_c_journals-begru.
          IF sy-subrc NE 0.                                "1805616
    AUTHORITY-CHECK OBJECT 'F_FBCJ'
      ID 'ACTVT' FIELD '32'                      "Save
      ID 'BEGRU' FIELD ls_tcj_c_journals-begru.
             IF sy-subrc NE 0.
                gd_display_cj = 'X'.             "only display
             ENDIF.                                        "1805616
          ENDIF.                                           "1805616
       ENDIF.                                              "1805616
    ENDIF.                                                "1805616
  ENDIF.                                         "begru initial?

* begin of note 1950538                                    "1950538
** Check authorization for company code                    "1805616
*  IF NOT f_comp_code IS INITIAL.
*    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
*      ID 'BUKRS' FIELD f_comp_code
*      ID 'ACTVT' FIELD '01'.            "Add/Create
*    CHECK sy-subrc NE 0.
*    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
*      ID 'BUKRS' FIELD f_comp_code
*      ID 'ACTVT' FIELD '02'.             "Change
*    CHECK sy-subrc NE 0.
*    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
*      ID 'BUKRS' FIELD f_comp_code
*      ID 'ACTVT' FIELD '06'.             "Delete
*    CHECK sy-subrc NE 0.
*    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
*      ID 'BUKRS' FIELD f_comp_code
*      ID 'ACTVT' FIELD '10'.             "Post
*    CHECK sy-subrc NE 0.
*    gd_display_cj = 'X'.                 "only display
*  ENDIF.                                                  "1805616
* end of note 1950538                                      "1950538

ENDFORM.                                         " auth_check_view

*&---------------------------------------------------------------------*
*&      Form  READ_TFBUF
*&---------------------------------------------------------------------*
*       Created by Note 852607
*       Read table TFBUF to get the values for starting cash journal
*       with the parameters of the last logon. New: check authorization
*       and simulate that not entry was found if the users authorization
*       was canceled. Afterwards Dynpro 0050 will be shown to enter
*       a new cash journal.
*----------------------------------------------------------------------*
*  -->  gd_defaults_exist
*  -->  ls_tcj_c_journals-begru
*  <--  gd_defaults_exist
*  <--  f_comp_code
*  <--  f_cajo_number
*  <--  f_display_period_lo
*  <--  f_display_period_hi
*  <--  ls_tcj_c_journals
*----------------------------------------------------------------------*
FORM READ_TFBUF.

  CLEAR gd_defaults_exist.
* Check for user defaults in table TFBUF
  SELECT SINGLE * FROM  tfbuf
         WHERE  usnam  = sy-uname
         AND    applk  = 'CJ'
         AND    lfdnr  = '001'.

  IF sy-subrc = 0.
*    tfbuf-buffer is structured as follows (only for the Cash
*    Journal):
*    cccc____nnnn____llllllll__hhhhhhhh
*    where 'c' is a digit of comp_code, n' a digit of cajo_number,
*    'l' display period lo and 'h' display period hi.
     f_comp_code   = tfbuf-buffr(4).
     f_cajo_number = tfbuf-buffr+8(4).
     f_display_period_lo = tfbuf-buffr+16(8).
     f_display_period_hi = tfbuf-buffr+26(8).

*    Read details of the cash journal (User group)
     CLEAR ls_tcj_c_journals.
     CALL FUNCTION 'FCJ_GET_CAJO_DATA2'
       EXPORTING
         i_comp_code             = f_comp_code
         i_cajo_number           = f_cajo_number
         i_langu                 = sy-langu
       IMPORTING
         E_TCJ_C_JOURNALS        = ls_tcj_c_journals
*        E_TCJ_CJ_NAMES          =
       EXCEPTIONS
         CAJO_NOT_EXISTENT       = 1
        OTHERS            = 2.
     IF sy-subrc = 0.
*       check if user is still authorised for this cash journal,
*       otherwise Dynpro 0050 should occur to enter another cash
*       journal
        AUTHORITY-CHECK OBJECT 'F_FBCJ'
             ID 'BEGRU' FIELD ls_tcj_c_journals-begru
             ID 'ACTVT' FIELD '33'.
        IF sy-subrc = 0.
           gd_defaults_exist = 'X'.                         "1950538
*begin of note 1950538                                      "1950538
**          Check authorization for company code            "1805616
*           IF NOT f_comp_code IS INITIAL.
*              AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
*                ID 'BUKRS' FIELD f_comp_code
*                ID 'ACTVT' FIELD '03'.
*              IF sy-subrc = 0.
*                 gd_defaults_exist = 'X'.
*              ELSE.
*                 CLEAR: f_comp_code, f_cajo_number.
*              ENDIF.
*           ENDIF.                                          "1805616
*end of note 1950538                                        "1950538
        ELSE.
           CLEAR: f_comp_code, f_cajo_number.
        ENDIF.
     ELSE.
        CLEAR: f_comp_code, f_cajo_number.
     ENDIF.
  ENDIF.

ENDFORM.                    " READ_TFBUF
**&---------------------------------------------------------------------*
**&      Form  CHECK_SGTXT
**&---------------------------------------------------------------------*
**       Created by note 1448153
**       Prevent print of receipt for initial but mandatory sgtxt
**----------------------------------------------------------------------*
* Commented with note 2394995.
*FORM check_sgtxt .
*
*  CALL FUNCTION 'FCJ_GET_TRANSACTION'
*    EXPORTING
*      i_transaction     = gd_postings-transact_name
*      i_comp_code       = gd_postings-comp_code
*    TABLES
*      itcj_transactions = lt_transactions.
*  LOOP AT lt_transactions.
*    ld_trans_type = lt_transactions-transact_type.
*  ENDLOOP.
*
*  CASE ld_dynpronr.
*    WHEN '0110'.
*      CASE ld_trans_type.
*        WHEN 'E' OR 'B'.
*          ld_bschl          = '50'.
*          ld_off_bschl      = '40'.
*        WHEN 'D'.
*          ld_bschl          = '50'.
*          ld_off_bschl      = '05'.
*        WHEN 'K'.
*          ld_bschl          = '50'.
*          ld_off_bschl      = '25'.
*      ENDCASE.
*    WHEN '0120' OR '0140'.
*      CASE ld_trans_type.
*        WHEN 'R' OR 'C'.
*          ld_bschl          = '40'.
*          ld_off_bschl      = '50'.
*        WHEN 'D'.
*          ld_bschl          = '40'.
*          ld_off_bschl      = '15'.
*        WHEN 'K'.
*          ld_bschl          = '40'.
*          ld_off_bschl      = '35'.
*        WHEN 'B'.
*          ld_bschl          = '50'.
*          ld_off_bschl      = '40'.
*      ENDCASE.
*  ENDCASE.
*  CLEAR ld_trans_type.
*
** Check sgtxt for cash journal line item.
*  CLEAR ld_journals.
*  CALL FUNCTION 'FCJ_GET_CAJO_DATA2'
*    EXPORTING
*      i_comp_code      = gd_postings-comp_code
*      i_cajo_number    = gd_postings-cajo_number
*      i_langu          = sy-langu
*    IMPORTING
*      e_tcj_c_journals = ld_journals.
*
*  PERFORM feldauswahl(sapff001) USING gd_postings-comp_code
*                                  ld_journals-gl_account
*                                  ld_bschl
*                         CHANGING ld_faus1
*                                  ld_faus2.
*
*  IF ld_faus1+1(1) EQ '+'.
** Store field status of sgtxt for cash journal account.         "2100552
*    ld_sgtxt_cj = 'X'.                                          "2100552
*    IF gd_postings-position_text is initial.                    "2100552
*      MESSAGE i808(f5) WITH 'Text' gd_postings-comp_code
*      ld_journals-gl_account.
*      ld_stop_print = 'X'.
*    ENDIF.                                                      "2100552
*  ENDIF.
*
** Check sgtxt for offsetting line item.
**  IF NOT gd_postings-customer IS INITIAL.                      "1769059
**    gd_postings-gl_account = ls_debi-akont.                    "1769059
**  ELSEIF NOT gd_postings-vendor_no IS INITIAL.                 "1769059
**    gd_postings-gl_account = ls_kred-akont.                    "1769059
**  ENDIF.                                                       "1769059
*
*  If not gd_postings-gl_account is initial.                     "2190637
*    PERFORM feldauswahl(sapff001) USING gd_postings-comp_code
*                                        gd_postings-gl_account
*                                        ld_off_bschl
*                               CHANGING ld_faus1
*                                        ld_faus2.
*  ELSE.                                                         "2190637
*
** Start of note 2190637.
** Determine reconciliation account number
*     IF NOT gd_postings-umskz IS initial.
*       IF  gd_postings-vendor_no is not initial.
*          CALL FUNCTION 'FI_SPECIAL_GL_TRANSACT_DATA'
*            EXPORTING
*              i_bukrs       = gd_postings-comp_code
*              i_koart       = 'K'
*              i_saknr       = ls_kred-akont
*              i_umskz       = gd_postings-umskz
*            IMPORTING
*              E_HKONT       = gd_postings-gl_account.
*       ELSE.
*          CALL FUNCTION 'FI_SPECIAL_GL_TRANSACT_DATA'
*            EXPORTING
*              i_bukrs       = gd_postings-comp_code
*              i_koart       = 'D'
*              i_saknr       = ls_debi-akont
*              i_umskz       = gd_postings-umskz
*            IMPORTING
*              E_HKONT       = gd_postings-gl_account.
*       ENDIF.
*     ELSE.
*       IF not gd_postings-vendor_no is initial.
*          gd_postings-gl_account = ls_kred-akont.
*       ELSEIF not gd_postings-customer is initial.
*          gd_postings-gl_account = ls_debi-akont.
*       ENDIF.
*     ENDIF.
*     PERFORM feldauswahl(sapff001) USING gd_postings-comp_code
*                                         gd_postings-gl_account
*                                         ld_off_bschl
*                                CHANGING ld_faus1
*                                         ld_faus2.
*     clear gd_postings-gl_account.
*  ENDIF.
** End of note 2190637.
*
*  IF ld_faus1+1(1) EQ '+'.
*    IF gd_postings-position_text is initial.                    "2231825
*      MESSAGE i808(f5) WITH 'Text' gd_postings-comp_code
*      gd_postings-gl_account.
*      ld_stop_print = 'X'.
*    ENDIF.                                                      "2231825
*  ENDIF.
*
**  IF NOT gd_postings-customer IS INITIAL OR                    "1769059
**  NOT gd_postings-vendor_no IS INITIAL.                        "1769059
**    CLEAR gd_postings-gl_account.                              "1769059
**  ENDIF.                                                       "1769059
*
*ENDFORM.                    " CHECK_SGTXT
**&---------------------------------------------------------------------*
**&      Form  CHECK_SGTXT_E_SPLIT
**&---------------------------------------------------------------------*
**       Created by note 1448153
**       Prevent print of receipt for initial but mandatory sgtxt
**----------------------------------------------------------------------*
* Commented with note 2394995.
*FORM check_sgtxt_e_split .
*
*  CALL FUNCTION 'FCJ_GET_TRANSACTION'
*    EXPORTING
*      i_transaction     = itcj_e_split_postings-transact_name
*      i_comp_code       = itcj_e_split_postings-comp_code
*    TABLES
*      itcj_transactions = lt_transactions.
*  LOOP AT lt_transactions.
*    ld_trans_type = lt_transactions-transact_type.
*  ENDLOOP.
*
*  CASE ld_trans_type.
*    WHEN 'E' OR 'B'.
*      ld_off_bschl      = '40'.
*    WHEN 'D'.
*      ld_off_bschl      = '05'.
*    WHEN 'K'.
*      ld_off_bschl      = '25'.
*  ENDCASE.
*  CLEAR ld_trans_type.
*
** Check sgtxt for offsetting line item.
**  IF NOT itcj_e_split_postings-customer IS INITIAL.            "1769059
**    itcj_e_split_postings-gl_account = ls_debi-akont.          "1769059
**  ELSEIF NOT itcj_e_split_postings-vendor_no IS INITIAL.       "1769059
**    itcj_e_split_postings-gl_account = ls_kred-akont.          "1769059
**  ENDIF.                                                       "1769059
*
*  If not itcj_e_split_postings-gl_account is initial.           "2190637
*  PERFORM feldauswahl(sapff001) USING itcj_e_split_postings-comp_code
*                                      itcj_e_split_postings-gl_account
*                                      ld_off_bschl
*                             CHANGING ld_faus1
*                                      ld_faus2.
*  ELSE.                                                         "2190637
*
** Start of note 2190637.
** Determine reconciliation account number
*     IF NOT itcj_e_split_postings-umskz IS initial.
*       IF  itcj_e_split_postings-vendor_no is not initial.
*          CALL FUNCTION 'FI_SPECIAL_GL_TRANSACT_DATA'
*            EXPORTING
*              i_bukrs       = itcj_e_split_postings-comp_code
*              i_koart       = 'K'
*              i_saknr       = ls_kred-akont
*              i_umskz       = itcj_e_split_postings-umskz
*            IMPORTING
*              E_HKONT       = itcj_e_split_postings-gl_account.
*       ELSE.
*          CALL FUNCTION 'FI_SPECIAL_GL_TRANSACT_DATA'
*            EXPORTING
*              i_bukrs       = itcj_e_split_postings-comp_code
*              i_koart       = 'D'
*              i_saknr       = ls_debi-akont
*              i_umskz       = itcj_e_split_postings-umskz
*            IMPORTING
*              E_HKONT       = itcj_e_split_postings-gl_account.
*       ENDIF.
*     ELSE.
*       IF not itcj_e_split_postings-vendor_no is initial.
*          itcj_e_split_postings-gl_account = ls_kred-akont.
*       ELSEIF not itcj_e_split_postings-customer is initial.
*          itcj_e_split_postings-gl_account = ls_debi-akont.
*       ENDIF.
*     ENDIF.
*     PERFORM feldauswahl(sapff001) USING itcj_e_split_postings-comp_code
*                                         itcj_e_split_postings-gl_account
*                                         ld_off_bschl
*                                CHANGING ld_faus1
*                                         ld_faus2.
*     clear itcj_e_split_postings-gl_account.
*  ENDIF.
** End of note 2190637.
*
*  IF ld_faus1+1(1) EQ '+'.
*    MESSAGE i808(f5) WITH 'Text' itcj_e_split_postings-comp_code
*    itcj_e_split_postings-gl_account.
*    ld_stop_print = 'X'.
*  ELSEIF ld_sgtxt_cj = 'X'.                                     "2100552
*    MESSAGE i808(f5) WITH 'Text' gd_postings-comp_code          "2100552
*    ld_journals-gl_account.                                     "2100552
*    ld_stop_print = 'X'.                                        "2100552
*  ENDIF.                                                        "2100552
*
**  IF NOT itcj_e_split_postings-customer IS INITIAL OR          "1769059
**  NOT itcj_e_split_postings-vendor_no IS INITIAL.              "1769059
**    CLEAR itcj_e_split_postings-gl_account.                    "1769059
**  ENDIF.                                                       "1769059
*
*ENDFORM.                    " CHECK_SGTXT_E_SPLIT
**&---------------------------------------------------------------------*
**&      Form  CHECK_SGTXT_R_SPLIT
**&---------------------------------------------------------------------*
**       Created by note 1448153
**       Prevent print of receipt for initial but mandatory sgtxt
**----------------------------------------------------------------------*
* Commented with note 2394995.
*FORM check_sgtxt_r_split .
*
*  CALL FUNCTION 'FCJ_GET_TRANSACTION'
*    EXPORTING
*      i_transaction     = itcj_r_split_postings-transact_name
*      i_comp_code       = itcj_r_split_postings-comp_code
*    TABLES
*      itcj_transactions = lt_transactions.
*  LOOP AT lt_transactions.
*    ld_trans_type = lt_transactions-transact_type.
*  ENDLOOP.
*
*  CASE ld_trans_type.
*    WHEN 'R' OR 'C'.
*      ld_off_bschl      = '50'.
*    WHEN 'D'.
*      ld_off_bschl      = '15'.
*    WHEN 'K'.
*      ld_off_bschl      = '35'.
*    WHEN 'B'.
*      ld_off_bschl      = '40'.
*  ENDCASE.
*  CLEAR ld_trans_type.
*
** Check sgtxt for offsetting line item.
**  IF NOT itcj_r_split_postings-customer IS INITIAL.            "1769059
**    itcj_r_split_postings-gl_account = ls_debi-akont.          "1769059
**  ELSEIF NOT itcj_r_split_postings-vendor_no IS INITIAL.       "1769059
**    itcj_r_split_postings-gl_account = ls_kred-akont.          "1769059
**  ENDIF.                                                       "1769059
*
*  If not itcj_r_split_postings-gl_account is initial.           "2190637
*  PERFORM feldauswahl(sapff001) USING itcj_r_split_postings-comp_code
*                                      itcj_r_split_postings-gl_account
*                                      ld_off_bschl
*                             CHANGING ld_faus1
*                                      ld_faus2.
*  ELSE.                                                         "2190637
*
** Start of note 2190637.
** Determine reconciliation account number
*     IF NOT itcj_r_split_postings-umskz IS initial.
*       IF  itcj_r_split_postings-vendor_no is not initial.
*          CALL FUNCTION 'FI_SPECIAL_GL_TRANSACT_DATA'
*            EXPORTING
*              i_bukrs       = itcj_r_split_postings-comp_code
*              i_koart       = 'K'
*              i_saknr       = ls_kred-akont
*              i_umskz       = itcj_r_split_postings-umskz
*            IMPORTING
*              E_HKONT       = itcj_r_split_postings-gl_account.
*       ELSE.
*          CALL FUNCTION 'FI_SPECIAL_GL_TRANSACT_DATA'
*            EXPORTING
*              i_bukrs       = itcj_r_split_postings-comp_code
*              i_koart       = 'D'
*              i_saknr       = ls_debi-akont
*              i_umskz       = itcj_r_split_postings-umskz
*            IMPORTING
*              E_HKONT       = itcj_r_split_postings-gl_account.
*       ENDIF.
*     ELSE.
*       IF not itcj_r_split_postings-vendor_no is initial.
*          itcj_r_split_postings-gl_account = ls_kred-akont.
*       ELSEIF not itcj_r_split_postings-customer is initial.
*          itcj_r_split_postings-gl_account = ls_debi-akont.
*       ENDIF.
*     ENDIF.
*     PERFORM feldauswahl(sapff001) USING itcj_r_split_postings-comp_code
*                                         itcj_r_split_postings-gl_account
*                                         ld_off_bschl
*                                CHANGING ld_faus1
*                                         ld_faus2.
*     clear itcj_r_split_postings-gl_account.
*  ENDIF.
** End of note 2190637.
*
*  IF ld_faus1+1(1) EQ '+'.
*    MESSAGE i808(f5) WITH 'Text' itcj_r_split_postings-comp_code
*    itcj_r_split_postings-gl_account.
*    ld_stop_print = 'X'.
*  ELSEIF ld_sgtxt_cj = 'X'.                                     "2100552
*    MESSAGE i808(f5) WITH 'Text' gd_postings-comp_code          "2100552
*    ld_journals-gl_account.                                     "2100552
*    ld_stop_print = 'X'.                                        "2100552
*  ENDIF.                                                        "2100552
*
**  IF NOT itcj_r_split_postings-customer IS INITIAL OR          "1769059
**  NOT itcj_r_split_postings-vendor_no IS INITIAL.              "1769059
**    CLEAR itcj_r_split_postings-gl_account.                    "1769059
**  ENDIF.                                                       "1769059
*
*ENDFORM.                    " CHECK_SGTXT_R_SPLIT
**&---------------------------------------------------------------------*
**&      Form  CHECK_SGTXT_CR_SPLIT
**&---------------------------------------------------------------------*
**       Created by note 1448153
**       Prevent print of receipt for initial but mandatory sgtxt
**----------------------------------------------------------------------*
* Commented with note 2394995.
*FORM check_sgtxt_cr_split .
*
*  CALL FUNCTION 'FCJ_GET_TRANSACTION'
*    EXPORTING
*      i_transaction     = itcj_cr_split_postings-transact_name
*      i_comp_code       = itcj_cr_split_postings-comp_code
*    TABLES
*      itcj_transactions = lt_transactions.
*  LOOP AT lt_transactions.
*    ld_trans_type = lt_transactions-transact_type.
*  ENDLOOP.
*
*  CASE ld_trans_type.
*    WHEN 'R' OR 'C'.
*      ld_off_bschl      = '50'.
*    WHEN 'D'.
*      ld_off_bschl      = '15'.
*    WHEN 'K'.
*      ld_off_bschl      = '35'.
*    WHEN 'B'.
*      ld_off_bschl      = '40'.
*  ENDCASE.
*  CLEAR ld_trans_type.
*
** Check sgtxt for offsetting line item.
**  IF NOT itcj_cr_split_postings-customer IS INITIAL.           "1769059
**    itcj_cr_split_postings-gl_account = ls_debi-akont.         "1769059
**  ELSEIF NOT itcj_cr_split_postings-vendor_no IS INITIAL.      "1769059
**    itcj_cr_split_postings-gl_account = ls_kred-akont.         "1769059
**  ENDIF.                                                       "1769059
*
*  If not itcj_cr_split_postings-gl_account is initial.          "2190637
*   PERFORM feldauswahl(sapff001) USING itcj_cr_split_postings-comp_code
*                                       itcj_cr_split_postings-gl_account
*                                       ld_off_bschl
*                              CHANGING ld_faus1
*                                       ld_faus2.
*  ELSE.                                                         "2190637
*
** Start of note 2190637.
** Determine reconciliation account number
*     IF NOT itcj_cr_split_postings-umskz IS initial.
*       IF  itcj_cr_split_postings-vendor_no is not initial.
*          CALL FUNCTION 'FI_SPECIAL_GL_TRANSACT_DATA'
*            EXPORTING
*              i_bukrs       = itcj_cr_split_postings-comp_code
*              i_koart       = 'K'
*              i_saknr       = ls_kred-akont
*              i_umskz       = itcj_cr_split_postings-umskz
*            IMPORTING
*              E_HKONT       = itcj_cr_split_postings-gl_account.
*       ELSE.
*          CALL FUNCTION 'FI_SPECIAL_GL_TRANSACT_DATA'
*            EXPORTING
*              i_bukrs       = itcj_cr_split_postings-comp_code
*              i_koart       = 'D'
*              i_saknr       = ls_debi-akont
*              i_umskz       = itcj_cr_split_postings-umskz
*            IMPORTING
*              E_HKONT       = itcj_cr_split_postings-gl_account.
*       ENDIF.
*     ELSE.
*       IF not itcj_cr_split_postings-vendor_no is initial.
*          itcj_cr_split_postings-gl_account = ls_kred-akont.
*       ELSEIF not itcj_cr_split_postings-customer is initial.
*          itcj_cr_split_postings-gl_account = ls_debi-akont.
*       ENDIF.
*     ENDIF.
*     PERFORM feldauswahl(sapff001) USING itcj_cr_split_postings-comp_code
*                                         itcj_cr_split_postings-gl_account
*                                         ld_off_bschl
*                                CHANGING ld_faus1
*                                         ld_faus2.
*     clear itcj_cr_split_postings-gl_account.
*  ENDIF.
** End of note 2190637.
*
*  IF ld_faus1+1(1) EQ '+'.
*    MESSAGE i808(f5) WITH 'Text' itcj_cr_split_postings-comp_code
*    itcj_cr_split_postings-gl_account.
*    ld_stop_print = 'X'.
*  ELSEIF ld_sgtxt_cj = 'X'.                                     "2100552
*    MESSAGE i808(f5) WITH 'Text' gd_postings-comp_code          "2100552
*    ld_journals-gl_account.                                     "2100552
*    ld_stop_print = 'X'.                                        "2100552
*  ENDIF.                                                        "2100552
*
**  IF NOT itcj_cr_split_postings-customer IS INITIAL OR         "1769059
**  NOT itcj_cr_split_postings-vendor_no IS INITIAL.             "1769059
**    CLEAR itcj_cr_split_postings-gl_account.                   "1769059
**  ENDIF.                                                       "1769059
*
*ENDFORM.                    " CHECK_SGTXT_CR_SPLIT
*&---------------------------------------------------------------------*
*&      Form  STORE_COBL_E
*&---------------------------------------------------------------------*
*       Created with note 1676745
*----------------------------------------------------------------------*
*       Save old CO values for COBL_CODINGBLOCK_CHECK
*----------------------------------------------------------------------*
FORM STORE_COBL_E .

  clear ls_i_cobl_old.
  MOVE-CORRESPONDING itcj_e_postings TO ls_i_cobl_old.
  ls_i_cobl_old-bukrs   = itcj_e_postings-comp_code.

  CALL FUNCTION 'GET_CURRENT_YEAR'
     EXPORTING
          bukrs   = itcj_e_postings-comp_code
          date    = itcj_e_postings-posting_date
     IMPORTING
          currm   = ld_monat
          curry   = ld_gjahr.

* Set additional fixed attributes
    ls_i_cobl_old-event   = 'PRUEFEN'.
    ls_i_cobl_old-process = 'BELEGPOS'.
    ls_i_cobl_old-buzei   = '001'.
    ls_i_cobl_old-glvor   = 'RFBU'.
    ls_i_cobl_old-vorgn   = 'RFBU'.

* Set derived attributes from above
    ls_i_cobl_old-monat   = ld_monat.
    ls_i_cobl_old-gjahr   = ld_gjahr.
    ls_i_cobl_old-budat   = itcj_e_postings-posting_date.
    ls_i_cobl_old-bldat   = itcj_e_postings-document_date.
    ls_i_cobl_old-hkont   = itcj_e_postings-gl_account.
    ls_i_cobl_old-saknr   = itcj_e_postings-gl_account.
    ls_i_cobl_old-koart   = 'S'.

*   Customer
    IF NOT itcj_e_postings-customer IS INITIAL.
       ls_i_cobl_old-kunnr   = itcj_e_postings-customer.
       ls_i_cobl_old-koart   = 'D'.
    ENDIF.

*   Vendor
    IF NOT itcj_e_postings-vendor_no IS INITIAL.
       ls_i_cobl_old-lifnr   = itcj_e_postings-vendor_no.
       ls_i_cobl_old-koart   = 'K'.
    ENDIF.

ENDFORM.                    " STORE_COBL_E
*&---------------------------------------------------------------------*
*&      Form  STORE_COBL_R
*&---------------------------------------------------------------------*
*       Created with note 1676745
*----------------------------------------------------------------------*
*       Save old CO values for COBL_CODINGBLOCK_CHECK
*----------------------------------------------------------------------*
FORM STORE_COBL_R .

  clear ls_i_cobl_old.
  MOVE-CORRESPONDING itcj_r_postings TO ls_i_cobl_old.
  ls_i_cobl_old-bukrs   = itcj_r_postings-comp_code.

  CALL FUNCTION 'GET_CURRENT_YEAR'
     EXPORTING
          bukrs   = itcj_r_postings-comp_code
          date    = itcj_r_postings-posting_date
     IMPORTING
          currm   = ld_monat
          curry   = ld_gjahr.

* Set additional fixed attributes
    ls_i_cobl_old-event   = 'PRUEFEN'.
    ls_i_cobl_old-process = 'BELEGPOS'.
    ls_i_cobl_old-buzei   = '001'.
    ls_i_cobl_old-glvor   = 'RFBU'.
    ls_i_cobl_old-vorgn   = 'RFBU'.

* Set derived attributes from above
    ls_i_cobl_old-monat   = ld_monat.
    ls_i_cobl_old-gjahr   = ld_gjahr.
    ls_i_cobl_old-budat   = itcj_r_postings-posting_date.
    ls_i_cobl_old-bldat   = itcj_r_postings-document_date.
    ls_i_cobl_old-hkont   = itcj_r_postings-gl_account.
    ls_i_cobl_old-saknr   = itcj_r_postings-gl_account.
    ls_i_cobl_old-koart   = 'S'.

*   Customer
    IF NOT itcj_r_postings-customer IS INITIAL.
       ls_i_cobl_old-kunnr   = itcj_r_postings-customer.
       ls_i_cobl_old-koart   = 'D'.
    ENDIF.

*   Vendor
    IF NOT itcj_r_postings-vendor_no IS INITIAL.
       ls_i_cobl_old-lifnr   = itcj_r_postings-vendor_no.
       ls_i_cobl_old-koart   = 'K'.
    ENDIF.

ENDFORM.                    " STORE_COBL_R
*&---------------------------------------------------------------------*
*&      Form  STORE_COBL_CR
*&---------------------------------------------------------------------*
*       Created with note 1676745
*----------------------------------------------------------------------*
*       Save old CO values for COBL_CODINGBLOCK_CHECK
*----------------------------------------------------------------------*
FORM STORE_COBL_CR .

  clear ls_i_cobl_old.
  MOVE-CORRESPONDING itcj_cr_postings TO ls_i_cobl_old.
  ls_i_cobl_old-bukrs   = itcj_cr_postings-comp_code.

  CALL FUNCTION 'GET_CURRENT_YEAR'
     EXPORTING
          bukrs   = itcj_cr_postings-comp_code
          date    = itcj_cr_postings-posting_date
     IMPORTING
          currm   = ld_monat
          curry   = ld_gjahr.

* Set additional fixed attributes
    ls_i_cobl_old-event   = 'PRUEFEN'.
    ls_i_cobl_old-process = 'BELEGPOS'.
    ls_i_cobl_old-buzei   = '001'.
    ls_i_cobl_old-glvor   = 'RFBU'.
    ls_i_cobl_old-vorgn   = 'RFBU'.

* Set derived attributes from above
    ls_i_cobl_old-monat   = ld_monat.
    ls_i_cobl_old-gjahr   = ld_gjahr.
    ls_i_cobl_old-budat   = itcj_cr_postings-posting_date.
    ls_i_cobl_old-bldat   = itcj_cr_postings-document_date.
    ls_i_cobl_old-hkont   = itcj_cr_postings-gl_account.
    ls_i_cobl_old-saknr   = itcj_cr_postings-gl_account.
    ls_i_cobl_old-koart   = 'S'.

*   Customer
    IF NOT itcj_cr_postings-customer IS INITIAL.
       ls_i_cobl_old-kunnr   = itcj_cr_postings-customer.
       ls_i_cobl_old-koart   = 'D'.
    ENDIF.

*   Vendor
    IF NOT itcj_cr_postings-vendor_no IS INITIAL.
       ls_i_cobl_old-lifnr   = itcj_cr_postings-vendor_no.
       ls_i_cobl_old-koart   = 'K'.
    ENDIF.

ENDFORM.                    " STORE_COBL_CR
*&---------------------------------------------------------------------*
*&      Form  STORE_COBL_E_SPLIT
*&---------------------------------------------------------------------*
*       Created with note 1676745
*----------------------------------------------------------------------*
*       Save old CO values for COBL_CODINGBLOCK_CHECK
*----------------------------------------------------------------------*
FORM STORE_COBL_E_SPLIT .

  clear ls_i_cobl_old.
  MOVE-CORRESPONDING itcj_e_split_postings TO ls_i_cobl_old.
  ls_i_cobl_old-bukrs   = itcj_e_split_postings-comp_code.

  CALL FUNCTION 'GET_CURRENT_YEAR'
     EXPORTING
          bukrs   = itcj_e_split_postings-comp_code
          date    = itcj_e_split_postings-posting_date
     IMPORTING
          currm   = ld_monat
          curry   = ld_gjahr.

* Set additional fixed attributes
    ls_i_cobl_old-event   = 'PRUEFEN'.
    ls_i_cobl_old-process = 'BELEGPOS'.
    ls_i_cobl_old-buzei   = '001'.
    ls_i_cobl_old-glvor   = 'RFBU'.
    ls_i_cobl_old-vorgn   = 'RFBU'.

* Set derived attributes from above
    ls_i_cobl_old-monat   = ld_monat.
    ls_i_cobl_old-gjahr   = ld_gjahr.
    ls_i_cobl_old-budat   = itcj_e_split_postings-posting_date.
    ls_i_cobl_old-bldat   = itcj_e_split_postings-document_date.
    ls_i_cobl_old-hkont   = itcj_e_split_postings-gl_account.
    ls_i_cobl_old-saknr   = itcj_e_split_postings-gl_account.
    ls_i_cobl_old-koart   = 'S'.

*   Customer
    IF NOT itcj_e_split_postings-customer IS INITIAL.
       ls_i_cobl_old-kunnr   = itcj_e_split_postings-customer.
       ls_i_cobl_old-koart   = 'D'.
    ENDIF.

*   Vendor
    IF NOT itcj_e_split_postings-vendor_no IS INITIAL.
       ls_i_cobl_old-lifnr   = itcj_e_split_postings-vendor_no.
       ls_i_cobl_old-koart   = 'K'.
    ENDIF.

ENDFORM.                    " STORE_COBL_E_SPLIT
*&---------------------------------------------------------------------*
*&      Form  STORE_COBL_R_SPLIT
*&---------------------------------------------------------------------*
*       Created with note 1676745
*----------------------------------------------------------------------*
*       Save old CO values for COBL_CODINGBLOCK_CHECK
*----------------------------------------------------------------------*
FORM STORE_COBL_R_SPLIT .

  clear ls_i_cobl_old.
  MOVE-CORRESPONDING itcj_r_split_postings TO ls_i_cobl_old.
  ls_i_cobl_old-bukrs   = itcj_r_split_postings-comp_code.

  CALL FUNCTION 'GET_CURRENT_YEAR'
     EXPORTING
          bukrs   = itcj_r_split_postings-comp_code
          date    = itcj_r_split_postings-posting_date
     IMPORTING
          currm   = ld_monat
          curry   = ld_gjahr.

* Set additional fixed attributes
    ls_i_cobl_old-event   = 'PRUEFEN'.
    ls_i_cobl_old-process = 'BELEGPOS'.
    ls_i_cobl_old-buzei   = '001'.
    ls_i_cobl_old-glvor   = 'RFBU'.
    ls_i_cobl_old-vorgn   = 'RFBU'.

* Set derived attributes from above
    ls_i_cobl_old-monat   = ld_monat.
    ls_i_cobl_old-gjahr   = ld_gjahr.
    ls_i_cobl_old-budat   = itcj_r_split_postings-posting_date.
    ls_i_cobl_old-bldat   = itcj_r_split_postings-document_date.
    ls_i_cobl_old-hkont   = itcj_r_split_postings-gl_account.
    ls_i_cobl_old-saknr   = itcj_r_split_postings-gl_account.
    ls_i_cobl_old-koart   = 'S'.

*   Customer
    IF NOT itcj_r_split_postings-customer IS INITIAL.
       ls_i_cobl_old-kunnr   = itcj_r_split_postings-customer.
       ls_i_cobl_old-koart   = 'D'.
    ENDIF.

*   Vendor
    IF NOT itcj_r_split_postings-vendor_no IS INITIAL.
       ls_i_cobl_old-lifnr   = itcj_r_split_postings-vendor_no.
       ls_i_cobl_old-koart   = 'K'.
    ENDIF.

ENDFORM.                    " STORE_COBL_R_SPLIT
*&---------------------------------------------------------------------*
*&      Form  STORE_COBL_CR_SPLIT
*&---------------------------------------------------------------------*
*       Created with note 1676745
*----------------------------------------------------------------------*
*       Save old CO values for COBL_CODINGBLOCK_CHECK
*----------------------------------------------------------------------*
FORM STORE_COBL_CR_SPLIT .

  clear ls_i_cobl_old.
  MOVE-CORRESPONDING itcj_cr_split_postings TO ls_i_cobl_old.
  ls_i_cobl_old-bukrs   = itcj_cr_split_postings-comp_code.

  CALL FUNCTION 'GET_CURRENT_YEAR'
     EXPORTING
          bukrs   = itcj_cr_split_postings-comp_code
          date    = itcj_cr_split_postings-posting_date
     IMPORTING
          currm   = ld_monat
          curry   = ld_gjahr.

* Set additional fixed attributes
    ls_i_cobl_old-event   = 'PRUEFEN'.
    ls_i_cobl_old-process = 'BELEGPOS'.
    ls_i_cobl_old-buzei   = '001'.
    ls_i_cobl_old-glvor   = 'RFBU'.
    ls_i_cobl_old-vorgn   = 'RFBU'.

* Set derived attributes from above
    ls_i_cobl_old-monat   = ld_monat.
    ls_i_cobl_old-gjahr   = ld_gjahr.
    ls_i_cobl_old-budat   = itcj_cr_split_postings-posting_date.
    ls_i_cobl_old-bldat   = itcj_cr_split_postings-document_date.
    ls_i_cobl_old-hkont   = itcj_cr_split_postings-gl_account.
    ls_i_cobl_old-saknr   = itcj_cr_split_postings-gl_account.
    ls_i_cobl_old-koart   = 'S'.

*   Customer
    IF NOT itcj_cr_split_postings-customer IS INITIAL.
       ls_i_cobl_old-kunnr   = itcj_cr_split_postings-customer.
       ls_i_cobl_old-koart   = 'D'.
    ENDIF.

*   Vendor
    IF NOT itcj_cr_split_postings-vendor_no IS INITIAL.
       ls_i_cobl_old-lifnr   = itcj_cr_split_postings-vendor_no.
       ls_i_cobl_old-koart   = 'K'.
    ENDIF.

ENDFORM.                    " STORE_COBL_CR_SPLIT
*&---------------------------------------------------------------------*
*&      Form  FILL_FI_FOR_SUBST
*&---------------------------------------------------------------------*
*       Created with note 1860806
*----------------------------------------------------------------------*
*      -->ITCJ_POSTINGS
*      <--LS_BKPF
*      <--LS_BSEG
*----------------------------------------------------------------------*
FORM FILL_FI_FOR_SUBST USING P_ISCJ_POSTINGS LIKE ISCJ_POSTINGS
                             LS_BKPF LIKE BKPF
                             LS_BSEG LIKE BSEG.
  CLEAR ld_vornr_conv.                                          "2729673

* Tranfer all relevant fields to ls_bkpf
  ls_bkpf-mandt = sy-mandt.
  ls_bkpf-bukrs = tcj_c_journals-comp_code.
  ls_bkpf-gjahr = p_iscj_postings-fisc_year.
  ls_bkpf-bldat = p_iscj_postings-document_date.
  ls_bkpf-budat = p_iscj_postings-posting_date.
  ls_bkpf-cpudt = sy-datum.
  ls_bkpf-cputm = sy-uzeit.
  ls_bkpf-usnam = sy-uname.
  ls_bkpf-tcode = sy-tcode.
  ls_bkpf-xblnr = p_iscj_postings-document_number.
  ls_bkpf-waers = tcj_c_journals-currency.
  ls_bkpf-glvor = 'RFBU'.
  ls_bkpf-awtyp = 'CAJO'.
  IF p_iscj_postings-document_status CA 'SPR'.                  "2109078
    ls_bkpf-awkey(10) = p_iscj_postings-posting_number.         "2109078
  ENDIF.                                                        "2109078
  ls_bkpf-awkey+10(4) = p_iscj_postings-cajo_number.            "2109078
  ls_bkpf-awkey+14(4) = p_iscj_postings-comp_code.              "2109078
  ls_bkpf-fikrs = ls_t001-fikrs.
  ls_bkpf-hwaer = ls_t001-waers.
  ls_bkpf-vatdate = p_iscj_postings-vatdate.
  ls_bkpf-kursf = p_iscj_postings-exch_rate.                    "2048367
  ls_bkpf-bktxt = p_iscj_postings-transact_name.                "2467314

* Transfer all relevant fields to ls_bseg
  move-corresponding p_iscj_postings to ls_bseg.
  ls_bseg-mandt = sy-mandt.
  ls_bseg-bukrs = tcj_c_journals-comp_code.
  ls_bseg-gjahr = p_iscj_postings-fisc_year.
  ls_bseg-mwskz = p_iscj_postings-tax_code.
  ls_bseg-wmwst = p_iscj_postings-p_tax_amount.
  ls_bseg-fwbas = p_iscj_postings-p_fwbas.
  ls_bseg-valut = p_iscj_postings-valuta_date.
  ls_bseg-zuonr = p_iscj_postings-alloc_nmbr.
  ls_bseg-sgtxt = p_iscj_postings-position_text.
  ls_bseg-hkont = p_iscj_postings-gl_account.
  ls_bseg-kunnr = p_iscj_postings-customer.
  ls_bseg-lifnr = p_iscj_postings-vendor_no.
  ls_bseg-filkd = p_iscj_postings-branch.
  ls_bseg-projk = p_iscj_postings-ps_psp_pnr.               "1950481
  ls_bseg-vbel2 = p_iscj_postings-kdauf.                    "1950481
  ls_bseg-posn2 = p_iscj_postings-kdpos.                    "1950481
  ls_bseg-eten2 = p_iscj_postings-kdein.                    "1950481
  ls_bseg-bewar = p_iscj_postings-rmvct.                    "1950481
  ls_bseg-pprct = p_iscj_postings-pprctr.                   "1950481

* Start of note 2729673
* Convert vornr
  IF not p_iscj_postings-vornr is initial.
    IF not p_iscj_postings-nplnr is initial.
      CALL FUNCTION 'CO_SF_NETWORKACTIVITY_CHECK'
        EXPORTING
          aufnr_imp     = p_iscj_postings-nplnr
          vornr_imp     = p_iscj_postings-vornr
        IMPORTING
          aplzl_exp     = ls_bseg-aplzl
          aufpl_exp     = ls_bseg-aufpl.
        ld_vornr_conv = 'X'.
    ELSEIF cl_erp_co_olc_sw_check=>erp_co_olc( ) NE space
    AND not p_iscj_postings-aufnr is initial.
      CALL FUNCTION 'CO_SF_NETWORKACTIVITY_CHECK'
        EXPORTING
          aufnr_imp     = p_iscj_postings-aufnr
          vornr_imp     = p_iscj_postings-vornr
          iv_order_catg = '30'
        IMPORTING
          aplzl_exp     = ls_bseg-aplzl
          aufpl_exp     = ls_bseg-aufpl.
        ld_vornr_conv = 'X'.
    ENDIF.
  ENDIF.
* End of note 2729673

* Derive fields
  IF not p_iscj_postings-umskz is initial.
    ls_bseg-vorgn = 'AZBU'.
  ELSE.
    ls_bseg-vorgn = 'RFBU'.
  ENDIF.

*   IF sy-dynnr = '0110'                                        "2086298
*   OR sy-dynnr = '0120'                                        "2086298
*   OR sy-dynnr = '0140'.                                       "2086298
   IF NOT gd_new_split_amount = 'X' AND                         "2086298
      NOT p_iscj_postings-split = 'X'.                          "2086298
* Non-split documents
* Use h_payments / h_receipts amounts
    perform get_bschl_shkzg(saplsaplfcj_document)
          using p_iscj_postings-h_payments
                p_iscj_postings-h_receipts
                p_iscj_postings-process_status
                p_iscj_postings-h_tax_amount
                p_iscj_postings-tax_percent
                p_iscj_postings-transact_name
                tcj_c_journals-comp_code
       changing ls_bseg-bschl
                ls_bseg-shkzg.
    IF not p_iscj_postings-h_payments is initial.
     ls_bseg-wrbtr = p_iscj_postings-h_payments.
    ELSE.
     ls_bseg-wrbtr = p_iscj_postings-h_receipts.
    ENDIF.
  ELSE.
* Split documents
* Use p_payments / p_receipts amounts
    perform get_bschl_shkzg(saplsaplfcj_document)
          using p_iscj_postings-p_payments
                p_iscj_postings-p_receipts
                p_iscj_postings-process_status
                p_iscj_postings-p_tax_amount
                p_iscj_postings-tax_percent
                p_iscj_postings-transact_name
                tcj_c_journals-comp_code
       changing ls_bseg-bschl
                ls_bseg-shkzg.
    IF not p_iscj_postings-p_payments is initial.
     ls_bseg-wrbtr = p_iscj_postings-p_payments.
    ELSE.
     ls_bseg-wrbtr = p_iscj_postings-p_receipts.
    ENDIF.
  ENDIF.

  IF not ls_bseg-lifnr is initial.
    ls_bseg-koart = 'K'.
  ELSEIF not ls_bseg-kunnr is initial.
    ls_bseg-koart = 'D'.
  ELSE.
    ls_bseg-koart = 'S'.
  ENDIF.

ENDFORM.                    " FILL_FI_FOR_SUBST
*&---------------------------------------------------------------------*
*&      Form  FILL_CJ_AFTER_SUBST_HD
*&---------------------------------------------------------------------*
*       Created with note 1860806
*----------------------------------------------------------------------*
*      -->LS_BKPF
*      <--P_ISCJ_POSTINGS
*----------------------------------------------------------------------*
FORM FILL_CJ_AFTER_SUBST_HD USING P_ISCJ_POSTINGS LIKE ISCJ_POSTINGS
                                  LS_BKPF         LIKE BKPF.    "2086298

* Only allow substitution for non-critical fields
  p_iscj_postings-document_date = ls_bkpf-bldat.
  p_iscj_postings-document_number = ls_bkpf-xblnr.
  p_iscj_postings-exch_rate = ls_bkpf-kursf.

ENDFORM.                    " FILL_CJ_AFTER_SUBST_HD
*&---------------------------------------------------------------------*
*&      Form  FILL_CJ_AFTER_SUBST_IT
*&---------------------------------------------------------------------*
*       Created with note 1860806
*----------------------------------------------------------------------*
*      -->LS_BSEG
*      <--P_ISCJ_POSTINGS
*----------------------------------------------------------------------*
FORM FILL_CJ_AFTER_SUBST_IT USING P_ISCJ_POSTINGS LIKE ISCJ_POSTINGS
                                  LS_BSEG         LIKE BSEG.    "2086298

* Only allow substitution for non-critical fields
* Map fields with different field names
  p_iscj_postings-alloc_nmbr = ls_bseg-zuonr.
  p_iscj_postings-position_text = ls_bseg-sgtxt.
  p_iscj_postings-taxjurcode = ls_bseg-txjcd.
  p_iscj_postings-valuta_date = ls_bseg-valut.
  p_iscj_postings-bupla = ls_bseg-bupla.
  p_iscj_postings-secco = ls_bseg-secco.
  p_iscj_postings-lzbkz = ls_bseg-lzbkz.
  p_iscj_postings-landl = ls_bseg-landl.
  p_iscj_postings-segment = ls_bseg-segment.
  p_iscj_postings-psegment = ls_bseg-segment.
  p_iscj_postings-stceg = ls_bseg-stceg.

* Substitute CO fields
  ld_fkber = p_iscj_postings-fkber.                         "2090632
  move-corresponding ls_bseg to ls_cobl_sub.
  move-corresponding ls_cobl_sub to p_iscj_postings.

* Map CO fields with differnt field names                   "1950481
  p_iscj_postings-ps_psp_pnr = ls_bseg-projk.               "1950481
  p_iscj_postings-kdauf      = ls_bseg-vbel2.               "1950481
  p_iscj_postings-kdpos      = ls_bseg-posn2.               "1950481
  p_iscj_postings-kdein      = ls_bseg-eten2.               "1950481
  p_iscj_postings-rmvct      = ls_bseg-bewar.               "1950481
  p_iscj_postings-pprctr     = ls_bseg-pprct.               "1950481
  if ls_bseg-fkber_long is not initial.                     "2090632
     p_iscj_postings-fkber = ls_bseg-fkber_long.            "2090632
  else.                                                     "2090632
     p_iscj_postings-fkber = ld_fkber.                      "2090632
  endif.                                                    "2090632

* Start of note 2729673
* Reconvert vornr
  IF ld_vornr_conv ne space.
    CALL FUNCTION 'CO_SF_AFVG_CHECK_WITH_KEY'
      EXPORTING
        aplzl  = ls_bseg-aplzl
        aufpl  = ls_bseg-aufpl
     IMPORTING
        vornr   = p_iscj_postings-vornr.
  ENDIF.
* End of note 2729673

ENDFORM.                    " FILL_CJ_AFTER_SUBST_IT
*&---------------------------------------------------------------------*
*& Form CHECK_HOUSE_BANK_AND_ACCOUNT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_ITCJ_E_POSTINGS  text
*&---------------------------------------------------------------------*
FORM CHECK_HOUSE_BANK_AND_ACCOUNT USING P_ISCJ_POSTINGS LIKE ISCJ_POSTINGS.
* form new with S4CORE101

  CALL FUNCTION 'FCJ_GET_TRANSACTION'
    EXPORTING
      I_TRANSACTION     = p_iscj_postings-transact_name
      I_COMP_CODE       = p_iscj_postings-comp_code
    TABLES
      ITCJ_TRANSACTIONS = lt_transactions.

  LOOP AT lt_transactions.
  ENDLOOP.

  IF lt_transactions-transact_type CA 'ERKD'.
* Transaction type needs no HBKID/HKTID field input
    IF p_iscj_postings-document_status = 'E' OR
       p_iscj_postings-document_status = 'S' OR
       p_iscj_postings-document_status = 'C' OR
       p_iscj_postings-document_status = ' '.
      IF NOT ( p_iscj_postings-HBKID IS INITIAL AND
               p_iscj_postings-HKTID IS INITIAL ).
         MESSAGE i196(f5a) WITH 'HBKID/HKTID' p_iscj_postings-gl_account.
         CLEAR: p_iscj_postings-HBKID,
                p_iscj_postings-HKTID.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lt_transactions-transact_type CA 'BC'
    AND not ( p_iscj_postings-hbkid is initial AND
              p_iscj_postings-hktid is initial ).
    IF p_iscj_postings-hktid is initial.
*     only check hbkid in T012
      SELECT SINGLE * FROM T012
                    WHERE BUKRS = p_iscj_postings-comp_code
                    AND   HBKID = p_iscj_postings-HBKID.
      IF SY-SUBRC <> 0.
         MESSAGE E755(FB) WITH p_iscj_postings-hbkid.
      ENDIF.
    ELSE.
*     check hbkid/hktid in T012K
      SELECT SINGLE * FROM T012K
                    WHERE BUKRS = p_iscj_postings-comp_code
                    AND   HBKID = p_iscj_postings-HBKID
                    AND   HKTID = p_iscj_postings-HKTID.
      IF SY-SUBRC <> 0.
         MESSAGE E736(FB) WITH p_iscj_postings-hbkid
                               p_iscj_postings-hktid.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_WITH_COMP_CODE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ISCJ_E_POSTINGS_TRANSACT_NAME
*&---------------------------------------------------------------------*
FORM F4_WITH_COMP_CODE  USING: P_ISCJ_POSTINGS like iscj_postings
                               structure_field type text50.
* form new with note 2497426

DATA: structure type text25,
      field type text25,
      search_field type text25.

  IF structure_field+15(1) = '-'.          "structure itcj_e_postings or itcj_r_postings
    structure = structure_field(15).
    field = structure_field+16(25).
  ELSEIF structure_field+16(1) = '-'.      "structure itcj_cr_postings
    structure = structure_field(16).
    field = structure_field+17(25).
  ELSEIF structure_field+21(1) = '-'.      "structure itcj_e_split_postings or itcj_r_split_postings
    structure = structure_field(21).
    field = structure_field+22(25).
  ELSEIF structure_field+22(1) = '-'.      "structure itcj_cr_split_postings
    structure = structure_field(22).
    field = structure_field+23(25).
  ENDIF.

  CASE field.
    WHEN 'TRANSACT_NAME'.     loc_sh = 'H_CJ_TRANSACTION'.       search_field = 'TRANSACT_NAME'.
    WHEN 'VENDOR_NO'.         loc_sh = 'KRED'.                   search_field = 'LIFNR'.
    WHEN 'CUSTOMER'.          loc_sh = 'DEBI'.                   search_field = 'KUNNR'.
  ENDCASE.

* get structure for search help
  call function 'F4IF_GET_SHLP_DESCR'
    exporting
      shlpname = loc_sh
    importing
      shlp     = loc_shlp
    exceptions
      others   = 1.

  CHECK sy-subrc = 0.

* fill interface for search help
  IF loc_sh = 'H_CJ_TRANSACTION'.
    read table loc_shlp-interface with key shlpfield = 'COMP_CODE' into loc_interface.
  ELSE.  "loc_sh = 'KRED' or 'DEBI'
    read table loc_shlp-interface with key shlpfield = 'BUKRS'     into loc_interface.
  ENDIF.
* fill company code / BUKRS (f_comp_code) from current cash journal in search help
  move f_comp_code to loc_interface-value.
  modify loc_shlp-interface from loc_interface index sy-tabix.

  read table loc_shlp-interface with key shlpfield = search_field into loc_interface.
  move structure to loc_interface-valtabname.
  move field     to loc_interface-valfield.

* get value for dynpro field and fill to F4 if it contains '*'
  ld_dynnr = sy-dynnr.
  refresh lt_dynpfields.
  move structure_field to lt_dynpfields-fieldname.
  append lt_dynpfields.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = 'SAPMFCJ0'
      dynumb     = ld_dynnr
    TABLES
      dynpfields = lt_dynpfields
    EXCEPTIONS
      OTHERS     = 1.
  If sy-subrc = 0 AND lt_dynpfields-fieldvalue  CA '*'.
     move lt_dynpfields-fieldvalue to loc_interface-value.
  ENDIF.
  modify loc_shlp-interface from loc_interface index sy-tabix.

* call search help now

  loc_call_control-maxrecords = '500'.                          "2703819

  call function 'F4_SEARCH_HELP'
    exporting
      shlp                = loc_shlp
      call_control        = loc_call_control
    tables
      flds_out_tab        = loc_flds_out_tab
   exceptions
      user_cancel         = 1
      no_data_found       = 2
      internal_error      = 3
      not_yet_implemented = 4
      others              = 5.

  read table loc_flds_out_tab with key fieldname = search_field.
  CHECK SY-SUBRC = 0.

* write result back to screen
  CASE field.
    WHEN 'TRANSACT_NAME'.     "loc_sh = 'H_CJ_TRANSACTION'.
         move loc_flds_out_tab-fieldval to P_ISCJ_POSTINGS-transact_name.
    WHEN 'VENDOR_NO'.         "loc_sh = 'KRED'.
         move loc_flds_out_tab-fieldval to P_ISCJ_POSTINGS-vendor_no.
    WHEN 'CUSTOMER'.          "loc_sh = 'DEBI'.
         move loc_flds_out_tab-fieldval to P_ISCJ_POSTINGS-customer.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_POSTINGS_TAX_AMOUNT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_ISCJ_POSTINGS
*&---------------------------------------------------------------------*
FORM check_postings_tax_amount USING p_iscj_postings TYPE iscj_postings
                                     p_amount_field TYPE fieldname.
  DATA: lv_is_big_amount TYPE boole_d.

  IF p_iscj_postings-tax_code IS NOT INITIAL.
    ASSIGN COMPONENT p_amount_field OF STRUCTURE p_iscj_postings
      TO FIELD-SYMBOL(<amount>).
    PERFORM check_afle_size USING <amount> lv_is_big_amount.
    IF lv_is_big_amount = abap_true.
      " AFLE: It is decided that tax calculations are not supported for large amounts.
      MESSAGE e813(ff).
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form INIT_AFLE_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_TC
*&---------------------------------------------------------------------*
FORM init_afle_check USING p_tc TYPE scxtab_control.
  " only editable rows are collected (and numbered with line index (IDX))
  " in lt_dynpfields_* in module snapshot_splitscreen.
  " Number of editable lines in current screen is either equal to
  " LOOPC or less:
  " 1. total # of lines in table control >= LOOPC and the last line is
  "    the last visible line of current splitscreen or it is on the next
  "    scroll page:
  "    # of editable lines = LOOPC
  " 2. total # of lines in table control >= LOOPC, some lines are
  "    scrolled up and editable lines < LOOPC:
  "    # of editable lines = LINES-TOP_LINE+1
  " 3. total # of lines in table control < LOOPC:
  "    # of editable lines = LINES-TOP_LINE+1
  gd_split_lines = COND i(
                     WHEN p_tc-top_line + gd_split_loopc - 1 < p_tc-lines
                     THEN gd_split_loopc
                     ELSE p_tc-lines - p_tc-top_line + 1 ).

  " Add empty lines (# of added lines: gd_split_lines)
  gt_afle_check = VALUE ty_afle_check_t(
                    FOR j = 1 THEN j + 1 UNTIL j > gd_split_lines ( ) ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SNAPSHOT_SPLITSCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      TABLES PT_ISCJ_POSTINGS
*&      --> P_ISCJ_POSTINGS_NAME
*&      --> P_TC
*&---------------------------------------------------------------------*
FORM snapshot_splitscreen TABLES pt_iscj_postings STRUCTURE iscj_postings
                          USING  p_iscj_postings_name TYPE typename
                                 p_amount_field TYPE fieldname
                                 p_tc TYPE scxtab_control.

  DATA: lt_dynpfields_tax_code TYPE dynpread_t.
  DATA: lt_dynpfields_amount TYPE dynpread_t.
  DATA: lv_amount TYPE cjamount.

  PERFORM get_tc_field_values TABLES lt_dynpfields_amount
                              USING p_iscj_postings_name p_tc p_amount_field.
  PERFORM get_tc_field_values TABLES lt_dynpfields_tax_code
                              USING p_iscj_postings_name p_tc 'TAX_CODE'.

  LOOP AT gt_afle_check ASSIGNING FIELD-SYMBOL(<gs_afle_check>).
    DATA(lv_amount_char) = lt_dynpfields_amount[ sy-tabix ]-fieldvalue.
    PERFORM convert_char_p USING lv_amount_char lv_amount gd_separator.
    PERFORM check_afle_size USING lv_amount <gs_afle_check>-afle_amount.
    <gs_afle_check>-tax_code =
      COND #( WHEN lt_dynpfields_tax_code[ sy-tabix ]-fieldvalue IS NOT INITIAL
              THEN abap_true ).
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SNAPSHOT_ITAB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      TABLES PT_ISCJ_POSTINGS
*&      --> P_TC
*&---------------------------------------------------------------------*
FORM snapshot_itab TABLES pt_iscj_postings STRUCTURE iscj_postings
                   USING p_tc TYPE scxtab_control.
  " backup current splitscreen entries...
  gt_split_postings = pt_iscj_postings[].
  " ...except records being edited in splitscreen (processed separately)
  DELETE gt_split_postings FROM p_tc-current_line
                           TO p_tc-current_line + gd_split_lines - 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_TC_FIELD_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      TABLES PT_DYNPFIELDS
*&      --> P_ISCJ_POSTINGS_NAME
*&      --> P_TC
*&      --> P_FIELDNAME
*&---------------------------------------------------------------------*
FORM get_tc_field_values  TABLES   pt_dynpfields TYPE dynpread_t
                          USING    p_iscj_postings_name TYPE typename
                                   p_tc TYPE scxtab_control
                                   VALUE(p_fieldname).

  pt_dynpfields[] = VALUE dynpread_t(
    FOR idx = '001' THEN CONV numc_3( idx + 1 ) UNTIL idx > gd_split_lines
    ( fieldname = |{ p_iscj_postings_name }-{ p_fieldname }({ idx })| ) ).

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = 'SAPMFCJ0'
      dynumb               = sy-dynnr
      translate_to_upper   = 'X'
    TABLES
      dynpfields           = pt_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.
  IF sy-subrc <> 0.
    " Error handling ignored on purpose
    " MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    "         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERT_CHAR_P
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_CHAR_P1
*&      --> P_P
*&      --> P_SEPARATOR
*&---------------------------------------------------------------------*
FORM convert_char_p  USING    p_char_p TYPE clike
                              p_p TYPE cjamount
                              p_separator TYPE char1.

  IF find( val = p_char_p sub = p_separator ) > -1. "decimal separator found?
    CASE p_separator.
      WHEN '.'.
        " remove all ','
        p_char_p = replace( val = p_char_p sub = ',' with = '' occ = 0 ).
      WHEN ','.
        " remove all '.' and then replace ',' with '.'
        p_char_p =
          replace( val = replace( val = p_char_p sub = '.' with = '' occ = 0 )
                           sub = ',' with = '.' occ = 0 ).
    ENDCASE.
  ENDIF.
  CONDENSE p_char_p NO-GAPS.
  p_p = p_char_p.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SEPARATOR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM get_separator.
  TYPES: ty_users_t TYPE STANDARD TABLE OF usdef WITH KEY bname.

  IF gd_separator IS INITIAL.
    DATA(lt_users) = VALUE ty_users_t( ( bname = sy-uname ) ).

    " SU3                LT_USERS-DCPFM
    " ---------------------------------
    " 1.234.567,89 (' ') ,
    " 1,234,567.89 ('X') .
    " 1 234 567,89 ('Y') ,
    CALL FUNCTION 'SUSR_GET_USER_DEFAULTS'
      TABLES
        users = lt_users.

    gd_separator = lt_users[ 1 ]-dcpfm.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_AFLE_SIZE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_AMOUNT
*&      --> P_IS_BIG_AMOUNT
*&---------------------------------------------------------------------*
FORM check_afle_size USING p_amount TYPE cjamount
                           p_is_big_amount TYPE boole_d.

  CLEAR p_is_big_amount.
  TRY.
      DATA(lv_wrbtr) = CONV komp-wrbtr( p_amount ).
    CATCH cx_sy_conversion_overflow.
      p_is_big_amount = abap_true.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AMNT_VS_TAX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_ISCJ_POSTINGS
*&---------------------------------------------------------------------*
FORM check_amnt_vs_tax USING ps_iscj_postings TYPE iscj_postings
                             p_amount_field TYPE fieldname.
  DATA: lv_is_big_amount TYPE boole_d.

  " Detect AFLE amount in this line
  ASSIGN COMPONENT p_amount_field OF STRUCTURE ps_iscj_postings
    TO FIELD-SYMBOL(<amount>).
  PERFORM check_afle_size USING <amount> lv_is_big_amount.

  " AFLE amount in this line?
  IF lv_is_big_amount = abap_true.
    PERFORM check_tax_code_presence USING ps_iscj_postings.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_TAX_VS_AMNT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_ISCJ_POSTINGS
*&---------------------------------------------------------------------*
FORM check_tax_vs_amnt USING ps_iscj_postings TYPE iscj_postings
                             p_amount_field TYPE fieldname.

  " Tax code in this line?
  IF ps_iscj_postings-tax_code IS NOT INITIAL.
    PERFORM check_afle_amount_presence USING ps_iscj_postings p_amount_field.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_TRANS_NAME_VS_TAX_CODE_1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ISCJ_E_SPLIT_POSTINGS
*&---------------------------------------------------------------------*
FORM check_trans_name_vs_tax_code_1
  USING ps_iscj_postings TYPE iscj_postings
        p_amount_field TYPE fieldname.

  " Single line is selected even though key is not fully specified.
  " Only single TRANSACT_NUMBER value is present - ensured by preceding
  " checks in module trans_check_split_XY
  SELECT SINGLE transact_number FROM tcj_trans_names
    INTO @DATA(lv_transact_number)
    WHERE comp_code = @f_comp_code
      AND langu = @sy-langu
      AND transact_name = @ps_iscj_postings-transact_name.
  " No error handling - that was already passed
  " in module trans_check_split_XY

  SELECT SINGLE * FROM tcj_transactions INTO @DATA(ls_transaction)
    WHERE comp_code = @f_comp_code
      AND transact_number = @lv_transact_number.
  " No error handling - that was already passed
  " in module trans_check_split_XY

  " Business-transaction-derived Tax code not initial?
  IF ls_transaction-tax_code IS NOT INITIAL.
    " Business-transaction-derived Tax code changeable?
    IF ls_transaction-cjtaxchange = abap_true.
      gd_tax_code_derived = abap_true.
    ELSE.
      PERFORM check_afle_amount_presence
        USING ps_iscj_postings p_amount_field.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_TRANS_NAME_VS_TAX_CODE_2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_trans_name_vs_tax_code_2
  USING ps_iscj_postings TYPE iscj_postings
        p_amount_field TYPE fieldname.

  IF gd_tax_code_derived = abap_true.
    " Tax code in this line?
    IF ps_iscj_postings-tax_code IS NOT INITIAL.
      PERFORM check_afle_amount_presence
        USING ps_iscj_postings p_amount_field.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_AFLE_AMOUNT_PRESENCE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_afle_amount_presence USING ps_iscj_postings TYPE iscj_postings
                                      p_amount_field TYPE fieldname.
  DATA: lv_is_big_amount TYPE boole_d.

  " Detect AFLE amount in this line
  ASSIGN COMPONENT p_amount_field OF STRUCTURE ps_iscj_postings
    TO FIELD-SYMBOL(<amount>).
  PERFORM check_afle_size USING <amount> lv_is_big_amount.

  IF lv_is_big_amount = abap_true.
    " AFLE: The combination of large quantities and taxes is not allowed
    MESSAGE e837(ff).
  ELSE.
    " AFLE amount in any other line of current splitscreen?
    LOOP AT gt_afle_check TRANSPORTING NO FIELDS WHERE afle_amount = abap_true.
      CHECK sy-tabix <> gd_afle_check_idx.
      " AFLE: The combination of large quantities and taxes is not allowed
      MESSAGE e837(ff).
    ENDLOOP.

    " AFLE amount in any of scrolled-out lines of the splitscreen?
    LOOP AT gt_split_postings ASSIGNING FIELD-SYMBOL(<ls_postings>).
      ASSIGN COMPONENT p_amount_field OF STRUCTURE <ls_postings> TO <amount>.
      PERFORM check_afle_size USING <amount> lv_is_big_amount.
      IF lv_is_big_amount = abap_true.
        " AFLE: The combination of large quantities and taxes is not allowed
        MESSAGE e837(ff).
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_TAX_CODE_PRESENCE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_tax_code_presence USING ps_iscj_postings TYPE iscj_postings.
  " Tax code set in current line?
  IF ps_iscj_postings-tax_code IS NOT INITIAL.
    " AFLE: The combination of large quantities and taxes is not allowed
    MESSAGE e837(ff).
  ELSE.
    " Tax code set in any other line of current splitscreen?
    LOOP AT gt_afle_check TRANSPORTING NO FIELDS WHERE tax_code = abap_true.
      CHECK sy-tabix <> gd_afle_check_idx.
      " AFLE: The combination of large quantities and taxes is not allowed
      MESSAGE e837(ff).
    ENDLOOP.

    " Tax_code set in any of scrolled-out lines of the splitscreen?
    LOOP AT gt_split_postings TRANSPORTING NO FIELDS
      WHERE tax_code IS NOT INITIAL.
      " AFLE: The combination of large quantities and taxes is not allowed
      MESSAGE e837(ff).
    ENDLOOP.
  ENDIF.
ENDFORM.
