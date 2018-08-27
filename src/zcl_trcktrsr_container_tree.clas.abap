class ZCL_TRCKTRSR_CONTAINER_TREE definition
  public
  create public .

public section.

  types:
    BEGIN OF ts_object_info,
             name TYPE string,
             ref  TYPE REF TO cl_gui_control,
             id   TYPE string,
           END OF ts_object_info .

  methods SHOW
    importing
      !IO_CONTROL type ref to CL_GUI_CONTROL optional
      !IV_OBJECT_ID type I optional .
  methods CONSTRUCTOR
    importing
      !IV_REPID type SYREPID optional .
  class-methods FACTORY
    importing
      !IV_REPID type SYREPID optional
    returning
      value(RO_CS) type ref to ZCL_TRCKTRSR_CONTAINER_TREE .
  methods SHOW_SCREEN
    importing
      !IV_NUMBER type CHAR01 optional .
protected section.
private section.

  data MO_TREE type ref to CL_COLUMN_TREE_MODEL .
  data MO_BOX type ref to CL_GUI_DIALOGBOX_CONTAINER .
  data:
    mt_object_info TYPE SORTED TABLE OF ts_object_info WITH UNIQUE KEY name .

  methods APPEND_OBJECT
    importing
      !IO_CONTROL type ref to CL_GUI_CONTROL
      !IV_PARENT type STRING .
  methods APPEND_CHILDREN
    importing
      !IO_CONTROL type ref to CL_GUI_CONTROL .
  methods APPEND_PARENT
    importing
      !IO_CONTROL type ref to CL_GUI_CONTROL .
  methods SHOW_STRUCTURE .
  methods HANDLE_DIALOGBOX_CLOSE
    for event CLOSE of CL_GUI_DIALOGBOX_CONTAINER .
  methods GET_OBJNAME
    importing
      !IO_OBJECT type ref to CL_GUI_CONTROL
    returning
      value(RV_OBJECT_NAME) type STRING .
ENDCLASS.



CLASS ZCL_TRCKTRSR_CONTAINER_TREE IMPLEMENTATION.


  method APPEND_CHILDREN.

    DATA lo_container TYPE REF TO cl_gui_container.

    CHECK io_control IS INSTANCE OF cl_gui_container.
    lo_container ?= io_control.

    LOOP AT lo_container->children INTO DATA(lo_child).
      append_object( io_control = lo_child iv_parent = get_objname( io_control ) ).
    ENDLOOP.


  endmethod.


  METHOD append_object.



    DATA(lv_name) = get_objname( io_control ).

    DATA(lv_icon) = COND tv_image(
                            WHEN io_control IS INSTANCE OF cl_gui_container THEN icon_wd_view_container
                            WHEN io_control IS INSTANCE OF cl_gui_control THEN icon_oo_object
                            ELSE icon_message_question ).

    TRY.
        DATA(lv_varname) = mt_object_info[ id = lv_name ]-name.
      CATCH cx_sy_itab_line_not_found.
        CASE io_control.
          WHEN cl_gui_container=>default_screen. lv_varname = 'DEFAULT'.
          WHEN cl_gui_container=>desktop.        lv_varname = 'DESKTOP'.
          WHEN cl_gui_container=>screen0.        lv_varname = 'SCREEN0'.
          WHEN cl_gui_container=>screen1.        lv_varname = 'SCREEN1'.
          WHEN cl_gui_container=>screen2.        lv_varname = 'SCREEN2'.
          WHEN cl_gui_container=>screen3.        lv_varname = 'SCREEN3'.
          WHEN cl_gui_container=>screen4.        lv_varname = 'SCREEN4'.
          WHEN cl_gui_container=>screen5.        lv_varname = 'SCREEN5'.
          WHEN cl_gui_container=>screen6.        lv_varname = 'SCREEN6'.
          WHEN cl_gui_container=>screen7.        lv_varname = 'SCREEN7'.
          WHEN cl_gui_container=>screen8.        lv_varname = 'SCREEN8'.
          WHEN cl_gui_container=>screen9.        lv_varname = 'SCREEN9'.
        ENDCASE.

    ENDTRY.

    mo_tree->add_node(
      EXPORTING
        isfolder          = abap_true
        node_key          = lv_name
        relative_node_key = iv_parent
        expanded_image    = lv_icon
        image             = lv_icon
        item_table        = VALUE #( ( class     = cl_column_tree_model=>item_class_text
                                       item_name = 'CLASS'
                                       text      = lv_name )
                                     ( class     =  cl_column_tree_model=>item_class_text
                                       item_name = 'VAR'
                                       text      = lv_varname )
                                       ) ).

    "search down
    append_children( io_control ).


  ENDMETHOD.


  method APPEND_PARENT.



  endmethod.


  METHOD constructor.

    mo_box = NEW #( width = 800 height = 400 top = 20 left = 20 ).
    SET HANDLER handle_dialogbox_close FOR mo_box.


    mo_tree = NEW #(
                node_selection_mode   = cl_column_tree_model=>node_sel_mode_single
                hierarchy_column_name = 'CLASS'
                hierarchy_header      = VALUE #( width = 90 ) ).
    mo_tree->add_column(
      EXPORTING
        name                = 'VAR'
        width               = 50
        header_text         = 'Variable name' ).

    mo_tree->create_tree_control( mo_box ).

    DATA lt_fieldlist    TYPE STANDARD TABLE OF rfieldlist.
    FIELD-SYMBOLS <object> TYPE any.
    DATA control TYPE REF TO cl_gui_control.
    DATA lv_global_name TYPE string.

    IF iv_repid IS NOT INITIAL.

      CALL FUNCTION 'GET_GLOBAL_SYMBOLS'
        EXPORTING
          program      = iv_repid
          name_pattern = '*'
        TABLES
          fieldlist    = lt_fieldlist.

      DATA ls_object_info TYPE ts_object_info.

      LOOP AT lt_fieldlist INTO DATA(ls_fieldinfo)
      WHERE type       = 'r'
      AND   reftypeloc = 'CLAS'
      AND   name(1)   <> '%'.
        CLEAR ls_object_info.
        ls_object_info-name = ls_fieldinfo-name.

        TRY.
            lv_global_name = |({ iv_repid }){ ls_fieldinfo-name }|.
            ASSIGN (lv_global_name) TO <object>. " CASTING.

            IF sy-subrc = 0
            AND <object> IS BOUND
            AND <object> IS INSTANCE OF cl_gui_object.
              control ?= <object>.
              ls_object_info-ref  = control.
              ls_object_info-id   = get_objname( control ).
              INSERT ls_object_info INTO TABLE mt_object_info.
            ENDIF.
          CATCH cx_sy_assign_cast_illegal_cast.
        ENDTRY.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD factory.
    ro_cs = NEW zcl_trcktrsr_container_tree( iv_repid ).
  ENDMETHOD.


  method GET_OBJNAME.


    DATA lo_obj TYPE REF TO cl_abap_objectdescr.

    lo_obj ?= cl_abap_typedescr=>describe_by_object_ref( io_object ).
    DATA(lv_relname) = lo_obj->get_relative_name( ).

    DATA lv_object_id TYPE i.

    CALL 'OBJMGR_GET_INFO' ID 'OPNAME' FIELD 'GET_OBJID'
                           ID 'OBJID'  FIELD lv_object_id
                           ID 'OBJ'    FIELD io_object.

    DATA lv_name TYPE string.

    rv_object_name = |\{O:{ lv_object_id }*{ lo_obj->absolute_name }|.


  endmethod.


  method HANDLE_DIALOGBOX_CLOSE.

    mo_box->set_visible( space ).

  endmethod.


  METHOD show.

    DATA lo_control TYPE REF TO cl_gui_control.
    DATA lo_object  TYPE REF TO object.

    mo_tree->delete_all_nodes( ).

    IF io_control IS NOT INITIAL.
      lo_control ?= io_control.
    ELSEIF iv_object_id IS NOT INITIAL.
      CALL 'OBJMGR_GET_INFO' ID 'OPNAME' FIELD 'WEAK_REF_GET'
                       ID 'OID'    FIELD iv_object_id
                       ID 'OBJ'    FIELD lo_object.

      IF lo_object IS BOUND AND lo_object IS INSTANCE OF cl_gui_control.
        lo_control ?= lo_object.
      ENDIF.
    ENDIF.

    CHECK lo_control IS BOUND.

    append_object( io_control = lo_control iv_parent = space ).

    show_structure( ).


  ENDMETHOD.


  METHOD show_screen.


    mo_tree->delete_all_nodes( ).

    CASE iv_number.
      WHEN space. "Default
        append_object( io_control = cl_gui_container=>default_screen iv_parent = space ).
      WHEN 'D'. "Desktop
        append_object( io_control = cl_gui_container=>desktop iv_parent = space ).
      WHEN '0'.
        append_object( io_control = cl_gui_container=>screen0 iv_parent = space ).
      WHEN '1'.
        append_object( io_control = cl_gui_container=>screen1 iv_parent = space ).
      WHEN '2'.
        append_object( io_control = cl_gui_container=>screen2 iv_parent = space ).
      WHEN '3'.
        append_object( io_control = cl_gui_container=>screen3 iv_parent = space ).
      WHEN '4'.
        append_object( io_control = cl_gui_container=>screen4 iv_parent = space ).
      WHEN '5'.
        append_object( io_control = cl_gui_container=>screen5 iv_parent = space ).
      WHEN '6'.
        append_object( io_control = cl_gui_container=>screen6 iv_parent = space ).
      WHEN '7'.
        append_object( io_control = cl_gui_container=>screen7 iv_parent = space ).
      WHEN '8'.
        append_object( io_control = cl_gui_container=>screen8 iv_parent = space ).
      WHEN '9'.
        append_object( io_control = cl_gui_container=>screen9 iv_parent = space ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    show_structure( ).


  ENDMETHOD.


  METHOD show_structure.

    mo_box->set_visible( abap_true ).

    mo_tree->expand_root_nodes( expand_subtree = abap_true level_count = 10 ).

  ENDMETHOD.
ENDCLASS.
