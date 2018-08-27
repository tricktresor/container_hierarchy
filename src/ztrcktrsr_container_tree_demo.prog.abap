REPORT ztrcktrsr_container_tree_demo.


PARAMETERS p_test.

AT SELECTION-SCREEN.

*  zcl_trcktrsr_container_tree=>factory( )->show( iv_object_id = 18 ). "'{O:18*\CLASS=CL_GUI_DOCKING_CONTAINER}' ).
  zcl_trcktrsr_container_tree=>factory( )->show_screen( space ). "Deault Screen

INITIALIZATION.
  DATA(go_docker) = NEW cl_gui_docking_container( side = cl_gui_docking_container=>dock_at_bottom ratio = 80 ).
  DATA(go_split1) = NEW cl_gui_easy_splitter_container( parent = go_docker orientation = cl_gui_easy_splitter_container=>orientation_horizontal ).
  DATA(go_text1)  = NEW cl_gui_textedit( parent = go_split1->top_left_container ).

  DATA(go_split2) = NEW cl_gui_easy_splitter_container( parent = go_split1->bottom_right_container orientation = cl_gui_easy_splitter_container=>orientation_vertical ).

  DATA(go_text2)  = NEW cl_gui_textedit( parent = go_split2->top_left_container ).
  DATA(go_pict3)  = NEW cl_gui_picture( parent = go_split2->bottom_right_container ).
  go_pict3->load_picture_from_sap_icons( icon_message_critical ).
  go_pict3->set_display_mode( cl_gui_picture=>display_mode_fit_center ).
