pragma Ada_95;
pragma Warnings (Off);
pragma Source_File_Name (ada_main, Spec_File_Name => "b__zbx_gjenkins.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__zbx_gjenkins.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E155 : Short_Integer; pragma Import (Ada, E155, "system__os_lib_E");
   E013 : Short_Integer; pragma Import (Ada, E013, "system__soft_links_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__exception_table_E");
   E096 : Short_Integer; pragma Import (Ada, E096, "ada__containers_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "ada__io_exceptions_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "ada__strings_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "ada__strings__maps_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "ada__strings__maps__constants_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "ada__strings__utf_encoding_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "ada__tags_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "ada__streams_E");
   E039 : Short_Integer; pragma Import (Ada, E039, "interfaces__c_E");
   E200 : Short_Integer; pragma Import (Ada, E200, "interfaces__c__strings_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exceptions_E");
   E158 : Short_Integer; pragma Import (Ada, E158, "system__file_control_block_E");
   E188 : Short_Integer; pragma Import (Ada, E188, "ada__streams__stream_io_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "system__file_io_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "system__finalization_root_E");
   E099 : Short_Integer; pragma Import (Ada, E099, "ada__finalization_E");
   E145 : Short_Integer; pragma Import (Ada, E145, "system__storage_pools_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "system__finalization_masters_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "system__storage_pools__subpools_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "ada__calendar_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "ada__calendar__time_zones_E");
   E168 : Short_Integer; pragma Import (Ada, E168, "ada__wide_text_io_E");
   E255 : Short_Integer; pragma Import (Ada, E255, "ada__wide_text_io__text_streams_E");
   E330 : Short_Integer; pragma Import (Ada, E330, "system__assertions_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "system__object_reader_E");
   E044 : Short_Integer; pragma Import (Ada, E044, "system__dwarf_lines_E");
   E182 : Short_Integer; pragma Import (Ada, E182, "system__pool_global_E");
   E009 : Short_Integer; pragma Import (Ada, E009, "system__secondary_stack_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "ada__strings__unbounded_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "ada__strings__wide_maps_E");
   E223 : Short_Integer; pragma Import (Ada, E223, "ada__strings__wide_unbounded_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "ada__directories_E");
   E160 : Short_Integer; pragma Import (Ada, E160, "system__regexp_E");
   E034 : Short_Integer; pragma Import (Ada, E034, "system__traceback__symbolic_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "ada__text_io_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "ada__text_io__text_streams_E");
   E304 : Short_Integer; pragma Import (Ada, E304, "glib_E");
   E322 : Short_Integer; pragma Import (Ada, E322, "gtkada__types_E");
   E394 : Short_Integer; pragma Import (Ada, E394, "gdk__frame_timings_E");
   E324 : Short_Integer; pragma Import (Ada, E324, "glib__glist_E");
   E360 : Short_Integer; pragma Import (Ada, E360, "gdk__visual_E");
   E326 : Short_Integer; pragma Import (Ada, E326, "glib__gslist_E");
   E477 : Short_Integer; pragma Import (Ada, E477, "glib__key_file_E");
   E306 : Short_Integer; pragma Import (Ada, E306, "glib__object_E");
   E408 : Short_Integer; pragma Import (Ada, E408, "glib__string_E");
   E308 : Short_Integer; pragma Import (Ada, E308, "glib__type_conversion_hooks_E");
   E316 : Short_Integer; pragma Import (Ada, E316, "glib__types_E");
   E318 : Short_Integer; pragma Import (Ada, E318, "glib__values_E");
   E332 : Short_Integer; pragma Import (Ada, E332, "cairo_E");
   E337 : Short_Integer; pragma Import (Ada, E337, "cairo__region_E");
   E390 : Short_Integer; pragma Import (Ada, E390, "gdk__color_E");
   E339 : Short_Integer; pragma Import (Ada, E339, "gdk__rectangle_E");
   E350 : Short_Integer; pragma Import (Ada, E350, "gdk__rgba_E");
   E342 : Short_Integer; pragma Import (Ada, E342, "glib__generic_properties_E");
   E412 : Short_Integer; pragma Import (Ada, E412, "gtk__editable_E");
   E320 : Short_Integer; pragma Import (Ada, E320, "gtkada__c_E");
   E311 : Short_Integer; pragma Import (Ada, E311, "gtkada__bindings_E");
   E392 : Short_Integer; pragma Import (Ada, E392, "gdk__frame_clock_E");
   E335 : Short_Integer; pragma Import (Ada, E335, "gdk__event_E");
   E358 : Short_Integer; pragma Import (Ada, E358, "gdk__display_E");
   E386 : Short_Integer; pragma Import (Ada, E386, "gdk__pixbuf_E");
   E362 : Short_Integer; pragma Import (Ada, E362, "glib__properties_E");
   E356 : Short_Integer; pragma Import (Ada, E356, "gdk__screen_E");
   E384 : Short_Integer; pragma Import (Ada, E384, "gdk__device_E");
   E402 : Short_Integer; pragma Import (Ada, E402, "gdk__drag_contexts_E");
   E388 : Short_Integer; pragma Import (Ada, E388, "gdk__window_E");
   E406 : Short_Integer; pragma Import (Ada, E406, "glib__variant_E");
   E404 : Short_Integer; pragma Import (Ada, E404, "glib__g_icon_E");
   E521 : Short_Integer; pragma Import (Ada, E521, "glib__menu_model_E");
   E396 : Short_Integer; pragma Import (Ada, E396, "gtk__accel_group_E");
   E511 : Short_Integer; pragma Import (Ada, E511, "gtk__actionable_E");
   E378 : Short_Integer; pragma Import (Ada, E378, "gtk__adjustment_E");
   E372 : Short_Integer; pragma Import (Ada, E372, "gtk__builder_E");
   E370 : Short_Integer; pragma Import (Ada, E370, "gtk__buildable_E");
   E420 : Short_Integer; pragma Import (Ada, E420, "gtk__cell_area_context_E");
   E410 : Short_Integer; pragma Import (Ada, E410, "gtk__cell_editable_E");
   E438 : Short_Integer; pragma Import (Ada, E438, "gtk__css_section_E");
   E414 : Short_Integer; pragma Import (Ada, E414, "gtk__entry_buffer_E");
   E364 : Short_Integer; pragma Import (Ada, E364, "gtk__enums_E");
   E432 : Short_Integer; pragma Import (Ada, E432, "gtk__icon_source_E");
   E380 : Short_Integer; pragma Import (Ada, E380, "gtk__orientable_E");
   E479 : Short_Integer; pragma Import (Ada, E479, "gtk__paper_size_E");
   E475 : Short_Integer; pragma Import (Ada, E475, "gtk__page_setup_E");
   E487 : Short_Integer; pragma Import (Ada, E487, "gtk__print_settings_E");
   E497 : Short_Integer; pragma Import (Ada, E497, "gtk__selection_data_E");
   E434 : Short_Integer; pragma Import (Ada, E434, "gtk__style_E");
   E469 : Short_Integer; pragma Import (Ada, E469, "gtk__target_entry_E");
   E467 : Short_Integer; pragma Import (Ada, E467, "gtk__target_list_E");
   E426 : Short_Integer; pragma Import (Ada, E426, "gtk__tree_model_E");
   E443 : Short_Integer; pragma Import (Ada, E443, "pango__enums_E");
   E451 : Short_Integer; pragma Import (Ada, E451, "pango__attributes_E");
   E445 : Short_Integer; pragma Import (Ada, E445, "pango__font_metrics_E");
   E447 : Short_Integer; pragma Import (Ada, E447, "pango__language_E");
   E441 : Short_Integer; pragma Import (Ada, E441, "pango__font_E");
   E493 : Short_Integer; pragma Import (Ada, E493, "gtk__text_attributes_E");
   E495 : Short_Integer; pragma Import (Ada, E495, "gtk__text_tag_E");
   E491 : Short_Integer; pragma Import (Ada, E491, "gtk__text_iter_E");
   E457 : Short_Integer; pragma Import (Ada, E457, "pango__font_face_E");
   E455 : Short_Integer; pragma Import (Ada, E455, "pango__font_family_E");
   E459 : Short_Integer; pragma Import (Ada, E459, "pango__fontset_E");
   E461 : Short_Integer; pragma Import (Ada, E461, "pango__matrix_E");
   E453 : Short_Integer; pragma Import (Ada, E453, "pango__context_E");
   E483 : Short_Integer; pragma Import (Ada, E483, "pango__font_map_E");
   E463 : Short_Integer; pragma Import (Ada, E463, "pango__tabs_E");
   E449 : Short_Integer; pragma Import (Ada, E449, "pango__layout_E");
   E481 : Short_Integer; pragma Import (Ada, E481, "gtk__print_context_E");
   E485 : Short_Integer; pragma Import (Ada, E485, "gtk__print_operation_preview_E");
   E346 : Short_Integer; pragma Import (Ada, E346, "gtk__widget_E");
   E509 : Short_Integer; pragma Import (Ada, E509, "gtk__action_E");
   E513 : Short_Integer; pragma Import (Ada, E513, "gtk__activatable_E");
   E424 : Short_Integer; pragma Import (Ada, E424, "gtk__cell_renderer_E");
   E422 : Short_Integer; pragma Import (Ada, E422, "gtk__cell_layout_E");
   E418 : Short_Integer; pragma Import (Ada, E418, "gtk__cell_area_E");
   E376 : Short_Integer; pragma Import (Ada, E376, "gtk__container_E");
   E398 : Short_Integer; pragma Import (Ada, E398, "gtk__bin_E");
   E368 : Short_Integer; pragma Import (Ada, E368, "gtk__box_E");
   E507 : Short_Integer; pragma Import (Ada, E507, "gtk__button_E");
   E416 : Short_Integer; pragma Import (Ada, E416, "gtk__entry_completion_E");
   E534 : Short_Integer; pragma Import (Ada, E534, "gtk__main_E");
   E344 : Short_Integer; pragma Import (Ada, E344, "gtk__marshallers_E");
   E523 : Short_Integer; pragma Import (Ada, E523, "gtk__menu_item_E");
   E525 : Short_Integer; pragma Import (Ada, E525, "gtk__menu_shell_E");
   E519 : Short_Integer; pragma Import (Ada, E519, "gtk__menu_E");
   E465 : Short_Integer; pragma Import (Ada, E465, "gtk__misc_E");
   E517 : Short_Integer; pragma Import (Ada, E517, "gtk__label_E");
   E471 : Short_Integer; pragma Import (Ada, E471, "gtk__notebook_E");
   E527 : Short_Integer; pragma Import (Ada, E527, "gtk__separator_E");
   E489 : Short_Integer; pragma Import (Ada, E489, "gtk__status_bar_E");
   E366 : Short_Integer; pragma Import (Ada, E366, "gtk__style_provider_E");
   E354 : Short_Integer; pragma Import (Ada, E354, "gtk__settings_E");
   E436 : Short_Integer; pragma Import (Ada, E436, "gtk__style_context_E");
   E430 : Short_Integer; pragma Import (Ada, E430, "gtk__icon_set_E");
   E428 : Short_Integer; pragma Import (Ada, E428, "gtk__image_E");
   E400 : Short_Integer; pragma Import (Ada, E400, "gtk__gentry_E");
   E499 : Short_Integer; pragma Import (Ada, E499, "gtk__tree_view_column_E");
   E382 : Short_Integer; pragma Import (Ada, E382, "gtk__window_E");
   E352 : Short_Integer; pragma Import (Ada, E352, "gtk__dialog_E");
   E473 : Short_Integer; pragma Import (Ada, E473, "gtk__print_operation_E");
   E348 : Short_Integer; pragma Import (Ada, E348, "gtk__arguments_E");
   E532 : Short_Integer; pragma Import (Ada, E532, "text_display_E");
   E194 : Short_Integer; pragma Import (Ada, E194, "zanyblue__os_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "zanyblue__text_E");
   E235 : Short_Integer; pragma Import (Ada, E235, "zanyblue__text__codecs_E");
   E245 : Short_Integer; pragma Import (Ada, E245, "zanyblue__text__filter_E");
   E244 : Short_Integer; pragma Import (Ada, E244, "zanyblue__text__format_errors_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "zanyblue__text__indexed_strings_E");
   E257 : Short_Integer; pragma Import (Ada, E257, "zanyblue__text__pseudo_E");
   E233 : Short_Integer; pragma Import (Ada, E233, "zanyblue__text__utils_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "zanyblue__wide_directories_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "zanyblue__text__locales_E");
   E501 : Short_Integer; pragma Import (Ada, E501, "locale_buttons_E");
   E301 : Short_Integer; pragma Import (Ada, E301, "button_cb_E");
   E515 : Short_Integer; pragma Import (Ada, E515, "display_strings_E");
   E225 : Short_Integer; pragma Import (Ada, E225, "zanyblue__text__arguments_E");
   E263 : Short_Integer; pragma Import (Ada, E263, "zanyblue__text__booleans_E");
   E269 : Short_Integer; pragma Import (Ada, E269, "zanyblue__text__characters_E");
   E503 : Short_Integer; pragma Import (Ada, E503, "zanyblue__text__cldr_E");
   E271 : Short_Integer; pragma Import (Ada, E271, "zanyblue__text__durations_E");
   E273 : Short_Integer; pragma Import (Ada, E273, "zanyblue__text__exceptions_E");
   E261 : Short_Integer; pragma Import (Ada, E261, "zanyblue__text__format_message_E");
   E265 : Short_Integer; pragma Import (Ada, E265, "zanyblue__text__format_parser_E");
   E229 : Short_Integer; pragma Import (Ada, E229, "zanyblue__text__generic_buffer_E");
   E538 : Short_Integer; pragma Import (Ada, E538, "zanyblue__text__generic_enumerations_E");
   E278 : Short_Integer; pragma Import (Ada, E278, "zanyblue__text__generic_floats_E");
   E275 : Short_Integer; pragma Import (Ada, E275, "zanyblue__text__floats_E");
   E282 : Short_Integer; pragma Import (Ada, E282, "zanyblue__text__generic_integers_E");
   E249 : Short_Integer; pragma Import (Ada, E249, "zanyblue__text__generic_printer_E");
   E280 : Short_Integer; pragma Import (Ada, E280, "zanyblue__text__integers_E");
   E284 : Short_Integer; pragma Import (Ada, E284, "zanyblue__text__long_floats_E");
   E287 : Short_Integer; pragma Import (Ada, E287, "zanyblue__text__null_object_E");
   E247 : Short_Integer; pragma Import (Ada, E247, "zanyblue__text__printer_E");
   E251 : Short_Integer; pragma Import (Ada, E251, "zanyblue__text__properties_parser_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "zanyblue__text__catalogs_E");
   E221 : Short_Integer; pragma Import (Ada, E221, "zanyblue__text__message_maps_E");
   E289 : Short_Integer; pragma Import (Ada, E289, "zanyblue__text__strings_E");
   E291 : Short_Integer; pragma Import (Ada, E291, "zanyblue__text__times_E");
   E293 : Short_Integer; pragma Import (Ada, E293, "zanyblue__text__unbounded_strings_E");
   E536 : Short_Integer; pragma Import (Ada, E536, "zanyblue__text__version_status_arguments_E");
   E299 : Short_Integer; pragma Import (Ada, E299, "zanyblue__text__wide_characters_E");
   E297 : Short_Integer; pragma Import (Ada, E297, "zanyblue__text__wide_strings_E");
   E295 : Short_Integer; pragma Import (Ada, E295, "zanyblue__text__unbounded_wide_strings_E");
   E259 : Short_Integer; pragma Import (Ada, E259, "zanyblue__text__formatting_E");
   E093 : Short_Integer; pragma Import (Ada, E093, "appmsg_E");
   E530 : Short_Integer; pragma Import (Ada, E530, "jenkins__messages_E");
   E505 : Short_Integer; pragma Import (Ada, E505, "zanyblue__text__cldr_data_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E501 := E501 - 1;
      E297 := E297 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "zanyblue__text__wide_strings__finalize_spec");
      begin
         F1;
      end;
      E299 := E299 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "zanyblue__text__wide_characters__finalize_spec");
      begin
         F2;
      end;
      E291 := E291 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "zanyblue__text__times__finalize_spec");
      begin
         F3;
      end;
      E289 := E289 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "zanyblue__text__strings__finalize_spec");
      begin
         F4;
      end;
      E095 := E095 - 1;
      E221 := E221 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "zanyblue__text__message_maps__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "zanyblue__text__catalogs__finalize_spec");
      begin
         F6;
      end;
      E247 := E247 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "zanyblue__text__printer__finalize_spec");
      begin
         F7;
      end;
      E287 := E287 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "zanyblue__text__null_object__finalize_spec");
      begin
         F8;
      end;
      E225 := E225 - 1;
      E271 := E271 - 1;
      E263 := E263 - 1;
      E269 := E269 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "zanyblue__text__format_parser__finalize_body");
      begin
         E265 := E265 - 1;
         F9;
      end;
      E273 := E273 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "zanyblue__text__exceptions__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "zanyblue__text__durations__finalize_spec");
      begin
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "zanyblue__text__characters__finalize_spec");
      begin
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "zanyblue__text__booleans__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "zanyblue__text__arguments__finalize_spec");
      begin
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "locale_buttons__finalize_spec");
      begin
         F15;
      end;
      E231 := E231 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "zanyblue__text__locales__finalize_spec");
      begin
         F16;
      end;
      E235 := E235 - 1;
      E257 := E257 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "zanyblue__text__pseudo__finalize_spec");
      begin
         F17;
      end;
      E216 := E216 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "zanyblue__text__indexed_strings__finalize_spec");
      begin
         F18;
      end;
      E244 := E244 - 1;
      declare
         procedure F19;
         pragma Import (Ada, F19, "zanyblue__text__format_errors__finalize_spec");
      begin
         F19;
      end;
      declare
         procedure F20;
         pragma Import (Ada, F20, "zanyblue__text__filter__finalize_spec");
      begin
         E245 := E245 - 1;
         F20;
      end;
      declare
         procedure F21;
         pragma Import (Ada, F21, "zanyblue__text__codecs__finalize_spec");
      begin
         F21;
      end;
      E532 := E532 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "text_display__finalize_spec");
      begin
         F22;
      end;
      E392 := E392 - 1;
      E358 := E358 - 1;
      E521 := E521 - 1;
      E396 := E396 - 1;
      E378 := E378 - 1;
      E414 := E414 - 1;
      E434 := E434 - 1;
      E426 := E426 - 1;
      E346 := E346 - 1;
      E509 := E509 - 1;
      E424 := E424 - 1;
      E418 := E418 - 1;
      E376 := E376 - 1;
      E507 := E507 - 1;
      E416 := E416 - 1;
      E523 := E523 - 1;
      E525 := E525 - 1;
      E519 := E519 - 1;
      E517 := E517 - 1;
      E471 := E471 - 1;
      E489 := E489 - 1;
      E436 := E436 - 1;
      E400 := E400 - 1;
      E499 := E499 - 1;
      E382 := E382 - 1;
      E352 := E352 - 1;
      E473 := E473 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "gtk__print_operation__finalize_spec");
      begin
         F23;
      end;
      declare
         procedure F24;
         pragma Import (Ada, F24, "gtk__dialog__finalize_spec");
      begin
         F24;
      end;
      declare
         procedure F25;
         pragma Import (Ada, F25, "gtk__window__finalize_spec");
      begin
         F25;
      end;
      declare
         procedure F26;
         pragma Import (Ada, F26, "gtk__tree_view_column__finalize_spec");
      begin
         F26;
      end;
      declare
         procedure F27;
         pragma Import (Ada, F27, "gtk__gentry__finalize_spec");
      begin
         F27;
      end;
      E428 := E428 - 1;
      declare
         procedure F28;
         pragma Import (Ada, F28, "gtk__image__finalize_spec");
      begin
         F28;
      end;
      E430 := E430 - 1;
      declare
         procedure F29;
         pragma Import (Ada, F29, "gtk__icon_set__finalize_spec");
      begin
         F29;
      end;
      declare
         procedure F30;
         pragma Import (Ada, F30, "gtk__style_context__finalize_spec");
      begin
         F30;
      end;
      E354 := E354 - 1;
      declare
         procedure F31;
         pragma Import (Ada, F31, "gtk__settings__finalize_spec");
      begin
         F31;
      end;
      declare
         procedure F32;
         pragma Import (Ada, F32, "gtk__status_bar__finalize_spec");
      begin
         F32;
      end;
      E527 := E527 - 1;
      declare
         procedure F33;
         pragma Import (Ada, F33, "gtk__separator__finalize_spec");
      begin
         F33;
      end;
      declare
         procedure F34;
         pragma Import (Ada, F34, "gtk__notebook__finalize_spec");
      begin
         F34;
      end;
      declare
         procedure F35;
         pragma Import (Ada, F35, "gtk__label__finalize_spec");
      begin
         F35;
      end;
      E465 := E465 - 1;
      declare
         procedure F36;
         pragma Import (Ada, F36, "gtk__misc__finalize_spec");
      begin
         F36;
      end;
      declare
         procedure F37;
         pragma Import (Ada, F37, "gtk__menu__finalize_spec");
      begin
         F37;
      end;
      declare
         procedure F38;
         pragma Import (Ada, F38, "gtk__menu_shell__finalize_spec");
      begin
         F38;
      end;
      declare
         procedure F39;
         pragma Import (Ada, F39, "gtk__menu_item__finalize_spec");
      begin
         F39;
      end;
      declare
         procedure F40;
         pragma Import (Ada, F40, "gtk__entry_completion__finalize_spec");
      begin
         F40;
      end;
      declare
         procedure F41;
         pragma Import (Ada, F41, "gtk__button__finalize_spec");
      begin
         F41;
      end;
      E368 := E368 - 1;
      declare
         procedure F42;
         pragma Import (Ada, F42, "gtk__box__finalize_spec");
      begin
         F42;
      end;
      E398 := E398 - 1;
      declare
         procedure F43;
         pragma Import (Ada, F43, "gtk__bin__finalize_spec");
      begin
         F43;
      end;
      declare
         procedure F44;
         pragma Import (Ada, F44, "gtk__container__finalize_spec");
      begin
         F44;
      end;
      declare
         procedure F45;
         pragma Import (Ada, F45, "gtk__cell_area__finalize_spec");
      begin
         F45;
      end;
      declare
         procedure F46;
         pragma Import (Ada, F46, "gtk__cell_renderer__finalize_spec");
      begin
         F46;
      end;
      declare
         procedure F47;
         pragma Import (Ada, F47, "gtk__action__finalize_spec");
      begin
         F47;
      end;
      declare
         procedure F48;
         pragma Import (Ada, F48, "gtk__widget__finalize_spec");
      begin
         F48;
      end;
      E481 := E481 - 1;
      declare
         procedure F49;
         pragma Import (Ada, F49, "gtk__print_context__finalize_spec");
      begin
         F49;
      end;
      E449 := E449 - 1;
      declare
         procedure F50;
         pragma Import (Ada, F50, "pango__layout__finalize_spec");
      begin
         F50;
      end;
      E463 := E463 - 1;
      declare
         procedure F51;
         pragma Import (Ada, F51, "pango__tabs__finalize_spec");
      begin
         F51;
      end;
      E483 := E483 - 1;
      declare
         procedure F52;
         pragma Import (Ada, F52, "pango__font_map__finalize_spec");
      begin
         F52;
      end;
      E453 := E453 - 1;
      declare
         procedure F53;
         pragma Import (Ada, F53, "pango__context__finalize_spec");
      begin
         F53;
      end;
      E459 := E459 - 1;
      declare
         procedure F54;
         pragma Import (Ada, F54, "pango__fontset__finalize_spec");
      begin
         F54;
      end;
      E455 := E455 - 1;
      declare
         procedure F55;
         pragma Import (Ada, F55, "pango__font_family__finalize_spec");
      begin
         F55;
      end;
      E457 := E457 - 1;
      declare
         procedure F56;
         pragma Import (Ada, F56, "pango__font_face__finalize_spec");
      begin
         F56;
      end;
      E495 := E495 - 1;
      declare
         procedure F57;
         pragma Import (Ada, F57, "gtk__text_tag__finalize_spec");
      begin
         F57;
      end;
      E441 := E441 - 1;
      declare
         procedure F58;
         pragma Import (Ada, F58, "pango__font__finalize_spec");
      begin
         F58;
      end;
      E447 := E447 - 1;
      declare
         procedure F59;
         pragma Import (Ada, F59, "pango__language__finalize_spec");
      begin
         F59;
      end;
      E445 := E445 - 1;
      declare
         procedure F60;
         pragma Import (Ada, F60, "pango__font_metrics__finalize_spec");
      begin
         F60;
      end;
      E451 := E451 - 1;
      declare
         procedure F61;
         pragma Import (Ada, F61, "pango__attributes__finalize_spec");
      begin
         F61;
      end;
      declare
         procedure F62;
         pragma Import (Ada, F62, "gtk__tree_model__finalize_spec");
      begin
         F62;
      end;
      E467 := E467 - 1;
      declare
         procedure F63;
         pragma Import (Ada, F63, "gtk__target_list__finalize_spec");
      begin
         F63;
      end;
      declare
         procedure F64;
         pragma Import (Ada, F64, "gtk__style__finalize_spec");
      begin
         F64;
      end;
      E497 := E497 - 1;
      declare
         procedure F65;
         pragma Import (Ada, F65, "gtk__selection_data__finalize_spec");
      begin
         F65;
      end;
      E487 := E487 - 1;
      declare
         procedure F66;
         pragma Import (Ada, F66, "gtk__print_settings__finalize_spec");
      begin
         F66;
      end;
      E475 := E475 - 1;
      declare
         procedure F67;
         pragma Import (Ada, F67, "gtk__page_setup__finalize_spec");
      begin
         F67;
      end;
      E479 := E479 - 1;
      declare
         procedure F68;
         pragma Import (Ada, F68, "gtk__paper_size__finalize_spec");
      begin
         F68;
      end;
      E432 := E432 - 1;
      declare
         procedure F69;
         pragma Import (Ada, F69, "gtk__icon_source__finalize_spec");
      begin
         F69;
      end;
      declare
         procedure F70;
         pragma Import (Ada, F70, "gtk__entry_buffer__finalize_spec");
      begin
         F70;
      end;
      E438 := E438 - 1;
      declare
         procedure F71;
         pragma Import (Ada, F71, "gtk__css_section__finalize_spec");
      begin
         F71;
      end;
      E420 := E420 - 1;
      declare
         procedure F72;
         pragma Import (Ada, F72, "gtk__cell_area_context__finalize_spec");
      begin
         F72;
      end;
      E372 := E372 - 1;
      declare
         procedure F73;
         pragma Import (Ada, F73, "gtk__builder__finalize_spec");
      begin
         F73;
      end;
      declare
         procedure F74;
         pragma Import (Ada, F74, "gtk__adjustment__finalize_spec");
      begin
         F74;
      end;
      declare
         procedure F75;
         pragma Import (Ada, F75, "gtk__accel_group__finalize_spec");
      begin
         F75;
      end;
      declare
         procedure F76;
         pragma Import (Ada, F76, "glib__menu_model__finalize_spec");
      begin
         F76;
      end;
      E406 := E406 - 1;
      declare
         procedure F77;
         pragma Import (Ada, F77, "glib__variant__finalize_spec");
      begin
         F77;
      end;
      E402 := E402 - 1;
      declare
         procedure F78;
         pragma Import (Ada, F78, "gdk__drag_contexts__finalize_spec");
      begin
         F78;
      end;
      E384 := E384 - 1;
      declare
         procedure F79;
         pragma Import (Ada, F79, "gdk__device__finalize_spec");
      begin
         F79;
      end;
      E356 := E356 - 1;
      declare
         procedure F80;
         pragma Import (Ada, F80, "gdk__screen__finalize_spec");
      begin
         F80;
      end;
      E386 := E386 - 1;
      declare
         procedure F81;
         pragma Import (Ada, F81, "gdk__pixbuf__finalize_spec");
      begin
         F81;
      end;
      declare
         procedure F82;
         pragma Import (Ada, F82, "gdk__display__finalize_spec");
      begin
         F82;
      end;
      declare
         procedure F83;
         pragma Import (Ada, F83, "gdk__frame_clock__finalize_spec");
      begin
         F83;
      end;
      E306 := E306 - 1;
      declare
         procedure F84;
         pragma Import (Ada, F84, "glib__object__finalize_spec");
      begin
         F84;
      end;
      E394 := E394 - 1;
      declare
         procedure F85;
         pragma Import (Ada, F85, "gdk__frame_timings__finalize_spec");
      begin
         F85;
      end;
      E304 := E304 - 1;
      declare
         procedure F86;
         pragma Import (Ada, F86, "glib__finalize_spec");
      begin
         F86;
      end;
      E206 := E206 - 1;
      declare
         procedure F87;
         pragma Import (Ada, F87, "ada__text_io__finalize_spec");
      begin
         F87;
      end;
      E168 := E168 - 1;
      E108 := E108 - 1;
      E160 := E160 - 1;
      declare
         procedure F88;
         pragma Import (Ada, F88, "system__regexp__finalize_spec");
      begin
         F88;
      end;
      declare
         procedure F89;
         pragma Import (Ada, F89, "ada__directories__finalize_spec");
      begin
         F89;
      end;
      E223 := E223 - 1;
      declare
         procedure F90;
         pragma Import (Ada, F90, "ada__strings__wide_unbounded__finalize_spec");
      begin
         F90;
      end;
      E164 := E164 - 1;
      declare
         procedure F91;
         pragma Import (Ada, F91, "ada__strings__wide_maps__finalize_spec");
      begin
         F91;
      end;
      E135 := E135 - 1;
      declare
         procedure F92;
         pragma Import (Ada, F92, "ada__strings__unbounded__finalize_spec");
      begin
         F92;
      end;
      declare
         procedure F93;
         pragma Import (Ada, F93, "system__file_io__finalize_body");
      begin
         E153 := E153 - 1;
         F93;
      end;
      E141 := E141 - 1;
      E139 := E139 - 1;
      E182 := E182 - 1;
      declare
         procedure F94;
         pragma Import (Ada, F94, "system__pool_global__finalize_spec");
      begin
         F94;
      end;
      declare
         procedure F95;
         pragma Import (Ada, F95, "ada__wide_text_io__finalize_spec");
      begin
         F95;
      end;
      declare
         procedure F96;
         pragma Import (Ada, F96, "system__storage_pools__subpools__finalize_spec");
      begin
         F96;
      end;
      declare
         procedure F97;
         pragma Import (Ada, F97, "system__finalization_masters__finalize_spec");
      begin
         F97;
      end;
      E188 := E188 - 1;
      declare
         procedure F98;
         pragma Import (Ada, F98, "ada__streams__stream_io__finalize_spec");
      begin
         F98;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := '8';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E019 := E019 + 1;
      Ada.Containers'Elab_Spec;
      E096 := E096 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E102 := E102 + 1;
      Ada.Strings'Elab_Spec;
      E049 := E049 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E055 := E055 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E101 := E101 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E021 := E021 + 1;
      System.File_Control_Block'Elab_Spec;
      E158 := E158 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E188 := E188 + 1;
      System.Finalization_Root'Elab_Spec;
      E104 := E104 + 1;
      Ada.Finalization'Elab_Spec;
      E099 := E099 + 1;
      System.Storage_Pools'Elab_Spec;
      E145 := E145 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E110 := E110 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E116 := E116 + 1;
      Ada.Wide_Text_Io'Elab_Spec;
      Ada.Wide_Text_Io.Text_Streams'Elab_Spec;
      E255 := E255 + 1;
      System.Assertions'Elab_Spec;
      E330 := E330 + 1;
      System.Object_Reader'Elab_Spec;
      System.Dwarf_Lines'Elab_Spec;
      System.Pool_Global'Elab_Spec;
      E182 := E182 + 1;
      E139 := E139 + 1;
      System.Finalization_Masters'Elab_Body;
      E141 := E141 + 1;
      System.File_Io'Elab_Body;
      E153 := E153 + 1;
      E200 := E200 + 1;
      E039 := E039 + 1;
      Ada.Tags'Elab_Body;
      E087 := E087 + 1;
      E212 := E212 + 1;
      E051 := E051 + 1;
      System.Soft_Links'Elab_Body;
      E013 := E013 + 1;
      System.Os_Lib'Elab_Body;
      E155 := E155 + 1;
      System.Secondary_Stack'Elab_Body;
      E009 := E009 + 1;
      E044 := E044 + 1;
      E063 := E063 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E135 := E135 + 1;
      Ada.Strings.Wide_Maps'Elab_Spec;
      E164 := E164 + 1;
      Ada.Strings.Wide_Unbounded'Elab_Spec;
      E223 := E223 + 1;
      Ada.Directories'Elab_Spec;
      System.Regexp'Elab_Spec;
      E160 := E160 + 1;
      Ada.Directories'Elab_Body;
      E108 := E108 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E034 := E034 + 1;
      Ada.Wide_Text_Io'Elab_Body;
      E168 := E168 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E206 := E206 + 1;
      Ada.Text_Io.Text_Streams'Elab_Spec;
      E208 := E208 + 1;
      Glib'Elab_Spec;
      E304 := E304 + 1;
      Gtkada.Types'Elab_Spec;
      E322 := E322 + 1;
      Gdk.Frame_Timings'Elab_Spec;
      E394 := E394 + 1;
      E324 := E324 + 1;
      Gdk.Visual'Elab_Body;
      E360 := E360 + 1;
      E326 := E326 + 1;
      Glib.Object'Elab_Spec;
      E308 := E308 + 1;
      Glib.Values'Elab_Body;
      E318 := E318 + 1;
      E332 := E332 + 1;
      E337 := E337 + 1;
      Gdk.Color'Elab_Spec;
      E339 := E339 + 1;
      Glib.Generic_Properties'Elab_Spec;
      Glib.Generic_Properties'Elab_Body;
      E342 := E342 + 1;
      E320 := E320 + 1;
      E311 := E311 + 1;
      E316 := E316 + 1;
      E408 := E408 + 1;
      E306 := E306 + 1;
      E350 := E350 + 1;
      E390 := E390 + 1;
      E477 := E477 + 1;
      Gdk.Frame_Clock'Elab_Spec;
      E335 := E335 + 1;
      Gdk.Display'Elab_Spec;
      Gdk.Pixbuf'Elab_Spec;
      E386 := E386 + 1;
      E362 := E362 + 1;
      Gdk.Screen'Elab_Spec;
      Gdk.Screen'Elab_Body;
      E356 := E356 + 1;
      Gdk.Device'Elab_Spec;
      Gdk.Device'Elab_Body;
      E384 := E384 + 1;
      Gdk.Drag_Contexts'Elab_Spec;
      Gdk.Drag_Contexts'Elab_Body;
      E402 := E402 + 1;
      Gdk.Window'Elab_Spec;
      E388 := E388 + 1;
      Glib.Variant'Elab_Spec;
      E406 := E406 + 1;
      E404 := E404 + 1;
      Glib.Menu_Model'Elab_Spec;
      Gtk.Accel_Group'Elab_Spec;
      Gtk.Actionable'Elab_Spec;
      E511 := E511 + 1;
      Gtk.Adjustment'Elab_Spec;
      Gtk.Builder'Elab_Spec;
      Gtk.Builder'Elab_Body;
      E372 := E372 + 1;
      E370 := E370 + 1;
      Gtk.Cell_Area_Context'Elab_Spec;
      Gtk.Cell_Area_Context'Elab_Body;
      E420 := E420 + 1;
      Gtk.Cell_Editable'Elab_Spec;
      Gtk.Css_Section'Elab_Spec;
      E438 := E438 + 1;
      Gtk.Entry_Buffer'Elab_Spec;
      E364 := E364 + 1;
      Gtk.Icon_Source'Elab_Spec;
      E432 := E432 + 1;
      Gtk.Orientable'Elab_Spec;
      E380 := E380 + 1;
      Gtk.Paper_Size'Elab_Spec;
      E479 := E479 + 1;
      Gtk.Page_Setup'Elab_Spec;
      Gtk.Page_Setup'Elab_Body;
      E475 := E475 + 1;
      Gtk.Print_Settings'Elab_Spec;
      Gtk.Print_Settings'Elab_Body;
      E487 := E487 + 1;
      Gtk.Selection_Data'Elab_Spec;
      E497 := E497 + 1;
      Gtk.Style'Elab_Spec;
      E469 := E469 + 1;
      Gtk.Target_List'Elab_Spec;
      E467 := E467 + 1;
      Gtk.Tree_Model'Elab_Spec;
      E443 := E443 + 1;
      Pango.Attributes'Elab_Spec;
      E451 := E451 + 1;
      Pango.Font_Metrics'Elab_Spec;
      E445 := E445 + 1;
      Pango.Language'Elab_Spec;
      E447 := E447 + 1;
      Pango.Font'Elab_Spec;
      Pango.Font'Elab_Body;
      E441 := E441 + 1;
      E493 := E493 + 1;
      Gtk.Text_Tag'Elab_Spec;
      Gtk.Text_Tag'Elab_Body;
      E495 := E495 + 1;
      E491 := E491 + 1;
      Pango.Font_Face'Elab_Spec;
      Pango.Font_Face'Elab_Body;
      E457 := E457 + 1;
      Pango.Font_Family'Elab_Spec;
      Pango.Font_Family'Elab_Body;
      E455 := E455 + 1;
      Pango.Fontset'Elab_Spec;
      Pango.Fontset'Elab_Body;
      E459 := E459 + 1;
      E461 := E461 + 1;
      Pango.Context'Elab_Spec;
      Pango.Context'Elab_Body;
      E453 := E453 + 1;
      Pango.Font_Map'Elab_Spec;
      Pango.Font_Map'Elab_Body;
      E483 := E483 + 1;
      Pango.Tabs'Elab_Spec;
      E463 := E463 + 1;
      Pango.Layout'Elab_Spec;
      Pango.Layout'Elab_Body;
      E449 := E449 + 1;
      Gtk.Print_Context'Elab_Spec;
      Gtk.Print_Context'Elab_Body;
      E481 := E481 + 1;
      Gtk.Widget'Elab_Spec;
      Gtk.Action'Elab_Spec;
      Gtk.Activatable'Elab_Spec;
      E513 := E513 + 1;
      Gtk.Cell_Renderer'Elab_Spec;
      E422 := E422 + 1;
      Gtk.Cell_Area'Elab_Spec;
      Gtk.Container'Elab_Spec;
      Gtk.Bin'Elab_Spec;
      Gtk.Bin'Elab_Body;
      E398 := E398 + 1;
      Gtk.Box'Elab_Spec;
      Gtk.Box'Elab_Body;
      E368 := E368 + 1;
      Gtk.Button'Elab_Spec;
      Gtk.Entry_Completion'Elab_Spec;
      E534 := E534 + 1;
      E344 := E344 + 1;
      Gtk.Menu_Item'Elab_Spec;
      Gtk.Menu_Shell'Elab_Spec;
      Gtk.Menu'Elab_Spec;
      Gtk.Misc'Elab_Spec;
      Gtk.Misc'Elab_Body;
      E465 := E465 + 1;
      Gtk.Label'Elab_Spec;
      Gtk.Notebook'Elab_Spec;
      Gtk.Separator'Elab_Spec;
      Gtk.Separator'Elab_Body;
      E527 := E527 + 1;
      Gtk.Status_Bar'Elab_Spec;
      E366 := E366 + 1;
      Gtk.Settings'Elab_Spec;
      Gtk.Settings'Elab_Body;
      E354 := E354 + 1;
      Gtk.Style_Context'Elab_Spec;
      Gtk.Icon_Set'Elab_Spec;
      E430 := E430 + 1;
      Gtk.Image'Elab_Spec;
      Gtk.Image'Elab_Body;
      E428 := E428 + 1;
      Gtk.Gentry'Elab_Spec;
      Gtk.Tree_View_Column'Elab_Spec;
      Gtk.Window'Elab_Spec;
      Gtk.Dialog'Elab_Spec;
      Gtk.Print_Operation'Elab_Spec;
      E348 := E348 + 1;
      Gtk.Print_Operation'Elab_Body;
      E473 := E473 + 1;
      Gtk.Dialog'Elab_Body;
      E352 := E352 + 1;
      Gtk.Window'Elab_Body;
      E382 := E382 + 1;
      Gtk.Tree_View_Column'Elab_Body;
      E499 := E499 + 1;
      Gtk.Gentry'Elab_Body;
      E400 := E400 + 1;
      Gtk.Style_Context'Elab_Body;
      E436 := E436 + 1;
      Gtk.Status_Bar'Elab_Body;
      E489 := E489 + 1;
      Gtk.Notebook'Elab_Body;
      E471 := E471 + 1;
      Gtk.Label'Elab_Body;
      E517 := E517 + 1;
      Gtk.Menu'Elab_Body;
      E519 := E519 + 1;
      Gtk.Menu_Shell'Elab_Body;
      E525 := E525 + 1;
      Gtk.Menu_Item'Elab_Body;
      E523 := E523 + 1;
      Gtk.Entry_Completion'Elab_Body;
      E416 := E416 + 1;
      Gtk.Button'Elab_Body;
      E507 := E507 + 1;
      Gtk.Container'Elab_Body;
      E376 := E376 + 1;
      Gtk.Cell_Area'Elab_Body;
      E418 := E418 + 1;
      Gtk.Cell_Renderer'Elab_Body;
      E424 := E424 + 1;
      Gtk.Action'Elab_Body;
      E509 := E509 + 1;
      Gtk.Widget'Elab_Body;
      E346 := E346 + 1;
      E485 := E485 + 1;
      E426 := E426 + 1;
      Gtk.Style'Elab_Body;
      E434 := E434 + 1;
      Gtk.Entry_Buffer'Elab_Body;
      E414 := E414 + 1;
      E410 := E410 + 1;
      Gtk.Adjustment'Elab_Body;
      E378 := E378 + 1;
      Gtk.Accel_Group'Elab_Body;
      E396 := E396 + 1;
      Glib.Menu_Model'Elab_Body;
      E521 := E521 + 1;
      Gdk.Display'Elab_Body;
      E358 := E358 + 1;
      Gdk.Frame_Clock'Elab_Body;
      E392 := E392 + 1;
      E412 := E412 + 1;
      Text_Display'Elab_Spec;
      E532 := E532 + 1;
      Zanyblue.Text'Elab_Spec;
      Zanyblue.Text.Codecs'Elab_Spec;
      Zanyblue.Text.Filter'Elab_Spec;
      E245 := E245 + 1;
      Zanyblue.Text.Format_Errors'Elab_Spec;
      E244 := E244 + 1;
      Zanyblue.Text.Indexed_Strings'Elab_Spec;
      E216 := E216 + 1;
      Zanyblue.Text.Pseudo'Elab_Spec;
      E257 := E257 + 1;
      E233 := E233 + 1;
      E210 := E210 + 1;
      E204 := E204 + 1;
      E194 := E194 + 1;
      Zanyblue.Text.Codecs'Elab_Body;
      E235 := E235 + 1;
      Zanyblue.Text.Locales'Elab_Spec;
      Zanyblue.Text.Locales'Elab_Body;
      E231 := E231 + 1;
      Locale_Buttons'Elab_Spec;
      Button_Cb'Elab_Spec;
      E301 := E301 + 1;
      Zanyblue.Text.Arguments'Elab_Spec;
      Zanyblue.Text.Booleans'Elab_Spec;
      Zanyblue.Text.Characters'Elab_Spec;
      Zanyblue.Text.Durations'Elab_Spec;
      Zanyblue.Text.Exceptions'Elab_Spec;
      E273 := E273 + 1;
      E261 := E261 + 1;
      Zanyblue.Text.Format_Parser'Elab_Spec;
      Zanyblue.Text.Format_Parser'Elab_Body;
      E265 := E265 + 1;
      E269 := E269 + 1;
      E263 := E263 + 1;
      E229 := E229 + 1;
      E271 := E271 + 1;
      E225 := E225 + 1;
      E538 := E538 + 1;
      E278 := E278 + 1;
      Zanyblue.Text.Floats'Elab_Spec;
      E275 := E275 + 1;
      E282 := E282 + 1;
      E249 := E249 + 1;
      Zanyblue.Text.Integers'Elab_Spec;
      E280 := E280 + 1;
      Zanyblue.Text.Long_Floats'Elab_Spec;
      E284 := E284 + 1;
      Zanyblue.Text.Null_Object'Elab_Spec;
      E287 := E287 + 1;
      Zanyblue.Text.Printer'Elab_Spec;
      Zanyblue.Text.Printer'Elab_Body;
      E247 := E247 + 1;
      Zanyblue.Text.Properties_Parser'Elab_Spec;
      E251 := E251 + 1;
      Zanyblue.Text.Catalogs'Elab_Spec;
      Zanyblue.Text.Message_Maps'Elab_Spec;
      E221 := E221 + 1;
      Zanyblue.Text.Catalogs'Elab_Body;
      E095 := E095 + 1;
      Zanyblue.Text.Strings'Elab_Spec;
      E289 := E289 + 1;
      Zanyblue.Text.Times'Elab_Spec;
      E291 := E291 + 1;
      E293 := E293 + 1;
      Zanyblue.Text.Version_Status_Arguments'Elab_Spec;
      E536 := E536 + 1;
      Zanyblue.Text.Wide_Characters'Elab_Spec;
      E299 := E299 + 1;
      Zanyblue.Text.Wide_Strings'Elab_Spec;
      E297 := E297 + 1;
      E295 := E295 + 1;
      Zanyblue.Text.Formatting'Elab_Body;
      E259 := E259 + 1;
      E501 := E501 + 1;
      APPMSG'ELAB_BODY;
      E093 := E093 + 1;
      Jenkins.Messages'Elab_Body;
      E530 := E530 + 1;
      E515 := E515 + 1;
      E505 := E505 + 1;
      Zanyblue.Text.Cldr'Elab_Body;
      E503 := E503 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_zbx_gjenkins");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /u/mrohan/zb/ZANYBLUE-1.3.0B/examples/text/gtkapp/jenkins.o
   --   /u/mrohan/zb/ZANYBLUE-1.3.0B/examples/text/gtkapp/text_display.o
   --   /u/mrohan/zb/ZANYBLUE-1.3.0B/examples/text/gtkapp/button_cb.o
   --   /u/mrohan/zb/ZANYBLUE-1.3.0B/examples/text/gtkapp/locale_buttons.o
   --   /u/mrohan/zb/ZANYBLUE-1.3.0B/examples/text/gtkapp/appmsg.o
   --   /u/mrohan/zb/ZANYBLUE-1.3.0B/examples/text/gtkapp/jenkins-messages.o
   --   /u/mrohan/zb/ZANYBLUE-1.3.0B/examples/text/gtkapp/display_strings.o
   --   /u/mrohan/zb/ZANYBLUE-1.3.0B/examples/text/gtkapp/zbx_gjenkins.o
   --   -L/u/mrohan/zb/ZANYBLUE-1.3.0B/examples/text/gtkapp/
   --   -L/u/mrohan/zb/ZANYBLUE-1.3.0B/examples/text/gtkapp/
   --   -L/usr/gnat/2016/lib/gtkada/gtkada.static/gtkada/
   --   -L/u/mrohan/zb/ZANYBLUE-1.3.0B/lib/zanyblue/
   --   -L/usr/gnat/2016/lib/gcc/x86_64-pc-linux-gnu/4.9.4/adalib/
   --   -static
   --   -shared-libgcc
   --   -shared-libgcc
   --   -shared-libgcc
   --   -lgnat
   --   -ldl
--  END Object file/option list   

end ada_main;
