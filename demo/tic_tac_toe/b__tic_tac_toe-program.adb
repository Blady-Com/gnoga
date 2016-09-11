pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__tic_tac_toe-program.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__tic_tac_toe-program.adb");

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E100 : Short_Integer; pragma Import (Ada, E100, "system__os_lib_E");
   E014 : Short_Integer; pragma Import (Ada, E014, "system__soft_links_E");
   E189 : Short_Integer; pragma Import (Ada, E189, "system__fat_flt_E");
   E192 : Short_Integer; pragma Import (Ada, E192, "system__fat_llf_E");
   E024 : Short_Integer; pragma Import (Ada, E024, "system__exception_table_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "ada__containers_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "ada__io_exceptions_E");
   E337 : Short_Integer; pragma Import (Ada, E337, "ada__numerics_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "ada__strings_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "ada__strings__maps_E");
   E273 : Short_Integer; pragma Import (Ada, E273, "ada__strings__maps__constants_E");
   E140 : Short_Integer; pragma Import (Ada, E140, "ada__strings__utf_encoding_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "ada__tags_E");
   E086 : Short_Integer; pragma Import (Ada, E086, "ada__streams_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "interfaces__c_E");
   E296 : Short_Integer; pragma Import (Ada, E296, "interfaces__c__strings_E");
   E030 : Short_Integer; pragma Import (Ada, E030, "system__exceptions_E");
   E096 : Short_Integer; pragma Import (Ada, E096, "system__finalization_root_E");
   E094 : Short_Integer; pragma Import (Ada, E094, "ada__finalization_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "system__storage_pools_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "system__finalization_masters_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "system__storage_pools__subpools_E");
   E226 : Short_Integer; pragma Import (Ada, E226, "system__task_info_E");
   E061 : Short_Integer; pragma Import (Ada, E061, "ada__calendar_E");
   E261 : Short_Integer; pragma Import (Ada, E261, "ada__calendar__delays_E");
   E069 : Short_Integer; pragma Import (Ada, E069, "ada__calendar__time_zones_E");
   E324 : Short_Integer; pragma Import (Ada, E324, "gnat__secure_hashes_E");
   E326 : Short_Integer; pragma Import (Ada, E326, "gnat__secure_hashes__sha1_E");
   E322 : Short_Integer; pragma Import (Ada, E322, "gnat__sha1_E");
   E203 : Short_Integer; pragma Import (Ada, E203, "system__assertions_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "system__pool_global_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "system__file_control_block_E");
   E207 : Short_Integer; pragma Import (Ada, E207, "ada__streams__stream_io_E");
   E092 : Short_Integer; pragma Import (Ada, E092, "system__file_io_E");
   E286 : Short_Integer; pragma Import (Ada, E286, "gnat__sockets_E");
   E298 : Short_Integer; pragma Import (Ada, E298, "system__pool_size_E");
   E018 : Short_Integer; pragma Import (Ada, E018, "system__secondary_stack_E");
   E148 : Short_Integer; pragma Import (Ada, E148, "ada__strings__unbounded_E");
   E270 : Short_Integer; pragma Import (Ada, E270, "ada__directories_E");
   E294 : Short_Integer; pragma Import (Ada, E294, "gnat__sockets__thin_common_E");
   E289 : Short_Integer; pragma Import (Ada, E289, "gnat__sockets__thin_E");
   E278 : Short_Integer; pragma Import (Ada, E278, "system__regexp_E");
   E205 : Short_Integer; pragma Import (Ada, E205, "system__strings__stream_ops_E");
   E385 : Short_Integer; pragma Import (Ada, E385, "system__tasking__async_delays_E");
   E246 : Short_Integer; pragma Import (Ada, E246, "system__tasking__initialization_E");
   E232 : Short_Integer; pragma Import (Ada, E232, "system__tasking__protected_objects_E");
   E282 : Short_Integer; pragma Import (Ada, E282, "ada__real_time_E");
   E084 : Short_Integer; pragma Import (Ada, E084, "ada__text_io_E");
   E248 : Short_Integer; pragma Import (Ada, E248, "system__tasking__protected_objects__entries_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "system__tasking__queuing_E");
   E312 : Short_Integer; pragma Import (Ada, E312, "system__tasking__stages_E");
   E387 : Short_Integer; pragma Import (Ada, E387, "system__interrupt_management__operations_E");
   E302 : Short_Integer; pragma Import (Ada, E302, "generic_unbounded_array_E");
   E364 : Short_Integer; pragma Import (Ada, E364, "generic_unbounded_ptr_array_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "gnoga_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "gnoga__application_E");
   E266 : Short_Integer; pragma Import (Ada, E266, "gnoga__server_E");
   E371 : Short_Integer; pragma Import (Ada, E371, "gnoga__server__mime_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "gnoga__types_E");
   E389 : Short_Integer; pragma Import (Ada, E389, "gnoga__gui__base_E");
   E264 : Short_Integer; pragma Import (Ada, E264, "gnoga__client__storage_E");
   E409 : Short_Integer; pragma Import (Ada, E409, "gnoga__gui__location_E");
   E280 : Short_Integer; pragma Import (Ada, E280, "gnoga__server__connection_E");
   E375 : Short_Integer; pragma Import (Ada, E375, "gnoga__server__database_E");
   E377 : Short_Integer; pragma Import (Ada, E377, "gnoga__server__model_E");
   E379 : Short_Integer; pragma Import (Ada, E379, "gnoga__server__model__queries_E");
   E373 : Short_Integer; pragma Import (Ada, E373, "gnoga__server__template_parser_E");
   E381 : Short_Integer; pragma Import (Ada, E381, "gnoga__server__template_parser__simple_E");
   E397 : Short_Integer; pragma Import (Ada, E397, "gnoga__types__colors_E");
   E393 : Short_Integer; pragma Import (Ada, E393, "gnoga__gui__element_E");
   E401 : Short_Integer; pragma Import (Ada, E401, "gnoga__gui__document_E");
   E399 : Short_Integer; pragma Import (Ada, E399, "gnoga__gui__view_E");
   E403 : Short_Integer; pragma Import (Ada, E403, "gnoga__gui__element__common_E");
   E411 : Short_Integer; pragma Import (Ada, E411, "gnoga__gui__element__form_E");
   E413 : Short_Integer; pragma Import (Ada, E413, "gnoga__gui__view__grid_E");
   E259 : Short_Integer; pragma Import (Ada, E259, "gnoga__gui__window_E");
   E170 : Short_Integer; pragma Import (Ada, E170, "gnoga__application__multi_connect_E");
   E314 : Short_Integer; pragma Import (Ada, E314, "object_E");
   E316 : Short_Integer; pragma Import (Ada, E316, "object__handle_E");
   E318 : Short_Integer; pragma Import (Ada, E318, "object__handle__generic_unbounded_array_E");
   E362 : Short_Integer; pragma Import (Ada, E362, "stack_storage_E");
   E306 : Short_Integer; pragma Import (Ada, E306, "strings_edit_E");
   E334 : Short_Integer; pragma Import (Ada, E334, "strings_edit__base64_E");
   E348 : Short_Integer; pragma Import (Ada, E348, "strings_edit__fields_E");
   E344 : Short_Integer; pragma Import (Ada, E344, "strings_edit__float_edit_E");
   E310 : Short_Integer; pragma Import (Ada, E310, "strings_edit__integer_edit_E");
   E304 : Short_Integer; pragma Import (Ada, E304, "gnat__sockets__server_E");
   E300 : Short_Integer; pragma Import (Ada, E300, "gnat__sockets__connection_state_machine_E");
   E332 : Short_Integer; pragma Import (Ada, E332, "gnat__sockets__connection_state_machine__big_endian__unsigneds_E");
   E358 : Short_Integer; pragma Import (Ada, E358, "gnat__sockets__connection_state_machine__expected_sequence_E");
   E360 : Short_Integer; pragma Import (Ada, E360, "gnat__sockets__connection_state_machine__terminated_strings_E");
   E336 : Short_Integer; pragma Import (Ada, E336, "strings_edit__floats_E");
   E335 : Short_Integer; pragma Import (Ada, E335, "strings_edit__floats_E");
   E346 : Short_Integer; pragma Import (Ada, E346, "strings_edit__quoted_E");
   E383 : Short_Integer; pragma Import (Ada, E383, "strings_edit__utf8__handling_E");
   E366 : Short_Integer; pragma Import (Ada, E366, "tables_E");
   E368 : Short_Integer; pragma Import (Ada, E368, "tables__names_E");
   E320 : Short_Integer; pragma Import (Ada, E320, "gnat__sockets__connection_state_machine__http_server_E");
   E369 : Short_Integer; pragma Import (Ada, E369, "gnoga__server__connection__common_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "tic_tac_toe__ui_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "tic_tac_toe__ui__finalize_body");
      begin
         E006 := E006 - 1;
         F1;
      end;
      E259 := E259 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "gnoga__server__connection__finalize_body");
      begin
         E280 := E280 - 1;
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "gnoga__server__connection__common__finalize_spec");
      begin
         E369 := E369 - 1;
         F3;
      end;
      E320 := E320 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "gnat__sockets__connection_state_machine__http_server__finalize_spec");
      begin
         F4;
      end;
      E304 := E304 - 1;
      E360 := E360 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "gnat__sockets__connection_state_machine__terminated_strings__finalize_spec");
      begin
         F5;
      end;
      E358 := E358 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "gnat__sockets__connection_state_machine__expected_sequence__finalize_spec");
      begin
         F6;
      end;
      E332 := E332 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "gnat__sockets__connection_state_machine__big_endian__unsigneds__finalize_spec");
      begin
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gnat__sockets__connection_state_machine__finalize_body");
      begin
         E300 := E300 - 1;
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "gnat__sockets__connection_state_machine__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "gnat__sockets__server__finalize_spec");
      begin
         F10;
      end;
      E362 := E362 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "stack_storage__finalize_spec");
      begin
         F11;
      end;
      E314 := E314 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "object__finalize_spec");
      begin
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "gnoga__application__multi_connect__finalize_body");
      begin
         E170 := E170 - 1;
         F13;
      end;
      E413 := E413 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "gnoga__gui__window__finalize_spec");
      begin
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "gnoga__gui__view__grid__finalize_spec");
      begin
         F15;
      end;
      E411 := E411 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "gnoga__gui__element__form__finalize_spec");
      begin
         F16;
      end;
      E399 := E399 - 1;
      E403 := E403 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "gnoga__gui__element__common__finalize_spec");
      begin
         F17;
      end;
      declare
         procedure F18;
         pragma Import (Ada, F18, "gnoga__gui__view__finalize_spec");
      begin
         F18;
      end;
      E401 := E401 - 1;
      declare
         procedure F19;
         pragma Import (Ada, F19, "gnoga__gui__document__finalize_spec");
      begin
         F19;
      end;
      E393 := E393 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "gnoga__gui__element__finalize_spec");
      begin
         F20;
      end;
      declare
         procedure F21;
         pragma Import (Ada, F21, "gnoga__server__template_parser__finalize_body");
      begin
         E373 := E373 - 1;
         F21;
      end;
      declare
         procedure F22;
         pragma Import (Ada, F22, "gnoga__server__template_parser__finalize_spec");
      begin
         F22;
      end;
      E379 := E379 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "gnoga__server__model__queries__finalize_spec");
      begin
         F23;
      end;
      E377 := E377 - 1;
      declare
         procedure F24;
         pragma Import (Ada, F24, "gnoga__server__model__finalize_spec");
      begin
         F24;
      end;
      E375 := E375 - 1;
      declare
         procedure F25;
         pragma Import (Ada, F25, "gnoga__server__database__finalize_spec");
      begin
         F25;
      end;
      E389 := E389 - 1;
      E264 := E264 - 1;
      E409 := E409 - 1;
      declare
         procedure F26;
         pragma Import (Ada, F26, "gnoga__gui__location__finalize_spec");
      begin
         F26;
      end;
      declare
         procedure F27;
         pragma Import (Ada, F27, "gnoga__client__storage__finalize_spec");
      begin
         F27;
      end;
      declare
         procedure F28;
         pragma Import (Ada, F28, "gnoga__gui__base__finalize_spec");
      begin
         F28;
      end;
      E176 := E176 - 1;
      declare
         procedure F29;
         pragma Import (Ada, F29, "gnoga__types__finalize_spec");
      begin
         F29;
      end;
      declare
         procedure F30;
         pragma Import (Ada, F30, "gnoga__application__finalize_body");
      begin
         E166 := E166 - 1;
         F30;
      end;
      E248 := E248 - 1;
      declare
         procedure F31;
         pragma Import (Ada, F31, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F31;
      end;
      E084 := E084 - 1;
      declare
         procedure F32;
         pragma Import (Ada, F32, "ada__text_io__finalize_spec");
      begin
         F32;
      end;
      E270 := E270 - 1;
      E278 := E278 - 1;
      declare
         procedure F33;
         pragma Import (Ada, F33, "system__regexp__finalize_spec");
      begin
         F33;
      end;
      declare
         procedure F34;
         pragma Import (Ada, F34, "gnat__sockets__finalize_body");
      begin
         E286 := E286 - 1;
         F34;
      end;
      declare
         procedure F35;
         pragma Import (Ada, F35, "ada__directories__finalize_spec");
      begin
         F35;
      end;
      E148 := E148 - 1;
      declare
         procedure F36;
         pragma Import (Ada, F36, "ada__strings__unbounded__finalize_spec");
      begin
         F36;
      end;
      E105 := E105 - 1;
      E119 := E119 - 1;
      declare
         procedure F37;
         pragma Import (Ada, F37, "system__file_io__finalize_body");
      begin
         E092 := E092 - 1;
         F37;
      end;
      E298 := E298 - 1;
      declare
         procedure F38;
         pragma Import (Ada, F38, "system__pool_size__finalize_spec");
      begin
         F38;
      end;
      declare
         procedure F39;
         pragma Import (Ada, F39, "gnat__sockets__finalize_spec");
      begin
         F39;
      end;
      E207 := E207 - 1;
      declare
         procedure F40;
         pragma Import (Ada, F40, "ada__streams__stream_io__finalize_spec");
      begin
         F40;
      end;
      declare
         procedure F41;
         pragma Import (Ada, F41, "system__file_control_block__finalize_spec");
      begin
         E103 := E103 - 1;
         F41;
      end;
      E115 := E115 - 1;
      declare
         procedure F42;
         pragma Import (Ada, F42, "system__pool_global__finalize_spec");
      begin
         F42;
      end;
      declare
         procedure F43;
         pragma Import (Ada, F43, "system__storage_pools__subpools__finalize_spec");
      begin
         F43;
      end;
      declare
         procedure F44;
         pragma Import (Ada, F44, "system__finalization_masters__finalize_spec");
      begin
         F44;
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
   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
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
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");

      procedure Install_Handler;
      pragma Import (C, Install_Handler, "__gnat_install_handler");

      Handler_Installed : Integer;
      pragma Import (C, Handler_Installed, "__gnat_handler_installed");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, True, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (True, False, False, True, True, True, False, True, 
           False, False, True, True, True, True, False, False, 
           True, False, False, True, True, False, True, True, 
           True, True, True, True, True, True, True, False, 
           True, False, True, True, False, True, True, False, 
           True, False, True, True, False, False, True, False, 
           True, True, False, False, False, True, False, True, 
           True, True, False, False, True, False, False, True, 
           False, True, True, False, True, True, True, False, 
           True, False, False, False, True, True, True, True, 
           True, True, False),
         Count => (0, 0, 0, 1, 2, 2, 3, 1, 5, 0),
         Unknown => (False, False, False, False, False, False, True, True, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      if Handler_Installed = 0 then
         Install_Handler;
      end if;

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Fat_Flt'Elab_Spec;
      E189 := E189 + 1;
      System.Fat_Llf'Elab_Spec;
      E192 := E192 + 1;
      System.Exception_Table'Elab_Body;
      E024 := E024 + 1;
      Ada.Containers'Elab_Spec;
      E171 := E171 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E087 := E087 + 1;
      Ada.Numerics'Elab_Spec;
      E337 := E337 + 1;
      Ada.Strings'Elab_Spec;
      E138 := E138 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E273 := E273 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E086 := E086 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E030 := E030 + 1;
      System.Finalization_Root'Elab_Spec;
      E096 := E096 + 1;
      Ada.Finalization'Elab_Spec;
      E094 := E094 + 1;
      System.Storage_Pools'Elab_Spec;
      E113 := E113 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      System.Task_Info'Elab_Spec;
      E226 := E226 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E061 := E061 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E261 := E261 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E069 := E069 + 1;
      E324 := E324 + 1;
      Gnat.Secure_Hashes.Sha1'Elab_Spec;
      E326 := E326 + 1;
      Gnat.Sha1'Elab_Spec;
      E322 := E322 + 1;
      System.Assertions'Elab_Spec;
      E203 := E203 + 1;
      System.Pool_Global'Elab_Spec;
      E115 := E115 + 1;
      System.File_Control_Block'Elab_Spec;
      E103 := E103 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E207 := E207 + 1;
      Gnat.Sockets'Elab_Spec;
      System.Pool_Size'Elab_Spec;
      E298 := E298 + 1;
      System.File_Io'Elab_Body;
      E092 := E092 + 1;
      E119 := E119 + 1;
      System.Finalization_Masters'Elab_Body;
      E105 := E105 + 1;
      E296 := E296 + 1;
      E063 := E063 + 1;
      Ada.Tags'Elab_Body;
      E050 := E050 + 1;
      E140 := E140 + 1;
      E152 := E152 + 1;
      System.Soft_Links'Elab_Body;
      E014 := E014 + 1;
      System.Os_Lib'Elab_Body;
      E100 := E100 + 1;
      System.Secondary_Stack'Elab_Body;
      E018 := E018 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E148 := E148 + 1;
      Ada.Directories'Elab_Spec;
      Gnat.Sockets.Thin_Common'Elab_Spec;
      E294 := E294 + 1;
      E289 := E289 + 1;
      Gnat.Sockets'Elab_Body;
      E286 := E286 + 1;
      System.Regexp'Elab_Spec;
      E278 := E278 + 1;
      Ada.Directories'Elab_Body;
      E270 := E270 + 1;
      System.Strings.Stream_Ops'Elab_Body;
      E205 := E205 + 1;
      System.Tasking.Initialization'Elab_Body;
      E246 := E246 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E232 := E232 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E282 := E282 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E084 := E084 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E248 := E248 + 1;
      System.Tasking.Queuing'Elab_Body;
      E250 := E250 + 1;
      System.Tasking.Stages'Elab_Body;
      E312 := E312 + 1;
      System.Interrupt_Management.Operations'Elab_Body;
      E387 := E387 + 1;
      System.Tasking.Async_Delays'Elab_Body;
      E385 := E385 + 1;
      E302 := E302 + 1;
      E364 := E364 + 1;
      E059 := E059 + 1;
      Gnoga.Application'Elab_Body;
      E166 := E166 + 1;
      Gnoga.Server'Elab_Body;
      E266 := E266 + 1;
      E371 := E371 + 1;
      Gnoga.Types'Elab_Spec;
      E176 := E176 + 1;
      Gnoga.Gui.Base'Elab_Spec;
      Gnoga.Client.Storage'Elab_Spec;
      Gnoga.Gui.Location'Elab_Spec;
      E409 := E409 + 1;
      Gnoga.Server.Connection'Elab_Spec;
      E264 := E264 + 1;
      E389 := E389 + 1;
      Gnoga.Server.Database'Elab_Spec;
      E375 := E375 + 1;
      Gnoga.Server.Model'Elab_Spec;
      E377 := E377 + 1;
      Gnoga.Server.Model.Queries'Elab_Spec;
      E379 := E379 + 1;
      Gnoga.Server.Template_Parser'Elab_Spec;
      Gnoga.Server.Template_Parser'Elab_Body;
      E373 := E373 + 1;
      E381 := E381 + 1;
      Gnoga.Types.Colors'Elab_Spec;
      E397 := E397 + 1;
      Gnoga.Gui.Element'Elab_Spec;
      E393 := E393 + 1;
      Gnoga.Gui.Document'Elab_Spec;
      E401 := E401 + 1;
      Gnoga.Gui.View'Elab_Spec;
      Gnoga.Gui.Element.Common'Elab_Spec;
      E403 := E403 + 1;
      E399 := E399 + 1;
      Gnoga.Gui.Element.Form'Elab_Spec;
      E411 := E411 + 1;
      Gnoga.Gui.View.Grid'Elab_Spec;
      Gnoga.Gui.Window'Elab_Spec;
      E413 := E413 + 1;
      Gnoga.Application.Multi_Connect'Elab_Body;
      E170 := E170 + 1;
      Object'Elab_Spec;
      Object'Elab_Body;
      E314 := E314 + 1;
      E316 := E316 + 1;
      E318 := E318 + 1;
      Stack_Storage'Elab_Spec;
      E362 := E362 + 1;
      Strings_Edit'Elab_Spec;
      E306 := E306 + 1;
      E334 := E334 + 1;
      E348 := E348 + 1;
      E310 := E310 + 1;
      GNAT.SOCKETS.SERVER'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE'ELAB_BODY;
      E300 := E300 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.BIG_ENDIAN.UNSIGNEDS'ELAB_SPEC;
      E332 := E332 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.EXPECTED_SEQUENCE'ELAB_SPEC;
      E358 := E358 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.TERMINATED_STRINGS'ELAB_SPEC;
      E360 := E360 + 1;
      GNAT.SOCKETS.SERVER'ELAB_BODY;
      E304 := E304 + 1;
      E344 := E344 + 1;
      Strings_Edit.Floats'Elab_Body;
      E336 := E336 + 1;
      E346 := E346 + 1;
      E383 := E383 + 1;
      E366 := E366 + 1;
      E368 := E368 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.HTTP_SERVER'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.HTTP_SERVER'ELAB_BODY;
      E320 := E320 + 1;
      Gnoga.Server.Connection.Common'Elab_Spec;
      E369 := E369 + 1;
      Gnoga.Server.Connection'Elab_Body;
      E280 := E280 + 1;
      E259 := E259 + 1;
      Tic_Tac_Toe.Ui'Elab_Body;
      E006 := E006 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_tic_tac_toe__program");

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
   --   /home/jrcarter/Code/gnoga/deps/simple_components/generic_set.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/generic_discrete_set.o
   --   /home/jrcarter/Code/Tic_Tac_Toe/tic_tac_toe.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/generic_unbounded_array.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/generic_unbounded_ptr_array.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/object.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/object-handle.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/object-handle-generic_unbounded_array.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/stack_storage.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/strings_edit.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/strings_edit-base64.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/strings_edit-fields.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/strings_edit-integer_edit.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/gnat-sockets-connection_state_machine.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/gnat-sockets-connection_state_machine-big_endian.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/gnat-sockets-connection_state_machine-big_endian-unsigneds.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/gnat-sockets-connection_state_machine-expected_sequence.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/strings_edit-integers.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/gnat-sockets-connection_state_machine-terminated_strings.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/gnat-sockets-server.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/strings_edit-float_edit.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/strings_edit-floats.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/strings_edit-quoted.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/strings_edit-utf8.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/strings_edit-utf8-handling.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/tables.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/tables-names.o
   --   /home/jrcarter/Code/gnoga/deps/simple_components/gnat-sockets-connection_state_machine-http_server.o
   --   /home/jrcarter/Code/Tic_Tac_Toe/tic_tac_toe-ui.o
   --   /home/jrcarter/Code/Tic_Tac_Toe/tic_tac_toe-program.o
   --   -L/home/jrcarter/Code/Tic_Tac_Toe/
   --   -L/home/jrcarter/Code/Tic_Tac_Toe/
   --   -L/home/jrcarter/Code/gnoga/lib/
   --   -L/home/jrcarter/Code/gnoga/
   --   -L/home/jrcarter/Code/gnoga/deps/simple_components/
   --   -L/home/jrcarter/Code/gnoga/deps/simple_components/xpm/
   --   -L/usr/lib/gcc/x86_64-linux-gnu/4.9/adalib/
   --   -shared
   --   -lgnarl-4.9
   --   -lgnat-4.9
   --   -lpthread
--  END Object file/option list   

end ada_main;
