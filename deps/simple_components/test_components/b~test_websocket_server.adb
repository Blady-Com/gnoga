pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b~test_websocket_server.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b~test_websocket_server.adb");

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E082 : Short_Integer; pragma Import (Ada, E082, "system__os_lib_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E253 : Short_Integer; pragma Import (Ada, E253, "system__fat_flt_E");
   E249 : Short_Integer; pragma Import (Ada, E249, "system__fat_llf_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exception_table_E");
   E069 : Short_Integer; pragma Import (Ada, E069, "ada__io_exceptions_E");
   E244 : Short_Integer; pragma Import (Ada, E244, "ada__numerics_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "ada__strings_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "ada__strings__maps_E");
   E227 : Short_Integer; pragma Import (Ada, E227, "ada__strings__maps__constants_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "ada__tags_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__streams_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "interfaces__c_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "interfaces__c__strings_E");
   E031 : Short_Integer; pragma Import (Ada, E031, "system__exceptions_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "system__finalization_root_E");
   E076 : Short_Integer; pragma Import (Ada, E076, "ada__finalization_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "system__storage_pools_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "system__finalization_masters_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "system__storage_pools__subpools_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__calendar_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__calendar__delays_E");
   E267 : Short_Integer; pragma Import (Ada, E267, "ada__calendar__time_zones_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "gnat__secure_hashes_E");
   E233 : Short_Integer; pragma Import (Ada, E233, "gnat__secure_hashes__sha1_E");
   E229 : Short_Integer; pragma Import (Ada, E229, "gnat__sha1_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "system__pool_global_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "system__file_control_block_E");
   E286 : Short_Integer; pragma Import (Ada, E286, "ada__streams__stream_io_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "system__file_io_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "gnat__sockets_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "system__pool_size_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__secondary_stack_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "gnat__sockets__thin_common_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "gnat__sockets__thin_E");
   E284 : Short_Integer; pragma Import (Ada, E284, "system__strings__stream_ops_E");
   E178 : Short_Integer; pragma Import (Ada, E178, "system__tasking__initialization_E");
   E186 : Short_Integer; pragma Import (Ada, E186, "system__tasking__protected_objects_E");
   E200 : Short_Integer; pragma Import (Ada, E200, "ada__real_time_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "ada__text_io_E");
   E188 : Short_Integer; pragma Import (Ada, E188, "system__tasking__protected_objects__entries_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "system__tasking__queuing_E");
   E174 : Short_Integer; pragma Import (Ada, E174, "system__tasking__stages_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "generic_unbounded_array_E");
   E290 : Short_Integer; pragma Import (Ada, E290, "generic_unbounded_ptr_array_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "object_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "object__handle_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "object__handle__generic_unbounded_array_E");
   E288 : Short_Integer; pragma Import (Ada, E288, "stack_storage_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "strings_edit_E");
   E241 : Short_Integer; pragma Import (Ada, E241, "strings_edit__base64_E");
   E220 : Short_Integer; pragma Import (Ada, E220, "strings_edit__fields_E");
   E255 : Short_Integer; pragma Import (Ada, E255, "strings_edit__float_edit_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "strings_edit__integer_edit_E");
   E128 : Short_Integer; pragma Import (Ada, E128, "gnat__sockets__server_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "gnat__sockets__connection_state_machine_E");
   E239 : Short_Integer; pragma Import (Ada, E239, "gnat__sockets__connection_state_machine__big_endian__unsigneds_E");
   E280 : Short_Integer; pragma Import (Ada, E280, "gnat__sockets__connection_state_machine__expected_sequence_E");
   E282 : Short_Integer; pragma Import (Ada, E282, "gnat__sockets__connection_state_machine__terminated_strings_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "gnat__sockets__server__pooled_E");
   E243 : Short_Integer; pragma Import (Ada, E243, "strings_edit__floats_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "strings_edit__floats_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "strings_edit__quoted_E");
   E292 : Short_Integer; pragma Import (Ada, E292, "tables_E");
   E294 : Short_Integer; pragma Import (Ada, E294, "tables__names_E");
   E224 : Short_Integer; pragma Import (Ada, E224, "gnat__sockets__connection_state_machine__http_server_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "test_websocket_servers_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E216 := E216 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "test_websocket_servers__finalize_spec");
      begin
         F1;
      end;
      E224 := E224 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "gnat__sockets__connection_state_machine__http_server__finalize_spec");
      begin
         F2;
      end;
      E128 := E128 - 1;
      E282 := E282 - 1;
      E214 := E214 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "gnat__sockets__server__pooled__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "gnat__sockets__connection_state_machine__terminated_strings__finalize_spec");
      begin
         F4;
      end;
      E280 := E280 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "gnat__sockets__connection_state_machine__expected_sequence__finalize_spec");
      begin
         F5;
      end;
      E239 := E239 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "gnat__sockets__connection_state_machine__big_endian__unsigneds__finalize_spec");
      begin
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "gnat__sockets__connection_state_machine__finalize_body");
      begin
         E222 := E222 - 1;
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gnat__sockets__connection_state_machine__finalize_spec");
      begin
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "gnat__sockets__server__finalize_spec");
      begin
         F9;
      end;
      E288 := E288 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "stack_storage__finalize_spec");
      begin
         F10;
      end;
      E206 := E206 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "object__finalize_spec");
      begin
         F11;
      end;
      E188 := E188 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F12;
      end;
      E066 := E066 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "ada__text_io__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "gnat__sockets__finalize_body");
      begin
         E106 := E106 - 1;
         F14;
      end;
      E087 := E087 - 1;
      E101 := E101 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "system__file_io__finalize_body");
      begin
         E074 := E074 - 1;
         F15;
      end;
      E121 := E121 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "system__pool_size__finalize_spec");
      begin
         F16;
      end;
      declare
         procedure F17;
         pragma Import (Ada, F17, "gnat__sockets__finalize_spec");
      begin
         F17;
      end;
      E286 := E286 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "ada__streams__stream_io__finalize_spec");
      begin
         F18;
      end;
      declare
         procedure F19;
         pragma Import (Ada, F19, "system__file_control_block__finalize_spec");
      begin
         E085 := E085 - 1;
         F19;
      end;
      E097 := E097 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "system__pool_global__finalize_spec");
      begin
         F20;
      end;
      declare
         procedure F21;
         pragma Import (Ada, F21, "system__storage_pools__subpools__finalize_spec");
      begin
         F21;
      end;
      declare
         procedure F22;
         pragma Import (Ada, F22, "system__finalization_masters__finalize_spec");
      begin
         F22;
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
          (True, False, False, True, True, False, False, True, 
           False, False, True, True, True, True, False, False, 
           True, False, False, True, True, False, True, True, 
           True, True, True, True, False, True, True, False, 
           True, False, False, True, False, True, True, False, 
           True, False, True, False, False, False, True, False, 
           True, True, False, False, False, True, False, True, 
           True, True, False, False, True, False, False, True, 
           False, True, True, False, True, True, True, False, 
           True, False, False, False, True, False, False, True, 
           False, True, False),
         Count => (0, 0, 0, 2, 0, 0, 2, 0, 3, 0),
         Unknown => (False, False, False, False, False, False, True, False, True, False));
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
      E253 := E253 + 1;
      System.Fat_Llf'Elab_Spec;
      E249 := E249 + 1;
      System.Exception_Table'Elab_Body;
      E025 := E025 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E069 := E069 + 1;
      Ada.Numerics'Elab_Spec;
      E244 := E244 + 1;
      Ada.Strings'Elab_Spec;
      E133 := E133 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E227 := E227 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E068 := E068 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E031 := E031 + 1;
      System.Finalization_Root'Elab_Spec;
      E078 := E078 + 1;
      Ada.Finalization'Elab_Spec;
      E076 := E076 + 1;
      System.Storage_Pools'Elab_Spec;
      E095 := E095 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E008 := E008 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E006 := E006 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E267 := E267 + 1;
      E231 := E231 + 1;
      Gnat.Secure_Hashes.Sha1'Elab_Spec;
      E233 := E233 + 1;
      Gnat.Sha1'Elab_Spec;
      E229 := E229 + 1;
      System.Pool_Global'Elab_Spec;
      E097 := E097 + 1;
      System.File_Control_Block'Elab_Spec;
      E085 := E085 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E286 := E286 + 1;
      Gnat.Sockets'Elab_Spec;
      System.Pool_Size'Elab_Spec;
      E121 := E121 + 1;
      System.File_Io'Elab_Body;
      E074 := E074 + 1;
      E101 := E101 + 1;
      System.Finalization_Masters'Elab_Body;
      E087 := E087 + 1;
      E117 := E117 + 1;
      E051 := E051 + 1;
      Ada.Tags'Elab_Body;
      E057 := E057 + 1;
      E135 := E135 + 1;
      System.Soft_Links'Elab_Body;
      E015 := E015 + 1;
      System.Os_Lib'Elab_Body;
      E082 := E082 + 1;
      System.Secondary_Stack'Elab_Body;
      E019 := E019 + 1;
      Gnat.Sockets.Thin_Common'Elab_Spec;
      E115 := E115 + 1;
      E109 := E109 + 1;
      Gnat.Sockets'Elab_Body;
      E106 := E106 + 1;
      System.Strings.Stream_Ops'Elab_Body;
      E284 := E284 + 1;
      System.Tasking.Initialization'Elab_Body;
      E178 := E178 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E186 := E186 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E200 := E200 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E066 := E066 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E188 := E188 + 1;
      System.Tasking.Queuing'Elab_Body;
      E184 := E184 + 1;
      System.Tasking.Stages'Elab_Body;
      E174 := E174 + 1;
      E204 := E204 + 1;
      E290 := E290 + 1;
      Object'Elab_Spec;
      Object'Elab_Body;
      E206 := E206 + 1;
      E208 := E208 + 1;
      E210 := E210 + 1;
      Stack_Storage'Elab_Spec;
      E288 := E288 + 1;
      Strings_Edit'Elab_Spec;
      E130 := E130 + 1;
      E241 := E241 + 1;
      E220 := E220 + 1;
      E141 := E141 + 1;
      GNAT.SOCKETS.SERVER'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE'ELAB_BODY;
      E222 := E222 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.BIG_ENDIAN.UNSIGNEDS'ELAB_SPEC;
      E239 := E239 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.EXPECTED_SEQUENCE'ELAB_SPEC;
      E280 := E280 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.TERMINATED_STRINGS'ELAB_SPEC;
      GNAT.SOCKETS.SERVER.POOLED'ELAB_SPEC;
      GNAT.SOCKETS.SERVER.POOLED'ELAB_BODY;
      E214 := E214 + 1;
      E282 := E282 + 1;
      GNAT.SOCKETS.SERVER'ELAB_BODY;
      E128 := E128 + 1;
      E255 := E255 + 1;
      Strings_Edit.Floats'Elab_Body;
      E243 := E243 + 1;
      E218 := E218 + 1;
      E292 := E292 + 1;
      E294 := E294 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.HTTP_SERVER'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.HTTP_SERVER'ELAB_BODY;
      E224 := E224 + 1;
      Test_Websocket_Servers'Elab_Spec;
      E216 := E216 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_test_websocket_server");

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
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/generic_set.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/generic_discrete_set.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/generic_unbounded_array.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/generic_unbounded_ptr_array.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/object.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/object-handle.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/object-handle-generic_unbounded_array.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/stack_storage.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/strings_edit.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/strings_edit-base64.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/strings_edit-fields.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/strings_edit-integer_edit.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/gnat-sockets-connection_state_machine.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/gnat-sockets-connection_state_machine-big_endian.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/gnat-sockets-connection_state_machine-big_endian-unsigneds.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/gnat-sockets-connection_state_machine-expected_sequence.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/gnat-sockets-server-pooled.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/strings_edit-integers.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/gnat-sockets-connection_state_machine-terminated_strings.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/gnat-sockets-server.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/strings_edit-float_edit.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/strings_edit-floats.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/strings_edit-quoted.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/strings_edit-utf8.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/tables.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/tables-names.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/gnat-sockets-connection_state_machine-http_server.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/test_components/test_websocket_servers.o
   --   /Users/dbotton/Projects/gnoga/deps/simple_components/test_components/test_websocket_server.o
   --   -L/Users/dbotton/Projects/gnoga/deps/simple_components/test_components/
   --   -L/Users/dbotton/Projects/gnoga/deps/simple_components/test_components/
   --   -L/Users/dbotton/Projects/gnoga/deps/simple_components/
   --   -L/opt/gcc-4.9.1/lib/gcc/x86_64-apple-darwin13/4.9.1/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
--  END Object file/option list   

end ada_main;
