pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: 4.9.1" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_test_http_server" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#1a525c8b#;
   pragma Export (C, u00001, "test_http_serverB");
   u00002 : constant Version_32 := 16#a964624f#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#a3884fb0#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#d6a2decd#;
   pragma Export (C, u00005, "ada__calendar__delaysB");
   u00006 : constant Version_32 := 16#474dd4b1#;
   pragma Export (C, u00006, "ada__calendar__delaysS");
   u00007 : constant Version_32 := 16#65712768#;
   pragma Export (C, u00007, "ada__calendarB");
   u00008 : constant Version_32 := 16#e791e294#;
   pragma Export (C, u00008, "ada__calendarS");
   u00009 : constant Version_32 := 16#8b8661e1#;
   pragma Export (C, u00009, "ada__exceptionsB");
   u00010 : constant Version_32 := 16#8368db55#;
   pragma Export (C, u00010, "ada__exceptionsS");
   u00011 : constant Version_32 := 16#51ba2b93#;
   pragma Export (C, u00011, "ada__exceptions__last_chance_handlerB");
   u00012 : constant Version_32 := 16#909606f8#;
   pragma Export (C, u00012, "ada__exceptions__last_chance_handlerS");
   u00013 : constant Version_32 := 16#f2f2d889#;
   pragma Export (C, u00013, "systemS");
   u00014 : constant Version_32 := 16#77a147eb#;
   pragma Export (C, u00014, "system__soft_linksB");
   u00015 : constant Version_32 := 16#03928334#;
   pragma Export (C, u00015, "system__soft_linksS");
   u00016 : constant Version_32 := 16#65bb1402#;
   pragma Export (C, u00016, "system__parametersB");
   u00017 : constant Version_32 := 16#597e6ce3#;
   pragma Export (C, u00017, "system__parametersS");
   u00018 : constant Version_32 := 16#643ddf46#;
   pragma Export (C, u00018, "system__secondary_stackB");
   u00019 : constant Version_32 := 16#599317e0#;
   pragma Export (C, u00019, "system__secondary_stackS");
   u00020 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00020, "system__storage_elementsB");
   u00021 : constant Version_32 := 16#df31928d#;
   pragma Export (C, u00021, "system__storage_elementsS");
   u00022 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00022, "system__stack_checkingB");
   u00023 : constant Version_32 := 16#7c4db361#;
   pragma Export (C, u00023, "system__stack_checkingS");
   u00024 : constant Version_32 := 16#7ff7f3a3#;
   pragma Export (C, u00024, "system__exception_tableB");
   u00025 : constant Version_32 := 16#0e7090b4#;
   pragma Export (C, u00025, "system__exception_tableS");
   u00026 : constant Version_32 := 16#c3335bfd#;
   pragma Export (C, u00026, "system__htableB");
   u00027 : constant Version_32 := 16#76306b63#;
   pragma Export (C, u00027, "system__htableS");
   u00028 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00028, "system__string_hashB");
   u00029 : constant Version_32 := 16#d46e001d#;
   pragma Export (C, u00029, "system__string_hashS");
   u00030 : constant Version_32 := 16#9e373b74#;
   pragma Export (C, u00030, "system__exceptionsB");
   u00031 : constant Version_32 := 16#caec7e2b#;
   pragma Export (C, u00031, "system__exceptionsS");
   u00032 : constant Version_32 := 16#cd9019cf#;
   pragma Export (C, u00032, "system__exceptions__machineS");
   u00033 : constant Version_32 := 16#a2eb6533#;
   pragma Export (C, u00033, "system__exceptions_debugB");
   u00034 : constant Version_32 := 16#09f5c931#;
   pragma Export (C, u00034, "system__exceptions_debugS");
   u00035 : constant Version_32 := 16#570325c8#;
   pragma Export (C, u00035, "system__img_intB");
   u00036 : constant Version_32 := 16#f029384b#;
   pragma Export (C, u00036, "system__img_intS");
   u00037 : constant Version_32 := 16#ff5c7695#;
   pragma Export (C, u00037, "system__tracebackB");
   u00038 : constant Version_32 := 16#daf647d4#;
   pragma Export (C, u00038, "system__tracebackS");
   u00039 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00039, "system__wch_conB");
   u00040 : constant Version_32 := 16#e98ffa5b#;
   pragma Export (C, u00040, "system__wch_conS");
   u00041 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00041, "system__wch_stwB");
   u00042 : constant Version_32 := 16#c49ed65a#;
   pragma Export (C, u00042, "system__wch_stwS");
   u00043 : constant Version_32 := 16#9b29844d#;
   pragma Export (C, u00043, "system__wch_cnvB");
   u00044 : constant Version_32 := 16#e63840a8#;
   pragma Export (C, u00044, "system__wch_cnvS");
   u00045 : constant Version_32 := 16#69adb1b9#;
   pragma Export (C, u00045, "interfacesS");
   u00046 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00046, "system__wch_jisB");
   u00047 : constant Version_32 := 16#66485989#;
   pragma Export (C, u00047, "system__wch_jisS");
   u00048 : constant Version_32 := 16#8cb17bcd#;
   pragma Export (C, u00048, "system__traceback_entriesB");
   u00049 : constant Version_32 := 16#47e3b81b#;
   pragma Export (C, u00049, "system__traceback_entriesS");
   u00050 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00050, "interfaces__cB");
   u00051 : constant Version_32 := 16#96001448#;
   pragma Export (C, u00051, "interfaces__cS");
   u00052 : constant Version_32 := 16#820eb304#;
   pragma Export (C, u00052, "system__os_primitivesB");
   u00053 : constant Version_32 := 16#422354a0#;
   pragma Export (C, u00053, "system__os_primitivesS");
   u00054 : constant Version_32 := 16#ee80728a#;
   pragma Export (C, u00054, "system__tracesB");
   u00055 : constant Version_32 := 16#00efb023#;
   pragma Export (C, u00055, "system__tracesS");
   u00056 : constant Version_32 := 16#08ba48f3#;
   pragma Export (C, u00056, "ada__tagsB");
   u00057 : constant Version_32 := 16#8cc81545#;
   pragma Export (C, u00057, "ada__tagsS");
   u00058 : constant Version_32 := 16#a3f44a26#;
   pragma Export (C, u00058, "system__unsigned_typesS");
   u00059 : constant Version_32 := 16#1e25d3f1#;
   pragma Export (C, u00059, "system__val_lluB");
   u00060 : constant Version_32 := 16#d9061d54#;
   pragma Export (C, u00060, "system__val_lluS");
   u00061 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00061, "system__val_utilB");
   u00062 : constant Version_32 := 16#5e526e77#;
   pragma Export (C, u00062, "system__val_utilS");
   u00063 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00063, "system__case_utilB");
   u00064 : constant Version_32 := 16#d6fbb15e#;
   pragma Export (C, u00064, "system__case_utilS");
   u00065 : constant Version_32 := 16#9a3f0a9b#;
   pragma Export (C, u00065, "ada__text_ioB");
   u00066 : constant Version_32 := 16#4581c4f0#;
   pragma Export (C, u00066, "ada__text_ioS");
   u00067 : constant Version_32 := 16#1b5643e2#;
   pragma Export (C, u00067, "ada__streamsB");
   u00068 : constant Version_32 := 16#2564c958#;
   pragma Export (C, u00068, "ada__streamsS");
   u00069 : constant Version_32 := 16#db5c917c#;
   pragma Export (C, u00069, "ada__io_exceptionsS");
   u00070 : constant Version_32 := 16#e0b7a7e8#;
   pragma Export (C, u00070, "interfaces__c_streamsB");
   u00071 : constant Version_32 := 16#6c8b8ac5#;
   pragma Export (C, u00071, "interfaces__c_streamsS");
   u00072 : constant Version_32 := 16#dd7004f4#;
   pragma Export (C, u00072, "system__crtlS");
   u00073 : constant Version_32 := 16#ec6e1273#;
   pragma Export (C, u00073, "system__file_ioB");
   u00074 : constant Version_32 := 16#63b3b9ae#;
   pragma Export (C, u00074, "system__file_ioS");
   u00075 : constant Version_32 := 16#8cbe6205#;
   pragma Export (C, u00075, "ada__finalizationB");
   u00076 : constant Version_32 := 16#22e22193#;
   pragma Export (C, u00076, "ada__finalizationS");
   u00077 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00077, "system__finalization_rootB");
   u00078 : constant Version_32 := 16#bd00ab19#;
   pragma Export (C, u00078, "system__finalization_rootS");
   u00079 : constant Version_32 := 16#d0432c8d#;
   pragma Export (C, u00079, "system__img_enum_newB");
   u00080 : constant Version_32 := 16#93bede49#;
   pragma Export (C, u00080, "system__img_enum_newS");
   u00081 : constant Version_32 := 16#8c1eb183#;
   pragma Export (C, u00081, "system__os_libB");
   u00082 : constant Version_32 := 16#fc501b71#;
   pragma Export (C, u00082, "system__os_libS");
   u00083 : constant Version_32 := 16#1a817b8e#;
   pragma Export (C, u00083, "system__stringsB");
   u00084 : constant Version_32 := 16#8c4dc9ef#;
   pragma Export (C, u00084, "system__stringsS");
   u00085 : constant Version_32 := 16#b8ebb9f6#;
   pragma Export (C, u00085, "system__file_control_blockS");
   u00086 : constant Version_32 := 16#1f8826cb#;
   pragma Export (C, u00086, "system__finalization_mastersB");
   u00087 : constant Version_32 := 16#bdf1b490#;
   pragma Export (C, u00087, "system__finalization_mastersS");
   u00088 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00088, "system__address_imageB");
   u00089 : constant Version_32 := 16#531e45b3#;
   pragma Export (C, u00089, "system__address_imageS");
   u00090 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00090, "system__img_boolB");
   u00091 : constant Version_32 := 16#072ba962#;
   pragma Export (C, u00091, "system__img_boolS");
   u00092 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00092, "system__ioB");
   u00093 : constant Version_32 := 16#6cb02fc6#;
   pragma Export (C, u00093, "system__ioS");
   u00094 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00094, "system__storage_poolsB");
   u00095 : constant Version_32 := 16#3cbc1a54#;
   pragma Export (C, u00095, "system__storage_poolsS");
   u00096 : constant Version_32 := 16#e34550ca#;
   pragma Export (C, u00096, "system__pool_globalB");
   u00097 : constant Version_32 := 16#c88d2d16#;
   pragma Export (C, u00097, "system__pool_globalS");
   u00098 : constant Version_32 := 16#6810466c#;
   pragma Export (C, u00098, "system__memoryB");
   u00099 : constant Version_32 := 16#ab8fbebd#;
   pragma Export (C, u00099, "system__memoryS");
   u00100 : constant Version_32 := 16#1220f12d#;
   pragma Export (C, u00100, "system__storage_pools__subpoolsB");
   u00101 : constant Version_32 := 16#b0e8cddc#;
   pragma Export (C, u00101, "system__storage_pools__subpoolsS");
   u00102 : constant Version_32 := 16#aba9f469#;
   pragma Export (C, u00102, "system__storage_pools__subpools__finalizationB");
   u00103 : constant Version_32 := 16#9662cb63#;
   pragma Export (C, u00103, "system__storage_pools__subpools__finalizationS");
   u00104 : constant Version_32 := 16#fd2ad2f1#;
   pragma Export (C, u00104, "gnatS");
   u00105 : constant Version_32 := 16#95cd6a48#;
   pragma Export (C, u00105, "gnat__socketsB");
   u00106 : constant Version_32 := 16#8801c4b2#;
   pragma Export (C, u00106, "gnat__socketsS");
   u00107 : constant Version_32 := 16#ef852e11#;
   pragma Export (C, u00107, "gnat__sockets__linker_optionsS");
   u00108 : constant Version_32 := 16#3c2ab1da#;
   pragma Export (C, u00108, "gnat__sockets__thinB");
   u00109 : constant Version_32 := 16#dbd7b0cd#;
   pragma Export (C, u00109, "gnat__sockets__thinS");
   u00110 : constant Version_32 := 16#8328c314#;
   pragma Export (C, u00110, "gnat__os_libS");
   u00111 : constant Version_32 := 16#00e9dcb1#;
   pragma Export (C, u00111, "gnat__task_lockS");
   u00112 : constant Version_32 := 16#0881bbf8#;
   pragma Export (C, u00112, "system__task_lockB");
   u00113 : constant Version_32 := 16#9378efe7#;
   pragma Export (C, u00113, "system__task_lockS");
   u00114 : constant Version_32 := 16#0a2632e6#;
   pragma Export (C, u00114, "gnat__sockets__thin_commonB");
   u00115 : constant Version_32 := 16#a458fed4#;
   pragma Export (C, u00115, "gnat__sockets__thin_commonS");
   u00116 : constant Version_32 := 16#2a41728f#;
   pragma Export (C, u00116, "interfaces__c__stringsB");
   u00117 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00117, "interfaces__c__stringsS");
   u00118 : constant Version_32 := 16#5de653db#;
   pragma Export (C, u00118, "system__communicationB");
   u00119 : constant Version_32 := 16#6a24f6ce#;
   pragma Export (C, u00119, "system__communicationS");
   u00120 : constant Version_32 := 16#994daa60#;
   pragma Export (C, u00120, "system__pool_sizeB");
   u00121 : constant Version_32 := 16#f3dc90d0#;
   pragma Export (C, u00121, "system__pool_sizeS");
   u00122 : constant Version_32 := 16#f8f38c17#;
   pragma Export (C, u00122, "system__val_intB");
   u00123 : constant Version_32 := 16#ba57f2b6#;
   pragma Export (C, u00123, "system__val_intS");
   u00124 : constant Version_32 := 16#4266b2a8#;
   pragma Export (C, u00124, "system__val_unsB");
   u00125 : constant Version_32 := 16#b35ca71d#;
   pragma Export (C, u00125, "system__val_unsS");
   u00126 : constant Version_32 := 16#2b8eff16#;
   pragma Export (C, u00126, "system__os_constantsS");
   u00127 : constant Version_32 := 16#e7a737ab#;
   pragma Export (C, u00127, "gnat__sockets__serverB");
   u00128 : constant Version_32 := 16#e6c1607f#;
   pragma Export (C, u00128, "gnat__sockets__serverS");
   u00129 : constant Version_32 := 16#1c5db45a#;
   pragma Export (C, u00129, "strings_editB");
   u00130 : constant Version_32 := 16#17d137b8#;
   pragma Export (C, u00130, "strings_editS");
   u00131 : constant Version_32 := 16#12c24a43#;
   pragma Export (C, u00131, "ada__charactersS");
   u00132 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00132, "ada__characters__latin_1S");
   u00133 : constant Version_32 := 16#af50e98f#;
   pragma Export (C, u00133, "ada__stringsS");
   u00134 : constant Version_32 := 16#96e9c1e7#;
   pragma Export (C, u00134, "ada__strings__mapsB");
   u00135 : constant Version_32 := 16#6a512c5d#;
   pragma Export (C, u00135, "ada__strings__mapsS");
   u00136 : constant Version_32 := 16#21cb99ef#;
   pragma Export (C, u00136, "system__bit_opsB");
   u00137 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00137, "system__bit_opsS");
   u00138 : constant Version_32 := 16#f7832339#;
   pragma Export (C, u00138, "strings_edit__integersB");
   u00139 : constant Version_32 := 16#377b453f#;
   pragma Export (C, u00139, "strings_edit__integersS");
   u00140 : constant Version_32 := 16#5c546d28#;
   pragma Export (C, u00140, "strings_edit__integer_editB");
   u00141 : constant Version_32 := 16#c0f86606#;
   pragma Export (C, u00141, "strings_edit__integer_editS");
   u00142 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00142, "system__concat_3B");
   u00143 : constant Version_32 := 16#f982842c#;
   pragma Export (C, u00143, "system__concat_3S");
   u00144 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00144, "system__concat_2B");
   u00145 : constant Version_32 := 16#f0520f59#;
   pragma Export (C, u00145, "system__concat_2S");
   u00146 : constant Version_32 := 16#608e2cd1#;
   pragma Export (C, u00146, "system__concat_5B");
   u00147 : constant Version_32 := 16#75ac9ba7#;
   pragma Export (C, u00147, "system__concat_5S");
   u00148 : constant Version_32 := 16#932a4690#;
   pragma Export (C, u00148, "system__concat_4B");
   u00149 : constant Version_32 := 16#8c96f3a9#;
   pragma Export (C, u00149, "system__concat_4S");
   u00150 : constant Version_32 := 16#46899fd1#;
   pragma Export (C, u00150, "system__concat_7B");
   u00151 : constant Version_32 := 16#0e358396#;
   pragma Export (C, u00151, "system__concat_7S");
   u00152 : constant Version_32 := 16#a83b7c85#;
   pragma Export (C, u00152, "system__concat_6B");
   u00153 : constant Version_32 := 16#2035f53b#;
   pragma Export (C, u00153, "system__concat_6S");
   u00154 : constant Version_32 := 16#9777733a#;
   pragma Export (C, u00154, "system__img_lliB");
   u00155 : constant Version_32 := 16#e3bd8d58#;
   pragma Export (C, u00155, "system__img_lliS");
   u00156 : constant Version_32 := 16#e3a166b3#;
   pragma Export (C, u00156, "system__taskingB");
   u00157 : constant Version_32 := 16#000be662#;
   pragma Export (C, u00157, "system__taskingS");
   u00158 : constant Version_32 := 16#41756927#;
   pragma Export (C, u00158, "system__task_primitivesS");
   u00159 : constant Version_32 := 16#bb68f74c#;
   pragma Export (C, u00159, "system__os_interfaceB");
   u00160 : constant Version_32 := 16#8dff5910#;
   pragma Export (C, u00160, "system__os_interfaceS");
   u00161 : constant Version_32 := 16#e3723342#;
   pragma Export (C, u00161, "system__task_primitives__operationsB");
   u00162 : constant Version_32 := 16#3a5bfb40#;
   pragma Export (C, u00162, "system__task_primitives__operationsS");
   u00163 : constant Version_32 := 16#89b55e64#;
   pragma Export (C, u00163, "system__interrupt_managementB");
   u00164 : constant Version_32 := 16#47187112#;
   pragma Export (C, u00164, "system__interrupt_managementS");
   u00165 : constant Version_32 := 16#f65595cf#;
   pragma Export (C, u00165, "system__multiprocessorsB");
   u00166 : constant Version_32 := 16#ca5e47fa#;
   pragma Export (C, u00166, "system__multiprocessorsS");
   u00167 : constant Version_32 := 16#81c6b8c3#;
   pragma Export (C, u00167, "system__task_infoB");
   u00168 : constant Version_32 := 16#13d23ab3#;
   pragma Export (C, u00168, "system__task_infoS");
   u00169 : constant Version_32 := 16#74372fee#;
   pragma Export (C, u00169, "system__tasking__debugB");
   u00170 : constant Version_32 := 16#b9ace83d#;
   pragma Export (C, u00170, "system__tasking__debugS");
   u00171 : constant Version_32 := 16#4e9d8c2e#;
   pragma Export (C, u00171, "system__stack_usageB");
   u00172 : constant Version_32 := 16#09222097#;
   pragma Export (C, u00172, "system__stack_usageS");
   u00173 : constant Version_32 := 16#f8794da0#;
   pragma Export (C, u00173, "system__tasking__stagesB");
   u00174 : constant Version_32 := 16#665f1f08#;
   pragma Export (C, u00174, "system__tasking__stagesS");
   u00175 : constant Version_32 := 16#100eaf58#;
   pragma Export (C, u00175, "system__restrictionsB");
   u00176 : constant Version_32 := 16#15d0ffb8#;
   pragma Export (C, u00176, "system__restrictionsS");
   u00177 : constant Version_32 := 16#10015ee5#;
   pragma Export (C, u00177, "system__tasking__initializationB");
   u00178 : constant Version_32 := 16#f20398cb#;
   pragma Export (C, u00178, "system__tasking__initializationS");
   u00179 : constant Version_32 := 16#a6a45076#;
   pragma Export (C, u00179, "system__soft_links__taskingB");
   u00180 : constant Version_32 := 16#e47ef8be#;
   pragma Export (C, u00180, "system__soft_links__taskingS");
   u00181 : constant Version_32 := 16#17d21067#;
   pragma Export (C, u00181, "ada__exceptions__is_null_occurrenceB");
   u00182 : constant Version_32 := 16#30a405b9#;
   pragma Export (C, u00182, "ada__exceptions__is_null_occurrenceS");
   u00183 : constant Version_32 := 16#a5fc5d01#;
   pragma Export (C, u00183, "system__tasking__queuingB");
   u00184 : constant Version_32 := 16#3d02e133#;
   pragma Export (C, u00184, "system__tasking__queuingS");
   u00185 : constant Version_32 := 16#65fc3619#;
   pragma Export (C, u00185, "system__tasking__protected_objectsB");
   u00186 : constant Version_32 := 16#6fa056d1#;
   pragma Export (C, u00186, "system__tasking__protected_objectsS");
   u00187 : constant Version_32 := 16#f4f232d3#;
   pragma Export (C, u00187, "system__tasking__protected_objects__entriesB");
   u00188 : constant Version_32 := 16#4d64e3b6#;
   pragma Export (C, u00188, "system__tasking__protected_objects__entriesS");
   u00189 : constant Version_32 := 16#a56b69a7#;
   pragma Export (C, u00189, "system__tasking__rendezvousB");
   u00190 : constant Version_32 := 16#d7d33e30#;
   pragma Export (C, u00190, "system__tasking__rendezvousS");
   u00191 : constant Version_32 := 16#e57c2b4c#;
   pragma Export (C, u00191, "system__tasking__entry_callsB");
   u00192 : constant Version_32 := 16#e5160f9e#;
   pragma Export (C, u00192, "system__tasking__entry_callsS");
   u00193 : constant Version_32 := 16#3852d274#;
   pragma Export (C, u00193, "system__tasking__protected_objects__operationsB");
   u00194 : constant Version_32 := 16#415d7a1b#;
   pragma Export (C, u00194, "system__tasking__protected_objects__operationsS");
   u00195 : constant Version_32 := 16#cfd258fe#;
   pragma Export (C, u00195, "system__tasking__utilitiesB");
   u00196 : constant Version_32 := 16#39283e2c#;
   pragma Export (C, u00196, "system__tasking__utilitiesS");
   u00197 : constant Version_32 := 16#bd6fc52e#;
   pragma Export (C, u00197, "system__traces__taskingB");
   u00198 : constant Version_32 := 16#33a47127#;
   pragma Export (C, u00198, "system__traces__taskingS");
   u00199 : constant Version_32 := 16#8db46565#;
   pragma Export (C, u00199, "ada__real_timeB");
   u00200 : constant Version_32 := 16#41de19c7#;
   pragma Export (C, u00200, "ada__real_timeS");
   u00201 : constant Version_32 := 16#08b07a85#;
   pragma Export (C, u00201, "system__arith_64B");
   u00202 : constant Version_32 := 16#db12ccc2#;
   pragma Export (C, u00202, "system__arith_64S");
   u00203 : constant Version_32 := 16#e1f95e24#;
   pragma Export (C, u00203, "generic_unbounded_arrayB");
   u00204 : constant Version_32 := 16#fd0ee3e2#;
   pragma Export (C, u00204, "generic_unbounded_arrayS");
   u00205 : constant Version_32 := 16#af7e9276#;
   pragma Export (C, u00205, "objectB");
   u00206 : constant Version_32 := 16#dfc1ee20#;
   pragma Export (C, u00206, "objectS");
   u00207 : constant Version_32 := 16#8d0a44e6#;
   pragma Export (C, u00207, "object__handleB");
   u00208 : constant Version_32 := 16#1ac8f53d#;
   pragma Export (C, u00208, "object__handleS");
   u00209 : constant Version_32 := 16#0b3b3742#;
   pragma Export (C, u00209, "object__handle__generic_unbounded_arrayB");
   u00210 : constant Version_32 := 16#7a27cc93#;
   pragma Export (C, u00210, "object__handle__generic_unbounded_arrayS");
   u00211 : constant Version_32 := 16#ffe20862#;
   pragma Export (C, u00211, "system__stream_attributesB");
   u00212 : constant Version_32 := 16#e5402c91#;
   pragma Export (C, u00212, "system__stream_attributesS");
   u00213 : constant Version_32 := 16#82dabc57#;
   pragma Export (C, u00213, "gnat__sockets__server__pooledB");
   u00214 : constant Version_32 := 16#0e91132c#;
   pragma Export (C, u00214, "gnat__sockets__server__pooledS");
   u00215 : constant Version_32 := 16#b41e8d29#;
   pragma Export (C, u00215, "test_http_serversB");
   u00216 : constant Version_32 := 16#960449b9#;
   pragma Export (C, u00216, "test_http_serversS");
   u00217 : constant Version_32 := 16#e5c17123#;
   pragma Export (C, u00217, "ada__streams__stream_ioB");
   u00218 : constant Version_32 := 16#68da1e56#;
   pragma Export (C, u00218, "ada__streams__stream_ioS");
   u00219 : constant Version_32 := 16#0598743c#;
   pragma Export (C, u00219, "strings_edit__quotedB");
   u00220 : constant Version_32 := 16#ce827f1f#;
   pragma Export (C, u00220, "strings_edit__quotedS");
   u00221 : constant Version_32 := 16#98ac7b90#;
   pragma Export (C, u00221, "strings_edit__fieldsB");
   u00222 : constant Version_32 := 16#0b9cc63b#;
   pragma Export (C, u00222, "strings_edit__fieldsS");
   u00223 : constant Version_32 := 16#dd8e3839#;
   pragma Export (C, u00223, "gnat__directory_operationsB");
   u00224 : constant Version_32 := 16#076c78bb#;
   pragma Export (C, u00224, "gnat__directory_operationsS");
   u00225 : constant Version_32 := 16#239b1678#;
   pragma Export (C, u00225, "ada__characters__handlingB");
   u00226 : constant Version_32 := 16#313e5d46#;
   pragma Export (C, u00226, "ada__characters__handlingS");
   u00227 : constant Version_32 := 16#34090881#;
   pragma Export (C, u00227, "ada__strings__maps__constantsS");
   u00228 : constant Version_32 := 16#914b496f#;
   pragma Export (C, u00228, "ada__strings__fixedB");
   u00229 : constant Version_32 := 16#dc686502#;
   pragma Export (C, u00229, "ada__strings__fixedS");
   u00230 : constant Version_32 := 16#19aaa432#;
   pragma Export (C, u00230, "ada__strings__searchB");
   u00231 : constant Version_32 := 16#b5a8c1d6#;
   pragma Export (C, u00231, "ada__strings__searchS");
   u00232 : constant Version_32 := 16#9e8a3eec#;
   pragma Export (C, u00232, "gnat__sockets__connection_state_machineB");
   u00233 : constant Version_32 := 16#3c2e4980#;
   pragma Export (C, u00233, "gnat__sockets__connection_state_machineS");
   u00234 : constant Version_32 := 16#c571ff30#;
   pragma Export (C, u00234, "gnat__sockets__connection_state_machine__http_serverB");
   u00235 : constant Version_32 := 16#cf5572e7#;
   pragma Export (C, u00235, "gnat__sockets__connection_state_machine__http_serverS");
   u00236 : constant Version_32 := 16#077f0b47#;
   pragma Export (C, u00236, "gnat__sha1B");
   u00237 : constant Version_32 := 16#6e14b0a7#;
   pragma Export (C, u00237, "gnat__sha1S");
   u00238 : constant Version_32 := 16#133318a1#;
   pragma Export (C, u00238, "gnat__secure_hashesB");
   u00239 : constant Version_32 := 16#76ee2707#;
   pragma Export (C, u00239, "gnat__secure_hashesS");
   u00240 : constant Version_32 := 16#cadfacae#;
   pragma Export (C, u00240, "gnat__secure_hashes__sha1B");
   u00241 : constant Version_32 := 16#a1cfcd9e#;
   pragma Export (C, u00241, "gnat__secure_hashes__sha1S");
   u00242 : constant Version_32 := 16#45efda4c#;
   pragma Export (C, u00242, "gnat__byte_swappingB");
   u00243 : constant Version_32 := 16#ab0a1c41#;
   pragma Export (C, u00243, "gnat__byte_swappingS");
   u00244 : constant Version_32 := 16#8b6eacc9#;
   pragma Export (C, u00244, "system__byte_swappingS");
   u00245 : constant Version_32 := 16#e40edcb6#;
   pragma Export (C, u00245, "gnat__sockets__connection_state_machine__big_endianS");
   u00246 : constant Version_32 := 16#bf2f42ad#;
   pragma Export (C, u00246, "gnat__sockets__connection_state_machine__big_endian__unsignedsB");
   u00247 : constant Version_32 := 16#f1a816f2#;
   pragma Export (C, u00247, "gnat__sockets__connection_state_machine__big_endian__unsignedsS");
   u00248 : constant Version_32 := 16#ebbe1dbe#;
   pragma Export (C, u00248, "strings_edit__base64B");
   u00249 : constant Version_32 := 16#3f0fc4fb#;
   pragma Export (C, u00249, "strings_edit__base64S");
   u00250 : constant Version_32 := 16#bf9cfb94#;
   pragma Export (C, u00250, "strings_edit__floatsB");
   u00251 : constant Version_32 := 16#707fba21#;
   pragma Export (C, u00251, "strings_edit__floatsS");
   u00252 : constant Version_32 := 16#84ad4a42#;
   pragma Export (C, u00252, "ada__numericsS");
   u00253 : constant Version_32 := 16#03e83d1c#;
   pragma Export (C, u00253, "ada__numerics__elementary_functionsB");
   u00254 : constant Version_32 := 16#47bfe5ef#;
   pragma Export (C, u00254, "ada__numerics__elementary_functionsS");
   u00255 : constant Version_32 := 16#3e0cf54d#;
   pragma Export (C, u00255, "ada__numerics__auxB");
   u00256 : constant Version_32 := 16#9f6e24ed#;
   pragma Export (C, u00256, "ada__numerics__auxS");
   u00257 : constant Version_32 := 16#2dc906b9#;
   pragma Export (C, u00257, "system__fat_llfS");
   u00258 : constant Version_32 := 16#fd49a347#;
   pragma Export (C, u00258, "system__machine_codeS");
   u00259 : constant Version_32 := 16#0be1b996#;
   pragma Export (C, u00259, "system__exn_llfB");
   u00260 : constant Version_32 := 16#7376c666#;
   pragma Export (C, u00260, "system__exn_llfS");
   u00261 : constant Version_32 := 16#712ba15e#;
   pragma Export (C, u00261, "system__fat_fltS");
   u00262 : constant Version_32 := 16#0a12f050#;
   pragma Export (C, u00262, "strings_edit__float_editB");
   u00263 : constant Version_32 := 16#cfe341b5#;
   pragma Export (C, u00263, "strings_edit__float_editS");
   u00264 : constant Version_32 := 16#5aac8b31#;
   pragma Export (C, u00264, "strings_edit__utf8B");
   u00265 : constant Version_32 := 16#d5db5807#;
   pragma Export (C, u00265, "strings_edit__utf8S");
   u00266 : constant Version_32 := 16#78cb869e#;
   pragma Export (C, u00266, "system__concat_9B");
   u00267 : constant Version_32 := 16#2eb8ecad#;
   pragma Export (C, u00267, "system__concat_9S");
   u00268 : constant Version_32 := 16#46b1f5ea#;
   pragma Export (C, u00268, "system__concat_8B");
   u00269 : constant Version_32 := 16#11f5955e#;
   pragma Export (C, u00269, "system__concat_8S");
   u00270 : constant Version_32 := 16#22ab03a2#;
   pragma Export (C, u00270, "system__img_unsB");
   u00271 : constant Version_32 := 16#3c0076d1#;
   pragma Export (C, u00271, "system__img_unsS");
   u00272 : constant Version_32 := 16#7a13e6d7#;
   pragma Export (C, u00272, "ada__calendar__formattingB");
   u00273 : constant Version_32 := 16#929f882b#;
   pragma Export (C, u00273, "ada__calendar__formattingS");
   u00274 : constant Version_32 := 16#e3cca715#;
   pragma Export (C, u00274, "ada__calendar__time_zonesB");
   u00275 : constant Version_32 := 16#98f012d7#;
   pragma Export (C, u00275, "ada__calendar__time_zonesS");
   u00276 : constant Version_32 := 16#8ff77155#;
   pragma Export (C, u00276, "system__val_realB");
   u00277 : constant Version_32 := 16#0cdbaf98#;
   pragma Export (C, u00277, "system__val_realS");
   u00278 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00278, "system__float_controlB");
   u00279 : constant Version_32 := 16#120e9bb5#;
   pragma Export (C, u00279, "system__float_controlS");
   u00280 : constant Version_32 := 16#a282befe#;
   pragma Export (C, u00280, "system__powten_tableS");
   u00281 : constant Version_32 := 16#5cf80e25#;
   pragma Export (C, u00281, "ada__task_identificationB");
   u00282 : constant Version_32 := 16#9ad8d545#;
   pragma Export (C, u00282, "ada__task_identificationS");
   u00283 : constant Version_32 := 16#f7029c34#;
   pragma Export (C, u00283, "generic_discrete_setB");
   u00284 : constant Version_32 := 16#5b503d69#;
   pragma Export (C, u00284, "generic_discrete_setS");
   u00285 : constant Version_32 := 16#d59316cc#;
   pragma Export (C, u00285, "generic_setB");
   u00286 : constant Version_32 := 16#5a9ca29d#;
   pragma Export (C, u00286, "generic_setS");
   u00287 : constant Version_32 := 16#036ed0e3#;
   pragma Export (C, u00287, "gnat__sockets__connection_state_machine__expected_sequenceB");
   u00288 : constant Version_32 := 16#1112fc75#;
   pragma Export (C, u00288, "gnat__sockets__connection_state_machine__expected_sequenceS");
   u00289 : constant Version_32 := 16#743971fd#;
   pragma Export (C, u00289, "gnat__sockets__connection_state_machine__terminated_stringsB");
   u00290 : constant Version_32 := 16#68dc845b#;
   pragma Export (C, u00290, "gnat__sockets__connection_state_machine__terminated_stringsS");
   u00291 : constant Version_32 := 16#0bf4b510#;
   pragma Export (C, u00291, "system__strings__stream_opsB");
   u00292 : constant Version_32 := 16#5ed775a4#;
   pragma Export (C, u00292, "system__strings__stream_opsS");
   u00293 : constant Version_32 := 16#fcc0579f#;
   pragma Export (C, u00293, "stack_storageB");
   u00294 : constant Version_32 := 16#d77a936b#;
   pragma Export (C, u00294, "stack_storageS");
   u00295 : constant Version_32 := 16#9f2ee7c1#;
   pragma Export (C, u00295, "generic_unbounded_ptr_arrayB");
   u00296 : constant Version_32 := 16#6a65cc50#;
   pragma Export (C, u00296, "generic_unbounded_ptr_arrayS");
   u00297 : constant Version_32 := 16#5dbee6d7#;
   pragma Export (C, u00297, "tablesB");
   u00298 : constant Version_32 := 16#e9c0911e#;
   pragma Export (C, u00298, "tablesS");
   u00299 : constant Version_32 := 16#6d8e325b#;
   pragma Export (C, u00299, "tables__namesB");
   u00300 : constant Version_32 := 16#4de14fef#;
   pragma Export (C, u00300, "tables__namesS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.handling%s
   --  ada.characters.latin_1%s
   --  gnat%s
   --  interfaces%s
   --  system%s
   --  gnat.byte_swapping%s
   --  system.arith_64%s
   --  system.byte_swapping%s
   --  gnat.byte_swapping%b
   --  system.case_util%s
   --  system.case_util%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.htable%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.machine_code%s
   --  system.multiprocessors%s
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.restrictions%s
   --  system.restrictions%b
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.os_lib%s
   --  gnat.os_lib%s
   --  system.task_info%s
   --  system.task_info%b
   --  system.task_lock%s
   --  gnat.task_lock%s
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.arith_64%b
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  system.soft_links%s
   --  system.task_lock%b
   --  system.traces%s
   --  system.traces%b
   --  system.unsigned_types%s
   --  system.fat_flt%s
   --  system.fat_llf%s
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.val_int%s
   --  system.val_llu%s
   --  system.val_real%s
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.val_real%b
   --  system.val_llu%b
   --  system.val_int%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  system.address_image%s
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_5%s
   --  system.concat_5%b
   --  system.concat_6%s
   --  system.concat_6%b
   --  system.concat_7%s
   --  system.concat_7%b
   --  system.concat_8%s
   --  system.concat_8%b
   --  system.concat_9%s
   --  system.concat_9%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.io_exceptions%s
   --  ada.numerics%s
   --  ada.numerics.aux%s
   --  ada.numerics.aux%b
   --  ada.numerics.elementary_functions%s
   --  ada.numerics.elementary_functions%b
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.fixed%s
   --  ada.strings.maps.constants%s
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.tags%s
   --  ada.streams%s
   --  ada.streams%b
   --  interfaces.c%s
   --  system.multiprocessors%b
   --  interfaces.c.strings%s
   --  system.communication%s
   --  system.communication%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  system.os_constants%s
   --  system.os_interface%s
   --  system.os_interface%b
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.task_primitives%s
   --  system.tasking%s
   --  ada.task_identification%s
   --  system.task_primitives.operations%s
   --  system.tasking%b
   --  system.tasking.debug%s
   --  system.task_primitives.operations%b
   --  system.traces.tasking%s
   --  system.traces.tasking%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.delays%s
   --  ada.calendar.delays%b
   --  ada.calendar.time_zones%s
   --  ada.calendar.time_zones%b
   --  ada.calendar.formatting%s
   --  gnat.directory_operations%s
   --  gnat.secure_hashes%s
   --  gnat.secure_hashes%b
   --  gnat.secure_hashes.sha1%s
   --  gnat.secure_hashes.sha1%b
   --  gnat.sha1%s
   --  gnat.sha1%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.file_control_block%s
   --  ada.streams.stream_io%s
   --  system.file_io%s
   --  ada.streams.stream_io%b
   --  gnat.sockets%s
   --  gnat.sockets.linker_options%s
   --  system.pool_size%s
   --  system.pool_size%b
   --  system.secondary_stack%s
   --  system.file_io%b
   --  system.tasking.debug%b
   --  system.storage_pools.subpools%b
   --  system.finalization_masters%b
   --  interfaces.c.strings%b
   --  interfaces.c%b
   --  ada.tags%b
   --  ada.strings.fixed%b
   --  ada.strings.maps%b
   --  system.soft_links%b
   --  system.os_lib%b
   --  ada.characters.handling%b
   --  system.secondary_stack%b
   --  gnat.directory_operations%b
   --  ada.calendar.formatting%b
   --  system.address_image%b
   --  gnat.sockets.thin_common%s
   --  gnat.sockets.thin_common%b
   --  gnat.sockets.thin%s
   --  gnat.sockets.thin%b
   --  gnat.sockets%b
   --  system.soft_links.tasking%s
   --  system.soft_links.tasking%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  system.tasking.entry_calls%s
   --  system.tasking.initialization%s
   --  system.tasking.initialization%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.utilities%s
   --  ada.task_identification%b
   --  system.traceback%s
   --  ada.exceptions%b
   --  system.traceback%b
   --  ada.real_time%s
   --  ada.real_time%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.tasking.protected_objects.entries%s
   --  system.tasking.protected_objects.entries%b
   --  system.tasking.queuing%s
   --  system.tasking.queuing%b
   --  system.tasking.utilities%b
   --  system.tasking.rendezvous%s
   --  system.tasking.protected_objects.operations%s
   --  system.tasking.protected_objects.operations%b
   --  system.tasking.rendezvous%b
   --  system.tasking.entry_calls%b
   --  system.tasking.stages%s
   --  system.tasking.stages%b
   --  generic_set%s
   --  generic_set%b
   --  generic_discrete_set%s
   --  generic_discrete_set%b
   --  generic_unbounded_array%s
   --  generic_unbounded_array%b
   --  generic_unbounded_ptr_array%s
   --  generic_unbounded_ptr_array%b
   --  object%s
   --  object%b
   --  object.handle%s
   --  object.handle%b
   --  object.handle.generic_unbounded_array%s
   --  object.handle.generic_unbounded_array%b
   --  stack_storage%s
   --  stack_storage%b
   --  strings_edit%s
   --  strings_edit%b
   --  strings_edit.base64%s
   --  strings_edit.base64%b
   --  strings_edit.fields%s
   --  strings_edit.fields%b
   --  strings_edit.float_edit%s
   --  strings_edit.integer_edit%s
   --  strings_edit.integer_edit%b
   --  gnat.sockets.server%s
   --  gnat.sockets.connection_state_machine%s
   --  gnat.sockets.connection_state_machine%b
   --  gnat.sockets.connection_state_machine.big_endian%s
   --  gnat.sockets.connection_state_machine.big_endian.unsigneds%s
   --  gnat.sockets.connection_state_machine.big_endian.unsigneds%b
   --  gnat.sockets.connection_state_machine.expected_sequence%s
   --  gnat.sockets.connection_state_machine.expected_sequence%b
   --  gnat.sockets.connection_state_machine.terminated_strings%s
   --  gnat.sockets.server.pooled%s
   --  gnat.sockets.server.pooled%b
   --  strings_edit.integers%s
   --  strings_edit.integers%b
   --  gnat.sockets.connection_state_machine.terminated_strings%b
   --  gnat.sockets.server%b
   --  strings_edit.float_edit%b
   --  strings_edit.floats%s
   --  strings_edit.floats%b
   --  strings_edit.quoted%s
   --  strings_edit.quoted%b
   --  strings_edit.utf8%s
   --  strings_edit.utf8%b
   --  tables%s
   --  tables%b
   --  tables.names%s
   --  tables.names%b
   --  gnat.sockets.connection_state_machine.http_server%s
   --  gnat.sockets.connection_state_machine.http_server%b
   --  test_http_servers%s
   --  test_http_servers%b
   --  test_http_server%b
   --  END ELABORATION ORDER


end ada_main;
