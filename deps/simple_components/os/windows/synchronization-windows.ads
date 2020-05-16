--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Windows                     Luebeck            --
--  Interface                                      Spring, 2018       --
--                                                           --
--                                Last revision :  22:08 06 Jan 2020  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--
--
--  This package  provides  binding to some Windows API functions useful
--  to communicate between processes.
--
with Ada.Exceptions;           use Ada.Exceptions;
with Interfaces;               use Interfaces;
with Interfaces.C;             use Interfaces.C;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;

package Synchronization.Windows is

   type BOOL   is new int;
   type HANDLE is new ptrdiff_t;
   type HANDLE_Ptr is access all HANDLE;
   type DWORD  is new unsigned_long;
   type SECURITY_ATTRIBUTES is record
      Length             : DWORD;
      SecurityDescriptor : Address;
      InheritHandle      : BOOL;
   end record;
   pragma Convention (C, SECURITY_ATTRIBUTES);
   type SECURITY_ATTRIBUTES_Ptr is access all SECURITY_ATTRIBUTES;

   INVALID_HANDLE_VALUE    : constant HANDLE := -1;

   PAGE_NOACCESS           : constant := 16#1#;
   PAGE_READONLY           : constant := 16#2#;
   PAGE_READWRITE          : constant := 16#4#;
   PAGE_WRITECOPY          : constant := 16#8#;
   PAGE_EXECUTE            : constant := 16#10#;
   PAGE_EXECUTE_READ       : constant := 16#20#;
   PAGE_EXECUTE_READWRITE  : constant := 16#40#;
   PAGE_EXECUTE_WRITECOPY  : constant := 16#80#;
   PAGE_GUARD              : constant := 16#100#;
   PAGE_NOCACHE            : constant := 16#200#;

   SEC_FILE                : constant := 16#800000#;
   SEC_IMAGE               : constant := 16#1000000#;
   SEC_RESERVE             : constant := 16#4000000#;
   SEC_COMMIT              : constant := 16#8000000#;
   SEC_NOCACHE             : constant := 16#10000000#;

   PROCESS_TERMINATE         : constant := 16#1#;
   PROCESS_CREATE_THREAD     : constant := 16#2#;
   PROCESS_VM_OPERATION      : constant := 16#8#;
   PROCESS_VM_READ           : constant := 16#10#;
   PROCESS_VM_WRITE          : constant := 16#20#;
   PROCESS_DUP_HANDLE        : constant := 16#40#;
   PROCESS_CREATE_PROCESS    : constant := 16#80#;
   PROCESS_SET_QUOTA         : constant := 16#100#;
   PROCESS_SET_INFORMATION   : constant := 16#200#;
   PROCESS_QUERY_INFORMATION : constant := 16#400#;
   PROCESS_ALL_ACCESS        : constant := 16#1F0FFF#;

   function CloseHandle (Object : HANDLE) return BOOL;
   function CreateEvent
            (  EventAttributes : SECURITY_ATTRIBUTES_Ptr := null;
               ManualReset     : BOOL                    := 0;
               InitialState    : BOOL                    := 0;
               Name            : chars_ptr               := Null_Ptr
            )  return HANDLE;
   function CreateFileMapping
            (  File               : HANDLE := INVALID_HANDLE_VALUE;
               SecurityAttributes : SECURITY_ATTRIBUTES_Ptr := null;
               Protect            : DWORD;
               MaximumSizeHigh    : DWORD := 0;
               MaximumSizeLow     : DWORD;
               Name               : chars_ptr
            )  return HANDLE;
   function CreateMutex
            (  MutexAttributes : SECURITY_ATTRIBUTES_Ptr := null;
               InitialOwner    : BOOL                    := 0;
               Name            :chars_ptr                := Null_Ptr
            )  return HANDLE;

   DUPLICATE_CLOSE_SOURCE : constant := 16#1#;
   DUPLICATE_SAME_ACCESS  : constant := 16#2#;

   function DuplicateHandle
            (  SourceProcessHandle : HANDLE;
               SourceHandle        : HANDLE;
               TargetProcessHandle : HANDLE;
               TargetHandle        : HANDLE_Ptr;
               DesiredAccess       : DWORD := 0;
               InheritHandle       : BOOL  := 0;
               Options             : DWORD := DUPLICATE_SAME_ACCESS
            )  return BOOL;

   function GetErrorText (Error : DWORD) return String;
   function GetCurrentProcess return HANDLE;
   function GetCurrentProcessId return DWORD;
   function GetLastError return DWORD;

   FILE_MAP_COPY       : constant := 16#1#;
   FILE_MAP_WRITE      : constant := 16#2#;
   FILE_MAP_READ       : constant := 16#4#;
   FILE_MAP_ALL_ACCESS : constant := 16#F001F#;

   function MapViewOfFile
            (  FileMappingObject  : HANDLE;
               DesiredAccess      : DWORD := FILE_MAP_ALL_ACCESS;
               FileOffsetHigh     : DWORD := 0;
               FileOffsetLow      : DWORD := 0;
               NumberOfBytesToMap : size_t
            )  return Address;
   function OpenFileMapping
            (  DesiredAccess : DWORD := FILE_MAP_ALL_ACCESS;
               InheritHandle : BOOL  := 0;
               Name          : chars_ptr
            )  return HANDLE;
   function OpenProcess
            (  DesiredAccess : DWORD;
               InheritHandle : BOOL := 0;
               ProcessId     : DWORD
            )  return HANDLE;

   function PulseEvent   (Event : HANDLE) return BOOL;
   function ReleaseMutex (Mutex : HANDLE) return BOOL;
   function ResetEvent   (Event : HANDLE) return BOOL;
   function SetEvent     (Event : HANDLE) return BOOL;

   INFINITE : constant := 16#FFFF_FFFF#;

   function Milliseconds (Value : Duration) return DWORD;

   WAIT_FAILED      : constant := 16#FFFF_FFFF#;
   WAIT_OBJECT_0    : constant := 16#0#;
   WAIT_ABANDONED   : constant := 16#80#;
   WAIT_ABANDONED_0 : constant := 16#80#;
   WAIT_TIMEOUT     : constant := 16#102#;

   function WaitForSingleObject
            (  Object       : HANDLE;
               Milliseconds : DWORD
            )  return DWORD;
   function UnmapViewOfFile (BaseAddress : Address) return BOOL;

   procedure Raise_From_LastError
             (  ID     : Exception_ID;
                Prefix : String := "";
                Error  : DWORD
             );

   NO_ERROR                                : constant := 0;
   ERROR_INVALID_FUNCTION                  : constant := 1;
   ERROR_FILE_NOT_FOUND                    : constant := 2;
   ERROR_PATH_NOT_FOUND                    : constant := 3;
   ERROR_TOO_MANY_OPEN_FILES               : constant := 4;
   ERROR_ACCESS_DENIED                     : constant := 5;
   ERROR_INVALID_HANDLE                    : constant := 6;
   ERROR_ARENA_TRASHED                     : constant := 7;
   ERROR_NOT_ENOUGH_MEMORY                 : constant := 8;
   ERROR_INVALID_BLOCK                     : constant := 9;
   ERROR_BAD_ENVIRONMENT                   : constant := 10;
   ERROR_BAD_FORMAT                        : constant := 11;
   ERROR_INVALID_ACCESS                    : constant := 12;
   ERROR_INVALID_DATA                      : constant := 13;
   ERROR_OUTOFMEMORY                       : constant := 14;
   ERROR_INVALID_DRIVE                     : constant := 15;
   ERROR_CURRENT_DIRECTORY                 : constant := 16;
   ERROR_NOT_SAME_DEVICE                   : constant := 17;
   ERROR_NO_MORE_FILES                     : constant := 18;
   ERROR_WRITE_PROTECT                     : constant := 19;
   ERROR_BAD_UNIT                          : constant := 20;
   ERROR_NOT_READY                         : constant := 21;
   ERROR_BAD_COMMAND                       : constant := 22;
   ERROR_CRC                               : constant := 23;
   ERROR_BAD_LENGTH                        : constant := 24;
   ERROR_SEEK                              : constant := 25;
   ERROR_NOT_DOS_DISK                      : constant := 26;
   ERROR_SECTOR_NOT_FOUND                  : constant := 27;
   ERROR_OUT_OF_PAPER                      : constant := 28;
   ERROR_WRITE_FAULT                       : constant := 29;
   ERROR_READ_FAULT                        : constant := 30;
   ERROR_GEN_FAILURE                       : constant := 31;
   ERROR_SHARING_VIOLATION                 : constant := 32;
   ERROR_LOCK_VIOLATION                    : constant := 33;
   ERROR_WRONG_DISK                        : constant := 34;
   ERROR_SHARING_BUFFER_EXCEEDED           : constant := 36;
   ERROR_HANDLE_EOF                        : constant := 38;
   ERROR_HANDLE_DISK_FULL                  : constant := 39;
   ERROR_NOT_SUPPORTED                     : constant := 50;
   ERROR_REM_NOT_LIST                      : constant := 51;
   ERROR_DUP_NAME                          : constant := 52;
   ERROR_BAD_NETPATH                       : constant := 53;
   ERROR_NETWORK_BUSY                      : constant := 54;
   ERROR_DEV_NOT_EXIST                     : constant := 55;
   ERROR_TOO_MANY_CMDS                     : constant := 56;
   ERROR_ADAP_HDW_ERR                      : constant := 57;
   ERROR_BAD_NET_RESP                      : constant := 58;
   ERROR_UNEXP_NET_ERR                     : constant := 59;
   ERROR_BAD_REM_ADAP                      : constant := 60;
   ERROR_PRINTQ_FULL                       : constant := 61;
   ERROR_NO_SPOOL_SPACE                    : constant := 62;
   ERROR_PRINT_CANCELLED                   : constant := 63;
   ERROR_NETNAME_DELETED                   : constant := 64;
   ERROR_NETWORK_ACCESS_DENIED             : constant := 65;
   ERROR_BAD_DEV_TYPE                      : constant := 66;
   ERROR_BAD_NET_NAME                      : constant := 67;
   ERROR_TOO_MANY_NAMES                    : constant := 68;
   ERROR_TOO_MANY_SESS                     : constant := 69;
   ERROR_SHARING_PAUSED                    : constant := 70;
   ERROR_REQ_NOT_ACCEP                     : constant := 71;
   ERROR_REDIR_PAUSED                      : constant := 72;
   ERROR_FILE_EXISTS                       : constant := 80;
   ERROR_CANNOT_MAKE                       : constant := 82;
   ERROR_FAIL_I24                          : constant := 83;
   ERROR_OUT_OF_STRUCTURES                 : constant := 84;
   ERROR_ALREADY_ASSIGNED                  : constant := 85;
   ERROR_INVALID_PASSWORD                  : constant := 86;
   ERROR_INVALID_PARAMETER                 : constant := 87;
   ERROR_NET_WRITE_FAULT                   : constant := 88;
   ERROR_NO_PROC_SLOTS                     : constant := 89;
   ERROR_TOO_MANY_SEMAPHORES               : constant := 100;
   ERROR_EXCL_SEM_ALREADY_OWNED            : constant := 101;
   ERROR_SEM_IS_SET                        : constant := 102;
   ERROR_TOO_MANY_SEM_REQUESTS             : constant := 103;
   ERROR_INVALID_AT_INTERRUPT_TIME         : constant := 104;
   ERROR_SEM_OWNER_DIED                    : constant := 105;
   ERROR_SEM_USER_LIMIT                    : constant := 106;
   ERROR_DISK_CHANGE                       : constant := 107;
   ERROR_DRIVE_LOCKED                      : constant := 108;
   ERROR_BROKEN_PIPE                       : constant := 109;
   ERROR_OPEN_FAILED                       : constant := 110;
   ERROR_BUFFER_OVERFLOW                   : constant := 111;
   ERROR_DISK_FULL                         : constant := 112;
   ERROR_NO_MORE_SEARCH_HANDLES            : constant := 113;
   ERROR_INVALID_TARGET_HANDLE             : constant := 114;
   ERROR_INVALID_CATEGORY                  : constant := 117;
   ERROR_INVALID_VERIFY_SWITCH             : constant := 118;
   ERROR_BAD_DRIVER_LEVEL                  : constant := 119;
   ERROR_CALL_NOT_IMPLEMENTED              : constant := 120;
   ERROR_SEM_TIMEOUT                       : constant := 121;
   ERROR_INSUFFICIENT_BUFFER               : constant := 122;
   ERROR_INVALID_NAME                      : constant := 123;
   ERROR_INVALID_LEVEL                     : constant := 124;
   ERROR_NO_VOLUME_LABEL                   : constant := 125;
   ERROR_MOD_NOT_FOUND                     : constant := 126;
   ERROR_PROC_NOT_FOUND                    : constant := 127;
   ERROR_WAIT_NO_CHILDREN                  : constant := 128;
   ERROR_CHILD_NOT_COMPLETE                : constant := 129;
   ERROR_DIRECT_ACCESS_HANDLE              : constant := 130;
   ERROR_NEGATIVE_SEEK                     : constant := 131;
   ERROR_SEEK_ON_DEVICE                    : constant := 132;
   ERROR_IS_JOIN_TARGET                    : constant := 133;
   ERROR_IS_JOINED                         : constant := 134;
   ERROR_IS_SUBSTED                        : constant := 135;
   ERROR_NOT_JOINED                        : constant := 136;
   ERROR_NOT_SUBSTED                       : constant := 137;
   ERROR_JOIN_TO_JOIN                      : constant := 138;
   ERROR_SUBST_TO_SUBST                    : constant := 139;
   ERROR_JOIN_TO_SUBST                     : constant := 140;
   ERROR_SUBST_TO_JOIN                     : constant := 141;
   ERROR_BUSY_DRIVE                        : constant := 142;
   ERROR_SAME_DRIVE                        : constant := 143;
   ERROR_DIR_NOT_ROOT                      : constant := 144;
   ERROR_DIR_NOT_EMPTY                     : constant := 145;
   ERROR_IS_SUBST_PATH                     : constant := 146;
   ERROR_IS_JOIN_PATH                      : constant := 147;
   ERROR_PATH_BUSY                         : constant := 148;
   ERROR_IS_SUBST_TARGET                   : constant := 149;
   ERROR_SYSTEM_TRACE                      : constant := 150;
   ERROR_INVALID_EVENT_COUNT               : constant := 151;
   ERROR_TOO_MANY_MUXWAITERS               : constant := 152;
   ERROR_INVALID_LIST_FORMAT               : constant := 153;
   ERROR_LABEL_TOO_LONG                    : constant := 154;
   ERROR_TOO_MANY_TCBS                     : constant := 155;
   ERROR_SIGNAL_REFUSED                    : constant := 156;
   ERROR_DISCARDED                         : constant := 157;
   ERROR_NOT_LOCKED                        : constant := 158;
   ERROR_BAD_THREADID_ADDR                 : constant := 159;
   ERROR_BAD_ARGUMENTS                     : constant := 160;
   ERROR_BAD_PATHNAME                      : constant := 161;
   ERROR_SIGNAL_PENDING                    : constant := 162;
   ERROR_MAX_THRDS_REACHED                 : constant := 164;
   ERROR_LOCK_FAILED                       : constant := 167;
   ERROR_BUSY                              : constant := 170;
   ERROR_CANCEL_VIOLATION                  : constant := 173;
   ERROR_ATOMIC_LOCKS_NOT_SUPPORTED        : constant := 174;
   ERROR_INVALID_SEGMENT_NUMBER            : constant := 180;
   ERROR_INVALID_ORDINAL                   : constant := 182;
   ERROR_ALREADY_EXISTS                    : constant := 183;
   ERROR_INVALID_FLAG_NUMBER               : constant := 186;
   ERROR_SEM_NOT_FOUND                     : constant := 187;
   ERROR_INVALID_STARTING_CODESEG          : constant := 188;
   ERROR_INVALID_STACKSEG                  : constant := 189;
   ERROR_INVALID_MODULETYPE                : constant := 190;
   ERROR_INVALID_EXE_SIGNATURE             : constant := 191;
   ERROR_EXE_MARKED_INVALID                : constant := 192;
   ERROR_BAD_EXE_FORMAT                    : constant := 193;
   ERROR_ITERATED_DATA_EXCEEDS_64K         : constant := 194;
   ERROR_INVALID_MINALLOCSIZE              : constant := 195;
   ERROR_DYNLINK_FROM_INVALID_RING         : constant := 196;
   ERROR_IOPL_NOT_ENABLED                  : constant := 197;
   ERROR_INVALID_SEGDPL                    : constant := 198;
   ERROR_AUTODATASEG_EXCEEDS_64K           : constant := 199;
   ERROR_RING2SEG_MUST_BE_MOVABLE          : constant := 200;
   ERROR_RELOC_CHAIN_XEEDS_SEGLIM          : constant := 201;
   ERROR_INFLOOP_IN_RELOC_CHAIN            : constant := 202;
   ERROR_ENVVAR_NOT_FOUND                  : constant := 203;
   ERROR_NO_SIGNAL_SENT                    : constant := 205;
   ERROR_FILENAME_EXCED_RANGE              : constant := 206;
   ERROR_RING2_STACK_IN_USE                : constant := 207;
   ERROR_META_EXPANSION_TOO_LONG           : constant := 208;
   ERROR_INVALID_SIGNAL_NUMBER             : constant := 209;
   ERROR_THREAD_1_INACTIVE                 : constant := 210;
   ERROR_LOCKED                            : constant := 212;
   ERROR_TOO_MANY_MODULES                  : constant := 214;
   ERROR_NESTING_NOT_ALLOWED               : constant := 215;
   ERROR_BAD_PIPE                          : constant := 230;
   ERROR_PIPE_BUSY                         : constant := 231;
   ERROR_NO_DATA                           : constant := 232;
   ERROR_PIPE_NOT_CONNECTED                : constant := 233;
   ERROR_MORE_DATA                         : constant := 234;
   ERROR_VC_DISCONNECTED                   : constant := 240;
   ERROR_INVALID_EA_NAME                   : constant := 254;
   ERROR_EA_LIST_INCONSISTENT              : constant := 255;
   ERROR_NO_MORE_ITEMS                     : constant := 259;
   ERROR_CANNOT_COPY                       : constant := 266;
   ERROR_DIRECTORY                         : constant := 267;
   ERROR_EAS_DIDNT_FIT                     : constant := 275;
   ERROR_EA_FILE_CORRUPT                   : constant := 276;
   ERROR_EA_TABLE_FULL                     : constant := 277;
   ERROR_INVALID_EA_HANDLE                 : constant := 278;
   ERROR_EAS_NOT_SUPPORTED                 : constant := 282;
   ERROR_NOT_OWNER                         : constant := 288;
   ERROR_TOO_MANY_POSTS                    : constant := 298;
   ERROR_PARTIAL_COPY                      : constant := 299;
   ERROR_MR_MID_NOT_FOUND                  : constant := 317;
   ERROR_INVALID_ADDRESS                   : constant := 487;
   ERROR_ARITHMETIC_OVERFLOW               : constant := 534;
   ERROR_PIPE_CONNECTED                    : constant := 535;
   ERROR_PIPE_LISTENING                    : constant := 536;
   ERROR_EA_ACCESS_DENIED                  : constant := 994;
   ERROR_OPERATION_ABORTED                 : constant := 995;
   ERROR_IO_INCOMPLETE                     : constant := 996;
   ERROR_IO_PENDING                        : constant := 997;
   ERROR_NOACCESS                          : constant := 998;
   ERROR_SWAPERROR                         : constant := 999;
   ERROR_STACK_OVERFLOW                    : constant := 1001;
   ERROR_INVALID_MESSAGE                   : constant := 1002;
   ERROR_CAN_NOT_COMPLETE                  : constant := 1003;
   ERROR_INVALID_FLAGS                     : constant := 1004;
   ERROR_UNRECOGNIZED_VOLUME               : constant := 1005;
   ERROR_FILE_INVALID                      : constant := 1006;
   ERROR_FULLSCREEN_MODE                   : constant := 1007;
   ERROR_NO_TOKEN                          : constant := 1008;
   ERROR_BADDB                             : constant := 1009;
   ERROR_BADKEY                            : constant := 1010;
   ERROR_CANTOPEN                          : constant := 1011;
   ERROR_CANTREAD                          : constant := 1012;
   ERROR_CANTWRITE                         : constant := 1013;
   ERROR_REGISTRY_RECOVERED                : constant := 1014;
   ERROR_REGISTRY_CORRUPT                  : constant := 1015;
   ERROR_REGISTRY_IO_FAILED                : constant := 1016;
   ERROR_NOT_REGISTRY_FILE                 : constant := 1017;
   ERROR_KEY_DELETED                       : constant := 1018;
   ERROR_NO_LOG_SPACE                      : constant := 1019;
   ERROR_KEY_HAS_CHILDREN                  : constant := 1020;
   ERROR_CHILD_MUST_BE_VOLATILE            : constant := 1021;
   ERROR_NOTIFY_ENUM_DIR                   : constant := 1022;
   ERROR_DEPENDENT_SERVICES_RUNNING        : constant := 1051;
   ERROR_INVALID_SERVICE_CONTROL           : constant := 1052;
   ERROR_SERVICE_REQUEST_TIMEOUT           : constant := 1053;
   ERROR_SERVICE_NO_THREAD                 : constant := 1054;
   ERROR_SERVICE_DATABASE_LOCKED           : constant := 1055;
   ERROR_SERVICE_ALREADY_RUNNING           : constant := 1056;
   ERROR_INVALID_SERVICE_ACCOUNT           : constant := 1057;
   ERROR_SERVICE_DISABLED                  : constant := 1058;
   ERROR_CIRCULAR_DEPENDENCY               : constant := 1059;
   ERROR_SERVICE_DOES_NOT_EXIST            : constant := 1060;
   ERROR_SERVICE_CANNOT_ACCEPT_CTRL        : constant := 1061;
   ERROR_SERVICE_NOT_ACTIVE                : constant := 1062;
   ERROR_FAILED_SERVICE_CONTROLLER_CONNECT : constant := 1063;
   ERROR_EXCEPTION_IN_SERVICE              : constant := 1064;
   ERROR_DATABASE_DOES_NOT_EXIST           : constant := 1065;
   ERROR_SERVICE_SPECIFIC_ERROR            : constant := 1066;
   ERROR_PROCESS_ABORTED                   : constant := 1067;
   ERROR_SERVICE_DEPENDENCY_FAIL           : constant := 1068;
   ERROR_SERVICE_LOGON_FAILED              : constant := 1069;
   ERROR_SERVICE_START_HANG                : constant := 1070;
   ERROR_INVALID_SERVICE_LOCK              : constant := 1071;
   ERROR_SERVICE_MARKED_FOR_DELETE         : constant := 1072;
   ERROR_SERVICE_EXISTS                    : constant := 1073;
   ERROR_ALREADY_RUNNING_LKG               : constant := 1074;
   ERROR_SERVICE_DEPENDENCY_DELETED        : constant := 1075;
   ERROR_BOOT_ALREADY_ACCEPTED             : constant := 1076;
   ERROR_SERVICE_NEVER_STARTED             : constant := 1077;
   ERROR_DUPLICATE_SERVICE_NAME            : constant := 1078;
   ERROR_END_OF_MEDIA                      : constant := 1100;
   ERROR_FILEMARK_DETECTED                 : constant := 1101;
   ERROR_BEGINNING_OF_MEDIA                : constant := 1102;
   ERROR_SETMARK_DETECTED                  : constant := 1103;
   ERROR_NO_DATA_DETECTED                  : constant := 1104;
   ERROR_PARTITION_FAILURE                 : constant := 1105;
   ERROR_INVALID_BLOCK_LENGTH              : constant := 1106;
   ERROR_DEVICE_NOT_PARTITIONED            : constant := 1107;
   ERROR_UNABLE_TO_LOCK_MEDIA              : constant := 1108;
   ERROR_UNABLE_TO_UNLOAD_MEDIA            : constant := 1109;
   ERROR_MEDIA_CHANGED                     : constant := 1110;
   ERROR_BUS_RESET                         : constant := 1111;
   ERROR_NO_MEDIA_IN_DRIVE                 : constant := 1112;
   ERROR_NO_UNICODE_TRANSLATION            : constant := 1113;
   ERROR_DLL_INIT_FAILED                   : constant := 1114;
   ERROR_SHUTDOWN_IN_PROGRESS              : constant := 1115;
   ERROR_NO_SHUTDOWN_IN_PROGRESS           : constant := 1116;
   ERROR_IO_DEVICE                         : constant := 1117;
   ERROR_SERIAL_NO_DEVICE                  : constant := 1118;
   ERROR_IRQ_BUSY                          : constant := 1119;
   ERROR_MORE_WRITES                       : constant := 1120;
   ERROR_COUNTER_TIMEOUT                   : constant := 1121;
   ERROR_FLOPPY_ID_MARK_NOT_FOUND          : constant := 1122;
   ERROR_FLOPPY_WRONG_CYLINDER             : constant := 1123;
   ERROR_FLOPPY_UNKNOWN_ERROR              : constant := 1124;
   ERROR_FLOPPY_BAD_REGISTERS              : constant := 1125;
   ERROR_DISK_RECALIBRATE_FAILED           : constant := 1126;
   ERROR_DISK_OPERATION_FAILED             : constant := 1127;
   ERROR_DISK_RESET_FAILED                 : constant := 1128;
   ERROR_EOM_OVERFLOW                      : constant := 1129;
   ERROR_NOT_ENOUGH_SERVER_MEMORY          : constant := 1130;
   ERROR_POSSIBLE_DEADLOCK                 : constant := 1131;
   ERROR_MAPPED_ALIGNMENT                  : constant := 1132;
   ERROR_BAD_USERNAME                      : constant := 2202;
   ERROR_NOT_CONNECTED                     : constant := 2250;
   ERROR_OPEN_FILES                        : constant := 2401;
   ERROR_ACTIVE_CONNECTIONS                : constant := 2402;
   ERROR_DEVICE_IN_USE                     : constant := 2404;
   ERROR_BAD_DEVICE                        : constant := 1200;
   ERROR_CONNECTION_UNAVAIL                : constant := 1201;
   ERROR_DEVICE_ALREADY_REMEMBERED         : constant := 1202;
   ERROR_NO_NET_OR_BAD_PATH                : constant := 1203;
   ERROR_BAD_PROVIDER                      : constant := 1204;
   ERROR_CANNOT_OPEN_PROFILE               : constant := 1205;
   ERROR_BAD_PROFILE                       : constant := 1206;
   ERROR_NOT_CONTAINER                     : constant := 1207;
   ERROR_EXTENDED_ERROR                    : constant := 1208;
   ERROR_INVALID_GROUPNAME                 : constant := 1209;
   ERROR_INVALID_COMPUTERNAME              : constant := 1210;
   ERROR_INVALID_EVENTNAME                 : constant := 1211;
   ERROR_INVALID_DOMAINNAME                : constant := 1212;
   ERROR_INVALID_SERVICENAME               : constant := 1213;
   ERROR_INVALID_NETNAME                   : constant := 1214;
   ERROR_INVALID_SHARENAME                 : constant := 1215;
   ERROR_INVALID_PASSWORDNAME              : constant := 1216;
   ERROR_INVALID_MESSAGENAME               : constant := 1217;
   ERROR_INVALID_MESSAGEDEST               : constant := 1218;
   ERROR_SESSION_CREDENTIAL_CONFLICT       : constant := 1219;
   ERROR_REMOTE_SESSION_LIMIT_EXCEEDED     : constant := 1220;
   ERROR_DUP_DOMAINNAME                    : constant := 1221;
   ERROR_NO_NETWORK                        : constant := 1222;
   ERROR_CANCELLED                         : constant := 1223;
   ERROR_USER_MAPPED_FILE                  : constant := 1224;
   ERROR_CONNECTION_REFUSED                : constant := 1225;
   ERROR_GRACEFUL_DISCONNECT               : constant := 1226;
   ERROR_ADDRESS_ALREADY_ASSOCIATED        : constant := 1227;
   ERROR_ADDRESS_NOT_ASSOCIATED            : constant := 1228;
   ERROR_CONNECTION_INVALID                : constant := 1229;
   ERROR_CONNECTION_ACTIVE                 : constant := 1230;
   ERROR_NETWORK_UNREACHABLE               : constant := 1231;
   ERROR_HOST_UNREACHABLE                  : constant := 1232;
   ERROR_PROTOCOL_UNREACHABLE              : constant := 1233;
   ERROR_PORT_UNREACHABLE                  : constant := 1234;
   ERROR_REQUEST_ABORTED                   : constant := 1235;
   ERROR_CONNECTION_ABORTED                : constant := 1236;
   ERROR_RETRY                             : constant := 1237;
   ERROR_CONNECTION_COUNT_LIMIT            : constant := 1238;
   ERROR_LOGIN_TIME_RESTRICTION            : constant := 1239;
   ERROR_LOGIN_WKSTA_RESTRICTION           : constant := 1240;
   ERROR_INCORRECT_ADDRESS                 : constant := 1241;
   ERROR_ALREADY_REGISTERED                : constant := 1242;
   ERROR_SERVICE_NOT_FOUND                 : constant := 1243;
   ERROR_NOT_ALL_ASSIGNED                  : constant := 1300;
   ERROR_SOME_NOT_MAPPED                   : constant := 1301;
   ERROR_NO_QUOTAS_FOR_ACCOUNT             : constant := 1302;
   ERROR_LOCAL_USER_SESSION_KEY            : constant := 1303;
   ERROR_NULL_LM_PASSWORD                  : constant := 1304;
   ERROR_UNKNOWN_REVISION                  : constant := 1305;
   ERROR_REVISION_MISMATCH                 : constant := 1306;
   ERROR_INVALID_OWNER                     : constant := 1307;
   ERROR_INVALID_PRIMARY_GROUP             : constant := 1308;
   ERROR_NO_IMPERSONATION_TOKEN            : constant := 1309;
   ERROR_CANT_DISABLE_MANDATORY            : constant := 1310;
   ERROR_NO_LOGON_SERVERS                  : constant := 1311;
   ERROR_NO_SUCH_LOGON_SESSION             : constant := 1312;
   ERROR_NO_SUCH_PRIVILEGE                 : constant := 1313;
   ERROR_PRIVILEGE_NOT_HELD                : constant := 1314;
   ERROR_INVALID_ACCOUNT_NAME              : constant := 1315;
   ERROR_USER_EXISTS                       : constant := 1316;
   ERROR_NO_SUCH_USER                      : constant := 1317;
   ERROR_GROUP_EXISTS                      : constant := 1318;
   ERROR_NO_SUCH_GROUP                     : constant := 1319;
   ERROR_MEMBER_IN_GROUP                   : constant := 1320;
   ERROR_MEMBER_NOT_IN_GROUP               : constant := 1321;
   ERROR_LAST_ADMIN                        : constant := 1322;
   ERROR_WRONG_PASSWORD                    : constant := 1323;
   ERROR_ILL_FORMED_PASSWORD               : constant := 1324;
   ERROR_PASSWORD_RESTRICTION              : constant := 1325;
   ERROR_LOGON_FAILURE                     : constant := 1326;
   ERROR_ACCOUNT_RESTRICTION               : constant := 1327;
   ERROR_INVALID_LOGON_HOURS               : constant := 1328;
   ERROR_INVALID_WORKSTATION               : constant := 1329;
   ERROR_PASSWORD_EXPIRED                  : constant := 1330;
   ERROR_ACCOUNT_DISABLED                  : constant := 1331;
   ERROR_NONE_MAPPED                       : constant := 1332;
   ERROR_TOO_MANY_LUIDS_REQUESTED          : constant := 1333;
   ERROR_LUIDS_EXHAUSTED                   : constant := 1334;
   ERROR_INVALID_SUB_AUTHORITY             : constant := 1335;
   ERROR_INVALID_ACL                       : constant := 1336;
   ERROR_INVALID_SID                       : constant := 1337;
   ERROR_INVALID_SECURITY_DESCR            : constant := 1338;
   ERROR_BAD_INHERITANCE_ACL               : constant := 1340;
   ERROR_SERVER_DISABLED                   : constant := 1341;
   ERROR_SERVER_NOT_DISABLED               : constant := 1342;
   ERROR_INVALID_ID_AUTHORITY              : constant := 1343;
   ERROR_ALLOTTED_SPACE_EXCEEDED           : constant := 1344;
   ERROR_INVALID_GROUP_ATTRIBUTES          : constant := 1345;
   ERROR_BAD_IMPERSONATION_LEVEL           : constant := 1346;
   ERROR_CANT_OPEN_ANONYMOUS               : constant := 1347;
   ERROR_BAD_VALIDATION_CLASS              : constant := 1348;
   ERROR_BAD_TOKEN_TYPE                    : constant := 1349;
   ERROR_NO_SECURITY_ON_OBJECT             : constant := 1350;
   ERROR_CANT_ACCESS_DOMAIN_INFO           : constant := 1351;
   ERROR_INVALID_SERVER_STATE              : constant := 1352;
   ERROR_INVALID_DOMAIN_STATE              : constant := 1353;
   ERROR_INVALID_DOMAIN_ROLE               : constant := 1354;
   ERROR_NO_SUCH_DOMAIN                    : constant := 1355;
   ERROR_DOMAIN_EXISTS                     : constant := 1356;
   ERROR_DOMAIN_LIMIT_EXCEEDED             : constant := 1357;
   ERROR_INTERNAL_DB_CORRUPTION            : constant := 1358;
   ERROR_INTERNAL_ERROR                    : constant := 1359;
   ERROR_GENERIC_NOT_MAPPED                : constant := 1360;
   ERROR_BAD_DESCRIPTOR_FORMAT             : constant := 1361;
   ERROR_NOT_LOGON_PROCESS                 : constant := 1362;
   ERROR_LOGON_SESSION_EXISTS              : constant := 1363;
   ERROR_NO_SUCH_PACKAGE                   : constant := 1364;
   ERROR_BAD_LOGON_SESSION_STATE           : constant := 1365;
   ERROR_LOGON_SESSION_COLLISION           : constant := 1366;
   ERROR_INVALID_LOGON_TYPE                : constant := 1367;
   ERROR_CANNOT_IMPERSONATE                : constant := 1368;
   ERROR_RXACT_INVALID_STATE               : constant := 1369;
   ERROR_RXACT_COMMIT_FAILURE              : constant := 1370;
   ERROR_SPECIAL_ACCOUNT                   : constant := 1371;
   ERROR_SPECIAL_GROUP                     : constant := 1372;
   ERROR_SPECIAL_USER                      : constant := 1373;
   ERROR_MEMBERS_PRIMARY_GROUP             : constant := 1374;
   ERROR_TOKEN_ALREADY_IN_USE              : constant := 1375;
   ERROR_NO_SUCH_ALIAS                     : constant := 1376;
   ERROR_MEMBER_NOT_IN_ALIAS               : constant := 1377;
   ERROR_MEMBER_IN_ALIAS                   : constant := 1378;
   ERROR_ALIAS_EXISTS                      : constant := 1379;
   ERROR_LOGON_NOT_GRANTED                 : constant := 1380;
   ERROR_TOO_MANY_SECRETS                  : constant := 1381;
   ERROR_SECRET_TOO_LONG                   : constant := 1382;
   ERROR_INTERNAL_DB_ERROR                 : constant := 1383;
   ERROR_TOO_MANY_CONTEXT_IDS              : constant := 1384;
   ERROR_LOGON_TYPE_NOT_GRANTED            : constant := 1385;
   ERROR_NT_CROSS_ENCRYPTION_REQUIRED      : constant := 1386;
   ERROR_NO_SUCH_MEMBER                    : constant := 1387;
   ERROR_INVALID_MEMBER                    : constant := 1388;
   ERROR_TOO_MANY_SIDS                     : constant := 1389;
   ERROR_LM_CROSS_ENCRYPTION_REQUIRED      : constant := 1390;
   ERROR_NO_INHERITANCE                    : constant := 1391;
   ERROR_FILE_CORRUPT                      : constant := 1392;
   ERROR_DISK_CORRUPT                      : constant := 1393;
   ERROR_NO_USER_SESSION_KEY               : constant := 1394;
   ERROR_INVALID_WINDOW_HANDLE             : constant := 1400;
   ERROR_INVALID_MENU_HANDLE               : constant := 1401;
   ERROR_INVALID_CURSOR_HANDLE             : constant := 1402;
   ERROR_INVALID_ACCEL_HANDLE              : constant := 1403;
   ERROR_INVALID_HOOK_HANDLE               : constant := 1404;
   ERROR_INVALID_DWP_HANDLE                : constant := 1405;
   ERROR_TLW_WITH_WSCHILD                  : constant := 1406;
   ERROR_CANNOT_FIND_WND_CLASS             : constant := 1407;
   ERROR_WINDOW_OF_OTHER_THREAD            : constant := 1408;
   ERROR_HOTKEY_ALREADY_REGISTERED         : constant := 1409;
   ERROR_CLASS_ALREADY_EXISTS              : constant := 1410;
   ERROR_CLASS_DOES_NOT_EXIST              : constant := 1411;
   ERROR_CLASS_HAS_WINDOWS                 : constant := 1412;
   ERROR_INVALID_INDEX                     : constant := 1413;
   ERROR_INVALID_ICON_HANDLE               : constant := 1414;
   ERROR_PRIVATE_DIALOG_INDEX              : constant := 1415;
   ERROR_LISTBOX_ID_NOT_FOUND              : constant := 1416;
   ERROR_NO_WILDCARD_CHARACTERS            : constant := 1417;
   ERROR_CLIPBOARD_NOT_OPEN                : constant := 1418;
   ERROR_HOTKEY_NOT_REGISTERED             : constant := 1419;
   ERROR_WINDOW_NOT_DIALOG                 : constant := 1420;
   ERROR_CONTROL_ID_NOT_FOUND              : constant := 1421;
   ERROR_INVALID_COMBOBOX_MESSAGE          : constant := 1422;
   ERROR_WINDOW_NOT_COMBOBOX               : constant := 1423;
   ERROR_INVALID_EDIT_HEIGHT               : constant := 1424;
   ERROR_DC_NOT_FOUND                      : constant := 1425;
   ERROR_INVALID_HOOK_FILTER               : constant := 1426;
   ERROR_INVALID_FILTER_PROC               : constant := 1427;
   ERROR_HOOK_NEEDS_HMOD                   : constant := 1428;
   ERROR_GLOBAL_ONLY_HOOK                  : constant := 1429;
   ERROR_JOURNAL_HOOK_SET                  : constant := 1430;
   ERROR_HOOK_NOT_INSTALLED                : constant := 1431;
   ERROR_INVALID_LB_MESSAGE                : constant := 1432;
   ERROR_SETCOUNT_ON_BAD_LB                : constant := 1433;
   ERROR_LB_WITHOUT_TABSTOPS               : constant := 1434;
   ERROR_DESTROY_OBJECT_OF_OTHER_THREAD    : constant := 1435;
   ERROR_CHILD_WINDOW_MENU                 : constant := 1436;
   ERROR_NO_SYSTEM_MENU                    : constant := 1437;
   ERROR_INVALID_MSGBOX_STYLE              : constant := 1438;
   ERROR_INVALID_SPI_VALUE                 : constant := 1439;
   ERROR_SCREEN_ALREADY_LOCKED             : constant := 1440;
   ERROR_HWNDS_HAVE_DIFF_PARENT            : constant := 1441;
   ERROR_NOT_CHILD_WINDOW                  : constant := 1442;
   ERROR_INVALID_GW_COMMAND                : constant := 1443;
   ERROR_INVALID_THREAD_ID                 : constant := 1444;
   ERROR_NON_MDICHILD_WINDOW               : constant := 1445;
   ERROR_POPUP_ALREADY_ACTIVE              : constant := 1446;
   ERROR_NO_SCROLLBARS                     : constant := 1447;
   ERROR_INVALID_SCROLLBAR_RANGE           : constant := 1448;
   ERROR_INVALID_SHOWWIN_COMMAND           : constant := 1449;
   ERROR_EVENTLOG_FILE_CORRUPT             : constant := 1500;
   ERROR_EVENTLOG_CANT_START               : constant := 1501;
   ERROR_LOG_FILE_FULL                     : constant := 1502;
   ERROR_EVENTLOG_FILE_CHANGED             : constant := 1503;

private
   pragma Import (Stdcall, CloseHandle,         "CloseHandle");
   pragma Import (Stdcall, CreateEvent,         "CreateEventA");
   pragma Import (Stdcall, CreateFileMapping,   "CreateFileMappingA");
   pragma Import (Stdcall, CreateMutex,         "CreateMutexA");
   pragma Import (Stdcall, DuplicateHandle,     "DuplicateHandle");
   pragma Import (Stdcall, GetCurrentProcess,   "GetCurrentProcess");
   pragma Import (Stdcall, GetCurrentProcessId, "GetCurrentProcessId");
   pragma Import (Stdcall, GetLastError,        "GetLastError");
   pragma Import (Stdcall, MapViewOfFile,       "MapViewOfFile");
   pragma Import (Stdcall, OpenFileMapping,     "OpenFileMappingA");
   pragma Import (Stdcall, OpenProcess,         "OpenProcess");
   pragma Import (Stdcall, PulseEvent,          "PulseEvent");
   pragma Import (Stdcall, ReleaseMutex,        "ReleaseMutex");
   pragma Import (Stdcall, ResetEvent,          "ResetEvent");
   pragma Import (Stdcall, SetEvent,            "SetEvent");
   pragma Import (Stdcall, WaitForSingleObject, "WaitForSingleObject");
   pragma Import (Stdcall, UnmapViewOfFile,     "UnmapViewOfFile");

end Synchronization.Windows;
