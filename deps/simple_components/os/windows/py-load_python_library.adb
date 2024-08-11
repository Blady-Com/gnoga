--                                                                    --
--  package Py.Load_Python_Library  Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2018       --
--                                                                    --
--                                Last revision :  14:35 02 Jul 2024  --
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

with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Strings_Edit;                use Strings_Edit;
with Strings_Edit.Integers;       use Strings_Edit.Integers;
with Strings_Edit.Quoted;         use Strings_Edit.Quoted;
with Strings_Edit.UTF8.Handling;  use Strings_Edit.UTF8.Handling;
with System.Storage_Elements;     use System.Storage_Elements;

with Ada.Streams.Stream_IO;
with Interfaces;

package body Py.Load_Python_Library is

   type BOOL     is new int;
   type DWORD    is new unsigned_long;
   type FILETIME is array (1..8) of unsigned_char;
   pragma Convention (C, FILETIME);

   LOAD_WITH_ALTERED_SEARCH_PATH       : constant := 16#00000008#;
   LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR    : constant := 16#00000100#;
   LOAD_LIBRARY_SEARCH_APPLICATION_DIR : constant := 16#00000200#;
   LOAD_LIBRARY_SEARCH_DEFAULT_DIRS    : constant := 16#00001000#;

   ERROR_PROC_NOT_FOUND : constant := 127;
   ERROR_BAD_EXE_FORMAT : constant := 193;

   REG_OPTION_RESERVED       : constant DWORD := 16#00000000#;
   REG_OPTION_NON_VOLATILE   : constant DWORD := 16#00000000#;
   REG_OPTION_VOLATILE       : constant DWORD := 16#00000001#;
   REG_OPTION_CREATE_LINK    : constant DWORD := 16#00000002#;
   REG_OPTION_BACKUP_RESTORE : constant DWORD := 16#00000004#;
   REG_OPTION_OPEN_LINK      : constant DWORD := 16#00000008#;

   type HKEY is new System.Address;
   type ACCESS_MASK is new DWORD;

   DELETE                   : constant ACCESS_MASK := 16#00010000#;
   READ_CONTROL             : constant ACCESS_MASK := 16#00020000#;
   WRITE_DAC                : constant ACCESS_MASK := 16#00040000#;
   WRITE_OWNER              : constant ACCESS_MASK := 16#00080000#;
   SYNCHRONIZE              : constant ACCESS_MASK := 16#00100000#;

   STANDARD_RIGHTS_REQUIRED : constant ACCESS_MASK := 16#000F0000#;

   STANDARD_RIGHTS_READ     : constant ACCESS_MASK := READ_CONTROL;
   STANDARD_RIGHTS_WRITE    : constant ACCESS_MASK := READ_CONTROL;
   STANDARD_RIGHTS_EXECUTE  : constant ACCESS_MASK := READ_CONTROL;

   STANDARD_RIGHTS_ALL      : constant ACCESS_MASK := 16#001F0000#;
   SPECIFIC_RIGHTS_ALL      : constant ACCESS_MASK := 16#0000FFFF#;

   KEY_QUERY_VALUE          : constant ACCESS_MASK := 16#0001#;
   KEY_SET_VALUE            : constant ACCESS_MASK := 16#0002#;
   KEY_CREATE_SUB_KEY       : constant ACCESS_MASK := 16#0004#;
   KEY_ENUMERATE_SUB_KEYS   : constant ACCESS_MASK := 16#0008#;
   KEY_NOTIFY               : constant ACCESS_MASK := 16#0010#;
   KEY_CREATE_LINK          : constant ACCESS_MASK := 16#0020#;
   KEY_WOW64_32KEY          : constant ACCESS_MASK := 16#0200#;
   KEY_WOW64_64KEY          : constant ACCESS_MASK := 16#0100#;
   KEY_WOW64_RES            : constant ACCESS_MASK := 16#0300#;

   KEY_READ       : constant ACCESS_MASK := (  (  STANDARD_RIGHTS_READ
                                               or KEY_QUERY_VALUE
                                               or KEY_ENUMERATE_SUB_KEYS
                                               or KEY_NOTIFY
                                               )
                                            and
                                              not SYNCHRONIZE
                                            );
   KEY_WRITE      : constant ACCESS_MASK := (  (  STANDARD_RIGHTS_WRITE
                                               or KEY_SET_VALUE
                                               or KEY_CREATE_SUB_KEY
                                               )
                                            and
                                              not SYNCHRONIZE
                                            );
   KEY_EXECUTE    : constant ACCESS_MASK := (  KEY_READ
                                            and
                                               not SYNCHRONIZE
                                            );
   KEY_ALL_ACCESS : constant ACCESS_MASK := (  (  STANDARD_RIGHTS_ALL
                                               or KEY_QUERY_VALUE
                                               or KEY_SET_VALUE
                                               or KEY_CREATE_SUB_KEY
                                               or KEY_ENUMERATE_SUB_KEYS
                                               or KEY_NOTIFY
                                               or KEY_CREATE_LINK
                                               )
                                            and
                                               not SYNCHRONIZE
                                            );

   HKEY_CURRENT_USER  : constant HKEY :=
                                       HKEY (To_Address (16#80000001#));
   HKEY_LOCAL_MACHINE : constant HKEY :=
                                       HKEY (To_Address (16#80000002#));

   REG_NONE                       : constant := 0;
   REG_SZ                         : constant := 1;
   REG_EXPAND_SZ                  : constant := 2;
   REG_BINARY                     : constant := 3;
   REG_DWORD                      : constant := 4;
   REG_DWORD_LITTLE_ENDIAN        : constant := 4;
   REG_DWORD_BIG_ENDIAN           : constant := 5;
   REG_LINK                       : constant := 6;
   REG_MULTI_SZ                   : constant := 7;
   REG_RESOURCE_LIST              : constant := 8;
   REG_FULL_RESOURCE_DESCRIPTOR   : constant := 9;
   REG_RESOURCE_REQUIREMENTS_LIST : constant := 10;
   REG_QWORD                      : constant := 11;
   REG_QWORD_LITTLE_ENDIAN        : constant := 11;

   function RegCloseKey (Key : HKEY) return Long;
   pragma Import (Stdcall, RegCloseKey, "RegCloseKey");

   type SECURITY_ATTRIBUTES is record
      Length              : DWORD;
      Security_Descriptor : System.Address;
      Inherit_Handle      : BOOL;
   end record;
   pragma Convention (C_Pass_By_Copy, SECURITY_ATTRIBUTES);

   function RegCreateKeyExA
            (  Key         : HKEY;
               SubKey      : char_array;
               Reserved    : DWORD := 0;
               Class       : char_array  := (0 => NUL);
               Options     : DWORD := REG_OPTION_NON_VOLATILE;
               Desired     : ACCESS_MASK := KEY_ALL_ACCESS;
               Security    : access SECURITY_ATTRIBUTES := null;
               Result      : access HKEY;
               Disposition : access DWORD
            )  return LONG;
   pragma Import (Stdcall, RegCreateKeyExA, "RegCreateKeyExA");

   function RegEnumKeyExA
            (  Key        : HKEY;
               Index      : DWORD;
               Name       : access char;
               Length     : access DWORD;
               Reserved   : access DWORD    := null;
               Class      : access char     := null;
               Class_Size : access DWORD    := null;
               Write_Time : access FILETIME := null
            )  return LONG;
   pragma Import (Stdcall, RegEnumKeyExA, "RegEnumKeyExA");

   function RegEnumValueA
            (  Key        : HKEY;
               Index      : DWORD;
               Name       : access char;
               Length     : access DWORD;
               Reserved   : access DWORD := null;
               Value_Type : access DWORD := null;
               Data       : System.Address     := System.Null_Address;
               Size       : access DWORD := null
            )  return LONG;
   pragma Import (Stdcall, RegEnumValueA, "RegEnumValueA");

   function RegOpenKeyExA
            (  Key     : HKEY;
               SubKey  : char_array;
               Options : DWORD := REG_OPTION_NON_VOLATILE;
               Desired : ACCESS_MASK := KEY_READ;
               Result  : access HKEY
            )  return LONG;
   pragma Import (Stdcall, RegOpenKeyExA, "RegOpenKeyExA");

   function RegQueryValueExA
            (  Key        : HKEY;
               Name       : char_array;
               Reserved   : DWORD        := 0;
               Value_Type : access DWORD := null;
               Data       : System.Address     := System.Null_Address;
               Size       : access DWORD := null
            )  return LONG;
   function RegQueryValueExA
            (  Key        : HKEY;
               Name       : System.Address     := System.Null_Address;
               Reserved   : DWORD        := 0;
               Value_Type : access DWORD := null;
               Data       : System.Address     := System.Null_Address;
               Size       : access DWORD := null
            )  return LONG;
   pragma Import (Stdcall, RegQueryValueExA, "RegQueryValueExA");

   function LoadLibrary
            (  Name  : wchar_array;
               File  : Address := Null_Address;
               Flags : DWORD   := LOAD_WITH_ALTERED_SEARCH_PATH
            )  return Address;
   pragma Import (Stdcall, LoadLibrary, "LoadLibraryExW");

   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "Py_AddPendingCall" & Nul
            )  return AddPendingCall_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyBool_FromLong" & Nul
            )  return Bool_FromLLong_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyByteArray_AsString" & Nul
            )  return ByteArray_AsString_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :=
                                   "PyByteArray_FromStringAndSize" & Nul
            )  return ByteArray_FromStringAndSize_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyByteArray_Size" & Nul
            )  return ByteArray_Size_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyBytes_AsStringAndSize" & Nul
            )  return Bytes_AsStringAndSize_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyBytes_FromStringAndSize" & Nul
            )  return Bytes_FromStringAndSize_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyBytes_Size" & Nul
            )  return Bytes_Size_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyCallable_Check" & Nul
            )  return Callable_Check_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyCapsule_GetContext" & Nul
            )  return Capsule_GetContext_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyCapsule_GetDestructor" & Nul
            )  return Capsule_GetDestructor_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyCapsule_GetName" & Nul
            )  return Capsule_GetName_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyCapsule_GetPointer" & Nul
            )  return Capsule_GetPointer_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyCapsule_New" & Nul
            )  return Capsule_New_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyCapsule_Import" & Nul
            )  return Capsule_Import_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyCapsule_IsValid" & Nul
            )  return Capsule_IsValid_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyCapsule_SetContext" & Nul
            )  return Capsule_SetContext_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyCapsule_SetDestructor" & Nul
            )  return Capsule_SetDestructor_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyCapsule_SetName" & Nul
            )  return Capsule_SetName_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyCapsule_SetPointer" & Nul
            )  return Capsule_SetPointer_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "Py_CompileString" & Nul
            )  return CompileString_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "Py_DecRef" & Nul
            )  return DecRef_Ptr;
-- function GetProcAddress
--          (  Module : Address;
--             Name   : char_array := "PyDict_Check" & Nul
--          )  return Dict_Check_Ptr;
-- function GetProcAddress
--          (  Module : Address;
--             Name   : char_array := "PyDict_CheckExact" & Nul
--          )  return Dict_CheckExact_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_Clear" & Nul
            )  return Dict_Clear_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_Contains" & Nul
            )  return Dict_Contains_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_Copy" & Nul
            )  return Dict_Copy_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_DelItem" & Nul
            )  return Dict_DelItem_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_DelItemString" & Nul
            )  return Dict_DelItemString_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_GetItemString" & Nul
            )  return Dict_GetItemString_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_Keys" & Nul
            )  return Dict_Keys_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_Merge" & Nul
            )  return Dict_Merge_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_Next" & Nul
            )  return Dict_Next_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_Items" & Nul
            )  return Dict_Items_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_New" & Nul
            )  return Dict_New_Ptr;
-- function GetProcAddress
--          (  Module : Address;
--             Name   : char_array := "PyDict_SetDefault" & Nul
--          )  return Dict_SetDefault_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_SetItem" & Nul
            )  return Dict_SetItem_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_SetItemString" & Nul
            )  return Dict_SetItemString_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyDict_Size" & Nul
            )  return Dict_Size_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "Py_EndInterpreter" & Nul
            )  return EndInterpreter_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyErr_BadArgument" & Nul
            )  return Err_BadArgument_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyErr_BadInternalCall" & Nul
            )  return Err_BadInternalCall_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyErr_Clear" & Nul
            )  return Err_Clear_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyErr_Fetch" & Nul
            )  return Err_Fetch_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyErr_NewException" & Nul
            )  return Err_NewException_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyErr_NormalizeException" & Nul
            )  return Err_NormalizeException_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyErr_Occurred" & Nul
            )  return Err_Occurred_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyErr_Restore" & Nul
            )  return Err_Restore_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyErr_SetString" & Nul
            )  return Err_SetString_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyEval_InitThreads" & Nul
            )  return Eval_InitThreads_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyEval_RestoreThread" & Nul
            )  return Eval_RestoreThread_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyEval_SaveThread" & Nul
            )  return Eval_SaveThread_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "Py_FinalizeEx" & Nul
            )  return FinalizeEx_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyFloat_AsDouble" & Nul
            )  return Float_AsDouble_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyFloat_FromDouble" & Nul
            )  return Float_FromDouble_Ptr;
-- function GetProcAddress
--          (  Module : Address;
--             Name   : char_array := "PyGILState_Check" & Nul
--          )  return GILState_Check_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyGILState_Ensure" & Nul
            )  return GILState_Ensure_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyGILState_Release" & Nul
            )  return GILState_Release_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyImport_AddModule" & Nul
            )  return Import_AddModule_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyImport_AppendInittab" & Nul
            )  return Import_AppendInittab_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "Py_IncRef" & Nul
            )  return IncRef_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "Py_InitializeEx" & Nul
            )  return InitializeEx_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyImport_ExecCodeModuleEx" & Nul
            )  return Import_ExecCodeModuleEx_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyImport_GetModule" & Nul
            )  return Import_GetModule_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyImport_Import" & Nul
            )  return Import_Import_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyImport_ImportModule" & Nul
            )  return Import_ImportModule_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyIndex_Check" & Nul
            )  return Index_Check_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyIter_Check" & Nul
            )  return Iter_Check_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyIter_Next" & Nul
            )  return Iter_Next_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyList_Append" & Nul
            )  return List_Append_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyList_AsTuple" & Nul
            )  return List_AsTuple_Ptr;
-- function GetProcAddress
--          (  Module : Address;
--             Name   : char_array := "PyList_Check" & Nul
--          )  return List_Check_Ptr;
-- function GetProcAddress
--          (  Module : Address;
--             Name   : char_array := "PyList_CheckExact" & Nul
--          )  return List_CheckExact_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyList_GetItem" & Nul
            )  return List_GetItem_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyList_GetSlice" & Nul
            )  return List_GetSlice_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyList_Insert" & Nul
            )  return List_Insert_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyList_New" & Nul
            )  return List_New_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyList_Reverse" & Nul
            )  return List_Reverse_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyList_SetItem" & Nul
            )  return List_SetItem_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyList_SetSlice" & Nul
            )  return List_SetSlice_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyList_Size" & Nul
            )  return List_Size_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyList_Sort" & Nul
            )  return List_Sort_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyLong_AsLong" & Nul
            )  return Long_AsLong_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyLong_AsLongLong" & Nul
            )  return Long_AsLongLong_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyLong_AsUnsignedLong" & Nul
            )  return Long_AsUnsignedLong_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyLong_AsUnsignedLongLong" & Nul
            )  return Long_AsUnsignedLongLong_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyLong_FromLong" & Nul
            )  return Long_FromLong_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyLong_FromLongLong" & Nul
            )  return Long_FromLongLong_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyLong_FromUnsignedLong" & Nul
            )  return Long_FromUnsignedLong_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :=
                           "PyLong_FromUnsignedLongLong" & Nul
            )  return Long_FromUnsignedLongLong_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyModule_AddObject" & Nul
            )  return Module_AddObject_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyModule_Create2" & Nul
            )  return Module_Create_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyModule_GetDict" & Nul
            )  return Module_GetDict_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyModule_GetName" & Nul
            )  return Module_GetName_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "Py_NewInterpreter" & Nul
            )  return NewInterpreter_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Absolute" & Nul
            )  return Number_Absolute_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Add" & Nul
            )  return Number_Add_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_And" & Nul
            )  return Number_And_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_AsSsize_t" & Nul
            )  return Number_AsSsize_t_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Check" & Nul
            )  return Number_Check_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Divmod" & Nul
            )  return Number_Divmod_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Float" & Nul
            )  return Number_Float_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_FloorDivide" & Nul
            )  return Number_FloorDivide_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Index" & Nul
            )  return Number_Index_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceAdd" & Nul
            )  return Number_InPlaceAdd_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceAnd" & Nul
            )  return Number_InPlaceAnd_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceFloorDivide" & Nul
            )  return Number_InPlaceFloorDivide_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceLshift" & Nul
            )  return Number_InPlaceLshift_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceMatrixMultiply" & Nul
            )  return Number_InPlaceMatrixMultiply_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceMultiply" & Nul
            )  return Number_InPlaceMultiply_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceOr" & Nul
            )  return Number_InPlaceOr_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlacePower" & Nul
            )  return Number_InPlacePower_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceRemainder" & Nul
            )  return Number_InPlaceRemainder_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceRshift" & Nul
            )  return Number_InPlaceRshift_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceSubtract" & Nul
            )  return Number_InPlaceSubtract_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceTrueDivide" & Nul
            )  return Number_InPlaceTrueDivide_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceXor" & Nul
            )  return Number_InPlaceXor_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Invert" & Nul
            )  return Number_Invert_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Long" & Nul
            )  return Number_Long_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Lshift" & Nul
            )  return Number_Lshift_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_MatrixMultiply" & Nul
            )  return Number_MatrixMultiply_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Multiply" & Nul
            )  return Number_Multiply_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Negative" & Nul
            )  return Number_Negative_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Or" & Nul
            )  return Number_Or_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Positive" & Nul
            )  return Number_Positive_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Power" & Nul
            )  return Number_Power_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Remainder" & Nul
            )  return Number_Remainder_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Rshift" & Nul
            )  return Number_Rshift_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Subtract" & Nul
            )  return Number_Subtract_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_ToBase" & Nul
            )  return Number_ToBase_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_TrueDivide" & Nul
            )  return Number_TrueDivide_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :="PyNumber_Xor" & Nul
            )  return Number_Xor_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_Bytes" & Nul
            )  return Object_Bytes_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_Call" & Nul
            )  return Object_Call_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_CallNoArgs" & Nul
            )  return Object_CallNoArgs_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_CallObject" & Nul
            )  return Object_CallObject_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_DelItem" & Nul
            )  return Object_DelItem_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_Dir" & Nul
            )  return Object_Dir_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_GenericGetAttr" & Nul
            )  return Object_GenericGetAttr_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_GenericSetAttr" & Nul
            )  return Object_GenericSetAttr_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_GetAttr" & Nul
            )  return Object_GetAttr_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_GetAttrString" & Nul
            )  return Object_GetAttrString_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_GetItem" & Nul
            )  return Object_GetItem_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_GetIter" & Nul
            )  return Object_GetIter_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_HasAttr" & Nul
            )  return Object_HasAttr_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_HasAttrString" & Nul
            )  return Object_HasAttrString_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_IsInstance" & Nul
            )  return Object_IsInstance_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_IsSubclass" & Nul
            )  return Object_IsSubclass_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_RichCompareBool" & Nul
            )  return Object_RichCompareBool_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_SetAttr" & Nul
            )  return Object_SetAttr_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_SetAttrString" & Nul
            )  return Object_SetAttrString_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_SetItem" & Nul
            )  return Object_SetItem_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_Size" & Nul
            )  return Object_Size_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_Str" & Nul
            )  return Object_Str_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyObject_Type" & Nul
            )  return Object_Type_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_Check" & Nul
            )  return Sequence_Check_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_Concat" & Nul
            )  return Sequence_Concat_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_Contains" & Nul
            )  return Sequence_Contains_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_Count" & Nul
            )  return Sequence_Count_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_DelItem" & Nul
            )  return Sequence_DelItem_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_DelSlice" & Nul
            )  return Sequence_DelSlice_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_GetItem" & Nul
            )  return Sequence_GetItem_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_GetSlice" & Nul
            )  return Sequence_GetSlice_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_Index" & Nul
            )  return Sequence_Index_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_List" & Nul
            )  return Sequence_List_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_Repeat" & Nul
            )  return Sequence_Repeat_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_SetItem" & Nul
            )  return Sequence_SetItem_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_SetSlice" & Nul
            )  return Sequence_SetSlice_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_Size" & Nul
            )  return Sequence_Size_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySequence_Tuple" & Nul
            )  return Sequence_Tuple_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySet_Add" & Nul
            )  return Set_Add_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySet_Clear" & Nul
            )  return Set_Clear_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySet_Contains" & Nul
            )  return Set_Contains_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySet_Discard" & Nul
            )  return Set_Discard_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyFrozenSet_New" & Nul
            )  return Frozenset_New_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySet_New" & Nul
            )  return Set_New_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySet_Pop" & Nul
            )  return Set_Pop_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySet_Size" & Nul
            )  return Set_Size_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PySys_GetObject" & Nul
            )  return Sys_GetObject_Ptr;
-- function GetProcAddress
--          (  Module : Address;
--             Name   : char_array := "_PySys_GetSizeOf" & Nul
--          )  return Sys_GetSizeOf_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyThreadState_Get" & Nul
            )  return ThreadState_Get_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyThreadState_Swap" & Nul
            )  return ThreadState_Swap_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyTuple_GetItem" & Nul
            )  return Tuple_GetItem_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyTuple_GetSlice" & Nul
            )  return Tuple_GetSlice_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyTuple_New" & Nul
            )  return Tuple_New_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyTuple_SetItem" & Nul
            )  return Tuple_SetItem_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyTuple_Size" & Nul
            )  return Tuple_Size_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyType_FromSpec" & Nul
            )  return Type_FromSpec_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyType_FromSpecWithBases" & Nul
            )  return Type_FromSpecWithBases_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyType_GetSlot" & Nul
            )  return Type_GetSlot_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyType_IsSubtype" & Nul
            )  return Type_IsSubtype_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyUnicode_AsEncodedString" & Nul
            )  return Unicode_AsEncodedString_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyUnicode_DecodeFSDefault" & Nul
            )  return Unicode_DecodeFSDefault_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyUnicode_FromString" & Nul
            )  return Unicode_FromString_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :=
                                     "PyUnicode_FromStringAndSize" & Nul
            )  return Unicode_FromStringAndSize_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyUnicode_GetLength" & Nul
            )  return Unicode_GetLength_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "PyUnicode_ReadChar" & Nul
            )  return Unicode_ReadChar_Ptr;

   function GetProcAddress
            (  Module : Address;
               Name   : char_array
            )  return Object;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array
            )  return Object_Ptr;
   pragma Import (Stdcall, GetProcAddress, "GetProcAddress");

   function GetLastError return DWORD;
   pragma Import (Stdcall, GetLastError, "GetLastError");

   Library : Address := Null_Address;
   Loaded  : Boolean := False;

   function Get_Default_Name (Major : Natural := 7) return String is
   begin
      return "python3.dll";
   end Get_Default_Name;

   function Get_Extension return String is
   begin
      return "*.dll";
   end Get_Extension;

   function Is_Loaded return Boolean is
   begin
      return Loaded;
   end Is_Loaded;

   type Machine_Type is new Interfaces.Unsigned_16;
   MACHINE_UNKNOWN   : constant Machine_Type := 16#0000#;
   MACHINE_AM33      : constant Machine_Type := 16#01d3#;
   MACHINE_AMD64     : constant Machine_Type := 16#8664#;
   MACHINE_ARM       : constant Machine_Type := 16#01c0#;
   MACHINE_EBC       : constant Machine_Type := 16#0ebc#;
   MACHINE_I386      : constant Machine_Type := 16#014c#;
   MACHINE_IA64      : constant Machine_Type := 16#0200#;
   MACHINE_M32R      : constant Machine_Type := 16#9041#;
   MACHINE_MIPS16    : constant Machine_Type := 16#0266#;
   MACHINE_MIPSFPU   : constant Machine_Type := 16#0366#;
   MACHINE_MIPSFPU16 : constant Machine_Type := 16#0466#;
   MACHINE_POWERPC   : constant Machine_Type := 16#01f0#;
   MACHINE_POWERPCFP : constant Machine_Type := 16#01f1#;
   MACHINE_R4000     : constant Machine_Type := 16#0166#;
   MACHINE_SH3       : constant Machine_Type := 16#01a2#;
   MACHINE_SH3DSP    : constant Machine_Type := 16#01a3#;
   MACHINE_SH4       : constant Machine_Type := 16#01a6#;
   MACHINE_SH5       : constant Machine_Type := 16#01a8#;
   MACHINE_THUMB     : constant Machine_Type := 16#01c2#;
   MACHINE_WCEMIPSV2 : constant Machine_Type := 16#0169#;

   function Get_Machine (Path : String) return Machine_Type is
      use Ada.Streams.Stream_IO;
      use Interfaces;
      File   : File_Type;
      function Byte return Stream_Element is
         Data : Stream_Element_Array (1..1);
         Last : Stream_Element_Offset;
      begin
         Read (File, Data, Last);
         if Last = 1 then
            return Data (1);
         end if;
         raise Ada.Streams.Stream_IO.End_Error;
      end Byte;
      Offset : Unsigned_32;
      Result : Machine_Type := MACHINE_UNKNOWN;
   begin
      Open (File, In_File, Path, "b");
      begin
         Set_Index (File, 16#3c# + 1);
         Offset :=          Unsigned_32 (Byte);
         Offset := Offset + Unsigned_32 (Byte) * 2**8;
         Offset := Offset + Unsigned_32 (Byte) * 2**16;
         Offset := Offset + Unsigned_32 (Byte) * 2**24;
         Set_Index (File, Positive_Count (Offset) + 1);
         if (  Character'Pos ('P') = Byte
            and then
               Character'Pos ('E') = Byte
            and then
               0 = Byte
            and then
               0 = Byte
            )
         then
            Result :=          Machine_Type (Byte);
            Result := Result + Machine_Type (Byte) * 2**8;
         end if;
         Close (File);
      exception
         when others =>
            Close (File);
      end;
      return Result;
   exception
      when others =>
         Close (File);
         return Result;
   end Get_Machine;

   FORMAT_MESSAGE_ALLOCATE_BUFFER : constant := 16#0100#;
   FORMAT_MESSAGE_IGNORE_INSERTS  : constant := 16#0200#;
   FORMAT_MESSAGE_FROM_STRING     : constant := 16#0400#;
   FORMAT_MESSAGE_FROM_HMODULE    : constant := 16#0800#;
   FORMAT_MESSAGE_FROM_SYSTEM     : constant := 16#1000#;
   FORMAT_MESSAGE_ARGUMENT_ARRAY  : constant := 16#2000#;
   FORMAT_MESSAGE_MAX_WIDTH_MASK  : constant := 16#00FF#;

   function FormatMessage
            (  Flags      : DWORD := FORMAT_MESSAGE_FROM_SYSTEM or
                                     FORMAT_MESSAGE_IGNORE_INSERTS or
                                     FORMAT_MESSAGE_ARGUMENT_ARRAY;
               Source     : chars_ptr := Null_Ptr;
               MessageId  : DWORD;
               LanguageId : DWORD := 0;
               Buffer     : access char;
               Size       : DWORD;
               Arguments  : System.Address := Null_Address
            )  return DWORD;
   pragma Import (Stdcall, FormatMessage, "FormatMessageA");

   function GetErrorText (Error : DWORD) return String is
      Buffer : char_array (1..2048);
      Length : DWORD;
   begin
      Length := FormatMessage
                (  MessageId => Error,
                   Buffer    => Buffer (1)'Access,
                   Size      => Buffer'Length
                );
      declare
         Message : String (1..Natural (Length));
      begin
         for Index in 1..size_t (Length) loop
            Message (Positive (Index)) :=
               Character'Val (char'Pos (Buffer (Index)));
         end loop;
         for Index in reverse Message'Range loop
            case Message (Index) is
               when Character'Val (0)..' ' =>
                  null;
               when others =>
                  return Message (1..Index) &
                         " [" & Image (Integer (Error)) & "]";
            end case;
         end loop;
         return "[" & Image (Integer (Error)) & "]";
      end;
   end GetErrorText;

   procedure Raise_From_LastError
             (  ID     : Exception_ID;
                Prefix : String := "";
                Error  : DWORD
             )  is
   begin
      if Prefix'Length > 0 then
         Raise_Exception (ID, Prefix & GetErrorText (Error));
      else
         Raise_Exception (ID, GetErrorText (Error));
      end if;
   end Raise_From_LastError;

   function Get_Python_Path (Major : Natural := 7) return String is
      Folder : aliased HKEY;
      Result : LONG;
      function Search return String is
         Data       : char_array (1..1024);
         Length     : aliased DWORD;
         Value_Type : aliased DWORD;
      begin
         for Index in DWORD range 0..DWORD'Last loop
            Length := Data'Length;
            exit when 0 /= RegEnumKeyExA
                           (  Key    => Folder,
                              Index  => Index,
                              Name   => Data (1)'Access,
                              Length => Length'Access
                           );
            declare
               Name    : constant String := To_Ada (Data);
               Pointer : Integer := Name'First + 2;
               Value   : Integer;
            begin
               if Length > 2 and then Data (1..2) = "3." then
                  Get (Name, Pointer, Value);
                  if Value >= Major then
                     declare
                        Subkey : aliased HKEY;
                     begin
                        Result :=
                           RegOpenKeyExA
                           (  Key    => Folder,
                              Subkey => To_C (Name & "\InstallPath"),
                              Result => Subkey'Access
                           );
                        if Result = 0 then
                           Length := Data'Length;
                           Result :=
                              RegQueryValueExA
                              (  Key        => Subkey,
                                 Value_Type => Value_Type'Access,
                                 Data       => Data'Address,
                                 Size       => Length'Access
                              );
                           Result := RegCloseKey (Subkey);
                           if Value_Type = REG_SZ then
                              return To_Ada (Data);
                           end if;
                        else
                           Result := RegCloseKey (Subkey);
                        end if;
                     end;
                  end if;
               end if;
            exception
               when others =>
                  null;
            end;
         end loop;
         Result := RegCloseKey (Folder);
         return "";
      end Search;
   begin
      Result := RegOpenKeyExA
                (  Key    => HKEY_CURRENT_USER,
                   SubKey => "Software\Python\PythonCore" & NUL,
                   Result => Folder'Access
                );
      if Result = 0 then
         declare
            Path : constant String := Search;
         begin
            if Path'Length > 0 then
               return Path;
            end if;
         end;
      end if;
      Result := RegOpenKeyExA
                (  Key    => HKEY_LOCAL_MACHINE,
                   SubKey => "Software\Python\PythonCore" & NUL,
                   Result => Folder'Access
                );
      if Result = 0 then
         declare
            Path : constant String := Search;
         begin
            if Path'Length > 0 then
               return Path;
            end if;
         end;
      end if;
      return "";
   end Get_Python_Path;

   procedure Load (Name : String := "") is
      Error : DWORD;
      function Get_Library_Path return String is
      begin
         if Name'Length = 0 then
            return Get_Python_Path & Get_Default_Name;
         else
            return Trim (Name);
         end if;
      end Get_Library_Path;
   begin
      if Loaded then
         return;
      end if;
      declare
         use Interfaces;
         Name  : constant String := Get_Library_Path;
      begin
         if Library = Null_Address then
            Library := LoadLibrary (To_C (To_Wide_String (Name)));
            if Library = Null_Address then
               Error := GetLastError;
               if Error = ERROR_BAD_EXE_FORMAT then
                  case Get_Machine (Name) is
                     when MACHINE_AMD64 =>
                        if Address'Size = 32 then
                           Raise_Exception
                           (  Use_Error'Identity,
                              (  "The DLL "
                              &  Quote (Name)
                              &  " is for AMD64 64-bit Windows,"
                              &  " use a 32-bit Python version instead"
                           )  );
                        end if;
                     when MACHINE_I386 =>
                        if Address'Size = 64 then
                           Raise_Exception
                           (  Use_Error'Identity,
                              (  "The DLL "
                              &  Quote (Name)
                              &  " is for x86 32-bit Windows,"
                              &  " use a 64-bit Python version instead"
                           )  );
                        end if;
                     when others =>
                        null;
                  end case;
               end if;
               Raise_From_LastError
               (  Use_Error'Identity,
                  "Failed to load Python DLL " & Quote (Name) & ": ",
                  Error
               );
            end if;
         end if;
         Links.AddPendingCall := GetProcAddress (Library);
         if Links.AddPendingCall = null then
            Error := GetLastError;
            if Error /= ERROR_PROC_NOT_FOUND then
               Raise_From_LastError
               (  Use_Error'Identity,
                  (  "Failed to load Python DLL "
                  &  Quote (Name)
                  &  ", when looking for entry points: "
                  ),
                  Error
               );
            end if;
         end if;
         Links.Bool_FromLong               := GetProcAddress (Library);
         Links.ByteArray_AsString          := GetProcAddress (Library);
         Links.ByteArray_FromStringAndSize := GetProcAddress (Library);
         Links.ByteArray_Size              := GetProcAddress (Library);
         Links.Bytes_AsStringAndSize       := GetProcAddress (Library);
         Links.Bytes_FromStringAndSize     := GetProcAddress (Library);
         Links.Bytes_Size                  := GetProcAddress (Library);
         Links.Callable_Check              := GetProcAddress (Library);
         Links.Capsule_GetContext          := GetProcAddress (Library);
         Links.Capsule_GetDestructor       := GetProcAddress (Library);
         Links.Capsule_GetName             := GetProcAddress (Library);
         Links.Capsule_GetPointer          := GetProcAddress (Library);
         Links.Capsule_Import              := GetProcAddress (Library);
         Links.Capsule_IsValid             := GetProcAddress (Library);
         Links.Capsule_New                 := GetProcAddress (Library);
         Links.Capsule_SetContext          := GetProcAddress (Library);
         Links.Capsule_SetDestructor       := GetProcAddress (Library);
         Links.Capsule_SetName             := GetProcAddress (Library);
         Links.Capsule_SetPointer          := GetProcAddress (Library);
         Links.CompileString               := GetProcAddress (Library);
         Links.DecRef                      := GetProcAddress (Library);
--       Links.Dict_Check                  := GetProcAddress (Library);
--       Links.Dict_CheckExact             := GetProcAddress (Library);
         Links.Dict_Clear                  := GetProcAddress (Library);
         Links.Dict_Contains               := GetProcAddress (Library);
         Links.Dict_Copy                   := GetProcAddress (Library);
         Links.Dict_DelItem                := GetProcAddress (Library);
         Links.Dict_DelItemString          := GetProcAddress (Library);
         Links.Dict_GetItemString          := GetProcAddress (Library);
         Links.Dict_Items                  := GetProcAddress (Library);
         Links.Dict_Keys                   := GetProcAddress (Library);
         Links.Dict_New                    := GetProcAddress (Library);
         Links.Dict_Merge                  := GetProcAddress (Library);
         Links.Dict_Next                   := GetProcAddress (Library);
--       Links.Dict_SetDefault             := GetProcAddress (Library);
         Links.Dict_SetItem                := GetProcAddress (Library);
         Links.Dict_SetItemString          := GetProcAddress (Library);
         Links.Dict_Size                   := GetProcAddress (Library);
         Links.EndInterpreter              := GetProcAddress (Library);
         Links.Err_BadArgument             := GetProcAddress (Library);
         Links.Err_BadInternalCall         := GetProcAddress (Library);
         Links.Err_Clear                   := GetProcAddress (Library);
         Links.Err_Fetch                   := GetProcAddress (Library);
         Links.Err_NewException            := GetProcAddress (Library);
         Links.Err_NormalizeException      := GetProcAddress (Library);
         Links.Err_Occurred                := GetProcAddress (Library);
         Links.Err_Restore                 := GetProcAddress (Library);
         Links.Err_SetString               := GetProcAddress (Library);
         Links.Eval_InitThreads            := GetProcAddress (Library);
         Links.Eval_RestoreThread          := GetProcAddress (Library);
         Links.Eval_SaveThread             := GetProcAddress (Library);
         Links.ByteArray_Type       := GetProcAddress
                                       (  Library,
                                          "PyByteArray_Type" & Nul
                                       );
         Links.Bytes_Type           := GetProcAddress
                                       (  Library,
                                           "PyBytes_Type" & Nul
                                       );
         Links.Dict_Type            := GetProcAddress
                                       (  Library,
                                           "PyDict_Type" & Nul
                                       );
         Links.Exc_KeyError         := GetProcAddress
                                       (  Library,
                                          "PyExc_KeyError" & Nul
                                       );
         Links.Exc_LookupError      := GetProcAddress
                                       (  Library,
                                          "PyExc_LookupError" & Nul
                                       );
         Links.Exc_NameError        := GetProcAddress
                                       (  Library,
                                          "PyExc_NameError" & Nul
                                       );
         Links.Exc_PermissionError  := GetProcAddress
                                       (  Library,
                                          "PyExc_PermissionError" & Nul
                                       );
         Links.Exc_RuntimeError     := GetProcAddress
                                       (  Library,
                                          "PyExc_RuntimeError" & Nul
                                       );
         Links.Exc_SyntaxError      := GetProcAddress
                                       (  Library,
                                          "PyExc_SyntaxError" & Nul
                                       );
         Links.Exc_SystemError      := GetProcAddress
                                       (  Library,
                                          "PyExc_SystemError" & Nul
                                       );
         Links.Exc_TimeoutError     := GetProcAddress
                                       (  Library,
                                          "PyExc_TimeoutError" & Nul
                                       );
         Links.Exc_TypeError        := GetProcAddress
                                       (  Library,
                                          "PyExc_TypeError" & Nul
                                       );
         Links.Exc_ValueError       := GetProcAddress
                                       (  Library,
                                          "PyExc_ValueError" & Nul
                                       );
         Links.False                := GetProcAddress
                                       (  Library,
                                          "_Py_FalseStruct" & Nul
                                       );
         Links.Float_Type           := GetProcAddress
                                       (  Library,
                                          "PyFloat_Type" & Nul
                                       );
         Links.List_Type            := GetProcAddress
                                       (  Library,
                                          "PyList_Type" & Nul
                                       );
         Links.Long_Type            := GetProcAddress
                                       (  Library,
                                          "PyLong_Type" & Nul
                                       );
         Links.Set_Type             := GetProcAddress
                                       (  Library,
                                          "PySet_Type" & Nul
                                       );
         Links.Tuple_Type           := GetProcAddress
                                       (  Library,
                                          "PyTuple_Type" & Nul
                                       );
         Links.Type_Type            := GetProcAddress
                                       (  Library,
                                          "PyType_Type" & Nul
                                       );
         Links.Unicode_Type         := GetProcAddress
                                       (  Library,
                                          "PyUnicode_Type" & Nul
                                       );
         Links.FinalizeEx                := GetProcAddress (Library);
         Links.Float_AsDouble            := GetProcAddress (Library);
         Links.Float_FromDouble          := GetProcAddress (Library);
--       Links.GILState_Check            := GetProcAddress (Library);
         Links.GILState_Ensure           := GetProcAddress (Library);
         Links.GILState_Release          := GetProcAddress (Library);
         Links.Import_AddModule          := GetProcAddress (Library);
         Links.Import_AppendInittab      := GetProcAddress (Library);
         Links.Import_ExecCodeModuleEx   := GetProcAddress (Library);
         Links.Import_GetModule          := GetProcAddress (Library);
         Links.Import_Import             := GetProcAddress (Library);
         Links.Import_ImportModule       := GetProcAddress (Library);
         Links.IncRef                    := GetProcAddress (Library);
         Links.InitializeEx              := GetProcAddress (Library);
         Links.Index_Check               := GetProcAddress (Library);
         Links.Iter_Check                := GetProcAddress (Library);
         Links.Iter_Next                 := GetProcAddress (Library);
         Links.List_Append               := GetProcAddress (Library);
         Links.List_AsTuple              := GetProcAddress (Library);
--       Links.List_Check                := GetProcAddress (Library);
--       Links.List_CheckExact           := GetProcAddress (Library);
         Links.List_GetItem              := GetProcAddress (Library);
         Links.List_GetSlice             := GetProcAddress (Library);
         Links.List_Insert               := GetProcAddress (Library);
         Links.List_New                  := GetProcAddress (Library);
         Links.List_Reverse              := GetProcAddress (Library);
         Links.List_SetItem              := GetProcAddress (Library);
         Links.List_SetSlice             := GetProcAddress (Library);
         Links.List_Size                 := GetProcAddress (Library);
         Links.List_Sort                 := GetProcAddress (Library);
         Links.Long_AsLong               := GetProcAddress (Library);
         Links.Long_AsLongLong           := GetProcAddress (Library);
         Links.Long_AsUnsignedLong       := GetProcAddress (Library);
         Links.Long_AsUnsignedLongLong   := GetProcAddress (Library);
         Links.Long_FromLong             := GetProcAddress (Library);
         Links.Long_FromLongLong         := GetProcAddress (Library);
         Links.Long_FromUnsignedLong     := GetProcAddress (Library);
         Links.Long_FromUnsignedLongLong := GetProcAddress (Library);
         Links.Module_AddObject          := GetProcAddress (Library);
         Links.Module_Create             := GetProcAddress (Library);
         Links.Module_GetDict            := GetProcAddress (Library);
         Links.Module_GetName            := GetProcAddress (Library);
         Links.NewInterpreter            := GetProcAddress (Library);
         Links.None                      := GetProcAddress
                                            (  Library,
                                               "_Py_NoneStruct" & Nul
                                            );
         Links.Number_Absolute           := GetProcAddress (Library);
         Links.Number_Add                := GetProcAddress (Library);
         Links.Number_And                := GetProcAddress (Library);
         Links.Number_AsSsize_t          := GetProcAddress (Library);
         Links.Number_Check              := GetProcAddress (Library);
         Links.Number_Divmod             := GetProcAddress (Library);
         Links.Number_Float              := GetProcAddress (Library);
         Links.Number_FloorDivide        := GetProcAddress (Library);
         Links.Number_Index              := GetProcAddress (Library);
         Links.Number_InPlaceAdd         := GetProcAddress (Library);
         Links.Number_InPlaceAnd         := GetProcAddress (Library);
         Links.Number_InPlaceFloorDivide := GetProcAddress (Library);
         Links.Number_InPlaceLshift         := GetProcAddress (Library);
         Links.Number_InPlaceMatrixMultiply := GetProcAddress (Library);
         Links.Number_InPlaceMultiply       := GetProcAddress (Library);
         Links.Number_InPlaceOr          := GetProcAddress (Library);
         Links.Number_InPlacePower       := GetProcAddress (Library);
         Links.Number_InPlaceRemainder   := GetProcAddress (Library);
         Links.Number_InPlaceRshift      := GetProcAddress (Library);
         Links.Number_InPlaceSubtract    := GetProcAddress (Library);
         Links.Number_InPlaceTrueDivide  := GetProcAddress (Library);
         Links.Number_InPlaceXor         := GetProcAddress (Library);
         Links.Number_Invert             := GetProcAddress (Library);
         Links.Number_Long               := GetProcAddress (Library);
         Links.Number_Lshift             := GetProcAddress (Library);
         Links.Number_MatrixMultiply     := GetProcAddress (Library);
         Links.Number_Multiply           := GetProcAddress (Library);
         Links.Number_Negative           := GetProcAddress (Library);
         Links.Number_Or                 := GetProcAddress (Library);
         Links.Number_Positive           := GetProcAddress (Library);
         Links.Number_Power              := GetProcAddress (Library);
         Links.Number_Remainder          := GetProcAddress (Library);
         Links.Number_Rshift             := GetProcAddress (Library);
         Links.Number_Subtract           := GetProcAddress (Library);
         Links.Number_ToBase             := GetProcAddress (Library);
         Links.Number_TrueDivide         := GetProcAddress (Library);
         Links.Number_Xor                := GetProcAddress (Library);
         Links.Object_Bytes              := GetProcAddress (Library);
         Links.Object_Call               := GetProcAddress (Library);
         Links.Object_CallNoArgs         := GetProcAddress (Library);
         Links.Object_CallObject         := GetProcAddress (Library);
         Links.Object_DelItem            := GetProcAddress (Library);
         Links.Object_Dir                := GetProcAddress (Library);
         Links.Object_GenericGetAttr     := GetProcAddress (Library);
         Links.Object_GenericSetAttr     := GetProcAddress (Library);
         Links.Object_GetAttr            := GetProcAddress (Library);
         Links.Object_GetAttrString      := GetProcAddress (Library);
         Links.Object_GetItem            := GetProcAddress (Library);
         Links.Object_GetIter            := GetProcAddress (Library);
         Links.Object_HasAttr            := GetProcAddress (Library);
         Links.Object_HasAttrString      := GetProcAddress (Library);
         Links.Object_IsInstance         := GetProcAddress (Library);
         Links.Object_IsSubclass         := GetProcAddress (Library);
         Links.Object_RichCompareBool    := GetProcAddress (Library);
         Links.Object_SetAttr            := GetProcAddress (Library);
         Links.Object_SetAttrString      := GetProcAddress (Library);
         Links.Object_SetItem            := GetProcAddress (Library);
         Links.Object_Size               := GetProcAddress (Library);
         Links.Object_Str                := GetProcAddress (Library);
         Links.Object_Type               := GetProcAddress (Library);
         Links.Sequence_Check            := GetProcAddress (Library);
         Links.Sequence_Concat           := GetProcAddress (Library);
         Links.Sequence_Contains         := GetProcAddress (Library);
         Links.Sequence_Count            := GetProcAddress (Library);
         Links.Sequence_DelItem          := GetProcAddress (Library);
         Links.Sequence_DelSlice         := GetProcAddress (Library);
         Links.Sequence_GetItem          := GetProcAddress (Library);
         Links.Sequence_GetSlice         := GetProcAddress (Library);
         Links.Sequence_Index            := GetProcAddress (Library);
         Links.Sequence_List             := GetProcAddress (Library);
         Links.Sequence_Repeat           := GetProcAddress (Library);
         Links.Sequence_SetItem          := GetProcAddress (Library);
         Links.Sequence_SetSlice         := GetProcAddress (Library);
         Links.Sequence_Size             := GetProcAddress (Library);
         Links.Sequence_Tuple            := GetProcAddress (Library);
         Links.Set_Add                   := GetProcAddress (Library);
         Links.Set_Clear                 := GetProcAddress (Library);
         Links.Set_Contains              := GetProcAddress (Library);
         Links.Set_Discard               := GetProcAddress (Library);
         Links.Frozenset_New             := GetProcAddress (Library);
         Links.Set_New                   := GetProcAddress (Library);
         Links.Set_Pop                   := GetProcAddress (Library);
         Links.Set_Size                  := GetProcAddress (Library);
         Links.Sys_GetObject             := GetProcAddress (Library);
--       Links.Sys_GetSizeOf             := GetProcAddress (Library);
         Links.ThreadState_Get           := GetProcAddress (Library);
         Links.ThreadState_Swap          := GetProcAddress (Library);
         Links.True                      := GetProcAddress
                                            (  Library,
                                               "_Py_TrueStruct" & Nul
                                            );
         Links.Tuple_GetItem             := GetProcAddress (Library);
         Links.Tuple_GetSlice            := GetProcAddress (Library);
         Links.Tuple_New                 := GetProcAddress (Library);
         Links.Tuple_SetItem             := GetProcAddress (Library);
         Links.Tuple_Size                := GetProcAddress (Library);
         Links.Type_FromSpec             := GetProcAddress (Library);
         Links.Type_FromSpecWithBases    := GetProcAddress (Library);
         Links.Type_GetSlot              := GetProcAddress (Library);
         Links.Type_IsSubtype            := GetProcAddress (Library);
         Links.Unicode_AsEncodedString   := GetProcAddress (Library);
         Links.Unicode_DecodeFSDefault   := GetProcAddress (Library);
         Links.Unicode_FromString        := GetProcAddress (Library);
         Links.Unicode_FromStringAndSize := GetProcAddress (Library);
         Links.Unicode_GetLength         := GetProcAddress (Library);
         Links.Unicode_ReadChar          := GetProcAddress (Library);

         Check_Links (Name);
      end;
      Loaded := True;
   end Load;
end Py.Load_Python_Library;
