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

with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

with Ada.Directories;
with Ada.Unchecked_Deallocation;

package body Py.Load_Python_Library is

      -- Lazy function call binding
   RTLD_LAZY         : constant := 16#00001#;
      -- Immediate function call binding
   RTLD_NOW          : constant := 16#00002#;
      -- Mask of binding time value
   RTLD_BINDING_MASK : constant := 16#00003#;
      -- Do not load the object
   TLD_NOLOAD        : constant := 16#00004#;
      -- Use deep binding
   RTLD_DEEPBIND     : constant := 16#00008#;
      -- If the following  bit is set  in the MODE argument to `dlopen',
      -- the symbols of the loaded object and its dependencies  are made
      -- visible as if the object were linked directly into the program.
   RTLD_GLOBAL       : constant := 16#00100#;
      -- Unix98 demands  the following  flag  which  is  the  inverse to
      -- RTLD_GLOBAL. The implementation  does this by default and so we
      -- can define the value to zero.
   RTLD_LOCAL        : constant := 16#0#;
      -- Do not delete object when closed
   RTLD_NODELETE     : constant := 16#01000#;

   function dlopen
            (  Name  : char_array;
               Flags : int := RTLD_NOW + RTLD_GLOBAL
            )  return Address;
   pragma Import (C, dlopen);

   function dlsym
            (  Module : Address;
               Name   : char_array := "Py_AddPendingCall" & Nul
            )  return AddPendingCall_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyBool_FromLong" & Nul
            )  return Bool_FromLLong_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyByteArray_AsString" & Nul
            )  return ByteArray_AsString_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :=
                                   "PyByteArray_FromStringAndSize" & Nul
            )  return ByteArray_FromStringAndSize_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyByteArray_Size" & Nul
            )  return ByteArray_Size_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyBytes_AsStringAndSize" & Nul
            )  return Bytes_AsStringAndSize_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyBytes_FromStringAndSize" & Nul
            )  return Bytes_FromStringAndSize_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyBytes_Size" & Nul
            )  return Bytes_Size_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyCallable_Check" & Nul
            )  return Callable_Check_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyCapsule_GetContext" & Nul
            )  return Capsule_GetContext_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyCapsule_GetDestructor" & Nul
            )  return Capsule_GetDestructor_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyCapsule_GetName" & Nul
            )  return Capsule_GetName_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyCapsule_GetPointer" & Nul
            )  return Capsule_GetPointer_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyCapsule_New" & Nul
            )  return Capsule_New_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyCapsule_Import" & Nul
            )  return Capsule_Import_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyCapsule_IsValid" & Nul
            )  return Capsule_IsValid_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyCapsule_SetContext" & Nul
            )  return Capsule_SetContext_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyCapsule_SetDestructor" & Nul
            )  return Capsule_SetDestructor_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyCapsule_SetName" & Nul
            )  return Capsule_SetName_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyCapsule_SetPointer" & Nul
            )  return Capsule_SetPointer_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "Py_CompileString" & Nul
            )  return CompileString_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "Py_DecRef" & Nul
            )  return DecRef_Ptr;
   --  function dlsym
   --           (  Module : Address;
   --              Name   : char_array := "PyDict_Check" & Nul
   --           )  return Dict_Check_Ptr;
   --  function dlsym
   --           (  Module : Address;
   --              Name   : char_array := "PyDict_CheckExact" & Nul
   --           )  return Dict_CheckExact_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_Clear" & Nul
            )  return Dict_Clear_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_Contains" & Nul
            )  return Dict_Contains_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_Copy" & Nul
            )  return Dict_Copy_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_DelItem" & Nul
            )  return Dict_DelItem_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_DelItemString" & Nul
            )  return Dict_DelItemString_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_GetItemString" & Nul
            )  return Dict_GetItemString_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_Keys" & Nul
            )  return Dict_Keys_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_Merge" & Nul
            )  return Dict_Merge_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_Next" & Nul
            )  return Dict_Next_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_Items" & Nul
            )  return Dict_Items_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_New" & Nul
            )  return Dict_New_Ptr;
-- function dlsym
--          (  Module : Address;
--             Name   : char_array := "PyDict_SetDefault" & Nul
--          )  return Dict_SetDefault_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_SetItem" & Nul
            )  return Dict_SetItem_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_SetItemString" & Nul
            )  return Dict_SetItemString_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyDict_Size" & Nul
            )  return Dict_Size_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "Py_EndInterpreter" & Nul
            )  return EndInterpreter_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyErr_BadArgument" & Nul
            )  return Err_BadArgument_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyErr_BadInternalCall" & Nul
            )  return Err_BadInternalCall_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyErr_Clear" & Nul
            )  return Err_Clear_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyErr_Fetch" & Nul
            )  return Err_Fetch_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyErr_NewException" & Nul
            )  return Err_NewException_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyErr_NormalizeException" & Nul
            )  return Err_NormalizeException_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyErr_Occurred" & Nul
            )  return Err_Occurred_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyErr_Restore" & Nul
            )  return Err_Restore_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyErr_SetString" & Nul
            )  return Err_SetString_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyEval_InitThreads" & Nul
            )  return Eval_InitThreads_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyEval_RestoreThread" & Nul
            )  return Eval_RestoreThread_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyEval_SaveThread" & Nul
            )  return Eval_SaveThread_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "Py_FinalizeEx" & Nul
            )  return FinalizeEx_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyFloat_AsDouble" & Nul
            )  return Float_AsDouble_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyFloat_FromDouble" & Nul
            )  return Float_FromDouble_Ptr;
-- function dlsym
--          (  Module : Address;
--             Name   : char_array := "PyGILState_Check" & Nul
--          )  return GILState_Check_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyGILState_Ensure" & Nul
            )  return GILState_Ensure_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyGILState_Release" & Nul
            )  return GILState_Release_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyImport_AddModule" & Nul
            )  return Import_AddModule_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyImport_AppendInittab" & Nul
            )  return Import_AppendInittab_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "Py_IncRef" & Nul
            )  return IncRef_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "Py_InitializeEx" & Nul
            )  return InitializeEx_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyImport_ExecCodeModuleEx" & Nul
            )  return Import_ExecCodeModuleEx_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyImport_GetModule" & Nul
            )  return Import_GetModule_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyImport_Import" & Nul
            )  return Import_Import_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyImport_ImportModule" & Nul
            )  return Import_ImportModule_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyIndex_Check" & Nul
            )  return Index_Check_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyIter_Check" & Nul
            )  return Iter_Check_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyIter_Next" & Nul
            )  return Iter_Next_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyList_Append" & Nul
            )  return List_Append_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyList_AsTuple" & Nul
            )  return List_AsTuple_Ptr;
-- function dlsym
--          (  Module : Address;
--             Name   : char_array := "PyList_Check" & Nul
--          )  return List_Check_Ptr;
-- function dlsym
--          (  Module : Address;
--             Name   : char_array := "PyList_CheckExact" & Nul
--          )  return List_CheckExact_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyList_GetItem" & Nul
            )  return List_GetItem_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyList_GetSlice" & Nul
            )  return List_GetSlice_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyList_Insert" & Nul
            )  return List_Insert_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyList_New" & Nul
            )  return List_New_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyList_Reverse" & Nul
            )  return List_Reverse_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyList_SetItem" & Nul
            )  return List_SetItem_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyList_SetSlice" & Nul
            )  return List_SetSlice_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyList_Size" & Nul
            )  return List_Size_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyList_Sort" & Nul
            )  return List_Sort_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyLong_AsLong" & Nul
            )  return Long_AsLong_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyLong_AsLongLong" & Nul
            )  return Long_AsLongLong_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyLong_AsUnsignedLong" & Nul
            )  return Long_AsUnsignedLong_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyLong_AsUnsignedLongLong" & Nul
            )  return Long_AsUnsignedLongLong_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyLong_FromLong" & Nul
            )  return Long_FromLong_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyLong_FromLongLong" & Nul
            )  return Long_FromLongLong_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyLong_FromUnsignedLong" & Nul
            )  return Long_FromUnsignedLong_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :=
                           "PyLong_FromUnsignedLongLong" & Nul
            )  return Long_FromUnsignedLongLong_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyModule_AddObject" & Nul
            )  return Module_AddObject_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyModule_Create2" & Nul
            )  return Module_Create_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyModule_GetDict" & Nul
            )  return Module_GetDict_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyModule_GetName" & Nul
            )  return Module_GetName_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "Py_NewInterpreter" & Nul
            )  return NewInterpreter_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Absolute" & Nul
            )  return Number_Absolute_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Add" & Nul
            )  return Number_Add_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_And" & Nul
            )  return Number_And_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_AsSsize_t" & Nul
            )  return Number_AsSsize_t_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Check" & Nul
            )  return Number_Check_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Divmod" & Nul
            )  return Number_Divmod_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Float" & Nul
            )  return Number_Float_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_FloorDivide" & Nul
            )  return Number_FloorDivide_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Index" & Nul
            )  return Number_Index_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceAdd" & Nul
            )  return Number_InPlaceAdd_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceAnd" & Nul
            )  return Number_InPlaceAnd_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceFloorDivide" & Nul
            )  return Number_InPlaceFloorDivide_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceLshift" & Nul
            )  return Number_InPlaceLshift_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceMatrixMultiply" & Nul
            )  return Number_InPlaceMatrixMultiply_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceMultiply" & Nul
            )  return Number_InPlaceMultiply_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceOr" & Nul
            )  return Number_InPlaceOr_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlacePower" & Nul
            )  return Number_InPlacePower_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceRemainder" & Nul
            )  return Number_InPlaceRemainder_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceRshift" & Nul
            )  return Number_InPlaceRshift_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceSubtract" & Nul
            )  return Number_InPlaceSubtract_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceTrueDivide" & Nul
            )  return Number_InPlaceTrueDivide_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_InPlaceXor" & Nul
            )  return Number_InPlaceXor_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Invert" & Nul
            )  return Number_Invert_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Long" & Nul
            )  return Number_Long_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Lshift" & Nul
            )  return Number_Lshift_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_MatrixMultiply" & Nul
            )  return Number_MatrixMultiply_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Multiply" & Nul
            )  return Number_Multiply_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Negative" & Nul
            )  return Number_Negative_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Or" & Nul
            )  return Number_Or_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Positive" & Nul
            )  return Number_Positive_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Power" & Nul
            )  return Number_Power_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Remainder" & Nul
            )  return Number_Remainder_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Rshift" & Nul
            )  return Number_Rshift_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Subtract" & Nul
            )  return Number_Subtract_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_ToBase" & Nul
            )  return Number_ToBase_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_TrueDivide" & Nul
            )  return Number_TrueDivide_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :="PyNumber_Xor" & Nul
            )  return Number_Xor_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_Bytes" & Nul
            )  return Object_Bytes_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_Call" & Nul
            )  return Object_Call_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_CallNoArgs" & Nul
            )  return Object_CallNoArgs_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_CallObject" & Nul
            )  return Object_CallObject_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_DelItem" & Nul
            )  return Object_DelItem_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_Dir" & Nul
            )  return Object_Dir_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_GenericGetAttr" & Nul
            )  return Object_GenericGetAttr_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_GenericSetAttr" & Nul
            )  return Object_GenericSetAttr_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_GetAttr" & Nul
            )  return Object_GetAttr_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_GetAttrString" & Nul
            )  return Object_GetAttrString_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_GetItem" & Nul
            )  return Object_GetItem_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_GetIter" & Nul
            )  return Object_GetIter_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_HasAttr" & Nul
            )  return Object_HasAttr_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_HasAttrString" & Nul
            )  return Object_HasAttrString_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_IsInstance" & Nul
            )  return Object_IsInstance_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_IsSubclass" & Nul
            )  return Object_IsSubclass_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_RichCompareBool" & Nul
            )  return Object_RichCompareBool_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_SetAttr" & Nul
            )  return Object_SetAttr_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_SetAttrString" & Nul
            )  return Object_SetAttrString_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_SetItem" & Nul
            )  return Object_SetItem_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_Size" & Nul
            )  return Object_Size_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_Str" & Nul
            )  return Object_Str_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyObject_Type" & Nul
            )  return Object_Type_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_Check" & Nul
            )  return Sequence_Check_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_Concat" & Nul
            )  return Sequence_Concat_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_Contains" & Nul
            )  return Sequence_Contains_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_Count" & Nul
            )  return Sequence_Count_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_DelItem" & Nul
            )  return Sequence_DelItem_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_DelSlice" & Nul
            )  return Sequence_DelSlice_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_GetItem" & Nul
            )  return Sequence_GetItem_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_GetSlice" & Nul
            )  return Sequence_GetSlice_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_Index" & Nul
            )  return Sequence_Index_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_List" & Nul
            )  return Sequence_List_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_Repeat" & Nul
            )  return Sequence_Repeat_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_SetItem" & Nul
            )  return Sequence_SetItem_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_SetSlice" & Nul
            )  return Sequence_SetSlice_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_Size" & Nul
            )  return Sequence_Size_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySequence_Tuple" & Nul
            )  return Sequence_Tuple_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySet_Add" & Nul
            )  return Set_Add_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySet_Clear" & Nul
            )  return Set_Clear_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySet_Contains" & Nul
            )  return Set_Contains_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySet_Discard" & Nul
            )  return Set_Discard_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyFrozenSet_New" & Nul
            )  return Frozenset_New_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySet_New" & Nul
            )  return Set_New_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySet_Pop" & Nul
            )  return Set_Pop_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySet_Size" & Nul
            )  return Set_Size_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PySys_GetObject" & Nul
            )  return Sys_GetObject_Ptr;
-- function dlsym
--          (  Module : Address;
--             Name   : char_array := "_PySys_GetSizeOf" & Nul
--          )  return Sys_GetSizeOf_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyThreadState_Get" & Nul
            )  return ThreadState_Get_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyThreadState_Swap" & Nul
            )  return ThreadState_Swap_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyTuple_GetItem" & Nul
            )  return Tuple_GetItem_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyTuple_GetSlice" & Nul
            )  return Tuple_GetSlice_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyTuple_New" & Nul
            )  return Tuple_New_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyTuple_SetItem" & Nul
            )  return Tuple_SetItem_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyTuple_Size" & Nul
            )  return Tuple_Size_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyType_FromSpec" & Nul
            )  return Type_FromSpec_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyType_FromSpecWithBases" & Nul
            )  return Type_FromSpecWithBases_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyType_GetSlot" & Nul
            )  return Type_GetSlot_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyType_IsSubtype" & Nul
            )  return Type_IsSubtype_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyUnicode_AsEncodedString" & Nul
            )  return Unicode_AsEncodedString_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyUnicode_DecodeFSDefault" & Nul
            )  return Unicode_DecodeFSDefault_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyUnicode_FromString" & Nul
            )  return Unicode_FromString_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :=
                                     "PyUnicode_FromStringAndSize" & Nul
            )  return Unicode_FromStringAndSize_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyUnicode_GetLength" & Nul
            )  return Unicode_GetLength_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "PyUnicode_ReadChar" & Nul
            )  return Unicode_ReadChar_Ptr;

   function dlsym
            (  Module : Address;
               Name   : char_array
            )  return Object;
   function dlsym
            (  Module : Address;
               Name   : char_array
            )  return Object_Ptr;
   pragma Import (Stdcall, dlsym, "dlsym");

   Library : Address := Null_Address;
   Loaded  : Boolean := False;

   function Get_Default_Name (Major : Natural := 7) return String is
      use Ada.Directories;
      Prefix : constant String := "libpython3.";

      type Found (Length : Natural) is record
         Major : Integer;
         Name  : String (1..Length);
      end record;
      type Found_Ptr is access Found;
      procedure Free is
         new Ada.Unchecked_Deallocation (Found, Found_Ptr);

      function "<" (Left : Found_Ptr; Right : Integer) return Boolean is
      begin
         return Left = null or else Left.Major < Right;
      end "<";

      Most : Found_Ptr;

      procedure Scan (Path : String; Descend : Boolean := True) is
         Items : Search_Type;
         This  : Directory_Entry_Type;
      begin
         if not Exists (Path) then
            return;
         end if;
         Start_Search (Items, Path, "*");
         while More_Entries (Items) loop
             Get_Next_Entry (Items, This);
             case Kind (This) is
                when Ordinary_File =>
                   declare
                      Name    : constant String := Simple_Name (This);
                      Version : Integer;
                      Pointer : Integer := Name'First;
                   begin
                      if Is_Prefix (Prefix, Name) then
                         Pointer := Pointer + Prefix'Length;
                         Get (Name, Pointer, Version);
                         while Pointer <= Name'Last loop
                            exit when Name (Pointer) = '.';
                            Pointer := Pointer + 1;
                         end loop;
                         if (  Is_Prefix (".so", Name, Pointer)
                            and then
                               Version >= Major
                            and then
                               Most < Version
                            )  then
                            Free (Most);
                            Most :=
                               new Found'(Name'Length, Version, Name);
                         end if;
                      end if;
                   exception
                      when others =>
                         null;
                   end;
                when Directory =>
                   if Descend then
                      Scan (Full_Name (This), False);
                   end if;
                when Special_File =>
                   null;
             end case;
         end loop;
         End_Search (Items);
      exception
         when others =>
            null;
      end Scan;
   begin
      Scan ("/usr/lib", True);
      if System.Address'Size >= 64 then
         Scan ("/usr/lib64", False);
      else
         Scan ("/usr/lib32", False);
      end if;
      if Most = null then
         return "libpython3.so";
      else
         declare
            Result : constant String := Most.Name;
         begin
            Free (Most);
            return Result;
         end;
      end if;
   end Get_Default_Name;

   function Get_Extension return String is
   begin
      return "*.so.*";
   end Get_Extension;

   function Is_Loaded return Boolean is
   begin
      return Loaded;
   end Is_Loaded;

   function Get_Python_Path (Major : Natural := 7) return String is
   begin
      return ""; -- Should be installed under the standard library path
   end Get_Python_Path;

   procedure Load (Name : String := "") is
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
         Name : constant String := Get_Library_Path;
      begin
         if Library = Null_Address then
            Library := dlopen (To_C (Name));
            if Library = Null_Address then
               Raise_Exception
               (  Use_Error'Identity,
                  "Failed to load Python shared library " & Quote (Name)
               );
            end if;
         end if;
         Links.AddPendingCall              := dlsym (Library);
         Links.Bool_FromLong               := dlsym (Library);
         Links.ByteArray_AsString          := dlsym (Library);
         Links.ByteArray_FromStringAndSize := dlsym (Library);
         Links.ByteArray_Size              := dlsym (Library);
         Links.Bytes_AsStringAndSize       := dlsym (Library);
         Links.Bytes_FromStringAndSize     := dlsym (Library);
         Links.Bytes_Size                  := dlsym (Library);
         Links.Callable_Check              := dlsym (Library);
         Links.Capsule_GetContext          := dlsym (Library);
         Links.Capsule_GetDestructor       := dlsym (Library);
         Links.Capsule_GetName             := dlsym (Library);
         Links.Capsule_GetPointer          := dlsym (Library);
         Links.Capsule_Import              := dlsym (Library);
         Links.Capsule_IsValid             := dlsym (Library);
         Links.Capsule_New                 := dlsym (Library);
         Links.Capsule_SetContext          := dlsym (Library);
         Links.Capsule_SetDestructor       := dlsym (Library);
         Links.Capsule_SetName             := dlsym (Library);
         Links.Capsule_SetPointer          := dlsym (Library);
         Links.CompileString               := dlsym (Library);
         Links.DecRef                      := dlsym (Library);
--       Links.Dict_Check                  := dlsym (Library);
--       Links.Dict_CheckExact             := dlsym (Library);
         Links.Dict_Clear                  := dlsym (Library);
         Links.Dict_Contains               := dlsym (Library);
         Links.Dict_Copy                   := dlsym (Library);
         Links.Dict_DelItem                := dlsym (Library);
         Links.Dict_DelItemString          := dlsym (Library);
         Links.Dict_GetItemString          := dlsym (Library);
         Links.Dict_Items                  := dlsym (Library);
         Links.Dict_Keys                   := dlsym (Library);
         Links.Dict_New                    := dlsym (Library);
         Links.Dict_Merge                  := dlsym (Library);
         Links.Dict_Next                   := dlsym (Library);
--       Links.Dict_SetDefault             := dlsym (Library);
         Links.Dict_SetItem                := dlsym (Library);
         Links.Dict_SetItemString          := dlsym (Library);
         Links.Dict_Size                   := dlsym (Library);
         Links.EndInterpreter              := dlsym (Library);
         Links.Err_BadArgument             := dlsym (Library);
         Links.Err_BadInternalCall         := dlsym (Library);
         Links.Err_Clear                   := dlsym (Library);
         Links.Err_Fetch                   := dlsym (Library);
         Links.Err_NewException            := dlsym (Library);
         Links.Err_NormalizeException      := dlsym (Library);
         Links.Err_Occurred                := dlsym (Library);
         Links.Err_Restore                 := dlsym (Library);
         Links.Err_SetString               := dlsym (Library);
         Links.Eval_InitThreads            := dlsym (Library);
         Links.Eval_RestoreThread          := dlsym (Library);
         Links.Eval_SaveThread             := dlsym (Library);
         Links.ByteArray_Type       := dlsym
                                       (  Library,
                                          "PyByteArray_Type" & Nul
                                       );
         Links.Bytes_Type           := dlsym
                                       (  Library,
                                          "PyBytes_Type" & Nul
                                       );
         Links.Dict_Type            := dlsym
                                       (  Library,
                                          "PyDict_Type" & Nul
                                       );
         Links.Exc_KeyError         := dlsym
                                       (  Library,
                                          "PyExc_KeyError" & Nul
                                       );
         Links.Exc_LookupError      := dlsym
                                       (  Library,
                                          "PyExc_LookupError" & Nul
                                       );
         Links.Exc_NameError        := dlsym
                                       (  Library,
                                          "PyExc_NameError" & Nul
                                       );
         Links.Exc_PermissionError  := dlsym
                                       (  Library,
                                          "PyExc_PermissionError" & Nul
                                       );
         Links.Exc_RuntimeError     := dlsym
                                       (  Library,
                                          "PyExc_RuntimeError" & Nul
                                       );
         Links.Exc_SyntaxError      := dlsym
                                       (  Library,
                                          "PyExc_SyntaxError" & Nul
                                       );
         Links.Exc_SystemError      := dlsym
                                       (  Library,
                                          "PyExc_SystemError" & Nul
                                       );
         Links.Exc_TimeoutError     := dlsym
                                       (  Library,
                                          "PyExc_TimeoutError" & Nul
                                       );
         Links.Exc_TypeError        := dlsym
                                       (  Library,
                                          "PyExc_TypeError" & Nul
                                       );
         Links.Exc_ValueError       := dlsym
                                       (  Library,
                                          "PyExc_ValueError" & Nul
                                       );
         Links.False                := dlsym
                                       (  Library,
                                          "_Py_FalseStruct" & Nul
                                       );
         Links.Float_Type           := dlsym
                                       (  Library,
                                          "PyFloat_Type" & Nul
                                       );
         Links.List_Type            := dlsym
                                       (  Library,
                                          "PyList_Type" & Nul
                                       );
         Links.Long_Type            := dlsym
                                       (  Library,
                                          "PyLong_Type" & Nul
                                       );
         Links.Set_Type             := dlsym
                                       (  Library,
                                          "PySet_Type" & Nul
                                       );
         Links.Tuple_Type           := dlsym
                                       (  Library,
                                          "PyTuple_Type" & Nul
                                       );
         Links.Type_Type            := dlsym
                                       (  Library,
                                          "PyType_Type" & Nul
                                       );
         Links.Unicode_Type         := dlsym
                                       (  Library,
                                          "PyUnicode_Type" & Nul
                                       );
         Links.FinalizeEx                := dlsym (Library);
         Links.Float_AsDouble            := dlsym (Library);
         Links.Float_FromDouble          := dlsym (Library);
--       Links.GILState_Check            := dlsym (Library);
         Links.GILState_Ensure           := dlsym (Library);
         Links.GILState_Release          := dlsym (Library);
         Links.Import_AddModule          := dlsym (Library);
         Links.Import_AppendInittab      := dlsym (Library);
         Links.Import_ExecCodeModuleEx   := dlsym (Library);
         Links.Import_GetModule          := dlsym (Library);
         Links.Import_Import             := dlsym (Library);
         Links.Import_ImportModule       := dlsym (Library);
         Links.IncRef                    := dlsym (Library);
         Links.InitializeEx              := dlsym (Library);
         Links.Index_Check               := dlsym (Library);
         Links.Iter_Check                := dlsym (Library);
         Links.Iter_Next                 := dlsym (Library);
         Links.List_Append               := dlsym (Library);
         Links.List_AsTuple              := dlsym (Library);
--       Links.List_Check                := dlsym (Library);
--       Links.List_CheckExact           := dlsym (Library);
         Links.List_GetItem              := dlsym (Library);
         Links.List_GetSlice             := dlsym (Library);
         Links.List_Insert               := dlsym (Library);
         Links.List_New                  := dlsym (Library);
         Links.List_Reverse              := dlsym (Library);
         Links.List_SetItem              := dlsym (Library);
         Links.List_SetSlice             := dlsym (Library);
         Links.List_Size                 := dlsym (Library);
         Links.List_Sort                 := dlsym (Library);
         Links.Long_AsLong               := dlsym (Library);
         Links.Long_AsLongLong           := dlsym (Library);
         Links.Long_AsUnsignedLong       := dlsym (Library);
         Links.Long_AsUnsignedLongLong   := dlsym (Library);
         Links.Long_FromLong             := dlsym (Library);
         Links.Long_FromLongLong         := dlsym (Library);
         Links.Long_FromUnsignedLong     := dlsym (Library);
         Links.Long_FromUnsignedLongLong := dlsym (Library);
         Links.Module_AddObject          := dlsym (Library);
         Links.Module_Create             := dlsym (Library);
         Links.Module_GetDict            := dlsym (Library);
         Links.Module_GetName            := dlsym (Library);
         Links.NewInterpreter            := dlsym (Library);
         Links.None                      := dlsym
                                            (  Library,
                                               "_Py_NoneStruct" & Nul
                                            );
         Links.Number_Absolute           := dlsym (Library);
         Links.Number_Add                := dlsym (Library);
         Links.Number_And                := dlsym (Library);
         Links.Number_AsSsize_t          := dlsym (Library);
         Links.Number_Check              := dlsym (Library);
         Links.Number_Divmod             := dlsym (Library);
         Links.Number_Float              := dlsym (Library);
         Links.Number_FloorDivide        := dlsym (Library);
         Links.Number_Index              := dlsym (Library);
         Links.Number_InPlaceAdd         := dlsym (Library);
         Links.Number_InPlaceAnd         := dlsym (Library);
         Links.Number_InPlaceFloorDivide := dlsym (Library);
         Links.Number_InPlaceLshift         := dlsym (Library);
         Links.Number_InPlaceMatrixMultiply := dlsym (Library);
         Links.Number_InPlaceMultiply       := dlsym (Library);
         Links.Number_InPlaceOr          := dlsym (Library);
         Links.Number_InPlacePower       := dlsym (Library);
         Links.Number_InPlaceRemainder   := dlsym (Library);
         Links.Number_InPlaceRshift      := dlsym (Library);
         Links.Number_InPlaceSubtract    := dlsym (Library);
         Links.Number_InPlaceTrueDivide  := dlsym (Library);
         Links.Number_InPlaceXor         := dlsym (Library);
         Links.Number_Invert             := dlsym (Library);
         Links.Number_Long               := dlsym (Library);
         Links.Number_Lshift             := dlsym (Library);
         Links.Number_MatrixMultiply     := dlsym (Library);
         Links.Number_Multiply           := dlsym (Library);
         Links.Number_Negative           := dlsym (Library);
         Links.Number_Or                 := dlsym (Library);
         Links.Number_Positive           := dlsym (Library);
         Links.Number_Power              := dlsym (Library);
         Links.Number_Remainder          := dlsym (Library);
         Links.Number_Rshift             := dlsym (Library);
         Links.Number_Subtract           := dlsym (Library);
         Links.Number_ToBase             := dlsym (Library);
         Links.Number_TrueDivide         := dlsym (Library);
         Links.Number_Xor                := dlsym (Library);
         Links.Object_Bytes              := dlsym (Library);
         Links.Object_Call               := dlsym (Library);
         Links.Object_CallNoArgs         := dlsym (Library);
         Links.Object_CallObject         := dlsym (Library);
         Links.Object_DelItem            := dlsym (Library);
         Links.Object_Dir                := dlsym (Library);
         Links.Object_GenericGetAttr     := dlsym (Library);
         Links.Object_GenericSetAttr     := dlsym (Library);
         Links.Object_GetAttr            := dlsym (Library);
         Links.Object_GetAttrString      := dlsym (Library);
         Links.Object_GetItem            := dlsym (Library);
         Links.Object_GetIter            := dlsym (Library);
         Links.Object_HasAttr            := dlsym (Library);
         Links.Object_HasAttrString      := dlsym (Library);
         Links.Object_IsInstance         := dlsym (Library);
         Links.Object_IsSubclass         := dlsym (Library);
         Links.Object_RichCompareBool    := dlsym (Library);
         Links.Object_SetAttr            := dlsym (Library);
         Links.Object_SetAttrString      := dlsym (Library);
         Links.Object_SetItem            := dlsym (Library);
         Links.Object_Size               := dlsym (Library);
         Links.Object_Str                := dlsym (Library);
         Links.Object_Type               := dlsym (Library);
         Links.Sequence_Check            := dlsym (Library);
         Links.Sequence_Concat           := dlsym (Library);
         Links.Sequence_Contains         := dlsym (Library);
         Links.Sequence_Count            := dlsym (Library);
         Links.Sequence_DelItem          := dlsym (Library);
         Links.Sequence_DelSlice         := dlsym (Library);
         Links.Sequence_GetItem          := dlsym (Library);
         Links.Sequence_GetSlice         := dlsym (Library);
         Links.Sequence_Index            := dlsym (Library);
         Links.Sequence_List             := dlsym (Library);
         Links.Sequence_Repeat           := dlsym (Library);
         Links.Sequence_SetItem          := dlsym (Library);
         Links.Sequence_SetSlice         := dlsym (Library);
         Links.Sequence_Size             := dlsym (Library);
         Links.Sequence_Tuple            := dlsym (Library);
         Links.Set_Add                   := dlsym (Library);
         Links.Set_Clear                 := dlsym (Library);
         Links.Set_Contains              := dlsym (Library);
         Links.Set_Discard               := dlsym (Library);
         Links.Frozenset_New             := dlsym (Library);
         Links.Set_New                   := dlsym (Library);
         Links.Set_Pop                   := dlsym (Library);
         Links.Set_Size                  := dlsym (Library);
         Links.Sys_GetObject             := dlsym (Library);
--       Links.Sys_GetSizeOf             := dlsym (Library);
         Links.ThreadState_Get           := dlsym (Library);
         Links.ThreadState_Swap          := dlsym (Library);
         Links.True                      := dlsym
                                            (  Library,
                                               "_Py_TrueStruct" & Nul
                                            );
         Links.Tuple_GetItem             := dlsym (Library);
         Links.Tuple_GetSlice            := dlsym (Library);
         Links.Tuple_New                 := dlsym (Library);
         Links.Tuple_SetItem             := dlsym (Library);
         Links.Tuple_Size                := dlsym (Library);
         Links.Type_FromSpec             := dlsym (Library);
         Links.Type_FromSpecWithBases    := dlsym (Library);
         Links.Type_GetSlot              := dlsym (Library);
         Links.Type_IsSubtype            := dlsym (Library);
         Links.Unicode_AsEncodedString   := dlsym (Library);
         Links.Unicode_DecodeFSDefault   := dlsym (Library);
         Links.Unicode_FromString        := dlsym (Library);
         Links.Unicode_FromStringAndSize := dlsym (Library);
         Links.Unicode_GetLength         := dlsym (Library);
         Links.Unicode_ReadChar          := dlsym (Library);

         Check_Links (Name);
      end;
      Loaded := True;
   end Load;
end Py.Load_Python_Library;
