--                                                                    --
--  package Py                      Copyright (c)  Dmitry A. Kazakov  --
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

with Ada.IO_Exceptions;    use Ada.IO_Exceptions;
with Strings_Edit;         use Strings_Edit;
with Strings_Edit.Quoted;  use Strings_Edit.Quoted;
with Strings_Edit.UTF8;    use Strings_Edit.UTF8;

with System.Address_To_Access_Conversions;
with Ada.Characters.Latin_1;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Directories;
with Ada.Tags;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Strings_Edit.ISO_8601;
with Strings_Edit.UTF8.Categorization;
with Py.Load_Python_Library;

package body Py is
--
-- Python library bug work-around.  The exported  exception  objects are
-- broken. The true object will be detected by other means.
--
   TypeError   : Object := Null_Object;
   SystemError : Object := Null_Object;

   Too_Large_Integer : constant String := "Integer value is too large";

   procedure Load (Name : String := "") is
   begin
      Py.Load_Python_Library.Load (Name);
   end Load;

   procedure Check_Error is
   begin
      if Null_Object /= Links.Err_Occurred.all then
         declare
            Error_Type  : Object := Null_Object;
            Error_Value : Object := Null_Object;
            Traceback   : Object := Null_Object;
            function Message return String is
               Encoded : Handle;
               Ptr     : aliased Char_Ptrs.Pointer;
               Size    : aliased ssize_t;
            begin
               if Error_Value = Null_Object then
                  return "[No error message]";
               end if;
               Encoded.Ptr :=
                  Links.Unicode_AsEncodedString (Error_Value);
               if Encoded.Ptr = Null_Object then
                  Encoded.Ptr := Links.Object_Str (Error_Type);
                  if Encoded.Ptr = Null_Object then
                     Err_Clear;
                     return "[Malcoded error message]";
                  else
                     Size := Links.Unicode_GetLength (Encoded.Ptr);
                     if Size < 0 then
                        Err_Clear;
                        return "[Malformed type]";
                     elsif Size = 0 then
                        return "";
                     else
                        Err_Clear;
                        declare
                           Result  : String (1..Integer (Size * 4));
                           Pointer : Integer := 1;
                        begin
                           for Index in 0..Size - 1 loop
                              Put
                              (  Result,
                                 Pointer,
                                 Code_Point
                                 (  Wide_Wide_Character'Pos
                                    (  Links.Unicode_ReadChar
                                       (  Encoded.Ptr,
                                          Index
                              )  )  )  );
                              if Is_Err_Occurred then
                                 Err_Clear;
                                 return "[Malcoded type]";
                              end if;
                           end loop;
                           return Result (1..Pointer - 1);
                        end;
                     end if;
                  end if;
               else
                  if ( -1
                     =  Links.Bytes_AsStringAndSize
                        (  Encoded.Ptr,
                           Ptr'Access,
                           Size'Access
                     )  )  then
                     return "[Malformed message]";
                  else
                     return To_Ada
                            (  Char_Ptrs.Value (Ptr, ptrdiff_t (Size)),
                               False
                            );
                  end if;
               end if;
            end Message;
         begin
            Links.Err_Fetch (Error_Type, Error_Value, Traceback);
            Links.Err_NormalizeException
            (  Error_Type,
               Error_Value,
               Traceback
            );
            declare
               Text : constant String := Message;
            begin
               Links.Err_Restore (Error_Type, Error_Value, Traceback);
               Raise_Exception (Python_Error'Identity, Text);
            end;
         end;
      end if;
   end Check_Error;

   Reserved : Keyword_Tables.Table;

   procedure Check_Links (Library_Name : String) is
      procedure Error (Entry_Name : String) is
      begin
         Raise_Exception
         (  Use_Error'Identity,
            (  "Failed to locate entry "
            &  Entry_Name
            &  " in the Python DLL "
            &  Quote (Library_Name)
         )  );
      end Error;
   begin
      if Links.AddPendingCall = null then
         Error ("Py_AddPendingCall");
      elsif Links.Bool_FromLong = null then
         Error ("PyBool_FromLong");
      elsif Links.ByteArray_AsString = null then
         Error ("PyByteArray_AsString");
      elsif Links.ByteArray_FromStringAndSize = null then
         Error ("PyByteArray_FromStringAndSize");
      elsif Links.ByteArray_Size = null then
         Error ("PyByteArray_Size");
      elsif Links.ByteArray_Type = Null_Object then
         Error ("PyByteArray_Type");
      elsif Links.Bytes_AsStringAndSize = null then
         Error ("PyBytes_AsStringAndSize");
      elsif Links.Bytes_FromStringAndSize = null then
         Error ("PyBytes_FromStringAndSize");
      elsif Links.Bytes_Size = null then
         Error ("PyBytes_Size");
      elsif Links.Bytes_Type = Null_Object then
         Error ("PyBytes_Type");
      elsif Links.Capsule_GetContext = null then
         Error ("PyCapsule_GetContext");
      elsif Links.Capsule_GetDestructor = null then
         Error ("PyCapsule_GetDestructor");
      elsif Links.Capsule_GetName = null then
         Error ("PyCapsule_GetName");
      elsif Links.Capsule_GetPointer = null then
         Error ("PyCapsule_GetPointer");
      elsif Links.Capsule_Import = null then
         Error ("PyCapsule_Import");
      elsif Links.Capsule_IsValid = null then
         Error ("PyCapsule_IsValid");
      elsif Links.Capsule_New = null then
         Error ("PyCapsule_New");
      elsif Links.Capsule_SetContext = null then
         Error ("PyCapsule_SetContext");
      elsif Links.Capsule_SetDestructor = null then
         Error ("PyCapsule_SetDestructor");
      elsif Links.Capsule_SetName = null then
         Error ("PyCapsule_SetName");
      elsif Links.Capsule_SetPointer = null then
         Error ("PyCapsule_SetPointer");
      elsif Links.Callable_Check = null then
         Error ("PyCallable_Check");
      elsif Links.CompileString = null then
         Error ("Py_CompileString");
      elsif Links.DecRef = null then
         Error ("Py_DecRef");
  --  elsif Links.Dict_Check = null then
  --     Error ("Py_Dict_Check");
  --  elsif Links.Dict_CheckExact = null then
  --     Error ("Py_Dict_CheckExact");
      elsif Links.Dict_Clear = null then
         Error ("Py_Dict_Clear");
      elsif Links.Dict_Contains = null then
         Error ("Py_Dict_Contains");
      elsif Links.Dict_Copy = null then
         Error ("Py_Dict_Copy");
      elsif Links.Dict_Keys = null then
         Error ("Py_Dict_Keys");
      elsif Links.Dict_DelItem = null then
         Error ("Py_Dict_DelItem");
      elsif Links.Dict_DelItemString = null then
         Error ("Py_Dict_DelItemString");
      elsif Links.Dict_GetItemString = null then
         Error ("Py_Dict_GetItemString");
      elsif Links.Dict_Merge = null then
         Error ("Py_Dict_Merge");
      elsif Links.Dict_Next = null then
         Error ("Py_Dict_Next");
      elsif Links.Dict_Items = null then
         Error ("Py_Dict_Items");
      elsif Links.Dict_New = null then
         Error ("Py_Dict_New");
--    elsif Links.Dict_SetDefault = null then
--       Error ("Py_Dict_SetDefault");
      elsif Links.Dict_SetItem = null then
         Error ("Py_Dict_SetItem");
      elsif Links.Dict_SetItemString = null then
         Error ("Py_Dict_SetItemString");
      elsif Links.Dict_Size = null then
         Error ("Py_Dict_Size");
      elsif Links.Dict_Type = Null_Object then
         Error ("PyDict_Type");
      elsif Links.EndInterpreter = null then
         Error ("Py_EndInterpreter");
      elsif Links.Err_BadArgument = null then
         Error ("PyErr_BadArgument");
      elsif Links.Err_BadInternalCall = null then
         Error ("PyErr_BadInternalCall");
      elsif Links.Err_Clear = null then
         Error ("PyErr_Clear");
      elsif Links.Err_Fetch = null then
         Error ("PyErr_Fetch");
      elsif Links.Err_NewException = null then
         Error ("PyErr_NewException");
      elsif Links.Err_NormalizeException = null then
         Error ("PyErr_NormalizeException");
      elsif Links.Err_Occurred = null then
         Error ("PyErr_Occurred");
      elsif Links.Err_SetString = null then
         Error ("PyErr_SetString");
      elsif Links.Eval_InitThreads = null then
         Error ("PyEval_InitThreads");
      elsif Links.Eval_RestoreThread = null then
         Error ("PyEval_RestoreThread");
      elsif Links.Eval_SaveThread = null then
         Error ("PyEval_SaveThread");
      elsif Links.Exc_KeyError = null then
         Error ("PyExc_KeyError");
      elsif Links.Exc_LookupError = null then
         Error ("PyExc_LookupError");
      elsif Links.Exc_NameError = null then
         Error ("PyExc_NameError");
      elsif Links.Exc_PermissionError = null then
         Error ("PyExc_RuntimeError");
      elsif Links.Exc_RuntimeError = null then
         Error ("PyExc_RuntimeError");
      elsif Links.Exc_SyntaxError = null then
         Error ("PyExc_SyntaxError");
      elsif Links.Exc_SystemError = null then
         Error ("PyExc_SystemError");
      elsif Links.Exc_TypeError = null then
         Error ("PyExc_TypeError");
      elsif Links.Exc_TimeoutError = null then
         Error ("PyExc_TimeoutError");
      elsif Links.Exc_ValueError = null then
         Error ("PyExc_ValueError");
      elsif Links.FinalizeEx = null then
         Error ("Py_FinalizeEx");
--    elsif Links.GILState_Check = null then
--       Error ("PyGILState_Check");
      elsif Links.GILState_Ensure = null then
         Error ("PyGILState_Ensure");
      elsif Links.GILState_Release = null then
         Error ("PyGILState_Release");
      elsif Links.Float_AsDouble = null then
         Error ("PyFloat_AsDouble");
      elsif Links.Float_FromDouble = null then
         Error ("PyFloat_FromDouble");
      elsif Links.Float_Type = Null_Object then
         Error ("PyFloat_Type");
      elsif Links.Import_AddModule = null then
         Error ("PyImport_AddModule");
      elsif Links.Import_AppendInittab = null then
         Error ("PyImport_AppendInittab");
      elsif Links.Import_ExecCodeModuleEx = null then
         Error ("PyImport_ExecCodeModuleEx");
      elsif Links.Import_GetModule = null then
         Error ("PyImport_GetModule");
      elsif Links.Import_ImportModule = null then
         Error ("PyImport_ImportModule");
      elsif Links.IncRef = null then
         Error ("Py_IncRef");
      elsif Links.InitializeEx = null then
         Error ("Py_InitializeEx");
      elsif Links.Iter_Check = null then
         Error ("PyIter_Check");
      elsif Links.Iter_Next = null then
         Error ("PyIter_Next");
      elsif Links.List_Append = null then
         Error ("PyList_Append");
      elsif Links.List_AsTuple = null then
         Error ("PyList_AsTuple");
--    elsif Links.List_Check = null then
--       Error ("PyList_Check");
      elsif Links.Index_Check = null then
         Error ("PyIndex_Check");
      elsif Links.List_Insert = null then
         Error ("PyList_Insert");
      elsif Links.List_GetItem = null then
         Error ("PyList_GetItem");
      elsif Links.List_GetSlice = null then
         Error ("PyList_GetSlice");
      elsif Links.List_New = null then
         Error ("PyList_New");
      elsif Links.List_Reverse = null then
         Error ("PyList_Reverse");
      elsif Links.List_SetItem = null then
         Error ("PyList_SetItem");
      elsif Links.List_SetSlice = null then
         Error ("PyList_SetSlice");
      elsif Links.List_Size = null then
         Error ("PyList_Size");
      elsif Links.List_Sort = null then
         Error ("PyList_Sort");
      elsif Links.List_Type = Null_Object then
         Error ("PyList_Type");
      elsif Links.Long_AsLong = null then
         Error ("PyLong_AsLong");
      elsif Address (Links.Long_AsLongLong) = Null_Address then
         Error ("PyLong_AsLongLong");
      elsif Links.Long_AsUnsignedLong = null then
         Error ("PyLong_AsUnsignedLong");
      elsif Address (Links.Long_AsUnsignedLongLong) = Null_Address then
         Error ("PyLong_AsUnsignedLongLong");
      elsif Links.Long_FromLong = null then
         Error ("PyLong_FromLong");
      elsif Address (Links.Long_FromLongLong) = Null_Address then
         Error ("PyLong_FromLongLong");
      elsif Links.Long_FromUnsignedLong = null then
         Error ("PyLong_FromUnsignedLong");
      elsif Address (Links.Long_FromUnsignedLongLong) =
                                                       Null_Address then
         Error ("PyLong_FromUnsignedLongLong");
      elsif Links.Long_Type = Null_Object then
         Error ("PyLong_Type");
      elsif Links.Module_AddObject = null then
         Error ("PyModule_AddObject");
      elsif Links.Module_Create = null then
         Error ("PyModule_Create2");
      elsif Links.Module_GetDict = null then
         Error ("PyModule_GetDict");
      elsif Links.Module_GetName = null then
         Error ("PyModule_GetName");
      elsif Links.NewInterpreter = null then
         Error ("Py_NewInterpreter");
      elsif Links.None = Null_Object then
         Error ("_PyNone_Struct");
      elsif Links.Number_Absolute = null then
         Error ("PyNumber_Absolute");
      elsif Links.Number_Add = null then
         Error ("PyNumber_Add");
      elsif Links.Number_And = null then
         Error ("PyNumber_And");
      elsif Links.Number_AsSsize_t = null then
         Error ("PyNumber_AsSsize_t");
      elsif Links.Number_Check = null then
         Error ("PyNumber_Check");
      elsif Links.Number_Divmod = null then
         Error ("PyNumber_Divmod");
      elsif Links.Number_Float = null then
         Error ("PyNumber_Float");
      elsif Links.Number_FloorDivide = null then
         Error ("PyNumber_FloorDivide");
      elsif Links.Number_Index = null then
         Error ("PyNumber_Index");
      elsif Links.Number_InPlaceAdd = null then
         Error ("PyNumber_InPlaceAdd");
      elsif Links.Number_InPlaceAnd = null then
         Error ("PyNumber_InPlaceAnd");
      elsif Links.Number_InPlaceFloorDivide = null then
         Error ("PyNumber_InPlaceFloorDivide");
      elsif Links.Number_InPlaceLshift = null then
         Error ("PyNumber_InPlaceLshift");
      elsif Links.Number_InPlaceMatrixMultiply = null then
         Error ("PyNumber_InPlaceMatrixMultiply");
      elsif Links.Number_InPlaceMultiply = null then
         Error ("PyNumber_InPlaceMultiply");
      elsif Links.Number_InPlaceOr = null then
         Error ("PyNumber_InPlaceOr");
      elsif Links.Number_InPlacePower = null then
         Error ("PyNumber_InPlacePower");
      elsif Links.Number_InPlaceRemainder = null then
         Error ("PyNumber_InPlaceRemainder");
      elsif Links.Number_InPlaceRshift = null then
         Error ("PyNumber_InPlaceRshift");
      elsif Links.Number_InPlaceSubtract = null then
         Error ("PyNumber_InPlaceSubtract");
      elsif Links.Number_InPlaceTrueDivide = null then
         Error ("PyNumber_InPlaceTrueDivide");
      elsif Links.Number_InPlaceXor = null then
         Error ("PyNumber_InPlaceXor");
      elsif Links.Number_Invert = null then
         Error ("PyNumber_Invert");
      elsif Links.Number_Long = null then
         Error ("PyNumber_Long");
      elsif Links.Number_Lshift = null then
         Error ("PyNumber_Lshift");
      elsif Links.Number_MatrixMultiply = null then
         Error ("PyNumber_MatrixMultiply");
      elsif Links.Number_Multiply = null then
         Error ("PyNumber_Multiply");
      elsif Links.Number_Negative = null then
         Error ("PyNumber_Negative");
      elsif Links.Number_Or = null then
         Error ("PyNumber_Or");
      elsif Links.Number_Positive = null then
         Error ("PyNumber_Positive");
      elsif Links.Number_Power = null then
         Error ("PyNumber_Power");
      elsif Links.Number_Remainder = null then
         Error ("PyNumber_Remainder");
      elsif Links.Number_Rshift = null then
         Error ("PyNumber_Rshift");
      elsif Links.Number_Subtract = null then
         Error ("PyNumber_Subtract");
      elsif Links.Number_ToBase = null then
         Error ("PyNumber_ToBase");
      elsif Links.Number_TrueDivide = null then
         Error ("PyNumber_TrueDivide");
      elsif Links.Object_Bytes = null then
         Error ("PyObject_Bytes");
--    elsif Links.Object_CallNoArgs = null then
--       Error ("PyObject_CallNoArgs");
      elsif Links.Object_CallObject = null then
         Error ("PyObject_CallObject");
      elsif Links.Object_DelItem = null then
         Error ("PyObject_DelItem");
      elsif Links.Object_Dir = null then
         Error ("PyObject_Dir");
      elsif Links.Object_GetAttr = null then
         Error ("PyObject_GetAttr");
      elsif Links.Object_GetAttrString = null then
         Error ("PyObject_GetAttrString");
      elsif Links.Object_GetItem = null then
         Error ("PyObject_GetItem");
      elsif Links.Object_GetIter = null then
         Error ("PyObject_GetIter");
      elsif Links.Object_GenericGetAttr = null then
         Error ("PyObject_GenericGetAttr");
      elsif Links.Object_GenericSetAttr = null then
         Error ("PyObject_GenericSetAttr");
      elsif Links.Object_HasAttr = null then
         Error ("PyObject_HasAttr");
      elsif Links.Object_HasAttrString = null then
         Error ("PyObject_HasAttrString");
      elsif Links.Object_IsInstance = null then
         Error ("PyObject_IsInstance");
      elsif Links.Object_IsSubclass = null then
         Error ("PyObject_IsSubclass");
      elsif Links.Object_RichCompareBool = null then
         Error ("PyObject_RichCompareBool");
      elsif Links.Object_SetAttr = null then
         Error ("PyObject_SetAttr");
      elsif Links.Object_SetAttrString = null then
         Error ("PyObject_SetAttrString");
      elsif Links.Object_SetItem = null then
         Error ("PyObject_SetItem");
      elsif Links.Object_Size = null then
         Error ("PyObject_Size");
      elsif Links.Object_Str = null then
         Error ("PyObject_Str");
      elsif Links.Object_Type = null then
         Error ("PyObject_Type");
      elsif Links.Sequence_Check = null then
         Error ("PySequence_Check");
      elsif Links.Sequence_Concat = null then
         Error ("PySequence_Concat");
      elsif Links.Sequence_Contains = null then
         Error ("PySequence_Contains");
      elsif Links.Sequence_Count = null then
         Error ("PySequence_Count");
      elsif Links.Sequence_DelItem = null then
         Error ("PySequence_DelItem");
      elsif Links.Sequence_DelSlice = null then
         Error ("PySequence_DelSlice");
      elsif Links.Sequence_GetItem = null then
         Error ("PySequence_GetItem");
      elsif Links.Sequence_GetSlice = null then
         Error ("PySequence_GetSlice");
      elsif Links.Sequence_Index = null then
         Error ("PySequence_Index");
      elsif Links.Sequence_List = null then
         Error ("PySequence_List");
      elsif Links.Sequence_Repeat = null then
         Error ("PySequence_Repeat");
      elsif Links.Sequence_SetItem = null then
         Error ("PySequence_SetItem");
      elsif Links.Sequence_SetSlice = null then
         Error ("PySequence_SetSlice");
      elsif Links.Sequence_Size = null then
         Error ("PySequence_Size");
      elsif Links.Sequence_Tuple = null then
         Error ("PySequence_Tuple");
      elsif Links.Set_Add = null then
         Error ("PySet_Add");
      elsif Links.Set_Clear = null then
         Error ("PySet_Clear");
      elsif Links.Set_Contains = null then
         Error ("PySet_Contains");
      elsif Links.Set_Discard = null then
         Error ("PySet_Discard_Ptr");
      elsif Links.FrozenSet_New = null then
         Error ("PyFrozenset_New");
      elsif Links.Set_New = null then
         Error ("PySet_New");
      elsif Links.Set_Pop = null then
         Error ("PySet_Pop");
      elsif Links.Set_Size = null then
         Error ("PySet_Size");
      elsif Links.Set_Type = Null_Object then
         Error ("PySet_Type");
      elsif Links.Sys_GetObject = null then
         Error ("PySys_GetObject");
--    elsif Links.Sys_GetSizeOf = null then
--       Error ("_PySys_GetSizeOf");
      elsif Links.ThreadState_Get = null then
         Error ("PyThreadState_Get");
      elsif Links.ThreadState_Swap = null then
         Error ("PyThreadState_Swap");
      elsif Links.Unicode_AsEncodedString = null then
         Error ("PyUnicode_AsEncodedString");
      elsif Links.Tuple_GetItem = null then
         Error ("PyTuple_GetItem");
      elsif Links.Tuple_GetSlice = null then
         Error ("PyTuple_GetSlice");
      elsif Links.Tuple_New = null then
         Error ("PyTuple_New");
      elsif Links.Tuple_SetItem = null then
         Error ("PyTuple_SetItem");
      elsif Links.Tuple_Size = null then
         Error ("PyTuple_Size");
      elsif Links.Tuple_Type = Null_Object then
         Error ("PyTuple_Type");
      elsif Links.Type_FromSpec = null then
         Error ("PyType_Type_FromSpec");
      elsif Links.Type_FromSpecWithBases = null then
         Error ("PyType_FromSpecWithBases");
      elsif Links.Type_IsSubtype = null then
         Error ("PyType_IsSubtype");
      elsif Links.Type_Type = Null_Object then
         Error ("PyType_Type");
      elsif Links.Unicode_GetLength = null then
         Error ("Unicode_GetLength");
      elsif Links.Unicode_DecodeFSDefault = null then
         Error ("PyUnicode_DecodeFSDefault");
      elsif Links.Unicode_FromString = null then
         Error ("Unicode_FromString");
      elsif Links.Unicode_FromStringAndSize = null then
         Error ("Unicode_FromStringAndSize");
      elsif Links.Unicode_ReadChar = null then
         Error ("Unicode_ReadChar");
      elsif Links.Unicode_Type = Null_Object then
         Error ("PyUnicode_Type");
      end if;
      Reserved.Add ("False",    True);
      Reserved.Add ("None",     True);
      Reserved.Add ("True",     True);
      Reserved.Add ("and",      True);
      Reserved.Add ("as",       True);
      Reserved.Add ("assert",   True);
      Reserved.Add ("break",    True);
      Reserved.Add ("class",    True);
      Reserved.Add ("continue", True);
      Reserved.Add ("def",      True);
      Reserved.Add ("del",      True);
      Reserved.Add ("elif",     True);
      Reserved.Add ("else",     True);
      Reserved.Add ("except",   True);
      Reserved.Add ("finally",  True);
      Reserved.Add ("for",      True);
      Reserved.Add ("from",     True);
      Reserved.Add ("global",   True);
      Reserved.Add ("if",       True);
      Reserved.Add ("import",   True);
      Reserved.Add ("in",       True);
      Reserved.Add ("is",       True);
      Reserved.Add ("lambda",   True);
      Reserved.Add ("nonlocal", True);
      Reserved.Add ("not",      True);
      Reserved.Add ("or",       True);
      Reserved.Add ("pass",     True);
      Reserved.Add ("raise",    True);
      Reserved.Add ("return",   True);
      Reserved.Add ("try",      True);
      Reserved.Add ("while",    True);
      Reserved.Add ("with",     True);
      Reserved.Add ("yield",    True);
   end Check_Links;

   procedure Check_Handle (Object : Handle) is
      pragma Inline (Check_Handle);
   begin
      if Object.Ptr = Null_Object then
         Raise_Exception (Constraint_Error'Identity, "Null object");
      end if;
   end Check_Handle;

   procedure Check_Spelling (Name : String) is
      use Keyword_Tables;
      Offset : constant Integer := Locate (Reserved, Name);
   begin
      if Offset > 0 then
         Raise_Exception
         (  Use_Error'Identity,
            (  Quote (GetName (Reserved, Offset))
            &  " is a reserved keyword"
         )  );
      end if;
   end Check_Spelling;

   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean is
      use Strings_Edit.UTF8.Categorization;
      Index : Integer;
      This  : UTF8_Code_Point;
   begin
      Index := Pointer;
      Get (Source, Index, This);
      if not Is_Alphanumeric (This) then
         return True;
      end if;
      Index := Pointer;
      Get_Backwards (Source, Index, This);
      return not Is_Alphanumeric (This);
   exception
      when others =>
         return True;
   end Check_Matched;

   function As_Integer64 (Value : Object)
      return Interfaces.Integer_64 is
      function To_Integer
               (  Ptr      : Long_AsLongLong_Ptr;
                  Data     : Py.Object;
                  Overflow : access int
               )  return Interfaces.Integer_64;
      pragma Import (C, To_Integer, "long_toint64");
      Result   : Interfaces.Integer_64;
      Overflow : aliased int;
   begin
      Result :=
         To_Integer (Links.Long_AsLongLong, Value, Overflow'Access);
      Check_Error;
      if Overflow = 0 then
         return Result;
      else
         Raise_Exception (Constraint_Error'Identity, Too_Large_Integer);
      end if;
   end As_Integer64;

   function As_String (Value : Object) return String is
      Length : ssize_t;
   begin
      Length := Links.Unicode_GetLength (Value);
      if Length < 0 then
         Check_Error;
         return "";
      elsif Length = 0 then
         return "";
      else
         declare
            Result  : String (1..Integer (Length * 4));
            Pointer : Integer := 1;
         begin
            for Index in 0..Length - 1 loop
               Put
               (  Result,
                  Pointer,
                  Code_Point
                  (  Wide_Wide_Character'Pos
                     (  Links.Unicode_ReadChar (Value, Index)
               )  )  );
            end loop;
            Check_Error;
            return Result (1..Pointer - 1);
         end;
      end if;
   end As_String;

   function As_String (Object : Handle) return String is
   begin
      Check_Handle (Object);
      return As_String (Object.Ptr);
   end As_String;

   function As_Unsigned64 (Value : Object)
      return Interfaces.Unsigned_64 is
      function To_Integer
               (  Ptr      : Long_AsUnsignedLongLong_Ptr;
                  Data     : Py.Object;
                  Overflow : access int
               )  return Interfaces.Unsigned_64;
      pragma Import (C, To_Integer, "long_touint64");
      Result   : Interfaces.Unsigned_64;
      Overflow : aliased int;
   begin
      Result := To_Integer
                (  Links.Long_AsUnsignedLongLong,
                   Value,
                   Overflow'Access
                );
      Check_Error;
      if Overflow = 0 then
         return Result;
      else
         Raise_Exception (Constraint_Error'Identity, Too_Large_Integer);
      end if;
   end As_Unsigned64;

   function ByteArray_AsString (Object : Handle) return String is
      use Char_Ptrs;
      Ptr  : aliased Pointer;
      Size : constant ssize_t := ByteArray_Size (Object);
   begin
      if Size = 0 then
         return "";
      end if;
      Ptr := Links.ByteArray_AsString (Object.Ptr);
      declare
         Result : String (1..Natural (Size));
      begin
         for Index in Result'Range loop
            Result (Index) := To_Ada (Ptr.all);
            Increment (Ptr);
         end loop;
         return Result;
      end;
   end ByteArray_AsString;

   function ByteArray_AsString (Object : Handle)
      return Stream_Element_Array is
      use Char_Ptrs;
      Ptr  : aliased Pointer;
      Size : constant ssize_t := ByteArray_Size (Object);
   begin
      if Size = 0 then
         return (1..0 => 0);
      end if;
      Ptr := Links.ByteArray_AsString (Object.Ptr);
      declare
         Result : Stream_Element_Array
                  (  1
                  .. Stream_Element_Count (Size)
                  );
      begin
         for Index in Result'Range loop
            Result (Index) := char'Pos (Ptr.all);
            Increment (Ptr);
         end loop;
         return Result;
      end;
   end ByteArray_AsString;

   function ByteArray_FromString (Value : String) return Handle is
      Result : Handle;
   begin
      if Value'Length = 0 then
         Result.Ptr :=
            Links.ByteArray_FromStringAndSize (Null_Address, 0);
      else
         Result.Ptr :=
            Links.ByteArray_FromStringAndSize
            (  Value'Address,
               Value'Length
            );
      end if;
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end ByteArray_FromString;

   function ByteArray_FromString (Value : Stream_Element_Array)
      return Handle is
      Result : Handle;
   begin
      if Value'Length = 0 then
         Result.Ptr :=
            Links.ByteArray_FromStringAndSize (Null_Address, 0);
      else
         Result.Ptr :=
            Links.ByteArray_FromStringAndSize
            (  Value'Address,
               Value'Length
            );
      end if;
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end ByteArray_FromString;

   function ByteArray_Size (Object : Handle) return ssize_t is
      Result : ssize_t;
   begin
      Check_Handle (Object);
      Result := Links.ByteArray_Size (Object.Ptr);
      if Result < 0 then
         Check_Error;
      end if;
      return Result;
   end ByteArray_Size;

   function ByteArray_Type return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.ByteArray_Type;
      Links.IncRef (Result.Ptr);
      return Result;
   end ByteArray_Type;

   function Bytes_AsString (Object : Handle) return String is
      use Char_Ptrs;
      Ptr  : aliased Pointer;
      Size : aliased ssize_t := 0;
   begin
      Check_Handle (Object);
      if -1 = Links.Bytes_AsStringAndSize
              (  Object.Ptr,
                 Ptr'Access,
                 Size'Access
              )  then
         Check_Error;
      end if;
      if Ptr = null or else Size = 0 then
         return "";
      else
         declare
            Result : String (1..Integer (Size));
         begin
            for Index in Result'Range loop
               Result (Index) := To_Ada (Ptr.all);
               Increment (Ptr);
            end loop;
            return Result;
         end;
      end if;
   end Bytes_AsString;

   function Bytes_AsString (Object : Handle)
      return Stream_Element_Array is
      use Char_Ptrs;
      Ptr  : aliased Pointer;
      Size : aliased ssize_t := 0;
   begin
      Check_Handle (Object);
      if -1 = Links.Bytes_AsStringAndSize
              (  Object.Ptr,
                 Ptr'Access,
                 Size'Access
              )  then
         Check_Error;
      end if;
      if Ptr = null or else Size = 0 then
         return (1..0 => 0);
      else
         declare
            Result : Stream_Element_Array
                     (  1
                     .. Stream_Element_Count (Size)
                     );
         begin
            for Index in Result'Range loop
               Result (Index) := char'Pos (Ptr.all);
               Increment (Ptr);
            end loop;
            return Result;
         end;
      end if;
   end Bytes_AsString;

   function Bytes_FromString (Value : String) return Handle is
      Result : Handle;
   begin
      if Value'Length = 0 then
         Result.Ptr :=
            Links.Bytes_FromStringAndSize (Null_Address, 0);
      else
         Result.Ptr :=
            Links.Bytes_FromStringAndSize (Value'Address, Value'Length);
      end if;
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Bytes_FromString;

   function Bytes_FromString (Value : Stream_Element_Array)
      return Handle is
      Result : Handle;
   begin
      if Value'Length = 0 then
         Result.Ptr :=
            Links.Bytes_FromStringAndSize (Null_Address, 0);
      else
         Result.Ptr :=
            Links.Bytes_FromStringAndSize (Value'Address, Value'Length);
      end if;
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Bytes_FromString;

   function Bytes_Size (Object : Handle) return ssize_t is
      Result : ssize_t;
   begin
      Check_Handle (Object);
      Result := Links.Bytes_Size (Object.Ptr);
      if Result < 0 then
         Check_Error;
      end if;
      return Result;
   end Bytes_Size;

   function Bytes_Type return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.Bytes_Type;
      Links.IncRef (Result.Ptr);
      return Result;
   end Bytes_Type;

   function Callable_Check (Object : Handle) return int is
   begin
      Check_Handle (Object);
      return Links.Callable_Check (Object.Ptr);
   end Callable_Check;

   Single_Input    : constant := 256;
   File_Input      : constant := 257;
   Eval_Input      : constant := 258;
   Func_Type_Input : constant := 345;

   function Compile
            (  Source    : String;
               File_Name : String
            )  return Handle is

      Module : Handle;
      Func   : Handle;
   begin
      Compile (Source, File_Name, Module, Func);
      return Func;
   end Compile;

   procedure Compile
             (  Source      : String;
                File_Name   : String;
                Module      : out Handle;
                Entry_Point : out Handle
             )  is
      function Get_Name return String is
         Line_Start : Boolean := True;
         Pointer    : Integer := Source'First;
         Start      : Integer;
      begin
         while Pointer <= Source'Last loop
            case Source (Pointer) is
               when 'd' =>
                  Pointer := Pointer + 1;
                  if Line_Start then
                     if Is_Prefix ("ef", Source, Pointer) then
                        Pointer := Pointer + 2;
                        Start   := Pointer;
                        Get (Source, Pointer);
                        if Start /= Pointer then
                           Start := Pointer;
                           while Pointer <= Source'Last loop
                              case Source (Pointer) is
                                 when Character'Val (9)  |
                                      Character'Val (13) |
                                      ' '                |
                                      '('                =>
                                    return Source (Start..Pointer - 1);
                                 when others =>
                                    Pointer := Pointer + 1;
                              end case;
                           end loop;
                           return Source (Start..Pointer - 1);
                        end if;
                     end if;
                  end if;
                  Line_Start := False;
               when Character'Val (10) =>
                  Line_Start := True;
                  Pointer    := Pointer + 1;
               when Character'Val (9) | Character'Val (13) | ' ' =>
                  Pointer := Pointer + 1;
               when others =>
                  Line_Start := False;
                  Pointer := Pointer + 1;
            end case;
         end loop;
         return "";
      end Get_Name;

      Name : constant String := Get_Name;
      Code : Handle;
   begin
      Invalidate (Module);
      Invalidate (Entry_Point);
      if Name'Length = 0 then
         Raise_Exception
         (  Data_Error'Identity,
            "No function definition found"
         );
      end if;
      Code.Ptr :=
         Links.CompileString
         (  To_C (Source),
            To_C (File_Name),
            File_Input
         );
      if Code.Ptr = Null_Object then
         Check_Error;
      end if;
      Module.Ptr :=
         Links.Import_ExecCodeModuleEx
         (  To_C (File_Name & '.' & Name),
            Code.Ptr
         );
      if Module.Ptr = Null_Object then
         Check_Error;
      end if;
      Entry_Point.Ptr :=
         Links.Object_GetAttrString (Module.Ptr, To_C (Name));
      if Entry_Point.Ptr = Null_Object then
         Check_Error;
      end if;
   end Compile;

   --  function Compile
   --           (  Source    : String;
   --              File_Name : String
   --           )  return Handle is
   --     function Get_Name return String is
   --        Line_Start : Boolean := True;
   --        Pointer    : Integer := Source'First;
   --        Start      : Integer;
   --     begin
   --        while Pointer <= Source'Last loop
   --           case Source (Pointer) is
   --              when 'd' =>
   --                 Pointer := Pointer + 1;
   --                 if Line_Start then
   --                    if Is_Prefix ("ef", Source, Pointer) then
   --                       Pointer := Pointer + 2;
   --                       Start   := Pointer;
   --                       Get (Source, Pointer);
   --                       if Start /= Pointer then
   --                          Start := Pointer;
   --                          while Pointer <= Source'Last loop
   --                             case Source (Pointer) is
   --                                when Character'Val (9)  |
   --                                     Character'Val (13) |
   --                                     ' '                |
   --                                     '('                =>
   --                                   return Source (Start..Pointer - 1);
   --                                when others =>
   --                                   Pointer := Pointer + 1;
   --                             end case;
   --                          end loop;
   --                          return Source (Start..Pointer - 1);
   --                       end if;
   --                    end if;
   --                 end if;
   --                 Line_Start := False;
   --              when Character'Val (10) =>
   --                 Line_Start := True;
   --                 Pointer    := Pointer + 1;
   --              when Character'Val (9) | Character'Val (13) | ' ' =>
   --                 Pointer := Pointer + 1;
   --              when others =>
   --                 Line_Start := False;
   --                 Pointer := Pointer + 1;
   --           end case;
   --        end loop;
   --        return "";
   --     end Get_Name;
   --
   --     Name   : constant String := Get_Name;
   --     Code   : Handle;
   --     Module : Handle;
   --     Func   : Handle;
   --  begin
   --     if Name'Length = 0 then
   --        Raise_Exception
   --        (  Data_Error'Identity,
   --           "No function definition found"
   --        );
   --     end if;
   --     Code.Ptr :=
   --        Links.CompileString
   --        (  To_C (Source),
   --           To_C (File_Name),
   --           File_Input
   --        );
   --     if Code.Ptr = Null_Object then
   --        Check_Error;
   --     end if;
   --     Module.Ptr :=
   --        Links.Import_ExecCodeModuleEx
   --        (  To_C (File_Name & '.' & Name),
   --           Code.Ptr
   --        );
   --     if Module.Ptr = Null_Object then
   --        Check_Error;
   --     end if;
   --     Func.Ptr := Links.Object_GetAttrString (Module.Ptr, To_C (Name));
   --     if Func.Ptr = Null_Object then
   --        Check_Error;
   --     end if;
   --     return Func;
   --  end Compile;

   procedure Dict_DelItem (Dictionary : Handle; Key : Handle) is
   begin
      Check_Handle (Dictionary);
      Check_Handle (Key);
      case Links.Dict_DelItem (Dictionary.Ptr, Key.Ptr) is
         when 0 =>
            null;
         when others =>
            Check_Error;
      end case;
   end Dict_DelItem;

   procedure Dict_DelItemString (Dictionary : Handle; Key : String) is
   begin
      Check_Handle (Dictionary);
      case Links.Dict_DelItemString (Dictionary.Ptr, To_C (Key)) is
         when 0 =>
            null;
         when others =>
            Check_Error;
      end case;
   end Dict_DelItemString;

   function Dict_GetItemString
            (  Dictionary : Handle;
               Key        : String
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Dictionary);
      Result.Ptr :=
         Links.Dict_GetItemString (Dictionary.Ptr, To_C (Key));
      if Result.Ptr /= Null_Object then -- Borrowed reference
         Links.IncRef (Result.Ptr);
      end if;
      return Result;
   end Dict_GetItemString;

--  function Dict_Check (Dictionary : Handle; Exact : Boolean := False)
--     return Boolean is
--  begin
--     Check_Handle (Dictionary);
--     if Exact then
--        return 0 /= Links.Dict_CheckExact (Dictionary.Ptr);
--     else
--        return 0 /= Links.Dict_Check (Dictionary.Ptr);
--     end if;
--  end Dict_Check;

   procedure Dict_Clear (Dictionary : Handle) is
   begin
      Check_Handle (Dictionary);
      Links.Dict_Clear (Dictionary.Ptr);
   end Dict_Clear;

   function Dict_Contains (Dictionary : Handle; Key : Handle)
      return Boolean is
   begin
      Check_Handle (Dictionary);
      Check_Handle (Key);
      case Links.Dict_Contains (Dictionary.Ptr, Key.Ptr) is
         when 0 =>
            return False;
         when 1 =>
            return True;
         when others =>
            Raise_Exception
            (  Python_Error'Identity,
               "Error in Dict_Contains"
            );
      end case;
   end Dict_Contains;

   function Dict_Copy (Dictionary : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Dictionary);
      Result.Ptr := Links.Dict_Copy (Dictionary.Ptr);
      if Result.Ptr = Null_Object then -- New reference
         Check_Error;
      end if;
      return Result;
   end Dict_Copy;

   function Dict_Keys (Dictionary : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Dictionary);
      Result.Ptr := Links.Dict_Keys (Dictionary.Ptr);
      if Result.Ptr = Null_Object then -- New reference
         Check_Error;
      end if;
      return Result;
   end Dict_Keys;

   procedure Dict_Merge
             (  Dictionary : Handle;
                Update     : Handle;
                Override   : Boolean
             )  is
   begin
      Check_Handle (Dictionary);
      Check_Handle (Update);
      if Override then
         if 0 = Links.Dict_Merge (Dictionary.Ptr, Update.Ptr, 1) then
            return;
         end if;
      else
         if 0 = Links.Dict_Merge (Dictionary.Ptr, Update.Ptr, 0) then
            return;
         end if;
      end if;
      Raise_Exception
      (  Python_Error'Identity,
         "Dictionaries merging error"
      );
   end Dict_Merge;

   function Dict_Next
            (  Dictionary : Handle;
               Position   : access ssize_t;
               Key        : access Handle;
               Value      : access Handle
            )  return Boolean is
   begin
      Check_Handle (Dictionary);
      Invalidate (Key.all);
      Invalidate (Value.all);
      if 0 = Links.Dict_Next
             (  Dictionary.Ptr,
                Position,
                Key.Ptr'Access,
                Value.Ptr'Access
             )  then
         return False;
      else
         Links.IncRef (Key.Ptr);
         Links.IncRef (Value.Ptr);
         return True;
      end if;
   end Dict_Next;

   function Dict_Items (Dictionary : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Dictionary);
      Result.Ptr := Links.Dict_Items (Dictionary.Ptr);
      if Result.Ptr = Null_Object then -- New reference
         Check_Error;
      end if;
      return Result;
   end Dict_Items;

   function Dict_New return Handle is
   begin
      return (Ada.Finalization.Controlled with Links.Dict_New.all);
   end Dict_New;

--  function Dict_SetDefault
--           (  Dictionary : Handle;
--              Key        : Handle;
--              Default    : Handle
--           )  return Handle is
--     Result : Handle;
--  begin
--     Check_Handle (Dictionary);
--     Check_Handle (Key);
--     Check_Handle (Default);
--     Result.Ptr := Links.Dict_SetDefault
--                   (  Dictionary.Ptr,
--                      Key.Ptr,
--                      Default.Ptr
--                   );
--     Links.IncRef (Result.Ptr);
--     return Result;
--  end Dict_SetDefault;

   procedure Dict_SetItem
             (  Dictionary : Handle;
                Key        : Handle;
                Value      : Handle
             ) is
   begin
      Check_Handle (Dictionary);
      Check_Handle (Key);
      Check_Handle (Value);
      if 0 /= Links.Dict_SetItem
              (  Dictionary.Ptr,
                 Key.Ptr,
                 Value.Ptr
              )
      then
         Raise_Exception
         (  Data_Error'Identity,
            "Failed to set dictionary item"
         );
      end if;
   end Dict_SetItem;

   procedure Dict_SetItemString
             (  Dictionary : Handle;
                Key        : String;
                Value      : Handle
             ) is
   begin
      Check_Handle (Dictionary);
      Check_Handle (Value);
      if 0 /= Links.Dict_SetItemString
              (  Dictionary.Ptr,
                 To_C (Key),
                 Value.Ptr
              )
      then
         Raise_Exception
         (  Data_Error'Identity,
            "Failed to set dictionary item by string"
         );
      end if;
   end Dict_SetItemString;

   function Dict_Size (Dictionary : Handle) return ssize_t is
      Result : ssize_t;
   begin
      Check_Handle (Dictionary);
      Result := Links.Dict_Size (Dictionary.Ptr);
      Check_Error;
      return Result;
   end Dict_Size;

   function Dict_Type return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.Dict_Type;
      Links.IncRef (Result.Ptr);
      return Result;
   end Dict_Type;

   procedure Err_Clear is
   begin
      Links.Err_Clear.all;
   end Err_Clear;

   function Err_Occurred return Handle is
   begin
      return (Ada.Finalization.Controlled with Links.Err_Occurred.all);
   end Err_Occurred;

   procedure Eval_InitThreads is
   begin
      Links.Eval_InitThreads.all;
   end Eval_InitThreads;

   procedure Eval_RestoreThread (State : ThreadState) is
   begin
      Links.Eval_RestoreThread (State);
   end Eval_RestoreThread;

   function Eval_SaveThread return ThreadState is
   begin
      return Links.Eval_SaveThread.all;
   end Eval_SaveThread;

   procedure Finalize (Lock : in out Global_Interpreter_Lock) is
   begin
      Links.GILState_Release (Lock.State);
   end Finalize;

   function FinalizeEx return int is
   begin
      TypeError   := Null_Object;
      SystemError := Null_Object;
      return Links.FinalizeEx.all;
   end FinalizeEx;

   function Float_AsDouble (Object : Handle) return double is
      Result : constant double := Links.Float_AsDouble (Object.Ptr);
   begin
      Check_Error;
      return Result;
   end Float_AsDouble;

   function Float_FromDouble (Value : double) return Handle is
      Result : constant Object := Links.Float_FromDouble (Value);
   begin
      if Result = Null_Object then
         Check_Error;
      end if;
      return (Ada.Finalization.Controlled with Result);
   end Float_FromDouble;

   function Float_Type return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.Float_Type;
      Links.IncRef (Result.Ptr);
      return Result;
   end Float_Type;

-- function GILState_Check return Boolean is
-- begin
--    return 0 /= Links.GILState_Check.all;
-- end GILState_Check;

   procedure Initialize is
   begin
      Links.InitializeEx (0);
   end Initialize;

   function Import
            (  File_Name  : String;
               Entry_Name : String
            )  return Handle is
      use Ada.Directories;
      Name_Start  : Integer := File_Name'First;
      Name_Stop   : Integer := File_Name'Last;
      System_Path : Handle;
      Module_Name : Handle;
      Module      : Handle;
      Result      : Handle;
   begin
      for Index in reverse File_Name'Range loop
         case File_Name (Index) is
            when '.' =>
               if Name_Stop = File_Name'Last then
                  Name_Stop := Index - 1;
               end if;
            when '/' | '\' =>
               Name_Start := Index + 1;
               exit;
            when others =>
               null;
         end case;
      end loop;
      if Name_Stop < Name_Start then
         Raise_Exception
         (  Ada.IO_Exceptions.Name_Error'Identity,
            "Module name is empty"
         );
      end if;
      Module_Name :=
         Unicode_FromString (File_Name (Name_Start..Name_Stop));
      Module.Ptr := Links.Import_GetModule (Module_Name.Ptr);
      if Module.Ptr = Null_Object then
         System_Path := Sys_GetObject ("path");
         declare
            Restore_Path : Boolean := False;
         begin
            if Is_Valid (System_Path) then
               if Name_Start - 1 > File_Name'First then -- Have a path
                  List_Insert
                  (  System_Path,
                     0,
                     Unicode_FromString
                     (  File_Name (File_Name'First..Name_Start - 2)
                  )  );
               else
                  List_Insert
                  (  System_Path,
                     0,
                     Unicode_FromString (Current_Directory)
                  );
               end if;
               Restore_Path := True;
            end if;
            Module.Ptr := Links.Import_Import (Module_Name.Ptr);
            if Module.Ptr = Null_Object then
               Check_Error;
            end if;
            if Restore_Path then
               if 0 > Links.List_SetSlice
                      (  System_Path.Ptr,
                         0,
                         0,
                         Null_Object
                      )  then
                  Check_Error;
               end if;
            end if;
         exception
            when others =>
               if Restore_Path then
                  if 0 > Links.List_SetSlice
                         (  System_Path.Ptr,
                            0,
                            0,
                            Null_Object
                         )  then
                     null;
                  end if;
               end if;
               raise;
         end;
      end if;
      Result := Object_GetAttrString (Module, Entry_Name);
      if Callable_Check (Result) = 0 then
         Raise_Exception
         (  Mode_Error'Identity,
            Entry_Name & " is not callable"
         );
      end if;
      return Result;
   end Import;

   function Import_AddModule (Name : String) return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.Import_AddModule (To_C (Name));
      if Result.Ptr = Null_Object then
         Check_Error;
      else -- Borrowed reference
         Links.IncRef (Result.Ptr);
      end if;
      return Result;
   end Import_AddModule;

   function Import_AppendInittab
            (  Name : char_array;
               Init : InitTab
            )  return int is
   begin
      return Links.Import_AppendInittab (Name, Init);
   end Import_AppendInittab;

   function Import_ExecCodeModuleEx
            (  Name : String;
               Code : Handle;
               Path : String := ""
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Code);
      if Path'Length = 0 then
         Result.Ptr :=
            Links.Import_ExecCodeModuleEx (To_C (Name), Code.Ptr);
      else
         declare
            File_Path : constant char_array := To_C (Path);
         begin
            Result.Ptr :=
               Links.Import_ExecCodeModuleEx
               (  To_C (Name),
                  Code.Ptr,
                  File_Path (File_Path'First)'Unchecked_Access
               );
         end;
      end if;
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Import_ExecCodeModuleEx;

   function Import_GetModule (Name : String) return Handle is
      Result : Handle;
      Object : constant Handle := Unicode_FromString (Name);
   begin
      Result.Ptr := Links.Import_GetModule (Object.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Import_GetModule;

   function Import_Import (Name : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Name);
      Result.Ptr := Links.Import_Import (Name.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Import_Import;

   procedure Import_Import
             (  Name        : Handle;
                Module      : out Handle;
                Destination : in out String;
                Pointer     : in out Integer;
                No_Error    : out Boolean;
                Decorator   : String := Character'Val (13) &
                                        Character'Val (10) &
                                        "   "
             )  is
   begin
      Initialize (Module);
      Check_Handle (Name);
      Module.Ptr := Links.Import_Import (Name.Ptr);
      if (  Module.Ptr = Null_Object
         or else
            Null_Object /= Links.Err_Occurred.all
         )  then
         Error_Traceback (Destination, Pointer, No_Error, Decorator);
      else
         No_Error := True;
      end if;
   end Import_Import;

   procedure Import_Import
             (  Name        : String;
                Module      : out Handle;
                Destination : in out String;
                Pointer     : in out Integer;
                No_Error    : out Boolean;
                Decorator   : String := Character'Val (13) &
                                        Character'Val (10) &
                                        "   "
             )  is
   begin
      Invalidate (Module);
      Module.Ptr := Links.Import_ImportModule (To_C (Name));
      if (  Module.Ptr = Null_Object
         or else
            Null_Object /= Links.Err_Occurred.all
         )  then
         Error_Traceback (Destination, Pointer, No_Error, Decorator);
      else
         No_Error := True;
      end if;
   end Import_Import;

   function Import_ImportModule (Name : String) return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.Import_ImportModule (To_C (Name));
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Import_ImportModule;

   function Index_Check (Value : Handle) return Boolean is
   begin
      Check_Handle (Value);
      return Links.Index_Check (Value.Ptr) = 1;
   end Index_Check;

   procedure Initialize (Lock : in out Global_Interpreter_Lock) is
   begin
      Lock.State := Links.GILState_Ensure.all;
   end Initialize;

   procedure Invalidate (Object : in out Handle) is
   begin
      if Object.Ptr /= Null_Object then
         Links.DecRef (Object.Ptr);
         Object.Ptr := Null_Object;
      end if;
   end Invalidate;

   function Is_Err_Occurred return Boolean is
   begin
      return Null_Object /= Links.Err_Occurred.all;
   end Is_Err_Occurred;

   function Is_In (Object : Handle; Class : Handle) return Boolean is
   begin
      Check_Handle (Class);
      return Object_IsInstance (Object, Class);
   end Is_In;

   function Is_None (Object : Handle) return Boolean is
   begin
      return Object.Ptr = Null_Object or else Object.Ptr = Links.None;
   end Is_None;

   function Is_Null (Reference : Object) return Boolean is
   begin
      return Reference = Null_Object;
   end Is_Null;

   function Is_Valid (Object : Handle) return Boolean is
   begin
      return Object.Ptr /= Null_Object;
   end Is_Valid;

   procedure Adjust (Object : in out Handle) is
   begin
      if Object.Ptr /= Null_Object then
         Links.IncRef (Object.Ptr);
      end if;
   end Adjust;

   procedure Error_Traceback
             (  Destination : in out String;
                Pointer     : in out Integer;
                No_Error    : out Boolean;
                Decorator   : String := Character'Val (13) &
                                        Character'Val (10) &
                                        "   ";
                Clear_Error : Boolean := True
             )  is
      Error : Object;
   begin
      Error := Links.Err_Occurred.all;
      if Error = Null_Object then
         No_Error := True;
         return;
      end if;
      No_Error := False;
      declare
         Class    : Handle;
         Message  : Handle;
         Trace    : Handle;
         Position : Integer := Pointer;
      begin
         Links.Err_Fetch (Class.Ptr, Message.Ptr, Trace.Ptr);
         Links.Err_NormalizeException
         (  Class.Ptr,
            Message.Ptr,
            Trace.Ptr
         );
         Put (Destination, Position, Object_Str (Message));
         Put (Destination, Position, " [");
         Put (Destination, Position, Object_Str (Class));
         Put (Destination, Position, "]");
         if not Is_Valid (Trace) then
            return;
         end if;
         declare
            Func   : Handle;
            Module : Handle;
         begin
            Module :=
               Import_Import (Unicode_DecodeFSDefault ("traceback"));
            Func := Object_GetAttrString (Module, "format_tb");
            if Callable_Check (Func) = 0 then
               declare
                  Args  : Handle;
                  Value : Handle;
               begin
                  Args := Tuple_New (1);
                  Tuple_SetItem (Args, 0, Trace);
                  Value := Object_CallObject (Func, Args);
                  if Is_Valid (Value) then
                     declare
                        Length : constant ssize_t := List_Size (Value);
                        Item   : Object;
                     begin
                        for Index in 0..Length - 1 loop
                           Item :=
                              Links.List_GetItem (Value.Ptr, Index);
                           if Item /= Null_Object then
                              Put (Destination, Position, Decorator);
                              Put
                              (  Destination,
                                 Position,
                                 As_String (Item)
                              );
                           end if;
                        end loop;
                     end;
                  end if;
               end;
            end if;
         end;
         if not Clear_Error then
            Links.Err_Restore (Class.Ptr, Message.Ptr, Trace.Ptr);
         end if;
         Pointer := Position;
      exception
         when others =>
            Links.Err_Restore (Class.Ptr, Message.Ptr, Trace.Ptr);
            raise;
      end;
   end Error_Traceback;

   procedure Finalize (Object : in out Handle) is
   begin
      Invalidate (Object);
   end Finalize;

   function Iter_Check (Iterator : Handle) return Boolean is
   begin
      return
      (  Iterator.Ptr /= Null_Object
      and then
         0 /= Links.Iter_Check (Iterator.Ptr)
      );
   end Iter_Check;

   function Iter_Next (Iterator : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Iterator);
      Result.Ptr := Links.Iter_Next (Iterator.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Iter_Next;

   procedure List_Append (List : Handle; Item : Handle) is
   begin
      Check_Handle (List);
      if Links.List_Append (List.Ptr, Item.Ptr) = -1 then
         Check_Error;
      end if;
   end List_Append;

   function List_AsTuple (List : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (List);
      Result.Ptr := Links.List_AsTuple (List.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end List_AsTuple;

-- function List_Check (List : Handle; Exact : Boolean := False)
--    return Boolean is
-- begin
--    Check_Handle (List);
--    if Exact then
--       return 0 /= Links.List_CheckExact (List.Ptr);
--    else
--        return 0 /= Links.List_Check (List.Ptr);
--    end if;
-- end List_Check;

   function List_New (Size : ssize_t) return Handle is
   begin
      return (Ada.Finalization.Controlled with Links.List_New (Size));
   end List_New;

   function List_GetItem
            (  List  : Handle;
               Index : ssize_t
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (List);
      Result.Ptr := Links.List_GetItem (List.Ptr, Index);
      if Result.Ptr = Null_Object then
         Check_Error;
      else
         Links.IncRef (Result.Ptr); -- Borrowed reference
      end if;
      return Result;
   end List_GetItem;

   function List_GetSlice
            (  List : Handle;
               Low  : ssize_t;
               High : ssize_t
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (List);
      Result.Ptr := Links.List_GetSlice (List.Ptr, Low, High);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end List_GetSlice;

   procedure List_Insert
             (  List  : Handle;
                Index : ssize_t;
                Item  : Handle
             )  is
   begin
      Check_Handle (List);
      Check_Handle (Item);
      if Links.List_Insert (List.Ptr, Index, Item.Ptr) = -1 then
         Check_Error;
      end if;
   end List_Insert;

   procedure List_Reverse (List : Handle) is
   begin
      Check_Handle (List);
      if Links.List_Reverse (List.Ptr) = -1 then
         Check_Error;
      end if;
   end List_Reverse;

   procedure List_SetItem
             (  List  : Handle;
                Index : ssize_t;
                Item  : Handle
             )  is
   begin
      Check_Handle (List);
      Check_Handle (Item);
      if Links.List_SetItem (List.Ptr, Index, Item.Ptr) = -1 then
         Check_Error;
      end if;
   end List_SetItem;

   procedure List_SetSlice
             (  List  : Handle;
                Low   : ssize_t;
                High  : ssize_t
             )  is
   begin
      Check_Handle (List);
      if (  Links.List_SetSlice (List.Ptr, Low, High, Null_Object)
         = -1
         )  then
         Check_Error;
      end if;
   end List_SetSlice;

   procedure List_SetSlice
             (  List  : Handle;
                Low   : ssize_t;
                High  : ssize_t;
                Items : Handle
             )  is
   begin
      Check_Handle (List);
      Check_Handle (Items);
      if Links.List_SetSlice (List.Ptr, Low, High, Items.Ptr) = -1 then
         Check_Error;
      end if;
   end List_SetSlice;

   function List_Size (Object : Handle) return ssize_t is
   begin
      if Object.Ptr = Null_Object then
         return 0;
      else
         return Links.List_Size (Object.Ptr);
      end if;
   end List_Size;

   procedure List_Sort (List : Handle) is
   begin
      Check_Handle (List);
      if Links.List_Sort (List.Ptr) = -1 then
         Check_Error;
      end if;
   end List_Sort;

   function List_Type return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.List_Type;
      Links.IncRef (Result.Ptr);
      return Result;
   end List_Type;

   function Long_AsLong (Object : Handle) return long is
      Result : constant long := Links.Long_AsLong (Object.Ptr);
   begin
      Check_Error;
      return Result;
   end Long_AsLong;

   function Long_AsInteger64 (Object : Handle)
      return Interfaces.Integer_64 is
   begin
      return As_Integer64 (Object.Ptr);
   end Long_AsInteger64;

   function Long_AsUnsignedLong (Object : Handle)
      return unsigned_long is
      Result : constant unsigned_long :=
                        Links.Long_AsUnsignedLong (Object.Ptr);
   begin
      Check_Error;
      return Result;
   end Long_AsUnsignedLong;

   function Long_AsUnsigned64 (Object : Handle)
      return Interfaces.Unsigned_64 is
   begin
      return As_Unsigned64 (Object.Ptr);
   end Long_AsUnsigned64;

   function Long_FromLong (Value : long) return Handle is
      Result : constant Object := Links.Long_FromLong (Value);
   begin
      if Result = Null_Object then
         Check_Error;
      end if;
      return (Ada.Finalization.Controlled with Result);
   end Long_FromLong;

   function Long_FromInteger64 (Value : Interfaces.Integer_64)
      return Handle is
      function ">"
               (  Ptr   : Long_FromLongLong_Ptr;
                  Value : Interfaces.Integer_64
               )  return Py.Object;
      pragma Import (C, ">", "long_fromint64");
      Result : constant Object := Links.Long_FromLongLong > Value;
   begin
      if Result = Null_Object then
         Check_Error;
      end if;
      return (Ada.Finalization.Controlled with Result);
   end Long_FromInteger64;

   function Long_FromUnsignedLong (Value : unsigned_long)
      return Handle is
      Result : constant Object := Links.Long_FromUnsignedLong (Value);
   begin
      if Result = Null_Object then
         Check_Error;
      end if;
      return (Ada.Finalization.Controlled with Result);
   end Long_FromUnsignedLong;

   function Long_FromUnsigned64 (Value : Interfaces.Unsigned_64)
      return Handle is
      function ">"
               (  Ptr   : Long_FromUnsignedLongLong_Ptr;
                  Value : Interfaces.Unsigned_64
               )  return Py.Object;
      pragma Import (C, ">", "long_fromuint64");
      Result : constant Object :=
               Links.Long_FromUnsignedLongLong > Value;
   begin
      if Result = Null_Object then
         Check_Error;
      end if;
      return (Ada.Finalization.Controlled with Result);
   end Long_FromUnsigned64;

   function Long_Type return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.Long_Type;
      Links.IncRef (Result.Ptr);
      return Result;
   end Long_Type;

   procedure Module_AddObject
             (  Module : Handle;
                Name   : String;
                Value  : Handle
             )  is
   begin
      Check_Handle (Module);
      Check_Handle (Value);
      if 0 > Links.Module_AddObject
             (  Module.Ptr,
                To_C (Name),
                Value.Ptr
             )  then -- Failure
         Check_Error;
      else -- Success, the reference is stolen
         Links.IncRef (Value.Ptr);
      end if;
   end Module_AddObject;

   function Module_Create
            (  Modules : ModuleDef;
               Version : int := 1013
            )  return Object is
   begin
      return Links.Module_Create (Modules, Version);
   end Module_Create;

   function Module_GetDict (Module : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Module);
      Result.Ptr := Links.Module_GetDict (Module.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      else
         Links.IncRef (Result.Ptr); -- Borrowed reference
      end if;
      return Result;
   end Module_GetDict;

   function Module_GetName (Module : Handle) return String is
      Result : chars_ptr;
   begin
      Check_Handle (Module);
      Result := Links.Module_GetName (Module.Ptr);
      if Result = Null_Ptr then
         Check_Error;
         return "";
      else
         return Value (Result);
      end if;
   end Module_GetName;

   function No_Value return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.None;
      Links.IncRef (Result.Ptr);
      return Result;
   end No_Value;

   function Number_Absolute (Value : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Value);
      Result.Ptr := Links.Number_Absolute (Value.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Absolute;

   function Number_Add (Left : Handle; Right : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_Add (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Add;

   function Number_And (Left : Handle;  Right : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_And (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      else
         Links.IncRef (Result.Ptr);
      end if;
      return Result;
   end Number_And;

   function Number_AsSsize_t (Value : Handle; Error : Handle)
      return size_t is
      Result : size_t;
   begin
      Check_Handle (Value);
      Check_Handle (Error);
      Result := Links.Number_AsSsize_t (Value.Ptr, Error.Ptr);
      return Result;
   end Number_AsSsize_t;

   function Number_Check (Value : Handle) return Boolean is
   begin
      Check_Handle (Value);
      return Links.Number_Check (Value.Ptr) = 1;
   end Number_Check;

   function Number_Divmod (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_Divmod (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Divmod;

   function Number_Float (Value : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Value);
      Result.Ptr := Links.Number_Float (Value.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Float;

   function Number_FloorDivide (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_FloorDivide (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_FloorDivide;

   function Number_Index (Value : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Value);
      Result.Ptr := Links.Number_Index (Value.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Index;

   function Number_InPlaceAdd (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_InPlaceAdd (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_InPlaceAdd;

   function Number_InPlaceAnd (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_InPlaceAnd (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_InPlaceAnd;

   function Number_InPlaceFloorDivide (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr :=
         Links.Number_InPlaceFloorDivide (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_InPlaceFloorDivide;

   function Number_InPlaceLshift (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_InPlaceAnd (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end ;

   function Number_InPlaceMatrixMultiply
            (  Left  : Handle;
               Right : Handle
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_InPlaceAnd (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end ;

   function Number_InPlaceMultiply (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_InPlaceAnd (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end ;

   function Number_InPlaceOr (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_InPlaceAnd (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end ;

   function Number_InPlacePower
            (  Left     : Handle;
               Right    : Handle;
               Optional : Handle
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Check_Handle (Optional);
      Result.Ptr :=
         Links.Number_InPlacePower (Left.Ptr, Right.Ptr, Optional.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_InPlacePower;

   function Number_InPlacePower (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr :=
         Links.Number_InPlacePower (Left.Ptr, Right.Ptr, Links.None);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_InPlacePower;

   function Number_InPlaceRemainder (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr :=
         Links.Number_InPlaceRemainder (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_InPlaceRemainder;

   function Number_InPlaceRshift (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_InPlaceAnd (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end ;

   function Number_InPlaceSubtract (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_InPlaceAnd (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end ;

   function Number_InPlaceTrueDivide (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr :=
         Links.Number_InPlaceTrueDivide (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_InPlaceTrueDivide;

   function Number_InPlaceXor (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_InPlaceXor (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_InPlaceXor;

   function Number_Invert (Value : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Value);
      Result.Ptr := Links.Number_Invert (Value.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Invert;

   function Number_Long (Value : Handle)  return Handle is
      Result : Handle;
   begin
      Check_Handle (Value);
      Result.Ptr := Links.Number_Long (Value.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Long;

   function Number_Lshift (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_Lshift (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Lshift;

   function Number_MatrixMultiply (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_MatrixMultiply (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_MatrixMultiply;

   function Number_Multiply (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_Multiply (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Multiply;

   function Number_Negative (Value : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Value);
      Result.Ptr := Links.Number_Negative (Value.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Negative;

   function Number_Or (Left : Handle; Right : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_Or (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Or;

   function Number_Positive (Value : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Value);
      Result.Ptr := Links.Number_Positive (Value.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Positive;

   function Number_Power
        (  Left     : Handle;
           Right    : Handle;
           Optional : Handle
        )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Check_Handle (Optional);
      Result.Ptr :=
         Links.Number_Power (Left.Ptr, Right.Ptr, Optional.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Power;

   function Number_Power (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr :=
         Links.Number_Power (Left.Ptr, Right.Ptr, Links.None);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Power;

   function Number_Remainder (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_Remainder (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Remainder;

   function Number_Rshift (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_Rshift (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Rshift;

   function Number_Subtract (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_Subtract (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Subtract;

   function Number_ToBase (Value : Handle; Base : Base_Type)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Value);
      case Base is
         when Binary =>
            Result.Ptr := Links.Number_ToBase (Value.Ptr, 2);
         when Octal =>
            Result.Ptr := Links.Number_ToBase (Value.Ptr, 8);
         when Decimal =>
            Result.Ptr := Links.Number_ToBase (Value.Ptr, 10);
         when Hexadecimal =>
            Result.Ptr := Links.Number_ToBase (Value.Ptr, 16);
      end case;
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_ToBase;

   function Number_TrueDivide (Left : Handle; Right : Handle)
      return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_TrueDivide (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_TrueDivide;

   function Number_Xor (Left : Handle; Right : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Number_Xor (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Number_Xor;

   function Object_Bytes (Object : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Object);
      Result.Ptr := Links.Object_Bytes (Object.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Object_Bytes;

   function Object_Call
            (  Operation : Handle;
               Arguments : Handle;
               Keyed     : Handle;
               Check     : Boolean := False
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Operation);
      Result.Ptr :=
         Links.Object_Call (Operation.Ptr, Arguments.Ptr, Keyed.Ptr);
      if Check and then Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Object_Call;

   function Object_CallNoArgs
            (  Operation : Handle;
               Check     : Boolean := False
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Operation);
      if Links.Object_CallNoArgs = null then
         declare
            Arguments : constant Handle := Tuple_New (0);
         begin
            Result.Ptr :=
               Links.Object_CallObject (Operation.Ptr, Arguments.Ptr);
            if Check and then Result.Ptr = Null_Object then
               Check_Error;
            end if;
         end;
      else
         Result.Ptr :=
            Links.Object_CallNoArgs (Operation.Ptr);
         if Check and then Result.Ptr = Null_Object then
            Check_Error;
         end if;
      end if;
      return Result;
   end Object_CallNoArgs;

   function Object_CallObject
            (  Operation : Handle;
               Arguments : Handle;
               Check     : Boolean := False
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Operation);
      Result.Ptr :=
         Links.Object_CallObject (Operation.Ptr, Arguments.Ptr);
      if Check and then Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Object_CallObject;

   procedure Object_DelItem
            (  Object : Handle;
               Key    : Handle
            )  is
   begin
      Check_Handle (Object);
      Check_Handle (Key);
      if (  -1
         =  Links.Object_DelItem (Object.Ptr, Key.Ptr)
         )  then
         Check_Error;
      end if;
   end Object_DelItem;

   function Object_GenericGetAttr
            (  Object : Handle;
               Name   : Handle
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Object);
      Check_Handle (Name);
      Result.Ptr := -- New reference
         Links.Object_GenericGetAttr (Object.Ptr, Name.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Object_GenericGetAttr;

   procedure Object_GenericSetAttr
            (  Object : Handle;
               Name   : Handle;
               Value  : Handle
            )  is
   begin
      Check_Handle (Object);
      Check_Handle (Name);
      Check_Handle (Value);
      if (  -1
         =  Links.Object_GenericSetAttr
            (  Object.Ptr,
               Name.Ptr,
               Value.Ptr
         )  )  then
         Check_Error;
      end if;
   end Object_GenericSetAttr;

   function Object_GetAttr
            (  Object    : Handle;
               Attribute : Handle
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Object);
      Check_Handle (Attribute);
      Result.Ptr := -- New reference
         Links.Object_GetAttr (Object.Ptr, Attribute.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Object_GetAttr;

   function Object_GetAttrString
            (  Object : Handle;
               Name   : String
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Object);
      Result.Ptr := -- New reference
         Links.Object_GetAttrString (Object.Ptr, To_C (Name));
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Object_GetAttrString;

   function Object_GetItem
            (  Object : Handle;
               Key    : Handle
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Object);
      Check_Handle (Key);
      Result.Ptr := -- New reference
         Links.Object_GetAttr (Object.Ptr, Key.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Object_GetItem;

   function Object_GetIter (Object : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Object);
      Result.Ptr := -- New reference
         Links.Object_GetIter (Object.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Object_GetIter;

   function Object_Dir (Object : Handle) return Handle is
       Result : Handle;
   begin
      Check_Handle (Object);
      Result.Ptr := Links.Object_Dir (Object.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Object_Dir;

   function Object_HasAttr
            (  Object    : Handle;
               Attribute : Handle
            )  return Boolean is
   begin
      Check_Handle (Object);
      Check_Handle (Attribute);
      return 0 /= Links.Object_HasAttr (Object.Ptr, Attribute.Ptr);
   end Object_HasAttr;

   function Object_HasAttrString
            (  Object : Handle;
               Name   : String
            )  return Boolean is
   begin
      Check_Handle (Object);
      return 0 /= Links.Object_HasAttrString (Object.Ptr, To_C (Name));
   end Object_HasAttrString;

   Head_Size : ssize_t := 0;

   function Object_HeadSize return ssize_t is
   begin
      if Head_Size = 0 then
         declare
            use Ada.Characters.Latin_1;
            Args   : Handle;
            Size   : Handle;
            Helper : Handle;
         begin
            Helper :=
               Compile
               (  "import sys"         & LF &
                  "def FloatSizeOf():" & LF &
                  "   x = 0.0;"        & LF &
                  "   return sys.getsizeof(x)",
                  "py.adb"
               );
            Args := Py.Tuple_New (0);
            Size := Py.Object_CallObject (Helper, Args, True);
            Head_Size :=
               ssize_t (Links.Long_AsLong (Size.Ptr)) - double'Size / 8;
         end;
      end if;
      return Head_Size;
   end Object_HeadSize;

   function Object_IsInstance
            (  Object : Handle;
               Class  : Handle
            )  return Boolean is
      Result : int;
   begin
      Check_Handle (Object);
      Check_Handle (Class);
      Result := Links.Object_IsInstance (Object.Ptr, Class.Ptr);
      if Result = -1 then
         Check_Error;
      end if;
      return Result /= 0;
   end Object_IsInstance;

   function Object_IsSubclass
            (  Derived : Handle;
               Class   : Handle
            )  return Boolean is
      Result : int;
   begin
      Check_Handle (Derived);
      Check_Handle (Class);
      Result := Links.Object_IsSubclass (Derived.Ptr, Class.Ptr);
      if Result = -1 then
         Check_Error;
      end if;
      return Result /= 0;
   end Object_IsSubclass;

   function Object_RichCompareBool
            (  Left      : Handle;
               Right     : Handle;
               Operation : Comparison_Type
            )  return Boolean is
      Op : int;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      case Operation is
         when LT => Op := 0;
         when LE => Op := 1;
         when EQ => Op := 2;
         when NE => Op := 3;
         when GT => Op := 4;
         when GE => Op := 5;
      end case;
      case Links.Object_RichCompareBool (Left.Ptr, Right.Ptr, Op) is
         when 0 =>
            return False;
         when 1 =>
            return True;
         when others =>
            Check_Error;
            return False;
      end case;
   end Object_RichCompareBool;

   procedure Object_SetAttr
            (  Object    : Handle;
               Attribute : Handle;
               Value     : Handle
            )  is
   begin
      Check_Handle (Object);
      Check_Handle (Attribute);
      Check_Handle (Value);
      if (  -1
         =  Links.Object_SetAttr
            (  Object.Ptr,
               Attribute.Ptr,
               Value.Ptr
         )  )  then
         Check_Error;
      end if;
   end Object_SetAttr;

   procedure Object_SetAttr
            (  Object    : Handle;
               Attribute : Handle
            )  is
   begin
      Check_Handle (Object);
      Check_Handle (Attribute);
      if (  -1
         =  Links.Object_SetAttr
            (  Object.Ptr,
               Attribute.Ptr,
               Null_Object
         )  )  then
         Check_Error;
      end if;
   end Object_SetAttr;

   procedure Object_SetAttrString
            (  Object : Handle;
               Name   : String;
               Value  : Handle
            )  is
   begin
      Check_Handle (Object);
      Check_Handle (Value);
      if (  -1
         =  Links.Object_SetAttrString
            (  Object.Ptr,
               To_C (Name),
               Value.Ptr
         )  )  then
         Check_Error;
      end if;
   end Object_SetAttrString;

   procedure Object_SetAttrString
            (  Object : Handle;
               Name   : String
            )  is
   begin
      Check_Handle (Object);
      if (  -1
         =  Links.Object_SetAttrString
            (  Object.Ptr,
               To_C (Name),
               Null_Object
         )  )  then
         Check_Error;
      end if;
   end Object_SetAttrString;

   procedure Object_SetItem
            (  Object : Handle;
               Key    : Handle;
               Value  : Handle
            )  is
   begin
      Check_Handle (Object);
      Check_Handle (Key);
      Check_Handle (Value);
      if (  -1
         =  Links.Object_SetItem (Object.Ptr, Key.Ptr, Value.Ptr)
         )  then
         Check_Error;
      end if;
   end Object_SetItem;

   function Object_Size (Object : Handle) return ssize_t is
      Result : ssize_t;
   begin
      Check_Handle (Object);
      Result := Links.Object_Size (Object.Ptr);
      if Result < 0 then
         Check_Error;
      end if;
      return Result;
   end Object_Size;

   function Object_Str (Value : Object) return String is
   begin
      if Value = Null_Object then
         return "";
      end if;
      declare
         Result : constant Object := Links.Object_Str (Value);
      begin
         if Result = Null_Object then
            return "";
         else
            return As_String
                   (  (Ada.Finalization.Controlled with Result)
                   );
         end if;
      end;
   end Object_Str;

   function Object_Str (Object : Handle) return String is
   begin
      if Object.Ptr = Null_Object then
         return "";
      else
         return Object_Str (Object.Ptr);
      end if;
   end Object_Str;

   function Object_Type (Object : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Object);
      Result.Ptr := Links.Object_Type (Object.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Object_Type;

   function Object_Super (Object : Handle; Name : String)
      return Handle is
      Builtins   : Handle;
      Arguments  : Handle;
      Super_Type : Handle;
      Result     : Handle;
   begin
      Builtins   := Import_AddModule ("builtins");
      Super_Type := Object_GetAttrString (Builtins, "super");
      Arguments  := Tuple_New (2);
      Tuple_SetItem (Arguments, 0, Object_Type (Object));
      Tuple_SetItem (Arguments, 1, Object);
      Result :=
         Object_GetAttrString
         (  Object_CallObject (Super_Type, Arguments, True),
            Name
         );
       if 0 /= Callable_Check (Result) then
          return Result;
       else
          Throw_NameError ('"' & Name & """ is not callable");
       end if;
   end Object_Super;

   function Parse
            (  Args     : Object;
               Keywords : Object;
               List     : Argument_List
            )  return Object_Array is
      use Object_Tables;

      procedure Error (Message : String) is
      begin
         Throw_TypeError (Message);
         Raise_Exception (Python_Error'Identity, Message);
      end Error;

      Unmatched : ssize_t := 0;
      Result    : Object_Array (1..List.Length) :=
                     (others => Null_Object);
   begin
      if Keywords /= Null_Object then
         Unmatched := Links.Dict_Size (Keywords);
      end if;
      if Result'Length < (Links.Tuple_Size (Args) + Unmatched) then
         Error ("Too many arguments");
      end if;
      for Index in 1..Links.Tuple_Size (Args) loop
         Result (Argument_Position (Index)) :=
            Links.Tuple_GetItem (Args, Index - 1);
      end loop;
      for Index in Argument_Position (Links.Tuple_Size (Args) + 1)
                .. Result'Last loop
         declare
            Name : constant String :=
                            GetName (List.Keys, List.Offsets (Index));
         begin
            if Keywords /= Null_Object then
               Result (Index) :=
                  Links.Dict_GetItemString (Keywords, To_C (Name));
            end if;
            if Result (Index) = Null_Object then
               if not List.Optional (Index) then
                  Error ("Missing mandatory argument " & Quote (Name));
               end if;
            else
               Unmatched := Unmatched - 1;
            end if;
         end;
      end loop;
      if Unmatched > 0 then
         declare
            Key      : aliased Object;
            Value    : aliased Object;
            Position : aliased ssize_t := 0;
         begin
            while 0 /= Links.Dict_Next
                       (  Keywords,
                          Position'Access,
                          Key'Access,
                          Value'Access
                       )  loop
               declare
                  Name : constant String := As_String (Key);
               begin
                  if 0 >= Locate (List.Keys, Name) then
                     Error ("Unmatched argument " & Quote (Name));
                  end if;
               end;
            end loop;
         end;
      end if;
      return Result;
   end Parse;

   function Quit (Data : System.Address) return int is
   begin
      Links.Err_SetString (Links.Exc_SystemError.all, "Abort" & Nul);
      return 0;
   end Quit;

   procedure Request_Abort is
   begin
      if 0 = Links.AddPendingCall (Quit'Access, System.Null_Address)
      then
         null;
      end if;
   end Request_Abort;

   procedure Reraise_As (ID : Exception_ID) is
      Error : Object;
   begin
      Error := Links.Err_Occurred.all;
      if Error /= Null_Object then
         declare
            Class   : Handle;
            Message : Handle;
            Trace   : Handle;
         begin
            Links.Err_Fetch (Class.Ptr, Message.Ptr, Trace.Ptr);
            Links.Err_NormalizeException
            (  Class.Ptr,
               Message.Ptr,
               Trace.Ptr
            );
            Raise_Exception (ID, Object_Str (Message));
         end;
      end if;
   end Reraise_As;

   function Sequence_Check (Sequence : Handle) return Boolean is
   begin
      return
      (  Sequence.Ptr /= Null_Object
      and then
         0 /= Links.Sequence_Check (Sequence.Ptr)
      );
   end Sequence_Check;

   function Sequence_Concat (Left, Right : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Left);
      Check_Handle (Right);
      Result.Ptr := Links.Sequence_Concat (Left.Ptr, Right.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Sequence_Concat;

   function Sequence_Contains (Sequence : Handle; Item : Handle)
      return Boolean is
      Result : int;
   begin
      Check_Handle (Sequence);
      Check_Handle (Item);
      Result := Links.Sequence_Contains (Sequence.Ptr, Item.Ptr);
      if Result = -1 then
         Check_Error;
      end if;
      return Result /= 0;
   end Sequence_Contains;

   function Sequence_Count (Sequence : Handle; Item : Handle)
      return Natural is
      Result : ssize_t;
   begin
      Check_Handle (Sequence);
      Check_Handle (Item);
      Result := Links.Sequence_Count (Sequence.Ptr, Item.Ptr);
      if Result = -1 then
         Check_Error;
      end if;
      return Natural (Result);
   end Sequence_Count;

   procedure Sequence_DelItem (Sequence : Handle; Index : ssize_t) is
   begin
      Check_Handle (Sequence);
      if Links.Sequence_DelItem (Sequence.Ptr, Index) = -1 then
         Check_Error;
      end if;
   end Sequence_DelItem;

   procedure Sequence_DelSlice
             (  Sequence : Handle;
                Low      : ssize_t;
                High     : ssize_t
             )  is
   begin
      Check_Handle (Sequence);
      if Links.Sequence_DelSlice (Sequence.Ptr, Low, High) = -1 then
         Check_Error;
      end if;
   end Sequence_DelSlice;

   function Sequence_GetItem
            (  Sequence : Handle;
               Index    : ssize_t
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Sequence);
      Result.Ptr := Links.Sequence_GetItem (Sequence.Ptr, Index);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Sequence_GetItem;

   function Sequence_GetSlice
            (  Sequence : Handle;
               Low      : ssize_t;
               High     : ssize_t
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Sequence);
      Result.Ptr := Links.Sequence_GetSlice (Sequence.Ptr, Low, High);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Sequence_GetSlice;

   function Sequence_Index
            (  Sequence : Handle;
               Item     : Handle
            )  return ssize_t is
      Result : ssize_t;
   begin
      Check_Handle (Sequence);
      Check_Handle (Item);
      Result := Links.Sequence_Index (Sequence.Ptr, Item.Ptr);
      if Result < 0 then
         Check_Error;
      end if;
      return Result;
   end Sequence_Index;

   function Sequence_List (Sequence : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Sequence);
      Result.Ptr := Links.Sequence_List (Sequence.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Sequence_List;

   function Sequence_Repeat
            (  Sequence : Handle;
               Count    : Positive := 1
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Sequence);
      Result.Ptr :=
         Links.Sequence_Repeat (Sequence.Ptr, ssize_t (Count));
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Sequence_Repeat;

   procedure Sequence_SetItem
             (  Sequence : Handle;
                Index    : ssize_t;
                Item     : Handle
             )  is
   begin
      Check_Handle (Sequence);
      Check_Handle (Item);
      if Links.Sequence_SetItem (Sequence.Ptr, Index, Item.Ptr) = -1
      then
         Check_Error;
      end if;
   end Sequence_SetItem;

   procedure Sequence_SetSlice
             (  Sequence : Handle;
                Low      : ssize_t;
                High     : ssize_t;
                Items    : Handle
             )  is
   begin
      Check_Handle (Sequence);
      Check_Handle (Items);
      if (  Links.Sequence_SetSlice
            (  Sequence.Ptr,
               Low,
               High,
               Items.Ptr
            )
         = -1
         )  then
         Check_Error;
      end if;
   end Sequence_SetSlice;

   function Sequence_Size (Sequence : Handle) return ssize_t is
      Result : ssize_t;
   begin
      Check_Handle (Sequence);
      Result := Links.Sequence_Size (Sequence.Ptr);
      if Result < 0 then
         Check_Error;
      end if;
      return Result;
   end Sequence_Size;

   function Sequence_Tuple (Sequence : Handle) return Handle is
      Result : Handle;
   begin
      Check_Handle (Sequence);
      Result.Ptr := Links.Sequence_Tuple (Sequence.Ptr);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Sequence_Tuple;

   procedure Set_Add (Set : Handle; Item : Handle) is
      Result : int;
   begin
      Check_Handle (Set);
      Check_Handle (Item);
      Result := Links.Set_Add (Set.Ptr, Item.Ptr);
      Check_Error;
   end Set_Add;

   procedure Set_Clear (Set : Handle) is
      Result : int;
   begin
      Check_Handle (Set);
      Result := Links.Set_Clear (Set.Ptr);
      Check_Error;
   end Set_Clear;

   function Set_Contains (Set : Handle; Item : Handle) return Boolean is
      Result : Boolean;
   begin
      Check_Handle (Set);
      Check_Handle (Item);
      Result := Links.Set_Contains (Set.Ptr, Item.Ptr) = 1;
      Check_Error;
      return Result;
   end Set_Contains;

   function Set_Discard (Set : Handle; Item : Handle) return Boolean is
      Result : Boolean;
   begin
      Check_Handle (Set);
      Check_Handle (Item);
      Result := Links.Set_Discard (Set.Ptr, Item.Ptr) = 1;
      Check_Error;
      return Result;
   end Set_Discard;

   function Set_New (Frozen : Boolean) return Handle is
      Result : Object;
   begin
      if Frozen then
         Result := Links.FrozenSet_New.all;
      else
         Result := Links.Set_New.all;
      end if;
      return (Ada.Finalization.Controlled with Result);
   end Set_New;

   function Set_Pop (Set : Handle) return Handle is
      Result : Object;
   begin
      Check_Handle (Set);
      Result := Links.Set_Pop (Set.Ptr);
      if Result = Null_Object then
         Check_Error;
      end if;
      return (Ada.Finalization.Controlled with Result);
   end Set_Pop;

   function Set_Size (Set : Handle) return ssize_t is
      Result : ssize_t;
   begin
      Check_Handle (Set);
      Result := Links.Set_Size (Set.Ptr);
      Check_Error;
      return Result;
   end Set_Size;

   function Set_Type return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.Set_Type;
      Links.IncRef (Result.Ptr);
      return Result;
   end Set_Type;

   function Sys_GetObject (Name : String) return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.Sys_GetObject (To_C (Name));
      if Result.Ptr /= Null_Object then
         Links.IncRef (Result.Ptr); -- Borrowed reference
      end if;
      return Result;
   end Sys_GetObject;

-- function Sys_GetSizeOf (Object : Handle) return ssize_t is
--    Result : ssize_t;
-- begin
--    Check_Handle (Object);
--    Result := Links.Sys_GetSizeOf (Object.Ptr);
--    if Result < 0 then
--       Check_Error;
--    end if;
--    return Result;
-- end Sys_GetSizeOf;

   function ThreadState_Get return ThreadState is
   begin
      return Links.ThreadState_Get.all;
   end ThreadState_Get;

   function ThreadState_Swap (State : ThreadState) return ThreadState is
   begin
      return Links.ThreadState_Swap (State);
   end ThreadState_Swap;

   procedure Throw_KeyError (Message : String) is
   begin
      Links.Err_SetString (Links.Exc_KeyError.all, To_C (Message));
   end Throw_KeyError;

   procedure Throw_LookupError (Message : String) is
   begin
      Links.Err_SetString (Links.Exc_LookupError.all, To_C (Message));
   end Throw_LookupError;

   procedure Throw_NameError (Message : String) is
   begin
      Links.Err_SetString (Links.Exc_NameError.all, To_C (Message));
   end Throw_NameError;

   procedure Throw_PermissionError (Message : String) is
   begin
      Links.Err_SetString (Links.Exc_PermissionError.all, To_C (Message));
   end Throw_PermissionError;

   procedure Throw_RunTimeError (Message : String) is
   begin
      Links.Err_SetString (Links.Exc_RunTimeError.all, To_C (Message));
   end Throw_RunTimeError;

   procedure Throw_SyntaxError (Message : String) is
   begin
      Links.Err_SetString (Links.Exc_SyntaxError.all, To_C (Message));
   end Throw_SyntaxError;

   procedure Throw_SystemError (Error : Exception_Occurrence) is
   begin
      Links.Err_SetString
      (  Links.Exc_SystemError.all,
         To_C (Exception_Message (Error))
      );
   end Throw_SystemError;

   procedure Throw_TimeoutError (Message : String) is
   begin
      Links.Err_SetString (Links.Exc_TimeoutError.all, To_C (Message));
   end Throw_TimeoutError;

   procedure Throw_TypeError (Message : String) is
   begin
      Links.Err_SetString (Links.Exc_KeyError.all, To_C (Message));
   end Throw_TypeError;

   procedure Throw_ValueError (Message : String) is
   begin
      Links.Err_SetString (Links.Exc_ValueError.all, To_C (Message));
   end Throw_ValueError;

   function To_Ada (Value : Handle) return Boolean is
   begin
      Check_Handle (Value);
      if Value = To_Python (True) then
         return True;
      elsif Value = To_Python (False) then
         return False;
      else
         Raise_Exception
         (  Data_Error'Identity,
            "Not a Boolean"
         );
      end if;
   end To_Ada;

   function To_Ada (Value : Object) return Time is
      Date : constant String := Object_Str (Value);
   begin
      return Strings_Edit.ISO_8601.Value (Date);
   exception
      when End_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            "No time"
         );
      when Ada.Calendar.Time_Zones.Unknown_Zone_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            "Unknown time zone"
         );
      when others =>
         Raise_Exception
         (  Data_Error'Identity,
            "Invalid time"
         );
   end To_Ada;

   function To_Ada (Value : Handle) return Time is
   begin
      return To_Ada (Value.Ptr);
   end To_Ada;

   function To_Python (Value : Boolean) return Handle is
   begin
      if Value then
         return
            (Ada.Finalization.Controlled with Links.Bool_FromLong (1));
      else
         return
            (Ada.Finalization.Controlled with Links.Bool_FromLong (0));
      end if;
   end To_Python;

   function To_Python (Value : Time) return Handle is
      use Ada.Calendar.Formatting;
      function From_Address is
         new Ada.Unchecked_Conversion
             (  System.Address,
                DateTime_CAPI_Ptr
             );
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;
      Leap_Second : Boolean;
      Result      : Object;
   begin
      if Links.DateTime_CAPI = null then
         Links.DateTime_CAPI :=
            From_Address
            (  Links.Capsule_Import ("datetime.datetime_CAPI" & Nul)
            );
         if Links.DateTime_CAPI = null then
            Raise_Exception
            (  Use_Error'Identity,
               "Failed to locate datetime.datetime_CAPI"
            );
         end if;
      end if;
      Split
      (  Date        => Value,
         Year        => Year,
         Month       => Month,
         Day         => Day,
         Hour        => Hour,
         Minute      => Minute,
         Second      => Second,
         Sub_Second  => Sub_Second,
         Leap_Second => Leap_Second
      );
      Result :=
         Links.DateTime_CAPI.DateTime_FromDateAndTime
         (  Year        => int (Year),
            Month       => int (Month),
            Day         => int (Day),
            Hour        => int (Hour),
            Minute      => int (Minute),
            Second      => int (Second),
            Microsecond => int
                           (  Long_Float'Floor
                              (  Long_Float (Sub_Second) * 1_000_000.0
                           )  ),
            Zone        => Links.DateTime_CAPI.TimeZone_UTC,
            DateType    => Links.DateTime_CAPI.DateTimeType
         );
      if Result = Null_Object then
         Check_Error;
      end if;
      return (Ada.Finalization.Controlled with Result);
   end To_Python;

--     procedure Throw_ValueError (Message : String) is
--     begin
--        Links.Err_SetString (Links.Exc_ValueError, To_C (Message));
--     end Throw_ValueError;

   function Tuple_GetItem
             (  Tuple    : Handle;
                Position : ssize_t
             )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Tuple);
      Result.Ptr := Links.Tuple_GetItem (Tuple.Ptr, Position);
      if Result.Ptr = Null_Object then
         Check_Error;
      else
         Links.IncRef (Result.Ptr); -- Borrowed reference
      end if;
      return Result;
   end Tuple_GetItem;

   function Tuple_GetSlice
            (  Tuple : Handle;
               Low   : ssize_t;
               High  : ssize_t
            )  return Handle is
      Result : Handle;
   begin
      Check_Handle (Tuple);
      Result.Ptr := Links.Tuple_GetSlice (Tuple.Ptr, Low, High);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Tuple_GetSlice;

   function Tuple_New (Size : ssize_t) return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.Tuple_New (Size);
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      return Result;
   end Tuple_New;

   procedure Tuple_SetItem
              (  Tuple    : Handle;
                 Position : ssize_t;
                 Item     : Handle
              )  is
   begin
      Check_Handle (Tuple);
      if Links.Tuple_SetItem (Tuple.Ptr, Position, Item.Ptr) = 0  then
         Links.IncRef (Item.Ptr); -- Success, reference stolen
      else
         Check_Error;
      end if;
   end Tuple_SetItem;

   function Tuple_Size (Tuple : Handle) return ssize_t is
      Result : ssize_t := 0;
   begin
      if Tuple.Ptr /= Null_Object then
         Result := Links.Tuple_Size (Tuple.Ptr);
         Check_Error;
      end if;
      return Result;
   end Tuple_Size;

   function Tuple_Type return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.Tuple_Type;
      Links.IncRef (Result.Ptr);
      return Result;
   end Tuple_Type;

   function Type_IsSubtype (Sub, Super : Handle) return Boolean is
      Result : int;
   begin
      Check_Handle (Sub);
      Check_Handle (Super);
      Result := Links.Type_IsSubtype (Sub.Ptr, Super.Ptr);
      if Result = -1 then
         Check_Error;
      end if;
      return Result /= 0;
   end Type_IsSubtype;

   function Type_Type return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.Type_Type;
      Links.IncRef (Result.Ptr);
      return Result;
   end Type_Type;

   function Unicode_DecodeFSDefault (Name : String) return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.Unicode_DecodeFSDefault (To_C (Name));
      if Result.Ptr = Null_Object then -- New reference
         Check_Error;
      end if;
      return Result;
   end Unicode_DecodeFSDefault;

   function Unicode_FromString (Name : String) return Handle is
      Result : Handle;
   begin
      if Name'Length = 0 then
         Result.Ptr := Links.Unicode_FromString ((0..1 => NUL));
      else
         Result.Ptr :=
            Links.Unicode_FromStringAndSize (Name'Address, Name'Length);
      end if;
      if Result.Ptr = Null_Object then -- New reference
         Check_Error;
      end if;
      return Result;
   end Unicode_FromString;

   function Unicode_GetLength (Object : Handle) return ssize_t is
      Result : ssize_t := 0;
   begin
      if Object.Ptr /= Null_Object then
         Result := Links.Unicode_GetLength (Object.Ptr);
         Check_Error;
      end if;
      return Result;
   end Unicode_GetLength;

   function Unicode_ReadChar (Object : Handle; Index : ssize_t)
      return Wide_Wide_Character is
      Result : Wide_Wide_Character;
   begin
      Check_Handle (Object);
      Result := Links.Unicode_ReadChar (Object.Ptr, Index);
      Check_Error;
      return Result;
   end Unicode_ReadChar;

   function Unicode_Type return Handle is
      Result : Handle;
   begin
      Result.Ptr := Links.Unicode_Type;
      Links.IncRef (Result.Ptr);
      return Result;
   end Unicode_Type;

   function "+" (Left : String) return Argument_List is
      use Object_Tables;
      Result : Argument_List (1);
   begin
      Result.Keys.Add (Left, 1);
      Result.Offsets  (1) := 1;
      Result.Optional (1) := False;
      return Result;
   end "+";

   function "+" (Left, Right : String) return Argument_List is
      use Object_Tables;
      Result : Argument_List (2);
   begin
      Result.Keys.Add (Left,  1);
      Result.Keys.Add (Right, 2);
      Result.Offsets (GetTag (Result.Keys, 1)) := 1;
      Result.Offsets (GetTag (Result.Keys, 2)) := 2;
      Result.Optional (1) := False;
      Result.Optional (2) := False;
      return Result;
   end "+";

   function "+" (List : Argument_List; Key : String)
      return Argument_List is
      use Object_Tables;
      Result : Argument_List (List.Length + 1);
      Offset : Natural;
   begin
      Result.Keys := List.Keys;
      Result.Keys.Add (Key, Result.Length, Offset);
      for Index in 1..GetSize (Result.Keys) loop
         Result.Offsets (GetTag (Result.Keys, Index)) := Index;
      end loop;
      Result.Optional (List.Optional'Range) := List.Optional;
      Result.Optional (Result.Length) := False;
      return Result;
   end "+";

   function "-" (Left : String) return Argument_List is
      use Object_Tables;
      Result : Argument_List (1);
   begin
      Result.Keys.Add (Left, 1);
      Result.Offsets  (1) := 1;
      Result.Optional (1) := True;
      return Result;
   end "-";

   function "-" (Left, Right : String) return Argument_List is
      Result : Argument_List := Left + Right;
   begin
      Result.Optional (2) := True;
      return Result;
   end "-";

   function "-" (List : Argument_List; Key : String)
      return Argument_List is
      Result : Argument_List := List + Key;
   begin
      Result.Optional (Result.Length) := True;
      return Result;
   end "-";

   package body Generic_Capsule is
      Name : constant char_array :=
                      To_C (Ada.Tags.Expanded_Name (Capsule_Name'Tag));

      procedure Destructor (Capsule : Object);
      pragma Convention (C, Destructor);

      package Conversion is
         new System.Address_To_Access_Conversions (Object_Type);

      type Local_Ptr is access procedure (Capsule : Object);
      pragma Convention (C, Local_Ptr);

      function From_Local_Ptr is
         new Ada.Unchecked_Conversion
             (  Local_Ptr,
                Capsule_Destructor_Ptr
             );

      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Object_Type,
                Conversion.Object_Pointer
             );

      procedure Destructor (Capsule : Object) is
         Ptr : Conversion.Object_Pointer;
      begin
         Ptr := Conversion.To_Pointer
                (  Links.Capsule_GetPointer (Capsule, Name)
                );
         Free (Ptr);
      end Destructor;

      function Create (Value : Object_Type) return Handle is
         Ptr    : constant Object_Type_Ptr := new Object_Type'(Value);
         Result : Handle;
      begin
         Result.Ptr :=
            Links.Capsule_New
            (  Ptr.all'Address,
               Name,
               From_Local_Ptr (Destructor'Access)
            );
         return Result;
      end Create;

      function Get (Value : Handle) return Object_Type_Ptr is
         Address : System.Address;
      begin
         Check_Handle (Value);
         Address := Links.Capsule_GetPointer (Value.Ptr, Name);
         if Address = System.Null_Address then
            Check_Error;
            return null;
         else
            return Conversion.To_Pointer (Address).all'Unchecked_Access;
         end if;
      end Get;

      function Is_Valid (Value : Handle) return Boolean is
      begin
         if Value.Ptr = Null_Object then
            return False;
         end if;
         return Links.Capsule_IsValid (Value.Ptr, Name) = 1;
      end Is_Valid;

   end Generic_Capsule;

end Py;
