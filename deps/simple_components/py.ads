--                                                                    --
--  package Py                      Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
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

with Ada.Calendar;          use Ada.Calendar;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Streams;           use Ada.Streams;
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with System;                use System;

with Ada.Finalization;
with Interfaces.C.Pointers;
with Tables;

package Py is

   Python_Error : exception;

   procedure Load (Name : String := "");
   type ssize_t is new ptrdiff_t;
   type hash_t  is new ssize_t;
--
-- Python Global Interpreter Lock (GIL)
--
-- The  object  takes  GIL upon  initialization  and  releases  it  when
-- finalized.  All calls must be done  under the lock except  explicitly
-- stated in Python documentation.
--
   type Global_Interpreter_Lock is
      new Ada.Finalization.Limited_Controlled with private;
   procedure Finalize (Lock : in out Global_Interpreter_Lock);
   procedure Initialize (Lock : in out Global_Interpreter_Lock);
--
-- Handle -- To Python object. Maintains a reference count automatically
--           increased on assignment and decreased on finalization
--
   type Handle is new Ada.Finalization.Controlled with private;
   function No_Value return Handle;
   procedure Check_Handle (Object : Handle);
   procedure Invalidate (Object : in out Handle);
   function Is_Valid (Object : Handle) return Boolean;
------------------------------------------------------------------------
-- General Python template is
--
--    Py.Load       -- Load library
--    Py.Initialize -- Initialize Python environment
--    declare
--       GIL : Py.Global_Interpreter_Lock;
--    begin
--       ... Python bindings calls
--    end;
--    Py.FinalizeEx
--
-- Initialize/FinalizeEx -- Initialization and finalization
--
   procedure Initialize;
   function FinalizeEx return int;
--
-- Compile -- High-level complie function
--
--    Source    - The Python source containing function
--    File_Name - The name of the source
--
-- The function identifies the entry point in  the source and returns it
-- as an object. An example is
--
--    def add(n1, n2 ) :
--       return n1+n2
--
-- Here add is the entry point.
--
-- Returns :
--
--    The module entry point
--
   function Compile
            (  Source    : String;
               File_Name : String
            )  return Handle;
--
-- Compile -- High-level complie function
--
--    Source      - The Python source containing function
--    File_Name   - The name of the source
--    Module      - The compiled module
--    Entry_Point - The module entry point
--
   procedure Compile
             (  Source      : String;
                File_Name   : String;
                Module      : out Handle;
                Entry_Point : out Handle
            );
--
-- Import - High-level compile function
--
--    File_Name  - The name of the source file
--    Entry_Name - The name of the function to call
--
-- The function imports  the module  specified by File_Name if it is not
-- already  loaded.  The  module  can  contain  several  functions.  The
-- parameter  Entry_Name  specifies the function to return as a callable
-- object.
--
-- Returns :
--
--    The executable object
--
   function Import
            (  File_Name  : String;
               Entry_Name : String
            )  return Handle;
--
-- Import_AddModule -- Create or load module by name
--
--    Name - The module name
--
-- Returns :
--
--    The module, created new if does not exist
--
   function Import_AddModule (Name : String) return Handle;
--
-- Import_Import[Module] -- Import module
--
--    Name - Of the module to look for
--
-- Returns :
--
--    The module
--
   function Import_Import (Name : Handle) return Handle;
   function Import_ImportModule (Name : String) return Handle;
--
-- Import_Import -- Import module
--
--    Name        - Of the module to look for
--    Module      - The result
--    Destination - The string buffer to receive error traceback
--    Pointer     - The position in the buffer to start at, advanced
--    No_Error    - Outcome indicator
--    Decorator   - Traceback lines
--
-- This is a variant that  stores error traceback  in the buffer instead
-- of raising an exception
--
   procedure Import_Import
             (  Name        : Handle;
                Module      : out Handle;
                Destination : in out String;
                Pointer     : in out Integer;
                No_Error    : out Boolean;
                Decorator   : String := Character'Val (13) &
                                        Character'Val (10) &
                                        "   "
             );
   procedure Import_Import
             (  Name        : String;
                Module      : out Handle;
                Destination : in out String;
                Pointer     : in out Integer;
                No_Error    : out Boolean;
                Decorator   : String := Character'Val (13) &
                                        Character'Val (10) &
                                        "   "
             );
--
-- Import_ExecCodeModuleEx -- Import module from compiled code
--
--    Name - The module name
--    Code - The code, see Compile
--    Path - The path
--
-- Returns :
--
--    A handle to the module
--
   function Import_ExecCodeModuleEx
            (  Name : String;
               Code : Handle;
               Path : String := ""
            )  return Handle;
--
-- Import_GetModule -- Module lookup
--
--    Name - The module name
--
-- This function  does not load the module only looks for already loaded
-- ones.
--
-- Returns :
--
--    A handle to the module
--
   function Import_GetModule (Name : String) return Handle;
   function Sys_GetObject (Name : String) return Handle;
-- function Sys_GetSizeOf (Object : Handle) return ssize_t;
------------------------------------------------------------------------
-- Strings and bytes array
--
-- As_String -- Get UTF-8 string
--
--    Object - The string object
--
-- Returns :
--
--    UTF-8 encoded string
--
   function As_String (Object : Handle) return String;
--
-- Byte{s|Array}_AsString -- Get bytes
--
--    Object - Bytes object
--
-- Note that strings are not bytes.  In order to get bytes from a string
-- it must be encoded, e.g. as str.encode("Hello World!")
--
-- Returns :
--
--    The corresponding string or stream element array
--
   function ByteArray_AsString (Object : Handle) return String;
   function ByteArray_AsString (Object : Handle)
      return Stream_Element_Array;
   function Bytes_AsString (Object : Handle) return String;
   function Bytes_AsString (Object : Handle)
      return Stream_Element_Array;
--
-- Byte{s|Array}_FromString -- Store bytes
--
--    Value - A string or an array of stream elements
--
-- Returns :
--
--    A handle to the new object
--
   function ByteArray_FromString (Value : String) return Handle;
   function ByteArray_FromString (Value : Stream_Element_Array)
      return Handle;
   function ByteArray_Type return Handle;
   function Bytes_FromString (Value : String) return Handle;
   function Bytes_FromString (Value : Stream_Element_Array)
      return Handle;
   function ByteArray_Size (Object : Handle) return ssize_t;
   function Bytes_Size (Object : Handle) return ssize_t;
   function Bytes_Type return Handle;
   function Unicode_DecodeFSDefault (Name : String) return Handle;
   function Unicode_FromString (Name : String) return Handle;
   function Unicode_GetLength (Object : Handle) return ssize_t;
   function Unicode_ReadChar (Object : Handle; Index : ssize_t)
      return Wide_Wide_Character;
   function Unicode_Type return Handle;
------------------------------------------------------------------------
-- Dictionaries
--
--  function Dict_Check (Dictionary : Handle; Exact : Boolean := False)
--     return Boolean;
   procedure Dict_Clear (Dictionary : Handle);
   function Dict_Contains
            (  Dictionary : Handle;
               Key        : Handle
            )  return Boolean;
   function Dict_Copy (Dictionary : Handle) return Handle;
   procedure Dict_DelItem (Dictionary : Handle; Key : Handle);
   procedure Dict_DelItemString (Dictionary : Handle; Key : String);
   function Dict_GetItemString
            (  Dictionary : Handle;
               Key        : String
            )  return Handle;
   function Dict_Keys (Dictionary : Handle) return Handle;
   function Dict_Items (Dictionary : Handle) return Handle;
   function Dict_New return Handle;
   procedure Dict_Merge
             (  Dictionary : Handle;
                Update     : Handle;
                Override   : Boolean
             );
   function Dict_Next
            (  Dictionary : Handle;
               Position   : access ssize_t;
               Key        : access Handle;
               Value      : access Handle
            )  return Boolean;
-- function Dict_SetDefault
--          (  Dictionary : Handle;
--             Key        : Handle;
--             Default    : Handle
--          )  return Handle;
   procedure Dict_SetItem
             (  Dictionary : Handle;
                Key        : Handle;
                Value      : Handle
             );
   procedure Dict_SetItemString
             (  Dictionary : Handle;
                Key        : String;
                Value      : Handle
             );
   function Dict_Size (Dictionary : Handle) return ssize_t;
   function Dict_Type return Handle;
------------------------------------------------------------------------
-- Exceptions
--
-- Check_Error -- Raise Python_Error if a Python exception is pending
--
   procedure Check_Error;
--
-- Err_Clear -- Clear pending Python exception
--
   procedure Err_Clear;
--
-- Err_Occurred -- Get pending exception
--
-- Returns :
--
--    The exception object, invalid if none
--
   function Err_Occurred return Handle;
--
-- Error_Traceback -- Python error traceback
--
--    Destination - To place traceback into
--    Pointer     - To start at and advance after the output
--    No_Error    - True if no error pending
--    Decorator   - Output lines decorator
--    Clear_Error - Clear exception state
--
   procedure Error_Traceback
             (  Destination : in out String;
                Pointer     : in out Integer;
                No_Error    : out Boolean;
                Decorator   : String := Character'Val (13) &
                                        Character'Val (10) &
                                        "   ";
                Clear_Error : Boolean := True
             );
--
-- Is_Err_Occurred -- Check if there is a pending Python exception
--
-- Returns :
--
--    True if a Python exception is pending
--
   function Is_Err_Occurred return Boolean;
--
-- Request_Abort -- Request Python execution abort
--
-- This procedure does not require GIL taken
--
   procedure Request_Abort;
--
-- Reraise_As -- Convert a Python exception into Ada exception
--
--    ID - The Ada exception to raise
--
-- If a Python  exception is pending it is caught  and  ID exception  is
-- raised instead
--
   procedure Reraise_As (ID : Exception_ID);
--
-- Throw_*Error -- Set Python exception
--
--    Message - The message text
--
   procedure Throw_KeyError        (Message : String);
   procedure Throw_LookupError     (Message : String);
   procedure Throw_NameError       (Message : String);
   procedure Throw_PermissionError (Message : String);
   procedure Throw_RunTimeError    (Message : String);
   procedure Throw_TimeoutError    (Message : String);
   procedure Throw_SyntaxError     (Message : String);
   procedure Throw_TypeError       (Message : String);
   procedure Throw_ValueError      (Message : String);
--
-- Throw_SystemError -- Convert Ada exception RuntimeError
--
--    Error - Ada exception
--
    procedure Throw_SystemError (Error : Exception_Occurrence);
------------------------------------------------------------------------
-- Python threading
--
   type ThreadState is private;
   Null_State : constant ThreadState;

   procedure Eval_InitThreads;
   function Eval_SaveThread return ThreadState;
   procedure Eval_RestoreThread (State : ThreadState);
-- function GILState_Check return Boolean;
   function ThreadState_Get return ThreadState;
   function ThreadState_Swap (State : ThreadState) return ThreadState;
------------------------------------------------------------------------
-- Floating-point
--
   function Float_AsDouble (Object : Handle) return double;
   function Float_FromDouble (Value : double) return Handle;
   function Float_Type return Handle;
------------------------------------------------------------------------
-- Iterators
--
   function Iter_Check (Iterator : Handle) return Boolean;
--
-- Iter_Next -- Get next iterator
--
--    Iterator - The iterator object
--
-- Returns :
--
--    The next iterator or an invalid handle at the end
--
   function Iter_Next (Iterator : Handle) return Handle;
------------------------------------------------------------------------
-- Lists
--
   procedure List_Append (List : Handle; Item : Handle);
   function List_AsTuple (List : Handle) return Handle;
-- function List_Check (List : Handle; Exact : Boolean := False)
--    return Boolean;
   function List_GetItem (List : Handle; Index : ssize_t) return Handle;
   function List_GetSlice
            (  List : Handle;
               Low  : ssize_t;
               High : ssize_t
            )  return Handle;
   procedure List_Insert
             (  List  : Handle;
                Index : ssize_t;
                Item  : Handle
             );
   function List_New (Size : ssize_t) return Handle;
   procedure List_Reverse (List : Handle);
   procedure List_SetItem
             (  List  : Handle;
                Index : ssize_t;
                Item  : Handle
             );
   procedure List_SetSlice
             (  List  : Handle;
                Low   : ssize_t;
                High  : ssize_t;
                Items : Handle
             );
   procedure List_SetSlice
             (  List  : Handle;
                Low   : ssize_t;
                High  : ssize_t
             );
   procedure List_Sort (List : Handle);
   function List_Size (Object : Handle) return ssize_t;
   function List_Type return Handle;
------------------------------------------------------------------------
-- Booleans
--
   function To_Ada (Value : Handle) return Boolean;
   function To_Python (Value : Boolean) return Handle;
------------------------------------------------------------------------
-- Integers
--
   function Long_AsLong (Object : Handle) return long;
   function Long_AsInteger64 (Object : Handle)
      return Interfaces.Integer_64;
   function Long_AsUnsignedLong (Object : Handle) return unsigned_long;
   function Long_AsUnsigned64 (Object : Handle)
      return Interfaces.Unsigned_64;
   function Long_FromLong (Value : long) return Handle;
   function Long_FromInteger64 (Value : Interfaces.Integer_64)
      return Handle;
   function Long_FromUnsignedLong (Value : unsigned_long) return Handle;
   function Long_FromUnsigned64 (Value : Interfaces.Unsigned_64)
      return Handle;
   function Long_Type return Handle;
------------------------------------------------------------------------
-- Numbers
--
   function Index_Check (Value : Handle) return Boolean;
   function Number_Absolute (Value : Handle) return Handle;
   function Number_Add (Left : Handle;  Right : Handle) return Handle;
   function Number_And (Left : Handle;  Right : Handle) return Handle;
   function Number_AsSsize_t (Value : Handle; Error : Handle)
      return size_t;
   function Number_Check (Value : Handle) return Boolean;
   function Number_Divmod (Left : Handle; Right : Handle) return Handle;
   function Number_Float (Value : Handle) return Handle;
   function Number_FloorDivide (Left : Handle; Right : Handle)
      return Handle;
   function Number_Index (Value : Handle) return Handle;
   function Number_InPlaceAdd (Left : Handle; Right : Handle)
      return Handle;
   function Number_InPlaceAnd (Left : Handle; Right : Handle)
      return Handle;
   function Number_InPlaceFloorDivide (Left : Handle; Right : Handle)
      return Handle;
   function Number_InPlaceLshift (Left : Handle; Right : Handle)
      return Handle;
   function Number_InPlaceMatrixMultiply
            (  Left  : Handle;
               Right : Handle
            )  return Handle;
   function Number_InPlaceMultiply (Left : Handle; Right : Handle)
      return Handle;
   function Number_InPlaceOr (Left : Handle; Right : Handle)
      return Handle;
   function Number_InPlacePower
            (  Left     : Handle;
               Right    : Handle;
               Optional : Handle
            )  return Handle;
   function Number_InPlacePower
            (  Left : Handle; Right : Handle) return Handle;
   function Number_InPlaceRemainder (Left : Handle; Right : Handle)
      return Handle;
   function Number_InPlaceRshift (Left : Handle; Right : Handle)
      return Handle;
   function Number_InPlaceSubtract (Left : Handle; Right : Handle)
      return Handle;
   function Number_InPlaceTrueDivide (Left : Handle; Right : Handle)
      return Handle;
   function Number_InPlaceXor (Left : Handle; Right : Handle)
      return Handle;
   function Number_Invert (Value : Handle) return Handle;
   function Number_Long (Value : Handle)  return Handle;
   function Number_Lshift (Left  : Handle; Right : Handle)
      return Handle;
   function Number_MatrixMultiply (Left : Handle; Right : Handle)
      return Handle;
   function Number_Multiply (Left : Handle; Right : Handle)
      return Handle;
   function Number_Negative (Value : Handle) return Handle;
   function Number_Or (Left : Handle; Right : Handle) return Handle;
   function Number_Positive (Value : Handle) return Handle;
   function Number_Power
        (  Left     : Handle;
           Right    : Handle;
           Optional : Handle
        )  return Handle;
   function Number_Power (Left : Handle; Right : Handle)
      return Handle;
   function Number_Remainder (Left : Handle; Right : Handle)
      return Handle;
   function Number_Rshift (Left : Handle; Right : Handle)
      return Handle;
   function Number_Subtract (Left : Handle; Right : Handle)
      return Handle;
   type Base_Type is (Binary, Octal, Decimal, Hexadecimal);
   function Number_ToBase (Value : Handle; Base : Base_Type)
      return Handle;
   function Number_TrueDivide (Left : Handle; Right : Handle)
      return Handle;
   function Number_Xor (Left : Handle; Right : Handle) return Handle;

------------------------------------------------------------------------
-- Objects
--
   procedure Module_AddObject
             (  Module : Handle;
                Name   : String;
                Value  : Handle
             );
   function Module_GetDict (Module : Handle) return Handle;
   function Module_GetName (Module : Handle) return String;
   function Callable_Check (Object : Handle) return int;
   function Is_None (Object : Handle) return Boolean;
   function Object_Bytes (Object : Handle) return Handle;
--
-- Object_Call*
--
--    Operation   - To call to
--    Argument[s] - To pass to
--    Keyed       - Positional arguments
--    Check       - Check for exceptions
--
-- Result :
--
--    The result or invalid on exception
--
   function Object_Call
            (  Operation : Handle;
               Arguments : Handle;
               Keyed     : Handle;
               Check     : Boolean := False
            )  return Handle;
   function Object_CallNoArgs
            (  Operation : Handle;
               Check     : Boolean := False
            )  return Handle;
   function Object_CallObject
            (  Operation : Handle;
               Arguments : Handle;
               Check     : Boolean := False
            )  return Handle;
   procedure Object_DelItem (Object : Handle; Key : Handle);
   function Object_Dir  (Object : Handle) return Handle;
   function Object_GenericGetAttr
            (  Object : Handle;
               Name   : Handle
            )  return Handle;
   procedure Object_GenericSetAttr
             (  Object : Handle;
                Name   : Handle;
                Value  : Handle
             );
   function Object_GetAttr
            (  Object    : Handle;
               Attribute : Handle
            )  return Handle;
   function Object_GetAttrString
            (  Object : Handle;
               Name   : String
            )  return Handle;
   function Object_GetItem
            (  Object : Handle;
               Key    : Handle
            )  return Handle;
   function Object_GetIter (Object : Handle) return Handle;
   function Object_HasAttr
            (  Object    : Handle;
               Attribute : Handle
            )  return Boolean;
   function Object_HasAttrString
            (  Object : Handle;
               Name   : String
            )  return Boolean;
--
-- Object_HeadSize -- An equivalent of PyObject_HEAD macro
--
-- Returns :
--
--    The size of PyObject_HEAD
--
   function Object_HeadSize return ssize_t;
   function Object_IsInstance
            (  Object : Handle;
               Class  : Handle
            )  return Boolean;
   function Object_IsSubclass
            (  Derived : Handle;
               Class   : Handle
            )  return Boolean;
   function Is_In (Object : Handle; Class : Handle) return Boolean;
   type Comparison_Type is (LT, LE, EQ, GE, GT, NE);
   function Object_RichCompareBool
            (  Left      : Handle;
               Right     : Handle;
               Operation : Comparison_Type
            )  return Boolean;
   procedure Object_SetAttr
            (  Object    : Handle;
               Attribute : Handle;
               Value     : Handle
            );
   procedure Object_SetAttr
            (  Object    : Handle;
               Attribute : Handle
            );
   procedure Object_SetAttrString
            (  Object : Handle;
               Name   : String;
               Value  : Handle
            );
   procedure Object_SetAttrString
            (  Object : Handle;
               Name   : String
            );
   procedure Object_SetItem
            (  Object : Handle;
               Key    : Handle;
               Value  : Handle
            );
   function Object_Size (Object : Handle) return ssize_t;
   function Object_Str  (Object : Handle) return String;
--
-- Object_Super -- An equivalent of Object.super().Name
--
-- Returns :
--
--     A handle to the parent's method
--
   function Object_Super (Object : Handle; Name : String) return Handle;
   function Object_Type (Object : Handle) return Handle;
   function Type_IsSubtype (Sub, Super : Handle) return Boolean;
   function Type_Type return Handle;
------------------------------------------------------------------------
-- Sequences
--
   function Sequence_Check (Sequence : Handle) return Boolean;
   function Sequence_Concat (Left, Right : Handle) return Handle;
   function Sequence_Count
            (  Sequence : Handle;
               Item     : Handle
            )  return Natural;
   function Sequence_Contains
            (  Sequence : Handle;
               Item     : Handle
            )  return Boolean;
   procedure Sequence_DelItem (Sequence : Handle; Index : ssize_t);
   procedure Sequence_DelSlice
             (  Sequence : Handle;
                Low      : ssize_t;
                High     : ssize_t
             );
   function Sequence_GetItem
            (  Sequence : Handle;
               Index    : ssize_t
            )  return Handle;
   function Sequence_GetSlice
            (  Sequence : Handle;
               Low      : ssize_t;
               High     : ssize_t
            )  return Handle;
   function Sequence_Index
            (  Sequence : Handle;
               Item     : Handle
            )  return ssize_t;
   function Sequence_List (Sequence : Handle) return Handle;
   function Sequence_Repeat
            (  Sequence : Handle;
               Count    : Positive := 1
            )  return Handle;
   procedure Sequence_SetItem
             (  Sequence : Handle;
                Index    : ssize_t;
                Item     : Handle
             );
   procedure Sequence_SetSlice
             (  Sequence : Handle;
                Low      : ssize_t;
                High     : ssize_t;
                Items    : Handle
             );
   function Sequence_Size (Sequence : Handle) return ssize_t;
   function Sequence_Tuple (Sequence : Handle) return Handle;
------------------------------------------------------------------------
-- Sets
--
   procedure Set_Add (Set : Handle; Item : Handle);
   procedure Set_Clear (Set : Handle);
   function Set_Contains (Set : Handle; Item : Handle) return Boolean;
   function Set_Discard (Set : Handle; Item : Handle) return Boolean;
   function Set_New (Frozen : Boolean) return Handle;
   function Set_Pop (Set : Handle) return Handle;
   function Set_Size (Set : Handle) return ssize_t;

   function Set_Type return Handle;
------------------------------------------------------------------------
-- Time conversions between Ada and Python.  A Python object is provided
-- by the datetime module. The type is datetime.datetime. For conversion
-- to Ada any object  which Python string  representation yelds ISO 8601
-- time is sufficient.
--
   function To_Ada (Value : Handle) return Time;
   function To_Python (Value : Time) return Handle;
------------------------------------------------------------------------
-- Tuples
--
   function Tuple_GetItem
            (  Tuple    : Handle;
               Position : ssize_t
            )  return Handle;
   function Tuple_GetSlice
            (  Tuple : Handle;
               Low   : ssize_t;
               High  : ssize_t
            )  return Handle;
   function Tuple_New (Size : ssize_t) return Handle;
   procedure Tuple_SetItem
             (  Tuple    : Handle;
                Position : ssize_t;
                Item     : Handle
             );
   function Tuple_Size (Tuple : Handle) return ssize_t;
   function Tuple_Type return Handle;
------------------------------------------------------------------------
-- Encapsulated objects.  An Ada object passed  to Python as  an  opaque
-- Python object. When passed back to Ada the Ada object can be accessed
-- via the Python object.
--
   generic
      type Object_Type (<>) is private;
   package Generic_Capsule is
      type Object_Type_Ptr is access all Object_Type;
      --
      -- Create -- A Python object encapsulating Ada object
      --
      --    Value - To be stored in the Python object
      --
      -- Returns :
      --
      --    A handle to the newly created Python object
      --
      function Create (Value : Object_Type) return Handle;
      --
      -- Get -- Ada contents of the Python object
      --
      --    Value - A handle to
      --
      -- Returns :
      --
      --    A pointer to the Ada object held in
      --
      function Get (Value : Handle) return Object_Type_Ptr;
      --
      -- Is_Valid -- Ada contents of the Python object
      --
      --    Value - A handle to
      --
      -- Returns :
      --
      --    True if the handle points to a capsule
      --
      function Is_Valid (Value : Handle) return Boolean;
   private
      type Capsule_Name is tagged null record;
   end Generic_Capsule;

private
   procedure Adjust (Object : in out Handle);
   procedure Finalize (Object : in out Handle);

   type GILState is new System.Address;
   type ThreadState is new System.Address;
   Null_State : constant ThreadState := ThreadState (Null_Address);

   type Object is new Address;
   Null_Object : constant Object := Object (Null_Address);

   type Object_Ptr is access all Object;
   pragma Convention (C, Object_Ptr);

   type Handle is new Ada.Finalization.Controlled with record
      Ptr : aliased Object := Null_Object;
   end record;

   METH_VARARGS  : constant := 16#0001#;
   METH_KEYWORDS : constant := 16#0002#;
   METH_NOARGS   : constant := 16#0004#;
   METH_O        : constant := 16#0008#;

   type Object_Head is record
      Object_Count : ssize_t;
      Object_Type  : Object;
   end record;
   pragma Convention (C, Object_Head);

   type ModuleDef_Base is record
      Base  : Object_Head;
      Init  : access function return Object;
      Index : ssize_t;
      Copy  : Object;
   end record;
   pragma Convention (C, ModuleDef_Base);

   type CFunction is access function
        (  Self : Object;
           Args : Object
        )  return Object;
   pragma Convention (C, CFunction);

   type CFunctionWithKeywords is access function
        (  Self     : Object;
           Args     : Object;
           Keywords : Object
        )  return Object;
   pragma Convention (C, CFunctionWithKeywords);

   type CMethod is access function
        (  Self           : Object;
           Defining_Class : Object;
           Args           : access Object;
           Arg_Count      : ssize_t;
           Keywords       : Object
        )  return Object;
   pragma Convention (C, CMethod);

   type Method_Entry (Keywords : Boolean := False) is record
      case Keywords is
         when False =>
            Positional : CFunction;
         when True =>
            Keyed : CFunctionWithKeywords;
      end case;
   end record;
   pragma Unchecked_Union (Method_Entry);
   pragma Convention (C, Method_Entry);

   type MethodDef is record
      Name  : chars_ptr;    -- The name of the built-in function/method
      Meth  : Method_Entry; -- The C function that implements it
      Flags : int;        -- Combination of METH_xxx flags, which mostly
                          -- describe the args expected by the C func
      Doc   : chars_ptr;  -- The __doc__ attribute, or NULL
   end record;
   pragma Convention (C, MethodDef);

   End_Method : constant MethodDef :=
                         (  Null_Ptr,
                            (False, null),
                            0,
                            Null_Ptr
                         );

   T_SHORT          : constant :=  0;
   T_INT            : constant :=  1;
   T_LONG           : constant :=  2;
   T_FLOAT          : constant :=  3;
   T_DOUBLE         : constant :=  4;
   T_STRING         : constant :=  5;
   T_OBJECT         : constant :=  6;
   T_CHAR           : constant :=  7; -- 1-character string
   T_BYTE           : constant :=  8; -- 8-bit signed int
   T_UBYTE          : constant :=  9;
   T_USHORT         : constant := 10;
   T_UINT           : constant := 11;
   T_ULONG          : constant := 12;
   T_STRING_INPLACE : constant := 13;
   T_BOOL           : constant := 14;
   T_OBJECT_EX      : constant := 16;
   T_LONGLONG       : constant := 17;
   T_ULONGLONG      : constant := 18;
   T_PYSSIZET       : constant := 19; -- Py_ssize_t
   T_NONE           : constant := 20; -- Value is always None

   READONLY         : constant := 1;
   READ_RESTRICTED  : constant := 2;
   WRITE_RESTRICTED : constant := 4;
   RESTRICTED       : constant := READ_RESTRICTED + WRITE_RESTRICTED;

   type MemberDef is record
      Name     : chars_ptr;
      The_Type : int;
      Offset   : ssize_t;
      Flags    : int;
      Doc      : chars_ptr;
   end record;
   pragma Convention (C, MemberDef);

   End_Member : constant MemberDef := (Null_Ptr, 0, 0, 0, Null_Ptr);

   type MemberDef_Ptr is access all MemberDef;
   pragma Convention (C, MemberDef_Ptr);

   type getter_Ptr is access function
        (  Self    : Object;
           Closure : System.Address
        )  return Object;
   pragma Convention (C, getter_Ptr);

   type setter_Ptr is access function
        (  Self  : Object;
           Value : Object;
           Closure : System.Address
        )  return int;
   pragma Convention (C, setter_Ptr);

   type GetSetDef is record
      Name    : chars_ptr;
      Get     : getter_Ptr;
      Set     : setter_Ptr;
      Doc     : chars_ptr;
      Closure : System.Address;
   end record;
   pragma Convention (C, GetSetDef);

   End_GetSet : constant GetSetDef :=
                (Null_Ptr, null, null, Null_Ptr, System.Null_Address);
------------------------------------------------------------------------
   type Type_Slot_Kind is
        (  None,
           bf_getbuffer,
           bf_releasebuffer,
           mp_ass_subscript,
           mp_length,
           mp_subscript,
           nb_absolute,
           nb_add,
           nb_and,
           nb_bool,
           nb_divmod,
           nb_float,
           nb_floor_divide,
           nb_index,
           nb_inplace_add,
           nb_inplace_and,
           nb_inplace_floor_divide,
           nb_inplace_lshift,
           nb_inplace_multiply,
           nb_inplace_or,
           nb_inplace_power,
           nb_inplace_remainder,
           nb_inplace_rshift,
           nb_inplace_subtract,
           nb_inplace_true_divide,
           nb_inplace_xor,
           nb_int,
           nb_invert,
           nb_lshift,
           nb_multiply,
           nb_negative,
           nb_or,
           nb_positive,
           nb_power,
           nb_remainder,
           nb_rshift,
           nb_subtract,
           nb_true_divide,
           nb_xor,
           sq_ass_item,
           sq_concat,
           sq_contains,
           sq_inplace_concat,
           sq_inplace_repeat,
           sq_item,
           sq_length,
           sq_repeat,
           tp_alloc,
           tp_base,
           tp_bases,
           tp_call,
           tp_clear,
           tp_dealloc,
           tp_del,
           tp_descr_get,
           tp_descr_set,
           tp_doc,
           tp_getattr,
           tp_getattro,
           tp_hash,
           tp_init,
           tp_is_gc,
           tp_iter,
           tp_iternext,
           tp_methods,
           tp_new,
           tp_repr,
           tp_richcompare,
           tp_setattr,
           tp_setattro,
           tp_str,
           tp_traverse,
           tp_members,
           tp_getset,
           tp_free,
           nb_matrix_multiply,
           nb_inplace_matrix_multiply,
           am_await,
           am_aiter,
           am_anext,
           tp_finalize
        );
   for Type_Slot_Kind'Size use int'Size;
   for Type_Slot_Kind use
       (  None                       =>  0,
          bf_getbuffer               =>  1,
          bf_releasebuffer           =>  2,
          mp_ass_subscript           =>  3,
          mp_length                  =>  4,
          mp_subscript               =>  5,
          nb_absolute                =>  6,
          nb_add                     =>  7,
          nb_and                     =>  8,
          nb_bool                    =>  9,
          nb_divmod                  => 10,
          nb_float                   => 11,
          nb_floor_divide            => 12,
          nb_index                   => 13,
          nb_inplace_add             => 14,
          nb_inplace_and             => 15,
          nb_inplace_floor_divide    => 16,
          nb_inplace_lshift          => 17,
          nb_inplace_multiply        => 18,
          nb_inplace_or              => 19,
          nb_inplace_power           => 20,
          nb_inplace_remainder       => 21,
          nb_inplace_rshift          => 22,
          nb_inplace_subtract        => 23,
          nb_inplace_true_divide     => 24,
          nb_inplace_xor             => 25,
          nb_int                     => 26,
          nb_invert                  => 27,
          nb_lshift                  => 28,
          nb_multiply                => 29,
          nb_negative                => 30,
          nb_or                      => 31,
          nb_positive                => 32,
          nb_power                   => 33,
          nb_remainder               => 34,
          nb_rshift                  => 35,
          nb_subtract                => 36,
          nb_true_divide             => 37,
          nb_xor                     => 38,
          sq_ass_item                => 39,
          sq_concat                  => 40,
          sq_contains                => 41,
          sq_inplace_concat          => 42,
          sq_inplace_repeat          => 43,
          sq_item                    => 44,
          sq_length                  => 45,
          sq_repeat                  => 46,
          tp_alloc                   => 47,
          tp_base                    => 48,
          tp_bases                   => 49,
          tp_call                    => 50,
          tp_clear                   => 51,
          tp_dealloc                 => 52,
          tp_del                     => 53,
          tp_descr_get               => 54,
          tp_descr_set               => 55,
          tp_doc                     => 56,
          tp_getattr                 => 57,
          tp_getattro                => 58,
          tp_hash                    => 59,
          tp_init                    => 60,
          tp_is_gc                   => 61,
          tp_iter                    => 62,
          tp_iternext                => 63,
          tp_methods                 => 64,
          tp_new                     => 65,
          tp_repr                    => 66,
          tp_richcompare             => 67,
          tp_setattr                 => 68,
          tp_setattro                => 69,
          tp_str                     => 70,
          tp_traverse                => 71,
          tp_members                 => 72,
          tp_getset                  => 73,
          tp_free                    => 74,
          nb_matrix_multiply         => 75,
          nb_inplace_matrix_multiply => 76,
          am_await                   => 77,
          am_aiter                   => 78,
          am_anext                   => 79,
          tp_finalize                => 80
       );
   pragma Convention (C, Type_Slot_Kind);

   type getbufferproc_Ptr is access function
        (  Self   : Object;
           Buffer : System.Address;
           Count  : int
        )  return int;
   pragma Convention (C, getbufferproc_Ptr);

   type releasebufferproc_Ptr is access procedure
        (  Self   : Object;
           Buffer : System.Address
        );
   pragma Convention (C, releasebufferproc_Ptr);

   type objobjargproc_Ptr is access function
        (  First  : Object;
           Second : Object;
           Arg    : Object
        )  return int;
   pragma Convention (C, objobjargproc_Ptr);

   type lenfunc_Ptr is access function (Self : Object) return ssize_t;
   pragma Convention (C, lenfunc_Ptr);

   type binaryfunc_Ptr is access function
        (  Left, Right : Object
        )  return Object;
   pragma Convention (C, binaryfunc_Ptr);

   type unaryfunc_Ptr is access function
        (  Left : Object
        )  return Object;
   pragma Convention (C, unaryfunc_Ptr);

   type inquiry_Ptr is access function (Self : Object) return int;
   pragma Convention (C, inquiry_Ptr);

   type ternaryfunc_Ptr is access function
        (  First, Second, Third : Object
        )  return Object;
   pragma Convention (C, ternaryfunc_Ptr);

   type ssizeobjargproc_Ptr is access function
        (  Self : Object;
           Size : ssize_t
        )  return int;
   pragma Convention (C, ssizeobjargproc_Ptr);

   type objobjproc_Ptr is access function
        (  Left, Right : Object
        )  return int;
   pragma Convention (C, objobjproc_Ptr);

   type ssizeargfunc_Ptr is access function
        (  Self : Object;
           Size : ssize_t
        )  return Object;
   pragma Convention (C, ssizeargfunc_Ptr);

   type allocfunc_Ptr is access function
        (  Class  : Object;
           nitems : ssize_t
        )  return Object;
   pragma Convention (C, allocfunc_Ptr);

   type destructor_Ptr is access procedure (Self : Object);
   pragma Convention (C, destructor_Ptr);

   type descrgetfunc_Ptr is access function
        (  Self     : Object;
           Item     : Object;
           The_Type : Object
        )  return Object;
   pragma Convention (C, descrgetfunc_Ptr);

   type descrsetfunc_Ptr is access function
        (  Self  : Object;
           Item  : Object;
           Value : Object
        )  return int;
   pragma Convention (C, descrsetfunc_Ptr);

   type getattrfunc_Ptr is access function
        (  Self : Object;
           Attr : char_array
        )  return Object;
   pragma Convention (C, getattrfunc_Ptr);

   type getattrofunc_Ptr is access function
        (  Self : Object;
           Attr : Object
        )  return Object;
   pragma Convention (C, getattrofunc_Ptr);

   type hashfunc_Ptr is access function
        (  Self : Object
        )  return hash_t;
   pragma Convention (C, hashfunc_Ptr);

   type initproc_Ptr is access function
        (  Self     : Object;
           Args     : Object;
           Keywords : Object
        )  return int;
   pragma Convention (C, initproc_Ptr);

   type getiterfunc_Ptr is access function
        (  Self : Object
        )  return Object;
   pragma Convention (C, getiterfunc_Ptr);

   type iternextfunc_Ptr is access function
        (  Self : Object
        )  return Object;
   pragma Convention (C, iternextfunc_Ptr);

   type newfunc_Ptr is access function
        (  Class    : Object;
           Args     : Object;
           Keywords : Object
        )  return Object;
   pragma Convention (C, newfunc_Ptr);

   type reprfunc_Ptr is access function
        (  Self : Object
        )  return Object;
   pragma Convention (C, reprfunc_Ptr);

   type richcmpfunc_Ptr is access function
        (  Self  : Object;
           Other : Object;
           Opt   : int
        )  return Object;
   pragma Convention (C, richcmpfunc_Ptr);

   type setattrfunc_Ptr is access function
        (  Self  : Object;
           Attr  : char_array;
           Value : Object
        )  return int;
   pragma Convention (C, setattrfunc_Ptr);

   type setattrofunc_Ptr is access function
        (  Self  : Object;
           Attr  : Object;
           Value : Object
        )  return int;
   pragma Convention (C, setattrofunc_Ptr);

   type visitproc_Ptr is access function
        (  This : Object;
           Aarg : System.Address
        )  return int;
   pragma Convention (C, visitproc_Ptr);

   type traverseproc_Ptr is access function
        (  Self  : Object;
           Visit : visitproc_Ptr;
           Arg   : System.Address
        )  return int;
   pragma Convention (C, traverseproc_Ptr);

   type freefunc_Ptr is access procedure (Memory : System.Address);
   pragma Convention (C, freefunc_Ptr);

   type Type_Slot_Function
        (  Kind_Of : Type_Slot_Kind := None
        )  is
   record
      case Kind_Of is
         when None =>
            null;
         when bf_getbuffer =>
            bf_getbuffer : getbufferproc_Ptr;
         when bf_releasebuffer =>
            bf_releasebuffer : releasebufferproc_Ptr;
         when mp_ass_subscript =>
            mp_ass_subscript : objobjargproc_Ptr;
         when mp_length =>
            length : lenfunc_Ptr;
         when mp_subscript =>
            mp_subscript : binaryfunc_Ptr;
         when nb_absolute =>
            nb_absolute : unaryfunc_Ptr;
         when nb_add =>
            nb_add : binaryfunc_Ptr;
         when nb_and =>
            nb_and : binaryfunc_Ptr;
         when nb_bool =>
            nb_bool : inquiry_Ptr;
         when nb_divmod =>
            nb_divmod : binaryfunc_Ptr;
         when nb_float =>
            nb_float : unaryfunc_Ptr;
         when nb_floor_divide =>
            nb_floor_divide : binaryfunc_Ptr;
         when nb_index =>
            nb_index : unaryfunc_Ptr;
         when nb_inplace_add =>
            nb_inplace_add : binaryfunc_Ptr;
         when nb_inplace_and =>
            nb_inplace_and : binaryfunc_Ptr;
         when nb_inplace_floor_divide =>
            nb_inplace_floor_divide : binaryfunc_Ptr;
         when nb_inplace_lshift =>
            nb_inplace_lshift : binaryfunc_Ptr;
         when nb_inplace_multiply =>
            nb_inplace_multiply : binaryfunc_Ptr;
         when nb_inplace_or =>
            nb_inplace_or : binaryfunc_Ptr;
         when nb_inplace_power =>
            nb_inplace_power : ternaryfunc_Ptr;
         when nb_inplace_remainder =>
            nb_inplace_remainder : binaryfunc_Ptr;
         when nb_inplace_rshift =>
            nb_inplace_rshift : binaryfunc_Ptr;
         when nb_inplace_subtract =>
            nb_inplace_subtract :binaryfunc_Ptr;
         when nb_inplace_true_divide =>
            nb_inplace_true_divide : binaryfunc_Ptr;
         when nb_inplace_xor =>
            nb_inplace_xor : binaryfunc_Ptr;
         when nb_int =>
            nb_int : unaryfunc_Ptr;
         when nb_invert =>
            nb_invert : unaryfunc_Ptr;
         when nb_lshift =>
            nb_lshift : binaryfunc_Ptr;
         when nb_multiply =>
            nb_multiply : binaryfunc_Ptr;
         when nb_negative =>
            nb_negative : unaryfunc_Ptr;
         when nb_or =>
            nb_or : binaryfunc_Ptr;
         when nb_positive =>
            nb_positive : unaryfunc_Ptr;
         when nb_power =>
            nb_power : ternaryfunc_Ptr;
         when nb_remainder =>
            nb_remainder : binaryfunc_Ptr;
         when nb_rshift =>
            nb_rshift : binaryfunc_Ptr;
         when nb_subtract =>
            nb_subtract : binaryfunc_Ptr;
         when nb_true_divide =>
            nb_true_divide : binaryfunc_Ptr;
         when nb_xor =>
            nb_xor : binaryfunc_Ptr;
         when sq_ass_item =>
            sq_ass_item : ssizeobjargproc_Ptr;
         when sq_concat =>
            sq_concat : binaryfunc_Ptr;
         when sq_contains =>
            sq_contains : objobjproc_Ptr;
         when sq_inplace_concat =>
            sq_inplace_concat : binaryfunc_Ptr;
         when sq_inplace_repeat =>
            sq_inplace_repeat : ssizeargfunc_Ptr;
         when sq_item =>
            sq_item : ssizeargfunc_Ptr;
         when sq_length =>
            sq_length : lenfunc_Ptr;
         when sq_repeat =>
            sq_repeat : ssizeargfunc_Ptr;
         when tp_alloc =>
            tp_alloc : allocfunc_Ptr;
         when tp_base =>
            tp_base : Object;
         when tp_bases =>
            tp_bases : Object; -- The list of
         when tp_call =>
            tp_call : ternaryfunc_Ptr;
         when tp_clear =>
            tp_clear : inquiry_Ptr;
         when tp_dealloc =>
            tp_dealloc : destructor_Ptr;
         when tp_del =>
            tp_del : destructor_Ptr;
         when tp_descr_get =>
            tp_descr_get : descrgetfunc_Ptr;
         when tp_descr_set =>
            tp_descr_set : descrsetfunc_Ptr;
         when tp_doc =>
            tp_doc : chars_ptr;
         when tp_getattr =>
            tp_getattr : getattrfunc_Ptr;
         when tp_getattro =>
            tp_getattro : getattrofunc_Ptr;
         when tp_hash =>
            tp_hash : hashfunc_Ptr;
         when tp_init =>
            tp_init : initproc_Ptr;
         when tp_is_gc =>
            tp_is_gc : inquiry_Ptr;
         when tp_iter =>
            tp_iter : getiterfunc_Ptr;
         when tp_iternext =>
            tp_iternext : iternextfunc_Ptr;
         when tp_methods =>
            tp_methods : access MethodDef;
         when tp_new =>
            tp_new : newfunc_Ptr;
         when tp_repr =>
            tp_repr : reprfunc_Ptr;
         when tp_richcompare =>
            tp_richcompare : richcmpfunc_Ptr;
         when tp_setattr =>
            tp_setattr : setattrfunc_Ptr;
         when tp_setattro =>
            tp_setattro : setattrofunc_Ptr;
         when tp_str =>
            tp_str : reprfunc_Ptr;
         when tp_traverse =>
            tp_traverse : traverseproc_Ptr;
         when tp_members =>
            tp_members : access MemberDef;
         when tp_getset =>
            tp_getset : access GetSetDef;
         when tp_free =>
            tp_free : freefunc_Ptr;
         when nb_matrix_multiply =>
            nb_matrix_multiply : binaryfunc_Ptr;
         when nb_inplace_matrix_multiply =>
            nb_inplace_matrix_multiply : binaryfunc_Ptr;
         when am_await =>
            am_await : unaryfunc_Ptr;
         when am_aiter =>
            am_aiter : unaryfunc_Ptr;
         when am_anext =>
            am_anext : unaryfunc_Ptr;
         when tp_finalize =>
            tp_finalize : destructor_Ptr;
      end case;
   end record;
   pragma Unchecked_Union (Type_Slot_Function);
   pragma Convention (C, Type_Slot_Function);

   type Type_Slot is record
      Slot : Type_Slot_Kind;     -- Slot id
      Func : Type_Slot_Function; -- Function pointer
   end record;
   pragma Convention (C, Type_Slot);

   End_Slot : constant Type_Slot := (None, (Kind_Of => None));

   TPFLAGS_HEAPTYPE                 : constant := 2**9;
   TPFLAGS_BASETYPE                 : constant := 2**10;
   TPFLAGS_HAVE_VECTORCALL          : constant := 2**11;
   TPFLAGS_READY                    : constant := 2**12;
   TPFLAGS_READYING                 : constant := 2**13;
   TPFLAGS_HAVE_GC                  : constant := 2**14;
   TPFLAGS_HAVE_STACKLESS_EXTENSION : constant := 2**15;
   TPFLAGS_METHOD_DESCRIPTOR        : constant := 2**17;
   TPFLAGS_HAVE_VERSION_TAG         : constant := 2**18;
   TPFLAGS_VALID_VERSION_TAG        : constant := 2**19;
   TPFLAGS_IS_ABSTRACT              : constant := 2**20;
   TPFLAGS_LONG_SUBCLASS            : constant := 2**24;
   TPFLAGS_LIST_SUBCLASS            : constant := 2**25;
   TPFLAGS_TUPLE_SUBCLASS           : constant := 2**26;
   TPFLAGS_BYTES_SUBCLASS           : constant := 2**27;
   TPFLAGS_UNICODE_SUBCLASS         : constant := 2**28;
   TPFLAGS_DICT_SUBCLASS            : constant := 2**29;
   TPFLAGS_BASE_EXC_SUBCLASS        : constant := 2**30;
   TPFLAGS_TYPE_SUBCLASS            : constant := 2**31;
   TPFLAGS_HAVE_FINALIZE            : constant := 2**0;
   TPFLAGS_DEFAULT                  : constant :=
      TPFLAGS_HAVE_STACKLESS_EXTENSION +
      TPFLAGS_HAVE_VERSION_TAG;

   type Type_Spec is record
      Name       : chars_ptr;
      Basic_Size : int;
      Item_Size  : int;
      Flags      : unsigned;
      Slots      : access Type_Slot; -- Terminated by null
   end record;
   pragma Convention (C, Type_Spec);
------------------------------------------------------------------------
   type ModuleDef_Slot is record
      Slot  : int;
      Value : System.Address;
   end record;
   pragma Convention (C, ModuleDef_Slot);

   type Visitproc is access function
        (  This : Object;
           Data : System.Address
        )  return int;
   pragma Convention (C, Visitproc);

   type Traverseproc is access function
        (  This  : Object;
           Visit : Visitproc;
           Data  : System.Address
        )  return int;
   pragma Convention (C, Traverseproc);

   type Inquiry is access function (This : Object) return int;
   pragma Convention (C, Inquiry);

   type Freefunc is access procedure (Address : System.Address);
   pragma Convention (C, Freefunc);

   type ModuleDef is record
      Base     : ModuleDef_Base;
      Name     : chars_ptr;
      Doc      : chars_ptr;
      Size     : ssize_t;
      Methods  : access MethodDef;
      Slots    : access ModuleDef_Slot;
      Traverse : Traverseproc;
      Clear    : Inquiry;
      Free     : Freefunc;
   end record;
   pragma Convention (C, ModuleDef);

   function Module_Create
            (  Modules : ModuleDef;
               Version : int := 1013
            )  return Object;

   type InitTab is access function return Object;
   pragma Convention (C, InitTab);

   function Import_AppendInittab
            (  Name : char_array;
               Init : InitTab
            )  return int;

   function Object_Str (Value : Object) return String;

   package Char_Ptrs is
       new Interfaces.C.Pointers
           (  Index              => size_t,
              Element            => char,
              Element_Array      => char_array,
              Default_Terminator => nul
           );

   function Is_Null (Reference : Object) return Boolean;

   type Pending_Call_Ptr is access function
        (  Data : System.Address
        )  return int;
   pragma Convention (C, Pending_Call_Ptr);

   function Quit (Data : System.Address) return int;
   pragma Convention (C, Quit);
------------------------------------------------------------------------
--
-- Python DLL entry points
--
   type AddPendingCall_Ptr is access function
        (  Call : Pending_Call_Ptr;
           Data : System.Address
        )  return int;
   pragma Convention (C, AddPendingCall_Ptr);

   type Bool_FromLLong_Ptr is access function
        (  Value : long
        )  return Object;
   pragma Convention (C, Bool_FromLLong_Ptr);

   type ByteArray_AsString_Ptr is access function
        (  Reference : Object
        )  return Char_Ptrs.Pointer;
   pragma Convention (C, ByteArray_AsString_Ptr);

   type ByteArray_FromStringAndSize_Ptr is access function
        (  Buffer : System.Address;
           Size   : ssize_t
        )  return Object;
   pragma Convention (C, ByteArray_FromStringAndSize_Ptr);

   type ByteArray_Size_Ptr is access function
        (  Reference : Object
        )  return ssize_t;
   pragma Convention (C, ByteArray_Size_Ptr);

   type Bytes_AsStringAndSize_Ptr is access function
        (  Reference : Object;
           Buffer    : access Char_Ptrs.Pointer;
           Size      : access ssize_t
        )  return int;
   pragma Convention (C, Bytes_AsStringAndSize_Ptr);

   type Bytes_FromStringAndSize_Ptr is access function
        (  Buffer : System.Address;
           Size   : ssize_t
        )  return Object;
   pragma Convention (C, Bytes_FromStringAndSize_Ptr);

   type Bytes_Size_Ptr is access function
        (  Reference : Object
        )  return ssize_t;
   pragma Convention (C, Bytes_Size_Ptr);

   type Callable_Check_Ptr is access
      function (Reference : Object) return int;
   pragma Convention (C, Callable_Check_Ptr);

   type CompileString_Ptr is access function
        (  Str       : char_array;
           File_Name : char_array;
           Start     : int
        )  return Object;
   pragma Convention (C, CompileString_Ptr);

--  type Dict_Check_Ptr is access
--     function (Dictionary : Object) return int;
--  pragma Convention (C, Dict_Check_Ptr);
--
--  type Dict_CheckExact_Ptr is access
--     function (Dictionary : Object) return int;
--  pragma Convention (C, Dict_CheckExact_Ptr);

   type Dict_Clear_Ptr is access procedure (Dictionary : Object);
   pragma Convention (C, Dict_Clear_Ptr);

   type Dict_Contains_Ptr is access
      function (Dictionary : Object; Key : Object) return int;
   pragma Convention (C, Dict_Contains_Ptr);

   type Dict_Copy_Ptr is access
      function (Dictionary : Object) return Object;
   pragma Convention (C, Dict_Copy_Ptr);

   type Dict_DelItem_Ptr is access function
        (  Dictionary : Object;
           Key        : Object
        )  return int;
   pragma Convention (C, Dict_DelItem_Ptr);

   type Dict_DelItemString_Ptr is access function
        (  Dictionary : Object;
           Key        : char_array
        )  return int;
   pragma Convention (C, Dict_DelItemString_Ptr);

   type Dict_GetItemString_Ptr is access function
        (  Dictionary : Object;
           Key        : char_array
        )  return Object;
   pragma Convention (C, Dict_GetItemString_Ptr);

   type Dict_Items_Ptr is access function
        (  Dictionary : Object
        )  return Object;
   pragma Convention (C, Dict_Items_Ptr);

   type Dict_Keys_Ptr is access function
        (  Dictionary : Object
        )  return Object;
   pragma Convention (C, Dict_Keys_Ptr);

   type Dict_Merge_Ptr is access function
        (  Dictionary : Object;
           Update     : Object;
           Override   : int
        )  return int;
   pragma Convention (C, Dict_Merge_Ptr);

   type Dict_Next_Ptr is access function
        (  Dictionary : Object;
           Position   : access ssize_t;
           Key        : access Object;
           Value      : access Object
        )  return int;
   pragma Convention (C, Dict_Next_Ptr);

   type Dict_New_Ptr is access function return Object;
   pragma Convention (C, Dict_New_Ptr);

-- type Dict_SetDefault_Ptr is access function
--      (  Dictionary : Object;
--         Key        : Object;
--         Default    : Object
--      )  return Object;
-- pragma Convention (C, Dict_SetDefault_Ptr);

   type Dict_SetItem_Ptr is access function
        (  Dictionary : Object;
           Key        : Object;
           Value      : Object
        )  return int;
   pragma Convention (C, Dict_SetItem_Ptr);

   type Dict_SetItemString_Ptr is access function
        (  Dictionary : Object;
           Key        : char_array;
           Value      : Object
        )  return int;
   pragma Convention (C, Dict_SetItemString_Ptr);

   type Dict_Size_Ptr is access function
        (  Dictionary : Object
        )  return ssize_t;
   pragma Convention (C, Dict_Size_Ptr);

   type DecRef_Ptr is access procedure (Reference : Object);
   pragma Convention (C, DecRef_Ptr);

   type Err_BadArgument_Ptr is access function return int;
   pragma Convention (C, Err_BadArgument_Ptr);

   type Err_BadInternalCall_Ptr is access procedure;
   pragma Convention (C, Err_BadInternalCall_Ptr);

   type Err_Clear_Ptr is access procedure;
   pragma Convention (C, Err_Clear_Ptr);

   type Err_Occurred_Ptr is access function return Object;
   pragma Convention (C, Err_Occurred_Ptr);

   type Err_Fetch_Ptr is access procedure
        (  Error_Type : out Object;
           Value      : out Object;
           Traceback  : out Object
        );
   pragma Convention (C, Err_Fetch_Ptr);

   type Err_NewException_Ptr is access function
        (  Name       : char_array;
           Base       : Object := Null_Object;
           Dictionary : Object := Null_Object
        )  return Object;
   pragma Convention (C, Err_NewException_Ptr);

   type Err_NormalizeException_Ptr is access procedure
        (  Error_Type : in out Object;
           Value      : in out Object;
           Traceback  : in out Object
        );
   pragma Convention (C, Err_NormalizeException_Ptr);

   type Err_Restore_Ptr is access procedure
        (  Error_Type : Object;
           Value      : Object;
           Traceback  : Object
        );
   pragma Convention (C, Err_Restore_Ptr);

   type Err_SetString_Ptr is access procedure
        (  Error   : Object;
           Message : char_array
        );
   pragma Convention (C, Err_SetString_Ptr);

   type EndInterpreter_Ptr is access procedure (State : System.Address);
   pragma Convention (C, EndInterpreter_Ptr);

   type Eval_InitThreads_Ptr is access procedure;
   pragma Convention (C, Eval_InitThreads_Ptr);

   type Eval_RestoreThread_Ptr is access
      procedure (State : ThreadState);
   pragma Convention (C, Eval_RestoreThread_Ptr);

   type Eval_SaveThread_Ptr is access function return ThreadState;
   pragma Convention (C, Eval_SaveThread_Ptr);

   type FinalizeEx_Ptr is access function return int;
   pragma Convention (C, FinalizeEx_Ptr);

   type Float_AsDouble_Ptr is access function
        (  Value : Object
        )  return double;
   pragma Convention (C, Float_AsDouble_Ptr);

   type Float_FromDouble_Ptr is access function
        (  Value : double
        )  return Object;
   pragma Convention (C, Float_FromDouble_Ptr);

   type Get_Object_Ptr is access function return Object;
   pragma Convention (C, Get_Object_Ptr);

-- type GILState_Check_Ptr is access function return int;
-- pragma Convention (C, GILState_Check_Ptr);

   type GILState_Ensure_Ptr is access function return GILState;
   pragma Convention (C, GILState_Ensure_Ptr);

   type GILState_Release_Ptr is access procedure (State : GILState);
   pragma Convention (C, GILState_Release_Ptr);

   type Import_AddModule_Ptr is access function
        (  Name : char_array
        )  return Object;
   pragma Convention (C, Import_AddModule_Ptr);

   type Import_AppendInittab_Ptr is access function
        (  Name : char_array;
           Init : InitTab
        )  return int;
   pragma Convention (C, Import_AppendInittab_Ptr);

   type IncRef_Ptr is access procedure (Reference : Object);
   pragma Convention (C, IncRef_Ptr);

   type InitializeEx_Ptr is access procedure (Init_Sigs : int);
   pragma Convention (C, InitializeEx_Ptr);

   type Import_ExecCodeModuleEx_Ptr is access function
        (  Name : char_array;
           Code : Object;
           Path : access constant char := null
        )  return Object;
   pragma Convention (C, Import_ExecCodeModuleEx_Ptr);

   type Import_GetModule_Ptr is access function
        (  Name : Object
        )  return Object;
   pragma Convention (C, Import_GetModule_Ptr);

   type Import_Import_Ptr is access
      function (Name : Object) return Object;
   pragma Convention (C, Import_Import_Ptr);

   type Import_ImportModule_Ptr is access function
        (  Name : char_array
        )  return Object;
   pragma Convention (C, Import_ImportModule_Ptr);

   type Index_Check_Ptr is access function
        (  Value : Object
        )  return int;
   pragma Convention (C,  Index_Check_Ptr);

   type Iter_Check_Ptr is access function
        (  Iterator : Object
        )  return int;
   pragma Convention (C, Iter_Check_Ptr);

   type Iter_Next_Ptr is access function
        (  Iterator : Object
        )  return Object;
   pragma Convention (C, Iter_Next_Ptr);

   type List_Append_Ptr is access function
        (  List : Object;
           Item : Object
        )  return int;
   pragma Convention (C, List_Append_Ptr);

   type List_AsTuple_Ptr is access
      function (List : Object) return Object;
   pragma Convention (C, List_AsTuple_Ptr);

-- type List_Check_Ptr is access function (List : Object) return int;
-- pragma Convention (C, List_Check_Ptr);

-- type List_CheckExact_Ptr is access
--    function (List : Object) return int;
-- pragma Convention (C, List_CheckExact_Ptr);

   type List_GetItem_Ptr is access function
        (  List  : Object;
           Index : ssize_t
        )  return Object;
   pragma Convention (C, List_GetItem_Ptr);

   type List_GetSlice_Ptr is access function
        (  List : Object;
           Low  : ssize_t;
           High : ssize_t
        )  return Object;
   pragma Convention (C, List_GetSlice_Ptr);

   type List_Insert_Ptr is access function
        (  List  : Object;
           Index : ssize_t;
           Item  : Object
        )  return int;
   pragma Convention (C, List_Insert_Ptr);

   type List_New_Ptr is access function (Size : ssize_t) return Object;
   pragma Convention (C, List_New_Ptr);

   type List_Reverse_Ptr is access function (List : Object) return int;
   pragma Convention (C, List_Reverse_Ptr);

   type List_SetItem_Ptr is access function
        (  List  : Object;
           Index : ssize_t;
           Item  : Object
        )  return int;
   pragma Convention (C, List_SetItem_Ptr);

   type List_SetSlice_Ptr is access function
        (  List  : Object;
           Low   : ssize_t;
           High  : ssize_t;
           Items : Object
        )  return int;
   pragma Convention (C, List_SetSlice_Ptr);

   type List_Size_Ptr is access function
        (  List : Object
        )  return ssize_t;
   pragma Convention (C, List_Size_Ptr);

   type List_Sort_Ptr is access function (List : Object) return int;
   pragma Convention (C, List_Sort_Ptr);

   type Long_AsLong_Ptr is access function
        (  Reference : Object
        )  return long;
   pragma Convention (C, Long_AsLong_Ptr);

   type Long_AsLongLong_Ptr is new System.Address;

   type Long_AsUnsignedLong_Ptr is access function
        (  Reference : Object
        )  return unsigned_long;
   pragma Convention (C, Long_AsUnsignedLong_Ptr);

   type Long_AsUnsignedLongLong_Ptr is new System.Address;

   type Long_FromLong_Ptr is access function
        (  Value : long
        )  return Object;
   pragma Convention (C, Long_FromLong_Ptr);

   type Long_FromLongLong_Ptr is new System.Address;

   type Long_FromUnsignedLong_Ptr is access function
        (  Value : unsigned_long
        )  return Object;
   pragma Convention (C, Long_FromUnsignedLong_Ptr);

   type Long_FromUnsignedLongLong_Ptr is new System.Address;

   type Module_AddObject_Ptr is access function
        (  Module : Object;
           Name   : char_array;
           Value  : Object
        )  return int;
   pragma Convention (C, Module_AddObject_Ptr);

   type Module_Create_Ptr is access function
        (  Definition : ModuleDef;
           Version    : int
        )  return Object;
   pragma Convention (C, Module_Create_Ptr);

   type Module_GetDict_Ptr is access function
        (  Module : Object
        )  return Object;
   pragma Convention (C, Module_GetDict_Ptr);

   type Module_GetName_Ptr is access function
        (  Module : Object
        )  return chars_ptr;
   pragma Convention (C, Module_GetName_Ptr);

   type NewInterpreter_Ptr is access
      function return System.Address;
   pragma Convention (C, NewInterpreter_Ptr);

   type Number_Absolute_Ptr is access function
        (  Value : Object
        )  return Object;
   pragma Convention (C,  Number_Absolute_Ptr);

   type Number_Add_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_Add_Ptr);

   type Number_And_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_And_Ptr);

   type Number_AsSsize_t_Ptr is access function
        (  Value : Object;
           Error : Object
        )  return size_t;
   pragma Convention (C,  Number_AsSsize_t_Ptr);

   type Number_Check_Ptr is access function
        (  Value : Object
        )  return int;
   pragma Convention (C,  Number_Check_Ptr);

   type Number_Divmod_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_Divmod_Ptr);

   type Number_Float_Ptr is access function
        (  Value : Object
        )  return Object;
   pragma Convention (C,  Number_Float_Ptr);

   type Number_FloorDivide_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_FloorDivide_Ptr);

   type Number_Index_Ptr is access function
        (  Value : Object
        )  return Object;
   pragma Convention (C,  Number_Index_Ptr);

   type Number_InPlaceAdd_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_InPlaceAdd_Ptr);

   type Number_InPlaceAnd_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_InPlaceAnd_Ptr);

   type Number_InPlaceFloorDivide_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_InPlaceFloorDivide_Ptr);

   type Number_InPlaceLshift_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_InPlaceLshift_Ptr);

   type Number_InPlaceMatrixMultiply_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_InPlaceMatrixMultiply_Ptr);

   type Number_InPlaceMultiply_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_InPlaceMultiply_Ptr);

   type Number_InPlaceOr_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_InPlaceOr_Ptr);

   type Number_InPlacePower_Ptr is access function
        (  Left     : Object;
           Right    : Object;
           Optional : Object
        )  return Object;
   pragma Convention (C,  Number_InPlacePower_Ptr);

   type Number_InPlaceRemainder_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_InPlaceRemainder_Ptr);

   type Number_InPlaceRshift_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_InPlaceRshift_Ptr);

   type Number_InPlaceSubtract_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_InPlaceSubtract_Ptr);

   type Number_InPlaceTrueDivide_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_InPlaceTrueDivide_Ptr);

   type Number_InPlaceXor_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_InPlaceXor_Ptr);

   type Number_Invert_Ptr is access function
        (  Value : Object
        )  return Object;
   pragma Convention (C,  Number_Invert_Ptr);

   type Number_Long_Ptr is access function
        (  Value : Object
        )  return Object;
   pragma Convention (C,  Number_Long_Ptr);

   type Number_Lshift_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_Lshift_Ptr);

   type Number_MatrixMultiply_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_MatrixMultiply_Ptr);

   type Number_Multiply_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_Multiply_Ptr);

   type Number_Negative_Ptr is access function
        (  Value : Object
        )  return Object;
   pragma Convention (C,  Number_Negative_Ptr);

   type Number_Or_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_Or_Ptr);

   type Number_Positive_Ptr is access function
        (  Value : Object
        )  return Object;
   pragma Convention (C,  Number_Positive_Ptr);

   type Number_Power_Ptr is access function
        (  Left     : Object;
           Right    : Object;
           Optional : Object
        )  return Object;
   pragma Convention (C,  Number_Power_Ptr);

   type Number_Remainder_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_Remainder_Ptr);

   type Number_Rshift_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_Rshift_Ptr);

   type Number_Subtract_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_Subtract_Ptr);

   type Number_ToBase_Ptr is access function
        (  Value : Object;
           Base  : int
        )  return Object;
   pragma Convention (C,  Number_ToBase_Ptr);

   type Number_TrueDivide_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_TrueDivide_Ptr);

   type Number_Xor_Ptr is access function
        (  Left  : Object;
           Right : Object
        )  return Object;
   pragma Convention (C,  Number_Xor_Ptr);

   type Object_Bytes_Ptr is access function
        (  Thing : Object
        )  return Object;
   pragma Convention (C, Object_Bytes_Ptr);

   type Object_Call_Ptr is access function
        (  Operation : Object;
           Arguments : Object;
           Keywords  : Object
        )  return Object;
   pragma Convention (C, Object_Call_Ptr);

   type Object_CallNoArgs_Ptr is access
      function (Operation : Object) return Object;
   pragma Convention (C, Object_CallNoArgs_Ptr);

   type Object_CallObject_Ptr is access
      function (Operation : Object; Arguments : Object) return Object;
   pragma Convention (C, Object_CallObject_Ptr);

   type Object_DelItem_Ptr is access function
        (  Thing : Object;
           Key   : Object
        )  return int;
   pragma Convention (C, Object_DelItem_Ptr);

   type Object_Dir_Ptr is access
      function (Thing : Object) return Object;
   pragma Convention (C, Object_Dir_Ptr);

   type Object_GenericGetAttr_Ptr is access function
        (  Thing : Object;
           Name  : Object
        )  return Object;
   pragma Convention (C, Object_GenericGetAttr_Ptr);

   type Object_GenericSetAttr_Ptr is access function
        (  Thing : Object;
           Name  : Object;
           Value : Object
        )  return int;
   pragma Convention (C, Object_GenericSetAttr_Ptr);

   type Object_GetAttr_Ptr is access function
        (  Thing     : Object;
           Attribute : Object
        )  return Object;
   pragma Convention (C, Object_GetAttr_Ptr);

   type Object_GetAttrString_Ptr is access function
        (  Thing : Object;
           Name  : char_array
        )  return Object;
   pragma Convention (C, Object_GetAttrString_Ptr);

   type Object_GetItem_Ptr is access function
        (  Thing : Object;
           Key   : Object
        )  return Object;
   pragma Convention (C, Object_GetItem_Ptr);

   type Object_GetIter_Ptr is access
      function (Thing : Object) return Object;
   pragma Convention (C, Object_GetIter_Ptr);

   type Object_HasAttr_Ptr is access function
        (  Thing     : Object;
           Attribute : Object
        )  return int;
   pragma Convention (C, Object_HasAttr_Ptr);

   type Object_HasAttrString_Ptr is access function
        (  Thing : Object;
           Name  : char_array
        )  return int;
   pragma Convention (C, Object_HasAttrString_Ptr);

   type Object_IsInstance_Ptr is access function
        (  Thing : Object;
           Class : Object
        )  return int;
   pragma Convention (C, Object_IsInstance_Ptr);

   type Object_IsSubclass_Ptr is access function
        (  Derived : Object;
           Class   : Object
        )  return int;
   pragma Convention (C, Object_IsSubclass_Ptr);

   type Object_RichCompareBool_Ptr is access function
        (  Left  : Object;
           Right : Object;
           Op    : int
        )  return int;
   pragma Convention (C, Object_RichCompareBool_Ptr);

   type Object_SetAttr_Ptr is access function
        (  Thing     : Object;
           Attribute : Object;
           Value     : Object
        )  return int;
   pragma Convention (C, Object_SetAttr_Ptr);

   type Object_SetAttrString_Ptr is access function
        (  Thing : Object;
           Name  : char_array;
           Value : Object
        )  return int;
   pragma Convention (C, Object_SetAttrString_Ptr);

   type Object_SetItem_Ptr is access function
        (  Thing : Object;
           Key   : Object;
           Value : Object
        )  return int;
   pragma Convention (C, Object_SetItem_Ptr);

   type Object_Size_Ptr is access function
        (  Thing : Object
        )  return ssize_t;
   pragma Convention (C, Object_Size_Ptr);

   type Object_Str_Ptr is access function
        (  Thing : Object
        )  return Object;
   pragma Convention (C, Object_Str_Ptr);

   type Object_Type_Ptr is access function
        (  Thing : Object
        )  return Object;
   pragma Convention (C, Object_Type_Ptr);

   type Capsule_GetContext_Ptr is access function
        (  Capsule : Object
        )  return System.Address;
   pragma Convention (C, Capsule_GetContext_Ptr);

   type Capsule_Destructor_Ptr is access procedure (Capsule : Object);
   pragma Convention (C, Capsule_Destructor_Ptr);

   type Capsule_GetDestructor_Ptr is access function
        (  Capsule : Object
        )  return Capsule_Destructor_Ptr;
   pragma Convention (C, Capsule_GetDestructor_Ptr);

   type Capsule_GetName_Ptr is access function
        (  Capsule : Object
        )  return chars_ptr;
   pragma Convention (C, Capsule_GetName_Ptr);

   type Capsule_GetPointer_Ptr is access function
        (  Capsule : Object;
           Name    : char_array
        )  return System.Address;
   pragma Convention (C, Capsule_GetPointer_Ptr);

   type Capsule_Import_Ptr is access function
        (  Name     : char_array;
           No_Block : int := 0
        )  return System.Address;
   pragma Convention (C, Capsule_Import_Ptr);

   type Capsule_IsValid_Ptr is access function
        (  Capsule : Object;
           Name    : char_array
        )  return int;
   pragma Convention (C, Capsule_IsValid_Ptr);

   type Capsule_New_Ptr is access function
        (  Pointer    : System.Address;
           Name       : char_array;
           Destructor : Capsule_Destructor_Ptr
        )  return Object;
   pragma Convention (C, Capsule_New_Ptr);

   type Capsule_SetContext_Ptr is access function
        (  Capsule : Object;
           Context : System.Address
        )  return int;
   pragma Convention (C, Capsule_SetContext_Ptr);

   type Capsule_SetDestructor_Ptr is access function
        (  Capsule    : Object;
           Destructor : Capsule_Destructor_Ptr
        )  return int;
   pragma Convention (C, Capsule_SetDestructor_Ptr);

   type Capsule_SetName_Ptr is access function
        (  Capsule : Object;
           Name    : char_array
        )  return int;
   pragma Convention (C, Capsule_SetName_Ptr);

   type Capsule_SetPointer_Ptr is access function
        (  Capsule : Object;
           Pointer : System.Address
        )  return int;
   pragma Convention (C, Capsule_SetPointer_Ptr);

   type Sequence_Check_Ptr is access
      function (Sequence : Object) return int;
   pragma Convention (C, Sequence_Check_Ptr);

   type Sequence_Concat_Ptr is access
      function (Left, Right : Object) return Object;
   pragma Convention (C, Sequence_Concat_Ptr);

   type Sequence_Contains_Ptr is access function
        (  Sequence : Object;
           Item     : Object
        )  return int;
   pragma Convention (C, Sequence_Contains_Ptr);

   type Sequence_Count_Ptr is access function
        (  Sequence : Object;
           Item     : Object
        )  return ssize_t;
   pragma Convention (C, Sequence_Count_Ptr);

   type Sequence_DelItem_Ptr is access function
        (  Sequence : Object;
           Index    : ssize_t
        )  return int;
   pragma Convention (C, Sequence_DelItem_Ptr);

   type Sequence_DelSlice_Ptr is access function
        (  Sequence : Object;
           Low      : ssize_t;
           High     : ssize_t
        )  return int;
   pragma Convention (C, Sequence_DelSlice_Ptr);

   type Sequence_GetItem_Ptr is access function
        (  Sequence : Object;
           Index    : ssize_t
        )  return Object;
   pragma Convention (C, Sequence_GetItem_Ptr);

   type Sequence_GetSlice_Ptr is access function
        (  Sequence : Object;
           Low      : ssize_t;
           High     : ssize_t
        )  return Object;
   pragma Convention (C, Sequence_GetSlice_Ptr);

   type Sequence_Index_Ptr is access function
        (  Sequence : Object;
           Item     : Object
        )  return ssize_t;
   pragma Convention (C, Sequence_Index_Ptr);

   type Sequence_List_Ptr is access
      function (Sequence : Object) return Object;
   pragma Convention (C, Sequence_List_Ptr);

   type Sequence_Repeat_Ptr is access function
        (  Sequence : Object;
           Count    : ssize_t
        )  return Object;
   pragma Convention (C, Sequence_Repeat_Ptr);

   type Sequence_SetItem_Ptr is access function
        (  Sequence : Object;
           Index    : ssize_t;
           Item     : Object
        )  return int;
   pragma Convention (C, Sequence_SetItem_Ptr);

   type Sequence_SetSlice_Ptr is access function
        (  Sequence : Object;
           Low      : ssize_t;
           High     : ssize_t;
           Items    : Object
        )  return int;
   pragma Convention (C, Sequence_SetSlice_Ptr);

   type Sequence_Size_Ptr is access
      function (Sequence : Object) return ssize_t;
   pragma Convention (C, Sequence_Size_Ptr);

   type Sequence_Tuple_Ptr is access
      function (Sequence : Object) return Object;
   pragma Convention (C, Sequence_Tuple_Ptr);

   type Set_Add_Ptr is access
      function (Set : Object; Item : Object) return int;
   pragma Convention (C, Set_Add_Ptr);

   type Set_Check_Ptr is access
      function (Set : Object) return int;
   pragma Convention (C, Set_Check_Ptr);

   type Set_CheckExact_Ptr is access
      function (Set : Object) return int;
   pragma Convention (C, Set_CheckExact_Ptr);

   type FrozenSet_Check_Ptr is access
      function (Set : Object) return int;
   pragma Convention (C, FrozenSet_Check_Ptr);

   type FrozenSet_CheckExact_Ptr is access
      function (Set : Object) return int;
   pragma Convention (C, FrozenSet_CheckExact_Ptr);

   type Set_Clear_Ptr is access function (Set : Object) return int;
   pragma Convention (C, Set_Clear_Ptr);

   type Set_Contains_Ptr is access
      function (Set : Object; Item : Object) return int;
   pragma Convention (C, Set_Contains_Ptr);

   type Set_Discard_Ptr is access
      function (Set : Object; Item : Object) return int;
   pragma Convention (C, Set_Discard_Ptr);

   type Frozenset_New_Ptr is access
      function (Initial : Object := Null_Object) return Object;
   pragma Convention (C, Frozenset_New_Ptr);

   type Set_New_Ptr is access
      function (Initial : Object := Null_Object) return Object;
   pragma Convention (C, Set_New_Ptr);

   type Set_Pop_Ptr is access function (Set : Object) return Object;
   pragma Convention (C, Set_Pop_Ptr);

   type Set_Size_Ptr is access function (Set : Object) return ssize_t;
   pragma Convention (C, Set_Size_Ptr);

   type Sys_GetObject_Ptr is access function
        (  Name : char_array
        )  return OBject;
   pragma Convention (C, Sys_GetObject_Ptr);

-- type Sys_GetSizeOf_Ptr is access function
--      (  Thing : Object
--      )  return ssize_t;
-- pragma Convention (C, Sys_GetSizeOf_Ptr);

   function To_Ada (Value : Object) return Time;

   type ThreadState_Get_Ptr is access function return ThreadState;
   pragma Convention (C, ThreadState_Get_Ptr);

   type ThreadState_Swap_Ptr is access
      function (State : ThreadState) return ThreadState;
   pragma Convention (C, ThreadState_Swap_Ptr);

   type Tuple_GetItem_Ptr is access function
        (  Tuple    : Object;
           Position : ssize_t
        )  return Object;
   pragma Convention (C, Tuple_GetItem_Ptr);

   type Tuple_GetSlice_Ptr is access function
        (  Tuple : Object;
           Low   : ssize_t;
           High  : ssize_t
        )  return Object;
   pragma Convention (C, Tuple_GetSlice_Ptr);

   type Tuple_New_Ptr is access function (Size : ssize_t) return Object;
   pragma Convention (C, Tuple_New_Ptr);

   type Tuple_SetItem_Ptr is access function
        (  Tuple    : Object;
           Position : ssize_t;
           Item     : Object
        )  return int;
   pragma Convention (C, Tuple_SetItem_Ptr);

   type Tuple_Size_Ptr is access
      function (Tuple : Object) return ssize_t;
   pragma Convention (C, Tuple_Size_Ptr);

   type Type_FromSpec_Ptr is access function
        (  Spec : Type_Spec
        )  return Object;
   pragma Convention (C, Type_FromSpec_Ptr);

   type Type_FromSpecWithBases_Ptr is access function
        (  Spec  : Type_Spec;
           Bases : Object
        )  return Object;
   pragma Convention (C, Type_FromSpecWithBases_Ptr);

   type Type_GetSlot_Ptr is access function
        (  Class : Object;
           Slot  : Type_Slot_Kind
        )  return Type_Slot_Function;
   pragma Convention (C, Type_GetSlot_Ptr);

   type Type_IsSubtype_Ptr is access
      function (Sub, Super : Object) return int;
   pragma Convention (C, Type_IsSubtype_Ptr);

   type Unicode_AsEncodedString_Ptr is access function
        (  Unicode  : Object;
           Encoding : char_array := "utf-8" & Nul;
           Errors   : char_array := "Error ~" & Nul
        )  return Object;
   pragma Convention (C, Unicode_AsEncodedString_Ptr);

   type Unicode_DecodeFSDefault_Ptr is access
      function (Name : char_array) return Object;
   pragma Convention (C, Unicode_DecodeFSDefault_Ptr);

   type Unicode_FromString_Ptr is access function
        (  Value : char_array
        )  return Object;
   pragma Convention (C, Unicode_FromString_Ptr);

   type Unicode_FromStringAndSize_Ptr is access function
        (  Value : System.Address;
           Size  : ssize_t
        )  return Object;
   pragma Convention (C, Unicode_FromStringAndSize_Ptr);

   type Unicode_GetLength_Ptr is access
      function (Value : Object) return ssize_t;
   pragma Convention (C, Unicode_GetLength_Ptr);

   type Unicode_ReadChar_Ptr is access function
        (  Value : Object;
           Index : ssize_t
        )  return Wide_Wide_Character;
   pragma Convention (C, Unicode_ReadChar_Ptr);
------------------------------------------------------------------------
   type Date_FromDate_Ptr is access function
        (  Year     : int;
           Month    : int;
           Day      : int;
           DateType : Object
        )  return Object;
   pragma Convention (C, Date_FromDate_Ptr);

   type DateTime_FromDateAndTime_Ptr is access function
        (  Year        : int;
           Month       : int;
           Day         : int;
           Hour        : int;
           Minute      : int;
           Second      : int;
           Microsecond : int;
           Zone        : Object;
           DateType    : Object
        )  return Object;
   pragma Convention (C, DateTime_FromDateAndTime_Ptr);

   type Time_FromTime_Ptr is access function
        (  Hour        : int;
           Minute      : int;
           Second      : int;
           Microsecond : int;
           Zone        : Object;
           DateType    : Object
        )  return Object;
   pragma Convention (C, Time_FromTime_Ptr);

   type Delta_FromDelta_Ptr is access function
        (  Hour        : int;
           Minute      : int;
           Second      : int;
           Microsecond : int;
           DateType    : Object
        )  return Object;
   pragma Convention (C, Delta_FromDelta_Ptr);

   type TimeZone_FromTimeZone_Ptr is access function
        (  Offset : Object;
           Name   : Object
        )  return Object;
   pragma Convention (C, TimeZone_FromTimeZone_Ptr);

   type DateTime_FromTimestamp_Ptr is access function
        (  DateType  : Object;
           Arguments : Object;
           Nothing   : Object := Null_Object
        )  return Object;
   pragma Convention (C, DateTime_FromTimestamp_Ptr);

   type Date_FromTimestamp_Ptr is access function
        (  DateType  : Object;
           Arguments : Object
        )  return Object;
   pragma Convention (C, Date_FromTimestamp_Ptr);

   type DateTime_FromDateAndTimeAndFold_Ptr is access function
        (  Year        : int;
           Month       : int;
           Day         : int;
           Hour        : int;
           Minute      : int;
           Second      : int;
           Microsecond : int;
           Zone        : Object;
           Fold        : int;
           DateType    : Object
        )  return Object;
   pragma Convention (C, DateTime_FromDateAndTimeAndFold_Ptr);

   type Time_FromTimeAndFold_Ptr is access function
        (  Hour        : int;
           Minute      : int;
           Second      : int;
           Microsecond : int;
           Zone        : Object;
           Fold        : int;
           DateType    : Object
        )  return Object;
   pragma Convention (C, Time_FromTimeAndFold_Ptr);

   type DateTime_CAPI is record
       -- type objects
      DateType     : Object;
      DateTimeType : Object;
      TimeType     : Object;
      DeltaType    : Object;
      TZInfoType   : Object;
         -- singletons
      TimeZone_UTC : Object;
       -- constructors
      Date_FromDate            : Date_FromDate_Ptr;
      DateTime_FromDateAndTime : DateTime_FromDateAndTime_Ptr;
      Time_FromTime            : Time_FromTime_Ptr;
      Delta_FromDelta          : Delta_FromDelta_Ptr;
      TimeZone_FromTimeZone    : TimeZone_FromTimeZone_Ptr;
       -- constructors for the DB API
      DateTime_FromTimestamp   : DateTime_FromTimestamp_Ptr;
      Date_FromTimestamp       : Date_FromTimestamp_Ptr;
       -- PEP 495 constructors
      DateTime_FromDateAndTimeAndFold :
                                 DateTime_FromDateAndTimeAndFold_Ptr;
      Time_FromTimeAndFold     : Time_FromTimeAndFold_Ptr;
   end record;
   pragma Convention (C, DateTime_CAPI);
   type DateTime_CAPI_Ptr is access all DateTime_CAPI;
   pragma Convention (C, DateTime_CAPI_Ptr);

   package Links is
      AddPendingCall               : AddPendingCall_Ptr;
      Bool_FromLong                : Bool_FromLLong_Ptr;
      ByteArray_AsString           : ByteArray_AsString_Ptr;
      ByteArray_FromStringAndSize  : ByteArray_FromStringAndSize_Ptr;
      ByteArray_Size               : ByteArray_Size_Ptr;
      ByteArray_Type               : Object := Null_Object;
      Bytes_AsStringAndSize        : Bytes_AsStringAndSize_Ptr;
      Bytes_FromStringAndSize      : Bytes_FromStringAndSize_Ptr;
      Bytes_Size                   : Bytes_Size_Ptr;
      Bytes_Type                   : Object := Null_Object;
      Callable_Check               : Callable_Check_Ptr;
      Capsule_GetContext           : Capsule_GetContext_Ptr;
      Capsule_GetDestructor        : Capsule_GetDestructor_Ptr;
      Capsule_GetName              : Capsule_GetName_Ptr;
      Capsule_GetPointer           : Capsule_GetPointer_Ptr;
      Capsule_Import               : Capsule_Import_Ptr;
      Capsule_IsValid              : Capsule_IsValid_Ptr;
      Capsule_New                  : Capsule_New_Ptr;
      Capsule_SetContext           : Capsule_SetContext_Ptr;
      Capsule_SetDestructor        : Capsule_SetDestructor_Ptr;
      Capsule_SetName              : Capsule_SetName_Ptr;
      Capsule_SetPointer           : Capsule_SetPointer_Ptr;
      CompileString                : CompileString_Ptr;
      DateTime_CAPI                : DateTime_CAPI_Ptr;
      DecRef                       : DecRef_Ptr;
--    Dict_Check                   : Dict_Check_Ptr;
--    Dict_CheckExact              : Dict_CheckExact_Ptr;
      Dict_Clear                   : Dict_Clear_Ptr;
      Dict_Contains                : Dict_Contains_Ptr;
      Dict_Copy                    : Dict_Copy_Ptr;
      Dict_DelItem                 : Dict_DelItem_Ptr;
      Dict_DelItemString           : Dict_DelItemString_Ptr;
      Dict_GetItemString           : Dict_GetItemString_Ptr;
      Dict_Items                   : Dict_Items_Ptr;
      Dict_Keys                    : Dict_Keys_Ptr;
      Dict_New                     : Dict_New_Ptr;
      Dict_Merge                   : Dict_Merge_Ptr;
      Dict_Next                    : Dict_Next_Ptr;
--    Dict_SetDefault              : Dict_Setdefault_Ptr;
      Dict_SetItem                 : Dict_SetItem_Ptr;
      Dict_SetItemString           : Dict_SetItemString_Ptr;
      Dict_Size                    : Dict_Size_Ptr;
      Dict_Type                    : Object := Null_Object;
      EndInterpreter               : EndInterpreter_Ptr;
      Err_BadArgument              : Err_BadArgument_Ptr;
      Err_BadInternalCall          : Err_BadInternalCall_Ptr;
      Err_Clear                    : Err_Clear_Ptr;
      Err_Fetch                    : Err_Fetch_Ptr;
      Err_NewException             : Err_NewException_Ptr;
      Err_NormalizeException       : Err_NormalizeException_Ptr;
      Err_Occurred                 : Err_Occurred_Ptr;
      Err_Restore                  : Err_Restore_Ptr;
      Err_SetString                : Err_SetString_Ptr;
      Eval_InitThreads             : Eval_InitThreads_Ptr;
      Eval_RestoreThread           : Eval_RestoreThread_Ptr;
      Eval_SaveThread              : Eval_SaveThread_Ptr;
      Exc_KeyError                 : Object_Ptr;
      Exc_LookupError              : Object_Ptr;
      Exc_NameError                : Object_Ptr;
      Exc_PermissionError          : Object_Ptr;
      Exc_RuntimeError             : Object_Ptr;
      Exc_ValueError               : Object_Ptr;
      Exc_SyntaxError              : Object_Ptr;
      Exc_SystemError              : Object_Ptr;
      Exc_TimeoutError             : Object_Ptr;
      Exc_TypeError                : Object_Ptr;
      False                        : Object := Null_Object;
      FinalizeEx                   : FinalizeEx_Ptr;
      Float_AsDouble               : Float_AsDouble_Ptr;
      Float_FromDouble             : Float_FromDouble_Ptr;
      Float_Type                   : Object := Null_Object;
--    GILState_Check               : GILState_Check_Ptr;
      GILState_Ensure              : GILState_Ensure_Ptr;
      GILState_Release             : GILState_Release_Ptr;
      IncRef                       : IncRef_Ptr;
      Import_AddModule             : Import_AddModule_Ptr;
      Import_AppendInittab         : Import_AppendInittab_Ptr;
      Import_ExecCodeModuleEx      : Import_ExecCodeModuleEx_Ptr;
      Import_GetModule             : Import_GetModule_Ptr;
      Import_Import                : Import_Import_Ptr;
      Import_ImportModule          : Import_ImportModule_Ptr;
      Index_Check                  : Index_Check_Ptr;
      InitializeEx                 : InitializeEx_Ptr;
      Iter_Check                   : Iter_Check_Ptr;
      Iter_Next                    : Iter_Next_Ptr;
--    List_Check                   : List_Check_Ptr;
--    List_CheckExact              : List_CheckExact_Ptr;
      List_Append                  : List_Append_Ptr;
      List_AsTuple                 : List_AsTuple_Ptr;
      List_GetItem                 : List_GetItem_Ptr;
      List_GetSlice                : List_GetSlice_Ptr;
      List_Insert                  : List_Insert_Ptr;
      List_New                     : List_New_Ptr;
      List_Reverse                 : List_Reverse_Ptr;
      List_SetItem                 : List_SetItem_Ptr;
      List_SetSlice                : List_SetSlice_Ptr;
      List_Size                    : List_Size_Ptr;
      List_Sort                    : List_Sort_Ptr;
      List_Type                    : Object := Null_Object;
      Long_AsLong                  : Long_AsLong_Ptr;
      Long_AsLongLong              : Long_AsLongLong_Ptr;
      Long_AsUnsignedLong          : Long_AsUnsignedLong_Ptr;
      Long_AsUnsignedLongLong      : Long_AsUnsignedLongLong_Ptr;
      Long_FromLong                : Long_FromLong_Ptr;
      Long_FromLongLong            : Long_FromLongLong_Ptr;
      Long_FromUnsignedLong        : Long_FromUnsignedLong_Ptr;
      Long_FromUnsignedLongLong    : Long_FromUnsignedLongLong_Ptr;
      Long_Type                    : Object := Null_Object;
      Module_AddObject             : Module_AddObject_Ptr;
      Module_Create                : Module_Create_Ptr;
      Module_GetDict               : Module_GetDict_Ptr;
      Module_GetName               : Module_GetName_Ptr;
      NewInterpreter               : NewInterpreter_Ptr;
      None                         : Object := Null_Object;
      Number_Absolute              : Number_Absolute_Ptr;
      Number_Add                   : Number_Add_Ptr;
      Number_And                   : Number_And_Ptr;
      Number_AsSsize_t             : Number_AsSsize_t_Ptr;
      Number_Check                 : Number_Check_Ptr;
      Number_Divmod                : Number_Divmod_Ptr;
      Number_Float                 : Number_Float_Ptr;
      Number_FloorDivide           : Number_FloorDivide_Ptr;
      Number_Index                 : Number_Index_Ptr;
      Number_InPlaceAdd            : Number_InPlaceAdd_Ptr;
      Number_InPlaceAnd            : Number_InPlaceAnd_Ptr;
      Number_InPlaceFloorDivide    : Number_InPlaceFloorDivide_Ptr;
      Number_InPlaceLshift         : Number_InPlaceLshift_Ptr;
      Number_InPlaceMatrixMultiply : Number_InPlaceMatrixMultiply_Ptr;
      Number_InPlaceMultiply       : Number_InPlaceMultiply_Ptr;
      Number_InPlaceOr             : Number_InPlaceOr_Ptr;
      Number_InPlacePower          : Number_InPlacePower_Ptr;
      Number_InPlaceRemainder      : Number_InPlaceRemainder_Ptr;
      Number_InPlaceRshift         : Number_InPlaceRshift_Ptr;
      Number_InPlaceSubtract       : Number_InPlaceSubtract_Ptr;
      Number_InPlaceTrueDivide     : Number_InPlaceTrueDivide_Ptr;
      Number_InPlaceXor            : Number_InPlaceXor_Ptr;
      Number_Invert                : Number_Invert_Ptr;
      Number_Long                  : Number_Long_Ptr;
      Number_Lshift                : Number_Lshift_Ptr;
      Number_MatrixMultiply        : Number_MatrixMultiply_Ptr;
      Number_Multiply              : Number_Multiply_Ptr;
      Number_Negative              : Number_Negative_Ptr;
      Number_Or                    : Number_Or_Ptr;
      Number_Positive              : Number_Positive_Ptr;
      Number_Power                 : Number_Power_Ptr;
      Number_Remainder             : Number_Remainder_Ptr;
      Number_Rshift                : Number_Rshift_Ptr;
      Number_Subtract              : Number_Subtract_Ptr;
      Number_ToBase                : Number_ToBase_Ptr;
      Number_TrueDivide            : Number_TrueDivide_Ptr;
      Number_Xor                   : Number_Xor_Ptr;
      Object_Bytes                 : Object_Bytes_Ptr;
      Object_Call                  : Object_Call_Ptr;
      Object_CallNoArgs            : Object_CallNoArgs_Ptr;
      Object_CallObject            : Object_CallObject_Ptr;
      Object_DelItem               : Object_DelItem_Ptr;
      Object_Dir                   : Object_Dir_Ptr;
      Object_HasAttr               : Object_HasAttr_Ptr;
      Object_HasAttrString         : Object_HasAttrString_Ptr;
      Object_IsInstance            : Object_IsInstance_Ptr;
      Object_IsSubclass            : Object_IsSubclass_Ptr;
      Object_GenericGetAttr        : Object_GenericGetAttr_Ptr;
      Object_GenericSetAttr        : Object_GenericSetAttr_Ptr;
      Object_GetAttr               : Object_GetAttr_Ptr;
      Object_GetAttrString         : Object_GetAttrString_Ptr;
      Object_GetItem               : Object_GetItem_Ptr;
      Object_GetIter               : Object_GetIter_Ptr;
      Object_RichCompareBool       : Object_RichCompareBool_Ptr;
      Object_SetAttr               : Object_SetAttr_Ptr;
      Object_SetAttrString         : Object_SetAttrString_Ptr;
      Object_SetItem               : Object_SetItem_Ptr;
      Object_Size                  : Object_Size_Ptr;
      Object_Str                   : Object_Str_Ptr;
      Object_Type                  : Object_Type_Ptr;
      Sequence_Check               : Sequence_Check_Ptr;
      Sequence_Concat              : Sequence_Concat_Ptr;
      Sequence_Contains            : Sequence_Contains_Ptr;
      Sequence_Count               : Sequence_Count_Ptr;
      Sequence_DelItem             : Sequence_DelItem_Ptr;
      Sequence_DelSlice            : Sequence_DelSlice_Ptr;
      Sequence_GetItem             : Sequence_GetItem_Ptr;
      Sequence_GetSlice            : Sequence_GetSlice_Ptr;
      Sequence_Index               : Sequence_Index_Ptr;
      Sequence_List                : Sequence_List_Ptr;
      Sequence_Repeat              : Sequence_Repeat_Ptr;
      Sequence_SetItem             : Sequence_SetItem_Ptr;
      Sequence_SetSlice            : Sequence_SetSlice_Ptr;
      Sequence_Size                : Sequence_Size_Ptr;
      Sequence_Tuple               : Sequence_Tuple_Ptr;
      Set_Add                      : Set_Add_Ptr;
      Set_Clear                    : Set_Clear_Ptr;
      Set_Contains                 : Set_Contains_Ptr;
      Set_Discard                  : Set_Discard_Ptr;
      Frozenset_New                : Frozenset_New_Ptr;
      Set_New                      : Set_New_Ptr;
      Set_Pop                      : Set_Pop_Ptr;
      Set_Size                     : Set_Size_Ptr;
      Set_Type                     : Object := Null_Object;
      Sys_GetObject                : Sys_GetObject_Ptr;
--    Sys_GetSizeOf                : Sys_GetSizeOf_Ptr;
      ThreadState_Get              : ThreadState_Get_Ptr;
      ThreadState_Swap             : ThreadState_Swap_Ptr;
      True                         : Object := Null_Object;
      Tuple_GetItem                : Tuple_GetItem_Ptr;
      Tuple_GetSlice               : Tuple_GetSlice_Ptr;
      Tuple_New                    : Tuple_New_Ptr;
      Tuple_SetItem                : Tuple_SetItem_Ptr;
      Tuple_Size                   : Tuple_Size_Ptr;
      Tuple_Type                   : Object := Null_Object;
      Type_FromSpec                : Type_FromSpec_Ptr;
      Type_FromSpecWithBases       : Type_FromSpecWithBases_Ptr;
      Type_GetSlot                 : Type_GetSlot_Ptr;
      Type_IsSubtype               : Type_IsSubtype_Ptr;
      Type_Type                    : Object := Null_Object;
      Unicode_AsEncodedString      : Unicode_AsEncodedString_Ptr;
      Unicode_DecodeFSDefault      : Unicode_DecodeFSDefault_Ptr;
      Unicode_FromString           : Unicode_FromString_Ptr;
      Unicode_FromStringAndSize    : Unicode_FromStringAndSize_Ptr;
      Unicode_GetLength            : Unicode_GetLength_Ptr;
      Unicode_ReadChar             : Unicode_ReadChar_Ptr;
      Unicode_Type                 : Object := Null_Object;
   end Links;
   procedure Check_Links (Library_Name : String);

   function As_Integer64 (Value : Object) return Interfaces.Integer_64;
   function As_Unsigned64 (Value : Object)
      return Interfaces.Unsigned_64;
   function As_String (Value : Object) return String;

   type Global_Interpreter_Lock is
      new Ada.Finalization.Limited_Controlled with
   record
      State : GILState;
   end record;

   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean;
   procedure Check_Spelling (Name : String);

   type Argument_Position is new Positive;
   type Object_Array is array (Argument_Position range <>) of Object;

   package Keyword_Tables is new Tables (Boolean);

   package Object_Tables is new Tables (Argument_Position);

   type Position_Array is
      array (Argument_Position range <>) of Positive;
   type Optional_Array is
      array (Argument_Position range <>) of Boolean;
--
-- Argument_List -- List of Python keyword arguments. Note that the list
--                  cannot  contain  reserved  keywords.   Use_Error  is
-- propagated on an attempt to include a reserved keyword.
--
   type Argument_List (Length : Argument_Position) is record
      Keys     : Object_Tables.Table;        -- Key tp position map
      Offsets  : Position_Array (1..Length); -- Position to key map
      Optional : Optional_Array (1..Length); -- Optional flag
   end record;
--
-- + -- Add a mandatory argument
--
   function "+" (Left : String) return Argument_List;
   function "+" (Left, Right : String) return Argument_List;
   function "+" (List : Argument_List; Key : String)
      return Argument_List;
--
-- - -- Add an optional argument
--
   function "-" (Left : String) return Argument_List;
   function "-" (Left, Right : String) return Argument_List;
   function "-" (List : Argument_List; Key : String)
      return Argument_List;
--
-- Parse -- Parsing keyed arguments
--
--    Args     - The positional arguments tuple as passed to the caller
--    Keywords - The keyed arguments dictionary
--    List     - The parsed arguments
--
-- Returns :
--
--    Actual  arguments  in their order.  If a argument  is omitted  the
--    result contains Null_Object in its position.
--
-- Exceptions :
--
--    Python_Error - An error in arguments and a Python exception is set
--
   function Parse
            (  Args     : Object;
               Keywords : Object;
               List     : Argument_List
            )  return Object_Array;
end Py;
