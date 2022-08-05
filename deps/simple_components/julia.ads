--                                                                    --
--  package Julia                   Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2018       --
--                                                                    --
--                                Last revision :  18:40 23 Oct 2021  --
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
--  The interface  to the Julia programming language.  The bindings  are
--  dynamic  so that Julia can be used on demand.  The first call  to be
--  done before using any other is  Load which  loads  the Julia dynamic
--  library.
--
--  After the library is loaded,  the Julia context  must be initialized
--  by calling to Init or to Init_With_Image.
--
with Ada.Calendar;          use Ada.Calendar;
with Ada.Exceptions;        use Ada.Exceptions;
with Interfaces;            use Interfaces;
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with System;                use System;

with Ada.Finalization;
with Generic_Unbounded_Array;
with Object.Handle;
with Tables;
with System.Storage_Elements;

package Julia is

   Julia_Error : exception;
--
-- Load -- The Julia relocatable library
--
--    Name - The library path, if missing the name is guessed
--
-- Note  that  when  Name  is  absolute,  the process   environment   is
-- changed  in order to allow searching  for Julia modules.  E.g.  under
-- Windows, the library directory is placed in front of the PATH.
--
-- Exceptions :
--
--    Use_Error - Error loading the library
--
   procedure Load (Name : String := "");

   type function_t is private;
   type datatype_t is private;
   type module_t   is private;
   type value_t    is private;

   type values_array is
      array (Positive range <>) of aliased value_t;
   pragma Convention (C, values_array);
   type datatypes_array is
      array (Positive range <>) of aliased datatype_t;
   pragma Convention (C, datatypes_array);

   No_Value : constant value_t;
------------------------------------------------------------------------
-- Life cycle:
--
-- AtExit_Hook -- Last call to finalize Julia
--
--    Status - The error code
--
   procedure AtExit_Hook (Status : Int := 0);
--
-- Init[_With_Image] -- The first call to initialize the Julia
--                      environment
--
--    Julia_Bin_Dir       - The location of Julia relocatable library
--    Image_Relative_Path - Julia image library path relative to above
--
-- The variant Init allows  Julia to guess paths,  which is  unlikely to
-- work. The variant Init_With_Image is more stable.  When Julia_Bin_Dir
-- is empty, it is guessed. Which is too unlikely to work.  The best way
-- is to use the absolute path to the file specified in Load. The second
-- parameter when empty  is automatically set  to the  default value for
-- the corresponding OS.
--
   procedure Init;
   procedure Init_With_Image
             (  Julia_Bin_Dir       : String := "";
                Image_Relative_Path : String := ""
             );
------------------------------------------------------------------------
-- Modules:
--
   function Base_Module return module_t;
   function Core_Module return module_t;
   function Main_Module return module_t;
   function Top_Module  return module_t;
------------------------------------------------------------------------
-- Computation:
--
-- Call -- Direct call to a Juila's function
--
--    Func     - The function
--    Argument - The arguments
--    ...
--
-- Exceptions :
--
--    Julia_Error - Any exception for Juila reraised
--
   function Call
            (  Func      : function_t;
               Arguments : values_array
            )  return value_t;
   function Call
            (  Func : function_t
            )  return value_t;
   function Call
            (  Func     : function_t;
               Argument : value_t
            )  return value_t;
   function Call
            (  Func       : function_t;
               Argument_1 : value_t;
               Argument_2 : value_t
            )  return value_t;
   function Call
            (  Func       : function_t;
               Argument_1 : value_t;
               Argument_2 : value_t;
               Argument_3 : value_t
            )  return value_t;
--
-- CCall_Address -- Julia pointer to a C subprogram
--
--    Location - The addrss of the subprogram
--
-- The C interface in Julia  uses  a relocatable  library  and the entry
-- point name in there to call a C subprogram. E.g.
--
--    ccall((:getenv, "libc"), ...
--
-- We need nothing  of that to be able  to call a subprogram  defined in
-- the  same  process  that invokes  Julia.  This  function  returns  an
-- expression for the first argument  for ccall  generated directly from
-- the address of the C subprogram to be called. It can be used in ccall
-- or stored in a Julia variable for later use.
--
-- Returns :
--
--    Ptr{Nothing}(UInt{32|64}(<integer-address>))
--
   function CCall_Address (Location : Address) return String;
--
-- Eval_[String|char_array] -- Evaluate argument as a Julia expression
--
--    Str - The expression to evaluate
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    Julia_Error - Any exception for Juila reraised
--
   function Eval_String (Str : String) return value_t;
   function Eval_char_array (Str : char_array) return value_t;
   procedure Eval_String (Str : String);
   procedure Eval_char_array (Str : char_array);
--
-- Get_Function -- Get function from a module
--
--    Module - The module
--    Name   - The function name
--
-- Returns :
--
--    The function
--
   function Get_Function
            (  Module : module_t;
               Name   : String
            )  return function_t;
--
-- Load -- Load from a file
--
--    Module - The module
--    File   - The file name to load Julia code from
--
-- This function parses contents of the file.
--
-- Returns :
--
--    The result of execution
--
-- Exceptions :
--
--    Julia_Error - Any exception for Juila reraised
--
   function Load (Module : module_t; File : String) return value_t;
--
-- Load_File_String
--
--    Module - The module
--    Source - The source code to parse
--    File   - The file name to associate with Julia code
--
-- Returns :
--
--    The result of execution
--
-- Exceptions :
--
--    Julia_Error - Any exception for Juila reraised
--
   function Load_File_String
            (  Module : module_t;
               Source : String;
               File   : String
            )  return value_t;
------------------------------------------------------------------------
--
-- Julia tuples
--
-- Get_Field -- Get element by index, e.g. an element of a tuple
--
--    Container   - The container
--    Index / Key - The index or key
--
-- Returns :
--
--    The element
--
   function Get_Field
            (  Container : value_t;
               Index     : Positive
            )  return value_t;
   function Get_Field
            (  Container : value_t;
               Key       : String
            )  return value_t;
--
-- Get_Name -- Get field name by index
--
--    Container - The container object or its type
--    Index     - The index
--
-- Returns :
--
--    The name
--
   function Get_Name
            (  Container : datatype_t;
               Index     : Positive
            )  return String;
   function Get_Name
            (  Container : value_t;
               Index     : Positive
            )  return String;
--
-- Is_Defined -- Check if a field is defined
--
--    Container - The container
--    Index     - The index
--
-- Returns :
--
--    True if defined
--
   function Is_Defined
            (  Container : value_t;
               Index     : Positive
            )  return Boolean;
--
-- N_Fields -- Get number of fields
--
--    Container - The container
--
-- Returns :
--
--    The number of fields
--
   function N_Fields (Container : value_t) return Natural;
--
-- Set_Field -- Set element by index, e.g. an element of a tuple
--
--    Container - The container
--    Index     - The index
--    Element   - The element
--
   procedure Set_Field
             (  Container : value_t;
                Index     : Positive;
                Element   : value_t
             );
------------------------------------------------------------------------
-- Julia's data types:
--
   function Any_Type          return datatype_t;
   function Anytuple_Type     return datatype_t;
   function Array_Type        return datatype_t;
   function Bool_Type         return datatype_t;
   function Char_Type         return datatype_t;
   function Datatype_Type     return datatype_t;
   function Float32_Type      return datatype_t;
   function Float64_Type      return datatype_t;
   function Int8_Type         return datatype_t;
   function Int16_Type        return datatype_t;
   function Int32_Type        return datatype_t;
   function Int64_Type        return datatype_t;
   function Method_Type       return datatype_t;
   function Module_Type       return datatype_t;
   function Namedtuple_Type   return datatype_t;
   function Simplevector_Type return datatype_t;
   function String_Type       return datatype_t;
   function UInt8_Type        return datatype_t;
   function UInt16_Type       return datatype_t;
   function UInt32_Type       return datatype_t;
   function UInt64_Type       return datatype_t;
   function Uniontype_Type    return datatype_t;

   function Equal (Left, Right : datatype_t) return Boolean;
   function Is_A (Left, Right : datatype_t) return Boolean;
   function Is_Mutable (Type_Object : datatype_t) return Boolean;
   function More_Specific (Left, Right : datatype_t) return Boolean;
   function TypeName (Type_Object : datatype_t) return String;
   function TypeName (Value : value_t) return String;
   function TypeOf (Value : value_t) return datatype_t;
   function TypeOf_Str (Value : value_t) return String;
------------------------------------------------------------------------
--
-- Tuple -- Ada type corresponding to Julia's named tuple
--
   type Tuple is tagged private;
--
-- Add -- A new element
--
--    List  - The tuple
--    Name  - The element name
--    Value - The value
--
-- Exceptions :
--
--    Constraint_Error - Value is invalid or duplicated name
--
   procedure Add
             (  List  : in out Tuple;
                Name  : String;
                Value : value_t
             );
--
-- Get_Name -- Tuple length
--
--    List  - The tuple
--    Index - Element position
--
-- Returns :
--
--    The element name
--
-- Exceptions :
--
--    Constraint_Error - Invalid index
--
   function Get_Name (List : Tuple; Index : Positive) return String;
--
-- Get_Value -- Tuple length
--
--    List         - The tuple
--    Name | Index - Element position
--
-- Returns :
--
--    The element value
--
-- Exceptions :
--
--    Constraint_Error - Invalid index
--
   function Get_Value (List : Tuple; Index : Positive) return value_t;
   function Get_Value (List : Tuple; Name  : String  ) return value_t;
--
-- Is_NamedTuple -- Type test
--
--    Object - The tuple or type
--
-- Returns :
--
--    True if it is a named tuple
--
   function Is_NamedTuple (Object : datatype_t) return Boolean;
   function Is_NamedTuple (Object : value_t   ) return Boolean;
--
-- Is_Tuple -- Type test
--
--    Object - The tuple or type
--
-- Returns :
--
--    True if it is a tuple
--
   function Is_Tuple (Object : datatype_t) return Boolean;
   function Is_Tuple (Object : value_t   ) return Boolean;
--
-- Length -- Tuple length
--
--    List  - The tuple
--
-- Returns :
--
--    Tuple length
--
   function Length (List : Tuple) return Natural;
------------------------------------------------------------------------
-- To_Julia -- Conversion to Julia
--
   function To_Julia (Value : Boolean     ) return value_t;
   function To_Julia (Value : char        ) return value_t;
   function To_Julia (Value : Character   ) return value_t;
   function To_Julia (Value : Day_Duration) return value_t;
   function To_Julia (Value : Double      ) return value_t;
   function To_Julia (Value : C_Float     ) return value_t;
   function To_Julia (Value : Integer_8   ) return value_t;
   function To_Julia (Value : Integer_16  ) return value_t;
   function To_Julia (Value : Integer_32  ) return value_t;
   function To_Julia (Value : Integer_64  ) return value_t;
   function To_Julia (Value : String      ) return value_t;
   function To_Julia (Value : Unsigned_8  ) return value_t;
   function To_Julia (Value : Unsigned_16 ) return value_t;
   function To_Julia (Value : Unsigned_32 ) return value_t;
   function To_Julia (Value : Unsigned_64 ) return value_t;
   function To_Julia (Value : values_array) return value_t; -- Tuple
   function To_Julia (Value : Time        ) return value_t;
   function To_Julia (Value : Tuple       ) return value_t; -- Tuple
------------------------------------------------------------------------
-- Value -- Conversion from Julia
--
   function Value (Object : value_t) return Boolean;
-- function Value (Object : value_t) return char;
-- function Value (Object : value_t) return Character;
   function Value (Object : value_t) return Day_Duration;
   function Value (Object : value_t) return Double;
   function Value (Object : value_t) return C_Float;
   function Value (Object : value_t) return Integer_8;
   function Value (Object : value_t) return Integer_16;
   function Value (Object : value_t) return Integer_32;
   function Value (Object : value_t) return Integer_64;
   function Value (Object : value_t) return String;
   function Value (Object : value_t) return Unsigned_8;
   function Value (Object : value_t) return Unsigned_16;
   function Value (Object : value_t) return Unsigned_32;
   function Value (Object : value_t) return Unsigned_64;
   function Value (Object : value_t) return values_array; -- Tuple
   function Value (Object : value_t) return Time;
   function Value (Object : value_t) return Tuple;        -- Tuple
------------------------------------------------------------------------
-- Arrays:
--
-- Dimension -- The array dimension
--
--    Object - The array
--
-- Returns :
--
--    The dimension
--
-- Exceptions :
--
--    Constraint_Error - Not an array
--
   function Dimension (Object : value_t) return Natural;
--
-- Element_Type -- The array element type
--
--    Object - The array
--
-- Returns :
--
--    The element's type
--
-- Exceptions :
--
--    Constraint_Error - Not an array
--
   function Element_Type (Object : value_t) return datatype_t;
--
-- Is_Array -- Type test
--
--    Object - The array or type
--
-- Returns :
--
--    True if it is an array
--
   function Is_Array (Object : datatype_t) return Boolean;
   function Is_Array (Object : value_t   ) return Boolean;
--
-- Length -- The array length
--
--    Object    - The array
--    Dimension - The dimension for which length is requested
--
-- Returns :
--
--    The array length in the dimension
--
-- Exceptions :
--
--    Constraint_Error - Not an array
--
   function Length (Object : value_t; Dimension : Positive)
      return Natural;
------------------------------------------------------------------------
-- Julia's exceptions:
--
-- Check_Error -- Reraise Ada exception
--
--    Error - The Ada exception to propagate
--
-- This procedure  raises Julia exception  if there  is an  active Julia
-- exception (see Exception_Occurred). The effect is void if there is no
-- Julia exception. Julia exception is cleared if Ada exception raised.
--
   procedure Check_Error (Error : Exception_ID := Julia_Error'Identity);
--
-- Exception_Occurred -- An exception check
--
-- Returns :
--
--    True or exception text
--
   function Exception_Occurred return Boolean;
   function Exception_Occurred return String;
--
-- Exception_Clear -- Clear any pending Julia exception
--
   procedure Exception_Clear;
--
-- Raising Julia's exceptions
--
   procedure Bounds_Error (Container, Index : value_t);
   procedure Error (Text : String);
   procedure Too_Few_Args (Name : String; Min : int);
   procedure Too_Many_Args (Name : String; Max : int);
   procedure Type_Error
             (  Name     : String;
                Expected : value_t;
                Got      : value_t
             );
--
-- Catching Julia exceptions
--
   function Get_Safe_Restore return Address;
-- function Setjmp (Buffer : Address; Value : int) return int;
   procedure Set_Safe_Restore (Buffer : Address);
------------------------------------------------------------------------
--
-- Garbage collection
--
-- GC_Collect -- Force collector
--
--    Full - True if full collection
--
   procedure GC_Collect (Full : Boolean := True);
--
-- GC_Diff_Total_Bytes -- Leftover bytes
--
-- Returns :
--
--    Leftover bytes
--
   function GC_Diff_Total_Bytes return Integer_64;
--
-- GC_Enable -- Change GC status
--
--    On - New status, True if enabled
--
-- Returns :
--
--    The old status
--
   function GC_Enable (On : Boolean) return Boolean;
--
-- GC_Is_Enabled -- GC status
--
-- Returns :
--
--    True if GC is enabled
--
   function GC_Is_Enabled return Boolean;
--
-- GC_Total_Bytes -- Total bytes
--
-- Returns :
--
--    Total bytes
--
   function GC_Total_Bytes return Integer_64;
--
-- GC_WB -- Write barrier
--
--    Parent - The container value
--    Child  - The contained value
--
   procedure GC_WB (Parent, Child : value_t);
--
-- Holder -- Hold reference to a value
--
   type Holder (Count : Natural) is tagged limited private;
--
-- Set -- Values to protect from collection
--
--    Object - The object
--    Slot   - The for the value
--    Value  - The value or datatype to protect
--
-- The procedure  pushes Value   onto the Julia's stack,  which protects
-- them from collection. Upon finalization the stack is popped.
--
-- Exceptions :
--
--    Constraint_Error - The number of values exceeds the discriminant
--    Use_Error        - The slot is already used
--
   procedure Set
             (  Object : in out Holder;
                Value  : datatype_t
             );
   procedure Set
             (  Object : in out Holder;
                Value  : value_t
             );
   procedure Set
             (  Object  : in out Holder;
                Value_1 : value_t;
                Value_2 : value_t
             );
   procedure Set
             (  Object  : in out Holder;
                Value_1 : value_t;
                Value_2 : value_t;
                Value_3 : value_t
             );
   procedure Set
             (  Object : in out Holder;
                Slot   : Positive;
                Value  : datatype_t
             );
   procedure Set
             (  Object : in out Holder;
                Slot   : Positive;
                Value  : value_t
             );
private
   type flat_value_t_array is array (size_t) of aliased value_t;
   pragma Convention (C, flat_value_t_array);

   type jl_svec_t is record
      length : size_t;
      data   : flat_value_t_array;
   end record;
   pragma Convention (C, jl_svec_t);

   type svec_t is access all jl_svec_t;
   pragma Convention (C, svec_t);

   type methtable_t is new Address;

   type jl_sym_t;
   type sym_t is access all jl_sym_t;
   pragma Convention (C, sym_t);

   type flat_char_array is array (size_t) of aliased char;
   pragma Convention (C, flat_char_array);
   type jl_sym_t is record
      left  : sym_t;
      right : sym_t;
      hash  : System.Storage_Elements.Integer_Address;
      name  : flat_char_array;
   end record;
   pragma Convention (C, jl_sym_t);
   function Get_Name (Symbol : jl_sym_t) return String;

   type jl_typename_t is record
      name        : sym_t;
      module      : module_t;
      names       : svec_t; -- field names
      wrapper     : value_t;
      cache       : svec_t;
      linearcache : svec_t;
      hash        : System.Storage_Elements.Integer_Address;
      mt          : methtable_t;
   end record;
   pragma Convention (C, jl_typename_t);

   type typename_t is access all jl_typename_t;
   pragma Convention (C, typename_t);

   type jl_datatype_layout_t is record
      nfields        : Unsigned_32;
      alignment      : Unsigned_32;
      haspadding     : Unsigned_32;
      npointers      : Unsigned_32;
      fielddesc_type : Unsigned_32;
   end record;
   pragma Convention (C, jl_datatype_layout_t);

   type datatype_layout_t is access all jl_datatype_layout_t;
   pragma Convention (C, datatype_layout_t);

   type jl_datatype_t is record
      name            : typename_t;
      super           : datatype_t;
      parameters      : svec_t;
      types           : svec_t;
      names           : svec_t;
      instance        : value_t; -- for singletons
      layout          : datatype_layout_t;
      size            : Integer_32;
      ninitialized    : Integer_32;
      uid             : Unsigned_32;
      is_abstract     : Unsigned_8;
      mutabl          : Unsigned_8;
      -- memorized properties
      hasfreetypevars : Unsigned_8;
      isconcretetype  : Unsigned_8;
      isdispatchtuple : Unsigned_8;
      isbitstype      : Unsigned_8;
      zeroinit        : Unsigned_8;
      isinlinealloc   : Unsigned_8;
      struct_decl     : Address;
      ditype          : Address;
   end record;
   pragma Convention (C, jl_datatype_t);

   type datatype_t is access all jl_datatype_t;
   pragma Convention (C, datatype_t);

   type jl_gcframe_t;
   type gcframe_t is access all jl_gcframe_t;
   pragma Convention (C, gcframe_t);

   type jl_gcframe_t is record
      nroots : size_t;
      prev   : gcframe_t;
      roots  : values_array (1..1);
   end record;
   pragma Convention (C, jl_gcframe_t);

   type jl_tls_states_t is record
      pgcstack             : gcframe_t;
      world_age            : size_t ;
      exception_in_transit : value_t;
   end record;
   pragma Convention (C, jl_tls_states_t);

   type tls_states_t is access all jl_tls_states_t;
   pragma Convention (C, tls_states_t);

   type Holder (Count : Natural) is
      new Ada.Finalization.Limited_Controlled with
   record
      Frame : aliased jl_gcframe_t := (0, null, (others => No_Value));
      Tail  : values_array (2..Count) := (others => No_Value);
   end record;
   procedure Finalize (Object : in out Holder);

   type function_t is new Address;
   type module_t   is new Address;
   type value_t    is new Address;
   No_Value : constant value_t := value_t (Null_Address);

   package Tuple_Tables is new Tables (Integer);
   type value_t_Array is array (Positive range <>) of value_t;
   package Tuple_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Positive,
             Object_Type       => value_t,
             Object_Array_Type => value_t_Array,
             Null_Element      => No_Value
          );
   type Tuple_Object is new Object.Entity with record
      Table : Tuple_Tables.Table;
      List  : Tuple_Arrays.Unbounded_Array;
   end record;
   type Tuple_Ptr is access Tuple_Object'Class;
   package Tuple_Handles is new Object.Handle (Tuple_Object, Tuple_Ptr);

   type Tuple is new Tuple_Handles.Handle with null record;

   function Array_Typename      return typename_t;
   function Tuple_Typename      return typename_t;
   function NamedTuple_Typename return typename_t;

   type AtExit_Hook_Ptr is access procedure (Status : int);
   pragma Convention (C, AtExit_Hook_Ptr);

   type Bounds_Error_Ptr is access procedure
        (  Container : value_t;
           Index     : value_t
        );
   pragma Convention (C, Bounds_Error_Ptr);

   type Alloc_Array_1D_Ptr is access function
        (  Element   : datatype_t;
           Dimension : size_t
        )  return value_t;
   pragma Convention (C, Alloc_Array_1D_Ptr);

   type Alloc_Array_2D_Ptr is access function
        (  Element     : datatype_t;
           Dimension_1 : size_t;
           Dimension_2 : size_t
        )  return value_t;
   pragma Convention (C, Alloc_Array_2D_Ptr);

   type Alloc_Array_3D_Ptr is access function
        (  Element     : datatype_t;
           Dimension_1 : size_t;
           Dimension_2 : size_t;
           Dimension_3 : size_t
        )  return value_t;
   pragma Convention (C, Alloc_Array_3D_Ptr);

   type Alloc_SVec_Uninit_Ptr is access
      function (Size : size_t) return svec_t;
   pragma Convention (C, Alloc_SVec_Uninit_Ptr);

   type Apply_Array_Type_Ptr is access function
        (  Element   : datatype_t;
           Dimension : size_t
        )  return datatype_t;
   pragma Convention (C, Apply_Array_Type_Ptr);

   type Apply_Tuple_Type_V_Ptr is access function
        (  Element : access datatype_t;
           Length  : size_t
        )  return datatype_t;
   pragma Convention (C, Apply_Tuple_Type_V_Ptr);

   type Array_Eltype_Ptr is access function
        (  Container : value_t
        )  return datatype_t;
   pragma Convention (C, Array_Eltype_Ptr);

   type Array_Rank_Ptr is access function
        (  Container : value_t
        )  return size_t;
   pragma Convention (C, Array_Rank_Ptr);

   type Array_Size_Ptr is access function
        (  Container : value_t;
           Dimension : size_t
        )  return size_t;
   pragma Convention (C, Array_Size_Ptr);

   type ArrayLen_Ptr is access function
        (  Container : value_t
        )  return value_t;
   pragma Convention (C, ArrayLen_Ptr);

   type Box_Bool_Ptr is access function (Value : Integer_8)
      return value_t;
   pragma Convention (C, Box_Bool_Ptr);

   type Box_Char_Ptr is access function (Value : char)
      return value_t;
   pragma Convention (C, Box_Char_Ptr);

   type Box_Double_Ptr is access function (Value : Double)
      return value_t;
   pragma Convention (C, Box_Double_Ptr);

   type Box_Float_Ptr is access function (Value : C_Float)
      return value_t;
   pragma Convention (C, Box_Float_Ptr);

   type Box_Int8_Ptr is access function (Value : Integer_8)
      return value_t;
   pragma Convention (C, Box_Int8_Ptr);

   type Box_Int16_Ptr is access function (Value : Integer_16)
      return value_t;
   pragma Convention (C, Box_Int16_Ptr);

   type Box_Int32_Ptr is access function (Value : Integer_32)
      return value_t;
   pragma Convention (C, Box_Int32_Ptr);

   type Box_Int64_Ptr is access function (Value : Integer_64)
      return value_t;
   pragma Convention (C, Box_Int64_Ptr);

   type Box_UInt8_Ptr is access function (Value : Unsigned_8)
      return value_t;
   pragma Convention (C, Box_UInt8_Ptr);

   type Box_UInt16_Ptr is access function (Value : Unsigned_16)
      return value_t;
   pragma Convention (C, Box_UInt16_Ptr);

   type Box_UInt32_Ptr is access function (Value : Unsigned_32)
      return value_t;
   pragma Convention (C, Box_UInt32_Ptr);

   type Box_UInt64_Ptr is access function (Value : Unsigned_64)
      return value_t;
   pragma Convention (C, Box_UInt64_Ptr);

   type Call_Ptr is access function
        (  Func      : function_t;
           Arguments : Address;
           Count     : Integer_32
        )  return value_t;
   pragma Convention (C, Call_Ptr);

   type Call0_Ptr is access function
        (  Module : function_t
        )  return value_t;
   pragma Convention (C, Call0_Ptr);

   type Call1_Ptr is access function
        (  Func     : function_t;
           Argument : value_t
        )  return value_t;
   pragma Convention (C, Call1_Ptr);

   type Call2_Ptr is access function
        (  Func       : function_t;
           Argument_1 : value_t;
           Argument_2 : value_t
        )  return value_t;
   pragma Convention (C, Call2_Ptr);

   type Call3_Ptr is access function
        (  Func       : function_t;
           Argument_1 : value_t;
           Argument_2 : value_t;
           Argument_3 : value_t
        )  return value_t;
   pragma Convention (C, Call3_Ptr);

   type Eval_String_Ptr is access function
        (  Value : char_array
        )  return value_t;
   pragma Convention (C, Eval_String_Ptr);

   type Error_Ptr is access procedure (Text : char_array);
   pragma Convention (C, Error_Ptr);

   type Exception_Occurred_Ptr is access function return value_t;
   pragma Convention (C, Exception_Occurred_Ptr);

   type Exception_Clear_Ptr is access procedure;
   pragma Convention (C, Exception_Clear_Ptr);

   type Field_Isdefined_Ptr is access function
        (  Value : value_t;
           Index : size_t
        )  return int;
   pragma Convention (C, Field_Isdefined_Ptr);

   type GC_Collect_Ptr is access procedure (On : int);
   pragma Convention (C, GC_Collect_Ptr);

   type GC_Diff_Total_Bytes_Ptr is access function return Integer_64;
   pragma Convention (C, GC_Diff_Total_Bytes_Ptr);

   type GC_Enable_Ptr is access function (On : int) return int;
   pragma Convention (C, GC_Enable_Ptr);

   type GC_Is_Enabled_Ptr is access function return int;
   pragma Convention (C, GC_Is_Enabled_Ptr);

   type GC_Total_Bytes_Ptr is access function return Integer_64;
   pragma Convention (C, GC_Total_Bytes_Ptr);

   type GC_Get_Total_Bytes_Ptr is access
      procedure (Bytes : access Integer_64);
   pragma Convention (C, GC_Get_Total_Bytes_Ptr);

   type GC_Queue_Root_Ptr is access procedure (Parent : value_t);
   pragma Convention (C, GC_Queue_Root_Ptr);

   type Get_Field_Ptr is access function
        (  Container : value_t;
           Name      : char_array
        )  return value_t;
   pragma Convention (C, Get_Field_Ptr);

   type Get_Global_Ptr is access function
        (  Module : module_t;
           Symbol : Address
        )  return Address;
   pragma Convention (C, Get_Global_Ptr);

   type Get_Nth_Field_Ptr is access function
        (  Container : value_t;
           Index     : size_t
        )  return value_t;
   pragma Convention (C, Get_Nth_Field_Ptr);

   type Get_Nth_Field_Checked_Ptr is access function
        (  Container : value_t;
           Index     : size_t
        )  return value_t;
   pragma Convention (C, Get_Nth_Field_Checked_Ptr);

   type Get_PTLS_States_Ptr is access function return tls_states_t;
   pragma Convention (C, Get_PTLS_States_Ptr);

   type Get_Safe_Restore_Ptr is access function return Address;

   type Init_Ptr is access procedure;
   pragma Convention (C, Init_Ptr);

   type Init_With_Image_Ptr is access procedure
        (  Julia_Bin_Dir       : chars_ptr := Null_Ptr;
           Image_Relative_Path : char_array
        );
   pragma Convention (C, Init_With_Image_Ptr);

   type Isa_Ptr is access function
        (  Left, Right : datatype_t
        )  return int;
   pragma Convention (C, Isa_Ptr);

   type Load_Ptr is access function
        (  Module : module_t;
           File   : char_array
        )  return value_t;
   pragma Convention (C, Load_Ptr);

   type Load_File_String_Ptr is access function
        (  Text      : char_array;
           Length    : size_t;
           File_Name : char_array;
           Module    : module_t
        )  return value_t;
   pragma Convention (C, Load_File_String_Ptr);

   type New_StructV_Ptr is access function
        (  Type_Of : datatype_t;
           Values  : access constant value_t;
           Length  : Unsigned_32
        )  return value_t;
   pragma Convention (C, New_StructV_Ptr);

   type New_Datatype_Ptr is access function
        (  Name         : sym_t;
           Module       : module_t;
           Super        : datatype_t;
           Parameters   : svec_t;
           Names        : svec_t;
           Types        : svec_t;
           Is_Abstract  : int;
           Is_Mutable   : int;
           ninitialized : int
        )  return datatype_t;
   pragma Convention (C, New_Datatype_Ptr);

   type New_Struct_Uninit_Ptr is access function
        (  Type_Of : datatype_t
        )  return value_t;
   pragma Convention (C, New_Struct_Uninit_Ptr);

   type Pchar_To_String_Ptr is access function
        (  Value  : Address;
           Length : size_t
        )  return value_t;
   pragma Convention (C, Pchar_To_String_Ptr);

   type Set_Nth_Field_Ptr is access procedure
        (  Container : value_t;
           Index     : size_t;
           Element   : value_t
        );
   pragma Convention (C, Set_Nth_Field_Ptr);

   type Set_Safe_Restore_Ptr is access procedure (Bufffer : Address);

-- type Setjmp_Ptr is access function
--      (  Buffer : Address;
--         Value  : int
--      )  return int;

   type Symbol_Ptr is access function
        (  Name : char_array
        )  return Address;
   pragma Convention (C, Symbol_Ptr);

   type String_Ptr_Ptr is access
      function (Value : value_t) return chars_ptr;
   pragma Convention (C, String_Ptr_Ptr);

   type Static_Show_Ptr is access
      function (Output : value_t; Object : value_t) return size_t;
   pragma Convention (C, Static_Show_Ptr);

   type SVec_Fill_Ptr is access function
        (  Length   : size_t;
           Elements : Address
        )  return svec_t;
   pragma Convention (C, SVec_Fill_Ptr);

   type Too_Few_Args_Ptr is access procedure
        (  Name : char_array;
           Min  : int
        );
   pragma Convention (C, Too_Few_Args_Ptr);

   type Too_Many_Args_Ptr is access procedure
        (  Name : char_array;
           Max  : int
        );
   pragma Convention (C, Too_Many_Args_Ptr);

   type Tupletype_Fill_Ptr is access function
        (  Length   : size_t;
           Elements : Address
        )  return value_t;
   pragma Convention (C, Tupletype_Fill_Ptr);

   type Type_Error_Ptr is access procedure
        (  Name     : char_array;
           Expected : value_t;
           Got      : value_t
        );
   pragma Convention (C, Type_Error_Ptr);

   type TypeName_Str_Ptr is access function (Value : datatype_t)
      return chars_ptr;
   pragma Convention (C, TypeName_Str_Ptr);

   type TypeOf_Ptr is access function (Value : value_t)
      return datatype_t;
   pragma Convention (C, TypeOf_Ptr);

   type TypeOf_Str_Ptr is access
      function (Value : value_t) return chars_ptr;
   pragma Convention (C, TypeOf_Str_Ptr);

   type Types_Equal_Ptr is access function
        (  Left, Right : datatype_t
        )  return int;
   pragma Convention (C, Types_Equal_Ptr);

   type Type_Morespecific_Ptr is access function
        (  Left, Right : datatype_t
        )  return int;
   pragma Convention (C, Type_Morespecific_Ptr);

   type Unbox_Bool_Ptr is access function (Value : value_t)
      return Integer_8;
   pragma Convention (C, Unbox_Bool_Ptr);

   type Unbox_Char_Ptr is access function (Value : value_t)
      return char;
   pragma Convention (C, Unbox_Char_Ptr);

   type Unbox_Double_Ptr is access function (Value : value_t)
      return Double;
   pragma Convention (C, Unbox_Double_Ptr);

   type Unbox_Float_Ptr is access function (Value : value_t)
      return C_Float;
   pragma Convention (C, Unbox_Float_Ptr);

   type Unbox_Int8_Ptr is access function (Value : value_t)
      return Integer_8;
   pragma Convention (C, Unbox_Int8_Ptr);

   type Unbox_Int16_Ptr is access function (Value : value_t)
      return Integer_16;
   pragma Convention (C, Unbox_Int16_Ptr);

   type Unbox_Int32_Ptr is access function (Value : value_t)
      return Integer_32;
   pragma Convention (C, Unbox_Int32_Ptr);

   type Unbox_Int64_Ptr is access function (Value : value_t)
      return Integer_64;
   pragma Convention (C, Unbox_Int64_Ptr);

   type Unbox_UInt8_Ptr is access function (Value : value_t)
      return Unsigned_8;
   pragma Convention (C, Unbox_UInt8_Ptr);

   type Unbox_UInt16_Ptr is access function (Value : value_t)
      return Unsigned_16;
   pragma Convention (C, Unbox_UInt16_Ptr);

   type Unbox_UInt32_Ptr is access function (Value : value_t)
      return Unsigned_32;
   pragma Convention (C, Unbox_UInt32_Ptr);

   type Unbox_UInt64_Ptr is access function (Value : value_t)
      return Unsigned_64;
   pragma Convention (C, Unbox_UInt64_Ptr);

   package Links is
      Alloc_Array_1D        : Alloc_Array_1D_Ptr;
      Alloc_Array_2D        : Alloc_Array_2D_Ptr;
      Alloc_Array_3D        : Alloc_Array_3D_Ptr;
      Alloc_SVec_Uninit     : Alloc_SVec_Uninit_Ptr;
      Any_Type              : Address;
      Anytuple_Type         : Address;
      Apply_Array_Type      : Apply_Array_Type_Ptr;
      Apply_Tuple_Type_V    : Apply_Tuple_Type_V_Ptr;
      Array_Eltype          : Array_Eltype_Ptr;
      Array_Rank            : Array_Rank_Ptr;
      Array_Size            : Array_Size_Ptr;
      ArrayLen              : ArrayLen_Ptr;
      Array_Type            : Address;
      Array_Typename        : Address;
      AtExit_Hook           : AtExit_Hook_Ptr;
      Base_Module           : Address;
      Bool_Type             : Address;
      Bounds_Error          : Bounds_Error_Ptr;
      Box_Bool              : Box_Bool_Ptr;
      Box_Char              : Box_Char_Ptr;
      Box_Double            : Box_Double_Ptr;
      Box_Float             : Box_Float_Ptr;
      Box_Int8              : Box_Int8_Ptr;
      Box_Int16             : Box_Int16_Ptr;
      Box_Int32             : Box_Int32_Ptr;
      Box_Int64             : Box_Int64_Ptr;
      Box_UInt8             : Box_UInt8_Ptr;
      Box_UInt16            : Box_UInt16_Ptr;
      Box_UInt32            : Box_UInt32_Ptr;
      Box_UInt64            : Box_UInt64_Ptr;
      Call                  : Call_Ptr;
      Call0                 : Call0_Ptr;
      Call1                 : Call1_Ptr;
      Call2                 : Call2_Ptr;
      Call3                 : Call3_Ptr;
      Char_Type             : Address;
      Core_Module           : Address;
      Datatype_Type         : Address;
      Init                  : Init_Ptr;
      Init_With_Image       : Init_With_Image_Ptr;
      IsA                   : IsA_Ptr;
      Error                 : Error_Ptr;
      Eval_String           : Eval_String_Ptr;
      Exception_Clear       : Exception_Clear_Ptr;
      Exception_Occurred    : Exception_Occurred_Ptr;
      Field_Isdefined       : Field_Isdefined_Ptr;
      Float32_Type          : Address;
      Float64_Type          : Address;
      GC_Collect            : GC_Collect_Ptr;
      GC_Diff_Total_Bytes   : GC_Diff_Total_Bytes_Ptr;
      GC_Enable             : GC_Enable_Ptr;
      GC_Is_Enabled         : GC_Is_Enabled_Ptr;
      GC_Queue_Root         : GC_Queue_Root_Ptr;
      GC_Total_Bytes        : GC_Total_Bytes_Ptr;
      GC_Get_Total_Bytes    : GC_Get_Total_Bytes_Ptr;
      Get_Field             : Get_Field_Ptr;
      Get_Global            : Get_Global_Ptr;
      Get_Nth_Field_Checked : Get_Nth_Field_Checked_Ptr;
      Get_Nth_Field         : Get_Nth_Field_Ptr;
      Get_PTLS_States       : Get_PTLS_States_Ptr;
      Get_Safe_Restore      : Get_Safe_Restore_Ptr;
      Load                  : Load_Ptr;
      Load_File_String      : Load_File_String_Ptr;
      Main_Module           : Address;
      Method_Type           : Address;
      Module_Type           : Address;
      Namedtuple_Type       : Address;
      Namedtuple_Typename   : Address;
      New_Datatype          : New_Datatype_Ptr;
      New_StructV           : New_StructV_Ptr;
      New_Struct_Uninit     : New_Struct_Uninit_Ptr;
      Int8_Type             : Address;
      Int16_Type            : Address;
      Int32_Type            : Address;
      Int64_Type            : Address;
      Pchar_To_String       : Pchar_To_String_Ptr;
      Set_Nth_Field         : Set_Nth_Field_Ptr;
      Set_Safe_Restore      : Set_Safe_Restore_Ptr;
--    Setjmp                : Setjmp_Ptr;
      Simplevector_Type     : Address;
      String_Ptr            : String_Ptr_Ptr;
      Static_Show           : Static_Show_Ptr;
      String_Type           : Address;
      SVec_Fill             : SVec_Fill_Ptr;
      Symbol                : Symbol_Ptr;
      Too_Few_Args          : Too_Few_Args_Ptr;
      Too_Many_Args         : Too_Many_Args_Ptr;
      Top_Module            : Address;
      Tuple_Typename        : Address;
      Tupletype_Fill        : Tupletype_Fill_Ptr;
      Type_Error            : Type_Error_Ptr;
      TypeName_Str          : TypeName_Str_Ptr;
      TypeOf                : TypeOf_Ptr;
      TypeOf_Str            : TypeOf_Str_Ptr;
      Types_Equal           : Types_Equal_Ptr;
      Type_Morespecific     : Type_Morespecific_Ptr;
      UInt8_Type            : Address;
      UInt16_Type           : Address;
      UInt32_Type           : Address;
      UInt64_Type           : Address;
      Unbox_Bool            : Unbox_Bool_Ptr;
--    Unbox_Char            : Unbox_Char_Ptr;
      Unbox_Double          : Unbox_Double_Ptr;
      Unbox_Float           : Unbox_Float_Ptr;
      Unbox_Int8            : Unbox_Int8_Ptr;
      Unbox_Int16           : Unbox_Int16_Ptr;
      Unbox_Int32           : Unbox_Int32_Ptr;
      Unbox_Int64           : Unbox_Int64_Ptr;
      Unbox_UInt8           : Unbox_UInt8_Ptr;
      Unbox_UInt16          : Unbox_UInt16_Ptr;
      Unbox_UInt32          : Unbox_UInt32_Ptr;
      Unbox_UInt64          : Unbox_UInt64_Ptr;
      Uniontype_Type        : Address;
   end Links;
   procedure Check_Links (Library_Name : String);

end Julia;
