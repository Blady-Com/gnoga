--                                                                    --
--  package Julia                   Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Unchecked_Conversion;
with Julia.Load_Julia_Library;
with Strings_Edit.ISO_8601;
with Strings_Edit.UTF8.Categorization;
with System.Address_To_Access_Conversions;

package body Julia is

   package Integer_Address_Conversions is
      new System.Address_To_Access_Conversions
          (  System.Storage_Elements.Integer_Address
          );

   package Value_Array_Conversions is
      new System.Address_To_Access_Conversions (flat_value_t_array);

   function "+" (Value : datatype_t) return value_t is
   begin
      return value_t (Value.all'Address);
   end "+";

   function "+" (Value : value_t) return datatype_t is
      package Conversions is
         new System.Address_To_Access_Conversions (jl_datatype_t);
      use Conversions;
   begin
      return To_Pointer (Address (Value)).all'Unchecked_Access;
   end "+";

   function "+" (Location : Address) return datatype_t is
      package Conversions is
         new System.Address_To_Access_Conversions (datatype_t);
      use Conversions;
   begin
      return To_Pointer (Location).all;
   end "+";

   function "+" (Location : Address) return module_t is
      package Conversions is
         new System.Address_To_Access_Conversions (module_t);
      use Conversions;
   begin
      return To_Pointer (Location).all;
   end "+";

   function "+" (Location : Address) return typename_t is
      package Conversions is
         new System.Address_To_Access_Conversions (typename_t);
      use Conversions;
   begin
      return To_Pointer (Location).all;
   end "+";

   procedure Check_Error
             (  Error : Exception_ID := Julia_Error'Identity
             )  is
--        function "+" (Value : chars_ptr) return String is
--        begin
--           if Value = Null_Ptr then
--              return "";
--           else
--              return Interfaces.C.Strings.Value (Value);
--           end if;
--        end "+";
      Result : constant value_t := Links.Exception_Occurred.all;
   begin
      if Result /= No_Value then
         declare
            Sprint    : constant function_t :=
                        function_t
                        (  Links.Get_Global
                           (  Base_Module,
                              Links.Symbol ("sprint" & Nul)
                        )  );
            Showerror : constant value_t :=
                        value_t
                        (  Links.Get_Global
                           (  Base_Module,
                              Links.Symbol ("showerror" & Nul)
                        )  );
            Message   : constant value_t :=
                        Links.Call2 (Sprint, Showerror, Result);
         begin
            if Message = No_Value then
               Links.Exception_Clear.all;
               Raise_Exception (Error);
            elsif TypeOf (Message) /= String_Type then
               Links.Exception_Clear.all;
               Raise_Exception (Error, TypeOf_Str (Message));
            else
               declare
                  Message_Text : constant String := Value (Message);
               begin
                  Links.Exception_Clear.all;
                  Raise_Exception (Error, Message_Text);
               end;
            end if;
         end;
      end if;
   end Check_Error;

   procedure Check_Function (Value : function_t) is
   begin
      if Address (Value) = Null_Address then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No function"
         );
      end if;
   end Check_Function;

   procedure Check_Index (Value : value_t; Index : Positive) is
   begin
      if Value = No_Value then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No value"
         );
      elsif N_Fields (Value) < Index then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No such field"
         );
      end if;
   end Check_Index;

   procedure Check_Type (Value : datatype_t) is
   begin
      if Value = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No type"
         );
      end if;
   end Check_Type;

   procedure Check_Value (Value : value_t) is
   begin
      if Value = No_Value then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No value"
         );
      end if;
   end Check_Value;

   procedure Check_Value (Value : value_t; Type_Of : Address) is
   begin
      if Value = No_Value then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No value"
         );
      elsif Links.TypeOf (Value) /= +Type_Of then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Type error"
         );
      end if;
   end Check_Value;

   procedure Add
             (  List  : in out Tuple;
                Name  : String;
                Value : value_t
             )  is
      use Strings_Edit.UTF8;
      use Strings_Edit.UTF8.Categorization;
      use Tuple_Arrays;
      use Tuple_Tables;
      Offset  : Integer;
      Code    : Code_Point;
   begin
      Check_Value (Value);
      if Name'Length = 0 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Empty name"
         );
      end if;
      declare
         Pointer : Integer := Name'First;
      begin
         Get (Name, Pointer, Code);
         if Code /= Character'Pos ('_') then
            case Category (Code) is
               when Lu | Ll | Lt | Lm | Lo | Nl =>
                  null;
               when others =>
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "First name character has illegal "
                     &  "character code "
                     &  Code_Point'Image (Code)
                     &  " of "
                     &  General_Category'Image (Category (Code))
                  )  );
            end case;
         end if;
         while Pointer <= Name'Last loop
            Get (Name, Pointer, Code);
            if Code /= Character'Pos ('!') then
               case Category (Code) is
                  when Nd | No | Mn | Mc | Me | Sk =>
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        (  "Name contains illegal character code"
                        &  Code_Point'Image (Code)
                        &  " of "
                        &  General_Category'Image (Category (Code))
                     )  );
                  when others =>
                     null;
               end case;
            end if;
         end loop;
      exception
         when Error : others =>
            Raise_Exception
            (  Constraint_Error'Identity,
               "Invalid name: " & Exception_Message (Error)
            );
      end;
      if Is_Valid (List) then
         Offset := Locate (Ptr (List).Table, Name);
         if Offset > 0 then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Element " & Name & " is already in the tuple"
            );
         end if;
      else
         Set (List, new Tuple_Object);
      end if;
      declare
         This  : Tuple_Object'Class renames Ptr (List).all;
         Index : constant Integer := GetSize (This.Table) + 1;
      begin
         Put (This.List, Index, Value);
         Add (This.Table, Name, Index);
      end;
   end Add;

   function Any_Type return datatype_t is
   begin
      return +Links.Any_Type;
   end Any_Type;

   function Anytuple_Type return datatype_t is
   begin
      return +Links.Anytuple_Type;
   end Anytuple_Type;

   function Array_Type return datatype_t is
   begin
      return +Links.Array_Type;
   end Array_Type;

   function Array_Typename return typename_t is
   begin
      return +Links.Array_Typename;
   end Array_Typename;

   procedure AtExit_Hook (Status : Int := 0) is
   begin
      Links.AtExit_Hook (Status);
   end AtExit_Hook;

   function Base_Module return module_t is
   begin
      return +Links.Base_Module;
   end Base_Module;

   function Bool_Type return datatype_t is
   begin
      return +Links.Bool_Type;
   end Bool_Type;

   procedure Bounds_Error (Container, Index : value_t) is
   begin
      Check_Value (Container);
      Check_Value (Index);
      Links.Bounds_Error (Container, Index);
   end Bounds_Error;

   function Call
            (  Func      : function_t;
               Arguments : values_array
            )  return value_t is
      Result : value_t;
   begin
      Check_Function (Func);
      if Arguments'Length > 0 then
         for Index in Arguments'Range loop
            Check_Value (Arguments (Index));
         end loop;
         Result :=
            Links.Call
            (  Func,
               Arguments (Arguments'First)'Address,
               Arguments'Length
            );
      else
         Result := Links.Call0 (Func);
      end if;
      Check_Error;
      return Result;
   end Call;

   function Call
            (  Func : function_t
            )  return value_t is
      Result : value_t;
   begin
      Check_Function (Func);
      Result := Links.Call0 (Func);
      Check_Error;
      return Result;
   end Call;

   function Call
            (  Func     : function_t;
               Argument : value_t
            )  return value_t is
      Result : value_t;
   begin
      Check_Function (Func);
      Check_Value (Argument);
      Result := Links.Call1 (Func, Argument);
      Check_Error;
      return Result;
   end Call;

   function Call
            (  Func       : function_t;
               Argument_1 : value_t;
               Argument_2 : value_t
            )  return value_t is
      Result : value_t;
   begin
      Check_Function (Func);
      Check_Value (Argument_1);
      Check_Value (Argument_2);
      Result := Links.Call2 (Func, Argument_1, Argument_2);
      Check_Error;
      return Result;
   end Call;

   function Call
            (  Func       : function_t;
               Argument_1 : value_t;
               Argument_2 : value_t;
               Argument_3 : value_t
            )  return value_t is
      Result : value_t;
   begin
      Check_Function (Func);
      Check_Value (Argument_1);
      Check_Value (Argument_2);
      Check_Value (Argument_3);
      Result := Links.Call3 (Func, Argument_1, Argument_2, Argument_3);
      Check_Error;
      return Result;
   end Call;

   function CCall_Address (Location : Address) return String is
      use System.Storage_Elements;
      Addr : constant String :=
                      Integer_Address'Image (To_Integer (Location));
   begin
      if Address'Size > 32 then
         return "Ptr{Nothing}(UInt64(" & Addr & "))";
      else
         return "Ptr{Nothing}(UInt32(" & Addr & "))";
      end if;
   end CCall_Address;

   function Char_Type return datatype_t is
   begin
      return +Links.Char_Type;
   end Char_Type;

   function Core_Module return module_t is
   begin
      return +Links.Core_Module;
   end Core_Module;

   function Datatype_Type return datatype_t is
   begin
      return +Links.Datatype_Type;
   end Datatype_Type;

   function Dimension (Object : value_t) return Natural is
   begin
      if Is_Array (Object) then
         return Natural (Links.Array_Rank (Object));
      else
         Raise_Exception
         (  Constraint_Error'Identity,
            "Not an array"
         );
      end if;
   end Dimension;

   function Element_Type (Object : value_t) return datatype_t is
   begin
      if Is_Array (Object) then
         return Links.Array_Eltype (Object);
      else
         Raise_Exception
         (  Constraint_Error'Identity,
            "Not an array"
         );
      end if;
   end Element_Type;

   function Equal (Left, Right : datatype_t) return Boolean is
   begin
      Check_Type (Left);
      Check_Type (Right);
      return Links.Types_Equal (Left, Right) /= 0;
   end Equal;

   procedure Error (Text : String) is
   begin
      Links.Error (To_C (Text));
   end Error;

   function Eval_String (Str : String) return value_t is
      Result : constant value_t := Links.Eval_String (To_C (Str));
   begin
      Check_Error;
      return Result;
   end Eval_String;

   function Eval_char_array (Str : char_array) return value_t is
      Result : constant value_t := Links.Eval_String (Str);
   begin
      Check_Error;
      return Result;
   end Eval_char_array;

   procedure Eval_String (Str : String) is
      Result : constant value_t := Links.Eval_String (To_C (Str));
   begin
      Check_Error;
   end Eval_String;

   procedure Eval_char_array (Str : char_array) is
      Result : constant value_t := Links.Eval_String (Str);
   begin
      Check_Error;
   end Eval_char_array;

   procedure Exception_Clear is
   begin
      Links.Exception_Clear.all;
   end Exception_Clear;

   function Exception_Occurred return Boolean is
   begin
      return Links.Exception_Occurred.all /= No_Value;
   end Exception_Occurred;

   function Exception_Occurred return String is
      Result : constant value_t := Links.Exception_Occurred.all;
   begin
      if Result = No_Value then
         return "";
      else
         return Typeof_Str (Result);
      end if;
   end Exception_Occurred;

   procedure Finalize (Object : in out Holder) is
      States : jl_tls_states_t renames Links.Get_PTLS_States.all.all;
   begin
      if Object.Frame.nroots > 0 then
         Object.Frame.nroots := 0;
         States.pgcstack := Object.Frame.prev;
      end if;
   end Finalize;

   function Float32_Type return datatype_t is
   begin
      return +Links.Float32_Type;
   end Float32_Type;

   function Float64_Type return datatype_t is
   begin
      return +Links.Float64_Type;
   end Float64_Type;

   procedure GC_Collect (Full : Boolean := True) is
   begin
      if Full then
         Links.GC_Collect (1);
      else
         Links.GC_Collect (0);
      end if;
   end GC_Collect;

   function GC_Diff_Total_Bytes return Integer_64 is
   begin
      return Links.GC_Diff_Total_Bytes.all;
   end GC_Diff_Total_Bytes;

   function GC_Enable (On : Boolean) return Boolean is
      Old : int;
   begin
      if On then
         Old := Links.GC_Enable (1);
      else
         Old := Links.GC_Enable (0);
      end if;
      return Old /= 0;
   end GC_Enable;

   function GC_Is_Enabled return Boolean is
   begin
      return Links.GC_Is_Enabled.all /= 0;
   end GC_Is_Enabled;

   function GC_Total_Bytes return Integer_64 is
   begin
      if Links.GC_Get_Total_Bytes = null then
         return Links.GC_Total_Bytes.all;
      else
         declare
            Result : aliased Integer_64;
         begin
            Links.GC_Get_Total_Bytes (Result'Unchecked_Access);
            return Result;
         end;
      end if;
   end GC_Total_Bytes;

   procedure GC_WB (Parent, Child : value_t) is
      use Integer_Address_Conversions;
      use System.Storage_Elements;
   begin
      Check_Value (Parent);
      Check_Value (Child);
      if (  To_Pointer (Address (Parent)).all = 3
         and then
            0 = (To_Pointer (Address (Child)).all and 1)
         )  then
         Links.GC_Queue_Root (Parent);
      end if;
   end GC_WB;

   function Get_Field
            (  Container : value_t;
               Index     : Positive
            )  return value_t is
      Result : value_t;
   begin
      Check_Index (Container, Index);
      Result := Links.Get_Nth_Field (Container, size_t (Index - 1));
      Check_Error;
      return Result;
   end Get_Field;

   function Get_Field
            (  Container : value_t;
               Key       : String
            )  return value_t is
      Result : value_t;
   begin
      Check_Value (Container);
      Result := Links.Get_Field (Container, To_C (Key));
      Check_Error;
      if Result = No_Value then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No such key"
         );
      end if;
      return Result;
   end Get_Field;

   function Get_Function
            (  Module : module_t;
               Name   : String
            )  return function_t is
      Result : function_t;
      Symbol : Address;
   begin
      if Address (Module) = Null_Address then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No module"
         );
      end if;
      Symbol := Links.Symbol (To_C (Name));
      Result := function_t (Links.Get_Global (Module, Symbol));
      Check_Error;
      if Address (Result) = Null_Address then
         Raise_Exception
         (  End_Error'Identity,
            "No such function in the module"
         );
      end if;
      return Result;
   end Get_Function;

   function Get_Name (Symbol : jl_sym_t) return String is
      Length : Natural;
   begin
      for Index in Symbol.name'Range loop
         if Symbol.name (Index) = Nul then
            Length := Natural (Index);
            exit;
         end if;
      end loop;
      declare
         Result : String (1..Length);
      begin
         for Index in Symbol.name'Range loop
            exit when Symbol.name (Index) = Nul;
            Result (Integer (Index) + 1) :=
               To_Ada (Symbol.name (Index));
         end loop;
         return Result;
      end;
   end Get_Name;

   function Get_Name
            (  Container : datatype_t;
               Index     : Positive
            )  return String is
     Names : svec_t;
   begin
      Check_Type (Container);
      Names := Container.names;
      if Names = null then
         Names := Container.name.names;
      end if;
      if TypeOf (value_t (Names.all'Address)) /= Simplevector_Type then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Internal datatype representation error"
         );
      elsif Names.length < size_t (Index) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No such field"
         );
      end if;
      declare
         Item   : constant value_t := Names.data (size_t (Index - 1));
         Symbol : jl_sym_t;
         for Symbol'Address use Address (Item);
         pragma Import (Ada, Symbol);
      begin
         return Get_Name (Symbol);
      end;
   end Get_Name;

   function Get_Name (List : Tuple; Index : Positive) return String is
      use Tuple_Tables;
   begin
      begin
         if Is_Valid (List) then
            declare
               This : Tuple_Object'Class renames Ptr (List).all;
            begin
               for Item in 1..GetSize (This.Table) loop
                  if GetTag (This.Table, Item) = Index then
                     return GetName (This.Table, Item);
                  end if;
               end loop;
            end;
         end if;
      exception
         when others =>
            null;
      end;
      Raise_Exception
      (  Constraint_Error'Identity,
         "Tuple index is out of range"
      );
   end Get_Name;

   function Get_Name
            (  Container : value_t;
               Index     : Positive
            )  return String is
   begin
      Check_Index (Container, Index);
      return Get_Name (TypeOf (Container), Index);
   end Get_Name;

   function Get_Value (List : Tuple; Index : Positive) return value_t is
      use Tuple_Arrays;
      use Tuple_Tables;
   begin
      begin
         if Is_Valid (List) and then Index <= GetSize (Ptr (List).Table)
         then
            return Get (Ptr (List).List, Index);
         end if;
      exception
         when others =>
             null;
      end;
      Raise_Exception
      (  Constraint_Error'Identity,
         "Tuple index is out of range"
      );
   end Get_Value;

   function Get_Value (List : Tuple; Name : String) return value_t is
      use Tuple_Arrays;
      use Tuple_Tables;
   begin
      if Is_Valid (List) then
         declare
            This  : Tuple_Object'Class renames Ptr (List).all;
            Index : constant Integer := Locate (This.Table, Name);
         begin
           if Index > 0 then
              return Get (This.List, GetTag (This.Table, Index));
           end if;
         end;
      end if;
      Raise_Exception
      (  Constraint_Error'Identity,
         "Tuple item " & Name & " does not exist"
      );
   end Get_Value;

   function Get_Safe_Restore return Address is
   begin
      return Links.Get_Safe_Restore.all;
   end Get_Safe_Restore;

   procedure Init is
   begin
      Links.Init.all;
   end Init;

   procedure Init_With_Image (Image_Relative_Path : String) is
   begin
      Links.Init_With_Image
      (  Image_Relative_Path => To_C (Image_Relative_Path)
      );
   end Init_With_Image;

   procedure Init_With_Image
             (  Julia_Bin_Dir       : String := "";
                Image_Relative_Path : String := ""
             )  is
      Dir : aliased char_array := To_C (Julia_Bin_Dir);
      function Path return char_array is
      begin
         if Image_Relative_Path'Length = 0 then
            return To_C
                   (  Julia.Load_Julia_Library.Get_Default_Relative_Path
                   );
         else
            return To_C (Image_Relative_Path);
         end if;
      end Path;
   begin
      if Julia_Bin_Dir'Length = 0 then
         Links.Init_With_Image (Image_Relative_Path => Path);
      else
         Links.Init_With_Image
         (  To_Chars_Ptr (Dir'Unchecked_Access),
            Path
         );
      end if;
   end Init_With_Image;

   function Int8_Type return datatype_t is
   begin
      return +Links.Int8_Type;
   end Int8_Type;

   function Int16_Type return datatype_t is
   begin
      return +Links.Int16_Type;
   end Int16_Type;

   function Int32_Type return datatype_t is
   begin
      return +Links.Int32_Type;
   end Int32_Type;

   function Int64_Type return datatype_t is
   begin
      return +Links.Int64_Type;
   end Int64_Type;

   function Is_A (Left, Right : datatype_t) return Boolean is
   begin
      Check_Type (Left);
      Check_Type (Right);
      return Links.IsA (Left, Right) /= 0;
   end Is_A;

   function Is_Mutable (Type_Object : datatype_t) return Boolean is
   begin
      Check_Type (Type_Object);
      return Type_Object.mutabl /= 0;
   end Is_Mutable;

   function Is_Array (Object : datatype_t) return Boolean is
   begin
      return
      (  TypeOf (value_t (Object.all'Address)) = Datatype_Type
      and then
         Object.name = Array_Typename
      );
   end Is_Array;

   function Is_Array (Object : value_t) return Boolean is
   begin
      return Is_Array (TypeOf (Object));
   end Is_Array;

   function Is_Defined
            (  Container : value_t;
               Index     : Positive
            )  return Boolean is
   begin
      Check_Index (Container, Index);
      return Links.Field_Isdefined (Container, size_t (Index - 1)) /= 0;
   end Is_Defined;

   function Is_Tuple (Object : datatype_t) return Boolean is
   begin
      return
      (  TypeOf (value_t (Object.all'Address)) = Datatype_Type
      and then
         Object.name = Tuple_Typename
      );
   end Is_Tuple;

   function Is_Tuple (Object : value_t) return Boolean is
   begin
      return Is_Tuple (TypeOf (Object));
   end Is_Tuple;

   function Is_NamedTuple (Object : datatype_t) return Boolean is
   begin
      return
      (  TypeOf (value_t (Object.all'Address)) = Datatype_Type
      and then
         Object.name = NamedTuple_Typename
      );
   end Is_NamedTuple;

   function Is_NamedTuple (Object : value_t) return Boolean is
   begin
      return Is_NamedTuple (TypeOf (Object));
   end Is_NamedTuple;

   function Length (Object : value_t; Dimension : Positive)
      return Natural is
   begin
      if Is_Array (Object) then
         return Natural
                (  Links.Array_Size (Object, size_t (Dimension - 1))
                );
      else
         Raise_Exception
         (  Constraint_Error'Identity,
            "Not an array"
         );
      end if;
   end Length;

   function Length (List : Tuple) return Natural is
      use Tuple_Tables;
   begin
      if Is_Valid (List) then
         return GetSize (Ptr (List).Table);
      else
         return 0;
      end if;
   end Length;

   function Load (Module : module_t; File : String) return value_t is
      Result : value_t;
   begin
      if Address (Module) = Null_Address then
         Raise_Exception (Constraint_Error'Identity, "Invalid module");
      end if;
      Result := Links.Load (Module, To_C (File));
      Check_Error;
      return Result;
   end Load;

   function Load_File_String
            (  Module : module_t;
               Source : String;
               File   : String
            )  return value_t is
      Result : value_t;
   begin
      if Address (Module) = Null_Address then
         Raise_Exception (Constraint_Error'Identity, "Invalid module");
      end if;
      Result :=
         Links.Load_File_String
         (  To_C (Source, False),
            Source'Length,
            To_C (File),
            Module
         );
      Check_Error;
      return Result;
   end Load_File_String;

   function Main_Module return module_t is
   begin
      return +Links.Main_Module;
   end Main_Module;

   function Method_Type return datatype_t is
   begin
      return +Links.Method_Type;
   end Method_Type;

   function Module_Type return datatype_t is
   begin
      return +Links.Module_Type;
   end Module_Type;

   function More_Specific (Left, Right : datatype_t) return Boolean is
   begin
      Check_Type (Left);
      Check_Type (Right);
      return Links.Type_Morespecific (Left, Right) /= 0;
   end More_Specific;

   function N_Fields (Container : value_t) return Natural is
      Kind_Of : constant datatype_t := TypeOf (Container);
   begin
      return Natural (Kind_Of.layout.nfields);
   end N_Fields;

   function Namedtuple_Typename return typename_t is
   begin
      return +Links.Namedtuple_Typename;
   end Namedtuple_Typename;

   function Namedtuple_Type return datatype_t is
   begin
      return +Links.Namedtuple_Type;
   end Namedtuple_Type;

   procedure Set
             (  Object : in out Holder;
                Value  : datatype_t
             )  is
   begin
      Set (Object, value_t (Value.all'Address));
   end Set;

   procedure Set
             (  Object : in out Holder;
                Value  : value_t
             )  is
   begin
      if Object.Frame.nroots > 0 then
         raise Use_Error;
      end if;
      declare
         States : jl_tls_states_t renames Links.Get_PTLS_States.all.all;
      begin
         Object.Frame.nroots    := 1 * 2 + 1;
         Object.Frame.prev      := States.pgcstack;
         Object.Frame.roots (1) := Value;
         States.pgcstack        := Object.Frame'Unchecked_Access;
      end;
   end Set;

   procedure Set
             (  Object  : in out Holder;
                Value_1 : value_t;
                Value_2 : value_t
             )  is
      use Value_Array_Conversions;
   begin
      if Object.Frame.nroots > 0 then
         raise Use_Error;
      elsif Object.Count < 2 then
         raise Constraint_Error;
      end if;
      declare
         States : jl_tls_states_t renames Links.Get_PTLS_States.all.all;
         Vector : flat_value_t_array renames
                  To_Pointer (Object.Frame.roots (1)'Address).all;
      begin
         Object.Frame.nroots := 2 * 2 + 1;
         Object.Frame.prev   := States.pgcstack;
         Vector (0)          := Value_1;
         Vector (1)          := Value_2;
         States.pgcstack     := Object.Frame'Unchecked_Access;
      end;
   end Set;

   procedure Set
             (  Object  : in out Holder;
                Value_1 : value_t;
                Value_2 : value_t;
                Value_3 : value_t
             )  is
      use Value_Array_Conversions;
   begin
      if Object.Frame.nroots > 0 then
         raise Use_Error;
      elsif Object.Count < 2 then
         raise Constraint_Error;
      end if;
      declare
         States : jl_tls_states_t renames Links.Get_PTLS_States.all.all;
         Vector : flat_value_t_array renames
                  To_Pointer (Object.Frame.roots (1)'Address).all;
      begin
         Object.Frame.nroots := 3 * 2 + 1;
         Object.Frame.prev   := States.pgcstack;
         Vector (0)          := Value_1;
         Vector (1)          := Value_2;
         Vector (2)          := Value_3;
         States.pgcstack     := Object.Frame'Unchecked_Access;
      end;
   end Set;

   procedure Set
             (  Object : in out Holder;
                Slot   : Positive;
                Value  : datatype_t
             )  is
   begin
      Set (Object, Slot, value_t (Value.all'Address));
   end Set;

   procedure Set
             (  Object : in out Holder;
                Slot   : Positive;
                Value  : value_t
             )  is
      use Value_Array_Conversions;
   begin
      if Object.Count < Slot then
         raise Constraint_Error;
      end if;
      declare
         Item : value_t renames
                        To_Pointer
                        (  Object.Frame.roots (1)'Address
                        )  (size_t (Slot) - 1);
      begin
         if Object.Frame.nroots = 0 then -- not initialized
            declare
               States : jl_tls_states_t renames
                        Links.Get_PTLS_States.all.all;
            begin
               Object.Frame.nroots := size_t (Object.Count) * 2;
               Object.Frame.prev   := States.pgcstack;
               States.pgcstack     := Object.Frame'Unchecked_Access;
            end;
         end if;
         if Item = No_Value then
            Item := Value;
         else
            Raise_Exception
            (  Use_Error'Identity,
               "The slot " & Image (Slot) & " is already in use"
            );
         end if;
      end;
   end Set;

   procedure Set_Field
             (  Container : value_t;
                Index     : Positive;
                Element   : value_t
             )  is
   begin
      Check_Value (Element);
      Check_Index (Container, Index);
      Links.Set_Nth_Field
      (  Container,
         size_t (Index - 1),
         Element
      );
      Check_Error;
   end Set_Field;

-- function Setjmp (Buffer : Address; Value : int) return int is
-- begin
--    return Links.Setjmp (Buffer, Value);
-- end Setjmp;

   procedure Set_Safe_Restore (Buffer : Address) is
   begin
      Links.Set_Safe_Restore (Buffer);
   end Set_Safe_Restore;

   function Simplevector_Type return datatype_t is
   begin
      return +Links.Simplevector_Type;
   end Simplevector_Type;

   function String_Type return datatype_t is
   begin
      return +Links.String_Type;
   end String_Type;

   procedure Too_Few_Args (Name : String; Min : int) is
   begin
      Links.Too_Few_Args (To_C (Name), Min);
   end Too_Few_Args;

   procedure Too_Many_Args (Name : String; Max : int) is
   begin
      Links.Too_Many_Args (To_C (Name), Max);
   end Too_Many_Args;

   function Top_Module return module_t is
   begin
      return +Links.Top_Module;
   end Top_Module;

   function Tuple_Typename return typename_t is
   begin
      return +Links.Tuple_Typename;
   end Tuple_Typename;

   procedure Type_Error
             (  Name     : String;
                Expected : value_t;
                Got      : value_t
             )  is
   begin
      Check_Value (Expected);
      Check_Value (Got);
      Links.Type_Error (To_C (Name), Expected, Got);
   end Type_Error;

   function TypeName (Type_Object : datatype_t) return String is
      Name : chars_ptr;
   begin
      Check_Type (Type_Object);
      Name := Links.TypeName_Str (Type_Object);
      if Name = Null_Ptr then
         return "";
      else
         return Value (Name);
      end if;
   end TypeName;

   function TypeName (Value : value_t) return String renames TypeOf_Str;

   function TypeOf (Value : value_t) return datatype_t is
      Result : datatype_t;
   begin
      Check_Value (Value);
      Result := Links.TypeOf (Value);
      return Result;
   end TypeOf;

   function TypeOf_Str (Value : value_t) return String is
      Result : chars_ptr;
   begin
      Check_Value (Value);
      Result := Links.TypeOf_Str (Value);
      if Result = Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (Result);
      end if;
   end TypeOf_Str;

   function UInt8_Type return datatype_t is
   begin
      return +Links.UInt8_Type;
   end UInt8_Type;

   function UInt16_Type return datatype_t is
   begin
      return +Links.UInt16_Type;
   end UInt16_Type;

   function UInt32_Type return datatype_t is
   begin
      return +Links.UInt32_Type;
   end UInt32_Type;

   function UInt64_Type return datatype_t is
   begin
      return +Links.UInt64_Type;
   end UInt64_Type;

   function Uniontype_Type return datatype_t is
   begin
      return +Links.Uniontype_Type;
   end Uniontype_Type;

   function Value (Object : value_t) return Day_Duration is
      function Self return value_t;
      pragma Convention (C, Self);

      function Self return value_t is
      begin
         return Object;
      end Self;
   begin
      Check_Value (Object);
      declare
         Nanosecond : constant String :=
            Julia.Value
            (  Eval_String
               (  "import Dates;"
               &  "string(ccall("
               &  CCall_Address (Self'Address)
               &  ",Any,()).instant)"
            )  );
      begin
         for Index in Nanosecond'Range loop
            if Nanosecond (Index) = ' ' then
               declare
                  Count : constant Integer_64 :=
                             Integer_64'Value
                             (  Nanosecond (Nanosecond'First..Index - 1)
                             );
               begin
                  return Day_Duration
                         (  Long_Float (Count)
                         /  1_000_000_000_000.0
                         );
               end;
            end if;
         end loop;
      end;
      return 0.0;
   end Value;

   function Value (Object : value_t) return Boolean is
      Result : Integer_8;
   begin
      Check_Value (Object, Links.Bool_Type);
      Result := Links.Unbox_Bool (Object);
      Check_Error;
      return Result /= 0;
   end Value;

--     function Value (Object : value_t) return char is
--        Result : char;
--     begin
--        Check_Value (Object, Links.Char_Type);
--        Result := Links.Unbox_Char (Object);
--        Check_Error;
--        return Result;
--     end Value;
--
--     function Value (Object : value_t) return Character is
--        Result : char;
--     begin
--        Check_Value (Object, Links.Char_Type);
--        Result := Links.Unbox_Char (Object);
--        Check_Error;
--        return To_Ada (Result);
--     end Value;

   function Value (Object : value_t) return values_array is
   begin
      Check_Value (Object, Links.Anytuple_Type);
      declare
         Result : values_array (1..N_Fields (Object));
      begin
         for Index in Result'Range loop
            Result (Index) := Get_Field (Object, Index);
         end loop;
         return Result;
      end;
   end Value;

   function Value (Object : value_t) return Tuple is
   begin
      Check_Value (Object, Links.NamedTuple_Type);
      declare
         Result : Tuple;
      begin
         for Index in 1..N_Fields (Object) loop
            Add
            (  Result,
               Get_Name (Object, Index),
               Get_Field (Object, Index)
            );
         end loop;
         return Result;
      end;
   end Value;

   function Value (Object : value_t) return Double is
      Result : Double;
   begin
      Check_Value (Object, Links.Float64_Type);
      Result := Links.Unbox_Double (Object);
      Check_Error;
      return Result;
   end Value;

   function Value (Object : value_t) return C_Float is
      Result : C_Float;
   begin
      Check_Value (Object, Links.Float32_Type);
      Result := Links.Unbox_Float (Object);
      Check_Error;
      return Result;
   end Value;

   function Value (Object : value_t) return Integer_8 is
      Result : Integer_8;
   begin
      Check_Value (Object, Links.Int8_Type);
      Result := Links.Unbox_Int8 (Object);
      Check_Error;
      return Result;
   end Value;

   function Value (Object : value_t) return Integer_16 is
      Result : Integer_16;
   begin
      Check_Value (Object, Links.Int16_Type);
      Result := Links.Unbox_Int16 (Object);
      Check_Error;
      return Result;
   end Value;

   function Value (Object : value_t) return Integer_32 is
      Result : Integer_32;
   begin
      Check_Value (Object, Links.Int32_Type);
      Result := Links.Unbox_Int32 (Object);
      Check_Error;
      return Result;
   end Value;

   function Value (Object : value_t) return Integer_64 is
      Result : Integer_64;
   begin
      Check_Value (Object, Links.Int64_Type);
      Result := Links.Unbox_Int64 (Object);
      Check_Error;
      return Result;
   end Value;

   function Value (Object : value_t) return String is
      type flat_char_array is array (size_t) of aliased char;
      pragma Convention (C, flat_char_array);

      type jl_string_t is record
         length : size_t;
         data   : flat_char_array;
      end record;
      pragma Convention (C, jl_string_t);

      package Conversions is
         new System.Address_To_Access_Conversions (jl_string_t);
      use Conversions;
   begin
      Check_Value (Object, Links.String_Type);
      declare
         Data : jl_string_t renames To_Pointer (Address (Object)).all;
      begin
         if Data.length = 0 then
            return "";
         end if;
         declare
            Result : String (1..Natural (Data.length));
            Count  : size_t := 0;
         begin
            for Index in Result'Range loop
               Result (Index) := To_Ada (Data.data (Count));
               Count := Count + 1;
            end loop;
            return Result;
         end;
      end;
   end Value;

   function Value (Object : value_t) return Time is
      function Self return value_t;
      pragma Convention (C, Self);

      function Self return value_t is
      begin
         return Object;
      end Self;
   begin
      Check_Value (Object);
      declare
         Data : constant String :=
                   Julia.Value
                   (  Eval_String
                      (  "string(ccall("
                      &  CCall_Address (Self'Address)
                      &  ",Any,()))"
                   )  );
      begin
         return Strings_Edit.ISO_8601.Value (Data);
      end;
   end Value;

   function Value (Object : value_t) return Unsigned_8 is
      Result : Unsigned_8;
   begin
      Check_Value (Object, Links.UInt8_Type);
      Result := Links.Unbox_UInt8 (Object);
      Check_Error;
      return Result;
   end Value;

   function Value (Object : value_t) return Unsigned_16 is
      Result : Unsigned_16;
   begin
      Check_Value (Object, Links.UInt16_Type);
      Result := Links.Unbox_UInt16 (Object);
      Check_Error;
      return Result;
   end Value;

   function Value (Object : value_t) return Unsigned_32 is
      Result : Unsigned_32;
   begin
      Check_Value (Object, Links.UInt32_Type);
      Result := Links.Unbox_UInt32 (Object);
      Check_Error;
      return Result;
   end Value;

   function Value (Object : value_t) return Unsigned_64 is
      Result : Unsigned_64;
   begin
      Check_Value (Object, Links.UInt64_Type);
      Result := Links.Unbox_UInt64 (Object);
      Check_Error;
      return Result;
   end Value;

   function To_Julia (Value : Boolean) return value_t is
   begin
      if Value then
         return Links.Box_Bool (1);
      else
         return Links.Box_Bool (0);
      end if;
   end To_Julia;

   function To_Julia (Value : char) return value_t is
   begin
      return Links.Box_Char (Value);
   end To_Julia;

   function To_Julia (Value : Character) return value_t is
   begin
      return Links.Box_Char (To_C (Value));
   end To_Julia;

   procedure Split
             (  Value       : Ada.Calendar.Formatting.Second_Duration;
                Millisecond : out Integer;
                Microsecond : out Integer;
                Nanosecond  : out Integer
             )  is
      Count : constant Unsigned_32 :=
                       Unsigned_32
                       (  Long_Float'Floor
                          (  Long_Float (Value)
                          *  1_000_000_000.0
                       )  );
   begin
      Millisecond := Integer ( Count / 1_000_000);
      Microsecond := Integer ((Count / 1_000) mod 1_000);
      Nanosecond  := Integer ( Count          mod 1_000);
   end Split;

   function To_Julia (Value : Day_Duration) return value_t is
      Hour        : Ada.Calendar.Formatting.Hour_Number;
      Minute      : Ada.Calendar.Formatting.Minute_Number;
      Second      : Ada.Calendar.Formatting.Second_Number;
      Sub_Second  : Ada.Calendar.Formatting.Second_Duration;
      Millisecond : Integer;
      Microsecond : Integer;
      Nanosecond  : Integer;
   begin
      Ada.Calendar.Formatting.Split
      (  Seconds    => Value,
         Hour       => Hour,
         Minute     => Minute,
         Second     => Second,
         Sub_Second => Sub_Second
      );
      Split (Sub_Second, Millisecond, Microsecond, Nanosecond);
      return Eval_String
             (  "import Dates; Dates.Time("
             &  Image (Integer (Hour))
             &  ","
             &  Image (Integer (Minute))
             &  ","
             &  Image (Integer (Second))
             &  ","
             &  Image (Millisecond)
             &  ","
             &  Image (Microsecond)
             &  ","
             &  Image (Nanosecond)
             &  ")"
             );
   end To_Julia;

   function To_Julia (Value : Double) return value_t is
   begin
      return Links.Box_Double (Value);
   end To_Julia;

   function To_Julia (Value : C_Float) return value_t is
   begin
      return Links.Box_Float (Value);
   end To_Julia;

   function To_Julia (Value : Integer_8  ) return value_t is
   begin
      return Links.Box_Int8 (Value);
   end To_Julia;

   function To_Julia (Value : Integer_16 ) return value_t is
   begin
      return Links.Box_Int16 (Value);
   end To_Julia;

   function To_Julia (Value : Integer_32 ) return value_t is
   begin
      return Links.Box_Int32 (Value);
   end To_Julia;

   function To_Julia (Value : Integer_64 ) return value_t is
   begin
      return Links.Box_Int64 (Value);
   end To_Julia;

   function To_Julia (Value : String) return value_t is
   begin
      if Value'Length = 0 then
         return Links.Pchar_To_String (Null_Address, 0);
      else
         return Links.Pchar_To_String
                (  Value (Value'First)'Address,
                   Value'Length
                );
      end if;
   end To_Julia;

   function To_Julia (Value : Time) return value_t is
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Ada.Calendar.Formatting.Hour_Number;
      Minute      : Ada.Calendar.Formatting.Minute_Number;
      Second      : Ada.Calendar.Formatting.Second_Number;
      Sub_Second  : Ada.Calendar.Formatting.Second_Duration;
      Millisecond : Integer;
      Microsecond : Integer;
      Nanosecond  : Integer;
   begin
      Ada.Calendar.Formatting.Split
      (  Date       => Value,
         Year       => Year,
         Month      => Month,
         Day        => Day,
         Hour       => Hour,
         Minute     => Minute,
         Second     => Second,
         Sub_Second => Sub_Second,
         Time_Zone  => Ada.Calendar.Time_Zones.UTC_Time_Offset (Value)
      );
      Split (Sub_Second, Millisecond, Microsecond, Nanosecond);
      return Eval_String
             (  "import Dates; Dates.DateTime("
             &  Image (Integer (Year))
             &  ","
             &  Image (Integer (Month))
             &  ","
             &  Image (Integer (Day))
             &  ","
             &  Image (Integer (Hour))
             &  ","
             &  Image (Integer (Minute))
             &  ","
             &  Image (Integer (Second))
             &  ","
             &  Image (Millisecond)
             &  ")"
             );
   end To_Julia;

   function To_Julia (Value : Unsigned_8 ) return value_t is
   begin
      return Links.Box_UInt8 (Value);
   end To_Julia;

   function To_Julia (Value : Unsigned_16) return value_t is
   begin
      return Links.Box_UInt16 (Value);
   end To_Julia;

   function To_Julia (Value : Unsigned_32) return value_t is
   begin
      return Links.Box_UInt32 (Value);
   end To_Julia;

   function To_Julia (Value : Unsigned_64) return value_t is
   begin
      return Links.Box_UInt64 (Value);
   end To_Julia;

   function To_Julia (Value : values_array) return value_t is
      Types_Of   : datatypes_array (Value'Range);
      Tuple_Type : datatype_t;
   begin
      for Index in Value'Range loop
         Check_Value (Value (Index));
      end loop;
      if Value'Length = 0 then
         declare
            Nothing : aliased datatype_t;
         begin
            Tuple_Type :=
               Links.Apply_Tuple_Type_V (Nothing'Unchecked_Access, 0);
            return Links.New_Struct_Uninit (Tuple_Type);
         end;
      else
         for Index in Value'Range loop
            Types_Of (Index) := Links.TypeOf (Value (Index));
         end loop;
         Tuple_Type :=
             Links.Apply_Tuple_Type_V
             (  Types_Of (Types_Of'First)'Unchecked_Access,
                Types_Of'Length
             );
         return Links.New_StructV
                (  Tuple_Type,
                   Value (Value'First)'Unchecked_Access,
                   Value'Length
                );
      end if;
   end To_Julia;

   function To_Julia (Value : Tuple) return value_t is
      use Strings_Edit;
      function Get_Types return value_t;
      pragma Convention (C, Get_Types);

      function Get_Types return value_t is
         Values : values_array (1..Length (Value));
      begin
         for Index in Values'Range loop
            Values (Index) := Get_Value (Value, Integer (Index));
         end loop;
         return To_Julia (Values);
      end Get_Types;

      Size  : Natural := 512;
      Roots : Holder (Length (Value) + 1);
   begin
      for Index in 1..Length (Value) loop
         Set (Roots, Index, Get_Value (Value, Index));
      end loop;
      loop
         declare
            Code    : String (1..Size);
            Pointer : Integer := 1;
         begin
            Put (Code, Pointer, "NamedTuple{(");
            for Index in 1..Length (Value) loop
               if Index > 1 then
                  Put (Code, Pointer, ", ");
               end if;
               Put (Code, Pointer, ":");
               Put (Code, Pointer, Get_Name (Value, Index));
            end loop;
            Put
            (  Code,
               Pointer,
               (  ")}(ccall("
               &  CCall_Address (Get_Types'Address)
               &  ",Any,()))"
            )  );
            return Eval_String (Code (1..Pointer - 1));
         exception
            when Layout_Error =>
               Size := Size * 2;
         end;
      end loop;
   end To_Julia;

   procedure Load (Name : String := "") is
   begin
      Julia.Load_Julia_Library.Load (Name);
   end Load;

   procedure Check_Links (Library_Name : String) is
      procedure Error (Entry_Name : String) is
      begin
         Raise_Exception
         (  Use_Error'Identity,
            (  "Failed to locate entry "
            &  Entry_Name
            &  " in the Julia DLL "
            &  Quote (Library_Name)
         )  );
      end Error;
   begin
      if Links.Alloc_Array_1D = null then
         Error ("jl_alloc_array_1d");
      elsif Links.Alloc_Array_2D = null then
         Error ("jl_alloc_array_2d");
      elsif Links.Alloc_Array_3D = null then
         Error ("jl_alloc_array_3d");
      elsif Links.Alloc_SVec_Uninit = null then
         Error ("jl_alloc_svec_uninit");
      elsif Links.Any_Type = Null_Address then
         Error ("jl_any_type");
      elsif Links.Anytuple_Type = Null_Address then
         Error ("jl_anytuple_type");
      elsif Links.Apply_Array_Type = null then
         Error ("jl_apply_array_type");
      elsif Links.Apply_Tuple_Type_V = null then
         Error ("jl_apply_tuple_type_v");
      elsif Links.Array_Eltype = null then
         Error ("jl_array_eltype");
      elsif Links.Array_Rank = null then
         Error ("jl_array_rank");
      elsif Links.Array_Size = null then
         Error ("jl_array_size");
      elsif Links.ArrayLen = null then
         Error ("jl_arraylen");
      elsif Links.Array_Type = Null_Address then
         Error ("jl_array_type");
      elsif Links.Array_Typename = Null_Address then
         Error ("jl_array_typename");
      elsif Links.AtExit_Hook = null then
         Error ("jl_atexit_hook");
      elsif Links.Base_Module = Null_Address then
         Error ("jl_base_module");
      elsif Links.Bool_Type = Null_Address then
         Error ("jl_bool_type");
      elsif Links.Bounds_Error = null then
         Error ("jl_bounds_error");
      elsif Links.Box_Bool = null then
         Error ("jl_box_bool");
      elsif Links.Box_Bool = null then
         Error ("jl_box_char");
      elsif Links.Box_Double = null then
         Error ("jl_box_float64");
      elsif Links.Box_Float = null then
         Error ("jl_box_float32");
      elsif Links.Box_Int8 = null then
         Error ("jl_box_int8");
      elsif Links.Box_Int16 = null then
         Error ("jl_box_int16");
      elsif Links.Box_Int8 = null then
         Error ("jl_box_int32");
      elsif Links.Box_Int8 = null then
         Error ("jl_box_int64");
      elsif Links.Box_UInt8 = null then
         Error ("jl_box_uint8");
      elsif Links.Box_UInt16 = null then
         Error ("jl_box_uint16");
      elsif Links.Box_UInt8 = null then
         Error ("jl_box_uint32");
      elsif Links.Box_UInt8 = null then
         Error ("jl_box_uint64");
      elsif Links.Call = null then
         Error ("jl_call");
      elsif Links.Call0 = null then
         Error ("jl_call0");
      elsif Links.Call1 = null then
         Error ("jl_call1");
      elsif Links.Call2 = null then
         Error ("jl_call2");
      elsif Links.Call3 = null then
         Error ("jl_call3");
      elsif Links.Char_Type = Null_Address then
         Error ("jl_char_type");
      elsif Links.Core_Module = Null_Address then
         Error ("jl_core_module");
      elsif Links.Datatype_Type = Null_Address then
         Error ("jl_datatype_type");
      elsif Links.Error = null then
         Error ("jl_error");
      elsif Links.Eval_String = null then
         Error ("jl_eval_string");
      elsif Links.Exception_Clear = null then
         Error ("jl_exception_clear");
      elsif Links.Exception_Occurred = null then
         Error ("jl_exception_occurred");
      elsif Links.Field_IsDefined = null then
         Error ("jl_field_isdefined");
      elsif Links.Float32_Type = Null_Address then
         Error ("jl_float32_type");
      elsif Links.Float64_Type = Null_Address then
         Error ("jl_float64_type");
      elsif Links.GC_Collect = null then
         Error ("jl_gc_collect");
      elsif Links.GC_Diff_Total_Bytes = null then
         Error ("jl_gc_diff_total_bytes");
      elsif Links.GC_Enable = null then
         Error ("jl_gc_enable");
      elsif Links.GC_Is_Enabled = null then
         Error ("jl_gc_is_enabled");
      elsif Links.GC_Queue_Root = null then
         Error ("jl_gc_queue_root");
      elsif Links.GC_Get_Total_Bytes = null and then
            Links.GC_Total_Bytes     = null     then
         Error ("jl_gc_total_bytes or jl_gc_get_total_bytes");
      elsif Links.Get_Field = null then
         Error ("jl_get_field");
      elsif Links.Get_Global = null then
         Error ("jl_get_global");
      elsif Links.Get_Nth_Field = null then
         Error ("jl_get_nth_field");
      elsif Links.Get_Nth_Field_Checked = null then
         Error ("jl_get_nth_field_checked");
      elsif Links.Get_PTLS_States = null then
         Error ("jl_get_ptls_states");
      elsif Links.Get_Safe_Restore = null then
         Error ("jl_get_safe_restore");
      elsif Links.Init = null then
         Error ("jl_init");
      elsif Links.Init_With_Image = null then
         Error ("jl_init_with_image");
      elsif Links.IsA = null then
         Error ("jl_isa");
      elsif Links.Int8_Type = Null_Address then
         Error ("jl_int8_type");
      elsif Links.Int16_Type = Null_Address then
         Error ("jl_int16_type");
      elsif Links.Int32_Type = Null_Address then
         Error ("jl_int32_type");
      elsif Links.Int64_Type = Null_Address then
         Error ("jl_int64_type");
      elsif Links.Load = null then
         Error ("jl_load");
      elsif Links.Load_File_String = null then
         Error ("jl_load_file_string");
      elsif Links.Main_Module = Null_Address then
         Error ("jl_main_module");
      elsif Links.Method_Type = Null_Address then
         Error ("jl_method_type");
      elsif Links.Module_Type = Null_Address then
         Error ("jl_module_type");
      elsif Links.Namedtuple_Type = Null_Address then
         Error ("jl_namedtuple_type");
      elsif Links.Namedtuple_Typename = Null_Address then
         Error ("jl_namedtuple_typename");
      elsif Links.New_Datatype = null then
         Error ("jl_new_datatype");
      elsif Links.New_StructV = null then
         Error ("jl_new_structv");
      elsif Links.New_Struct_Uninit = null then
         Error ("jl_new_struct_uninit");
      elsif Links.Pchar_To_String = null then
         Error ("jl_pchar_to_string");
      elsif Links.Set_Nth_Field = null then
         Error ("jl_set_nth_field");
      elsif Links.Set_Safe_Restore = null then
         Error ("jl_set_safe_restore");
--    elsif Links.Setjmp = null then
--       Error ("jl_setjmp");
      elsif Links.Simplevector_Type = Null_Address then
         Error ("jl_simplevector_type");
      elsif Links.Static_Show = null then
         Error ("jl_static_show");
      elsif Links.String_Ptr = null then
         Error ("jl_string_ptr");
      elsif Links.String_Type = Null_Address then
         Error ("jl_string_type");
      elsif Links.SVec_Fill = null then
         Error ("jl_svec_fill");
      elsif Links.Symbol = null then
         Error ("jl_symbol");
      elsif Links.Too_Few_Args = null then
         Error ("jl_too_few_args");
      elsif Links.Too_Many_Args = null then
         Error ("jl_too_many_args");
      elsif Links.Top_Module = Null_Address then
         Error ("jl_top_module");
      elsif Links.Tuple_Typename = Null_Address then
         Error ("jl_tupe_typename");
      elsif Links.Tupletype_Fill = null then
         Error ("jl_tupletype_fill");
      elsif Links.Type_Error = null then
         Error ("jl_type_error");
      elsif Links.TypeName_Str = null then
         Error ("jl_typename_str");
      elsif Links.TypeOf = null then
         Error ("jl_typeof");
      elsif Links.TypeOf_Str = null then
         Error ("jl_typeof_str");
      elsif Links.Types_Equal = null then
         Error ("jl_types_equal");
      elsif Links.Type_Morespecific = null then
         Error ("jl_type_morespecific");
      elsif Links.Unbox_Bool = null then
         Error ("jl_unbox_bool");
--    elsif Links.Unbox_Char = null then
--       Error ("jl_unbox_char");
      elsif Links.Unbox_Double = null then
         Error ("jl_unbox_float64");
      elsif Links.Unbox_Float = null then
         Error ("jl_unbox_float32");
      elsif Links.Unbox_Int8 = null then
         Error ("jl_unbox_int8");
      elsif Links.Unbox_Int16 = null then
         Error ("jl_unbox_int16");
      elsif Links.Unbox_Int8 = null then
         Error ("jl_unbox_int32");
      elsif Links.Unbox_Int8 = null then
         Error ("jl_unbox_int64");
      elsif Links.Unbox_UInt8 = null then
         Error ("jl_unbox_uint8");
      elsif Links.Unbox_UInt16 = null then
         Error ("jl_unbox_uint16");
      elsif Links.Unbox_UInt8 = null then
         Error ("jl_unbox_uint32");
      elsif Links.Unbox_UInt8 = null then
         Error ("jl_unbox_uint64");
      elsif Links.UInt8_Type = Null_Address then
         Error ("jl_uint8_type");
      elsif Links.UInt16_Type = Null_Address then
         Error ("jl_uint16_type");
      elsif Links.UInt32_Type = Null_Address then
         Error ("jl_uint32_type");
      elsif Links.UInt64_Type = Null_Address then
         Error ("jl_uint64_type");
      elsif Links.Uniontype_Type = Null_Address then
         Error ("jl_uniontype_type");
      end if;
   end Check_Links;

end Julia;
