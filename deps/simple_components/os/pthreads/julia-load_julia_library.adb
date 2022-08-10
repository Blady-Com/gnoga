--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Julia.Load_Julia_Library                    Luebeck            --
--  Implementation                                 Winter, 2019       --
--                                                                    --
--                                Last revision :  11:37 20 Jan 2019  --
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

with Ada.Streams.Stream_IO;
with Interfaces;

package body Julia.Load_Julia_Library is

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
               Name   : char_array := "jl_alloc_array_1d" & Nul
            )  return Alloc_Array_1D_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_alloc_array_2d" & Nul
            )  return Alloc_Array_2D_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_alloc_array_3d" & Nul
            )  return Alloc_Array_3D_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_alloc_svec_uninit" & Nul
            )  return Alloc_SVec_Uninit_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_apply_array_type" & Nul
            )  return Apply_Array_Type_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_array_eltype" & Nul
            )  return Array_Eltype_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_array_rank" & Nul
            )  return Array_Rank_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_array_size" & Nul
            )  return Array_Size_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_apply_tuple_type_v" & Nul
            )  return Apply_Tuple_Type_V_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_arraylen" & Nul
            )  return ArrayLen_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_atexit_hook" & Nul
            )  return AtExit_Hook_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_bounds_error" & Nul
            )  return Bounds_Error_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_box_bool" & Nul
            )  return Box_Bool_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_box_char" & Nul
            )  return Box_Char_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_box_float32" & Nul
            )  return Box_Float_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_box_float64" & Nul
            )  return Box_Double_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_box_int8" & Nul
            )  return Box_Int8_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_box_int16" & Nul
            )  return Box_Int16_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_box_int32" & Nul
            )  return Box_Int32_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_box_int64" & Nul
            )  return Box_Int64_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_box_uint8" & Nul
            )  return Box_UInt8_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_box_uint16" & Nul
            )  return Box_UInt16_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_box_uint32" & Nul
            )  return Box_UInt32_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_box_uint64" & Nul
            )  return Box_UInt64_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_call" & Nul
            )  return Call_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_call0" & Nul
            )  return Call0_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_call1" & Nul
            )  return Call1_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_call2" & Nul
            )  return Call2_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_call3" & Nul
            )  return Call3_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_error" & Nul
            )  return Error_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_eval_string" & Nul
            )  return Eval_String_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_exception_clear" & Nul
            )  return Exception_Clear_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_exception_occurred" & Nul
            )  return Exception_Occurred_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_field_isdefined" & Nul
            )  return Field_IsDefined_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_gc_collect" & Nul
            )  return GC_Collect_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_gc_diff_total_bytes" & Nul
            )  return GC_Diff_Total_Bytes_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_gc_enable" & Nul
            )  return GC_Enable_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_gc_is_enabled" & Nul
            )  return GC_Is_Enabled_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_gc_queue_root" & Nul
            )  return GC_Queue_Root_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_gc_total_bytes" & Nul
            )  return GC_Total_Bytes_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_gc_get_total_bytes" & Nul
            )  return GC_Get_Total_Bytes_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_get_field" & Nul
            )  return Get_Field_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_get_global" & Nul
            )  return Get_Global_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_get_nth_field" & Nul
            )  return Get_Nth_Field_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_get_nth_field_checked" & Nul
            )  return Get_Nth_Field_Checked_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_get_ptls_states" & Nul
            )  return Get_PTLS_States_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_get_safe_restore" & Nul
            )  return Get_Safe_Restore_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_init__threading" & Nul
            )  return Init_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array :=
                                   "jl_init_with_image__threading" & Nul
            )  return Init_With_Image_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_isa" & Nul
            )  return IsA_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_load" & Nul
            )  return Load_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_load_file_string" & Nul
            )  return Load_File_String_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_new_datatype" & Nul
            )  return New_Datatype_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_new_structv" & Nul
            )  return New_StructV_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_new_struct_uninit" & Nul
            )  return New_Struct_Uninit_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_pchar_to_string" & Nul
            )  return Pchar_To_String_Ptr;
--   function dlsym
--            (  Module : Address;
--               Name   : char_array := "jl_setjmp" & Nul
--            )  return Setjmp_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_set_nth_field" & Nul
            )  return Set_Nth_Field_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_set_safe_restore" & Nul
            )  return Set_Safe_Restore_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_static_show" & Nul
            )  return Static_Show_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_string_ptr" & Nul
            )  return String_Ptr_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_svec_fill" & Nul
            )  return SVec_Fill_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_symbol" & Nul
            )  return Symbol_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_too_few_args" & Nul
            )  return Too_Few_Args_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_tupletype_fill" & Nul
            )  return Tupletype_Fill_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_type_error" & Nul
            )  return Type_Error_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_too_many_args" & Nul
            )  return Too_Many_Args_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_typename_str" & Nul
            )  return TypeName_Str_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_typeof" & Nul
            )  return TypeOf_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_typeof_str" & Nul
            )  return TypeOf_Str_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_types_equal" & Nul
            )  return Types_Equal_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_type_morespecific" & Nul
            )  return Type_Morespecific_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_unbox_bool" & Nul
            )  return Unbox_Bool_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_unbox_char" & Nul
            )  return Unbox_Char_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_unbox_float32" & Nul
            )  return Unbox_Float_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_unbox_float64" & Nul
            )  return Unbox_Double_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_unbox_int8" & Nul
            )  return Unbox_Int8_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_unbox_int16" & Nul
            )  return Unbox_Int16_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_unbox_int32" & Nul
            )  return Unbox_Int32_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_unbox_int64" & Nul
            )  return Unbox_Int64_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_unbox_uint8" & Nul
            )  return Unbox_UInt8_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_unbox_uint16" & Nul
            )  return Unbox_UInt16_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_unbox_uint32" & Nul
            )  return Unbox_UInt32_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array := "jl_unbox_uint64" & Nul
            )  return Unbox_UInt64_Ptr;
   function dlsym
            (  Module : Address;
               Name   : char_array
            )  return Address;
   pragma Import (C, dlsym);

   Library : Address := Null_Address;
   Loaded  : Boolean := False;

   function Get_Default_Name return String is
   begin
      return "libjulia.so.1";
   end Get_Default_Name;

   function Get_Default_Relative_Path return String is
   begin
      return "julia/sys.so";
   end Get_Default_Relative_Path;

   function Get_Extension return String is
   begin
      return "*.so.*";
   end Get_Extension;

   function Is_Loaded return Boolean is
   begin
      return Loaded;
   end Is_Loaded;

   procedure Load (Name : String := "") is
      function Get_Library_Path return String is
      begin
         if Name'Length = 0 then
            return Get_Default_Name;
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
                  "Failed to load Julia relocatable " & Quote (Name)
               );
            end if;
         end if;
         Links.AtExit_Hook           := dlsym (Library);
         Links.Alloc_Array_1D        := dlsym (Library);
         Links.Alloc_Array_2D        := dlsym (Library);
         Links.Alloc_Array_3D        := dlsym (Library);
         Links.Alloc_SVec_Uninit     := dlsym (Library);
         Links.Apply_Array_Type      := dlsym (Library);
         Links.Apply_Tuple_Type_V    := dlsym (Library);
         Links.Array_Eltype          := dlsym (Library);
         Links.Array_Rank            := dlsym (Library);
         Links.Array_Size            := dlsym (Library);
         Links.ArrayLen              := dlsym (Library);
         Links.Bounds_Error          := dlsym (Library);
         Links.Box_Bool              := dlsym (Library);
         Links.Box_Char              := dlsym (Library);
         Links.Box_Double            := dlsym (Library);
         Links.Box_Float             := dlsym (Library);
         Links.Box_Int8              := dlsym (Library);
         Links.Box_Int16             := dlsym (Library);
         Links.Box_Int32             := dlsym (Library);
         Links.Box_Int64             := dlsym (Library);
         Links.Box_UInt8             := dlsym (Library);
         Links.Box_UInt16            := dlsym (Library);
         Links.Box_UInt32            := dlsym (Library);
         Links.Box_UInt64            := dlsym (Library);
         Links.Call                  := dlsym (Library);
         Links.Call0                 := dlsym (Library);
         Links.Call1                 := dlsym (Library);
         Links.Call2                 := dlsym (Library);
         Links.Call3                 := dlsym (Library);
         Links.Error                 := dlsym (Library);
         Links.Eval_String           := dlsym (Library);
         Links.Exception_Clear       := dlsym (Library);
         Links.Exception_Occurred    := dlsym (Library);
         Links.Field_IsDefined       := dlsym (Library);
         Links.GC_Collect            := dlsym (Library);
         Links.GC_Diff_Total_Bytes   := dlsym (Library);
         Links.GC_Enable             := dlsym (Library);
         Links.GC_Is_Enabled         := dlsym (Library);
         Links.GC_Queue_Root         := dlsym (Library);
         Links.GC_Total_Bytes        := dlsym (Library);
         Links.GC_Get_Total_Bytes    := dlsym (Library);
         Links.Get_Field             := dlsym (Library);
         Links.Get_Global            := dlsym (Library);
         Links.Get_Nth_Field         := dlsym (Library);
         Links.Get_Nth_Field_Checked := dlsym (Library);
         Links.Get_PTLS_States       := dlsym (Library);
         Links.Get_Safe_Restore      := dlsym (Library);
         Links.Init                  := dlsym (Library);
         Links.Init_With_Image       := dlsym (Library);
         Links.IsA                   := dlsym (Library);
         Links.Load                  := dlsym (Library);
         Links.Load_File_String      := dlsym (Library);
         Links.New_Datatype          := dlsym (Library);
         Links.New_Struct_Uninit     := dlsym (Library);
         Links.New_StructV           := dlsym (Library);
         Links.Pchar_To_String       := dlsym (Library);
         Links.Set_Nth_Field         := dlsym (Library);
         Links.Set_Safe_Restore      := dlsym (Library);
--       Links.Setjmp                := dlsym (Library);
         Links.Static_Show           := dlsym (Library);
         Links.String_Ptr            := dlsym (Library);
         Links.SVec_Fill             := dlsym (Library);
         Links.Symbol                := dlsym (Library);
         Links.Too_Few_Args          := dlsym (Library);
         Links.Too_Many_Args         := dlsym (Library);
         Links.Tupletype_Fill        := dlsym (Library);
         Links.Type_Error            := dlsym (Library);
         Links.TypeName_Str          := dlsym (Library);
         Links.TypeOf                := dlsym (Library);
         Links.TypeOf_Str            := dlsym (Library);
         Links.Types_Equal           := dlsym (Library);
         Links.Type_Morespecific     := dlsym (Library);
         Links.Unbox_Bool            := dlsym (Library);
--       Links.Unbox_Char            := dlsym (Library);
         Links.Unbox_Double          := dlsym (Library);
         Links.Unbox_Float           := dlsym (Library);
         Links.Unbox_Int8            := dlsym (Library);
         Links.Unbox_Int16           := dlsym (Library);
         Links.Unbox_Int32           := dlsym (Library);
         Links.Unbox_Int64           := dlsym (Library);
         Links.Unbox_UInt8           := dlsym (Library);
         Links.Unbox_UInt16          := dlsym (Library);
         Links.Unbox_UInt32          := dlsym (Library);
         Links.Unbox_UInt64          := dlsym (Library);

         Links.Any_Type :=
            dlsym (Library, "jl_any_type" & Nul);
         Links.Anytuple_Type :=
            dlsym (Library, "jl_anytuple_type" & Nul);
         Links.Array_Type :=
            dlsym (Library, "jl_array_type" & Nul);
         Links.Array_Typename :=
            dlsym (Library, "jl_array_typename" & Nul);
         Links.Base_Module :=
            dlsym (Library, "jl_base_module" & Nul);
         Links.Bool_Type :=
            dlsym (Library, "jl_bool_type" & Nul);
         Links.Char_Type :=
            dlsym (Library, "jl_char_type" & Nul);
         Links.Core_Module :=
            dlsym (Library, "jl_core_module" & Nul);
         Links.Datatype_Type :=
            dlsym (Library, "jl_datatype_type" & Nul);
         Links.Float32_Type :=
            dlsym (Library, "jl_float32_type" & Nul);
         Links.Float64_Type :=
            dlsym (Library, "jl_float64_type" & Nul);
         Links.Int8_Type :=
            dlsym (Library, "jl_int8_type" & Nul);
         Links.Int16_Type :=
            dlsym (Library, "jl_int16_type" & Nul);
         Links.Int32_Type :=
            dlsym (Library, "jl_int32_type" & Nul);
         Links.Int64_Type :=
            dlsym (Library, "jl_int64_type" & Nul);
         Links.Main_Module :=
            dlsym (Library, "jl_main_module" & Nul);
         Links.Method_Type :=
            dlsym (Library, "jl_method_type" & Nul);
         Links.Module_Type :=
            dlsym (Library, "jl_module_type" & Nul);
         Links.Namedtuple_Typename :=
            dlsym (Library, "jl_namedtuple_typename" & Nul);
         Links.Namedtuple_Type :=
            dlsym (Library, "jl_namedtuple_type" & Nul);
         Links.Simplevector_Type :=
            dlsym (Library, "jl_simplevector_type" & Nul);
         Links.String_Type :=
            dlsym (Library, "jl_string_type" & Nul);
         Links.Top_Module :=
            dlsym (Library, "jl_top_module" & Nul);
         Links.Tuple_Typename :=
            dlsym (Library, "jl_tuple_typename" & Nul);
         Links.UInt8_Type :=
            dlsym (Library, "jl_uint8_type" & Nul);
         Links.UInt16_Type :=
            dlsym (Library, "jl_uint16_type" & Nul);
         Links.UInt32_Type :=
            dlsym (Library, "jl_uint32_type" & Nul);
         Links.UInt64_Type :=
            dlsym (Library, "jl_uint64_type" & Nul);
         Links.Uniontype_Type :=
            dlsym (Library, "jl_uniontype_type" & Nul);

         Check_Links (Name);
      end;
      Loaded := True;
   end Load;
end Julia.Load_Julia_Library;
