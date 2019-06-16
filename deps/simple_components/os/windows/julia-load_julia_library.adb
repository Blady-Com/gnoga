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

   LOAD_WITH_ALTERED_SEARCH_PATH       : constant := 16#00000008#;
   LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR    : constant := 16#00000100#;
   LOAD_LIBRARY_SEARCH_APPLICATION_DIR : constant := 16#00000200#;
   LOAD_LIBRARY_SEARCH_DEFAULT_DIRS    : constant := 16#00001000#;

   ERROR_PROC_NOT_FOUND : constant := 127;
   ERROR_BAD_EXE_FORMAT : constant := 193;

   type BOOL  is new int;
   type DWORD is new unsigned_long;

   function LoadLibrary
            (  Name  : wchar_array;
               File  : Address := Null_Address;
               Flags : DWORD   := LOAD_WITH_ALTERED_SEARCH_PATH
            )  return Address;
   pragma Import (Stdcall, LoadLibrary, "LoadLibraryExW");

   function GetEnvironmentVariable
            (  Name   : wchar_array;
               Buffer : in out wchar_array;
               Size   : DWORD
            )  return DWORD;
   pragma Import
          (  Stdcall,
             GetEnvironmentVariable,
             "GetEnvironmentVariableW"
          );

   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_alloc_array_1d" & Nul
            )  return Alloc_Array_1D_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_alloc_array_2d" & Nul
            )  return Alloc_Array_2D_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_alloc_array_3d" & Nul
            )  return Alloc_Array_3D_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_alloc_svec_uninit" & Nul
            )  return Alloc_SVec_Uninit_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_apply_array_type" & Nul
            )  return Apply_Array_Type_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_array_eltype" & Nul
            )  return Array_Eltype_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_array_rank" & Nul
            )  return Array_Rank_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_array_size" & Nul
            )  return Array_Size_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_apply_tuple_type_v" & Nul
            )  return Apply_Tuple_Type_V_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_arraylen" & Nul
            )  return ArrayLen_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_atexit_hook" & Nul
            )  return AtExit_Hook_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_bounds_error" & Nul
            )  return Bounds_Error_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_box_bool" & Nul
            )  return Box_Bool_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_box_char" & Nul
            )  return Box_Char_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_box_float32" & Nul
            )  return Box_Float_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_box_float64" & Nul
            )  return Box_Double_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_box_int8" & Nul
            )  return Box_Int8_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_box_int16" & Nul
            )  return Box_Int16_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_box_int32" & Nul
            )  return Box_Int32_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_box_int64" & Nul
            )  return Box_Int64_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_box_uint8" & Nul
            )  return Box_UInt8_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_box_uint16" & Nul
            )  return Box_UInt16_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_box_uint32" & Nul
            )  return Box_UInt32_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_box_uint64" & Nul
            )  return Box_UInt64_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_call" & Nul
            )  return Call_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_call0" & Nul
            )  return Call0_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_call1" & Nul
            )  return Call1_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_call2" & Nul
            )  return Call2_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_call3" & Nul
            )  return Call3_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_error" & Nul
            )  return Error_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_eval_string" & Nul
            )  return Eval_String_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_exception_clear" & Nul
            )  return Exception_Clear_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_exception_occurred" & Nul
            )  return Exception_Occurred_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_field_isdefined" & Nul
            )  return Field_IsDefined_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_gc_collect" & Nul
            )  return GC_Collect_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_gc_diff_total_bytes" & Nul
            )  return GC_Diff_Total_Bytes_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_gc_enable" & Nul
            )  return GC_Enable_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_gc_is_enabled" & Nul
            )  return GC_Is_Enabled_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_gc_queue_root" & Nul
            )  return GC_Queue_Root_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_gc_total_bytes" & Nul
            )  return GC_Total_Bytes_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_get_field" & Nul
            )  return Get_Field_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_get_global" & Nul
            )  return Get_Global_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_get_nth_field" & Nul
            )  return Get_Nth_Field_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_get_nth_field_checked" & Nul
            )  return Get_Nth_Field_Checked_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_get_ptls_states" & Nul
            )  return Get_PTLS_States_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_init__threading" & Nul
            )  return Init_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array :=
                                   "jl_init_with_image__threading" & Nul
            )  return Init_With_Image_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_isa" & Nul
            )  return IsA_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_load" & Nul
            )  return Load_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_new_datatype" & Nul
            )  return New_Datatype_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_new_structv" & Nul
            )  return New_StructV_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_new_struct_uninit" & Nul
            )  return New_Struct_Uninit_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_pchar_to_string" & Nul
            )  return Pchar_To_String_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_set_nth_field" & Nul
            )  return Set_Nth_Field_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_static_show" & Nul
            )  return Static_Show_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_string_ptr" & Nul
            )  return String_Ptr_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_svec_fill" & Nul
            )  return SVec_Fill_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_symbol" & Nul
            )  return Symbol_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_too_few_args" & Nul
            )  return Too_Few_Args_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_tupletype_fill" & Nul
            )  return Tupletype_Fill_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_type_error" & Nul
            )  return Type_Error_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_too_many_args" & Nul
            )  return Too_Many_Args_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_typename_str" & Nul
            )  return TypeName_Str_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_typeof" & Nul
            )  return TypeOf_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_typeof_str" & Nul
            )  return TypeOf_Str_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_types_equal" & Nul
            )  return Types_Equal_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_type_morespecific" & Nul
            )  return Type_Morespecific_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_unbox_bool" & Nul
            )  return Unbox_Bool_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_unbox_char" & Nul
            )  return Unbox_Char_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_unbox_float32" & Nul
            )  return Unbox_Float_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_unbox_float64" & Nul
            )  return Unbox_Double_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_unbox_int8" & Nul
            )  return Unbox_Int8_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_unbox_int16" & Nul
            )  return Unbox_Int16_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_unbox_int32" & Nul
            )  return Unbox_Int32_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_unbox_int64" & Nul
            )  return Unbox_Int64_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_unbox_uint8" & Nul
            )  return Unbox_UInt8_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_unbox_uint16" & Nul
            )  return Unbox_UInt16_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_unbox_uint32" & Nul
            )  return Unbox_UInt32_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array := "jl_unbox_uint64" & Nul
            )  return Unbox_UInt64_Ptr;
   function GetProcAddress
            (  Module : Address;
               Name   : char_array
            )  return Address;
   pragma Import (Stdcall, GetProcAddress, "GetProcAddress");

   function GetLastError return DWORD;
   pragma Import (Stdcall, GetLastError, "GetLastError");

   function SetEnvironmentVariable
            (  Name  : wchar_array;
               Value : wchar_array
            )  return BOOL;
   pragma Import
          (  Stdcall,
             SetEnvironmentVariable,
             "SetEnvironmentVariableW"
          );

   Library : Address := Null_Address;
   Loaded  : Boolean := False;

   function Get_Default_Name return String is
   begin
      return "libjulia.dll";
   end Get_Default_Name;

   function Get_Default_Relative_Path return String is
   begin
      return "..\lib\julia\sys.dll";
   end Get_Default_Relative_Path;

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
      use Ada.Streams;
      use Ada.Streams.Stream_IO;
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

   procedure Load (Name : String := "") is
      Error : DWORD;
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
                              &  " use a 32-bit Julia version instead"
                           )  );
                        end if;
                     when MACHINE_I386 =>
                        if Address'Size = 64 then
                           Raise_Exception
                           (  Use_Error'Identity,
                              (  "The DLL "
                              &  Quote (Name)
                              &  " is for x86 32-bit Windows,"
                              &  " use a 64-bit Julia version instead"
                           )  );
                        end if;
                     when others =>
                        null;
                  end case;
               end if;
               Raise_From_LastError
               (  Use_Error'Identity,
                  "Failed to load Julia DLL " & Quote (Name) & ": ",
                  Error
               );
            end if;
         end if;
         Links.AtExit_Hook := GetProcAddress (Library);
         if Links.AtExit_Hook = null then
            Error := GetLastError;
            if Error /= ERROR_PROC_NOT_FOUND then
               Raise_From_LastError
               (  Use_Error'Identity,
                  (  "Failed to load Julia DLL "
                  &  Quote (Name)
                  &  ", when looking for entry points: "
                  ),
                  Error
               );
            end if;
         end if;
         Links.Alloc_Array_1D        := GetProcAddress (Library);
         Links.Alloc_Array_2D        := GetProcAddress (Library);
         Links.Alloc_Array_3D        := GetProcAddress (Library);
         Links.Alloc_SVec_Uninit     := GetProcAddress (Library);
         Links.Apply_Array_Type      := GetProcAddress (Library);
         Links.Apply_Tuple_Type_V    := GetProcAddress (Library);
         Links.Array_Eltype          := GetProcAddress (Library);
         Links.Array_Rank            := GetProcAddress (Library);
         Links.Array_Size            := GetProcAddress (Library);
         Links.ArrayLen              := GetProcAddress (Library);
         Links.Bounds_Error          := GetProcAddress (Library);
         Links.Box_Bool              := GetProcAddress (Library);
         Links.Box_Char              := GetProcAddress (Library);
         Links.Box_Double            := GetProcAddress (Library);
         Links.Box_Float             := GetProcAddress (Library);
         Links.Box_Int8              := GetProcAddress (Library);
         Links.Box_Int16             := GetProcAddress (Library);
         Links.Box_Int32             := GetProcAddress (Library);
         Links.Box_Int64             := GetProcAddress (Library);
         Links.Box_UInt8             := GetProcAddress (Library);
         Links.Box_UInt16            := GetProcAddress (Library);
         Links.Box_UInt32            := GetProcAddress (Library);
         Links.Box_UInt64            := GetProcAddress (Library);
         Links.Call                  := GetProcAddress (Library);
         Links.Call0                 := GetProcAddress (Library);
         Links.Call1                 := GetProcAddress (Library);
         Links.Call2                 := GetProcAddress (Library);
         Links.Call3                 := GetProcAddress (Library);
         Links.Error                 := GetProcAddress (Library);
         Links.Eval_String           := GetProcAddress (Library);
         Links.Exception_Clear       := GetProcAddress (Library);
         Links.Exception_Occurred    := GetProcAddress (Library);
         Links.Field_IsDefined       := GetProcAddress (Library);
         Links.GC_Collect            := GetProcAddress (Library);
         Links.GC_Diff_Total_Bytes   := GetProcAddress (Library);
         Links.GC_Enable             := GetProcAddress (Library);
         Links.GC_Is_Enabled         := GetProcAddress (Library);
         Links.GC_Queue_Root         := GetProcAddress (Library);
         Links.GC_Total_Bytes        := GetProcAddress (Library);
         Links.Get_Field             := GetProcAddress (Library);
         Links.Get_Global            := GetProcAddress (Library);
         Links.Get_Nth_Field         := GetProcAddress (Library);
         Links.Get_Nth_Field_Checked := GetProcAddress (Library);
         Links.Get_PTLS_States       := GetProcAddress (Library);
         Links.Init                  := GetProcAddress (Library);
         Links.Init_With_Image       := GetProcAddress (Library);
         Links.IsA                   := GetProcAddress (Library);
         Links.Load                  := GetProcAddress (Library);
         Links.New_Datatype          := GetProcAddress (Library);
         Links.New_StructV           := GetProcAddress (Library);
         Links.New_Struct_Uninit     := GetProcAddress (Library);
         Links.Pchar_To_String       := GetProcAddress (Library);
         Links.Set_Nth_Field         := GetProcAddress (Library);
         Links.Static_Show           := GetProcAddress (Library);
         Links.String_Ptr            := GetProcAddress (Library);
         Links.SVec_Fill             := GetProcAddress (Library);
         Links.Symbol                := GetProcAddress (Library);
         Links.Too_Few_Args          := GetProcAddress (Library);
         Links.Too_Many_Args         := GetProcAddress (Library);
         Links.Tupletype_Fill        := GetProcAddress (Library);
         Links.Type_Error            := GetProcAddress (Library);
         Links.TypeName_Str          := GetProcAddress (Library);
         Links.TypeOf                := GetProcAddress (Library);
         Links.TypeOf_Str            := GetProcAddress (Library);
         Links.Types_Equal           := GetProcAddress (Library);
         Links.Type_Morespecific     := GetProcAddress (Library);
         Links.Unbox_Bool            := GetProcAddress (Library);
--       Links.Unbox_Char            := GetProcAddress (Library);
         Links.Unbox_Double          := GetProcAddress (Library);
         Links.Unbox_Float           := GetProcAddress (Library);
         Links.Unbox_Int8            := GetProcAddress (Library);
         Links.Unbox_Int16           := GetProcAddress (Library);
         Links.Unbox_Int32           := GetProcAddress (Library);
         Links.Unbox_Int64           := GetProcAddress (Library);
         Links.Unbox_UInt8           := GetProcAddress (Library);
         Links.Unbox_UInt16          := GetProcAddress (Library);
         Links.Unbox_UInt32          := GetProcAddress (Library);
         Links.Unbox_UInt64          := GetProcAddress (Library);

         Links.Any_Type :=
            GetProcAddress (Library, "jl_any_type" & Nul);
         Links.Anytuple_Type :=
            GetProcAddress (Library, "jl_anytuple_type" & Nul);
         Links.Array_Type :=
            GetProcAddress (Library, "jl_array_type" & Nul);
         Links.Array_Typename :=
            GetProcAddress (Library, "jl_array_typename" & Nul);
         Links.Base_Module :=
            GetProcAddress (Library, "jl_base_module" & Nul);
         Links.Bool_Type :=
            GetProcAddress (Library, "jl_bool_type" & Nul);
         Links.Char_Type :=
            GetProcAddress (Library, "jl_char_type" & Nul);
         Links.Core_Module :=
            GetProcAddress (Library, "jl_core_module" & Nul);
         Links.Datatype_Type :=
            GetProcAddress (Library, "jl_datatype_type" & Nul);
         Links.Float32_Type :=
            GetProcAddress (Library, "jl_float32_type" & Nul);
         Links.Float64_Type :=
            GetProcAddress (Library, "jl_float64_type" & Nul);
         Links.Int8_Type :=
            GetProcAddress (Library, "jl_int8_type" & Nul);
         Links.Int16_Type :=
            GetProcAddress (Library, "jl_int16_type" & Nul);
         Links.Int32_Type :=
            GetProcAddress (Library, "jl_int32_type" & Nul);
         Links.Int64_Type :=
            GetProcAddress (Library, "jl_int64_type" & Nul);
         Links.Main_Module :=
            GetProcAddress (Library, "jl_main_module" & Nul);
         Links.Method_Type :=
            GetProcAddress (Library, "jl_method_type" & Nul);
         Links.Module_Type :=
            GetProcAddress (Library, "jl_module_type" & Nul);
         Links.Namedtuple_Type :=
            GetProcAddress (Library, "jl_namedtuple_type" & Nul);
         Links.Namedtuple_Typename :=
            GetProcAddress (Library, "jl_namedtuple_typename" & Nul);
         Links.Simplevector_Type :=
            GetProcAddress (Library, "jl_simplevector_type" & Nul);
         Links.String_Type :=
            GetProcAddress (Library, "jl_string_type" & Nul);
         Links.Top_Module :=
            GetProcAddress (Library, "jl_top_module" & Nul);
         Links.Tuple_Typename :=
            GetProcAddress (Library, "jl_tuple_typename" & Nul);
         Links.UInt8_Type :=
            GetProcAddress (Library, "jl_uint8_type" & Nul);
         Links.UInt16_Type :=
            GetProcAddress (Library, "jl_uint16_type" & Nul);
         Links.UInt32_Type :=
            GetProcAddress (Library, "jl_uint32_type" & Nul);
         Links.UInt64_Type :=
            GetProcAddress (Library, "jl_uint64_type" & Nul);
         Links.Uniontype_Type :=
            GetProcAddress (Library, "jl_uniontype_type" & Nul);

         Check_Links (Name);
         for Index in reverse Name'Range loop
            case Name (Index) is
               when '\' =>
                  declare
                     Key  : constant wchar_array := "PATH" & wide_nul;
                     Size : DWORD;
                     Data : wchar_array (1..0);
                  begin
                     Size := GetEnvironmentVariable (Key, Data, 0);
                     exit when Size = 0;
                     declare
                        Result   : BOOL;
                        Old_Path : wchar_array (1..size_t (Size));
                     begin
                        Size := GetEnvironmentVariable
                                (  Key,
                                   Old_Path,
                                   Size
                                );
                        exit when Size = 0;
                        declare
                           New_Path : constant wchar_array :=
                              (  To_C
                                 (  To_Wide_String
                                    (  Name (Name'First..Index - 1)
                                    ),
                                    False
                                 )
                              &  ';'
                              &  Old_Path
                                 (  Old_Path'First
                                 .. Old_Path'First + size_t (Size) - 1
                              )  );
                        begin
                           Result :=
                              SetEnvironmentVariable (Key, New_Path);
                        end;
                     end;
                  end;
                  exit;
               when others =>
                  null;
            end case;
         end loop;
      end;
      Loaded := True;
   end Load;
end Julia.Load_Julia_Library;
