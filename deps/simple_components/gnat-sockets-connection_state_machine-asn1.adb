--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--        ASN1                                     Spring, 2019       --
--  Implementation                                                    --
--                                Last revision :  10:13 29 Nov 2020  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Ada.Tags;               use Ada.Tags;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body GNAT.Sockets.Connection_State_Machine.ASN1 is

   function "=" (Left, Right : Tag_Type) return Boolean is
   begin
      return Left.Class = Right.Class and then Left.Value = Right.Value;
   end "=";

   function "<" (Left, Right : Tag_Type) return Boolean is
   begin
      return
      (  Left.Class < Right.Class
      or else
         (  Left.Class = Right.Class
         and then
            Left.Value < Right.Value
      )  );
   end "<";

   function Always_Constructed
            (  Item : ASN1_Data_Item
            )  return Boolean is
   begin
      return False;
   end Always_Constructed;

   function Always_Constructed (Tag : ASN1_Type) return Boolean is
   begin
      case Tag is
         when Sequence_Tag | Set_Tag =>
            return True;
         when others =>
            return False;
      end case;
   end Always_Constructed;

   procedure Check
             (  Tag       : Stream_Element;
                Expected  : ASN1_Type_Array;
                Primitive : Boolean := False
             )  is
      This : constant ASN1_Type := ASN1_Type (Tag and 16#1F#);
   begin
      if Primitive and then 0 /= (ASN1_Tag (Tag) and Constructed) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Object of ASN.1 type "
            &  Image (This)
            &  " must be primitive"
         )  );
      elsif Universal_Class = (ASN1_Tag (Tag) and 2#1100_0000#) then
         for Index in Expected'Range loop
            if This = Expected (Index) then
               return;
            end if;
         end loop;
         Raise_Exception
         (  Data_Error'Identity,
            (  "ASN.1 type "
            &  Image (This)
            &  " does not match expected type "
            &  Image (Expected)
         )  );
      elsif Expected'Length = 1 then
         Raise_Exception
         (  Data_Error'Identity,
            "ASN.1 type " & Image (Expected) & " is expected"
         );
      else
         Raise_Exception
         (  Data_Error'Identity,
            "One of ASN.1 types " & Image (Expected) & " is expected"
         );
      end if;
   end Check;

   procedure Get
             (  Data        : Stream_Element_Array;
                Pointer     : in out Stream_Element_Offset;
                Tag         : out Tag_Type;
                Constructed : out Boolean
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )
      then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      elsif Pointer <= Data'Last then
         case ASN1_Tag (Data (Pointer) and 2#1100_0000#) is
            when Application_Class =>
               Tag.Class := Application_Tag;
            when Context_Specific_Class =>
               Tag.Class := Context_Specific_Tag;
            when Private_Class =>
               Tag.Class := Private_Tag;
            when others =>
               Tag.Class := Universal_Tag;
         end case;
         Tag.Optional := False;
         Constructed :=
             0 /= (ASN1_Tag (Data (Pointer)) and ASN1.Constructed);
         Tag.Value := ASN1_Type (Data (Pointer) and 16#1F#);
         if Tag.Value < 31 then
            Pointer := Pointer + 1;
            return;
         end if;
         declare
            Index : Stream_Element_Offset := Pointer + 1;
         begin
            Tag.Value := 0;
            while Index <= Data'Last loop
               begin
                  Tag.Value :=
                     (  Tag.Value * 128
                     +  ASN1_Type (Data (Index) and 16#7F#)
                     );
               exception
                  when Constraint_Error =>
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        Tag_Too_Large
                     );
               end;
               if 0 = (Data (Index) and 16#80#) then
                  Pointer := Index + 1;
                  return;
               end if;
               Index := Index + 1;
            end loop;
         end;
         Raise_Exception
         (  End_Error'Identity,
            "Non-terminated or missing ASN.1 tag"
         );
      end if;
   end Get;

   function Image (Tag : ASN1_Type) return String is
   begin
      case Tag is
         when Boolean_Tag           => return "BOOLEAN";
         when Integer_Tag           => return "INTEGER";
         when Bit_String_Tag        => return "BITSTRING";
         when Octet_String_Tag      => return "OCTET STRING";
         when Null_Tag              => return "NULL";
         when Object_Identifier_Tag => return "OBJECT IDENTIFIER";
         when Object_Descriptor_Tag => return "ObjectDescriptor";
         when Instance_Tag          => return "INSTANCE";
         when Real_Tag              => return "REAL";
         when Enumerated_Tag        => return "ENUMERATED";
         when Embedded_Tag          => return "EMBEDDED PDV";
         when UTF8_String_Tag       => return "UTF8String";
         when Relative_OID_Tag      => return "RELATIVE-OID";
         when Time_Tag              => return "TIME";
         when Sequence_Tag          => return "SEQUENCE";
         when Set_Tag               => return "SET";
         when Numeric_String_Tag    => return "NumericString";
         when Printable_String_Tag  => return "PrintableString";
         when Teletext_String_Tag   => return "TeletexString";
         when Videotext_String_Tag  => return "VideotexString";
         when IA5_String_Tag        => return "IA5String";
         when UTC_Time_Tag          => return "UTCTime";
         when Generalized_Time_Tag  => return "GeneralizedTime";
         when Graphic_String_Tag    => return "GraphicString";
         when ISO646_String_Tag     => return "ISO 646 string";
         when General_String_Tag    => return "GeneralString";
         when Universal_String_Tag  => return "UniversalString";
         when Character_String_Tag  => return "CHARACTER STRING";
         when BMP_String_Tag        => return "BMPString";
         when Date_Tag              => return "DATE";
         when Time_Of_Day_Tag       => return "TIME-OF-DAY";
         when Date_Time_Tag         => return "DATE-TIME";
         when Duration_Tag          => return "DURATION";
         when others                => return Image (Integer (Tag));
      end case;
   end Image;

   function Image (Tags : ASN1_Type_Array) return String is
      Buffer  : String (1..512);
      Pointer : Integer := 1;
   begin
      for Index in Tags'Range loop
         if Index > Tags'First then
            Put (Buffer, Pointer, ", ");
         end if;
         Put (Buffer, Pointer, Image (Tags (Index)));
      end loop;
      return Buffer (1..Pointer - 1);
   end Image;

   function Image (Value : Tag_Type) return String is
      Result  : String (1..60);
      Pointer : Integer := 1;
   begin
      case Value.Class is
         when Application_Tag =>
            Put (Result, Pointer, "[APPLICATION ");
            Put (Result, Pointer, Integer (Value.Value));
            Put (Result, Pointer, "]");
         when Context_Specific_Tag =>
            Put (Result, Pointer, "[");
            Put (Result, Pointer, Integer (Value.Value));
            Put (Result, Pointer, "]");
         when Private_Tag =>
            Put (Result, Pointer, "[PRIVATE ");
            Put (Result, Pointer, Integer (Value.Value));
            Put (Result, Pointer, "]");
         when Universal_Tag =>
            Put (Result, Pointer, "[UNIVERSAL ");
            Put (Result, Pointer, Integer (Value.Value));
            Put (Result, Pointer, "]");
      end case;
      return Result (1..Pointer - 1);
   end Image;

   procedure Put
             (  Data        : in out Stream_Element_Array;
                Pointer     : in out Stream_Element_Offset;
                Tag         : Tag_Type;
                Constructed : Boolean
             )  is
      Header : ASN1_Tag;
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      end if;
      case Tag.Class is
         when Application_Tag =>
            Header := Application_Class;
         when Context_Specific_Tag =>
            Header := Context_Specific_Class;
         when Private_Tag =>
            Header := Private_Class;
         when Universal_Tag =>
            Header := Universal_Class;
      end case;
      if Constructed then
         Header := Header + ASN1.Constructed;
      end if;
      if Tag.Value < 31 then -- Single octet length
         if Pointer > Data'Last then
            Raise_Exception (End_Error'Identity, No_Room);
         end if;
         Data (Pointer) :=
            Stream_Element (Header) + Stream_Element (Tag.Value);
         Pointer := Pointer + 1;
      else
         declare
            Value  : ASN1_Type := Tag.Value;
            Length : Stream_Element_Count := 1;
         begin
            loop
               Value := Value / 128;
               exit when Value = 0;
               Length := Length + 1;
            end loop;
            if Data'Last - Pointer < Length then
               Raise_Exception (End_Error'Identity, No_Room);
            end if;
            Data (Pointer) := Stream_Element (Header) or 16#1F#;
            Value := Tag.Value;
            Data (Pointer + Length) := Stream_Element (Value mod 128);
            Value := Value / 128;
            for Octet in reverse Pointer + 1..Pointer + Length - 1 loop
               Data (Octet) := Stream_Element (Value mod 128) or 16#80#;
               Value := Value / 128;
            end loop;
            Pointer := Pointer + Length + 1;
         end;
      end if;
   end Put;

   procedure Set_Implicit_Tag
             (  Item   : in out Abstract_ASN1_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length < 0 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "A non-constructed object "
            &  Expanded_Name (Abstract_ASN1_Data_Item'Class (Item)'Tag)
            &  " has indefinite length"
         )  );
      elsif Length = 0 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "ASN.1 object "
            &  Expanded_Name (Abstract_ASN1_Data_Item'Class (Item)'Tag)
            &  " has zero length"
         )  );
      end if;
   end Set_Implicit_Tag;

   procedure Set_Untagged (Item : in out Abstract_ASN1_Data_Item) is
   begin
      if Is_Implicit (Abstract_ASN1_Data_Item'Class (Item)) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "ASN.1 indefinite object "
            &  Expanded_Name (Abstract_ASN1_Data_Item'Class (Item)'Tag)
            &  " has no tag"
         )  );
      end if;
   end Set_Untagged;

end GNAT.Sockets.Connection_State_Machine.ASN1;
