--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Text_IO                                Summer, 2019       --
--  Implementation                                                    --
--                                Last revision :  13:37 03 Aug 2019  --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.IO_Exceptions;              use Ada.IO_Exceptions;
with Ada.Tags;                       use Ada.Tags;
with Strings_Edit.Floats;            use Strings_Edit.Floats;
with Strings_Edit.Long_Floats;       use Strings_Edit.Long_Floats;
with Strings_Edit.Integers;          use Strings_Edit.Integers;
with Strings_Edit.Time_Conversions;  use Strings_Edit.Time_Conversions;

with GNAT.Sockets.Connection_State_Machine.ASN1.Booleans;
with GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.Implicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Dates;
with GNAT.Sockets.Connection_State_Machine.ASN1.Distinguished_Names;
with GNAT.Sockets.Connection_State_Machine.ASN1.Floats;
with GNAT.Sockets.Connection_State_Machine.ASN1.Indefinite_Unsigneds;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers_8;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers_16;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers_32;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers_64;
with GNAT.Sockets.Connection_State_Machine.ASN1.Long_Floats;
with GNAT.Sockets.Connection_State_Machine.ASN1.Nulls;
with GNAT.Sockets.Connection_State_Machine.ASN1.Object_Identifiers;
with GNAT.Sockets.Connection_State_Machine.ASN1.Objects;
with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Unsigneds_8;
with GNAT.Sockets.Connection_State_Machine.ASN1.Unsigneds_16;
with GNAT.Sockets.Connection_State_Machine.ASN1.Unsigneds_32;
with GNAT.Sockets.Connection_State_Machine.ASN1.Unsigneds_64;

with GNAT.Sockets.Connection_State_Machine.ASN1.Choices;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Choices;

with Strings_Edit.Distinguished_Names;
use  Strings_Edit.Distinguished_Names;

with Strings_Edit.Object_Identifiers;
use  Strings_Edit.Object_Identifiers;

with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Connection_State_Machine.ASN1.Text_IO is

   Extra : constant := 40;

   function To_String (Value : Duration) return String is
   begin
      return Image (Long_Float (Value), AbsSmall => -3) & "s";
   end To_String;

   procedure Put
             (  Item   : ASN1_Data_Item'Class;
                Prefix : String  := "";
                Wrap   : Positive_Count := 72
             )  is
   begin
      Put (Standard_Output, Item, Prefix);
   end Put;

   procedure Put
             (  File   : File_Type;
                Item   : ASN1_Data_Item'Class;
                Prefix : String  := "";
                Wrap   : Positive_Count := 72
             )  is
      procedure Dump_Bit_String
                (  Value : ASN1.Bit_Strings.Boolean_Array
                )  is
         From    : Positive_Count;
         To      : Positive_Count;
         Pointer : Integer := Value'First;
      begin
         if Value'Length = 0 then
            Put_Line (File, "<empty>");
            return;
         end if;
         Put (File, " ");
         From := Col (File);
         To   := Positive_Count'Max (Wrap, From + Extra) - 1;
         while Pointer <= Value'Last loop
            if Pointer > Value'First then
               Set_Col (File, From);
            end if;
            for Count in Positive_Count'Range loop
               if Count > 1 then
                  if (Count + 3) mod 4 = 0 then
                     Put (File, "_");
                  end if;
               end if;
               if Value (Pointer) then
                  Put (File, "1");
               else
                  Put (File, "0");
               end if;
               Pointer := Pointer + 1;
               exit when Pointer > Value'Last
                 or else Col (File) + 4 - (Count mod 4) > To;
            end loop;
            New_Line (File);
         end loop;
      end Dump_Bit_String;

      procedure Dump_Child
                (  Item   : Data_Item'Class;
                   Prefix : String
                )  is
      begin
          if Item in ASN1_Data_Item'Class then
             Put
             (  File,
                ASN1_Data_Item'Class (Item),
                Prefix,
                Wrap
             );
          elsif Item in ASN1.Choices.Choice_Data_Item'Class then
             Put_Line (File, Prefix & "CHOICE");
             Put
             (  File,
                ASN1_Data_Item'Class
                (  ASN1.Choices.Resolve_Selected
                   (  ASN1.Choices.Choice_Data_Item'Class (Item)
                   ) .all
                ),
                Prefix & "   ",
                Wrap
             );
          else
             Put_Line
             (  File,
                Prefix & Ada.Tags.Expanded_Name (Item'Tag)
             );
          end if;
      end Dump_Child;

      procedure Dump_Integer (Value : Stream_Element_Array) is
         From    : Positive_Count;
         To      : Positive_Count;
         Pointer : Stream_Element_Offset := Value'First;
         Buffer  : String (1..2);
         Index   : Integer;
      begin
         if Value'Length = 0 then
            Put_Line (File, "0");
            return;
         end if;
         Put (File, " 16#");
         From := Col (File);
         To   := Positive_Count'Max (Wrap, From + Extra) - 1;
         while Pointer <= Value'Last loop
            if Pointer > Value'First then
               Set_Col (File, From);
            end if;
            for Count in Positive_Count'Range loop
               if Count > 1 then
                  if (Count + 4) mod 5 = 0 then
                     Put (File, "_");
                  end if;
               end if;
               Index := 1;
               Put
               (  Destination => Buffer,
                  Pointer     => Index,
                  Value       => Integer (Value (Pointer)),
                  Base        => 16,
                  Justify     => Strings_Edit.Right,
                  Fill        => '0',
                  Field       => 2
               );
               Put (File, Buffer);
               Pointer := Pointer + 1;
               exit when Pointer > Value'Last
                 or else Col (File) + 15 - (Count mod 5) * 3 > To;
            end loop;
            if Pointer > Value'Last then
               Put_Line (File, "#");
            else
               New_Line (File);
            end if;
         end loop;
      end Dump_Integer;

      procedure Dump_Octet_String (Value : String) is
         From    : Positive_Count;
         To      : Positive_Count;
         Pointer : Integer := Value'First;
         Buffer  : String (1..2);
         Index   : Integer;
      begin
         if Value'Length = 0 then
            Put_Line (File, "<empty>");
            return;
         end if;
         Put (File, " ");
         From := Col (File);
         To   := Positive_Count'Max (Wrap, From + Extra) - 1;
         while Pointer <= Value'Last loop
            if Pointer > Value'First then
               Set_Col (File, From);
            end if;
            for Count in Positive_Count'Range loop
               if Count > 1 then
                  if (Count + 4) mod 5 = 0 then
                     Put (File, "  ");
                  else
                     Put (File, " ");
                  end if;
               end if;
               Index := 1;
               Put
               (  Destination => Buffer,
                  Pointer     => Index,
                  Value       => Character'Pos (Value (Pointer)),
                  Base        => 16,
                  Justify     => Strings_Edit.Right,
                  Fill        => '0',
                  Field       => 2
               );
               Put (File, Buffer);
               Pointer := Pointer + 1;
               exit when Pointer > Value'Last
                 or else Col (File) + 15 - (Count mod 5) * 3 > To;
            end loop;
            New_Line (File);
         end loop;
      end Dump_Octet_String;

      procedure Dump_Children
                (  Children : Data_Item_Ptr_Array;
                   Prefix   : String
                )  is
      begin
         for Index in Children'Range loop
            Dump_Child (Children (Index).all, Prefix & "   ");
         end loop;
      end Dump_Children;

      procedure Dump_String (Value : String) is
         From    : Positive_Count;
         Pointer : Integer := Value'First;
         Length  : Natural;
      begin
         if Value'Length = 0 then
            New_Line (File);
            return;
         end if;
         Put (File, " ");
         From   := Col (File);
         Length :=
            Natural
            (  Positive_Count'Max
               (  Wrap + 1,
                  From + Extra
               )
            -  From
            );
         while Pointer <= Value'Last loop
            Length := Integer'Min (Length, Value'Last + 1 - Pointer);
            if Pointer > Value'First then
               Set_Col (File, From);
            end if;
            Put_Line (File, Value (Pointer..Pointer + Length - 1));
            Pointer := Pointer + Length;
         end loop;
      end Dump_String;

   begin
      if Item in ASN1.Objects.Any_Data_Item'Class then
         declare
            use ASN1.Objects;
            This : Any_Data_Item'Class renames
                   Any_Data_Item'Class (Item);
         begin
            if Get_Tag (This).Class = Universal_Tag then
               Put
               (  File,
                  ASN1_Data_Item'Class (Get (This).all),
                  Prefix,
                  Wrap
               );
            else
               Put_Line (File, Prefix & Image (Get_Tag (This)));
               Put
               (  File,
                  ASN1_Data_Item'Class (Get (This).all),
                  Prefix & "   ",
                  Wrap
               );
            end if;
         end;
         return;
      end if;
      Put
      (  Prefix
      &  Image (Get_ASN1_Type (Item))
--        &  " "
--        &  Ada.Tags.Expanded_Name (Item'Tag)
      );
      case Get_ASN1_Type (Item) is
         when Sequence_Tag | ASN1.Set_Tag =>
            New_Line;
            if Item in ASN1.ASN1_Tagged_List_Data_Item'Class then
               declare
                  Sequence : ASN1.ASN1_Tagged_List_Data_Item'Class
                     renames ASN1.ASN1_Tagged_List_Data_Item'Class
                             (  Item
                             );
                  Children : constant Data_Item_Ptr_Array :=
                                      Get_Children (Item);
               begin
                  for Index in Children'Range loop
                     if not Is_Set (Sequence, Integer (Index)) then
                        null;
                     elsif (  Is_Untagged (Sequence, Integer (Index))
                        or else
                           (  Get_Tag (Sequence, Integer (Index)).Class
                           =  Universal_Tag
                        )  )  then
                        Dump_Child
                        (  Children (Index).all,
                           Prefix & "   "
                        );
                     else
                        Put_Line
                        (  Prefix
                        &  "   "
                        &  Image (Get_Tag (Sequence, Integer (Index)))
                        );
                        Dump_Child
                        (  Children (Index).all,
                           Prefix & "      "
                        );
                     end if;
                  end loop;
               end;
            else
               Dump_Children (Get_Children (Item), Prefix);
            end if;
         when Bit_String_Tag =>
            if Item in ASN1.Bit_Strings.Implicit.
                       Implicit_External_Bit_String_Data_Item'Class then
               Dump_Bit_String
               (  ASN1.Bit_Strings.Implicit.Get_Value
                  (  ASN1.Bit_Strings.Implicit.
                     Implicit_External_Bit_String_Data_Item'Class
                     (  Item
               )  )  );
            elsif Item in ASN1.Bit_Strings.Implicit.
                          Implicit_Bit_String_Data_Item'Class then
               Dump_Bit_String
               (  ASN1.Bit_Strings.Implicit.Get_Value
                  (  ASN1.Bit_Strings.Implicit.
                     Implicit_Bit_String_Data_Item'Class
                     (  Item
               )  )  );
            else
               Put_Line
               (  File,
                  Ada.Tags.Expanded_Name (Item'Tag)
               );
            end if;
         when Boolean_Tag =>
            Put_Line
            (  File,
               (  " "
               &  Boolean'Image
                  (  ASN1.Booleans.Get_Value
                     (  ASN1.Booleans.Implicit_Boolean_Data_Item'Class
                        (  Item
            )  )  )  )  );
         when Date_Tag =>
            Put_Line
            (  File,
               (  " "
               &  To_String
                  (  ASN1.Dates.Get_Time
                     (  ASN1.Dates.Public_Time_Data_Item
                        (  Item
            )  )  )  )  );
         when Date_Time_Tag =>
            Put_Line
            (  File,
               (  " "
               &  To_String
                  (  ASN1.Dates.Get_Time
                     (  ASN1.Dates.Public_Time_Data_Item
                        (  Item
            )  )  )  )  );
         when Duration_Tag =>
            Put_Line
            (  File,
               (  " "
               &  To_String
                  (  ASN1.Dates.Get_Duration
                     (  ASN1.Dates.Public_Duration_Data_Item
                        (  Item
            )  )  )  )  );
         when Generalized_Time_Tag =>
            Put_Line
            (  File,
               (  " "
               &  To_String
                  (  ASN1.Dates.Get_Time
                     (  ASN1.Dates.Public_Time_Data_Item
                        (  Item
            )  )  )  )  );
         when Integer_Tag | Enumerated_Tag =>
            if Item in
               ASN1.Integers_64.Implicit_Integer_Data_Item'Class then
               Put_Line
               (  File,
                  Interfaces.Integer_64'Image
                  (  ASN1.Integers_64.Get_Value
                     (  ASN1.Integers_64.
                        Implicit_Integer_Data_Item'Class
                        (  Item
               )  )  )  );
            elsif Item in
               ASN1.Integers_32.Implicit_Integer_Data_Item'Class then
               Put_Line
               (  File,
                  Interfaces.Integer_32'Image
                  (  ASN1.Integers_32.Get_Value
                     (  ASN1.Integers_32.
                        Implicit_Integer_Data_Item'Class
                        (  Item
               )  )  )  );
            elsif Item in
               ASN1.Integers_16.Implicit_Integer_Data_Item'Class then
               Put_Line
               (  File,
                  Interfaces.Integer_16'Image
                  (  ASN1.Integers_16.Get_Value
                     (  ASN1.Integers_16.
                        Implicit_Integer_Data_Item'Class
                        (  Item
               )  )  )  );
            elsif Item in
               ASN1.Unsigneds_64.Implicit_Unsigned_Data_Item'Class then
               Put_Line
               (  File,
                  Interfaces.Unsigned_64'Image
                  (  ASN1.Unsigneds_64.Get_Value
                     (  ASN1.Unsigneds_64.
                        Implicit_Unsigned_Data_Item'Class
                        (  Item
               )  )  )  );
            elsif Item in
               ASN1.Unsigneds_32.Implicit_Unsigned_Data_Item'Class then
               Put_Line
               (  File,
                  Interfaces.Unsigned_32'Image
                  (  ASN1.Unsigneds_32.Get_Value
                     (  ASN1.Unsigneds_32.
                        Implicit_Unsigned_Data_Item'Class
                        (  Item
               )  )  )  );
            elsif Item in
               ASN1.Unsigneds_16.Implicit_Unsigned_Data_Item'Class then
               Put_Line
               (  File,
                  Interfaces.Unsigned_16'Image
                  (  ASN1.Unsigneds_16.Get_Value
                     (  ASN1.Unsigneds_16.
                        Implicit_Unsigned_Data_Item'Class
                        (  Item
               )  )  )  );
            elsif Item in
               ASN1.Unsigneds_8.Implicit_Unsigned_Data_Item'Class then
               Put_Line
               (  File,
                  Interfaces.Unsigned_8'Image
                  (  ASN1.Unsigneds_8.Get_Value
                     (  ASN1.Unsigneds_8.
                        Implicit_Unsigned_Data_Item'Class
                        (  Item
               )  )  )  );
            elsif Item in
                  ASN1.Indefinite_Unsigneds.
                  Implicit_Indefinite_Unsigned_Data_Item'Class then
               Dump_Integer
               (  ASN1.Indefinite_Unsigneds.Get_Value
                  (  ASN1.Indefinite_Unsigneds.
                     Implicit_Indefinite_Unsigned_Data_Item'Class
                     (  Item
               )  )  );
            else
               Put_Line
               (  File,
                  " " & Ada.Tags.Expanded_Name (Item'Tag)
               );
            end if;
         when Octet_String_Tag =>
            if Item in ASN1.Strings.Implicit.
                       Implicit_External_String_Data_Item'Class then
               Dump_Octet_String
               (  ASN1.Strings.Implicit.Get_Value
                  (  ASN1.Strings.Implicit.
                     Implicit_External_String_Data_Item'Class
                     (  Item
               )  )  );
            elsif Item in ASN1.Strings.
                       Public_String_Data_Item'Class then
               Dump_Octet_String
               (  ASN1.Strings.Get_Value
                  (  ASN1.Strings.Public_String_Data_Item'Class
                     (  Item
               )  )  );
            else
               Put_Line
               (  File,
                  Ada.Tags.Expanded_Name (Item'Tag)
               );
            end if;
         when Null_Tag =>
            New_Line;
         when Object_Identifier_Tag =>
            if Item in ASN1.Object_Identifiers.
                       Implicit_OID_Data_Item'Class then
               Dump_String
               (  Image
                  (  ASN1.Object_Identifiers.Get_Value
                     (  ASN1.Object_Identifiers.
                        Implicit_OID_Data_Item'Class
                        (  Item
               )  )  )  );
            elsif Item in ASN1.Object_Identifiers.
                          OID_Data_Item'Class then
               Dump_String
               (  Image
                  (  ASN1.Object_Identifiers.Get_Value
                     (  ASN1.Object_Identifiers.OID_Data_Item'Class
                        (  Item
               )  )  )  );
            elsif Item in ASN1.Object_Identifiers.
                          Implicit_External_OID_Data_Item'Class then
               Dump_String
               (  Image
                  (  ASN1.Object_Identifiers.Get_Value
                     (  ASN1.Object_Identifiers.
                        Implicit_External_OID_Data_Item'Class
                        (  Item
               )  )  )  );
            elsif Item in ASN1.Object_Identifiers.
                          External_OID_Data_Item'Class then
               Dump_String
               (  Image
                  (  ASN1.Object_Identifiers.Get_Value
                     (  ASN1.Object_Identifiers.
                        External_OID_Data_Item'Class
                        (  Item
               )  )  )  );
            else
               Put_Line
               (  File,
                  Ada.Tags.Expanded_Name (Item'Tag)
               );
            end if;
         when Object_Descriptor_Tag =>
            if Item in ASN1.Distinguished_Names.
                       Implicit_DN_Data_Item'Class then
               Dump_String
               (  Image
                  (  ASN1.Distinguished_Names.Get_Name
                     (  ASN1.Distinguished_Names.
                        Implicit_DN_Data_Item'Class
                        (  Item
               )  )  )  );
            elsif Item in ASN1.Distinguished_Names.
                          DN_Data_Item'Class then
               Dump_String
               (  Image
                  (  ASN1.Distinguished_Names.Get_Name
                     (  ASN1.Distinguished_Names.DN_Data_Item'Class
                        (  Item
               )  )  )  );
            elsif Item in ASN1.Distinguished_Names.
                          Implicit_External_DN_Data_Item'Class then
               Dump_String
               (  Image
                  (  ASN1.Distinguished_Names.Get_Name
                     (  ASN1.Distinguished_Names.
                        Implicit_External_DN_Data_Item'Class
                        (  Item
               )  )  )  );
            elsif Item in ASN1.Distinguished_Names.
                          External_DN_Data_Item'Class then
               Dump_String
               (  Image
                  (  ASN1.Distinguished_Names.Get_Name
                     (  ASN1.Distinguished_Names.
                        External_DN_Data_Item'Class
                        (  Item
               )  )  )  );
            else
               Put_Line
               (  File,
                  Ada.Tags.Expanded_Name (Item'Tag)
               );
            end if;
         when Real_Tag =>
            if Item in ASN1.Long_Floats.
                       Implicit_Real_Data_Item'Class then
               Put_Line
               (  File,
                  (  " "
                  &  Image
                     (  ASN1.Long_Floats.Get_Value
                        (  ASN1.Long_Floats.
                           Implicit_Real_Data_Item'Class
                           (  Item
               )  )  )  )  );
            elsif Item in ASN1.Floats.
                          Implicit_Real_Data_Item'Class then
               Put_Line
               (  File,
                  (  " "
                  &  Image
                     (  ASN1.Floats.Get_Value
                        (  ASN1.Floats.Implicit_Real_Data_Item'Class
                           (  Item
               )  )  )  )  );
            else
               Put_Line
               (  File,
                  Ada.Tags.Expanded_Name (Item'Tag)
               );
            end if;
         when Relative_OID_Tag =>
            Put_Line
            (  File,
               (  " "
               &  Strings_Edit.Object_Identifiers.Image
                  (  ASN1.Object_Identifiers.Get_Value
                     (  ASN1.Object_Identifiers.
                        Implicit_External_Relative_OID_Data_Item'Class
                        (  Item
            )  )  )  )  );
         when Time_Tag =>
            Put_Line
            (  File,
               (  " "
               &  To_String
                  (  ASN1.Dates.Get_Time
                     (  ASN1.Dates.Public_Time_Data_Item
                        (  Item
            )  )  )  )  );
         when Time_Of_Day_Tag =>
            Put_Line
            (  File,
               (  " "
               &  To_String
                  (  ASN1.Dates.Get_Duration
                     (  ASN1.Dates.Public_Duration_Data_Item
                        (  Item
            )  )  )  )  );
         when UTC_Time_Tag =>
            Put_Line
            (  File,
               (  " "
               &  To_String
                  (  ASN1.Dates.Get_Time
                     (  ASN1.Dates.Public_Time_Data_Item (Item)
            )  )  )  );
         when UTF8_String_Tag      |
              Printable_String_Tag |
              Teletext_String_Tag  |
              Videotext_String_Tag |
              IA5_String_Tag       |
              Graphic_String_Tag   |
              ISO646_String_Tag    |
              General_String_Tag   |
              Universal_String_Tag |
              Character_String_Tag |
              Numeric_String_Tag   |
              BMP_String_Tag       =>
            if Item in ASN1.Strings.Implicit.
                       Implicit_External_String_Data_Item'Class then
               Dump_String
               (  ASN1.Strings.Implicit.Get_Value
                  (  ASN1.Strings.Implicit.
                     Implicit_External_String_Data_Item'Class
                     (  Item
               )  )  );
            elsif Item in ASN1.Strings.
                       Public_String_Data_Item'Class then
               Dump_String
               (  ASN1.Strings.Get_Value
                  (  ASN1.Strings.Public_String_Data_Item'Class
                     (  Item
               )  )  );
            else
               Put_Line
               (  File,
                  Ada.Tags.Expanded_Name (Item'Tag)
               );
            end if;
         when others =>
            Put_Line
            (  File,
               Ada.Tags.Expanded_Name (Item'Tag)
            );
      end case;
   end Put;

end GNAT.Sockets.Connection_State_Machine.ASN1.Text_IO;
