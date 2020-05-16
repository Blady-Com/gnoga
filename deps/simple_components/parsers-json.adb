--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.JSON                                Luebeck            --
--  Implementation                                 Autumn, 2019       --
--                                                                    --
--                                Last revision :  13:13 14 Sep 2019  --
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

with Ada.IO_Exceptions;         use Ada.IO_Exceptions;
with Strings_Edit.Fields;       use Strings_Edit.Fields;
with Strings_Edit.Integers;     use Strings_Edit.Integers;
with Strings_Edit.Long_Floats;  use Strings_Edit.Long_Floats;
with Strings_Edit.UTF8;         use Strings_Edit.UTF8;

with Strings_Edit.UTF8.Categorization;

package body Parsers.JSON is

   function "and" (Left, Right : Operations) return Boolean is
   begin
      case Left is
         when Comma =>
            case Right is
               when Comma | Right_Brace | Right_Bracket =>
                  return True;
               when others =>
                  return False;
            end case;
         when Left_Brace =>
            case Right is
               when Colon | Comma | Right_Brace =>
                  return True;
               when others =>
                  return False;
            end case;
         when Left_Bracket =>
            case Right is
               when Comma | Right_Bracket =>
                  return True;
               when others =>
                  return False;
            end case;
         when others =>
           return False;
      end case;
   end "and";

   function Is_Commutative (Left, Right : Operations) return Boolean is
   begin
      return False;
   end Is_Commutative;

   function Is_Inverse (Operation : Operations) return Boolean is
   begin
      return False;
   end Is_Inverse;

   function Group_Inverse (Operation : Operations) return Operations is
   begin
      return Comma;
   end Group_Inverse;

   procedure Check_Spelling (Name : String) is
   begin
      null;
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

   function Image
            (  Value  : JSON_Sequence;
               Escape : Boolean := False
            )  return String is
      Size : Integer := 512;
   begin
      loop
         declare
            Text    : String (1..Size);
            Pointer : Integer := Text'First;
         begin
            Put (Text, Pointer, Value, Escape);
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               Size := (Size * 3) / 2;
         end;
      end loop;
   end Image;

   function Image
            (  Value  : JSON_Pair_Array;
               Escape : Boolean := False
            ) return String is
      Size : Integer := 512;
   begin
      loop
         declare
            Text    : String (1..Size);
            Pointer : Integer := Text'First;
         begin
            Put (Text, Pointer, Value, Escape);
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               Size := (Size * 3) / 2;
         end;
      end loop;
   end Image;

   function Image
            (  Value  : JSON_Value;
               Escape : Boolean := False
            ) return String is
      Size : Integer := 512;
   begin
      loop
         declare
            Text    : String (1..Size);
            Pointer : Integer := Text'First;
         begin
            Put (Text, Pointer, Value, Escape);
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               Size := (Size * 3) / 2;
         end;
      end loop;
   end Image;

   function Image
            (  Value  : String;
               Escape : Boolean := False
            )  return String is
      Size : Integer := 512;
   begin
      loop
         declare
            Text    : String (1..Size);
            Pointer : Integer := Text'First;
         begin
            Put (Text, Pointer, Value, Escape);
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               Size := (Size * 3) / 2;
         end;
      end loop;
   end Image;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : JSON_Sequence;
                Escape      : Boolean   := False;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '

             )  is
      Out_Field : constant Natural :=
         Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;
      Text  : Output renames
                     Destination (Pointer..Pointer + Out_Field - 1);
   begin
      Strings_Edit.Put (Text, Index, "[");
      for Item in Value'Range loop
         if Item /= Value'First then
            Strings_Edit.Put (Text, Index, ",");
         end if;
         Put (Text, Index, Value (Item), Escape);
      end loop;
      Strings_Edit.Put (Text, Index, "]");
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : JSON_Pair_Array;
                Escape      : Boolean   := False;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Out_Field : constant Natural :=
         Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;
      Text  : Output renames
                     Destination (Pointer..Pointer + Out_Field - 1);
   begin
      Strings_Edit.Put (Text, Index, "{");
      for Item in Value'Range loop
         if Item /= Value'First then
            Strings_Edit.Put (Text, Index, ",");
         end if;
         if Value (Item).Name = null then
            Strings_Edit.Put (Text, Index, """"":");
         else
            Strings_Edit.Put (Text, Index, """");
            Put (Text, Index, Value (Item).Name.all, Escape);
            Strings_Edit.Put (Text, Index, """:");
         end if;
         Put (Text, Index, Value (Item).Value, Escape);
      end loop;
      Strings_Edit.Put (Text, Index, "}");
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : JSON_Value;
                Escape      : Boolean   := False;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Out_Field : constant Natural :=
         Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;
      Text  : Output renames
                     Destination (Pointer..Pointer + Out_Field - 1);
   begin
      case Value.JSON_Type is
         when JSON_Boolean =>
            if Value.Condition then
               Strings_Edit.Put (Text, Index, "true");
            else
               Strings_Edit.Put (Text, Index, "false");
            end if;
         when JSON_Null =>
            Strings_Edit.Put (Text, Index, "null");
         when JSON_Number =>
            if Long_Float'Truncation (Value.Value) = Value.Value then
               Put (Text, Index, Value.Value, AbsSmall => 0);
            else
               Put (Text, Index, Value.Value);
            end if;
         when JSON_String =>
            Strings_Edit.Put (Text, Index, """");
            Parsers.JSON.Put (Text, Index, Value.Text.all, Escape);
            Strings_Edit.Put (Text, Index, """");
         when JSON_Array =>
            Put (Text, Index, Value.Sequence.all, Escape);
         when JSON_Object =>
            Put (Text, Index, Value.Map.all, Escape);
      end case;
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : String;
                Escape      : Boolean   := False;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Out_Field : constant Natural :=
         Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      This  : UTF8_Code_Point;
      Index : Integer := Pointer;
      Start : Integer := Value'First;
      Next  : Integer := Value'First;
      Text  : Output renames
                     Destination (Pointer..Pointer + Out_Field - 1);
   begin
      while Start <= Value'Last loop
         Next := Start;
         Get (Value, Next, This);
         case This is
            when 8 =>
               Strings_Edit.Put (Text, Index, "\b");
            when 9 =>
               Strings_Edit.Put (Text, Index, "\t");
            when 10 =>
               Strings_Edit.Put (Text, Index, "\n");
            when 12 =>
               Strings_Edit.Put (Text, Index, "\f");
            when 13 =>
               Strings_Edit.Put (Text, Index, "\r");
            when 34 =>
               Strings_Edit.Put (Text, Index, "\""");
            when 92 =>
               Strings_Edit.Put (Text, Index, "\\");
            when 0..7 | 11 | 14..31 | 127 =>
               Strings_Edit.Put (Text, Index, "\u");
               Put
               (  Destination => Text,
                  Pointer     => Index,
                  Value       => Integer (This),
                  Base        => 16,
                  Field       => 4,
                  Fill        => '0',
                  Justify     => Right
               );
            when 32..33 | 35..91 | 93..126 =>
               Strings_Edit.Put
               (  Text,
                  Index,
                  Value (Start..Next - 1)
               );
            when others =>
               if Escape then
                  Strings_Edit.Put (Text, Index, "\u");
                  Put
                  (  Destination => Text,
                     Pointer     => Index,
                     Value       => Integer (This),
                     Base        => 16,
                     Field       => 4,
                     Fill        => '0',
                     Justify     => Right
                  );
               else
                  Strings_Edit.Put
                  (  Text,
                     Index,
                     Value (Start..Next - 1)
                  );
               end if;
         end case;
         Start := Next;
      end loop;
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   end Put;

end Parsers.JSON;
