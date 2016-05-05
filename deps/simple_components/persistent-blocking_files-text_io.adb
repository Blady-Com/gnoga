--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Persistent.Blocking_Files.                 Luebeck            --
--         Text_IO                                Spring, 2014       --
--  Implementation                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Strings_Edit.Fields;    use Strings_Edit.Fields;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Strings_Edit.Integer_Edit;

package body Persistent.Blocking_Files.Text_IO is
   package Edit is new Strings_Edit.Integer_Edit (Block_Count);

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Byte_Index
             )  is
      Block : Block_Count;
      Index : Integer := Pointer;
   begin
      Edit.Get (Source, Index, Block);
      if Pointer > Source'Last then
         Value := Compose (Block + 1, 0);
      elsif Source (Index) = '.' then
         Index := Index + 1;
         declare
            Field  : Integer := Index;
            Offset : Natural;
         begin
            Get
            (  Source  => Source,
               Pointer => Index,
               Value   => Offset,
               Base    => 16,
               First   => 0
            );
            Field := Index - Field - (Byte_Offset_Bits + 3) / 4;
            if Field > 0 then
               Offset := Offset / 16 ** (Field);
            elsif Field < 0 then
               Offset := Offset * 16 ** (-Field);
            end if;
            Value := Compose (Block + 1, Block_Offset (Offset));
         exception
            when End_Error =>
               raise Data_Error;
         end;
      else
         Value := Compose (Block + 1, 0);
      end if;
      Pointer := Index;
   end Get;

   function Image (Value : Block_Count) return String is
   begin
      return Edit.Image (Value);
   end Image;

   function Image
            (  Value      : Byte_Index;
               Put_Offset : Boolean := True
            )  return String is
   begin
      if Put_Offset then
         return
         (  Edit.Image (Block_Count (Value / Block_Byte_Size))
         &  "."
         &  Image (Get_Offset (Value))
         );
      else
         return Edit.Image (Block_Count (Value / Block_Byte_Size));
      end if;
   end Image;

   function Image (Value : Block_Offset) return String is
      Result  : String (1..(Byte_Offset_Bits + 3) / 4);
      Pointer : Integer := Result'First;
   begin
      Put
      (  Destination => Result,
         Pointer     => Pointer,
         Value       => Integer (Value),
         Base        => 16,
         Field       => (Byte_Offset_Bits + 3) / 4,
         Fill        => '0',
         Justify     => Strings_Edit.Right
      );
      return Result;
   end Image;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Block_Count;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Edit.Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Value       => Value,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Byte_Index;
                Put_Offset  : Boolean   := True;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Out_Field : constant Natural :=
                  Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Text  : Output renames
              Destination (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;
   begin
      Edit.Put
      (  Destination => Text,
         Pointer     => Index,
         Value       => Block_Count (Value / Block_Byte_Size)
      );
      if Put_Offset then
         Put
         (  Destination => Text,
            Pointer     => Index,
            Value       => '.'
         );
         Put
         (  Destination => Text,
            Pointer     => Index,
            Value       => Get_Offset (Value)
         );
      end if;
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
                Value       : Block_Offset;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Out_Field : constant Natural :=
                  Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Text  : Output renames
              Destination (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;
   begin
      Put
      (  Destination => Text,
         Pointer     => Index,
         Value       => Integer (Value),
         Base        => 16,
         Field       => (Byte_Offset_Bits + 3) / 4,
         Fill        => '0',
         Justify     => Strings_Edit.Right
      );
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

   function Value (Source : String) return Byte_Index is
      Pointer : Integer := Source'First;
      Value   : Byte_Index;
   begin
      Get (Source, Pointer, SpaceAndTab);
      Get (Source, Pointer, Value);
      Get (Source, Pointer, SpaceAndTab);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Value;
   end Value;

end Persistent.Blocking_Files.Text_IO;
