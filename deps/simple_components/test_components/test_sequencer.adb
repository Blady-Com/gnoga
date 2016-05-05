--                                                                    --
--  procedure Test_Sequencer        Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Winter, 2008       --
--                                                                    --
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
--
--  This is a small test procedure for the stack pool.
--
with Ada.Exceptions;  use Ada.Exceptions;

with Strings_Edit.Symmetric_Serialization;
use  Strings_Edit.Symmetric_Serialization;

with Test_Sequencer_Sets; use Test_Sequencer_Sets;

procedure Test_Sequencer is
   use Random_Items, Items_Sequence, Positions_Set;

begin
   -- Testing random sequences
   declare
      Dice   : Generator;
      Source : Sequence;
      Result : Set;
      This   : Item;
   begin
      for Element in Item'Range loop
         This := Next (Source, Random (Dice));
         if Is_In (Result, Item_Position (This)) then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Sequence error, item"
               &  Item'Image (This)
               &  " repeats at"
               &  Item'Image (Element)
            )  );
         end if;
      end loop;
   end;
   -- Testing string mangling
   declare
      Key      : constant String := "ABCDEF";
      Text     : constant String (1..256) :=
                 (  "1234567890123456789012345678901234567890"
                 &  "/ffffffff1ffffffff 2         3   fdsfet4"
                 &  "                                        "
                 &  "ffff/00000000000000-----------------89()"
                 &  "6666666666666666666666666666666666666666"
                 &  "ffff000000000!!!!!!!!!!!!!!!!$$%&/()=?\ "
                 &  "qwertzuiop123456"
                 );
      Stored   : constant Encoded_String := Encode (Text, Key);
      Restored : constant String         := Decode (Stored, Key);
   begin
      if Restored (1..Text'Length) /= Text then
         for Index in Text'Range loop
            if Restored (Index - Text'First + 1) /= Text (Index) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Encoding/Decoding error: at"
                  &  Integer'Image (Index - Text'First + 1)
                  &  " restored '"
                  &  Restored (Index - Text'First + 1)
                  &  "', expected '"
                  &  Text (Index)
                  &  "'"
               )  );
            end if;
         end loop;
      end if;
   end;
   declare
      Key      : constant String := "ABCDEF";
      Text     : constant String := "Test text";
      Stored   : constant Encoded_String := Encode (Text, Key);
      Restored : constant String         := Decode (Stored, Key);
   begin
      if Restored (1..Text'Length) /= Text then
         for Index in Text'Range loop
            if Restored (Index - Text'First + 1) /= Text (Index) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Encoding/Decoding error: at"
                  &  Integer'Image (Index - Text'First + 1)
                  &  " restored '"
                  &  Restored (Index - Text'First + 1)
                  &  "', expected '"
                  &  Text (Index)
                  &  "'"
               )  );
            end if;
         end loop;
      end if;
   end;
end Test_Sequencer;
