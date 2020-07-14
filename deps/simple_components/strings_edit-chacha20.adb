--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.ChaCha20                       Luebeck            --
--  Implementation                                 Summer, 2018       --
--                                                                    --
--                                Last revision :  11:26 29 May 2020  --
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

with Ada.Exceptions;   use Ada.Exceptions;

package body Strings_Edit.ChaCha20 is

--     function Image (Value : Unsigned_32) return String is
--        package Edit is new Strings_Edit.Integer_Edit (Integer_64);
--        use Edit;
--        Result  : String (1..8);
--        Pointer : Integer :=1;
--     begin
--        Put
--        (  Destination => Result,
--           Pointer     => Pointer,
--           Value       => Integer_64 (Value),
--           Base        => 16,
--           Field       => 8,
--           Justify     => Strings_Edit.Right,
--           Fill        => '0'
--        );
--        return Result;
--     end Image;
--
--     procedure Dump (Text : String; Block : Block_Type) is
--     begin
--        Ada.Text_IO.Put_Line (Text);
--        Ada.Text_IO.Put_Line
--        (  Image (Block (0)) & " "
--        &  Image (Block (1)) & " "
--        &  Image (Block (2)) & " "
--        &  Image (Block (3))
--        );
--        Ada.Text_IO.Put_Line
--        (  Image (Block (4)) & " "
--        &  Image (Block (5)) & " "
--        &  Image (Block (6)) & " "
--        &  Image (Block (7))
--        );
--        Ada.Text_IO.Put_Line
--        (  Image (Block (8)) & " "
--        &  Image (Block (9)) & " "
--        &  Image (Block (10)) & " "
--        &  Image (Block (11))
--        );
--        Ada.Text_IO.Put_Line
--        (  Image (Block (12)) & " "
--        &  Image (Block (13)) & " "
--        &  Image (Block (14)) & " "
--        &  Image (Block (15))
--        );
--     end Dump;

   function "+" (Data : Stream_Element_Array) return Unsigned_32 is
   begin
      return
      (  Unsigned_32 (Data (Data'First    ))
      +  Unsigned_32 (Data (Data'First + 1)) * 2**8
      +  Unsigned_32 (Data (Data'First + 2)) * 2**16
      +  Unsigned_32 (Data (Data'First + 3)) * 2**24
      );
   end "+";

   procedure Generate (Cipher : in out ChaCha20_Cipher);

   procedure Quarter_Round (A, B, C, D : in out Unsigned_32);
   pragma Inline (Generate, Quarter_Round);

   procedure Quarter_Round (A, B, C, D : in out Unsigned_32) is
   begin
      A := A + B; D := Rotate_Left (D xor A, 16);
      C := C + D; B := Rotate_Left (B xor C, 12);
      A := A + B; D := Rotate_Left (D xor A,  8);
      C := C + D; B := Rotate_Left (B xor C,  7);
   end Quarter_Round;

   procedure Generate (Cipher : in out ChaCha20_Cipher) is
      Stream : Stream_Element_Array renames Cipher.Stream;
      Offset : Stream_Element_Count := 0;
      Block  : Block_Type := Cipher.State;
   begin -- 1
      Quarter_Round (Block (0), Block (4), Block (8),  Block (12));
      Quarter_Round (Block (1), Block (5), Block (9),  Block (13));
      Quarter_Round (Block (2), Block (6), Block (10), Block (14));
      Quarter_Round (Block (3), Block (7), Block (11), Block (15));
      Quarter_Round (Block (0), Block (5), Block (10), Block (15));
      Quarter_Round (Block (1), Block (6), Block (11), Block (12));
      Quarter_Round (Block (2), Block (7), Block (8),  Block (13));
      Quarter_Round (Block (3), Block (4), Block (9),  Block (14));
         -- 2
      Quarter_Round (Block (0), Block (4), Block (8),  Block (12));
      Quarter_Round (Block (1), Block (5), Block (9),  Block (13));
      Quarter_Round (Block (2), Block (6), Block (10), Block (14));
      Quarter_Round (Block (3), Block (7), Block (11), Block (15));
      Quarter_Round (Block (0), Block (5), Block (10), Block (15));
      Quarter_Round (Block (1), Block (6), Block (11), Block (12));
      Quarter_Round (Block (2), Block (7), Block (8),  Block (13));
      Quarter_Round (Block (3), Block (4), Block (9),  Block (14));
         -- 3
      Quarter_Round (Block (0), Block (4), Block (8),  Block (12));
      Quarter_Round (Block (1), Block (5), Block (9),  Block (13));
      Quarter_Round (Block (2), Block (6), Block (10), Block (14));
      Quarter_Round (Block (3), Block (7), Block (11), Block (15));
      Quarter_Round (Block (0), Block (5), Block (10), Block (15));
      Quarter_Round (Block (1), Block (6), Block (11), Block (12));
      Quarter_Round (Block (2), Block (7), Block (8),  Block (13));
      Quarter_Round (Block (3), Block (4), Block (9),  Block (14));
         -- 4
      Quarter_Round (Block (0), Block (4), Block (8),  Block (12));
      Quarter_Round (Block (1), Block (5), Block (9),  Block (13));
      Quarter_Round (Block (2), Block (6), Block (10), Block (14));
      Quarter_Round (Block (3), Block (7), Block (11), Block (15));
      Quarter_Round (Block (0), Block (5), Block (10), Block (15));
      Quarter_Round (Block (1), Block (6), Block (11), Block (12));
      Quarter_Round (Block (2), Block (7), Block (8),  Block (13));
      Quarter_Round (Block (3), Block (4), Block (9),  Block (14));
         -- 5
      Quarter_Round (Block (0), Block (4), Block (8),  Block (12));
      Quarter_Round (Block (1), Block (5), Block (9),  Block (13));
      Quarter_Round (Block (2), Block (6), Block (10), Block (14));
      Quarter_Round (Block (3), Block (7), Block (11), Block (15));
      Quarter_Round (Block (0), Block (5), Block (10), Block (15));
      Quarter_Round (Block (1), Block (6), Block (11), Block (12));
      Quarter_Round (Block (2), Block (7), Block (8),  Block (13));
      Quarter_Round (Block (3), Block (4), Block (9),  Block (14));
         -- 6
      Quarter_Round (Block (0), Block (4), Block (8),  Block (12));
      Quarter_Round (Block (1), Block (5), Block (9),  Block (13));
      Quarter_Round (Block (2), Block (6), Block (10), Block (14));
      Quarter_Round (Block (3), Block (7), Block (11), Block (15));
      Quarter_Round (Block (0), Block (5), Block (10), Block (15));
      Quarter_Round (Block (1), Block (6), Block (11), Block (12));
      Quarter_Round (Block (2), Block (7), Block (8),  Block (13));
      Quarter_Round (Block (3), Block (4), Block (9),  Block (14));
         -- 7
      Quarter_Round (Block (0), Block (4), Block (8),  Block (12));
      Quarter_Round (Block (1), Block (5), Block (9),  Block (13));
      Quarter_Round (Block (2), Block (6), Block (10), Block (14));
      Quarter_Round (Block (3), Block (7), Block (11), Block (15));
      Quarter_Round (Block (0), Block (5), Block (10), Block (15));
      Quarter_Round (Block (1), Block (6), Block (11), Block (12));
      Quarter_Round (Block (2), Block (7), Block (8),  Block (13));
      Quarter_Round (Block (3), Block (4), Block (9),  Block (14));
         -- 8
      Quarter_Round (Block (0), Block (4), Block (8),  Block (12));
      Quarter_Round (Block (1), Block (5), Block (9),  Block (13));
      Quarter_Round (Block (2), Block (6), Block (10), Block (14));
      Quarter_Round (Block (3), Block (7), Block (11), Block (15));
      Quarter_Round (Block (0), Block (5), Block (10), Block (15));
      Quarter_Round (Block (1), Block (6), Block (11), Block (12));
      Quarter_Round (Block (2), Block (7), Block (8),  Block (13));
      Quarter_Round (Block (3), Block (4), Block (9),  Block (14));
         -- 9
      Quarter_Round (Block (0), Block (4), Block (8),  Block (12));
      Quarter_Round (Block (1), Block (5), Block (9),  Block (13));
      Quarter_Round (Block (2), Block (6), Block (10), Block (14));
      Quarter_Round (Block (3), Block (7), Block (11), Block (15));
      Quarter_Round (Block (0), Block (5), Block (10), Block (15));
      Quarter_Round (Block (1), Block (6), Block (11), Block (12));
      Quarter_Round (Block (2), Block (7), Block (8),  Block (13));
      Quarter_Round (Block (3), Block (4), Block (9),  Block (14));
         -- 10
      Quarter_Round (Block (0), Block (4), Block (8),  Block (12));
      Quarter_Round (Block (1), Block (5), Block (9),  Block (13));
      Quarter_Round (Block (2), Block (6), Block (10), Block (14));
      Quarter_Round (Block (3), Block (7), Block (11), Block (15));
      Quarter_Round (Block (0), Block (5), Block (10), Block (15));
      Quarter_Round (Block (1), Block (6), Block (11), Block (12));
      Quarter_Round (Block (2), Block (7), Block (8),  Block (13));
      Quarter_Round (Block (3), Block (4), Block (9),  Block (14));

      Offset := 0;
      for Index in Block'Range loop
         declare
            Data : Unsigned_32 := Block (Index) + Cipher.State (Index);
         begin
            Stream (Offset) := Stream_Element (Data mod 256);
            Offset := Offset + 1;
            Data   := Shift_Right (Data, 8);
            Stream (Offset) := Stream_Element (Data mod 256);
            Offset := Offset + 1;
            Data   := Shift_Right (Data, 8);
            Stream (Offset) := Stream_Element (Data mod 256);
            Offset := Offset + 1;
            Data   := Shift_Right (Data, 8);
            Stream (Offset) := Stream_Element (Data mod 256);
            Offset := Offset + 1;
         end;
      end loop;
      Cipher.State (12) := Cipher.State (12) + 1;
      Cipher.Count := 0;
   end Generate;

   function Decrypt
            (  Cipher : access ChaCha20_Cipher;
               Input  : Stream_Element_Array
            )  return Stream_Element_Array renames Encrypt;
   function Decrypt
            (  Cipher : access ChaCha20_Cipher;
               Input  : String
            )  return String renames Encrypt;
   procedure Decrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : Stream_Element_Array;
                Output : out Stream_Element_Array
             )  renames Encrypt;
   procedure Decrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : Stream_Element_Array;
                Output : out String
             )  renames Encrypt;
   procedure Decrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : String;
                Output : out String
             )  renames Encrypt;
   procedure Decrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : String;
                Output : out Stream_Element_Array
             )  renames Encrypt;
   procedure Decrypt
             (  Cipher : in out ChaCha20_Cipher;
                Data   : in out Stream_Element_Array
             )  renames Encrypt;
   procedure Decrypt
             (  Cipher : in out ChaCha20_Cipher;
                Data   : in out String
             )  renames Encrypt;

   function Encrypt
            (  Cipher : access ChaCha20_Cipher;
               Input  : Stream_Element_Array
            )  return Stream_Element_Array is
      Result : Stream_Element_Array (Input'Range);
   begin
      Encrypt (Cipher.all, Input, Result);
      return Result;
   end Encrypt;

   function Encrypt
            (  Cipher : access ChaCha20_Cipher;
               Input  : String
            )  return String is
      Result : String (Input'Range);
   begin
      Encrypt (Cipher.all, Input, Result);
      return Result;
   end Encrypt;

   procedure Encrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : Stream_Element_Array;
                Output : out Stream_Element_Array
             )  is
      Count : ChaCha20_Count renames Cipher.Count;
   begin
      if Input'Length /= Output'Length then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Different lengths of the input and output arrays"
         );
      end if;
      for Index in Input'Range loop
         if Cipher.Count = ChaCha20_Count'Last then
            Generate (Cipher);
         else
            Cipher.Count := Cipher.Count + 1;
         end if;
         Output (Index - Input'First + Output'First) :=
            Input (Index) xor Cipher.Stream (Cipher.Count);
      end loop;
   end Encrypt;

   procedure Encrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : String;
                Output : out String
             )  is
      Count : ChaCha20_Count renames Cipher.Count;
   begin
      if Input'Length /= Output'Length then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Different lengths of the input and output arrays"
         );
      end if;
      for Index in Input'Range loop
         if Cipher.Count = ChaCha20_Count'Last then
            Generate (Cipher);
         else
            Cipher.Count := Cipher.Count + 1;
         end if;
         Output (Index - Input'First + Output'First) :=
            Character'Val
            (  Character'Pos (Input (Index))
            xor
               Cipher.Stream (Cipher.Count)
            );
      end loop;
   end Encrypt;

   procedure Encrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : Stream_Element_Array;
                Output : out String
             )  is
      Count : ChaCha20_Count renames Cipher.Count;
   begin
      if Input'Length /= Output'Length then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Different lengths of the input and output arrays"
         );
      end if;
      for Index in Input'Range loop
         if Cipher.Count = ChaCha20_Count'Last then
            Generate (Cipher);
         else
            Cipher.Count := Cipher.Count + 1;
         end if;
         Output (Integer (Index - Input'First) + Output'First) :=
            Character'Val
            (  Input (Index)
            xor
               Cipher.Stream (Cipher.Count)
            );
      end loop;
   end Encrypt;

   procedure Encrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : String;
                Output : out Stream_Element_Array
             )  is
      Count : ChaCha20_Count renames Cipher.Count;
   begin
      if Input'Length /= Output'Length then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Different lengths of the input and output arrays"
         );
      end if;
      for Index in Input'Range loop
         if Cipher.Count = ChaCha20_Count'Last then
            Generate (Cipher);
         else
            Cipher.Count := Cipher.Count + 1;
         end if;
         Output
         (  Stream_Element_Offset (Index - Input'First)
         +  Output'First
         ) := (  Character'Pos (Input (Index))
              xor
                 Cipher.Stream (Cipher.Count)
              );
      end loop;
   end Encrypt;

   procedure Encrypt
             (  Cipher : in out ChaCha20_Cipher;
                Data   : in out Stream_Element_Array
             )  is
      Count : ChaCha20_Count renames Cipher.Count;
   begin
      for Index in Data'Range loop
         if Cipher.Count = ChaCha20_Count'Last then
            Generate (Cipher);
         else
            Cipher.Count := Cipher.Count + 1;
         end if;
         Data (Index) := Data (Index) xor Cipher.Stream (Cipher.Count);
      end loop;
   end Encrypt;

   procedure Encrypt
             (  Cipher : in out ChaCha20_Cipher;
                Data   : in out String
             )  is
      Count : ChaCha20_Count renames Cipher.Count;
   begin
      for Index in Data'Range loop
         if Cipher.Count = ChaCha20_Count'Last then
            Generate (Cipher);
         else
            Cipher.Count := Cipher.Count + 1;
         end if;
         Data (Index) :=
            Character'Val
            (  Character'Pos (Data (Index))
            xor
               Cipher.Stream (Cipher.Count)
            );
      end loop;
   end Encrypt;

   function Get_Count (Cipher : ChaCha20_Cipher) return Unsigned_32 is
   begin
      return Cipher.State (12);
   end Get_Count;

   function Get_Count (Stream : ChaCha20_Stream) return Unsigned_32 is
   begin
      return Get_Count (Stream.Cipher);
   end Get_Count;

   function Get_Key_Stream
            (  Cipher : access ChaCha20_Cipher;
               Full   : Boolean := False
            )  return Stream_Element_Array is
   begin
      if Full or else Cipher.Count = ChaCha20_Count'Last then
         Generate (Cipher.all);
         Cipher.Count := ChaCha20_Count'Last;
         return Cipher.Stream;
      else
         Cipher.Count := Cipher.Count + 1;
         return Cipher.Stream (Cipher.Count..Cipher.Stream'Last);
      end if;
   end Get_Key_Stream;

   function Get_Key_Stream
            (  Stream : access ChaCha20_Stream;
               Full   : Boolean := False
            )  return Stream_Element_Array is
   begin
      return Get_Key_Stream (Stream.Cipher'Access, Full);
   end Get_Key_Stream;

   procedure Read
             (  Stream : in out ChaCha20_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      Read (Stream.Transport.all, Item, Last);
      Decrypt (Stream.Cipher, Item (Item'First..Last));
   end Read;

   procedure Set_Key
             (  Cipher : in out ChaCha20_Cipher;
                Key    : ChaCha20_Key;
                Nonce  : ChaCha20_Nonce := (others => 0);
                Count  : Unsigned_32    := 0
             )  is
   begin
      Cipher.State (4)  := +Key ( 1.. 4);
      Cipher.State (5)  := +Key ( 5.. 8);
      Cipher.State (6)  := +Key ( 9..12);
      Cipher.State (7)  := +Key (13..16);
      Cipher.State (8)  := +Key (17..20);
      Cipher.State (9)  := +Key (21..24);
      Cipher.State (10) := +Key (25..28);
      Cipher.State (11) := +Key (29..32);
      Cipher.State (12) := Count;
      Cipher.State (13) := +Nonce (1.. 4);
      Cipher.State (14) := +Nonce (5.. 8);
      Cipher.State (15) := +Nonce (9..12);
      Cipher.Count := ChaCha20_Count'Last;
   end Set_Key;

   procedure Set_Key
             (  Stream : in out ChaCha20_Stream;
                Key    : ChaCha20_Key;
                Nonce  : ChaCha20_Nonce := (others => 0);
                Count  : Unsigned_32    := 0
             )  is
   begin
      Set_Key (Stream.Cipher, Key, Nonce, Count);
   end Set_Key;

   procedure Write
             (  Stream : in out ChaCha20_Stream;
                Item   : Stream_Element_Array
             )  is
      Pointer : Stream_Element_Offset := Item'First;
   begin
      while Pointer <= Item'Last loop
         declare
            Tail_Length : constant Stream_Element_Count :=
                                   Item'Last - Pointer + 1;
         begin
            if Tail_Length <= Stream.Buffer'Length then
               Encrypt
               (  Stream.Cipher,
                  Item (Pointer..Item'Last),
                  Stream.Buffer (1..Tail_Length)
               );
               Write
               (  Stream.Transport.all,
                  Stream.Buffer (1..Tail_Length)
               );
               return;
            end if;
         end;
         Encrypt
         (  Stream.Cipher,
            Item (Pointer..Pointer + Stream.Size - 1),
            Stream.Buffer
         );
         Write (Stream.Transport.all, Stream.Buffer);
         Pointer := Pointer + Stream.Size;
      end loop;
   end Write;

end Strings_Edit.ChaCha20;
