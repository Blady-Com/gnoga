--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Universally_Unique_Identifiers.Edit         Luebeck            --
--  Implementation                                 Winter, 2021       --
--                                                                    --
--                                Last revision :  13:12 05 Jan 2021  --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Interfaces;         use Interfaces;

with Strings_Edit.Integers;

package body Universally_Unique_Identifiers.Edit is

   Figure : constant array (Byte range 0..15) of Character :=
               (  '0', '1', '2', '3',   '4', '5', '6', '7',
                  '8', '9', 'a', 'b',   'c', 'd', 'e', 'f'
               );

   function To_String (Source : String) return UUID_Value renames Value;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out UUID_Value
             )  is
   begin
      if Pointer < Source'First then
         raise Layout_Error;
      elsif Pointer > Source'Last then
         if Pointer - 1 > Source'Last then
            raise Layout_Error;
         else
            raise End_Error;
         end if;
      elsif Source'Last - Pointer < 35 then
         raise End_Error;
      end if;
      Value   := To_String (Source (Pointer..Pointer + 35));
      Pointer := Pointer + 36;
   end Get;

   function Image (Value : UUID_Value) return UUID_String is
   begin
      return
      (   1 => Figure (Shift_Right (Value ( 1), 4) and 16#0F#),
          2 => Figure (             Value ( 1)     and 16#0F#),
          3 => Figure (Shift_Right (Value ( 2), 4) and 16#0F#),
          4 => Figure (             Value ( 2)     and 16#0F#),
          5 => Figure (Shift_Right (Value ( 3), 4) and 16#0F#),
          6 => Figure (             Value ( 3)     and 16#0F#),
          7 => Figure (Shift_Right (Value ( 4), 4) and 16#0F#),
          8 => Figure (             Value ( 4)     and 16#0F#),
          9 => '-',
         10 => Figure (Shift_Right (Value ( 5), 4) and 16#0F#),
         11 => Figure (             Value ( 5)     and 16#0F#),
         12 => Figure (Shift_Right (Value ( 6), 4) and 16#0F#),
         13 => Figure (             Value ( 6)     and 16#0F#),
         14 => '-',
         15 => Figure (Shift_Right (Value ( 7), 4) and 16#0F#),
         16 => Figure (             Value ( 7)     and 16#0F#),
         17 => Figure (Shift_Right (Value ( 8), 4) and 16#0F#),
         18 => Figure (             Value ( 8)     and 16#0F#),
         19 => '-',
         20 => Figure (Shift_Right (Value ( 9), 4) and 16#0F#),
         21 => Figure (             Value ( 9)     and 16#0F#),
         22 => Figure (Shift_Right (Value (10), 4) and 16#0F#),
         23 => Figure (             Value (10)     and 16#0F#),
         24 => '-',
         25 => Figure (Shift_Right (Value (11), 4) and 16#0F#),
         26 => Figure (             Value (11)     and 16#0F#),
         27 => Figure (Shift_Right (Value (12), 4) and 16#0F#),
         28 => Figure (             Value (12)     and 16#0F#),
         29 => Figure (Shift_Right (Value (13), 4) and 16#0F#),
         30 => Figure (             Value (13)     and 16#0F#),
         31 => Figure (Shift_Right (Value (14), 4) and 16#0F#),
         32 => Figure (             Value (14)     and 16#0F#),
         33 => Figure (Shift_Right (Value (15), 4) and 16#0F#),
         34 => Figure (             Value (15)     and 16#0F#),
         35 => Figure (Shift_Right (Value (16), 4) and 16#0F#),
         36 => Figure (             Value (16)     and 16#0F#)
      );
   end Image;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : UUID_Value;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Value       => Image (Value),
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   function Value (Source : String) return UUID_Value is
      function Get (Pointer : Integer) return Byte is
      begin
         return Byte
                (  Strings_Edit.Integers.Value
                   (  Source => Source (Pointer..Pointer + 1),
                      Base  => 16
                )  );
      end Get;
   begin
      if (  Source'Length /= 36
         or else
            Source (Source'First + 8) /= '-'
         or else
            Source (Source'First + 13) /= '-'
         or else
            Source (Source'First + 18) /= '-'
         or else
            Source (Source'First + 23) /= '-'
         )  then
         raise Data_Error;
      end if;
      return
      (   1 => Get (Source'First     ),
          2 => Get (Source'First +  2),
          3 => Get (Source'First +  4),
          4 => Get (Source'First +  6),

          5 => Get (Source'First +  9),
          6 => Get (Source'First + 11),

          7 => Get (Source'First + 14),
          8 => Get (Source'First + 16),

          9 => Get (Source'First + 19),
         10 => Get (Source'First + 21),

         11 => Get (Source'First + 24),
         12 => Get (Source'First + 26),
         13 => Get (Source'First + 28),
         14 => Get (Source'First + 30),
         15 => Get (Source'First + 32),
         16 => Get (Source'First + 34)
      );
   end Value;

end Universally_Unique_Identifiers.Edit;
