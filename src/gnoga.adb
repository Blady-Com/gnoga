------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                                G N O G A                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Gnoga is

   -------------------
   -- Escape_Quotes --
   -------------------

   function Escape_Quotes (S : String) return String is
   begin
      if S'Length = 0 then
         return "";
      else
         declare
            New_Char : Character := S (S'First);
         begin
            if New_Char = '"' then
               return "\x22"
                 & Escape_Quotes (S (S'First + 1 .. S'Last));
            elsif New_Char = Character'Val (10) then
               return "\x0A"
                 & Escape_Quotes (S (S'First + 1 .. S'Last));
            elsif New_Char = Character'Val (13) then
               return "\x0D"
                 & Escape_Quotes (S (S'First + 1 .. S'Last));
            else
               return New_Char
                 & Escape_Quotes (S (S'First + 1 .. S'Last));
            end if;
         end;
      end if;
   end Escape_Quotes;

   ---------------
   -- Left_Trim --
   ---------------

   function Left_Trim (S : String) return String is
   begin
      if S'Length = 0 then
         return S;
      end if;

      if S (S'First) = ' ' or S (S'First) = Character'Val (9) then
         return Left_Trim (S ((S'First + 1) .. S'Last));
      else
         return S;
      end if;
   end Left_Trim;

   ----------------
   -- Right_Trim --
   ----------------

   function Right_Trim (S : String) return String is
   begin
      if S'Length = 0 then
         return S;
      end if;

      if S (S'Last) = ' ' or S (S'Last) = Character'Val (9)
      then
         return Right_Trim (S (S'First .. (S'Last - 1)));
      else
         return S;
      end if;
   end Right_Trim;

   -----------------------
   -- Left_Trim_Slashes --
   -----------------------

   function Left_Trim_Slashes (S : String) return String is
   begin
      if S'Length = 0 then
         return S;
      end if;

      if
        S (S'First) = ' ' or
        S (S'First) = Character'Val (9) or
        S (S'First) = '/'
      then
         return Left_Trim_Slashes (S ((S'First + 1) .. S'Last));
      else
         return S;
      end if;
   end Left_Trim_Slashes;

   ------------------------
   -- Right_Trim_Slashes --
   ------------------------

   function Right_Trim_Slashes (S : String) return String is
   begin
      if S'Length = 0 then
         return S;
      end if;

      if
        S (S'Last) = ' ' or
        S (S'Last) = Character'Val (9) or
        S (S'Last) = '/'
      then
         return Right_Trim_Slashes (S (S'First .. (S'Last - 1)));
      else
         return S;
      end if;
   end Right_Trim_Slashes;

   ----------------------
   -- Write_To_Console --
   ----------------------

   procedure Write_To_Console (Message : in String) is
   begin
      Ada.Text_IO.Put_Line (Message);
   end Write_To_Console;

   ---------
   -- Log --
   ---------

   procedure Log (Message : in String) is
   begin
      Ada.Text_IO.Put_Line (Message);
   end Log;

end Gnoga;
