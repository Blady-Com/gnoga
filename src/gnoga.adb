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

   Use_File : Boolean := False;
   Log_File : Ada.Text_IO.File_Type;

   -------------------
   -- Escape_Quotes --
   -------------------

   function Escape_Quotes (S : String) return String is
      use type Ada.Strings.Unbounded.Unbounded_String;

      function Translate_Character (C : Character) return String;

      function Translate_Character (C : Character) return String is
      begin
         if C = '"' then
            return "\x22";
         elsif C = ''' then
            return "\x27";
         elsif C = Character'Val (10) then
            return "\x0A";
         elsif C = Character'Val (13) then
            return "\x0D";
         else
            return C & "";
         end if;
      end Translate_Character;

      R : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for C in S'Range loop
         R := R & Translate_Character (S (C));
      end loop;

      return Ada.Strings.Unbounded.To_String (R);
   end Escape_Quotes;

   ---------------------
   -- Unescape_Quotes --
   ---------------------

   function Unescape_Quotes (S : String) return String is
      use type Ada.Strings.Unbounded.Unbounded_String;

      C : Integer := S'First;

      function Translate_Character return String;

      function Translate_Character return String is
      begin
         if C < S'Last - 1 then
            if S (C .. C + 1) = "\\" then
               C := C + 2;
               return "\";
            elsif S (C .. C + 1) = "\x" then
               declare
                  H : Integer := Integer'Value
                    ("16#" & S (C + 2 .. C + 3) & "#");
               begin
                  C := C + 4;

                  return Character'Val (H) & "";
               end;
            end if;
         end if;

         declare
            R : String := S (C) & "";
         begin
            C := C + 1;
            return R;
         end;
      end Translate_Character;

      R : Ada.Strings.Unbounded.Unbounded_String;
   begin
      loop
         R := R & Translate_Character;
         exit when C > S'Last;
      end loop;

      return Ada.Strings.Unbounded.To_String (R);
   end Unescape_Quotes;

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

      if S (S'Last) = ' ' or S (S'Last) = Character'Val (9) then
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

   --------------------
   -- String_Replace --
   --------------------

   procedure String_Replace
     (Source      : in out Ada.Strings.Unbounded.Unbounded_String;
      Pattern     : in     String;
      Replacement : in     String)
   is
      use Ada.Strings.Unbounded;

      I : Natural;
   begin
      loop
         I := Index (Source => Source, Pattern => Pattern);

         exit when I = 0;

         Replace_Slice (Source => Source,
                        Low    => I,
                        High   => I + Pattern'Length - 1,
                        By     => Replacement);
      end loop;
   end String_Replace;

   ----------------------
   -- Write_To_Console --
   ----------------------

   procedure Write_To_Console (Message : in String) is
   begin
      Ada.Text_IO.Put_Line (Message);
   end Write_To_Console;

   -----------------
   -- Log_To_File --
   -----------------

   procedure Log_To_File (File_Name : in String) is
      use Ada.Text_IO;
   begin
      Create (File => Log_File,
              Mode => Append_File,
              Name => File_Name);

      Use_File := True;
   exception
      when others =>
         Gnoga.Write_To_Console ("Failed to open log file " & File_Name);
   end Log_To_File;

   ---------
   -- Log --
   ---------

   procedure Log (Message : in String) is
   begin
      if Use_File then
         Ada.Text_IO.Put_Line (Log_File, Message);
      else
         Write_To_Console (Message);
      end if;
   end Log;

end Gnoga;
