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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Integer_Text_IO;
with Ada.Task_Termination;

with UXStrings.Text_IO;

package body Gnoga is

   Use_File        : Boolean := False;
   Automatic_Flush : Boolean := False;
   Log_File        : UXStrings.Text_IO.File_Type;

   -------------------
   -- Escape_Quotes --
   -------------------

   function Escape_Quotes
     (S : String)
      return String
   is

      function Translate_Character
        (C : Unicode_Character)
         return String;

      function Translate_Character
        (C : Unicode_Character)
         return String
      is
      begin
         if C = '"' then
            return "\x22";
         elsif C = ''' then
            return "\x27";
         elsif C = '\' then
            return "\x5C";
         elsif C = Unicode_Character'Val (10) then
            return "\x0A";
         elsif C = Unicode_Character'Val (13) then
            return "\x0D";
         else
            return From_Unicode (C);
         end if;
      end Translate_Character;

      R : String;
   begin
      for C in S loop
         Append (R, Translate_Character (S (C)));
      end loop;

      return R;
   end Escape_Quotes;

   ---------------------
   -- Unescape_Quotes --
   ---------------------

   function Unescape_Quotes
     (S : String)
      return String
   is

      C : Integer := S.First;

      function Translate_Character return String;

      function Translate_Character return String is
      begin
         if C < S.Last - 1 then
            if S.Slice (C, C + 1) = "\\" then
               C := C + 2;
               return "\";
            elsif S.Slice (C, C + 1) = "\x" then
               declare
                  H : constant Integer := Value (S.Slice (C + 2, C + 3), 16);
               begin
                  C := C + 4;

                  return From_Unicode (Unicode_Character'Val (H));
               end;
            end if;
         end if;

         declare
            R : constant String := S.Slice (C, C);
         begin
            C := C + 1;
            return R;
         end;
      end Translate_Character;

      R : String;
   begin
      loop
         Append (R, Translate_Character);
         exit when C > S.Last;
      end loop;

      return R;
   end Unescape_Quotes;

   ----------------
   -- URL_Encode --
   ----------------

   function URL_Encode
     (S        : String;
      Encoding : String := "")
      return String
   is

      function Translate_Character
        (C : Character)
         return String;

      function Translate_Character
        (C : Character)
         return String
      is
      begin
         if C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '*' | '_' then
            return From_ASCII (C);
         elsif C = ' ' then
            return "+";
         else
            declare
               V : ASCII_Character_Array (1 .. 6); -- 16#HH#
            begin
               Ada.Integer_Text_IO.Put (V, Character'Pos (C), 16);
               return From_ASCII ("%" & V (4 .. 5));
            end;
         end if;
      end Translate_Character;

      R : String;
      T : constant Latin_1_Character_Array :=
        (if Encoding = "UTF-8" then Latin_1_Character_Array (To_UTF_8 (S)) else To_Latin_1 (S));
   begin
      for C in 1 .. T'Length loop
         Append (R, Translate_Character (T (C)));
      end loop;

      return R;
   end URL_Encode;

   ----------------
   -- URL_Decode --
   ----------------

   function URL_Decode
     (S        : String;
      Encoding : String := "")
      return String
   is
      C : Integer := S.First;

      function Translate_Character return Unicode_Character;

      function Translate_Character return Unicode_Character is
         R : Unicode_Character := S (C);
      begin
         if R = '+' then
            R := ' ';
         elsif R = '%' and C < S.Last - 1 then
            R := Unicode_Character'Val (Value (S.Slice (C + 1, C + 2), 16));
            C := C + 2;
         end if;
         C := C + 1;
         return R;
      end Translate_Character;

      R : String;
   begin
      while C in S.First .. S.Last loop
         Append (R, Translate_Character);
      end loop;

      if Encoding = "UTF-8" then
         return From_UTF_8 (UTF_8_Character_Array (To_Latin_1 (R)));
      else
         return R;
      end if;
   end URL_Decode;

   ---------------
   -- Left_Trim --
   ---------------

   function Left_Trim
     (S : String)
      return String
   is
      Space : constant Unicode_Character := ' ';
      Tab   : constant Unicode_Character := Unicode_Character'Val (9);
   begin
      if S.Length = 0 then
         return S;
      end if;

      if S (S.First) = Space or S (S.First) = Tab then
         return Left_Trim (S.Slice ((S.First + 1), S.Last));
      else
         return S;
      end if;
   end Left_Trim;

   ----------------
   -- Right_Trim --
   ----------------

   function Right_Trim
     (S : String)
      return String
   is
      Space : constant Unicode_Character := ' ';
      Tab   : constant Unicode_Character := Unicode_Character'Val (9);
   begin
      if S.Length = 0 then
         return S;
      end if;

      if S (S.Last) = Space or S (S.Last) = Tab then
         return Right_Trim (S.Slice (S.First, (S.Last - 1)));
      else
         return S;
      end if;
   end Right_Trim;

   -----------------------
   -- Left_Trim_Slashes --
   -----------------------

   function Left_Trim_Slashes
     (S : String)
      return String
   is
      Space : constant Unicode_Character := ' ';
      Tab   : constant Unicode_Character := Unicode_Character'Val (9);
      Slash : constant Unicode_Character := '/';
   begin
      if S.Length = 0 then
         return S;
      end if;

      if S (S.First) = Space or S (S.First) = Tab or S (S.First) = Slash then
         return Left_Trim_Slashes (S.Slice ((S.First + 1), S.Last));
      else
         return S;
      end if;
   end Left_Trim_Slashes;

   ------------------------
   -- Right_Trim_Slashes --
   ------------------------

   function Right_Trim_Slashes
     (S : String)
      return String
   is
      Space : constant Unicode_Character := ' ';
      Tab   : constant Unicode_Character := Unicode_Character'Val (9);
      Slash : constant Unicode_Character := '/';
   begin
      if S.Length = 0 then
         return S;
      end if;

      if S (S.Last) = Space or S (S.Last) = Tab or S (S.Last) = Slash then
         return Right_Trim_Slashes (S.Slice (S.First, (S.Last - 1)));
      else
         return S;
      end if;
   end Right_Trim_Slashes;

   --------------------
   -- String_Replace --
   --------------------

   procedure String_Replace
     (Source      : in out String;
      Pattern     : in     String;
      Replacement : in     String)
   is

      I : Natural;
   begin
      loop
         I := Index (Source => Source, Pattern => Pattern);

         exit when I = 0;

         Replace_Slice (Source => Source, Low => I, High => I + Pattern.Length - 1, By => Replacement);
      end loop;
   end String_Replace;

   ----------------------
   -- Write_To_Console --
   ----------------------

   procedure Write_To_Console (Message : in String) is
   begin
      UXStrings.Text_IO.Put_Line (Message);
   end Write_To_Console;

   -----------------
   -- Log_To_File --
   -----------------

   procedure Log_To_File
     (File_Name  : in String;
      Flush_Auto : in Boolean := False)
   is
   begin
      UXStrings.Text_IO.Create
        (File   => Log_File, Mode => UXStrings.Text_IO.Append_File, Name => File_Name, Scheme => UTF_8,
         Ending => UXStrings.Text_IO.LF_Ending);

      Use_File        := True;
      Automatic_Flush := Flush_Auto;
   exception
      when E : others =>
         Log ("Error failed to open log file " & File_Name, E);
   end Log_To_File;

   ---------
   -- Log --
   ---------

   procedure Log (Message : in String) is
      T            : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Date_Message : constant String            :=
        From_ASCII
          (Ada.Calendar.Formatting.Image
             (Date => T, Include_Time_Fraction => True, Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset (T)) &
           " : ") &
        Message;
   begin
      if Use_File then
         UXStrings.Text_IO.Put_Line (Log_File, Date_Message);
         if Automatic_Flush then
            Flush_Log;
         end if;
      else
         Write_To_Console (Date_Message);
      end if;
   end Log;

   procedure Log
     (Message    : in String;
      Occurrence : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Log (Message & From_UTF_8 (Ada.Exceptions.Exception_Information (Occurrence)));
   end Log;

   procedure Log (Occurrence : in Ada.Exceptions.Exception_Occurrence) is
   begin
      Log (From_UTF_8 (Ada.Exceptions.Exception_Information (Occurrence)));
   end Log;

   ---------------
   -- Flush_Log --
   ---------------

   procedure Flush_Log is
   begin
      UXStrings.Text_IO.Flush (Log_File);
   end Flush_Log;

   --------------------------------
   -- Activate_Exception_Handler --
   --------------------------------

   protected Exception_Handler is
      procedure Log
        (Cause      : in Ada.Task_Termination.Cause_Of_Termination;
         Id         : in Ada.Task_Identification.Task_Id;
         Occurrence : in Ada.Exceptions.Exception_Occurrence);
   end Exception_Handler;

   protected body Exception_Handler is
      procedure Log
        (Cause      : in Ada.Task_Termination.Cause_Of_Termination;
         Id         : in Ada.Task_Identification.Task_Id;
         Occurrence : in Ada.Exceptions.Exception_Occurrence)
      is
         use all type Ada.Task_Termination.Cause_Of_Termination;
      begin
         case Cause is
            when Normal =>
               Log (From_UTF_8 ("Normal exit of task: " & Ada.Task_Identification.Image (Id)));
            when Abnormal =>
               Log (From_UTF_8 ("Abnormal exit of task: " & Ada.Task_Identification.Image (Id)));
            when Unhandled_Exception =>
               Log (From_UTF_8 ("Unhandled exception in task: " & Ada.Task_Identification.Image (Id)));
               Log (Occurrence);
         end case;
      end Log;
   end Exception_Handler;

   procedure Activate_Exception_Handler (Id : Ada.Task_Identification.Task_Id) is
   begin
      Ada.Task_Termination.Set_Specific_Handler (Id, Exception_Handler.Log'Access);
   end Activate_Exception_Handler;

begin
   --  Change the default to LF and UTF-8
   UXStrings.Text_IO.Ending (UXStrings.Text_IO.Current_Output, UXStrings.Text_IO.LF_Ending);
   UXStrings.Text_IO.Line_Mark (UXStrings.Text_IO.LF_Ending);
   UXStrings.Text_IO.Scheme (UXStrings.Text_IO.Current_Output, UTF_8);
end Gnoga;
