--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2016, Michael Rohan <mrohan@zanyblue.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--    * Neither the name of ZanyBlue nor the names of its contributors may
--      be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

with Ada.Exceptions;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Wide_Text_IO.Text_Streams;
with ZanyBlue.OS;
with ZanyBlue.Text.Utils;

package body ZanyBlue.Text.Properties_Parser is

   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use Ada.Characters.Latin_1;
   use Ada.Characters.Handling;
   use Ada.Wide_Text_IO.Text_Streams;
   use ZanyBlue.OS;
   use ZanyBlue.Text.Utils;

   ------------------
   -- Get_N_Errors --
   ------------------

   function Get_N_Errors (Handler : Parser_Handler_Type) return Natural is
   begin
      return Handler.N_Errors;
   end Get_N_Errors;

   --------------------
   -- Get_N_Messages --
   --------------------

   function Get_N_Messages (Handler : Parser_Handler_Type) return Natural is
   begin
      return Handler.N_Messages;
   end Get_N_Messages;

   ----------------------
   -- Increment_Errors --
   ----------------------

   procedure Increment_Errors (Handler   : in out Parser_Handler_Type;
                               By_Amount : Natural := 1) is
   begin
      Handler.N_Errors := Handler.N_Errors + By_Amount;
   end Increment_Errors;

   ------------------------
   -- Increment_Messages --
   ------------------------

   procedure Increment_Messages (Handler : in out Parser_Handler_Type) is
   begin
      Handler.N_Messages := Handler.N_Messages + 1;
   end Increment_Messages;

   -----------
   -- Parse --
   -----------

   procedure Parse (Handler          : in out Parser_Handler_Type'Class;
                    File_Name        : Wide_String;
                    Facility         : Wide_String;
                    Locale           : Locale_Type) is
   begin
      Parse (Handler, File_Name, Facility, Locale, Locale);
   end Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse (Handler       : in out Parser_Handler_Type'Class;
                    File_Name     : Wide_String;
                    Facility      : Wide_String;
                    Locale        : Locale_Type;
                    Source_Locale : Locale_Type) is
      Source_File   : File_Type;
      Opened        : Boolean := False;
   begin
      Wide_Open (Source_File, In_File, File_Name);
      Opened := True;
      Parse (Handler, Source_File, File_Name, Facility, Locale, Source_Locale);
      Close (Source_File);
   exception
   when others =>
      if Opened then
         Close (Source_File);
      end if;
      raise;
   end Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse (Handler          : in out Parser_Handler_Type'Class;
                    Source_File      : in out File_Type;
                    File_Name        : Wide_String;
                    Facility         : Wide_String;
                    Locale           : Locale_Type) is
   begin
      Parse (Handler, Source_File, File_Name, Facility, Locale, Locale);
   end Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse (Handler       : in out Parser_Handler_Type'Class;
                    Source_File   : in out File_Type;
                    File_Name     : Wide_String;
                    Facility      : Wide_String;
                    Locale        : Locale_Type;
                    Source_Locale : Locale_Type) is

      package Key_To_Line_Maps is
         new Ada.Containers.Indefinite_Hashed_Maps
            (Key_Type        => String,
             Element_Type    => Natural,
             Equivalent_Keys => "=",
             Hash            => Ada.Strings.Hash);

      Key_Definitions : Key_To_Line_Maps.Map;
      Source_Stream   : Stream_Access;
      Cur_Character   : Character;
      Cur_Key         : Unbounded_String;
      Cur_Value       : Unbounded_String;
      Cur_Line        : Natural := 1;
      Def_Line        : Positive := 1;

      procedure Kill_Line;
      --  Kill, ignore, a line when an error has been encountered.

      procedure New_Message (Key   : String;
                             Value : String);
      --  Perform the call back to define a new key/value pair.

      function Next (Require_ISO_646 : Boolean := True) return Character;
      --  Return the next character from the input stream.

      ---------------
      -- Kill_Line --
      ---------------

      procedure Kill_Line is
      begin
         Line_Characters : loop
            case Next (Require_ISO_646 => False) is
            when LF | CR | EOT =>
               exit Line_Characters;
            when others =>
               null;
            end case;
         end loop Line_Characters;
      end Kill_Line;

      -----------------
      -- New_Message --
      -----------------

      procedure New_Message (Key   : String;
                             Value : String) is

         use Key_To_Line_Maps;

         Position : constant Cursor := Find (Key_Definitions, Key);
         WKey : constant Wide_String := To_Wide_String (Key);
      begin
         if Position /= No_Element then
            Handler.Increment_Errors;
            Handler.Duplicate_Key (Facility, WKey, Locale, File_Name,
                                   Def_Line, Element (Position));
         end if;
         Handler.Increment_Messages;
         Handler.Add_Key_Value (Facility, WKey, Unescape_String (Value),
                                Locale, Source_Locale, File_Name, Def_Line);
         Include (Key_Definitions, Key, Def_Line);
      end New_Message;

      ----------
      -- Next --
      ----------

      function Next (Require_ISO_646 : Boolean := True) return Character is
      begin
         if not End_Of_File (Source_File) then
            Character'Read (Source_Stream, Cur_Character);
            if Require_ISO_646 and then not Is_ISO_646 (Cur_Character) then
               Handler.Increment_Errors;
               Handler.Invalid_Character (Facility, File_Name, Cur_Line,
                                          Cur_Character);
            end if;
         else
            Cur_Character := EOT;
         end if;
         if Cur_Character = LF then
            Cur_Line := Cur_Line + 1;
         end if;
         return Cur_Character;
      end Next;

   begin
      Source_Stream := Stream (Source_File);

   <<Start>>
      case Next is
      when EOT =>
         goto EOF;
      when Space | HT | VT | FF =>
         goto Start;
      when LF | CR =>
         goto Start;
      when '#' =>
         goto Comment;
      when others =>
         Cur_Key := To_Unbounded_String ("" & Cur_Character);
         Cur_Value := To_Unbounded_String ("");
         goto Key;
      end case;

   <<Key>>
      Def_Line := Cur_Line;
      case Next is
      when '\' =>
         goto Key_Escape;
      when CR | LF | EOT =>
         goto Finish;
      when Space | HT | VT | FF =>
         goto Key_End_1;
      when ':' | '=' =>
         goto Key_End_2;
      when others =>
         Append (Cur_Key, Cur_Character);
         goto Key;
      end case;

   <<Key_Escape>>
      case Next is
      when 'r' =>
         Append (Cur_Key, CR);
         goto Key;
      when 'n' =>
         Append (Cur_Key, LF);
         goto Key;
      when others =>
         Append (Cur_Key, Cur_Character);
         goto Key;
      end case;

   <<Key_End_1>>
      case Next is
      when LF | CR | EOT =>
         goto Finish;
      when Space | HT | VT | FF =>
         goto Key_End_1;
      when '=' | ':' =>
         goto Key_End_2;
      when others =>
         goto Value;
      end case;

   <<Key_End_2>>
      case Next is
      when LF | CR | EOT =>
         goto Finish;
      when Space | HT | VT | FF =>
         goto Key_End_2;
      when '\' =>
         goto Value_Escape;
      when others =>
         goto Value;
      end case;

   <<Value>>
      Append (Cur_Value, Cur_Character);
      case Next is
      when CR | LF | EOT =>
         goto Finish;
      when '\' =>
         goto Value_Escape;
      when others =>
         goto Value;
      end case;

   <<Value_Escape>>
      case Next is
      when CR =>
         goto Value_Escape_CR;
      when LF =>
         goto Value_Escape_LF;
      when others =>
         Append (Cur_Value, '\');
         goto Value;
      end case;

   <<Value_Escape_CR>>
      case Next is
      when LF | Space | HT =>
         goto Value_Escape_Space;
      when others =>
         goto Value;
      end case;

   <<Value_Escape_LF>>
      case Next is
      when CR | Space | HT =>
         goto Value_Escape_Space;
      when others =>
         goto Value;
      end case;

   <<Value_Escape_Space>>
      case Next is
      when Space | HT =>
         goto Value_Escape_Space;
      when LF | CR =>
         goto Finish;
      when others =>
         goto Value;
      end case;

   <<Comment>>
      Kill_Line;
      goto Start;

   <<Finish>>
      New_Message (To_String (Cur_Key), To_String (Cur_Value));
      goto Start;

   <<EOF>>
      return;

   exception
   when Error : Unicode_Format_Error =>
      --  Augment the exception with the file name ane line number
      Handler.Increment_Errors;
      Handler.Invalid_Definition (Facility, Locale, File_Name, Def_Line,
                                  Exception_Message (Error));
   end Parse;

   ----------------------
   -- Reset_N_Messages --
   ----------------------

   procedure Reset_N_Messages (Handler : in out Parser_Handler_Type) is
   begin
      Handler.N_Messages := 0;
   end Reset_N_Messages;

end ZanyBlue.Text.Properties_Parser;
