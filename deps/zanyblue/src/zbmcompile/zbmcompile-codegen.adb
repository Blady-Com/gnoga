--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, Michael Rohan <mrohan@zanyblue.com>
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

with Ada.Strings.Wide_Fixed;
with Ada.Containers.Vectors;
with Ada.Strings.Wide_Unbounded;
with Ada.Wide_Characters.Unicode;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;

package body ZBMCompile.Codegen is

   use ZanyBlue;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Formatting;

   Continue_Character : constant Wide_Character := '⤶';        --  U+2936
   --  U+2936 ARROW POINTING DOWNWARDS THEN CURVING LEFTWARDS
   --  Used to indicate lines of the accessor comment that have been split
   --  to ensure the generated output is within 80 column limit.

   Newline_Character  : constant Wide_Character := '⏎';
   --  U+23CE RETURN SYMBOL
   --  Used to indicate lines of the accessor comment that have embedded
   --  new line characters.  To preserve the intended structure, this
   --  character is always followed by a new line on output to the accessor
   --  comment.

   ------------------
   -- Modes_String --
   ------------------

   function Modes_String (Options : in Parameter_Set_Type) return Wide_String
   is
   begin
      if Options.Get_Boolean ("parameter_modes") then
         return " in ";
      else
         return " ";
      end if;
   end Modes_String;

   --------------
   -- Optimize --
   --------------

   function Optimize (Catalog : in Catalog_Type;
                      Options : in Parameter_Set_Type) return Catalog_Type is

      type Message_Record_Type is
         record
            Facility      : Facility_Index_Type;
            Key           : Key_Index_Type;
            Locale        : Locale_Index_Type;
            Source_Locale : Locale_Index_Type;
            Length        : Natural;
         end record;

      function "<" (Left, Right : in Message_Record_Type) return Boolean;

      package Message_Vectors is
         new Ada.Containers.Vectors (Index_Type   => Positive,
                                     Element_Type => Message_Record_Type);

      use Message_Vectors;

      package Message_Sorting is new Generic_Sorting;

      procedure Add_Message (Message : in Message_Record_Type);

      procedure Iteration_Handler (Facility      : in Facility_Index_Type;
                                   Key           : in Key_Index_Type;
                                   Locale        : in Locale_Index_Type;
                                   Source_Locale : in Locale_Index_Type;
                                   First         : in Positive;
                                   Last          : in Natural;
                                   Count         : in Natural);

      procedure Summarize_Locale (Catalog         : in Catalog_Type;
                                  Previous_Locale : in out Natural;
                                  Message_Count   : in out Natural;
                                  New_Locale      : in Natural);

      Result            : constant Catalog_Type := Create;
      Messages          : Message_Vectors.Vector;
      Previous_Locale   : Natural := 0;
      Message_Count     : Natural := 0;

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : in Message_Record_Type) return Boolean is
      begin
         if Left.Locale /= Right.Locale then
            return Left.Locale < Right.Locale;
         else
            return Left.Length > Right.Length;
         end if;
      end "<";

      -----------------
      -- Add_Message --
      -----------------

      procedure Add_Message (Message : in Message_Record_Type) is
         Facility           : constant Wide_String :=
                                 Get_Facility (Catalog, Message.Facility);
         Key                : constant Wide_String :=
                                 Get_Key (Catalog, Message.Key);
         Text               : constant Wide_String :=
                                 Get_Text (Catalog,
                                           Message.Facility,
                                           Message.Key,
                                           Message.Locale);
         Locale             : constant Locale_Type :=
                                 Get_Locale (Catalog, Message.Locale);
         Source_Locale   : constant Locale_Type :=
                                 Get_Locale (Catalog,
                                             Message.Source_Locale);
      begin
         Summarize_Locale (Result, Previous_Locale, Message_Count,
                           Message.Locale);
         Add (Result, Facility, Key, Text, Locale, Source_Locale);
      end Add_Message;

      -----------------------
      -- Iteration_Handler --
      -----------------------

      procedure Iteration_Handler (Facility      : in Facility_Index_Type;
                                   Key           : in Key_Index_Type;
                                   Locale        : in Locale_Index_Type;
                                   Source_Locale : in Locale_Index_Type;
                                   First         : in Positive;
                                   Last          : in Natural;
                                   Count         : in Natural) is

         pragma Unreferenced (Count);

         Message_Length : constant Natural := Natural'Max (0, Last - First);
         New_Record : constant Message_Record_Type := (
                                    Facility      => Facility,
                                    Key           => Key,
                                    Locale        => Locale,
                                    Source_Locale => Source_Locale,
                                    Length        => Message_Length);
      begin
         Messages.Append (New_Record);
      end Iteration_Handler;

      ----------------------
      -- Summarize_Locale --
      ----------------------

      procedure Summarize_Locale (Catalog         : in Catalog_Type;
                                  Previous_Locale : in out Natural;
                                  Message_Count   : in out Natural;
                                  New_Locale      : in Natural) is
      begin
         if Previous_Locale /= New_Locale then
            if Previous_Locale /= 0 then
               Print_Line (ZBMCompile_Facility, "I00005",
                           Argument0 => +Locale_Name (Get_Locale (Catalog,
                                                         Previous_Locale)),
                           Argument1 => +Message_Count);
            end if;
            Previous_Locale := New_Locale;
            Message_Count := 0;
         end if;
         Message_Count := Message_Count + 1;
      end Summarize_Locale;

   begin    --  Optimize
      Print_Line (ZBMCompile_Facility, "V00011");
      Use_Single_Pool (Result);
      Reserve (Result, Pool_Size => Pool_Size (Catalog),
                       Messages  => Number_Of_Messages (Catalog));
      Iterate (Catalog, Iteration_Handler'Access);
      Message_Sorting.Sort (Messages);
      for I in 1 .. Length (Messages) loop
         Add_Message (Messages.Element (Integer (I)));
      end loop;
      Summarize_Locale (Result, Previous_Locale, Message_Count, 0);
      if Options.Get_Boolean ("debug") then
         Print_Line ("zbmcompile", "D00001");
         Dump (Result, "optimized.dump");
      end if;
      return Result;
   end Optimize;

   --------------
   -- Sanitize --
   --------------

   function Sanitize (Value : in Wide_String) return Wide_String is
      use Ada.Strings.Wide_Unbounded;
      use Ada.Wide_Characters.Unicode;
      Buffer : Unbounded_Wide_String;
      Code   : Natural;
   begin
      for I in Value'Range loop
         if Is_Non_Graphic (Value (I)) then
            Code := Wide_Character'Pos (Value (I));
            case Code is
            when 0 .. 9 | 11 .. 31 =>
               --  Regular control characters: display as Unicode equivalent
               Append (Buffer, Wide_Character'Val (16#2400# + Code));
            when 10 =>
               --  New line characters are OK
               Append (Buffer, Value (I));
            when 127 =>
               --  Delete character, display as the Unicode DEL character
               Append (Buffer, Wide_Character'Val (16#2421#));
            when others =>
               --  Some other non-graphic character, display APL question
               Append (Buffer, Wide_Character'Val (16#2370#));
            end case;
         else
            Append (Buffer, Value (I));
         end if;
      end loop;
      return To_Wide_String (Buffer);
   end Sanitize;

   --------------------------
   -- Write_Commented_Text --
   --------------------------

   procedure Write_Commented_Text (File       : in File_Type;
                                   Value      : in Wide_String;
                                   Block_Size : in Positive) is
      use Ada.Strings.Wide_Fixed;
      NL_String  : constant Wide_String (1 .. 1)
                              := (others => Wide_Character'Val (10));
      Safe_Value : constant Wide_String := Sanitize (Value);
      Last       : constant Natural := Safe_Value'Last;
      Length     : constant Natural := Safe_Value'Length;
      From       : Positive := Safe_Value'First;
      To         : Natural;
   begin
      Comment_Blocks : loop
         To := Index (Safe_Value, NL_String, From);
         exit Comment_Blocks when To = 0;
         Write_Commented_Text_Line (File,
                                    Safe_Value (From .. To - 1)
                                    & Newline_Character,
                                    Block_Size);
         From := To + 1;
      end loop Comment_Blocks;
      if From < Last then
         Write_Commented_Text_Line (File, Safe_Value (From .. Last),
                                    Block_Size);
      elsif Length = 0 then
         Write_Commented_Text_Line (File, Safe_Value, Block_Size);
      end if;
   end Write_Commented_Text;

   -------------------------------
   -- Write_Commented_Text_Line --
   -------------------------------

   procedure Write_Commented_Text_Line (File       : in File_Type;
                                        Value      : in Wide_String;
                                        Block_Size : in Positive) is
      use Ada.Wide_Characters.Unicode;
      N_Blocks : constant Natural := Value'Length / Block_Size;
      From, To : Natural;
   begin
      if Value'Length = 0 then
         Print_Line (File, ZBMBase_Facility, "10021");
      else
         for I in 1 .. N_Blocks loop
            From := Value'First + (I - 1) * Block_Size;
            To := From + Block_Size - 1;
            Print_Line (File, ZBMBase_Facility, "10023",
                              Argument0 => +Value (From .. To),
                              Argument1 => +Continue_Character);
         end loop;
         From := Value'First + N_Blocks * Block_Size;
         To := Natural'Min (From + Block_Size, Value'Last);
         while To >= Value'First
           and then (Is_Space (Value (To)) and then To >= From)
         loop
            To := To - 1;
         end loop;
         if To >= From then
            Print_Line (File, ZBMBase_Facility, "10024",
                              Argument0 => +Value (From .. To));
         end if;
      end if;
   end Write_Commented_Text_Line;

end ZBMCompile.Codegen;
