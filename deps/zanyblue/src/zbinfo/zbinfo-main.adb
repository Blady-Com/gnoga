--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2016, Michael Rohan <mrohan@zanyblue.com>
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

with Ada.Calendar;
with ZanyBlue.Command_Line;
with ZanyBlue.Text.Formatting;
with ZBInfo.Dump_Locale;
with ZBInfo.Dump_Encoding;
with ZBInfo.List_Encodings;
with ZBInfo_Messages.ZBInfo_Exceptions;
with ZBInfo_Messages.ZBInfo_Prints;

procedure ZBInfo.Main is

   use Ada.Calendar;
   use ZanyBlue.Command_Line;
   use ZanyBlue.Text.Formatting;
   use ZBInfo_Messages.ZBInfo_Exceptions;
   use ZBInfo_Messages.ZBInfo_Prints;

   type Mode_Type is (None, Help, Locale_Info, Encoding_Info, Encoding_List);

   Usage_Error : exception;

   function Banner return Time;
   procedure Process_Command_Line
     (Mode            : out Mode_Type;
      Locale_Name     : out String;
      Locale          : out Locale_Type;
      Encoding_Name   : out String;
      Reverse_Mapping : out Boolean);
   procedure Trailer (Start_Time : Time);

   ------------
   -- Banner --
   ------------

   function Banner return Time is
      Start_Time : constant Time := Clock;
   begin
      Print_00001
        (+ZanyBlue.Version_Major, +ZanyBlue.Version_Minor,
         +ZanyBlue.Version_Patch, +ZanyBlue.Revision, +Start_Time);
      Print_00002 (+ZanyBlue.Copyright_Year);
      return Start_Time;
   end Banner;

   --------------------------
   -- Process_Command_Line --
   --------------------------

   procedure Process_Command_Line
     (Mode            : out Mode_Type;
      Locale_Name     : out String;
      Locale          : out Locale_Type;
      Encoding_Name   : out String;
      Reverse_Mapping : out Boolean)
   is

      procedure Handle_Argument
        (Value :        String;
         Index : in out Positive);

      procedure Set_Option_Value
        (Target :    out String;
         Index  : in out Positive);

      ---------------------
      -- Handle_Argument --
      ---------------------

      procedure Handle_Argument
        (Value :        String;
         Index : in out Positive)
      is
      begin
         if Value = "-h" or Value = "--help" then
            Mode := Help;
         elsif Value = "-r" or Value = "--reverse" then
            Reverse_Mapping := True;
         elsif Value = "--list-encodings" then
            Mode := Encoding_List;
         elsif Value = "--dump-locale" then
            Mode := Locale_Info;
            Set_Option_Value (Locale_Name, Index);
            Locale := Make_Locale (Locale_Name);
         elsif Value = "--dump-encoding" then
            Mode := Encoding_Info;
            Set_Option_Value (Encoding_Name, Index);
         else
            Raise_10001 (Usage_Error'Identity, +Value);
         end if;
         Index := Index + 1;
      end Handle_Argument;

      ----------------------
      -- Set_Option_Value --
      ----------------------

      procedure Set_Option_Value
        (Target :    out String;
         Index  : in out Positive)
      is
      begin
         if Index < Argument_Count then
            Index  := Index + 1;
            Target := Argument (Index);
         else
            Raise_10002 (Usage_Error'Identity, +Argument (Index));
         end if;
      end Set_Option_Value;

      Index : Positive := 1;

   begin
      Mode            := Locale_Info;
      Reverse_Mapping := False;
      Locale          := Current_Locale;
      Locale_Name     := Current_Locale.Locale_Name;
      while Index <= Argument_Count loop
         Handle_Argument (Argument (Index), Index);
      end loop;
   end Process_Command_Line;

   -------------
   -- Trailer --
   -------------

   procedure Trailer (Start_Time : Time) is
      Now     : constant Time     := Clock;
      Elapsed : constant Duration := Now - Start_Time;
   begin
      Print_00003 (+Now, +Elapsed);
   end Trailer;

   Start_Time      : constant Time := Banner;
   Locale_Name     : String;
   Locale          : Locale_Type;
   Encoding_Name   : String;
   Reverse_Mapping : Boolean;
   Mode            : Mode_Type;

begin
   Process_Command_Line
     (Mode, Locale_Name, Locale, Encoding_Name, Reverse_Mapping);
   case Mode is
      when Help =>
         Print_00004;
      when None | Locale_Info =>
         ZBInfo.Dump_Locale (Locale, Locale_Name);
      when Encoding_List =>
         ZBInfo.List_Encodings;
      when Encoding_Info =>
         ZBInfo.Dump_Encoding (Encoding_Name, Reverse_Mapping);
   end case;
   Trailer (Start_Time);
end ZBInfo.Main;
