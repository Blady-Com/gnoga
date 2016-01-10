--  -*- encoding: utf-8 -*-
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

with Ada.Command_Line;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Version_Status_Arguments;
with Definitions;
with DL_Messages;

procedure X_DumpLocale is

   use Ada.Command_Line;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Formatting;
   use ZanyBlue.Text.Version_Status_Arguments;
   use Definitions.Day_Arguments;
   use Definitions.Era_Arguments;
   use Definitions.Month_Arguments;
   use Definitions.Hash_Type_Arguments;
   use Definitions.Day_Period_Arguments;
   use Definitions.Text_Layout_Arguments;
   use Definitions.Numeric_Item_Arguments;
   use Definitions.Numeric_Style_Arguments;
   use Definitions.Date_Time_Style_Arguments;

   type Mode_Type is (Normal, Help);

   Usage_Error : exception;

   procedure Display (Attribute  : String;
                      Value      : Argument_Type'Class;
                      Quoted     : Boolean := False);
   procedure Dump_Locale (Locale : Locale_Type);
   procedure Process_Command_Line (Mode : in out Mode_Type);

   procedure Display (Attribute  : String;
                      Value      : Argument_Type'Class;
                      Quoted     : Boolean := False) is
   begin
      if Quoted then
         Print_Line ("dl", "0004", +Attribute, Value);
      else
         Print_Line ("dl", "0003", +Attribute, Value);
      end if;
   end Display;

   procedure Dump_Locale (Locale : Locale_Type) is
   begin
      Print_Line ("dl", "0002", +Locale_Name (Locale));
      Display ("Language",       +Language (Locale), True);
      Display ("Script",         +Script (Locale), True);
      Display ("Territory",      +Territory (Locale), True);
      Display ("Name",           +Locale_Name (Locale), True);
      Display ("Traits Name",    +Traits_Name (Locale), True);
      Display ("Traits Tag",     +Traits_Tag (Locale), True);
      Display ("Locale Level",   +Integer (Locale_Level (Locale)));
      Display ("Is Root Locale", +Is_Root_Locale (Locale));
      Display ("Layout",         +Text_Layout (Locale));
      Display ("Lower Digits",   +Locale_Digits (Locale, True));
      Display ("Upper Digits",   +Locale_Digits (Locale, False));
      Display ("Hash Value",     +Hash (Locale));
      Print_Line ("dl", "0005");
      Print_Line ("dl", "0006");
      for Style in Date_Time_Style_Type loop
         Print_Line ("dl", "0016", +Style,
                                   +Date_Format (Locale, Style),
                                   +Time_Format (Locale, Style),
                                   +Date_Time_Format (Locale, Style));
      end loop;
      Print_Line ("dl", "0007");
      for Period in Day_Period_Type loop
         Print_Line ("dl", "0003", +Period, +Day_Period_Name (Locale, Period));
      end loop;
      Print_Line ("dl", "0018");
      for Hour in Integer range 0 .. 23 loop
         Print_Line ("dl", "0019",
            +Hour,
            +Day_Period_For_Time (Locale, Hour, 0, 0),
            +Day_Period_For_Time (Locale, Hour, 30, 0));
      end loop;
      Print_Line ("dl", "0008");
      for Era in Era_Type loop
         Print_Line ("dl", "0003", +Era, +Era_Name (Locale, Era));
      end loop;
      Print_Line ("dl", "0009");
      Print_Line ("dl", "0012");
      for Day in Day_Type loop
         Print_Line ("dl", "0011", +Day,
                                   +Short_Day_Name (Locale, Day),
                                   +Full_Day_Name (Locale, Day));
      end loop;
      Print_Line ("dl", "0010");
      Print_Line ("dl", "0012");
      for Month in Month_Type loop
         Print_Line ("dl", "0011", +Month,
                                   +Short_Month_Name (Locale, Month),
                                   +Full_Month_Name (Locale, Month));
      end loop;
      Print_Line ("dl", "0014");
      for Style in Numeric_Style_Type loop
         Print_Line ("dl", "0003", +Style, +Numeric_Format (Locale, Style));
      end loop;
      Print_Line ("dl", "0015");
      for Item in Numeric_Item_Type loop
         Print_Line ("dl", "0003", +Item, +Numeric_Item (Locale, Item));
      end loop;
   end Dump_Locale;

   procedure Process_Command_Line (Mode : in out Mode_Type) is
      use ZanyBlue.Text.Pseudo;
   begin
      for I in 1 .. Argument_Count loop
         declare
            Option : constant String := Argument (I);
         begin
            if Option = "-xh" or Option = "-x" then
               Pseudo_Translate (Halfwidth_Forms_Map);
            elsif Option = "-xe" then
               Pseudo_Translate (Enclosed_Alphanumeric_Map);
            elsif Option = "-xl" then
               Pseudo_Translate (Lowercase_Map);
            elsif Option = "-xu" then
               Pseudo_Translate (Uppercase_Map);
            elsif Option = "-xn" then
               null;
            elsif Option = "-h" then
               Mode := Help;
            elsif Option (1 .. 2) = "-l" then
               Set_Locale (Option (3 .. Option'Last));
            elsif Option (1) = '-' then
               raise Usage_Error;
            else
               Set_Locale (Option);
            end if;
         end;
      end loop;
   end Process_Command_Line;

   Mode : Mode_Type := Normal;

begin
   Print_Line ("dl", "0000", +ZanyBlue.Version_Major,
                             +ZanyBlue.Version_Minor,
                             +ZanyBlue.Version_Patch,
                             +ZanyBlue.Version_Status);
   Process_Command_Line (Mode);
   case Mode is
   when Normal =>
      Dump_Locale (Current_Locale);
   when Help =>
      Print_Line ("dl", "0017");
   end case;
exception
when Usage_Error =>
   Print_Line ("dl", "0001");
end X_DumpLocale;
