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

with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Formatting;
with ZBInfo_Messages.ZBInfo_Prints;

-----------------
-- Dump_Locale --
-----------------

procedure ZBInfo.Dump_Locale (Locale : ZanyBlue.Text.Locales.Locale_Type;
                              Name   : String) is

   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Formatting;
   use ZBInfo_Messages.ZBInfo_Prints;
   use Day_Arguments;
   use Era_Arguments;
   use Month_Arguments;
   use Hash_Type_Arguments;
   use Day_Period_Arguments;
   use Text_Layout_Arguments;
   use Numeric_Item_Arguments;
   use Numeric_Style_Arguments;
   use Date_Time_Style_Arguments;

   procedure Display (Attribute  : String;
                      Value      : Argument_Type'Class;
                      Quoted     : Boolean := False);
   procedure Display2 (Attribute  : String;
                       Value1     : Argument_Type'Class;
                       Value2     : Argument_Type'Class);

   -------------
   -- Display --
   -------------

   procedure Display (Attribute  : String;
                      Value      : Argument_Type'Class;
                      Quoted     : Boolean := False) is
   begin
      if Quoted then
         Print_00008 (+Attribute, Value);
      else
         Print_00009 (+Attribute, Value);
      end if;
   end Display;

   --------------
   -- Display2 --
   --------------

   procedure Display2 (Attribute  : String;
                       Value1     : Argument_Type'Class;
                       Value2     : Argument_Type'Class) is
   begin
      Print_00010 (+Attribute, Value1, Value2);
   end Display2;

begin
   Print_00005 (+Name);
   Print_00007 (+Locale_Name (Locale));
   Display ("Language",       +Language (Locale), True);
   Display ("Script",         +Script (Locale), True);
   Display ("Territory",      +Territory (Locale), True);
   Display2 ("Encoding",      +Encoding (Locale),
                              +Encoding_Implementation (Locale));
   Display ("Name",           +Locale_Name (Locale), True);
   Display ("Traits Name",    +Traits_Name (Locale), True);
   Display ("Traits Tag",     +Traits_Tag (Locale), True);
   Display ("Locale Level",   +Integer (Locale_Level (Locale)));
   Display ("Is Root Locale", +Is_Root_Locale (Locale));
   Display ("Layout",         +Text_Layout (Locale));
   Display ("Lower Digits",   +Locale_Digits (Locale, True));
   Display ("Upper Digits",   +Locale_Digits (Locale, False));
   Display ("Hash Value",     +Hash (Locale));
   Print_00011;
   Print_00012;
   for Style in Date_Time_Style_Type loop
      Print_00013 (+Style,
                   +Date_Format (Locale, Style),
                   +Time_Format (Locale, Style),
                   +Date_Time_Format (Locale, Style));
   end loop;
   Print_00014;
   for Period in Day_Period_Type loop
      Print_00009 (+Period, +Day_Period_Name (Locale, Period));
   end loop;
   Print_00015;
   for Hour in Integer range 0 .. 23 loop
      Print_00016 (+Hour,
                   +Day_Period_For_Time (Locale, Hour, 0, 0),
                   +Day_Period_For_Time (Locale, Hour, 30, 0));
   end loop;
   Print_00017;
   for Era in Era_Type loop
      Print_00009 (+Era, +Era_Name (Locale, Era));
   end loop;
   Print_00018;
   Print_00019;
   for Day in Day_Type loop
      Print_00021 (+Day,
                   +Short_Day_Name (Locale, Day),
                   +Full_Day_Name (Locale, Day));
   end loop;
   Print_00020;
   Print_00019;
   for Month in Month_Type loop
      Print_00021 (+Month,
                   +Short_Month_Name (Locale, Month),
                   +Full_Month_Name (Locale, Month));
   end loop;
   Print_00022;
   for Style in Numeric_Style_Type loop
      Print_00009 (+Style, +Numeric_Format (Locale, Style));
   end loop;
   Print_00023;
   for Item in Numeric_Item_Type loop
      Print_00009 (+Item, +Numeric_Item (Locale, Item));
   end loop;
end ZBInfo.Dump_Locale;
