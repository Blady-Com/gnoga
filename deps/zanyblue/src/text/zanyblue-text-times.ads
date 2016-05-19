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

with Ada.Calendar;
with Ada.Calendar.Time_Zones;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Arguments;

package ZanyBlue.Text.Times is

   use Ada.Calendar;
   use Ada.Calendar.Time_Zones;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Arguments;

   type Time_Argument_Type is new Calendar_Category_Type with private;

   function Create (Time_Value : Time) return Time_Argument_Type;
   --  Create a "boxed" instance of a time type.

   function Create (Time_Value : Time;
                    TZ_Offset  : Time_Offset) return Time_Argument_Type;
   --  Create a "boxed" instance of a time type with time zone offset.

   function "+" (Time_Value : Time) return Time_Argument_Type
      renames Create;
   --  Utility renaming of the "Create" function.

   overriding
   function Format (Value     : Time_Argument_Type;
                    Type_Name : Wide_String;
                    Template  : Wide_String;
                    Locale    : Locale_Type) return Wide_String;
   --  Format a time value according to the Template.

   function Day_In_Week (Day   : Day_Number;
                         Month : Month_Number;
                         Year  : Year_Number) return Day_Type;
   --  Return the day of the week given a particular date.  This is
   --  a utility routine exposed here mainly for testing purposes.

private

   type Time_Argument_Type is new Calendar_Category_Type with
   record
      Data      : Ada.Calendar.Time;
      TZ_Offset : Time_Offset;
   end record;

end ZanyBlue.Text.Times;
