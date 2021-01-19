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
with Ada.Text_IO;
with Ada.Wide_Text_IO;

package ZanyBlue.Parameters.Values is

   use Ada.Calendar;

   type Value_Type is abstract tagged private;

   function To_Boolean
     (Value : Value_Type;
      Name  : Wide_String)
      return Boolean;
   --  Return the value of a parameter as a boolean.  The exception
   --  Not_A_Boolean_Error can be raised.

   function To_Float
     (Value : Value_Type;
      Name  : Wide_String)
      return Float;
   --  Return the value of a floating point parameter.  The exception
   --  Not_A_Real_Error can be raised.

   function To_Integer
     (Value : Value_Type;
      Name  : Wide_String)
      return Integer;
   --  Return the value of an integer parameter.  The exception
   --  Not_An_Integer_Error can be raised.

   function To_List
     (Value : Value_Type;
      Name  : Wide_String)
      return List_Type is abstract;
   --  Return the value of a list parameter.  This will return the value
   --  converted to a list for if necessary.

   function To_String
     (Value : Value_Type;
      Name  : Wide_String)
      return Wide_String is abstract;
   --  Return the value of a parameter as a string.  This can be used
   --  for any parameter type.

   function To_Time
     (Value : Value_Type;
      Name  : Wide_String)
      return Time;
   --  Return the value of an time parameter.  The exception
   --  Not_A_Time_Error can be raised.

   function Type_Name
     (Value : Value_Type;
      Name  : Wide_String)
      return Wide_String is abstract;
   --  Return the type name for a parameter ("float", "integer", "string" or
   --  "time").

   procedure Increment
     (Value     : in out Value_Type;
      Name      :        Wide_String;
      By_Amount :        Integer);
   --  Increment a parameter value by the given amount.  The exception
   --  Not_An_Integer_Error can be raised.

   procedure Dump
     (Value       : Value_Type'Class;
      Name        : Wide_String;
      Destination : Ada.Text_IO.File_Type;
      Level       : Natural := 0);
   --  Write a parameter value definition as an XML definition to the given
   --  file.

   procedure Dump
     (Value       : Value_Type'Class;
      Name        : Wide_String;
      Destination : Ada.Wide_Text_IO.File_Type;
      Level       : Natural := 0);
   --  Write a parameter value definition as an XML definition to the given
   --  file.

   function To_Boolean_Value
     (Data : Boolean)
      return Value_Type'Class;
   --  Convert a Boolean to a Value_Type (boxing)

   function To_Integer_Value
     (Data : Integer)
      return Value_Type'Class;
   --  Convert an integer to a Value_Type (boxing)

   function To_Float_Value
     (Data : Float)
      return Value_Type'Class;
   --  Convert a floating point to a Value_Type (boxing)

   function To_List_Value
     (Data : List_Type)
      return Value_Type'Class;
   --  Convert a list value to a Value_Type (boxing)

   function To_String_Value
     (Data : Wide_String)
      return Value_Type'Class;
   --  Convert a string value to a Value_Type (boxing)

   function To_Time_Value
     (Data : Time)
      return Value_Type'Class;
   --  Convert a Time value to a Value_Type (boxing)

private

   type Value_Type is abstract tagged null record;
   --  Root type for the value class hierarchy, doesn't contain anything.

end ZanyBlue.Parameters.Values;
