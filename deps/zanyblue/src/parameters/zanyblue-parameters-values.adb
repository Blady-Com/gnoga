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

with Ada.Characters.Handling;
with ZanyBlue.Text.Formatting;

package body ZanyBlue.Parameters.Values is

   use Ada.Characters.Handling;
   use ZanyBlue.Text.Formatting;

   type Boolean_Value_Type is new Value_Type with
   record
      Data : Boolean;
   end record;

   overriding
   function To_Boolean (Value : in Boolean_Value_Type;
                        Name  : in Wide_String) return Boolean;
   --  Extract the Boolean value from a boxed Boolean value.

   overriding
   function To_List (Value : in Boolean_Value_Type;
                     Name  : in Wide_String) return List_Type;
   --  Extract the Boolean value from a boxed value as a list.

   overriding
   function To_String (Value : in Boolean_Value_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Extract the Boolean value as a formatted string.

   overriding
   function Type_Name (Value : in Boolean_Value_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Return the type name string for the boxed Boolean value.

   type Float_Value_Type is new Value_Type with
   record
      Data : Float;
   end record;

   overriding
   function To_Float (Value : in Float_Value_Type;
                      Name  : in Wide_String) return Float;
   --  Extract the floating point value from a boxed value.

   overriding
   function To_List (Value : in Float_Value_Type;
                     Name  : in Wide_String) return List_Type;
   --  Extract the floating point value from a boxed value as a list.

   overriding
   function To_String (Value : in Float_Value_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Extract the floating point value as a formatted string.

   overriding
   function Type_Name (Value : in Float_Value_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Return the type name string for the boxed floating point value.

   type Integer_Value_Type is new Value_Type with
   record
      Data : Integer;
   end record;

   overriding
   procedure Increment (Value     : in out Integer_Value_Type;
                        Name      : in Wide_String;
                        By_Amount : in Integer);
   --  Increment an integer parameter value.

   overriding
   function To_Integer (Value : in Integer_Value_Type;
                        Name  : in Wide_String) return Integer;
   --  Extract the integer value as a formatted string.

   overriding
   function To_List (Value : in Integer_Value_Type;
                     Name  : in Wide_String) return List_Type;
   --  Extract the integer value from a boxed value as a list.

   overriding
   function To_String (Value : in Integer_Value_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Extract the integer value as a formatted string.

   overriding
   function Type_Name (Value : in Integer_Value_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Return the type name string for the boxed integer value.

   type List_Value_Type is new Value_Type with
   record
      Data : List_Type;
   end record;

   overriding
   function To_List (Value : in List_Value_Type;
                     Name  : in Wide_String) return List_Type;
   --  Extract the list value from a boxed value.

   overriding
   function To_String (Value : in List_Value_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Extract the list value as a formatted string.

   overriding
   function Type_Name (Value : in List_Value_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Return the type name string for the boxed list value.

   type String_Value_Type (Length : Natural) is new Value_Type with
   record
      Data : Wide_String (1 .. Length);
   end record;

   overriding
   function To_List (Value : in String_Value_Type;
                     Name  : in Wide_String) return List_Type;
   --  Extract the floating point value from a boxed value as a list.

   overriding
   function To_String (Value : in String_Value_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Extract the string value from a boxed value.

   overriding
   function Type_Name (Value : in String_Value_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Return the type name string for the boxed string value.

   type Time_Value_Type is new Value_Type with
   record
      Data : Time;
   end record;

   overriding
   function To_List (Value : in Time_Value_Type;
                     Name  : in Wide_String) return List_Type;
   --  Extract the floating point value from a boxed value as a list.

   overriding
   function To_String (Value : in Time_Value_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Extract the time value as a formatted string.

   overriding
   function To_Time (Value : in Time_Value_Type;
                     Name  : in Wide_String) return Time;
   --  Extract the time value from a boxed value.

   overriding
   function Type_Name (Value : in Time_Value_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Return the type name string for the boxed time value.

   -------------------------------------------------------------------------

   ----------
   -- Dump --
   ----------

   procedure Dump (Value       : in Value_Type'Class;
                   Name        : in Wide_String;
                   Destination : in File_Type;
                   Level       : in Natural := 0) is

      Indentation : constant Wide_String (1 .. 2 * Level) := (others => ' ');
      List        : List_Type;

   begin
      Print (Destination, "{0}  <parameter name=""{1}"" type=""{2}""",
                          +Indentation, +Name, +Value.Type_Name (Name));
      if Value in List_Value_Type then
         List := List_Value_Type (Value).Data;
         Print_Line (Destination, ">");
         for I in 1 .. Length (List) loop
            Print_Line (Destination, "{0}    <value>{1}</value>",
                                     +Indentation, +List.Element (I));
         end loop;
         Print_Line (Destination, "{0}  </parameter>",
                                  +Indentation);
      else
         Print_Line (Destination, " value=""{0}"" />",
                                  +Value.To_String (Name));
      end if;
   end Dump;

   ---------------
   -- Increment --
   ---------------

   procedure Increment (Value     : in out Value_Type;
                        Name      : in Wide_String;
                        By_Amount : in Integer) is
      pragma Unreferenced (Value);
      pragma Unreferenced (By_Amount);
   begin
      raise Not_An_Integer_Error with To_String (Name);
   end Increment;

   ---------------
   -- Increment --
   ---------------

   overriding
   procedure Increment (Value     : in out Integer_Value_Type;
                        Name      : in Wide_String;
                        By_Amount : in Integer) is
      pragma Unreferenced (Name);
   begin
      Value.Data := Value.Data + By_Amount;
   end Increment;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (Value : in Value_Type;
                        Name  : in Wide_String) return Boolean is
      pragma Unreferenced (Value);
   begin
      raise Not_A_Boolean_Error with To_String (Name);
      return False;
   end To_Boolean;

   ----------------
   -- To_Boolean --
   ----------------

   overriding
   function To_Boolean (Value : in Boolean_Value_Type;
                        Name  : in Wide_String) return Boolean is
      pragma Unreferenced (Name);
   begin
      return Value.Data;
   end To_Boolean;

   ----------------------
   -- To_Boolean_Value --
   ----------------------

   function To_Boolean_Value (Data : in Boolean) return Value_Type'Class is
   begin
      return Boolean_Value_Type'(Data => Data);
   end To_Boolean_Value;

   --------------
   -- To_Float --
   --------------

   function To_Float (Value : in Value_Type;
                      Name  : in Wide_String) return Float is
      pragma Unreferenced (Value);
   begin
      raise Not_A_Real_Error with To_String (Name);
      return 0.0;
   end To_Float;

   --------------
   -- To_Float --
   --------------

   overriding
   function To_Float (Value : in Float_Value_Type;
                      Name  : in Wide_String) return Float is
      pragma Unreferenced (Name);
   begin
      return Value.Data;
   end To_Float;

   --------------------
   -- To_Float_Value --
   --------------------

   function To_Float_Value (Data : in Float) return Value_Type'Class is
   begin
      return Float_Value_Type'(Data => Data);
   end To_Float_Value;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Value : in Value_Type;
                        Name  : in Wide_String) return Integer is
      pragma Unreferenced (Value);
   begin
      raise Not_An_Integer_Error with To_String (Name);
      return 0;
   end To_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Value : in Integer_Value_Type;
                        Name  : in Wide_String) return Integer is
      pragma Unreferenced (Name);
   begin
      return Value.Data;
   end To_Integer;

   ----------------------
   -- To_Integer_Value --
   ----------------------

   function To_Integer_Value (Data : in Integer) return Value_Type'Class is
   begin
      return Integer_Value_Type'(Data => Data);
   end To_Integer_Value;

   -------------
   -- To_List --
   -------------

   overriding
   function To_List (Value : in Boolean_Value_Type;
                     Name  : in Wide_String) return List_Type is
      Result : List_Type;
   begin
      Append (Result, Value.To_String (Name));
      return Result;
   end To_List;

   -------------
   -- To_List --
   -------------

   overriding
   function To_List (Value : in Float_Value_Type;
                     Name  : in Wide_String) return List_Type is
      Result : List_Type;
   begin
      Append (Result, Value.To_String (Name));
      return Result;
   end To_List;

   -------------
   -- To_List --
   -------------

   overriding
   function To_List (Value : in Integer_Value_Type;
                     Name  : in Wide_String) return List_Type is
      Result : List_Type;
   begin
      Append (Result, Value.To_String (Name));
      return Result;
   end To_List;

   -------------
   -- To_List --
   -------------

   overriding
   function To_List (Value : in List_Value_Type;
                     Name  : in Wide_String) return List_Type is
      pragma Unreferenced (Name);
   begin
      return Value.Data;
   end To_List;

   -------------
   -- To_List --
   -------------

   overriding
   function To_List (Value : in String_Value_Type;
                     Name  : in Wide_String) return List_Type is
      Result : List_Type;
   begin
      Append (Result, Value.To_String (Name));
      return Result;
   end To_List;

   -------------
   -- To_List --
   -------------

   overriding
   function To_List (Value : in Time_Value_Type;
                     Name  : in Wide_String) return List_Type is
      Result : List_Type;
   begin
      Append (Result, Value.To_String (Name));
      return Result;
   end To_List;

   -------------------
   -- To_List_Value --
   -------------------

   function To_List_Value (Data : in List_Type) return Value_Type'Class is
   begin
      return List_Value_Type'(Data => Data);
   end To_List_Value;

   ---------------
   -- To_String --
   ---------------

   overriding
   function To_String (Value : in Boolean_Value_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Name);
   begin
      return Format ("{0}", Argument0 => +Value.Data);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding
   function To_String (Value : in Float_Value_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Name);
   begin
      return Format ("{0}", Argument0 => +Value.Data);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding
   function To_String (Value : in Integer_Value_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Name);
   begin
      return Format ("{0}", Argument0 => +Value.Data);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding
   function To_String (Value : in List_Value_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Name);
      List_Length : constant Natural := Length (Value.Data);
      Buffer      : Unbounded_Wide_String;
   begin
      Append (Buffer, "[");
      for I in 1 .. List_Length loop
         Append (Buffer, Value.Data.Element (I));
         if I /= List_Length then
            Append (Buffer, ", ");
         end if;
      end loop;
      Append (Buffer, "]");
      return To_Wide_String (Buffer);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding
   function To_String (Value : in String_Value_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Name);
   begin
      return Value.Data;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding
   function To_String (Value : in Time_Value_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Name);
   begin
      return Format ("{0}", Argument0 => +Value.Data);
   end To_String;

   ---------------------
   -- To_String_Value --
   ---------------------

   function To_String_Value (Data : in Wide_String) return Value_Type'Class is
   begin
      return String_Value_Type'(Length => Data'Length, Data => Data);
   end To_String_Value;

   -------------
   -- To_Time --
   -------------

   overriding
   function To_Time (Value : in Time_Value_Type;
                     Name  : in Wide_String) return Time is
      pragma Unreferenced (Name);
   begin
      return Value.Data;
   end To_Time;

   -------------
   -- To_Time --
   -------------

   function To_Time (Value : in Value_Type;
                     Name  : in Wide_String) return Time is
      pragma Unreferenced (Value);
   begin
      raise Not_A_Time_Error with To_String (Name);
      return Clock;
   end To_Time;

   -------------------
   -- To_Time_Value --
   -------------------

   function To_Time_Value (Data : in Time) return Value_Type'Class is
   begin
      return Time_Value_Type'(Data => Data);
   end To_Time_Value;

   ---------------
   -- Type_Name --
   ---------------

   overriding
   function Type_Name (Value : in Boolean_Value_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Value);
      pragma Unreferenced (Name);
   begin
      return "boolean";
   end Type_Name;

   ---------------
   -- Type_Name --
   ---------------

   overriding
   function Type_Name (Value : in Float_Value_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Value);
      pragma Unreferenced (Name);
   begin
      return "float";
   end Type_Name;

   ---------------
   -- Type_Name --
   ---------------

   overriding
   function Type_Name (Value : in Integer_Value_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Value);
      pragma Unreferenced (Name);
   begin
      return "integer";
   end Type_Name;

   ---------------
   -- Type_Name --
   ---------------

   overriding
   function Type_Name (Value : in List_Value_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Value);
      pragma Unreferenced (Name);
   begin
      return "list";
   end Type_Name;

   ---------------
   -- Type_Name --
   ---------------

   overriding
   function Type_Name (Value : in String_Value_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Value);
      pragma Unreferenced (Name);
   begin
      return "string";
   end Type_Name;

   ---------------
   -- Type_Name --
   ---------------

   overriding
   function Type_Name (Value : in Time_Value_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Value);
      pragma Unreferenced (Name);
   begin
      return "time";
   end Type_Name;

end ZanyBlue.Parameters.Values;
