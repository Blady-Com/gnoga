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

with Ada.Characters.Handling;
with ZanyBlue.Text.Formatting;

package body ZanyBlue.Parameters.Sets is

   use Ada.Characters.Handling;
   use ZanyBlue.Text.Formatting;
   use Params_Hash_Map;

   ------------
   -- Append --
   ------------

   procedure Append
     (Params : in out Parameter_Set_Type;
      Name   :        Wide_String;
      Value  :        Wide_String)
   is
      Buffer : List_Type;
   begin
      if Params.Is_Defined (Name) then
         Buffer := Params.Get (Name).To_List (Name);
      end if;
      Append (Buffer, Value);
      Set (Params, Name, To_List_Value (Buffer));
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Params : in out Parameter_Set_Type) is
   begin
      Clear (Params.Values);
   end Clear;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Params      : Parameter_Set_Type;
      Destination : Ada.Text_IO.File_Type;
      Level       : Natural := 0)
   is

      Indentation : constant Wide_String (1 .. 2 * Level) := (others => ' ');

      procedure Dump_Parameter (Position : Cursor);
      --  Helper routine to handle an individual parameter

      --------------------
      -- Dump_Parameter --
      --------------------

      procedure Dump_Parameter (Position : Cursor) is
      begin
         Element (Position).Dump (Key (Position), Destination, Level => Level);
      end Dump_Parameter;

   begin
      Print_Line
        (Destination, "{0}<parameter-set name=""{1}"">", +Indentation,
         +Get_Name (Params));
      Iterate (Params.Values, Dump_Parameter'Access);
      Print_Line (Destination, "{0}</parameter-set>", +Indentation);
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Params      : Parameter_Set_Type;
      Destination : Ada.Wide_Text_IO.File_Type;
      Level       : Natural := 0)
   is

      Indentation : constant Wide_String (1 .. 2 * Level) := (others => ' ');

      procedure Dump_Parameter (Position : Cursor);
      --  Helper routine to handle an individual parameter

      --------------------
      -- Dump_Parameter --
      --------------------

      procedure Dump_Parameter (Position : Cursor) is
      begin
         Element (Position).Dump (Key (Position), Destination, Level => Level);
      end Dump_Parameter;

   begin
      Print_Line
        (Destination, "{0}<parameter-set name=""{1}"">", +Indentation,
         +Get_Name (Params));
      Iterate (Params.Values, Dump_Parameter'Access);
      Print_Line (Destination, "{0}</parameter-set>", +Indentation);
   end Dump;

   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys
     (Left, Right : Wide_String)
      return Boolean
   is
   begin
      return Left = Right;
   end Equivalent_Keys;

   ---------
   -- Get --
   ---------

   function Get
     (Params : Parameter_Set_Type;
      Name   : Wide_String)
      return Value_Type'Class
   is
      C : constant Cursor := Find (Params.Values, Name);
   begin
      if C = No_Element then
         raise Not_Defined_Error with To_String (Name);
      end if;
      return Element (C);
   end Get;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
     (Params : Parameter_Set_Type;
      Name   : Wide_String)
      return Boolean
   is
   begin
      return Get (Params, Name).To_Boolean (Name);
   end Get_Boolean;

   ---------------
   -- Get_Float --
   ---------------

   function Get_Float
     (Params : Parameter_Set_Type;
      Name   : Wide_String)
      return Float
   is
   begin
      return Get (Params, Name).To_Float (Name);
   end Get_Float;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer
     (Params : Parameter_Set_Type;
      Name   : Wide_String)
      return Integer
   is
   begin
      return Get (Params, Name).To_Integer (Name);
   end Get_Integer;

   --------------
   -- Get_List --
   --------------

   function Get_List
     (Params : Parameter_Set_Type;
      Name   : Wide_String)
      return List_Type
   is
   begin
      return Params.Get (Name).To_List (Name);
   end Get_List;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Params : Parameter_Set_Type)
      return Wide_String
   is
   begin
      return To_Wide_String (Params.Name);
   end Get_Name;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (Params : Parameter_Set_Type;
      Name   : Wide_String)
      return Wide_String
   is
   begin
      return Get (Params, Name).To_String (Name);
   end Get_String;

   --------------
   -- Get_Time --
   --------------

   function Get_Time
     (Params : Parameter_Set_Type;
      Name   : Wide_String)
      return Time
   is
   begin
      return Get (Params, Name).To_Time (Name);
   end Get_Time;

   ---------------
   -- Increment --
   ---------------

   procedure Increment
     (Params    : in out Parameter_Set_Type;
      Name      :        Wide_String;
      By_Amount :        Integer := 1)
   is

      procedure Increment_Value
        (Key     :        Wide_String;
         Element : in out Value_Type'Class);
      --  Helper routine to to the incrementing of a value

      ---------------------
      -- Increment_Value --
      ---------------------

      procedure Increment_Value
        (Key     :        Wide_String;
         Element : in out Value_Type'Class)
      is
         pragma Unreferenced (Key);
      begin
         Element.Increment (Name, By_Amount);
      end Increment_Value;

      C : constant Cursor := Find (Params.Values, Name);

   begin
      if C /= No_Element then
         Update_Element (Params.Values, C, Increment_Value'Access);
      else
         raise Not_Defined_Error with To_String (Name);
      end if;
   end Increment;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined
     (Params : Parameter_Set_Type;
      Name   : Wide_String)
      return Boolean
   is
   begin
      return Find (Params.Values, Name) /= No_Element;
   end Is_Defined;

   --------------------------
   -- Number_Of_Parameters --
   --------------------------

   function Number_Of_Parameters
     (Params : Parameter_Set_Type)
      return Natural
   is
   begin
      return Natural (Length (Params.Values));
   end Number_Of_Parameters;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Params : in out Parameter_Set_Type;
      Name   :        Wide_String;
      Value  :        Wide_String)
   is
      Buffer : List_Type;
   begin
      Append (Buffer, Value);
      if Params.Is_Defined (Name) then
         Append (Buffer, Params.Get (Name).To_List (Name));
      end if;
      Set (Params, Name, To_List_Value (Buffer));
   end Prepend;

   ---------
   -- Set --
   ---------

   procedure Set
     (Params : in out Parameter_Set_Type;
      Name   :        Wide_String;
      Value  :        Value_Type'Class)
   is
      Position : constant Cursor := Params.Values.Find (Name);
   begin
      if Position = No_Element then
         Params.Values.Insert (Name, Value);
      else
         Params.Values.Replace (Name, Value);
      end if;
   end Set;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean
     (Params : in out Parameter_Set_Type;
      Name   :        Wide_String;
      Value  :        Boolean)
   is
   begin
      Set (Params, Name, To_Boolean_Value (Value));
   end Set_Boolean;

   ---------------
   -- Set_Float --
   ---------------

   procedure Set_Float
     (Params : in out Parameter_Set_Type;
      Name   :        Wide_String;
      Value  :        Float)
   is
   begin
      Set (Params, Name, To_Float_Value (Value));
   end Set_Float;

   -----------------
   -- Set_Integer --
   -----------------

   procedure Set_Integer
     (Params : in out Parameter_Set_Type;
      Name   :        Wide_String;
      Value  :        Integer)
   is
   begin
      Set (Params, Name, To_Integer_Value (Value));
   end Set_Integer;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Params : in out Parameter_Set_Type;
      Name   :        Wide_String)
   is
   begin
      Set_Unbounded_Wide_String (Params.Name, Name);
   end Set_Name;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (Params : in out Parameter_Set_Type;
      Name   :        Wide_String;
      Value  :        Wide_String)
   is
   begin
      Set (Params, Name, To_String_Value (Value));
   end Set_String;

   --------------
   -- Set_Time --
   --------------

   procedure Set_Time
     (Params : in out Parameter_Set_Type;
      Name   :        Wide_String;
      Value  :        Time)
   is
   begin
      Set (Params, Name, To_Time_Value (Value));
   end Set_Time;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (Params : Parameter_Set_Type;
      Name   : Wide_String)
      return Wide_String
   is
   begin
      return Get (Params, Name).Type_Name (Name);
   end Type_Name;

end ZanyBlue.Parameters.Sets;
