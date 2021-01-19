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

with ZanyBlue.Text.Formatting;

package body ZanyBlue.Parameters.Scopes is

   use Ada.Containers;
   use ZanyBlue.Text.Formatting;
   use Parameter_Set_Vectors;

   function Top_Index
     (Param_Stack : Parameter_Stack_Type)
      return Natural;
   --  Return the index of the top of the parameter stack.

   ------------
   -- Append --
   ------------

   procedure Append
     (Param_Stack : in out Parameter_Stack_Type;
      Name        :        String;
      Value       :        String)
   is

      procedure Append_Value (Params : in out Parameter_Set_Type);
      --  Helper routine to append the value.

      ------------------
      -- Append_Value --
      ------------------

      procedure Append_Value (Params : in out Parameter_Set_Type) is
      begin
         Params.Append (Name, Value);
      end Append_Value;

      Index : constant Natural := Top_Index (Param_Stack);

   begin    -- Append
      if Index = 0 then
         raise Empty_Parameter_Stack;
      end if;
      Update_Element (Param_Stack.Values, Index, Append_Value'Access);
   end Append;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Param_Stack : Parameter_Stack_Type;
      Destination : UXStrings.Text_IO.File_Type;
      All_Scopes  : Boolean)
   is

      use UXStrings.Text_IO;

      procedure Dump_Set (Params : Parameter_Set_Type);
      --  Helper routine used to dump an individual parameter set.

      Level : Natural := 0;

      --------------
      -- Dump_Set --
      --------------

      procedure Dump_Set (Params : Parameter_Set_Type) is
      begin
         Params.Dump (Destination, Level => Level);
      end Dump_Set;

   begin
      if All_Scopes then
         Level := 1;
         Put_Line (Destination, "<parameter-stack>");
         for Index in 1 .. Top_Index (Param_Stack) loop
            Query_Element (Param_Stack.Values, Index, Dump_Set'Access);
         end loop;
         Put_Line (Destination, "</parameter-stack>");
      else
         Query_Element
           (Param_Stack.Values, Top_Index (Param_Stack), Dump_Set'Access);
      end if;
   end Dump;

   ----------
   -- Dump --
   ----------

--     procedure Dump
--       (Param_Stack : Parameter_Stack_Type;
--        Destination : Ada.Wide_Text_IO.File_Type;
--        All_Scopes  : Boolean)
--     is
--
--        use Ada.Wide_Text_IO;
--
--        procedure Dump_Set (Params : Parameter_Set_Type);
--        --  Helper routine used to dump an individual parameter set.
--
--        Level : Natural := 0;
--
--        --------------
--        -- Dump_Set --
--        --------------
--
--        procedure Dump_Set (Params : Parameter_Set_Type) is
--        begin
--           Params.Dump (Destination, Level => Level);
--        end Dump_Set;
--
--     begin
--        if All_Scopes then
--           Level := 1;
--           Put_Line (Destination, "<parameter-stack>");
--           for Index in 1 .. Top_Index (Param_Stack) loop
--              Query_Element (Param_Stack.Values, Index, Dump_Set'Access);
--           end loop;
--           Put_Line (Destination, "</parameter-stack>");
--        else
--           Query_Element
--             (Param_Stack.Values, Top_Index (Param_Stack), Dump_Set'Access);
--        end if;
--     end Dump;

   ---------------
   -- End_Scope --
   ---------------

   procedure End_Scope (Param_Stack : in out Parameter_Stack_Type) is
      Index : constant Natural := Top_Index (Param_Stack);
   begin
      if Index = 0 then
         raise Empty_Parameter_Stack;
      end if;
      Set_Length (Param_Stack.Values, Count_Type (Index - 1));
   end End_Scope;

   ---------
   -- Get --
   ---------

   function Get
     (Param_Stack : Parameter_Stack_Type;
      Name        : String)
      return Value_Type'Class
   is
      Params : Parameter_Set_Type;
   begin
      for I in reverse 1 .. Top_Index (Param_Stack) loop
         Params := Element (Param_Stack.Values, I);
         if Is_Defined (Params, Name) then
            return Get (Params, Name);
         end if;
      end loop;
      raise Not_Defined_Error with To_Latin_1 (Name);
   end Get;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
     (Param_Stack : Parameter_Stack_Type;
      Name        : String)
      return Boolean
   is
   begin
      return Get (Param_Stack, Name).To_Boolean (Name);
   end Get_Boolean;

   ---------------
   -- Get_Float --
   ---------------

   function Get_Float
     (Param_Stack : Parameter_Stack_Type;
      Name        : String)
      return Float
   is
   begin
      return Get (Param_Stack, Name).To_Float (Name);
   end Get_Float;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer
     (Param_Stack : Parameter_Stack_Type;
      Name        : String)
      return Integer
   is
   begin
      return Get (Param_Stack, Name).To_Integer (Name);
   end Get_Integer;

   --------------
   -- Get_List --
   --------------

   function Get_List
     (Param_Stack : Parameter_Stack_Type;
      Name        : String;
      Deep        : Boolean)
      return List_Type
   is

      procedure Accumulate (Params : Parameter_Set_Type);
      --  Helper routine to accumulate the list of values from various
      --  scopes.

      Lower_Index : Natural := Top_Index (Param_Stack);
      Result      : List_Type;

      ----------------
      -- Accumulate --
      ----------------

      procedure Accumulate (Params : Parameter_Set_Type) is
      begin
         if Params.Is_Defined (Name) then
            Append (Result, Params.Get (Name).To_List (Name));
         end if;
      end Accumulate;

   begin
      if Deep then
         Lower_Index := 1;
      end if;
      for I in reverse Lower_Index .. Top_Index (Param_Stack) loop
         Query_Element (Param_Stack.Values, I, Accumulate'Access);
      end loop;
      return Result;
   end Get_List;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (Param_Stack : Parameter_Stack_Type;
      Name        : String)
      return String
   is
   begin
      return Get (Param_Stack, Name).To_String (Name);
   end Get_String;

   --------------
   -- Get_Time --
   --------------

   function Get_Time
     (Param_Stack : Parameter_Stack_Type;
      Name        : String)
      return Time
   is
   begin
      return Get (Param_Stack, Name).To_Time (Name);
   end Get_Time;

   ---------------
   -- Increment --
   ---------------

   procedure Increment
     (Param_Stack : in out Parameter_Stack_Type;
      Name        :        String;
      By_Amount   :        Integer := 1;
      Deep        :        Boolean := True)
   is

      procedure Increment_Value (Params : in out Parameter_Set_Type);
      --  Helper routine to do the incrementing.

      Incremented : Boolean := False;

      ---------------------
      -- Increment_Value --
      ---------------------

      procedure Increment_Value (Params : in out Parameter_Set_Type) is
      begin
         if Is_Defined (Params, Name) then
            Increment (Params, Name, By_Amount => By_Amount);
            Incremented := True;
         end if;
      end Increment_Value;

   begin
      for I in reverse 1 .. Top_Index (Param_Stack) loop
         Update_Element (Param_Stack.Values, I, Increment_Value'Access);
         if not Deep and then Incremented then
            return;
         end if;
      end loop;
   end Increment;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined
     (Param_Stack : Parameter_Stack_Type;
      Name        : String;
      Any_Scope   : Boolean := True)
      return Boolean
   is
      Params       : Parameter_Set_Type;
      Lowest_Scope : Positive := 1;
   begin
      if not Any_Scope then
         Lowest_Scope := Top_Index (Param_Stack);
      end if;
      for I in reverse Lowest_Scope .. Top_Index (Param_Stack) loop
         Params := Element (Param_Stack.Values, I);
         if Is_Defined (Params, Name) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Defined;

   ---------------
   -- New_Scope --
   ---------------

   procedure New_Scope (Param_Stack : in out Parameter_Stack_Type) is
      Level       : constant Natural := Top_Index (Param_Stack);
      New_Element : Parameter_Set_Type;
   begin
      Set_Name (New_Element, Format ("PARAMS-{0}", Argument0 => +Level));
      Set_Integer (New_Element, "_level", Level);
      Append (Param_Stack.Values, New_Element);
   end New_Scope;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Param_Stack : in out Parameter_Stack_Type;
      Name        :        String;
      Value       :        String)
   is

      procedure Prepend_Value (Params : in out Parameter_Set_Type);
      --  Helper routine to do the actual prepending

      -------------------
      -- Prepend_Value --
      -------------------

      procedure Prepend_Value (Params : in out Parameter_Set_Type) is
      begin
         Params.Prepend (Name, Value);
      end Prepend_Value;

      Index : constant Natural := Top_Index (Param_Stack);

   begin
      if Index = 0 then
         raise Empty_Parameter_Stack;
      end if;
      Update_Element (Param_Stack.Values, Index, Prepend_Value'Access);
   end Prepend;

   ---------
   -- Set --
   ---------

   procedure Set
     (Param_Stack : in out Parameter_Stack_Type;
      Name        :        String;
      Value       :        Value_Type'Class)
   is

      procedure Set_Value (Params : in out Parameter_Set_Type);
      --  Helper routine to do the value set in a parameter set.

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value (Params : in out Parameter_Set_Type) is
      begin
         Set (Params, Name, Value);
      end Set_Value;

      Index : constant Natural := Top_Index (Param_Stack);

   begin
      if Index = 0 then
         raise Empty_Parameter_Stack;
      end if;
      Update_Element (Param_Stack.Values, Index, Set_Value'Access);
   end Set;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean
     (Param_Stack : in out Parameter_Stack_Type;
      Name        :        String;
      Value       :        Boolean)
   is
   begin
      Set (Param_Stack, Name, To_Boolean_Value (Value));
   end Set_Boolean;

   ---------------
   -- Set_Float --
   ---------------

   procedure Set_Float
     (Param_Stack : in out Parameter_Stack_Type;
      Name        :        String;
      Value       :        Float)
   is
   begin
      Set (Param_Stack, Name, To_Float_Value (Value));
   end Set_Float;

   -----------------
   -- Set_Integer --
   -----------------

   procedure Set_Integer
     (Param_Stack : in out Parameter_Stack_Type;
      Name        :        String;
      Value       :        Integer)
   is
   begin
      Set (Param_Stack, Name, To_Integer_Value (Value));
   end Set_Integer;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (Param_Stack : in out Parameter_Stack_Type;
      Name        :        String;
      Value       :        String)
   is
   begin
      Set (Param_Stack, Name, To_String_Value (Value));
   end Set_String;

   --------------
   -- Set_Time --
   --------------

   procedure Set_Time
     (Param_Stack : in out Parameter_Stack_Type;
      Name        :        String;
      Value       :        Time)
   is
   begin
      Set (Param_Stack, Name, To_Time_Value (Value));
   end Set_Time;

   ---------------
   -- Top_Index --
   ---------------

   function Top_Index
     (Param_Stack : Parameter_Stack_Type)
      return Natural
   is
   begin
      return Natural (Length (Param_Stack.Values));
   end Top_Index;

end ZanyBlue.Parameters.Scopes;
