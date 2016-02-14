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

--
--  The ZanyBlue.Parameters.Scopes implements a stack based parameter sets
--  with scoping, i.e., local parameters to a scope.  Changes made to
--  parameters with a scope do not impact parameter values in enclosing
--  scopes.  Scopes are started with New_Scope and ended with End_Scope.
--  Access to parameter values will traverse the set of defined scopes
--  until a value is found.  E.g.,
--
--    New_Scope
--       Set (A, "B")
--       Get (A)          => Return "B"
--       New_Scope
--          Get (A)       => Return "B"
--          Set (A, "C")
--          Get (A)       => Return "C"
--       End_Scope
--       Get (A)          => Return "B"
--    End_Scope
--
--  For list types, the functionality uses scope accumulation, i.e.,
--  the value return for a list parameter is a concatenation of values
--  for all scopes, e.g.,
--
--    New_Scope
--       Set (A, [1, 2])
--       Get (A)          => Return [1, 2]
--       New_Scope
--          Get (A)       => Return [1, 2]
--          Set (A, [3])
--          Get (A)       => Return [3, 1, 2]
--       End_Scope
--       Get (A)          => Return [1, 2]
--    End_Scope
--
--  In a list context, values that are not lists are convert to singleton
--  lists, e.g.
--
--    New_Scope
--       Set (A, [1, 2])
--       Get (A)          => Return [1, 2]
--       New_Scope
--          Get (A)       => Return [1, 2]
--          Set (A, 3)
--          Get (A)       => Return [3, 1, 2]   (List context, Get_List)
--          Get (A)       => 3                  (Integer context, Get_Integer)
--       End_Scope
--       Get (A)          => Return [1, 2]
--    End_Scope
--
--  This accumulation can be disabled by passing False for the Deep argument
--  of Get_List.
--
--  As a side effect, getting a parameter in a list context always returns
--  a value.  If the parameter is not defined in any scope, the empty list is
--  returned.
--
--  The Increment is a special update operation and will, if invoked with
--  the Deep option, increment the parameter value in all scopes, e.g.,
--
--    New_Scope
--       Set (N, 1)
--       Get (A)          => Return 1
--       New_Scope
--          Set (N, 1)
--          Get (N)       => Return 1
--          Incr (N)      => 2
--       End_Scope
--       Get (N)          => 2          (N of parent scope also incremented)
--    End_Scope
--

with Ada.Calendar;
with Ada.Wide_Text_IO;
with ZanyBlue.Parameters.Sets;
with ZanyBlue.Parameters.Values;

private with Ada.Containers.Indefinite_Vectors;

package ZanyBlue.Parameters.Scopes is

   use Ada.Calendar;
   use Ada.Wide_Text_IO;
   use ZanyBlue.Parameters.Values;
   use ZanyBlue.Parameters.Sets;

   type Parameter_Stack_Type is tagged private;
   --  The type used to represent scoped parameter values.

   procedure New_Scope (Param_Stack : in out Parameter_Stack_Type);
   --  Create a new scope.  The scope is given the name "PARAMS-NNN" where
   --  NNN is the level of the scope in the stack of scopes.  The name is
   --  normally only seen if the stack is written to an XML file.
   --
   --  The New_Scope function defines a single parameter in the new scope
   --  named "_level" with an integer value of the level with in the stack.
   --

   procedure End_Scope (Param_Stack : in out Parameter_Stack_Type);
   --  Return to the enclosing scope.  Changes to local parameters are lost
   --  and the previous values are made available.

   function Is_Defined (Param_Stack : Parameter_Stack_Type;
                        Name        : Wide_String;
                        Any_Scope   : Boolean := True) return Boolean;
   --  Is a parameter defined, either for the current scope or in one of the
   --  enclosing scopes.  If Any_Scope is False, only the current scope
   --  is checked.

   function Get (Param_Stack : Parameter_Stack_Type;
                 Name        : Wide_String) return Value_Type'Class;
   --  General parameter query routine.  This can be used to handle user
   --  defined parameter types.

   function Get_Boolean (Param_Stack : Parameter_Stack_Type;
                         Name        : Wide_String) return Boolean;
   --  Get the value of a Boolean parameter.  Not_A_Boolean_Error is raised
   --  if the value is not a Boolean.

   function Get_Float (Param_Stack : Parameter_Stack_Type;
                       Name        : Wide_String) return Float;
   --  Get the value of a Float parameter.  Not_A_Real_Error is raised
   --  if the value is not a Float.

   function Get_Integer (Param_Stack : Parameter_Stack_Type;
                         Name        : Wide_String) return Integer;
   --  Get the value of an Integer parameter.  Not_An_Integer_Error is raised
   --  if the value is not a Integer.

   function Get_List (Param_Stack : Parameter_Stack_Type;
                      Name        : Wide_String;
                      Deep        : Boolean) return List_Type;
   --  Get the value of a List parameter.  If Deep is False, only the current
   --  scope is queried.  If Deep is True, the all scopes are queried and the
   --  the result is an accumulation of all values found.

   function Get_Time (Param_Stack : Parameter_Stack_Type;
                      Name        : Wide_String) return Time;
   --  Get the value of a Time parameter.  Not_A_Time_Error is raised
   --  if the value is not a Time.

   function Get_String (Param_Stack : Parameter_Stack_Type;
                        Name        : Wide_String) return Wide_String;
   --  Get the value of a String parameter.  All types support conversion to
   --  string so a value is always returned unless the parameter is not
   --  defined (Not_Defined_Error is raised in this case).

   procedure Increment (Param_Stack : in out Parameter_Stack_Type;
                        Name        : Wide_String;
                        By_Amount   : Integer := 1;
                        Deep        : Boolean := True);
   --  Increment an Integer parameter.  If Deep is True, then all instances
   --  of the parameter are increment, i.e., all scopes.

   procedure Append (Param_Stack : in out Parameter_Stack_Type;
                     Name        : Wide_String;
                     Value       : Wide_String);
   --  Append a value to the list parameter.  If the current parameter is not
   --  a list value, it converted (Get_String of the value) to a list prior to
   --  appending.

   procedure Prepend (Param_Stack : in out Parameter_Stack_Type;
                      Name        : Wide_String;
                      Value       : Wide_String);
   --  Prepend a value to the list parameter.  If the current parameter is not
   --  a list value, it converted (Get_String of the value) to a list prior to
   --  prepending.

   procedure Set (Param_Stack : in out Parameter_Stack_Type;
                  Name        : Wide_String;
                  Value       : Value_Type'Class);
   --  General parameter set routine.  This can be used to handle user
   --  defined parameter types.

   procedure Set_Boolean (Param_Stack : in out Parameter_Stack_Type;
                          Name        : Wide_String;
                          Value       : Boolean);
   --  Set a parameter to a Boolean value.

   procedure Set_Integer (Param_Stack : in out Parameter_Stack_Type;
                          Name        : Wide_String;
                          Value       : Integer);
   --  Set a parameter to an Integer value.

   procedure Set_Float (Param_Stack : in out Parameter_Stack_Type;
                        Name        : Wide_String;
                        Value       : Float);
   --  Set a parameter to a Float value.

   procedure Set_String (Param_Stack : in out Parameter_Stack_Type;
                         Name        : Wide_String;
                         Value       : Wide_String);
   --  Set a parameter to a String value.

   procedure Set_Time (Param_Stack : in out Parameter_Stack_Type;
                       Name        : Wide_String;
                       Value       : Time);
   --  Set a parameter to a Time value.

   procedure Dump (Param_Stack : Parameter_Stack_Type;
                   Destination : File_Type;
                   All_Scopes  : Boolean);
   --  Dump the set of parameters defined in a parameter set to a file.  This
   --  is a debugging routine: no XML quoting is currently performed on the
   --  parameter values so the general XML might not be valid XML!

private

   package Parameter_Set_Vectors is
      new Ada.Containers.Indefinite_Vectors (
             Index_Type => Positive,
             Element_Type => Parameter_Set_Type);

   type Parameter_Stack_Type is tagged
      record
         Values : Parameter_Set_Vectors.Vector;
      end record;

end ZanyBlue.Parameters.Scopes;
