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

with Ada.Calendar;
with Ada.Wide_Text_IO;
with ZanyBlue.Parameters.Values;

private with ZanyBlue.Text;
private with Ada.Containers.Indefinite_Hashed_Maps;

package ZanyBlue.Parameters.Sets is

   use Ada.Calendar;
   use Ada.Wide_Text_IO;
   use ZanyBlue.Parameters.Values;

   type Parameter_Set_Type is tagged private;

   function Number_Of_Parameters (Params : in Parameter_Set_Type)
      return Natural;
   --  Return the number of parameters defined in a parameter set.

   procedure Clear (Params : in out Parameter_Set_Type);
   --  Clear any parameters defined in the parameter set.

   procedure Append (Params : in out Parameter_Set_Type;
                     Name   : in Wide_String;
                     Value  : in Wide_String);
   --  Append a string to a list value.  The list value is created if it
   --  doesn't already exist.

   procedure Prepend (Params : in out Parameter_Set_Type;
                      Name   : in Wide_String;
                      Value  : in Wide_String);
   --  Prepend a string to a list value.  The list value is created if it
   --  doesn't already exist.

   function Get (Params : in Parameter_Set_Type;
                 Name   : in Wide_String) return Value_Type'Class;
   --  General parameter query routine.  This can be used to handle user
   --  defined parameter types.

   function Get_Boolean (Params : in Parameter_Set_Type;
                         Name   : in Wide_String) return Boolean;
   --  Get the value of boolean parameter.  Both Not_A_Boolean_Error and
   --  Not_Defined_Error can be raised.

   function Get_Float (Params : in Parameter_Set_Type;
                       Name   : in Wide_String) return Float;
   --  Get the value of floating point parameter.  Both Not_A_Real_Error and
   --  Not_Defined_Error can be raised.

   function Get_Integer (Params : in Parameter_Set_Type;
                         Name   : in Wide_String) return Integer;
   --  Return the value of an integer parameter.  Both Not_An_Integer and
   --  Not_Defined_Error can be raised.

   function Get_List (Params : in Parameter_Set_Type;
                      Name   : in Wide_String) return List_Type;
   --  Return the value of a list parameter.  An Not_Defined_Error can be
   --  raised.

   function Get_String (Params : in Parameter_Set_Type;
                        Name   : in Wide_String) return Wide_String;
   --  Return the value of a string parameter.  Not_Defined_Error can be
   --  raised.

   function Get_Time (Params : in Parameter_Set_Type;
                      Name   : in Wide_String) return Time;
   --  Return the value of a time parameter.  Not_Defined_Error can be
   --  raised.

   function Get_Name (Params : in Parameter_Set_Type) return Wide_String;
   --  Return the name associated with the parameter set.  This the empty
   --  string, by default, but can be set using the Set_Name routine.

   procedure Increment (Params    : in out Parameter_Set_Type;
                        Name      : in Wide_String;
                        By_Amount : in Integer := 1);
   --  Increment the value of an integer parameter.  Both Not_An_Integer and
   --  Not_Defined_Error can be raised.

   function Is_Defined (Params : in Parameter_Set_Type;
                        Name   : in Wide_String) return Boolean;
   --  Determine if a parameter is defined in the parameter set.

   procedure Set_Name (Params : in out Parameter_Set_Type;
                       Name   : in Wide_String);
   --  Set the name associated with a parameter set.

   procedure Set (Params : in out Parameter_Set_Type;
                  Name   : in Wide_String;
                  Value  : in Value_Type'Class);
   --  General parameter set routine.  This can be used to support user
   --  defined parameter types.

   procedure Set_Boolean (Params : in out Parameter_Set_Type;
                          Name   : in Wide_String;
                          Value  : in Boolean);
   --  Set a boolean parameter value.

   procedure Set_Float (Params : in out Parameter_Set_Type;
                        Name   : in Wide_String;
                        Value  : in Float);
   --  Set a floating point parameter value.

   procedure Set_Integer (Params : in out Parameter_Set_Type;
                          Name   : in Wide_String;
                          Value  : in Integer);
   --  Set an integer parameter value.

   procedure Set_Time (Params : in out Parameter_Set_Type;
                       Name   : in Wide_String;
                       Value  : in Time);
   --  Set a time parameter value.

   procedure Set_String (Params : in out Parameter_Set_Type;
                         Name   : in Wide_String;
                         Value  : in Wide_String);
   --  Set a string parameter value.

   function Type_Name (Params : in Parameter_Set_Type;
                       Name   : in Wide_String) return Wide_String;
   --  Return the type name a parameter.  Not_Defined_Error can be
   --  raised.

   procedure Dump (Params      : in Parameter_Set_Type;
                   Destination : in File_Type;
                   Level       : in Natural := 0);
   --  Write the set of parameters defined in a parameter set to a file.  This
   --  is a debugging routine.

private

   function Equivalent_Keys (Left, Right : in Wide_String) return Boolean;
   --  Key matching function to support the Hashed_Map data structure.

   package Params_Hash_Map is
      new Ada.Containers.Indefinite_Hashed_Maps (
             Key_Type => Wide_String,
             Element_Type => Value_Type'Class,
             Hash => ZanyBlue.Text.Wide_Hash,
             Equivalent_Keys => Equivalent_Keys);
   --  Parameters are stored as a simple Hashed_Map.

   type Parameter_Set_Type is tagged
      record
         Name   : Unbounded_Wide_String;
         Values : Params_Hash_Map.Map;
      end record;
   --  The parameter set is a name and the set of values.

end ZanyBlue.Parameters.Sets;
