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

--
--  Implementation of handling of parameters: simple associative maps
--  of parameter names to various types (String, Integer, Boolean, etc,
--  and a list of strings).  Parameters allow storage and retrieval of
--  values by name.
--

with Ada.Strings.Wide_Unbounded;
with Ada.Containers.Indefinite_Vectors;

package ZanyBlue.Parameters is

   use Ada.Strings.Wide_Unbounded;

   package Word_List_Package is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Unbounded_Wide_String);
   subtype List_Type is Word_List_Package.Vector;
   --  The list of strings type for parameters

   Empty_List : constant List_Type := Word_List_Package.Empty_Vector;
   --  Constant empty list.

   procedure Append
     (List  : in out List_Type;
      Value :        Wide_String);
   --  Append a string to a list type.

   procedure Append
     (List  : in out List_Type;
      Value :        List_Type) renames Word_List_Package.Append;
   --  Append one list to another.

   function Length
     (List : List_Type)
      return Natural;
   --  Return the length of a list.

   function Value
     (List  : List_Type;
      Index : Positive)
      return Wide_String;
   --  Return the value at the given index.  Raises Constraint_Error for
   --  out of bound index values.

   Not_Defined_Error : exception;
   --  Exception raised when accessing a parameter that has not been defined.

   Not_A_Boolean_Error : exception;
   --  Exception raised for a parameter type mis-match.

   Not_An_Integer_Error : exception;
   --  Exception raised for a parameter type mis-match.

   Not_A_Real_Error : exception;
   --  Exception raised for a parameter type mis-match.

   Not_A_Time_Error : exception;
   --  Exception raised for a parameter type mis-match.

   Empty_Parameter_Stack : exception;
   --  Exception raised when ending a parameter scope for an empty parameter
   --  stack.

end ZanyBlue.Parameters;
