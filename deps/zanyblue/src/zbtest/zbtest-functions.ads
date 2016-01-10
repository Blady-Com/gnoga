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

with ZBTest.States;
with ZanyBlue.Parameters;

package ZBTest.Functions is

   use ZBTest.States;
   use ZanyBlue.Parameters;

   Unknown_Function_Error : exception;
   --  Exception raised if an unknown function is used.

   Function_Usage_Error   : exception;
   --  Exception raised by function implementations of the arguments are
   --  invalid.

   type Function_Type is
      access function (State   : access State_Type;
                       Args    : in List_Type) return Wide_String;
   --  Functions are implementated via routines that accept a state and the
   --  list of arguments.  A dispatch table is maintained mapping function
   --  names to implementations (this access type).

   function Number_Of_Functions return Positive;
   --  Return the number of known functions.  This routine is primarily used
   --  by the "help" command to iterate over available functions.

   function Find (Name : in Wide_String) return Function_Type;
   --  Given a function name, return the implementation.  Unknown functions
   --  return an implementation that simply raises Unknown_Function_Error.

   function Function_Name (Index : Positive) return Wide_String;
   --  Return the name of the Index'th function.

   procedure Print_Function_Help (Name : in Wide_String);
   --  Print help information on the named function.

   procedure Print_Function_Usage (Name : Wide_String);
   --  Print the usage message for a function.

   procedure Print_Function_Summary (Name  : in Wide_String;
                                     Index : in Positive);
   --  Print the message for the one line summary description associated
   --  with the named function.  The message returned includes a argument
   --  for the function number: the help command lists the summary lines with
   --  a sequence number.

end ZBTest.Functions;
