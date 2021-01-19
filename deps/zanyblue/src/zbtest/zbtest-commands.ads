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

with ZBTest.States;
with ZanyBlue.Parameters;

package ZBTest.Commands is

   use ZBTest.States;
   use ZanyBlue.Parameters;

   Unknown_Command_Error : exception;
   --  Exception raised if an unknown command is entered.

   Command_Usage_Error   : exception;
   --  Exception raised by command implementations if the arguments are
   --  invalid.

   Command_Table_Not_Sorted : exception;
   --  The commands are stored in an internal lookup table which is expected
   --  to be sorted.  If the table is not sorted, i.e., a coding error, then
   --  this exception is raised with the command name as an argument.

   type Command_Type is access procedure (State   : in out State_Type;
                                          Args    : List_Type);
   --  Command are implementated via routines that accept a state and the
   --  list of arguments.  A dispatch table is maintained mapping command
   --  names to implementations (this access type).

   function Number_Of_Commands return Positive;
   --  Return the number of known commands.  This routine is primarily used
   --  by the "help" command to iterate over available commands.

   function Command_Name (Index : Natural) return String;
   --  Given a command index, return the corresponding command name.  This
   --  routine is primarily used by the "help" command to iterate over
   --  available commands.

   function Find (Name : String) return Command_Type;
   --  Given a command name, return the implementation.  Unknown commands
   --  return an implementation that simply raises Unknown_Command_Error.

   procedure Print_Command_Help (Name : String);
   --  Print the help information for a command.

   procedure Print_Command_Usage (Name : String);
   --  Print the usage message for a command.

   procedure Print_Command_Summary (Name  : String;
                                    Index : Positive);
   --  Print the message for the one line summary description associated
   --  with the named command.  The message returned includes a argument
   --  for the command number: the help command lists the summary lines with
   --  a sequence number.

end ZBTest.Commands;
