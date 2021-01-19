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
with Ada.Strings.Wide_Unbounded;
with ZanyBlue.Parameters.Values;

private with ZanyBlue.Parameters.Scopes;

package ZBTest.States is

   use Ada.Text_IO;
   use Ada.Strings.Wide_Unbounded;
   use ZanyBlue.Parameters;
   use ZanyBlue.Parameters.Values;

   type State_Type is tagged private;
   --  The state of the test.  State attributes are stored in a parameter
   --  stack.

   File_Not_Found      : exception;
   --  Exception raise when a file is not found, e.g., an executable, a log
   --  file, etc.

   Invalid_Environment : exception;
   --  Exception raise when expected actions cannot be taken, e.g., creating
   --  the test area directory, etc.

   procedure Define_Initial_Parameters (State : in out State_Type);
   --  Define the initial parameters on startup.  This is called one to
   --  initialize the root parameter set.

   procedure Setup_Test_Area (State : in out State_Type);
   --  Setup for testing, create the test area directory

   procedure Read_Eval_Loop (State       : in out State_Type;
                             Input       : File_Type;
                             Interactive : Boolean);
   --  Enter the read-evaluate loop for the tester.  If Interactive, then
   --  a prompt is printed before reading lines.

   procedure Execute_Command (State : in out State_Type;
                              Args  : List_Type);
   --  Execute a single command based on words parsed from an input line.

   procedure Execute_Line (State        : in out State_Type;
                           Input_Line   : String;
                           Interactive  : Boolean);
   --  Execute a single line: parse to words and then execute.  The
   --  Line is then executed by the lower level Execute_Line routine.

   procedure Execute_Line (State        : in out State_Type;
                           Input_Line   : String);
   --  Low level input line execution, parse and dispatch.

   function Locate_Directory (State : State_Type;
                              Name  : String) return String;
   --  Using the search path parameter, locate a directory.

   function Locate_Executable (State : State_Type;
                               Name  : String) return String;
   --  Using the path parameter, locate an executeable.

   function Locate_File (State : State_Type;
                         Name  : String) return String;
   --  Using the search path parameter, locate a file.

   procedure New_Scope (State : in out State_Type);
   --  Begin a new parameter scope, normally used when running a new test
   --  script.

   procedure End_Scope (State : in out State_Type);
   --  Exit (end) a scrope, normally used when a test script complete.

   procedure End_Scope (State  : in out State_Type;
                        N_OK   : out Natural;
                        N_Fail : out Natural);
   --  Exit (end) a scrope, normally used when a test script complete.

   procedure Initialize_Scope (State          : in out State_Type;
                               Script_Dir     : String;
                               Implicit_Scope : Boolean := False);
   --  Initialize a new scope on starting a new test script.

   procedure Register_Failure (State     : in out State_Type;
                               Test_Name : String);
   --  Regisiter a test failure.

   procedure Register_Success (State     : in out State_Type;
                               Test_Name : String);
   --  Regisiter a test success.

   function Full_Test_Name (State : State_Type) return String;
   --  The full test name for the current test.  This includes the enclosing
   --  tests names to generate a path like name, e.g., "app.area1.subarea2".

   function Is_Defined (State     : State_Type;
                        Name      : String;
                        Any_Scope : Boolean := True) return Boolean;
   --  Is a parameter defined, either for the current scope or in one of the
   --  enclosing scopes.  If Any_Scope is False, only the current scope
   --  is checked.

   procedure Expand (State  : in out State_Type;
                     Buffer : in out Unbounded_Wide_String);
   --  Expand parameter and text function references in a string, e.g.,
   --
   --   "The current script is $_testname"
   --   "The zbmcompile executeable is locate at $(which -e zbmcompile)"
   --

   procedure Add_Undo_Action (State  : in out State_Type;
                              Action : String);
   --  Add an "undo" action to be performed when a scope is exited.

   function Get (State  : State_Type;
                 Name   : String) return Value_Type'Class;
   --  Get the value, as a generic boxed value.  Can be used to handle
   --  extended types, e.g., XML nodes.

   function Get_Boolean (State : State_Type;
                         Name  : String) return Boolean;
   --  Get the value of a Boolean parameter.

   function Get_Character (State : State_Type;
                           Name  : String) return Unicode_Character;
   --  Get the value of a character parameter.

   function Get_Float (State : State_Type;
                       Name  : String) return Float;
   --  Get the value of a floating point parameter.

   function Get_Integer (State : State_Type;
                         Name  : String) return Integer;
   --  Get the value of an integer parameter.

   function Get_List (State : State_Type;
                      Name  : String;
                      Deep  : Boolean := True) return List_Type;
   --  Get the value of a list parameter.  If Deep is true, then the values
   --  in all enclosing scopes are appended to the result.

   function Get_String (State : State_Type;
                        Name  : String) return String;
   --  Get the value of a string parameter.

   function Get_Time (State : State_Type;
                      Name  : String) return Ada.Calendar.Time;
   --  Get the value of a time parameter.

   procedure Append (State  : in out State_Type;
                     Name   : String;
                     Value  : String);
   --  Append a value to a list parameter.

   procedure Prepend (State  : in out State_Type;
                      Name   : String;
                      Value  : String);
   --  Prepend a value to a list parameter.

   procedure Increment (State     : in out State_Type;
                        Name      : String;
                        By_Amount : Integer := 1;
                        Deep      : Boolean := True);
   --  Increment an integer parameter.  If Deep is true, then instances
   --  of the parmeter in all scopes is incremented.

   procedure Set (State  : in out State_Type;
                  Name   : String;
                  Value  : Value_Type'Class);
   --  Set the value, as a generic boxed value.  Can be used to handle
   --  extended types, e.g., XML nodes.

   procedure Set_Boolean (State  : in out State_Type;
                          Name   : String;
                          Value  : Boolean);
   --  Set a Boolean value.

   procedure Set_Integer (State  : in out State_Type;
                          Name   : String;
                          Value  : Integer);
   --  Set an integer value.

   procedure Set_Float (State  : in out State_Type;
                        Name   : String;
                        Value  : Float);
   --  Set a floating point value.

   procedure Set_String (State  : in out State_Type;
                         Name   : String;
                         Value  : String);
   --  Set a string value.

   procedure Set_Time (State  : in out State_Type;
                       Name   : String;
                       Value  : Ada.Calendar.Time);
   --  Set a time value.

   procedure Dump (State       : State_Type;
                   File_Name   : String;
                   All_Scopes  : Boolean);
   --  Dump the parameters defined as a simple XML document to the named file.
   --  NOTE: value are not currently XML quoted!

   procedure Dump (State       : State_Type;
                   Destination : File_Type;
                   All_Scopes  : Boolean);
   --  Dump the parameters defined as a simple XML document to the open file.

   procedure Write_XML_Report (State     : State_Type);
   --  Write the XML summary report of the testing.

private

   use ZanyBlue.Parameters.Scopes;

   type State_Type is tagged
   record
      Parameters : Parameter_Stack_Type;
   end record;

end ZBTest.States;
