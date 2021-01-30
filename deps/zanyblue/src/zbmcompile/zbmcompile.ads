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

with Ada.Wide_Text_IO;
with Ada.Containers.Indefinite_Vectors;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Null_Object;
with ZanyBlue.Parameters.Sets;

package ZBMCompile is

   use Ada.Wide_Text_IO;
   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Null_Object;
   use ZanyBlue.Parameters.Sets;

   Output_Pool_Size : constant := 60;
   --  Size of the outputted comment strings for accessors, i.e., the base
   --  message text is split up into block of characters of this size.

   Output_Comment_Size : constant := 55;
   --  Size of comment text added to access routine defintions.  Message text
   --  longer than this will be output on multiple comment lines.

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => String);
   --  The String_Vectors here is used to store the category type associated
   --  with an argument, e.g., "{0,number}" would associated the Number
   --  category with argument 0.  This information is used to verify consistent
   --  usage of arguments within a message and to create type aware accessor
   --  routines.  The empty string is a special case as it is created by
   --  default if arguments are referenced out of sequence, e.g., "There
   --  are {1,number} moons around {0,string}".

   subtype Message_Id_Type is String (1 .. 5);

   Accessor_Exceptions : aliased constant String := "exceptions";
   Accessor_Strings    : aliased constant String := "strings";
   Accessor_WStrings   : aliased constant String := "wstrings";
   Accessor_Prints     : aliased constant String := "prints";
   Accessor_WPrints    : aliased constant String := "wprints";
   Accessor_Types      : constant ZanyBlue.Text.Constant_String_List :=
     (Accessor_Exceptions'Access, Accessor_Strings'Access,
      Accessor_WStrings'Access, Accessor_Prints'Access,
      Accessor_WPrints'Access);
   --  Names assigned to the various accessor type routines generated.  The
   --  names in this list are used to handle arguments to the -G command line
   --  option to select a particular accessor package type and also map to the
   --  code properties files, e.g, "zbmexceptions.properties" use to generate
   --  the accessor code.

   ZBMBase_Facility    : constant String := "zbmbase";
   ZBMCompile_Facility : constant String := "zbmcompile";

   function Process
     (Options : Parameter_Set_Type)
      return Boolean;
   --  Perform the compilation of the .properties files.  Returns True for
   --  success, False for failure.

   function Select_Message
     (Condition : Boolean;
      True_Id   : String;
      False_Id  : String)
      return String;
   --  Depending on Condition, return either the true or false message
   --  id.  This is a simple utility function.

   procedure Print_If
     (Condition : Boolean;
      File      : File_Type;
      Facility  : String;
      Key       : String;
      Argument0 : Argument_Type'Class := Null_Argument;
      Argument1 : Argument_Type'Class := Null_Argument;
      Argument2 : Argument_Type'Class := Null_Argument;
      Argument3 : Argument_Type'Class := Null_Argument;
      Argument4 : Argument_Type'Class := Null_Argument);
   --  Print a message if the given condition is true.

   function Is_Ada_Identifier_OK
     (Name : String)
      return Boolean;
   --  Is the given name a valid Ada identifier name?  Leading digits are
   --  OK as the name will be prefixed with standard prefixes later.

end ZBMCompile;
