--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2017, Michael Rohan <mrohan@zanyblue.com>
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

with Ada.Containers;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Wide_Text_IO;
with Ada.Characters.Conversions;

package ZanyBlue.Text is

   No_Such_Facility_Error : exception;
   --  Exception raised when accessing a message associated with an undefined
   --  facility.  The name of the unknown facility (or index, if accessing the
   --  message data by indexes) is included as an exception message.

   No_Such_Key_Error : exception;
   --  The exception raised when attempting to query for an unknown key value.
   --  The facility and key values separated by a '/' character are included
   --  in the exception message.  These are index values if accessing the
   --  message data by index.

   No_Such_Locale_Error : exception;
   --  The locale supplied is not referenced in the catalog.  This exception
   --  does not indiciate the locale is an invalid locale name.

   No_Such_Message_Error : exception;
   --  The exception raised when a message cannot be found for a particular
   --  facility, key and local triple, e.g., the key exists in some locale
   --  for the facility but not in the locale required.  The facility and
   --  key values separated by a '/' character are included in the exception
   --  message.  Again, these are index values if accessing the message data
   --  by index.

   No_Such_Argument_Error : exception;
   --  This exception is raised when formatting a message that includes a
   --  reference to an argument beyond the set supplied by the user, e.g.,
   --  referencing argument number 5 when only 2 arguments were supplied.
   --  If formatting exceptions are disabled, then the a reference to the
   --  argument number is included in the formatted string.

   Invalid_Format_Error : exception;
   --  This exception is raised if the format string includes syntax errors
   --  wrt the micro-grammar for references, e.g., an argument reference
   --  does not have a corresponding closing '}'.

   Internal_Error : exception;
   --  The exception raised when an inconsistency is detected and represents
   --  a bug in the implementation.

   Unicode_Escape_Error : exception;
   --  The exception raised when processing Unicode escape sequences in a
   --  string that do not contain hexadecimal values, e.g., "\u00xy" would
   --  raise this error.  The exception is raised with the character that
   --  is not a hexadecimal character.  It can also be raised in the context
   --  of loading a properties file, in which case the exception is raised
   --  with the file name, line number and offending character as colon
   --  separated arguments.

   Unicode_Format_Error : exception;
   --  The exception raised when a Unicode escaped string is converted to
   --  a Wide_String.

   Duplicate_Key_Error : exception;
   --  This exception is raised when a duplicate key definition is encountered
   --  when processing .properties file.  The exception is raised with the
   --  file name, the duplicate key value, the line number for the duplicate
   --  definition and the line number for the original definition as colon
   --  separated arguments, e.g., "myfile.properties:0001: 10: 5".

   Unsupported_Radix_Error : exception;
   --  When formatting floating point values, the value needs to be inspected.
   --  The inspection support machine radix values of 2, 4, 8 or 16.  This
   --  execption is raised if executed on a system that uses a different value.

   Internal_Container_Error : exception;
   --  Internally, the Text library used the Ada.Containers types to store
   --  the message data.  There are some conditions where errors might be
   --  indicated by the Containers library.  If these errors occurs this
   --  exception is raised.  These conditions are all in the category of
   --  "should never happen", e.g., a failure to get after a successful
   --  lookup.

   Pool_Size_Mismatch_Error : exception;
   --  The "zbmcompile" utility compiles messages into Ada sources.  The
   --  string data is accumulated and eventually written to an Ada source
   --  file as the value of a Pool string.  If this string contains non-Latin
   --  characters, the compilation must use options that allow non-Latin
   --  source character, e.g., "-gnatW8" for GNAT.  If not given, the non-Latin
   --  characters are interpreted as two individual characters increasing the
   --  size of the Pool.  If the compiled length of the Pool does not match
   --  length generated by the "zbmcompile" utility, this exception is raised.

   Invalid_Static_Message_Error : exception;
   --  Internal error

   Multiple_Pools_Error : exception;
   --  Internal error

   type Constant_String_Access is access constant Wide_String;

   type Constant_String_List is
     array (Positive range <>) of Constant_String_Access;

   type Static_Message_Pool_Type is access constant Wide_String;

   function Wide_To_UTF8
     (Value : Wide_String)
      return String is
     (String'(Ada.Strings.UTF_Encoding.Wide_Strings.Encode (Value)));
   --  Convert a Wide_String to a UTF-8 encoded String.

   function Wide_From_UTF8
     (Value : String)
      return Wide_String is
     (Ada.Strings.UTF_Encoding.Wide_Strings.Decode
        (Ada.Strings.UTF_Encoding.UTF_8_String'(Value)));
   --  Convert a UTF-8 encoded String to a Wide_String;

   function Wide_Wide_To_UTF8
     (Value : Wide_Wide_String)
      return String is
     (String'(Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Value)));
   --  Convert a Wide_Wide_String to a UTF-8 encoded String.

   function Wide_Wide_From_UTF8
     (Value : String)
      return Wide_Wide_String is
     (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode
        (Ada.Strings.UTF_Encoding.UTF_8_String'(Value)));
   --  Convert a UTF-8 encoded String to a Wide_String;

   function To_UTF8
     (Value : Wide_String)
      return String renames Wide_To_UTF8;
   --  Convert a Wide_String to a UTF-8 encoded String (deprecated).
   pragma Obsolescent
     (Entity => To_UTF8, Message => "Use Wide_To_UTF8 instead of To_UTF8");

   function From_UTF8
     (Value : String)
      return Wide_String renames Wide_From_UTF8;
   --  Convert a UTF-8 encoded String to a Wide_String (deprecated).
   pragma Obsolescent
     (Entity  => From_UTF8,
      Message => "Use Wide_From_UTF8 instead of From_UTF8");

   function Files_Differ
     (Left_File_Name  : Wide_String;
      Right_File_Name : Wide_String)
      return Boolean;
   --  Determine if the contents of two files differ.

   function Wide_Hash
     (Key : Wide_String)
      return Ada.Containers.Hash_Type;
   --  Return the container hash value for a wide string.  Simply use the
   --  standard String hash function on the UTF8 encoded value.

   function To_Wide_String
     (S : String)
      return Wide_String renames Ada.Characters.Conversions.To_Wide_String;
   --  Utility name to convert a String to Wide_String

   procedure Wide_Create_For_Update
     (File : in out Ada.Wide_Text_IO.File_Type;
      Name :        Wide_String);
   --  Create a file.  The file created is a temporary file that is renamed
   --  to the target file on close using Close_And_Update.  The renaming
   --  only occurs if the file contents have changed.  These routines are
   --  used when generating source code: only create a new version of a file
   --  if the contents have changed.

   procedure Close_And_Update
     (File    : in out Ada.Wide_Text_IO.File_Type;
      Updated :    out Boolean);
   --  Close the file and rename to the base name if the contents of the file
   --  have changed.

end ZanyBlue.Text;
