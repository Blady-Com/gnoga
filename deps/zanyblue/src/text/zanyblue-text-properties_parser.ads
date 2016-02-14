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

with Ada.Wide_Text_IO;
with ZanyBlue.Text.Locales;

package ZanyBlue.Text.Properties_Parser is

   use Ada.Wide_Text_IO;
   use ZanyBlue.Text.Locales;

   type Parser_Handler_Type is abstract tagged private;

   procedure Add_Key_Value (Handler       : in out Parser_Handler_Type;
                            Facility      : Wide_String;
                            Key           : Wide_String;
                            Value         : Wide_String;
                            Locale        : Locale_Type;
                            Source_Locale : Locale_Type;
                            File_Name     : Wide_String;
                            Line          : Natural)
      is abstract;
   --  Call back to handle the definition of a key/value pair.

   procedure Duplicate_Key (Handler       : in out Parser_Handler_Type;
                            Facility      : Wide_String;
                            Key           : Wide_String;
                            Locale        : Locale_Type;
                            File_Name     : Wide_String;
                            Current_Line  : Natural;
                            Previous_Line : Natural)
      is abstract;
   --  Call back used to report a duplicate key error.

   procedure Invalid_Character (Handler         : in out Parser_Handler_Type;
                                Facility        : Wide_String;
                                File_Name       : Wide_String;
                                Current_Line    : Natural;
                                Ch              : Character)
      is abstract;
   --  Call back used to report an invalid character, non-ISO-646, in the
   --  source properties file.

   procedure Invalid_Definition (Handler         : in out Parser_Handler_Type;
                                 Facility        : Wide_String;
                                 Locale          : Locale_Type;
                                 File_Name       : Wide_String;
                                 Current_Line    : Natural;
                                 Additional_Info : String)
      is abstract;
   --  Call back used to report an invalid definition.

   function Get_N_Messages (Handler : Parser_Handler_Type) return Natural;
   --  Return the number of messages parsed.

   procedure Reset_N_Messages (Handler : in out Parser_Handler_Type);
   --  Reset the number of messages parse (used when re-using a parser).

   function Get_N_Errors (Handler : Parser_Handler_Type) return Natural;
   --  Get the number of error encountered during the parsing of a file.

   procedure Increment_Errors (Handler   : in out Parser_Handler_Type;
                               By_Amount : Natural := 1);
   --  Increment the number of errors associated with the properies files
   --  parsed.

   procedure Parse (Handler          : in out Parser_Handler_Type'Class;
                    File_Name        : Wide_String;
                    Facility         : Wide_String;
                    Locale           : Locale_Type);
   --  Parse a properties file using the calling back via the Handler.

   procedure Parse (Handler       : in out Parser_Handler_Type'Class;
                    File_Name     : Wide_String;
                    Facility      : Wide_String;
                    Locale        : Locale_Type;
                    Source_Locale : Locale_Type);
   --  Parse a properties file using the calling back via the Handler to
   --  process message definitions and errors (see the abstract methods
   --  above).

   procedure Parse (Handler          : in out Parser_Handler_Type'Class;
                    Source_File      : in out File_Type;
                    File_Name        : Wide_String;
                    Facility         : Wide_String;
                    Locale           : Locale_Type);
   --  Same as Parse but on an already open file handle.

   procedure Parse (Handler       : in out Parser_Handler_Type'Class;
                    Source_File   : in out File_Type;
                    File_Name     : Wide_String;
                    Facility      : Wide_String;
                    Locale        : Locale_Type;
                    Source_Locale : Locale_Type);
   --  Same as Parse but on an already open file handle.

private

   type Parser_Handler_Type is abstract tagged
      record
         N_Messages : Natural := 0;
         N_Errors   : Natural := 0;
      end record;

   procedure Increment_Messages (Handler : in out Parser_Handler_Type);
   --  Increment the number of messages parsed.

end ZanyBlue.Text.Properties_Parser;
