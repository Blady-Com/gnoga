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

with ZanyBlue.Text.Catalogs;

package ZBMCompile.Codegen is

   use ZanyBlue.Text.Catalogs;

   function Modes_String (Options : in Parameter_Set_Type) return Wide_String;
   --  Depending on the selected mode for flags return either a space string
   --  " " or the string " in ".   This value is subsituted in function and
   --  procedures generated for arguments to explicitly include, or not, the
   --  "in" keyword for arguments.

   function Optimize (Catalog : in Catalog_Type;
                      Options : in Parameter_Set_Type) return Catalog_Type;
   --  Optimize the input catalog by organizing the messages by locale.
   --  For large catalogs, data associated with unreferenced locales might
   --  never be loaded into RAM.

   function Sanitize (Value : in Wide_String) return Wide_String;
   --  Sanitize a string for print replacing any control (non-graphic)
   --  characters with corresponding Unicode representations, if any.

   procedure Write_Commented_Text (File       : in File_Type;
                                   Value      : in Wide_String;
                                   Block_Size : in Positive);
   --  Write a string value to the given file over multiple lines split
   --  first by new line and then by line length.  This routine is used
   --  write the base text associated with a key to the generated source
   --  file.  I.

   procedure Write_Commented_Text_Line (File   : in File_Type;
                                        Value  : in Wide_String;
                                        Block_Size : in Positive);
   --  Write a single line from a base message as a comment to the generated
   --  file.  The line might need to be split into multiple blocks of text
   --  depending on it's lenght (style restriction to 78 character output
   --  line lengths.

end ZBMCompile.Codegen;
