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
--  Root package for the ZanyBlue library.  This is pure package containing
--  function definitions giving version information.
--

with Ada.Wide_Text_IO;

package ZanyBlue.OS is

   type OS_Name_Type is (Unix, Windows);

   procedure Integrity_Check;
   --  Perform OS specific integrity checks on internal data: used only by
   --  the regression tests where the Windows NT LCID table should be verified
   --  it's sorted.

   function OS_Locale_Name return Wide_String;
   --  Operating system defined locale name, e.g., $LANG on Unix.

   function OS_Name return OS_Name_Type;
   --  Return the name of the build operating system.

   function To_UTF8 (Value : in Wide_String) return String;
   --  Convert a Wide_String to a UTF-8 encoded String.

   function From_UTF8 (Value : in String) return Wide_String;
   --  Convert a UTF-8 encoded String to a Wide_String;

   function UTF8_File_Form return String;
   --  Return the form string used by the compiler to identify an UTF-8 file.

   procedure Wide_Copy_Tree (Source_Name : in Wide_String;
                             Target_Name : in Wide_String);
   --  Copy a directory tree.

   procedure Wide_Create (File : in out Ada.Wide_Text_IO.File_Type;
                          Name : in Wide_String);
   --  Create a text with UTF8 encoding files.

   procedure Wide_Open (File : in out Ada.Wide_Text_IO.File_Type;
                        Mode : in Ada.Wide_Text_IO.File_Mode;
                        Name : in Wide_String);
   --  Create a text with UTF8 encoding files.

   function Wide_Is_Directory (Name : Wide_String) return Boolean;
   --  Is the named path a directory?

   function Wide_Is_File (Name : Wide_String) return Boolean;
   --  Is the named path a file?

   function Wide_Is_Executable_File (Name : Wide_String) return Boolean;
   --  Is the named path a file?

end ZanyBlue.OS;
