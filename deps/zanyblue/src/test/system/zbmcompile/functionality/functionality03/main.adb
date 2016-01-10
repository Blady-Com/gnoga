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
with GPS_Text;
with Ada.Wide_Text_IO;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Formatting;

procedure Main is

   use Ada.Wide_Text_IO;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Formatting;

   procedure Dump_Message (Facility : in Wide_String;
                           Key      : in Wide_String;
                           Locale   : in Locale_Type := Root_Locale);

   procedure Dump_Message (Facility : in Wide_String;
                           Key      : in Wide_String;
                           Locale   : in Locale_Type := Root_Locale) is

      Text : constant Wide_String := Get_Text (Standard_Catalog,
                                               Facility,
                                               Key,
                                               Locale);

   begin
      Put_Line (Facility & "/" & Key & " => """ & Text & """");
   end Dump_Message;

   fr : constant Locale_Type := Make_Locale ("fr");

begin
   GPS_Text.Initialize;
   Dump_Message ("Action_Editor",     "00004");
   Dump_Message ("Ada_Module",        "00004");
   Dump_Message ("Aliases",           "00004");
   Dump_Message ("Aunit",             "00004");
   Dump_Message ("Browsers",          "00004");
   Dump_Message ("Builder",           "00004");
   Dump_Message ("Code_Analysis",     "00004");
   Dump_Message ("Codefix",           "00004");
   Dump_Message ("Completion",        "00004");
   Dump_Message ("Cpp",               "00004");
   Dump_Message ("Custom",            "00004");
   Dump_Message ("Docgen2",           "00004");
   Dump_Message ("Gbuilder",          "00004");
   Dump_Message ("GPS",               "00004");
   Dump_Message ("GVD",               "00004");
   Dump_Message ("Help",              "00004");
   Dump_Message ("Kernel",            "00004");
   Dump_Message ("KeyManager",        "00004");
   Dump_Message ("Navigation",        "00004");
   Dump_Message ("Prj_Editor",        "00004");
   Dump_Message ("Python",            "00004");
   Dump_Message ("Refactoring",       "00004");
   Dump_Message ("Remote",            "00004");
   Dump_Message ("Shell",             "00004");
   Dump_Message ("Src_Editor",        "00004");
   Dump_Message ("Theme_Manager",     "00004");
   Dump_Message ("Toolchains_Editor", "00004");
   Dump_Message ("Toolchains",        "00004");
   Dump_Message ("VCS",               "00004");
   Dump_Message ("Vdiff",             "00004");
   Dump_Message ("VFS",               "00004");
   Dump_Message ("Views",             "00004");
   Dump_Message ("Vsearch",           "00004");
   Dump_Message ("Widgets",           "00004");
   Dump_Message ("GPS",               "00015", fr);
   Dump_Message ("Kernel",            "00401", fr);
end Main;
