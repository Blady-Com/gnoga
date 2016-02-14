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

with Ada.Calendar;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Formatting;

package ZanyBlue.Utils is

   use Ada.Calendar;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Formatting;

   type Source_Naming_Style_Type is (GNAT_Naming_Style);

   function Banner (Facility_Name      : Wide_String;
                    Banner_Message     : Wide_String := "00001";
                    Copyright_Message  : Wide_String := "00002";
                    Catalog            : Catalog_Type := Standard_Catalog)
      return Time;
   --  Print a stdandard application banner on startup returning the start
   --  time.

   function Body_File_Name (Package_Name : Wide_String;
                            Style        : Source_Naming_Style_Type)
      return Wide_String;
   --  Return the file name the compiler expects for a body file.

   function Spec_File_Name (Package_Name : Wide_String;
                            Style        : Source_Naming_Style_Type)
      return Wide_String;
   --  Return the file name the compiler expects for a spec file.

   procedure Trailer (Facility_Name     : Wide_String;
                      Start_Time        : Time;
                      Trailer_Message   : Wide_String := "00003";
                      Catalog           : Catalog_Type := Standard_Catalog);
   --  Print the standard "goodbye" message prior to exiting an application.

end ZanyBlue.Utils;
