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

with Ada.Integer_Wide_Text_IO;
with ZanyBlue.OS;

package body ZanyBlue.Text.Metrics is

   use Ada.Integer_Wide_Text_IO;
   use ZanyBlue.OS;

   -----------------
   -- Write_Usage --
   -----------------

   procedure Write_Usage (Destination : Ada.Wide_Text_IO.File_Type;
                          Catalog     : Catalog_Type := Standard_Catalog) is

      procedure Iterator (Facility      : Facility_Index_Type;
                          Key           : Key_Index_Type;
                          Locale        : Locale_Index_Type;
                          Source_Locale : Locale_Index_Type;
                          First         : Positive;
                          Last          : Natural;
                          Count         : Natural);

      procedure Iterator (Facility      : Facility_Index_Type;
                          Key           : Key_Index_Type;
                          Locale        : Locale_Index_Type;
                          Source_Locale : Locale_Index_Type;
                          First         : Positive;
                          Last          : Natural;
                          Count         : Natural) is
         pragma Unreferenced (Source_Locale);
         pragma Unreferenced (First);
         pragma Unreferenced (Last);
      begin
         Put (Destination, "  <message facility=" &
                   """" & Get_Facility (Catalog, Facility) & """");
         Put (Destination, " locale=" &
                   """" & Get_Locale_Name (Catalog, Locale) & """");
         Put (Destination, " key=" &
                   """" & Get_Key (Catalog, Key) & """");
         Put (Destination, " count=""");
         Put (Destination, Count, Width => 0);
         Put_Line (Destination, """ />");
      end Iterator;

   begin
      Put_Line (Destination, "<?xml version=""1.0"" encoding=""utf-8""?>");
      Put_Line (Destination, "<zanyblue-message-usage>");
      Iterate (Catalog, Iterator'Access);
      Put_Line (Destination, "</zanyblue-message-usage>");
   end Write_Usage;

   -----------------
   -- Write_Usage --
   -----------------

   procedure Write_Usage (File_Name   : Wide_String;
                          Catalog     : Catalog_Type := Standard_Catalog) is
      Destination : Ada.Wide_Text_IO.File_Type;
   begin
      Wide_Create (Destination, File_Name);
      Write_Usage (Destination, Catalog);
      Close (Destination);
   end Write_Usage;

   -----------------
   -- Write_Usage --
   -----------------

   procedure Write_Usage (File_Name : String;
                          Catalog   : Catalog_Type := Standard_Catalog) is
   begin
      Write_Usage (From_UTF8 (File_Name), Catalog);
   end Write_Usage;

end ZanyBlue.Text.Metrics;
