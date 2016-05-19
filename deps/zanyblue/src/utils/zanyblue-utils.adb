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

with Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Fixed;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Version_Status_Arguments;

package body ZanyBlue.Utils is

   use Ada.Strings.Wide_Maps;
   use Ada.Strings.Wide_Fixed;
   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Version_Status_Arguments;

   Mapping : constant Wide_Character_Mapping := To_Mapping (
                                           "ABCDEFGHIJKLMNOPQRSTUVWXYZ.",
                                           "abcdefghijklmnopqrstuvwxyz-");
   --  Lower case alpha characters, map '.' to '-' for GNAT style naming

   function Gnat_Source_Name (Package_Name : Wide_String)
      return Wide_String;
   --  Return the GNAT externally formatted (file name) for a package.

   ------------
   -- Banner --
   ------------

   function Banner (Facility_Name      : Wide_String;
                    Banner_Message     : Wide_String := "00001";
                    Copyright_Message  : Wide_String := "00002";
                    Catalog            : Catalog_Type := Standard_Catalog)
      return Time
   is

      Arguments  : ZanyBlue.Text.Arguments.Argument_List;
      Start_Time : Time;

   begin
      Start_Time := Clock;
      Append (Arguments, +ZanyBlue.Version_Major);
      Append (Arguments, +ZanyBlue.Version_Minor);
      Append (Arguments, +ZanyBlue.Version_Patch);
      Append (Arguments, +ZanyBlue.Version_Status);
      Append (Arguments, +ZanyBlue.Revision);
      Append (Arguments, +Start_Time);
      Print_Line (Facility_Name, Banner_Message, Arguments,
                  Catalog => Catalog);
      Print_Line (Facility_Name, Copyright_Message,
                  Argument0 => +ZanyBlue.Copyright_Year,
                  Catalog   => Catalog);
      return Start_Time;
   end Banner;

   --------------------
   -- Body_File_Name --
   --------------------

   function Body_File_Name (Package_Name : Wide_String;
                            Style        : Source_Naming_Style_Type)
      return Wide_String
   is
   begin
      case Style is
      when GNAT_Naming_Style =>
         return Gnat_Source_Name (Package_Name) & ".adb";
      end case;
   end Body_File_Name;

   ----------------------
   -- Gnat_Source_Name --
   ----------------------

   function Gnat_Source_Name (Package_Name : Wide_String) return Wide_String
   is
   begin
      --  Lowercase the package name and replace periods with dashes.
      return Translate (Package_Name, Mapping);
   end Gnat_Source_Name;

   --------------------
   -- Spec_File_Name --
   --------------------

   function Spec_File_Name (Package_Name : Wide_String;
                            Style        : Source_Naming_Style_Type)
      return Wide_String
   is
   begin
      case Style is
      when GNAT_Naming_Style =>
         return Gnat_Source_Name (Package_Name) & ".ads";
      end case;
   end Spec_File_Name;

   -------------
   -- Trailer --
   -------------

   procedure Trailer (Facility_Name     : Wide_String;
                      Start_Time        : Ada.Calendar.Time;
                      Trailer_Message   : Wide_String := "00003";
                      Catalog           : Catalog_Type := Standard_Catalog)
   is

      Now     : constant Time := Clock;
      Elapsed : constant Duration := Now - Start_Time;

   begin
      Print_Line (Facility_Name, Trailer_Message,
                  Argument0 => +Now,
                  Argument1 => +Elapsed,
                  Catalog   => Catalog);
   end Trailer;

end ZanyBlue.Utils;
