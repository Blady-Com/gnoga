--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2018, Michael Rohan <mrohan@zanyblue.com>
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

with Ada.Strings.Wide_Wide_Maps;

package body ZanyBlue.Utils is

   use Ada.Strings.Wide_Wide_Maps;

   Mapping : constant Wide_Wide_Character_Mapping :=
     To_Mapping ("ABCDEFGHIJKLMNOPQRSTUVWXYZ.", "abcdefghijklmnopqrstuvwxyz-");
   --  Lower case alpha characters, map '.' to '-' for GNAT style naming

   function Gnat_Source_Name
     (Package_Name : String)
      return String;
   --  Return the GNAT externally formatted (file name) for a package.

   --------------------
   -- Body_File_Name --
   --------------------

   function Body_File_Name
     (Package_Name : String;
      Style        : Source_Naming_Style_Type)
      return String
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

   function Gnat_Source_Name
     (Package_Name : String)
      return String
   is
   begin
      --  Lowercase the package name and replace periods with dashes.
      return Translate (Package_Name, Mapping);
   end Gnat_Source_Name;

   --------------------
   -- Spec_File_Name --
   --------------------

   function Spec_File_Name
     (Package_Name : String;
      Style        : Source_Naming_Style_Type)
      return String
   is
   begin
      case Style is
         when GNAT_Naming_Style =>
            return Gnat_Source_Name (Package_Name) & ".ads";
      end case;
   end Spec_File_Name;

end ZanyBlue.Utils;
