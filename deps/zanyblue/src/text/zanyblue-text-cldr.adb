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

with Ada.Exceptions;
with ZanyBlue.Text.CLDR_Data;
with ZanyBlue.Text.Catalogs;
pragma Elaborate_All (ZanyBlue.Text.Catalogs);

package body ZanyBlue.Text.CLDR is

   use Ada.Exceptions;
   use ZanyBlue.Text.Catalogs;

   CLDR_Catalog : Catalog_Type;

   function Lookup
     (Facility : Wide_String;
      Key      : Wide_String;
      Unknown  : Wide_String;
      Locale   : Locale_Type)
      return Wide_String;
   --  Use the CLDR catalog to locate a key for a locale.  If the key is not
   --  present, simply return the Unknown value rather than raising an
   --  exception.

   ----------------------
   -- Full_Locale_Name --
   ----------------------

   function Full_Locale_Name
     (Value  : Locale_Type;
      Locale : Locale_Type := Current_Locale)
      return Wide_String
   is
   begin
      if Territory (Value) = "" then
         return Language_Name (Language (Value), Locale => Locale);
      end if;
      return
        Language_Name (Language (Value), Locale => Locale) & " (" &
        Territory_Name (Territory (Value), Locale => Locale) & ")";
   end Full_Locale_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not Is_Valid (CLDR_Catalog) then
         CLDR_Catalog := Create;
         ZanyBlue.Text.CLDR_Data.Initialize (Catalog => CLDR_Catalog);
      end if;
   end Initialize;

   -------------------
   -- Language_Name --
   -------------------

   function Language_Name
     (Code    : Wide_String;
      Unknown : Wide_String := "";
      Locale  : Locale_Type := Current_Locale)
      return Wide_String
   is
   begin
      if Code = "" then
         return Lookup ("l", "root", Unknown, Locale);
      else
         return Lookup ("l", Code, Unknown, Locale);
      end if;
   end Language_Name;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Facility : Wide_String;
      Key      : Wide_String;
      Unknown  : Wide_String;
      Locale   : Locale_Type)
      return Wide_String
   is
   begin
      if not Is_Valid (CLDR_Catalog) then
         Raise_Exception
           (Program_Error'Identity,
            Message => "ZanyBlue.Text.CLDR.Initialize not called");
      end if;
      return Get_Text (CLDR_Catalog, Facility, Key, Locale);
   exception
      when No_Such_Key_Error =>
         return Unknown;
   end Lookup;

   -----------------
   -- Script_Name --
   -----------------

   function Script_Name
     (Code    : Wide_String;
      Unknown : Wide_String := "";
      Locale  : Locale_Type := Current_Locale)
      return Wide_String
   is
   begin
      return Lookup ("s", Code, Unknown, Locale);
   end Script_Name;

   --------------------
   -- Territory_Name --
   --------------------

   function Territory_Name
     (Code    : Wide_String;
      Unknown : Wide_String := "";
      Locale  : Locale_Type := Current_Locale)
      return Wide_String
   is
   begin
      return Lookup ("t", Code, Unknown, Locale);
   end Territory_Name;

end ZanyBlue.Text.CLDR;
