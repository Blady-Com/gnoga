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

with Ada.Text_IO;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Formatting;
with ZBTest_Messages.ZBTest_Prints;
with ZBTest_Messages.Functions_Prints;

pragma Elaborate_All (ZBTest_Messages.ZBTest_Prints);

package body ZBTest.Functions is

   use Ada.Text_IO;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Formatting;
   use ZBTest_Messages.ZBTest_Prints;
   use ZBTest_Messages.Functions_Prints;

   type Simple_Message_Printer is access procedure
     (Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog);

   type Argument_Message_Printer is access procedure
     (Argument0   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog);

   type Function_Definition is record
      Name           : Wide_String_Access;
      Implementation : Function_Type;
      Usage          : Simple_Message_Printer;
      Summary        : Argument_Message_Printer;
      Help           : Simple_Message_Printer;
   end record;
   --  Information on a text function.

   type Function_List is array (Positive range <>) of Function_Definition;
   --  List of known functions.

   function Find_Index
     (Name : String)
      return Positive;
   --  Return the index in the function table for the named command.  If the
   --  function is not found, 0 is returned.

   function Dirname_Function
     (State : access State_Type;
      Args  : List_Type)
      return String;
   function Joinpaths_Function
     (State : access State_Type;
      Args  : List_Type)
      return String;
   function Nextlog_Function
     (State : access State_Type;
      Args  : List_Type)
      return String;
   function Which_Function
     (State : access State_Type;
      Args  : List_Type)
      return String;

   Function_Table : constant Function_List :=
     ((Name           => new String'("dirname"),
       Implementation => Dirname_Function'Access,
       Usage          => Print_Dirname_Usage'Access,
       Summary        => Print_Dirname_Summary'Access,
       Help           => Print_Dirname_Docstring'Access),

      (Name           => new String'("joinpaths"),
       Implementation => Joinpaths_Function'Access,
       Usage          => Print_Joinpaths_Usage'Access,
       Summary        => Print_Joinpaths_Summary'Access,
       Help           => Print_Joinpaths_Docstring'Access),

      (Name           => new String'("nextlog"),
       Implementation => Nextlog_Function'Access,
       Usage          => Print_Nextlog_Usage'Access,
       Summary        => Print_Nextlog_Summary'Access,
       Help           => Print_Nextlog_Docstring'Access),

      (Name           => new String'("which"),
       Implementation => Which_Function'Access,
       Usage          => Print_Which_Usage'Access,
       Summary        => Print_Which_Summary'Access,
       Help           => Print_Which_Docstring'Access));

   ----------------------
   -- Dirname_Function --
   ----------------------

   function Dirname_Function
     (State : access State_Type;
      Args  : List_Type)
      return String is separate;

   ----------
   -- Find --
   ----------

   function Find
     (Name : String)
      return Function_Type
   is
   begin
      return Function_Table (Find_Index (Name)).Implementation;
   end Find;

   ----------------
   -- Find_Index --
   ----------------

   function Find_Index
     (Name : String)
      return Positive
   is
   begin
      for I in 1 .. Function_Table'Last loop
         if Name = Function_Table (I).Name.all then
            return I;
         end if;
      end loop;
      raise Unknown_Function_Error;
   end Find_Index;

   -------------------
   -- Function_Name --
   -------------------

   function Function_Name
     (Index : Positive)
      return String
   is
   begin
      return Function_Table (Index).Name.all;
   end Function_Name;

   ------------------------
   -- Joinpaths_Function --
   ------------------------

   function Joinpaths_Function
     (State : access State_Type;
      Args  : List_Type)
      return String is separate;

   ----------------------
   -- Nextlog_Function --
   ----------------------

   function Nextlog_Function
     (State : access State_Type;
      Args  : List_Type)
      return String is separate;

   -------------------------
   -- Number_Of_Functions --
   -------------------------

   function Number_Of_Functions return Positive is
   begin
      return Function_Table'Last;
   end Number_Of_Functions;

   -------------------------
   -- Print_Function_Help --
   -------------------------

   procedure Print_Function_Help (Name : String) is
   begin
      Function_Table (Find_Index (Name)).Help.all;
   exception
      when Unknown_Function_Error =>
         Print_10042 (+Name);
   end Print_Function_Help;

   ----------------------------
   -- Print_Function_Summary --
   ----------------------------

   procedure Print_Function_Summary
     (Name  : String;
      Index : Positive)
   is
   begin
      Function_Table (Find_Index (Name)).Summary.all (+Index);
   end Print_Function_Summary;

   --------------------------
   -- Print_Function_Usage --
   --------------------------

   procedure Print_Function_Usage (Name : String) is
   begin
      Function_Table (Find_Index (Name)).Usage.all;
   end Print_Function_Usage;

   --------------------
   -- Which_Function --
   --------------------

   function Which_Function
     (State : access State_Type;
      Args  : List_Type)
      return String is separate;

end ZBTest.Functions;
