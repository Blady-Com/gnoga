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

with GNAT.OS_Lib;
with Ada.Directories;
with Ada.Environment_Variables;
with ZanyBlue.Text;
with ZanyBlue.Wide_Directories;

package body ZanyBlue.OS is

   use Ada.Directories;
   use Ada.Environment_Variables;
   use ZanyBlue.Text;
   use ZanyBlue.Wide_Directories;

   Lang_Environment_Name : constant String := "LANG";
   --  Name of the environment variable defining the locale.

   ---------------------
   -- Integrity_Check --
   ---------------------

   procedure Integrity_Check is
   begin
      --  No integrity checks required on Unix.
      null;
   end Integrity_Check;

   --------------------
   -- OS_Locale_Name --
   --------------------

   function OS_Locale_Name return Wide_String is
   begin
      if Ada.Environment_Variables.Exists (Lang_Environment_Name) then
         return To_Wide_String (Value (Lang_Environment_Name));
      else
         return "en_US.UTF-8";
      end if;
   end OS_Locale_Name;

   -------------
   -- OS_Name --
   -------------

   function OS_Name return OS_Name_Type is
   begin
      return Unix;
   end OS_Name;

   --------------------
   -- UTF8_File_Form --
   --------------------

   function UTF8_File_Form return String is
   begin
      return "WCEM=8";
   end UTF8_File_Form;

   --------------------
   -- Wide_Copy_Tree --
   --------------------

   procedure Wide_Copy_Tree (Source_Name : Wide_String;
                             Target_Name : Wide_String) is

      procedure Process_Entry (Path : String;
                               Elem : String;
                               Kind : File_Kind);

      procedure Process_Entry (Path : String;
                               Elem : String;
                               Kind : File_Kind) is
         Dest_Path : constant Wide_String := Wide_Compose (Target_Name,
                                                           From_UTF8 (Elem));
      begin
         if Elem'Length = 0 or else Elem (Elem'First) = '.' then
            return;
         end if;
         case Kind is
         when Ordinary_File =>
            Wide_Copy_File (From_UTF8 (Path), Dest_Path);
         when Directory =>
            Wide_Copy_Tree (From_UTF8 (Path), Dest_Path);
         when others =>
            null;
         end case;
      end Process_Entry;

      Item   : Directory_Entry_Type;
      Search : Search_Type;

   begin
      Wide_Create_Directory (Target_Name);
      Start_Search (Search, To_UTF8 (Source_Name), "*");
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Item);
         Process_Entry (Full_Name (Item), Simple_Name (Item), Kind (Item));
      end loop;
      End_Search (Search);
   end Wide_Copy_Tree;

   -----------------
   -- Wide_Create --
   -----------------

   procedure Wide_Create (File : in out Ada.Text_IO.File_Type;
                          Name : Wide_String) is
      use Ada.Text_IO;
   begin
      Create (File,
              Mode => Out_File,
              Name => To_UTF8 (Name),
              Form => UTF8_File_Form);
   end Wide_Create;

   -----------------
   -- Wide_Create --
   -----------------

   procedure Wide_Create (File : in out Ada.Wide_Text_IO.File_Type;
                          Name : Wide_String) is
      use Ada.Wide_Text_IO;
   begin
      Create (File,
              Mode => Out_File,
              Name => To_UTF8 (Name),
              Form => UTF8_File_Form);
   end Wide_Create;

   -----------------------
   -- Wide_Is_Directory --
   -----------------------

   function Wide_Is_Directory (Name : Wide_String) return Boolean is
   begin
      return Wide_Exists (Name) and then Kind (To_UTF8 (Name)) = Directory;
   end Wide_Is_Directory;

   -----------------------------
   -- Wide_Is_Executable_File --
   -----------------------------

   function Wide_Is_Executable_File (Name : Wide_String) return Boolean is
   begin
      return Wide_Is_File (Name)
             and then GNAT.OS_Lib.Is_Executable_File (To_UTF8 (Name));
   end Wide_Is_Executable_File;

   ------------------
   -- Wide_Is_File --
   ------------------

   function Wide_Is_File (Name : Wide_String) return Boolean is
   begin
      return Wide_Exists (Name) and then Kind (To_UTF8 (Name)) = Ordinary_File;
   end Wide_Is_File;

   ---------------
   -- Wide_Open --
   ---------------

   procedure Wide_Open (File : in out Ada.Text_IO.File_Type;
                        Mode : Ada.Text_IO.File_Mode;
                        Name : Wide_String) is
      use Ada.Text_IO;
   begin
      Open (File,
            Mode => Mode,
            Name => To_UTF8 (Name),
            Form => UTF8_File_Form);
   end Wide_Open;

   ---------------
   -- Wide_Open --
   ---------------

   procedure Wide_Open (File : in out Ada.Wide_Text_IO.File_Type;
                        Mode : Ada.Wide_Text_IO.File_Mode;
                        Name : Wide_String) is
      use Ada.Wide_Text_IO;
   begin
      Open (File,
            Mode => Mode,
            Name => To_UTF8 (Name),
            Form => UTF8_File_Form);
   end Wide_Open;

end ZanyBlue.OS;
