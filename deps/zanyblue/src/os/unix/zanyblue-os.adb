--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2017, Michael Rohan <mrohan@zanyblue.com>
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
with Ada.Environment_Variables;
with ZanyBlue.Directories;

package body ZanyBlue.OS is

   use Ada.Environment_Variables;
   use ZanyBlue.Directories;

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

   function OS_Locale_Name return String is
   begin
      if Ada.Environment_Variables.Exists (To_UTF_8 (Lang_Environment_Name))
      then
         return From_UTF_8 (Value (To_UTF_8 (Lang_Environment_Name)));
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

   -----------------
   -- OS_New_Line --
   -----------------

   function OS_New_Line return String is
   begin
      return From_Latin_1 (ASCII.LF);
   end OS_New_Line;

   --------------------
   -- UTF8_File_Form --
   --------------------

   function UTF8_File_Form return String is
   begin
      return "WCEM=8";
   end UTF8_File_Form;

   ---------------
   -- Copy_Tree --
   ---------------

   procedure Copy_Tree
     (Source_Name : String;
      Target_Name : String)
   is

      procedure Process_Entry
        (Path : String;
         Elem : String;
         Kind : File_Kind);

      procedure Process_Entry
        (Path : String;
         Elem : String;
         Kind : File_Kind)
      is
         Dest_Path : constant String := Compose (Target_Name, Elem);
         use all type File_Kind;
      begin
         if Elem.Length = 0 or else Elem (Elem.First) = '.' then
            return;
         end if;
         case Kind is
            when Ordinary_File =>
               Copy_File (Path, Dest_Path);
            when Directory =>
               Copy_Tree (Path, Dest_Path);
            when others =>
               null;
         end case;
      end Process_Entry;

      Item   : Directory_Entry_Type;
      Search : Search_Type;

   begin
      Create_Directory (Target_Name);
      Start_Search (Search, Source_Name, "*");
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Item);
         Process_Entry (Full_Name (Item), Simple_Name (Item), Kind (Item));
      end loop;
      End_Search (Search);
   end Copy_Tree;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out UXStrings.Text_IO.File_Type;
      Name :        String)
   is
      use UXStrings.Text_IO;
   begin
      Create (File, Mode => Out_File, Name => Name, Scheme => UTF_8);
   end Create;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory
     (Name : String)
      return Boolean
   is
      use all type File_Kind;
   begin
      return Exists (Name) and then Kind (Name) = Directory;
   end Is_Directory;

   ------------------------
   -- Is_Executable_File --
   ------------------------

   function Is_Executable_File
     (Name : String)
      return Boolean
   is
   begin
      return
        Is_File (Name)
        and then GNAT.OS_Lib.Is_Executable_File (To_UTF_8 (Name));
   end Is_Executable_File;

   -------------
   -- Is_File --
   -------------

   function Is_File
     (Name : String)
      return Boolean
   is
      use all type File_Kind;
   begin
      return Exists (Name) and then Kind (Name) = Ordinary_File;
   end Is_File;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out UXStrings.Text_IO.File_Type;
      Mode :        UXStrings.Text_IO.File_Mode;
      Name :        String)
   is
      use UXStrings.Text_IO;
   begin
      Open (File, Mode => Mode, Name => Name, Scheme => UTF_8);
   end Open;

end ZanyBlue.OS;
