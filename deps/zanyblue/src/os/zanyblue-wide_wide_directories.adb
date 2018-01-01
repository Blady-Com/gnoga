--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2017, Michael Rohan <mrohan@zanyblue.com>
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
--  This is a simple wrapper package around the standard Ada.Directories
--  package with Wide_String arguments and functions.  The underlying
--  Strings from Ada.Directories are simply interpreted as UTF-8 encoded
--  strings and are decoded to Wide_Strings.  This, obviously, does not
--  support UTF-8 encoded Wide_Wide_Strings.
--

with ZanyBlue.Text;

package body ZanyBlue.Wide_Wide_Directories is

   use ZanyBlue.Text;
   use Ada.Directories;

   -------------------------
   -- Wide_Wide_Base_Name --
   -------------------------

   function Wide_Wide_Base_Name (Name : Wide_Wide_String)
      return Wide_Wide_String is
   begin
      return Wide_Wide_From_UTF8 (Base_Name (Wide_Wide_To_UTF8 (Name)));
   end Wide_Wide_Base_Name;

   -----------------------
   -- Wide_Wide_Compose --
   -----------------------

   function Wide_Wide_Compose
     (Containing_Directory : Wide_Wide_String := "";
      Name                 : Wide_Wide_String;
      Extension            : Wide_Wide_String := "") return Wide_Wide_String is
   begin
      return Wide_Wide_From_UTF8 (
         Compose (
            Wide_Wide_To_UTF8 (Containing_Directory),
            Wide_Wide_To_UTF8 (Name),
            Wide_Wide_To_UTF8 (Extension)));
   end Wide_Wide_Compose;

   ------------------------------------
   -- Wide_Wide_Containing_Directory --
   ------------------------------------

   function Wide_Wide_Containing_Directory
      (Name : Wide_Wide_String) return Wide_Wide_String is
   begin
      return Wide_Wide_From_UTF8 (
         Containing_Directory (Wide_Wide_To_UTF8 (Name)));
   end Wide_Wide_Containing_Directory;

   -------------------------
   -- Wide_Wide_Copy_File --
   -------------------------

   procedure Wide_Wide_Copy_File
     (Source_Name   : Wide_Wide_String;
      Target_Name   : Wide_Wide_String;
      Form          : Wide_Wide_String := "") is
   begin
      Copy_File (Wide_Wide_To_UTF8 (Source_Name),
                 Wide_Wide_To_UTF8 (Target_Name),
                 Wide_Wide_To_UTF8 (Form));
   end Wide_Wide_Copy_File;

   --------------------------------
   -- Wide_Wide_Create_Directory --
   --------------------------------

   procedure Wide_Wide_Create_Directory
     (New_Directory : Wide_Wide_String;
      Form          : Wide_Wide_String := "") is
   begin
      Create_Directory (Wide_Wide_To_UTF8 (New_Directory),
                        Wide_Wide_To_UTF8 (Form));
   end Wide_Wide_Create_Directory;

   ---------------------------
   -- Wide_Wide_Create_Path --
   ---------------------------

   procedure Wide_Wide_Create_Path
     (New_Directory : Wide_Wide_String;
      Form          : Wide_Wide_String := "") is
   begin
      Create_Path (Wide_Wide_To_UTF8 (New_Directory),
                   Wide_Wide_To_UTF8 (Form));
   end Wide_Wide_Create_Path;

   ---------------------------------
   -- Wide_Wide_Current_Directory --
   ---------------------------------

   function Wide_Wide_Current_Directory return Wide_Wide_String is
   begin
      return Wide_Wide_From_UTF8 (Current_Directory);
   end Wide_Wide_Current_Directory;

   --------------------------------
   -- Wide_Wide_Delete_Directory --
   --------------------------------

   procedure Wide_Wide_Delete_Directory (Directory : Wide_Wide_String) is
   begin
      Delete_Directory (Wide_Wide_To_UTF8 (Directory));
   end Wide_Wide_Delete_Directory;

   ---------------------------
   -- Wide_Wide_Delete_File --
   ---------------------------

   procedure Wide_Wide_Delete_File (Name : Wide_Wide_String) is
   begin
      Delete_File (Wide_Wide_To_UTF8 (Name));
   end Wide_Wide_Delete_File;

   ---------------------------
   -- Wide_Wide_Delete_Tree --
   ---------------------------

   procedure Wide_Wide_Delete_Tree (Directory : Wide_Wide_String) is
   begin
      Delete_Tree (Wide_Wide_To_UTF8 (Directory));
   end Wide_Wide_Delete_Tree;

   --------------------------
   -- Wide_Wide_End_Search --
   --------------------------

   procedure Wide_Wide_End_Search (Search : in out Wide_Wide_Search_Type) is
   begin
      End_Search (Search);
   end Wide_Wide_End_Search;

   ----------------------
   -- Wide_Wide_Exists --
   ----------------------

   function Wide_Wide_Exists (Name : Wide_Wide_String) return Boolean is
   begin
      return Exists (Wide_Wide_To_UTF8 (Name));
   end Wide_Wide_Exists;

   -------------------------
   -- Wide_Wide_Extension --
   -------------------------

   function Wide_Wide_Extension (Name : Wide_Wide_String)
      return Wide_Wide_String is
   begin
      return Wide_Wide_From_UTF8 (Extension (Wide_Wide_To_UTF8 (Name)));
   end Wide_Wide_Extension;

   --------------------
   -- Wide_Wide_Full_Name --
   --------------------

   function Wide_Wide_Full_Name (Name : Wide_Wide_String)
      return Wide_Wide_String is
   begin
      return Wide_Wide_From_UTF8 (Full_Name (Wide_Wide_To_UTF8 (Name)));
   end Wide_Wide_Full_Name;

   -------------------------
   -- Wide_Wide_Full_Name --
   -------------------------

   function Wide_Wide_Full_Name
      (Directory_Entry : Wide_Wide_Directory_Entry_Type)
      return Wide_Wide_String is
   begin
      return Wide_Wide_From_UTF8 (Full_Name (Directory_Entry));
   end Wide_Wide_Full_Name;

   ------------------------------
   -- Wide_Wide_Get_Next_Entry --
   ------------------------------

   procedure Wide_Wide_Get_Next_Entry
     (Search          : in out Wide_Wide_Search_Type;
      Directory_Entry : out Wide_Wide_Directory_Entry_Type) is
   begin
      Get_Next_Entry (Search, Directory_Entry);
   end Wide_Wide_Get_Next_Entry;

   --------------------
   -- Wide_Wide_Kind --
   --------------------

   function Wide_Wide_Kind (Name : Wide_Wide_String)
      return Wide_Wide_File_Kind is
   begin
      return Kind (Wide_Wide_To_UTF8 (Name));
   end Wide_Wide_Kind;

   --------------------
   -- Wide_Wide_Kind --
   --------------------

   function Wide_Wide_Kind
      (Directory_Entry : Wide_Wide_Directory_Entry_Type)
      return Wide_Wide_File_Kind is
   begin
      return Kind (Directory_Entry);
   end Wide_Wide_Kind;

   ---------------------------------
   -- Wide_Wide_Modification_Time --
   ---------------------------------

   function Wide_Wide_Modification_Time
      (Name : Wide_Wide_String) return Ada.Calendar.Time is
   begin
      return Modification_Time (Wide_Wide_To_UTF8 (Name));
   end Wide_Wide_Modification_Time;

   ---------------------------------
   -- Wide_Wide_Modification_Time --
   ---------------------------------

   function Wide_Wide_Modification_Time
      (Directory_Entry : Wide_Wide_Directory_Entry_Type)
      return Ada.Calendar.Time is
   begin
      return Modification_Time (Directory_Entry);
   end Wide_Wide_Modification_Time;

   ----------------------------
   -- Wide_Wide_More_Entries --
   ----------------------------

   function Wide_Wide_More_Entries (Search : Wide_Wide_Search_Type)
      return Boolean is
   begin
      return More_Entries (Search);
   end Wide_Wide_More_Entries;

   ----------------------
   -- Wide_Wide_Rename --
   ----------------------

   procedure Wide_Wide_Rename (Old_Name, New_Name : Wide_Wide_String) is
   begin
      Rename (Wide_Wide_To_UTF8 (Old_Name), Wide_Wide_To_UTF8 (New_Name));
   end Wide_Wide_Rename;

   ----------------------
   -- Wide_Wide_Search --
   ----------------------

   procedure Wide_Wide_Search
     (Directory : Wide_Wide_String;
      Pattern   : Wide_Wide_String;
      Filter    : Wide_Wide_Filter_Type := (others => True);
      Process   : not null access procedure
                            (Directory_Entry : Wide_Wide_Directory_Entry_Type))
   is
   begin
      Search (Wide_Wide_To_UTF8 (Directory),
              Wide_Wide_To_UTF8 (Pattern),
              Filter,
              Process);
   end Wide_Wide_Search;

   -----------------------------
   -- Wide_Wide_Set_Directory --
   -----------------------------

   procedure Wide_Wide_Set_Directory (Directory : Wide_Wide_String) is
   begin
      Set_Directory (Wide_Wide_To_UTF8 (Directory));
   end Wide_Wide_Set_Directory;

   ---------------------------
   -- Wide_Wide_Simple_Name --
   ---------------------------

   function Wide_Wide_Simple_Name (Name : Wide_Wide_String)
      return Wide_Wide_String is
   begin
      return Wide_Wide_From_UTF8 (Simple_Name (Wide_Wide_To_UTF8 (Name)));
   end Wide_Wide_Simple_Name;

   ---------------------------
   -- Wide_Wide_Simple_Name --
   ---------------------------

   function Wide_Wide_Simple_Name
      (Directory_Entry : Wide_Wide_Directory_Entry_Type)
      return Wide_Wide_String is
   begin
      return Wide_Wide_From_UTF8 (Simple_Name (Directory_Entry));
   end Wide_Wide_Simple_Name;

   --------------------
   -- Wide_Wide_Size --
   --------------------

   function Wide_Wide_Size (Name : Wide_Wide_String)
      return Wide_Wide_File_Size is
   begin
      return Size (Wide_Wide_To_UTF8 (Name));
   end Wide_Wide_Size;

   --------------------
   -- Wide_Wide_Size --
   --------------------

   function Wide_Wide_Size
      (Directory_Entry : Wide_Wide_Directory_Entry_Type)
      return Wide_Wide_File_Size is
   begin
      return Size (Directory_Entry);
   end Wide_Wide_Size;

   ----------------------------
   -- Wide_Wide_Start_Search --
   ----------------------------

   procedure Wide_Wide_Start_Search
     (Search    : in out Wide_Wide_Search_Type;
      Directory : Wide_Wide_String;
      Pattern   : Wide_Wide_String;
      Filter    : Wide_Wide_Filter_Type := (others => True)) is
   begin
      Start_Search (Search,
                    Wide_Wide_To_UTF8 (Directory),
                    Wide_Wide_To_UTF8 (Pattern),
                    Filter);
   end Wide_Wide_Start_Search;

end ZanyBlue.Wide_Wide_Directories;
