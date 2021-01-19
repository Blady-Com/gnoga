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

--
--  This is a simple wrapper package around the standard Ada.Directories
--  package with Wide_String arguments and functions.  The underlying
--  Strings from Ada.Directories are simply interpreted as UTF-8 encoded
--  strings and are decoded to Wide_Strings.  This, obviously, does not
--  support UTF-8 encoded Wide_Wide_Strings.
--

with ZanyBlue.Text;

package body ZanyBlue.Wide_Directories is

   use ZanyBlue.Text;
   use Ada.Directories;

   --------------------
   -- Wide_Base_Name --
   --------------------

   function Wide_Base_Name
     (Name : Wide_String)
      return Wide_String
   is
   begin
      return Wide_From_UTF8 (Base_Name (Wide_To_UTF8 (Name)));
   end Wide_Base_Name;

   ------------------
   -- Wide_Compose --
   ------------------

   function Wide_Compose
     (Containing_Directory : Wide_String := "";
      Name                 : Wide_String;
      Extension            : Wide_String := "")
      return Wide_String
   is
   begin
      return
        Wide_From_UTF8
          (Compose
             (Wide_To_UTF8 (Containing_Directory), Wide_To_UTF8 (Name),
              Wide_To_UTF8 (Extension)));
   end Wide_Compose;

   -------------------------------
   -- Wide_Containing_Directory --
   -------------------------------

   function Wide_Containing_Directory
     (Name : Wide_String)
      return Wide_String
   is
   begin
      return Wide_From_UTF8 (Containing_Directory (Wide_To_UTF8 (Name)));
   end Wide_Containing_Directory;

   --------------------
   -- Wide_Copy_File --
   --------------------

   procedure Wide_Copy_File
     (Source_Name : Wide_String;
      Target_Name : Wide_String;
      Form        : Wide_String := "")
   is
   begin
      Copy_File
        (Wide_To_UTF8 (Source_Name), Wide_To_UTF8 (Target_Name),
         Wide_To_UTF8 (Form));
   end Wide_Copy_File;

   ---------------------------
   -- Wide_Create_Directory --
   ---------------------------

   procedure Wide_Create_Directory
     (New_Directory : Wide_String;
      Form          : Wide_String := "")
   is
   begin
      Create_Directory (Wide_To_UTF8 (New_Directory), Wide_To_UTF8 (Form));
   end Wide_Create_Directory;

   ----------------------
   -- Wide_Create_Path --
   ----------------------

   procedure Wide_Create_Path
     (New_Directory : Wide_String;
      Form          : Wide_String := "")
   is
   begin
      Create_Path (Wide_To_UTF8 (New_Directory), Wide_To_UTF8 (Form));
   end Wide_Create_Path;

   ----------------------------
   -- Wide_Current_Directory --
   ----------------------------

   function Wide_Current_Directory return Wide_String is
   begin
      return Wide_From_UTF8 (Current_Directory);
   end Wide_Current_Directory;

   ---------------------------
   -- Wide_Delete_Directory --
   ---------------------------

   procedure Wide_Delete_Directory (Directory : Wide_String) is
   begin
      Delete_Directory (Wide_To_UTF8 (Directory));
   end Wide_Delete_Directory;

   ----------------------
   -- Wide_Delete_File --
   ----------------------

   procedure Wide_Delete_File (Name : Wide_String) is
   begin
      Delete_File (Wide_To_UTF8 (Name));
   end Wide_Delete_File;

   ----------------------
   -- Wide_Delete_Tree --
   ----------------------

   procedure Wide_Delete_Tree (Directory : Wide_String) is
   begin
      Delete_Tree (Wide_To_UTF8 (Directory));
   end Wide_Delete_Tree;

   ---------------------
   -- Wide_End_Search --
   ---------------------

   procedure Wide_End_Search (Search : in out Wide_Search_Type) is
   begin
      End_Search (Search);
   end Wide_End_Search;

   -----------------
   -- Wide_Exists --
   -----------------

   function Wide_Exists
     (Name : Wide_String)
      return Boolean
   is
   begin
      return Exists (Wide_To_UTF8 (Name));
   end Wide_Exists;

   --------------------
   -- Wide_Extension --
   --------------------

   function Wide_Extension
     (Name : Wide_String)
      return Wide_String
   is
   begin
      return Wide_From_UTF8 (Extension (Wide_To_UTF8 (Name)));
   end Wide_Extension;

   --------------------
   -- Wide_Full_Name --
   --------------------

   function Wide_Full_Name
     (Name : Wide_String)
      return Wide_String
   is
   begin
      return Wide_From_UTF8 (Full_Name (Wide_To_UTF8 (Name)));
   end Wide_Full_Name;

   --------------------
   -- Wide_Full_Name --
   --------------------

   function Wide_Full_Name
     (Directory_Entry : Wide_Directory_Entry_Type)
      return Wide_String
   is
   begin
      return Wide_From_UTF8 (Full_Name (Directory_Entry));
   end Wide_Full_Name;

   -------------------------
   -- Wide_Get_Next_Entry --
   -------------------------

   procedure Wide_Get_Next_Entry
     (Search          : in out Wide_Search_Type;
      Directory_Entry :    out Wide_Directory_Entry_Type)
   is
   begin
      Get_Next_Entry (Search, Directory_Entry);
   end Wide_Get_Next_Entry;

   ---------------
   -- Wide_Kind --
   ---------------

   function Wide_Kind
     (Name : Wide_String)
      return Wide_File_Kind
   is
   begin
      return Kind (Wide_To_UTF8 (Name));
   end Wide_Kind;

   ---------------
   -- Wide_Kind --
   ---------------

   function Wide_Kind
     (Directory_Entry : Wide_Directory_Entry_Type)
      return Wide_File_Kind
   is
   begin
      return Kind (Directory_Entry);
   end Wide_Kind;

   ----------------------------
   -- Wide_Modification_Time --
   ----------------------------

   function Wide_Modification_Time
     (Name : Wide_String)
      return Ada.Calendar.Time
   is
   begin
      return Modification_Time (Wide_To_UTF8 (Name));
   end Wide_Modification_Time;

   ----------------------------
   -- Wide_Modification_Time --
   ----------------------------

   function Wide_Modification_Time
     (Directory_Entry : Wide_Directory_Entry_Type)
      return Ada.Calendar.Time
   is
   begin
      return Modification_Time (Directory_Entry);
   end Wide_Modification_Time;

   -----------------------
   -- Wide_More_Entries --
   -----------------------

   function Wide_More_Entries
     (Search : Wide_Search_Type)
      return Boolean
   is
   begin
      return More_Entries (Search);
   end Wide_More_Entries;

   -----------------
   -- Wide_Rename --
   -----------------

   procedure Wide_Rename (Old_Name, New_Name : Wide_String) is
   begin
      Rename (Wide_To_UTF8 (Old_Name), Wide_To_UTF8 (New_Name));
   end Wide_Rename;

   -----------------
   -- Wide_Search --
   -----------------

   procedure Wide_Search
     (Directory : Wide_String;
      Pattern   : Wide_String;
      Filter    : Wide_Filter_Type := (others => True);
      Process   : not null access procedure
        (Directory_Entry : Wide_Directory_Entry_Type))
   is
   begin
      Search
        (Wide_To_UTF8 (Directory), Wide_To_UTF8 (Pattern), Filter, Process);
   end Wide_Search;

   ------------------------
   -- Wide_Set_Directory --
   ------------------------

   procedure Wide_Set_Directory (Directory : Wide_String) is
   begin
      Set_Directory (Wide_To_UTF8 (Directory));
   end Wide_Set_Directory;

   ----------------------
   -- Wide_Simple_Name --
   ----------------------

   function Wide_Simple_Name
     (Name : Wide_String)
      return Wide_String
   is
   begin
      return Wide_From_UTF8 (Simple_Name (Wide_To_UTF8 (Name)));
   end Wide_Simple_Name;

   ----------------------
   -- Wide_Simple_Name --
   ----------------------

   function Wide_Simple_Name
     (Directory_Entry : Wide_Directory_Entry_Type)
      return Wide_String
   is
   begin
      return Wide_From_UTF8 (Simple_Name (Directory_Entry));
   end Wide_Simple_Name;

   ---------------
   -- Wide_Size --
   ---------------

   function Wide_Size
     (Name : Wide_String)
      return Wide_File_Size
   is
   begin
      return Size (Wide_To_UTF8 (Name));
   end Wide_Size;

   ---------------
   -- Wide_Size --
   ---------------

   function Wide_Size
     (Directory_Entry : Wide_Directory_Entry_Type)
      return Wide_File_Size
   is
   begin
      return Size (Directory_Entry);
   end Wide_Size;

   -----------------------
   -- Wide_Start_Search --
   -----------------------

   procedure Wide_Start_Search
     (Search    : in out Wide_Search_Type;
      Directory :        Wide_String;
      Pattern   :        Wide_String;
      Filter    :        Wide_Filter_Type := (others => True))
   is
   begin
      Start_Search
        (Search, Wide_To_UTF8 (Directory), Wide_To_UTF8 (Pattern), Filter);
   end Wide_Start_Search;

end ZanyBlue.Wide_Directories;
