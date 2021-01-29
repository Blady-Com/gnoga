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
--  package with String arguments and functions.  The underlying
--  Strings from Ada.Directories are simply interpreted as UTF-8 encoded
--  strings and are decoded to UXStrings.
--

package body ZanyBlue.Directories is

   use Ada.Directories;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (Name : String)
      return String
   is
   begin
      return From_UTF_8 (Base_Name (To_UTF_8 (Name)));
   end Base_Name;

   -------------
   -- Compose --
   -------------

   function Compose
     (Containing_Directory : String := "";
      Name                 : String;
      Extension            : String := "")
      return String
   is
   begin
      return
        From_UTF_8
          (Compose
             (To_UTF_8 (Containing_Directory), To_UTF_8 (Name),
              To_UTF_8 (Extension)));
   end Compose;

   --------------------------
   -- Containing_Directory --
   --------------------------

   function Containing_Directory
     (Name : String)
      return String
   is
   begin
      return From_UTF_8 (Containing_Directory (To_UTF_8 (Name)));
   end Containing_Directory;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File
     (Source_Name : String;
      Target_Name : String;
      Form        : String := "")
   is
   begin
      Copy_File
        (To_UTF_8 (Source_Name), To_UTF_8 (Target_Name), To_UTF_8 (Form));
   end Copy_File;

   ----------------------
   -- Create_Directory --
   ----------------------

   procedure Create_Directory
     (New_Directory : String;
      Form          : String := "")
   is
   begin
      Create_Directory (To_UTF_8 (New_Directory), To_UTF_8 (Form));
   end Create_Directory;

   -----------------
   -- Create_Path --
   -----------------

   procedure Create_Path
     (New_Directory : String;
      Form          : String := "")
   is
   begin
      Create_Path (To_UTF_8 (New_Directory), To_UTF_8 (Form));
   end Create_Path;

   -----------------------
   -- Current_Directory --
   -----------------------

   function Current_Directory return String is
   begin
      return From_UTF_8 (Current_Directory);
   end Current_Directory;

   ----------------------
   -- Delete_Directory --
   ----------------------

   procedure Delete_Directory (Directory : String) is
   begin
      Delete_Directory (To_UTF_8 (Directory));
   end Delete_Directory;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Name : String) is
   begin
      Delete_File (To_UTF_8 (Name));
   end Delete_File;

   -----------------
   -- Delete_Tree --
   -----------------

   procedure Delete_Tree (Directory : String) is
   begin
      Delete_Tree (To_UTF_8 (Directory));
   end Delete_Tree;

   ----------------
   -- End_Search --
   ----------------

   procedure End_Search (Search : in out Search_Type) renames
     Ada.Directories.End_Search;

   ------------
   -- Exists --
   ------------

   function Exists
     (Name : String)
      return Boolean
   is
   begin
      return Exists (To_UTF_8 (Name));
   end Exists;

   ---------------
   -- Extension --
   ---------------

   function Extension
     (Name : String)
      return String
   is
   begin
      return From_UTF_8 (Extension (To_UTF_8 (Name)));
   end Extension;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name
     (Name : String)
      return String
   is
   begin
      return From_UTF_8 (Full_Name (To_UTF_8 (Name)));
   end Full_Name;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name
     (Directory_Entry : Directory_Entry_Type)
      return String
   is
   begin
      return From_UTF_8 (Full_Name (Directory_Entry));
   end Full_Name;

   --------------------
   -- Get_Next_Entry --
   --------------------

   procedure Get_Next_Entry
     (Search          : in out Search_Type;
      Directory_Entry :    out Directory_Entry_Type) renames
     Ada.Directories.Get_Next_Entry;

   ----------
   -- Kind --
   ----------

   function Kind
     (Name : String)
      return File_Kind
   is
   begin
      return Kind (To_UTF_8 (Name));
   end Kind;

   ----------
   -- Kind --
   ----------

   function Kind
     (Directory_Entry : Directory_Entry_Type)
      return File_Kind renames Ada.Directories.Kind;

   -----------------------
   -- Modification_Time --
   -----------------------

   function Modification_Time
     (Name : String)
      return Ada.Calendar.Time
   is
   begin
      return Modification_Time (To_UTF_8 (Name));
   end Modification_Time;

   -----------------------
   -- Modification_Time --
   -----------------------

   function Modification_Time
     (Directory_Entry : Directory_Entry_Type)
      return Ada.Calendar.Time renames Ada.Directories.Modification_Time;

   ------------------
   -- More_Entries --
   ------------------

   function More_Entries
     (Search : Search_Type)
      return Boolean renames Ada.Directories.More_Entries;

   ------------
   -- Rename --
   ------------

   procedure Rename (Old_Name, New_Name : String) is
   begin
      Rename (To_UTF_8 (Old_Name), To_UTF_8 (New_Name));
   end Rename;

   ------------
   -- Search --
   ------------

   procedure Search
     (Directory : String;
      Pattern   : String;
      Filter    : Filter_Type := (others => True);
      Process   : not null access procedure
        (Directory_Entry : Directory_Entry_Type))
   is
   begin
      Search (To_UTF_8 (Directory), To_UTF_8 (Pattern), Filter, Process);
   end Search;

   -------------------
   -- Set_Directory --
   -------------------

   procedure Set_Directory (Directory : String) is
   begin
      Set_Directory (To_UTF_8 (Directory));
   end Set_Directory;

   -----------------
   -- Simple_Name --
   -----------------

   function Simple_Name
     (Name : String)
      return String
   is
   begin
      return From_UTF_8 (Simple_Name (To_UTF_8 (Name)));
   end Simple_Name;

   -----------------
   -- Simple_Name --
   -----------------

   function Simple_Name
     (Directory_Entry : Directory_Entry_Type)
      return String
   is
   begin
      return From_UTF_8 (Simple_Name (Directory_Entry));
   end Simple_Name;

   ----------
   -- Size --
   ----------

   function Size
     (Name : String)
      return File_Size
   is
   begin
      return Size (To_UTF_8 (Name));
   end Size;

   ----------
   -- Size --
   ----------

   function Size
     (Directory_Entry : Directory_Entry_Type)
      return File_Size renames Ada.Directories.Size;

   ------------------
   -- Start_Search --
   ------------------

   procedure Start_Search
     (Search    : in out Search_Type;
      Directory :        String;
      Pattern   :        String;
      Filter    :        Filter_Type := (others => True))
   is
   begin
      Start_Search (Search, To_UTF_8 (Directory), To_UTF_8 (Pattern), Filter);
   end Start_Search;

end ZanyBlue.Directories;
