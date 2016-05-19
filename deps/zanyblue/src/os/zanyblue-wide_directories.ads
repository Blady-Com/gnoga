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

--
--  This is a simple wrapper package around the standard Ada.Directories
--  package with Wide_String arguments and functions.  The underlying
--  Strings from Ada.Directories are simply interpreted as UTF-8 encoded
--  strings and are decoded to Wide_Strings.  This, obviously, does not
--  support UTF-8 encoded Wide_Wide_Strings.
--

with Ada.Calendar;
with Ada.Directories;
with Ada.IO_Exceptions;

package ZanyBlue.Wide_Directories is

   function Wide_Current_Directory return Wide_String;
   --  Returns the full directory name for the current default directory.

   procedure Wide_Set_Directory (Directory : Wide_String);
   --  Sets the current default directory.

   procedure Wide_Create_Directory (New_Directory : Wide_String;
                                    Form          : Wide_String := "");
   --  Creates a directory with name New_Directory.

   procedure Wide_Delete_Directory (Directory : Wide_String);
   --  Deletes an existing empty directory with name Directory.

   procedure Wide_Create_Path (New_Directory : Wide_String;
                               Form          : Wide_String := "");
   --  Creates zero or more directories with name New_Directory.

   procedure Wide_Delete_Tree (Directory : Wide_String);
   --  Deletes an existing directory with name Directory.

   procedure Wide_Delete_File (Name : Wide_String);
   --  Deletes an existing ordinary or special file with Name.

   procedure Wide_Rename (Old_Name, New_Name : Wide_String);
   --  Renames an existing external file with Old_Name to New_Name.

   procedure Wide_Copy_File (Source_Name   : Wide_String;
                             Target_Name   : Wide_String;
                             Form          : Wide_String := "");
   --  Copies the contents of the existing external file with Source_Name
   --  to Target_Name.

   function Wide_Full_Name (Name : Wide_String) return Wide_String;
   --  Returns the full name corresponding to the file name specified by Name.

   function Wide_Simple_Name (Name : Wide_String) return Wide_String;
   --  Returns the simple name portion of the file name specified by Name.

   function Wide_Containing_Directory (Name : Wide_String) return Wide_String;
   --  Returns the name of the containing directory of the external file
   --  (including directories) identified by Name.

   function Wide_Extension (Name : Wide_String) return Wide_String;
   --  Returns the extension name corresponding to Name.

   function Wide_Base_Name (Name : Wide_String) return Wide_String;
   --  Returns the base name corresponding to Name.

   function Wide_Compose
     (Containing_Directory : Wide_String := "";
      Name                 : Wide_String;
      Extension            : Wide_String := "") return Wide_String;
   --  Returns the name of the external file with the specified
   --  Containing_Directory, Name, and Extension.

   subtype Wide_File_Kind is Ada.Directories.File_Kind;
   --  The type File_Kind represents the kind of file represented by an
   --  external file or directory.

   subtype Wide_File_Size is Ada.Directories.File_Size;
   --  The type File_Size represents the size of an external file

   function Wide_Exists (Name : Wide_String) return Boolean;
   --  Returns True if external file represented by Name exists, and False
   --  otherwise.

   function Wide_Kind (Name : Wide_String) return Wide_File_Kind;
   --  Returns the kind of external file represented by Name.

   function Wide_Size (Name : Wide_String) return Wide_File_Size;
   --  Returns the size of the external file represented by Name.

   function Wide_Modification_Time
      (Name : Wide_String) return Ada.Calendar.Time;
   --  Returns the time that the external file represented by Name was most
   --  recently modified.

   subtype Wide_Directory_Entry_Type is Ada.Directories.Directory_Entry_Type;
   --  The type Directory_Entry_Type represents a single item in a directory.

   subtype Wide_Filter_Type is Ada.Directories.Filter_Type;
   --  The type Filter_Type specifies which directory entries are provided from
   --  a search operation.

   subtype Wide_Search_Type is Ada.Directories.Search_Type;
   --  The type Search_Type contains the state of a directory search.

   procedure Wide_Start_Search
     (Search    : in out Wide_Search_Type;
      Directory : Wide_String;
      Pattern   : Wide_String;
      Filter    : Wide_Filter_Type := (others => True));
   --  Starts a search in the directory entry in the directory named by
   --  Directory for entries matching Pattern. Pattern represents a file name
   --  matching pattern.

   procedure Wide_End_Search (Search : in out Wide_Search_Type);
   --  Ends the search represented by Search.

   function Wide_More_Entries (Search : Wide_Search_Type) return Boolean;
   --  Returns True if more entries are available to be returned by a call
   --  to Get_Next_Entry for the specified search object, and False otherwise.

   procedure Wide_Get_Next_Entry
     (Search          : in out Wide_Search_Type;
      Directory_Entry : out Wide_Directory_Entry_Type);
   --  Returns the next Directory_Entry for the search described by Search that
   --  matches the pattern and filter.

   procedure Wide_Search
     (Directory : Wide_String;
      Pattern   : Wide_String;
      Filter    : Wide_Filter_Type := (others => True);
      Process   : not null access procedure
                             (Directory_Entry : Wide_Directory_Entry_Type));
   --  Searches in the directory named by Directory for entries matching
   --  Pattern.

   function Wide_Simple_Name
      (Directory_Entry : Wide_Directory_Entry_Type) return Wide_String;
   --  Returns the simple external name of the external file (including
   --  directories) represented by Directory_Entry.

   function Wide_Full_Name
      (Directory_Entry : Wide_Directory_Entry_Type) return Wide_String;
   --  Returns the full external name of the external file (including
   --  directories) represented by Directory_Entry.

   function Wide_Kind
      (Directory_Entry : Wide_Directory_Entry_Type) return Wide_File_Kind;
   --  Returns the kind of external file represented by Directory_Entry.

   function Wide_Size
      (Directory_Entry : Wide_Directory_Entry_Type) return Wide_File_Size;
   --  Returns the size of the external file represented by Directory_Entry.

   function Wide_Modification_Time
     (Directory_Entry : Wide_Directory_Entry_Type) return Ada.Calendar.Time;
   --  Returns the time that the external file represented by Directory_Entry
   --  was most recently modified.

   Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
   Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;
   Use_Error    : exception renames Ada.IO_Exceptions.Use_Error;
   Device_Error : exception renames Ada.IO_Exceptions.Device_Error;

end ZanyBlue.Wide_Directories;
