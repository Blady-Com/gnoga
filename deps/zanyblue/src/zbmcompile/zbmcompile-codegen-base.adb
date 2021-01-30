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

with Ada.Wide_Wide_Characters.Unicode;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Utils;
with ZanyBlue.Directories;

package body ZBMCompile.Codegen.Base is

   use ZanyBlue;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Formatting;
   use ZanyBlue.Utils;
   use ZanyBlue.Directories;

   procedure Create_Message_List
     (File    : in out File_Type;
      Catalog :        Catalog_Type;
      Options :        Parameter_Set_Type);
   --  Write the code to add the messages using indexes to vectors already
   --  generated.

   procedure Write_Name_List
     (File    : in out File_Type;
      Catalog :        Catalog_Type;
      Name    :        String;
      Names   :        String;
      N       :        Natural;
      Options :        Parameter_Set_Type;
      Namer   :        access function
        (Catalog : Catalog_Type;
         I       : Positive)
         return String);
   --  Write a list of names to the generated source file, Keys, Locales, etc.

   procedure Write_Query_Decl
     (File    : in out File_Type;
      Name    :        String;
      Options :        Parameter_Set_Type);
   --  Write the declaration for a generated query function which returns
   --  a facility or key name given an index.

   procedure Write_Query_Impl
     (File       : in out File_Type;
      Name       :        String;
      Table_Name :        String;
      Options    :        Parameter_Set_Type);
   --  Write the implementation for a generated query function which returns
   --  a facility or key name given an index.

   procedure Write_String
     (File       : in out File_Type;
      Name       :        String;
      Value      :        String;
      ASCII      :        Boolean;
      Width      :        Positive;
      Decl_Index :        Positive := 1);
   --  Write a string (facility names, keys names, locale names and string
   --  pool) allowing for line over-runs.  The string is written to the file
   --  in a series of sub-strings concatenated together (the output line
   --  length should not exceed 80 for Ada style reasons).  The nested
   --  procedures and functions are defined to allow this sub-stringing
   --  and handle the case of control characters in the string data which
   --  are written on their own lines.

   -------------------------
   -- Create_Message_List --
   -------------------------

   procedure Create_Message_List
     (File    : in out File_Type;
      Catalog :        Catalog_Type;
      Options :        Parameter_Set_Type)
   is

      procedure Add_Message
        (F     : Facility_Index_Type;
         K     : Key_Index_Type;
         L     : Locale_Index_Type;
         EL    : Locale_Index_Type;
         First : Positive;
         Last  : Natural;
         Count : Natural);
      --  Add an individual message definition, handling the case where
      --  the message is the last message in the list, i.e., no comma.

      Pool                 : constant String          := Get_Pool (Catalog);
      N_Messages           : constant Natural := Number_Of_Messages (Catalog);
      First_Last_Message   : Message_Id_Type          := "10017";
      Facility_Key_Message : constant Message_Id_Type := "10018";
      Locale_Message       : constant Message_Id_Type := "10019";
      Last_Message         : constant Message_Id_Type := "10020";
      Current              : Positive                 := 1;

      -----------------
      -- Add_Message --
      -----------------

      procedure Add_Message
        (F     : Facility_Index_Type;
         K     : Key_Index_Type;
         L     : Locale_Index_Type;
         EL    : Locale_Index_Type;
         First : Positive;
         Last  : Natural;
         Count : Natural)
      is

         pragma Unreferenced (Count);

      begin
         Print_Line
           (File, ZBMBase_Facility, First_Last_Message, Argument0 => +Current,
            Argument1 => +First, Argument2 => +Last);
         Print_Line
           (File, ZBMBase_Facility, Facility_Key_Message,
            Argument0 => +Positive (F), Argument1 => +Positive (K));
         Print_Line
           (File, ZBMBase_Facility,
            Select_Message
              (Current < N_Messages, Locale_Message, Last_Message),
            Argument0 => +Positive (L), Argument1 => +Positive (EL));
         if not Options.Get_Boolean ("ascii_only") then
            Write_Commented_Text
              (File, Pool.Slice (First, Last),
               Options.Get_Integer ("comment_size"));
         end if;
         Current := Current + 1;
      end Add_Message;

   begin
      if Options.Get_Boolean ("positional_elements") or else N_Messages = 1
      then
         First_Last_Message := "10016";
      end if;
      Print_Line (File, ZBMBase_Facility, "10015", Argument0 => +N_Messages);
      Iterate (Catalog, Add_Message'Access);
      New_Line (File);
   end Create_Message_List;

   ----------------------
   -- Create_Root_Body --
   ----------------------

   procedure Create_Root_Body
     (Catalog : Catalog_Type;
      Options : Parameter_Set_Type)
   is

      Output_Directory : constant String :=
        Options.Get_String ("output_directory");
      Package_Name : constant String := Options.Get_String ("package");
      Pool         : constant String := Get_Pool (Catalog);
      File_Name    : constant String :=
        Compose
          (Output_Directory, Body_File_Name (Package_Name, GNAT_Naming_Style));
      Updated : Boolean;
      File    : File_Type;

   begin
      Wide_Create_For_Update (File, File_Name);
      Print_Line
        (File, ZBMBase_Facility, "10001", Argument0 => +Version_Major,
         Argument1 => +Version_Minor, Argument2 => +Version_Patch,
         Argument3                                  => +Revision);
      Print_Line (File, ZBMBase_Facility, "10002", Argument0 => +Package_Name);
      if Number_Of_Messages (Catalog) > 0 then
         Print_Line (File, ZBMBase_Facility, "10003");
         --  Write the list of facility variables
         Write_Name_List
           (File, Catalog, "Facility", "Facilities",
            Number_Of_Facilities (Catalog), Options, Get_Facility'Access);
         --  Write the list of key variables
         Write_Name_List
           (File, Catalog, "Key", "Keys", Number_Of_Keys (Catalog), Options,
            Get_Key'Access);
         --  Write the list of locale variables
         Write_Name_List
           (File, Catalog, "Locale", "Locales", Number_Of_Locales (Catalog),
            Options, Get_Locale_Name'Access);
         --  Write the string pool
         Write_String
           (File, "Pool_Data", Pool, Options.Get_Boolean ("ascii_only"),
            Options.Get_Integer ("pool_size"));
         Print_Line (File, ZBMBase_Facility, "10014");
         Create_Message_List (File, Catalog, Options);
      end if;
      Write_Query_Impl (File, "Facility", "Facilities", Options);
      --  Create the Initialize routine
      if Number_Of_Keys (Catalog) > 0 then
         Print_Line
           (File, ZBMBase_Facility, "10027", +Modes_String (Options),
            +Package_Name, +Pool.Length);
      else
         Print_Line
           (File, ZBMBase_Facility, "10028", +Modes_String (Options),
            +Package_Name, +Pool.Length);
      end if;
      Write_Query_Impl (File, "Key", "Keys", Options);
      Print_If
        (Options.Get_Boolean ("body_initialize"), File, ZBMBase_Facility,
         "10029", Argument0 => +Package_Name);
      Print_Line (File, ZBMBase_Facility, "10030", Argument0 => +Package_Name);
      Close_And_Update (File, Updated);
      Print_Line
        (ZBMCompile_Facility, Select_Message (Updated, "V00012", "V00013"),
         Argument0 => +Package_Name, Argument1 => +File_Name);
   end Create_Root_Body;

   ----------------------
   -- Create_Root_Spec --
   ----------------------

   procedure Create_Root_Spec
     (Catalog : Catalog_Type;
      Options : Parameter_Set_Type)
   is

      pragma Unreferenced (Catalog);

      Output_Directory : constant String :=
        Options.Get_String ("output_directory");
      Package_Name : constant String := Options.Get_String ("package");
      File_Name    : constant String :=
        Compose
          (Output_Directory, Spec_File_Name (Package_Name, GNAT_Naming_Style));
      Updated : Boolean;
      File    : File_Type;

   begin
      Wide_Create_For_Update (File, File_Name);
      Print_Line
        (File, ZBMBase_Facility, "00001", Argument0 => +Version_Major,
         Argument1 => +Version_Minor, Argument2 => +Version_Patch,
         Argument3                                  => +Revision);
      Print_Line (File, ZBMBase_Facility, "00002");
      Print_Line (File, ZBMBase_Facility, "00003", Argument0 => +Package_Name);
      Print_If
        (Options.Get_Boolean ("body_initialize"), File, ZBMBase_Facility,
         "00004", Argument0 => +Package_Name);
      Print_Line (File, ZBMBase_Facility, "00006");
      Write_Query_Decl (File, "Facility", Options);
      Write_Query_Decl (File, "Key", Options);
      Print_Line (File, ZBMBase_Facility, "00009", +Modes_String (Options));
      if Options.Get_Boolean ("use_export_name") then
         Print_Line
           (File, ZBMBase_Facility, "00010",
            Argument0 => +Options.Get_String ("export_name"));
      end if;
      New_Line (File);
      Print_Line (File, ZBMBase_Facility, "00011", Argument0 => +Package_Name);
      Close_And_Update (File, Updated);
      Print_Line
        (ZBMCompile_Facility, Select_Message (Updated, "V00014", "V00015"),
         Argument0 => +Package_Name, Argument1 => +File_Name);
   end Create_Root_Spec;

   ---------------------
   -- Write_Name_List --
   ---------------------

   procedure Write_Name_List
     (File    : in out File_Type;
      Catalog :        Catalog_Type;
      Name    :        String;
      Names   :        String;
      N       :        Natural;
      Options :        Parameter_Set_Type;
      Namer   :        access function
        (Catalog : Catalog_Type;
         I       : Positive)
         return String)
   is
   begin
      for I in 1 .. N loop
         Write_String
           (File, Name, Namer (Catalog, I), Options.Get_Boolean ("ascii_only"),
            Options.Get_Integer ("pool_size"), Decl_Index => I);
      end loop;
      Print_Line
        (File, ZBMBase_Facility, "10008", Argument0 => +Names,
         Argument1                                  => +N);
      if N = 0 then
         Print_Line (File, ZBMBase_Facility, "10013");
      elsif N = 1 then
         Print_Line
           (File, ZBMBase_Facility, "10011", Argument0 => +Name,
            Argument1                                  => +N);
      else
         for I in 1 .. N loop
            Print_Line
              (File, ZBMBase_Facility,
               Select_Message
                 (I < N,
                  Select_Message
                    (Options.Get_Boolean ("positional_elements"), "10009",
                     "10010"),
                  Select_Message
                    (Options.Get_Boolean ("positional_elements"), "10011",
                     "10012")),
               Argument0 => +Name, Argument1 => +I);
         end loop;
      end if;
      New_Line (File);
   end Write_Name_List;

   ----------------------
   -- Write_Query_Decl --
   ----------------------

   procedure Write_Query_Decl
     (File    : in out File_Type;
      Name    :        String;
      Options :        Parameter_Set_Type)
   is
      M_String : constant String := Modes_String (Options);
   begin
      Print_Line (File, ZBMBase_Facility, "00007", +Name, +M_String);
      Print_Line (File, ZBMBase_Facility, "00008", +Name);
   end Write_Query_Decl;

   ----------------------
   -- Write_Query_Impl --
   ----------------------

   procedure Write_Query_Impl
     (File       : in out File_Type;
      Name       :        String;
      Table_Name :        String;
      Options    :        Parameter_Set_Type)
   is
      M_String : constant String            := Modes_String (Options);
      Dash     : constant Unicode_Character := '-';
   begin
      Print_Line
        (File, ZBMBase_Facility, "10025", +Name, +M_String, +Table_Name, +Dash,
         +Name.Length);
      Print_Line
        (File, ZBMBase_Facility, "10026", +Name, +Table_Name, +Dash,
         +Name.Length);
   end Write_Query_Impl;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String
     (File       : in out File_Type;
      Name       :        String;
      Value      :        String;
      ASCII      :        Boolean;
      Width      :        Positive;
      Decl_Index :        Positive := 1)
   is

      use Ada.Wide_Wide_Characters.Unicode;

      Buffer           : String;
      Current_Position : Natural := Value.First;

      function Current_Character return Unicode_Character;
      function Current_Character_Pos return Natural;
      procedure Advance;
      function Buffered_Data return String;
      function Finished return Boolean;

      procedure Advance is
      begin
         Current_Position := Current_Position + 1;
      end Advance;

      function Buffered_Data return String is
         I : Positive := Buffer.First;
      begin
         while I < Buffer.Last and then not Finished
           and then not Is_Non_Graphic (Current_Character)
         loop
            Buffer.Replace_Unicode (I, Current_Character);
            Advance;
            if Buffer (I) = '"' then
               I := I + 1;
               Buffer.Replace_Unicode (I, '"');
            end if;
            I := I + 1;
         end loop;
         return Buffer.Slice (Buffer.First, I - 1);
      end Buffered_Data;

      function Current_Character return Unicode_Character is
      begin
         if Finished then
            return Unicode_Character'Val (0);
         end if;
         return Value (Current_Position);
      end Current_Character;

      function Current_Character_Pos return Natural is
      begin
         return Unicode_Character'Pos (Current_Character);
      end Current_Character_Pos;

      function Finished return Boolean is
      begin
         return Current_Position > Value.Length;
      end Finished;

   begin
      Print_Line (File, ZBMBase_Facility, "10004", +Name, +Decl_Index);
      if ASCII then
         for I in Value loop
            Print_Line
              (File, ZBMBase_Facility, "10005",
               +Unicode_Character'Pos (Value (I)));
         end loop;
      else
         while not Finished loop
            if Is_Non_Graphic (Current_Character) then
               Print_Line
                 (File, ZBMBase_Facility, "10005", +Current_Character_Pos);
               Advance;
            else
               Print_Line (File, ZBMBase_Facility, "10006", +Buffered_Data);
            end if;
         end loop;
      end if;
      Print_Line (File, ZBMBase_Facility, "10007");
   end Write_String;

end ZBMCompile.Codegen.Base;
