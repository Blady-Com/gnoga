------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                           G N O G A _ M A K E                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;

with GNAT.OS_Lib;

with Gnoga.Server;
with Gnoga.Server.Template_Parser.Simple;

package body Gnoga_Make is

   Gnoga_Make_Templates : constant String :=
                            Gnoga.Server.Templates_Directory &
                            "gnoga_make" &
                            GNAT.OS_Lib.Directory_Separator;

   -------------
   -- Version --
   -------------

   procedure Version is
   begin
      New_Line;
      Put_Line ("Gnoga_Make Version 0.0 - pre-alpha");
   end Version;

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help is
   begin
      New_Line;
      Put_Line ("usage: " & Ada.Command_Line.Command_Name &
                " command [arguments]");
      New_Line;
      Put_Line
        ("command is one of the options below followed by appropriate " &
         "arguments for that ");
      Put_Line ("command.");
      New_Line;
      Put_Line ("version                 - display version information");
      Put_Line ("new app_name [template] - new application called app_name");
      New_Line;
      Put_Line ("For more help run " & Ada.Command_Line.Command_Name &
                " command with no other arguments.");
      New_Line;
   end Display_Help;

   -----------------------
   -- Display_New_Usage --
   -----------------------

   procedure Display_New_Usage is
   begin
      New_Line;
      Put_Line ("usage: " & Ada.Command_Line.Command_Name & " new " &
                  "Application_Name [template_name] [arguments...]");
      New_Line;
      Put_Line
        ("Creates a new Gnoga application called Application_Name.");
      New_Line;
      Put_Line
        ("If template_name is not provided the multi_connect template " &
           "will be used.");
      New_Line;
      Put_Line
        ("Templates are located at:");
      Put_Line ("   " & Gnoga_Make_Templates);
      New_Line;
      Put_Line ("Some templates may allow for additional arguments, see the" &
                  " templates's README file for more details.");
   end Display_New_Usage;

   ---------------------
   -- New_Application --
   ---------------------

   procedure New_Application (App_Name          : in String;
                              App_Template_Name : in String)
   is
      Data       : Gnoga.Server.Template_Parser.View_Data;
      Lower_Name : String := Translate (App_Name, Lower_Case_Map);

      procedure Create_Tree (Directory_Entry : Directory_Entry_Type);
      --  Copy template tree and parse template files

      function Parse_Name (Name : String) return String;
      --  Replace App_Template_Name in Name with application's name and remove
      --  the .tpl extension

      function Parse_Name (Name : String) return String is
         use Ada.Strings.Fixed;
         P : Natural := Index (Name, App_Template_Name);
      begin
         if P = 0 then
            return Name (Name'First .. Name'Last - 4);
         end if;

         return Name (Name'First .. Name'First + P - 2) & Lower_Name &
            Name (App_Template_Name'Length + P .. Name'Last - 4);
      end Parse_Name;

      procedure Create_Tree (Directory_Entry : Directory_Entry_Type) is
      begin
         if Kind (Directory_Entry) = Directory and then
           Index (Simple_Name (Directory_Entry), ".") /= 1
         then
            Create_Directory (Simple_Name (Directory_Entry));
            Set_Directory (Simple_Name (Directory_Entry));
            Search (Full_Name (Directory_Entry), "",
                    Process => Create_Tree'Access);
            Set_Directory ("..");

         elsif Kind (Directory_Entry) = Ordinary_File then
            if Extension (Simple_Name (Directory_Entry)) = "tpl" then
               Gnoga.Server.Template_Parser.Write_String_To_File
                 (File_Name => Parse_Name (Simple_Name (Directory_Entry)),
                  Value     => Gnoga.Server.Template_Parser.Simple.Load_View
                    (Full_Name (Directory_Entry), Data));
            else
               Copy_File (Full_Name (Directory_Entry),
                          Simple_Name (Directory_Entry));
            end if;
         else
            null;
         end if;
      end Create_Tree;

   begin
      Gnoga.Server.Template_Parser.Set_Template_Directory
        ("");

      Data.Insert ("App_Name", App_Name);
      Data.Insert ("App_Name_Lower", Lower_Name);

      for i in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            n : String := i'Img;
         begin
            Data.Insert ("Argument_" & n (n'First + 1 .. n'Last),
                         Ada.Command_Line.Argument (i));
         end;
      end loop;

      Put_Line ("Template Directory : " &
                  Gnoga_Make_Templates & App_Template_Name);
      Put_Line ("Creating directory : " & Lower_Name);
      Create_Directory (Lower_Name);
      Set_Directory (Lower_Name);
      Search (Directory => Gnoga_Make_Templates & App_Template_Name,
              Pattern   => "",
              Process   => Create_Tree'Access);
   end New_Application;

end Gnoga_Make;
