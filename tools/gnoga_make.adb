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
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
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
        ("command is one of the options below followed by appropriate ");
      Put_Line ("arguments for that command.");
      New_Line;
      Put_Line
        ("Command");
      Put_Line
        ("==========");
      Put_Line
        ("version                               - display version");
      Put_Line
        ("new        app_name [template]        - create a new application");
--      Put_Line
--        ("view       app_name name [template]   - add a new view");
--      Put_Line
--        ("controller app_name name [template]   - add a new controler");
--      New_Line;
--      Put_Line
--       ("With the exception of 'new', gnoga_make commands should be run in");
--      Put_Line
--        ("the applications directory.");
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

   ------------------------
   -- Display_View_Usage --
   ------------------------

   procedure Display_View_Usage is
   begin
      New_Line;
      Put_Line ("usage: " & Ada.Command_Line.Command_Name & " view " &
                  "Application_Name View_Name [template_name] [arguments...]");
      New_Line;
      Put_Line
        ("Creates a new Gnoga View called View_Name for Application_Name.");
      New_Line;
      Put_Line
        ("If template_name is not provided the page_controller template " &
           "will be used.");
      New_Line;
      Put_Line
        ("Templates are located at:");
      Put_Line ("   " & Gnoga_Make_Templates);
      New_Line;
      Put_Line ("Some templates may allow for additional arguments, see the" &
                  " templates's README file for more details.");
   end Display_View_Usage;

   ------------------------------
   -- Display_Controller_Usage --
   ------------------------------

   procedure Display_Controller_Usage is
   begin
      New_Line;
      Put_Line
        ("usage: " & Ada.Command_Line.Command_Name & " view " &
           "Application_Name Controller_Name [template_name] [arguments...]");
      New_Line;
      Put_Line
        ("Creates a new Gnoga Controller called Controller_Name for" &
           " Application_Name.");
      New_Line;
      Put_Line
        ("If template_name is not provided the simple_view template " &
           "will be used.");
      New_Line;
      Put_Line
        ("Templates are located at:");
      Put_Line ("   " & Gnoga_Make_Templates);
      New_Line;
      Put_Line ("Some templates may allow for additional arguments, see the" &
                  " templates's README file for more details.");
   end Display_Controller_Usage;

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

   procedure New_From_Template (App_Name           : in String;
                                Replace_Name       : in String;
                                Name               : in String;
                                Template_Name      : in String);

   procedure New_From_Template (App_Name           : in String;
                                Replace_Name       : in String;
                                Name               : in String;
                                Template_Name      : in String)
   is
      Data              : Gnoga.Server.Template_Parser.View_Data;
      App_Template_Name : String := "app_name";
      App_Lower_Name    : String := Translate (App_Name, Lower_Case_Map);
      Lower_Name        : String := Translate (Name, Lower_Case_Map);

      procedure Create_Tree (Directory_Entry : Directory_Entry_Type);
      --  Copy template tree and parse template files

      function Parse_Name (Name : String) return String;
      --  Replace App_Template_Name and Template_NAme in Name
      --  with application's name, name and remove the .tpl
      --  extension

      function Parse_Name (Name : String) return String is
         P  : Natural := Index (Name, App_Template_Name);
         B  : Ada.Strings.Unbounded.Unbounded_String;
      begin
         if P = 0 then
            B := To_Unbounded_String (Name (Name'First .. Name'Last - 4));
         else
            B := To_Unbounded_String
              (Name (Name'First .. Name'First + P - 2) & Lower_Name &
                 Name (App_Template_Name'Length + P .. Name'Last - 4));
         end if;

         declare
            Name : String := To_String (B);
            P    : Natural := Index (Name, Template_Name);
         begin
            if P = 0 then
               return Name (Name'First .. Name'Last - 4);
            else
               return Name (Name'First .. Name'First + P - 2) & Lower_Name &
                 Name (App_Template_Name'Length + P .. Name'Last - 4);
            end if;
         end;
      end Parse_Name;

      procedure Create_Tree (Directory_Entry : Directory_Entry_Type) is
      begin
         if Kind (Directory_Entry) = Directory and then
           Index (Simple_Name (Directory_Entry), ".") /= 1
         then
            if not Exists (Simple_Name (Directory_Entry)) then
               Create_Directory (Simple_Name (Directory_Entry));
            end if;

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
      Data.Insert ("App_Name_Lower", App_Lower_Name);
      Data.Insert (Replace_Name & "_Name", Name);
      Data.Insert (Replace_Name & "_Name_Lower", Lower_Name);

      Put_Line ("Template Directory : " &
                  Gnoga_Make_Templates & Template_Name);
   end New_From_Template;

   procedure New_View (App_Name           : in String;
                       View_Name          : in String;
                       View_Template_Name : in String)
   is
   begin
      New_From_Template (App_Name,
                         "View",
                         View_Name,
                         View_Template_Name);
   end New_View;

   procedure New_Controller (App_Name                 : in String;
                             Controller_Name          : in String;
                             Controller_Template_Name : in String)
   is
   begin
      New_From_Template (App_Name,
                         "Controller",
                         Controller_Name,
                         Controller_Template_Name);
   end New_Controller;
end Gnoga_Make;
