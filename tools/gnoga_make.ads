------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                           G N O G A _ M A K E                            --
--                                                                          --
--                                 S p e c                                  --
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
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

with Gnoga;

package Gnoga_Make is
   use all type Gnoga.String;

   subtype String is Gnoga.String;

   procedure Version;
   --  Display version information

   procedure Display_Help;
   --  Display command line help for Gnoga_Make

   procedure Display_New_Usage;
   --  Display command line help for 'new' command

   procedure Display_View_Usage;
   --  Display command line help for 'view' command

   procedure Display_Controller_Usage;
   --  Display command line help for 'controller' command

   procedure List_Templates;

   procedure New_Application
     (App_Name          : in String;
      App_Template_Name : in String);
   --  Copy new_application template directory and make appropriate project
   --  name substitutions

   procedure New_View
     (App_Name           : in String;
      View_Name          : in String;
      View_Template_Name : in String);

   procedure New_Controller
     (App_Name                 : in String;
      Controller_Name          : in String;
      Controller_Template_Name : in String);
end Gnoga_Make;
