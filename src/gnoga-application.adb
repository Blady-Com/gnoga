------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                     G N O G A . A P P L I C A T I O N                    --
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
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

with GNAT.OS_Lib;

package body Gnoga.Application is
   App_Name : String := "Gnoga - The GNU Omnificent GUI for Ada";

   HTML_For_On_Close : String;

   Favicon_URL : String;

   ----------------------
   -- Application_Name --
   ----------------------

   procedure Title (Name : in String) is
   begin
      App_Name := Name;
   end Title;

   function Title return String is
   begin
      return App_Name;
   end Title;

   -------------------
   -- HTML_On_Close --
   -------------------

   procedure HTML_On_Close (HTML : String) is
   begin
      HTML_For_On_Close := HTML;
   end HTML_On_Close;

   function HTML_On_Close return String is
   begin
      return HTML_For_On_Close;
   end HTML_On_Close;

   --------------
   -- Open_URL --
   --------------

   function Open_Command return String;
   --  Returns the appropriate command for opening URL regarding the system

   function Open_Command return String is separate;

   procedure Open_URL (URL : String := "http://127.0.0.1:8080") is
      Args : GNAT.OS_Lib.Argument_List_Access;
      PID  : GNAT.OS_Lib.Process_Id;
      pragma Unreferenced (PID);
   begin
      Args := GNAT.OS_Lib.Argument_String_To_List (To_UTF_8 (Open_Command & URL));
      PID  :=
        GNAT.OS_Lib.Non_Blocking_Spawn
          (Program_Name => Args (Args'First).all, Args => Args (Args'First + 1 .. Args'Last));
   end Open_URL;

   -------------------
   -- Favicon --
   -------------------

   procedure Favicon (URL : String) is
   begin
      Favicon_URL := URL;
   end Favicon;

   function Favicon return String is
   begin
      return Favicon_URL;
   end Favicon;

end Gnoga.Application;
