------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--          G N O G A . A P P L I C A T I O N . S I N G L E T O N           --
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
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Gnoga.Types;
with Gnoga.Gui.Window;

package Gnoga.Application.Singleton is

   --  This package allows for the creation of simple GUI applications
   --  using Gnoga. It allows only a single connection and application
   --  terminates whenever End_Application is called.

   --  See test/singleton.adb for an example.

   procedure Initialize
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Host        : in     String  := "";
      Port        : in     Integer := 8080;
      Boot        : in     String  := "boot.html");
   --  Initialize applicaiton for single connection is Boot for bootstrap html.
   --  If Host = "" then will listen on all interfaces.
   --  Use Host = "locahost" to constrain to local use only.

   procedure Message_Loop;
   --  Remain running until connection to browser lost.

   procedure End_Application;
   --  Terminate application.
   --  This will disconnect application connection to browser and finalize
   --  Gnoga objects, however Gnoga objects on finalization do not destroy
   --  DOM object in browser so browser state will remain with document
   --  after the loop has terminated and application has ended.
end Gnoga.Application.Singleton;
