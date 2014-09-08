------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . N A V I G A T O R                         --
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
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Gnoga.Connections;

package body Gnoga.Navigator is

   ---------------
   -- Code_Name --
   ---------------

   function Code_Name (ID : Gnoga.Types.Connection_ID) return String is
   begin
      return Gnoga.Connections.Execute_Script (ID, "navigator.appCodeName");
   end Code_Name;

   ----------
   -- Name --
   ----------

   function Name (ID : Gnoga.Types.Connection_ID) return String is
   begin
      return Gnoga.Connections.Execute_Script (ID, "navigator.appName");
   end Name;

   -------------
   -- Version --
   -------------

   function Version (ID : Gnoga.Types.Connection_ID) return String is
   begin
      return Gnoga.Connections.Execute_Script (ID, "navigator.appVersion");
   end Version;

   ---------------------
   -- Cookies_Enabled --
   ---------------------

   function Cookies_Enabled (ID : Gnoga.Types.Connection_ID) return Boolean is
   begin
      return Gnoga.Connections.Execute_Script (ID, "navigator.appVersion") =
        "true";
   end Cookies_Enabled;

   --------------
   -- Language --
   --------------

   function Language (ID : Gnoga.Types.Connection_ID) return String is
   begin
      return Gnoga.Connections.Execute_Script (ID, "navigator.language");
   end Language;

   --------------
   -- Platform --
   --------------

   function Platform (ID : Gnoga.Types.Connection_ID) return String is
   begin
      return Gnoga.Connections.Execute_Script (ID, "navigator.platform");
   end Platform;

   -------------
   -- Product --
   -------------

   function Product (ID : Gnoga.Types.Connection_ID) return String is
   begin
      return Gnoga.Connections.Execute_Script (ID, "navigator.product");
   end Product;

   ----------------
   -- User_Agent --
   ----------------

   function User_Agent (ID : Gnoga.Types.Connection_ID) return String is
   begin
      return Gnoga.Connections.Execute_Script (ID, "navigator.userAgent");
   end User_Agent;

end Gnoga.Navigator;
