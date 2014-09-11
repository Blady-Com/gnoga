------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . N A V I G A T O R                         --
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
------------------------------------------------------------------------------                                                                          --

with Gnoga.Types;

package Gnoga.Navigator is

   --  Access information about the user's browser

   function Code_Name (ID : Gnoga.Types.Connection_ID) return String;
   --  Always returns Mozilla on all browsers.

   function Name (ID : Gnoga.Types.Connection_ID) return String;
   --  Almost all browsers regardless of brand will return Navigator.

   function Version (ID : Gnoga.Types.Connection_ID) return String;
   --  Most browsers return 4.0, is unreliable for any real information.

   function Cookie_Enabled (ID : Gnoga.Types.Connection_ID) return Boolean;
   --  Returns true of browser will accept cookies

   function Language (ID : Gnoga.Types.Connection_ID) return String;

   function Platform  (ID : Gnoga.Types.Connection_ID) return String;
   --  May be "", sometimes will report actual platform MacIntel, Win32, etc.

   function Product (ID : Gnoga.Types.Connection_ID) return String;
   --  All browsers return "Gecko"

   function User_Agent (ID : Gnoga.Types.Connection_ID) return String;
   --  The user may use settings on their browser to modify this so not
   --  100% reliable, however most browsers will follow the following standard:
   --
   -- userAgent = appCodeName/appVersion number (Platform; Security; OS-or-CPU;
   --             Localization; rv: revision-version-number) product/productSub
   --             Application-Name Application-Name-version

end Gnoga.Navigator;
