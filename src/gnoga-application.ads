------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                     G N O G A . A P P L I C A T I O N                    --
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

package Gnoga.Application is
   --  Application packages simplify the management of single or multipage
   --  Gnoga GUIs.

   procedure Title (Name : in String);
   function Title return String;
   --  Set the name of the application. This will automatically set the
   --  browser window title to Name on new connections.

   procedure HTML_On_Close (HTML : String);
   function HTML_On_Close return String;
   --  Set the HTML that will display on all clients when connection is
   --  terminated or application is closed. By default an alert box will
   --  be displayed stating connection has been closed, but the current
   --  state of the browser window will be left as is. This value is
   --  sent on connection to application and therefore must be set before
   --  incoming connections. To change after connection has been established
   --  see Gnoga.Connections.HTML_On_Close

   --  The following browser launcher procedures spawn processes. On some
   --  OSes this can lead to instabilities for multi tasking apps if not
   --  spawned at program start. Therefore these if used should be used
   --  before the application initialization is called.

   procedure Open_URL_OSX (url : String := "http://127.0.0.1:8080");
   --  Open the default browser on Mac OS X

   procedure Open_URL_Linux (url : String := "http://127.0.0.1:8080");
   --  Open the default browser on Linux
   --  Uses /usr/bin/xdg-open

   procedure Open_URL_Windows (url : String := "http://127.0.0.1:8080");
   --  Open the default browser on Windows
end Gnoga.Application;
