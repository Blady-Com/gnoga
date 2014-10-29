------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                 G N O G A . S E R V E R . C O N N E C I O N              --
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
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Gnoga.Types;
with Gnoga.Gui.Base;

package Gnoga.Server.Connection is

   procedure Initialize (Host    : in String  := "";
                         Port    : in Integer := 8080;
                         Boot    : in String  := "boot.html";
                         Verbose : in Boolean := True);
   --  Ininialize connections web server and dispatchers
   --  If Host = "" then will listen on all interfaces.
   --  Use Host = "locahost" to constrain to local use only.
   --  If Verbose then display start up details.

   procedure Run (Wait_For_Q : in Boolean := True);
   --  Start webserer
   --  If Wait_For_Q then server shutdown will occur if the Q key pressed
   --  on console. If not server must be shutdown using Stop.

   procedure Stop;
   --  Close all connections and Stop webserver

   function Shutting_Down return Boolean;
   --  If application is shutting down returns true

   procedure Execute_Script (ID     : in Gnoga.Types.Connection_ID;
                             Script : in String);
   --  Execute Script on Connection ID

   function Execute_Script (ID     : in Gnoga.Types.Connection_ID;
                            Script : in String)
                            return String;
   --  Execute Script on Connection ID and return result of script

   Script_Error : exception;

   protected type Connection_Holder_Type is
      entry Hold;
      procedure Release;
   private
      Connected : Boolean := True;
   end Connection_Holder_Type;
   --  This type is a binary semiphore starting in the seized stated.
   --  It is used to allow Connect_Event handlers to remain in memory
   --  until the web socket connection is closed.

   procedure Connection_Data
     (ID   : in     Gnoga.Types.Connection_ID;
      Data : access Gnoga.Types.Connection_Data_Type'Class);
   function Connection_Data
     (ID : in Gnoga.Types.Connection_ID)
      return Gnoga.Types.Pointer_to_Connection_Data_Class;

   type Connect_Event is access
     procedure (ID         : in Gnoga.Types.Connection_ID;
                Connection : access Connection_Holder_Type);

   procedure On_Connect_Handler (Event : in Connect_Event);
   --  Set event handler for new socket connections.

   type Post_Event is access
     procedure (URI        : in String;
                Parameters : in out Gnoga.Types.Data_Map_Type);

   procedure On_Post_Handler (Event : Post_Event);
   --  By default if a form "post" is received the post parameters will
   --  be parsed and inserted in to the returned, bootstrap html. A custom
   --  Post handler can be set to filter incoming form posts. Any changes
   --  made to Paremeters will replace the recevied post parameters with those
   --  changes.
   --
   --  In order to handle file uploads, you must move or rename them in the
   --  On_Post_Handler files that are uploaded are destroyed once the
   --  bootloader file has been uploaded.

   function Form_Parameter (ID   : Gnoga.Types.Connection_ID;
                            Name : String)
                            return String;
   --  Returns the value of parameters passed in on URL. Returns "undefined"
   --  if Name is not in URL search parameters or sent via post.
   --  For example: http://localhost:8080/?page_id=2
   --  Search_Parameter (ID, "page_id") = "2"

   function Valid (ID : Gnoga.Types.Connection_ID) return Boolean;
   --  If ID is valid return true. Note that a broken web socket that has not
   --  been closed properly will have to time out first before reporting an
   --  error and Gnoga invalidating the ID.

   procedure Close (ID : in Gnoga.Types.Connection_ID);
   --  Close connection ID

   procedure HTML_On_Close (ID   : in Gnoga.Types.Connection_ID;
                            HTML : in String);
   --  On connection closed or lost HTML to display in browser.
   --  By default pages are left in the state they were in and an alter box
   --  announcing connection interuption is displayed.

   procedure New_Unique_ID (New_ID : out Gnoga.Types.Unique_ID);
   --  Generates a new unique ID in to New_ID

   function New_GID return String;
   --  Genertes unique ID for use in browser storage of elements

   procedure Add_To_Message_Queue
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Add Object to Message Queue

   procedure Delete_From_Message_Queue
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Delete an Object from Message Queue

   Connection_Error : exception;

end Gnoga.Server.Connection;
