--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     HTTP_Client.Signaled                        Spring, 2015       --
--  Interface                                                         --
--                                Last revision :  22:35 24 May 2015  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

package GNAT.Sockets.Connection_State_Machine.HTTP_Client.Signaled is
--
-- Timeout_Error -- Timeout expired
--
   Timeout_Error : exception;
   Cancel_Error  : exception;
--
-- HTTP_Session_Signaled -- An object with event
--
   type HTTP_Session_Signaled is new HTTP_Session with private;
--
-- Cancel -- Waiting
--
--    Session - The connection object
--
-- This  procedure  is used to cancel Connect  or Wait  pending in other
-- tasks. It does not influence the communication itself. The operations
-- will propagate Cancel_Error if not yet completed.
--
   procedure Cancel (Session : in out HTTP_Session_Signaled);
--
-- Connect -- Connect to a server
--
--    Session        - The connection object
--    Host           - The host name or IP address
--    Port           - The port number
--    Max_Connect_No - Maximal number of connection attempts
--    Timeout        - For the operation
--
-- This procedure  is used  to connect to a remote server.  When already
-- connected  the current connection  is dropped  and another initiated.
-- The procedure ends when the host is connected.
--
-- Exceptions :
--
--    Cancel_Error  - Waiting canceled by a call to Cancel
--    Socket_Error  - Socket error
--    Status_Error  - Connection failure, e.g. attempts exhausted
--    Timeout_Error - Timeout expired
--
   procedure Connect
             (  Session        : in out HTTP_Session_Signaled;
                Host           : String;
                Port           : Port_Type := 80;
                Max_Connect_No : Positive  := Positive'Last;
                Timeout        : Duration  := Duration'Last
             );
--
-- Wait -- For connection object ready
--
--    Session   - The connection object
--    Connected - Require connection
--    Timeout   - For the operation
--
-- This procedure is called to wait for the session to become ready  for
-- another request.
--
-- Exceptions :
--
--    Cancel_Error  - Waiting canceled by a call to Cancel
--    Socket_Error  - Socket error
--    Status_Error  - Connection failure, e.g. attempts exhausted
--    Timeout_Error - Timeout expired
--
   procedure Wait
             (  Session   : in out HTTP_Session_Signaled;
                Connected : Boolean;
                Timeout   : Duration := Duration'Last
             );
private
   protected type Event_Type
                  (  Session : access HTTP_Session_Signaled'Class
                  )  is
      entry Cancel;
      entry Released;
      entry Wait (Connected : in out Boolean);
      procedure Set;
   private
      Ready : Boolean := False;
      Down  : Boolean := True;
   end Event_Type;

   procedure Connected (Session : in out HTTP_Session_Signaled);
   procedure End_Of_Query (Session : in out HTTP_Session_Signaled);
   procedure Released (Session : in out HTTP_Session_Signaled);

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Event_Type
             );
   for Event_Type'Write use Write;

   type HTTP_Session_Signaled is new HTTP_Session with record
      Event : Event_Type (HTTP_Session_Signaled'Unchecked_Access);
   end record;

end GNAT.Sockets.Connection_State_Machine.HTTP_Client.Signaled;
