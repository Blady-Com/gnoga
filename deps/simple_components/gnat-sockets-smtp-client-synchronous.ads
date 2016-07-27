--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.SMTP.Client.                   Luebeck            --
--        Synchronous                              Summer, 2016       --
--  Interface                                                                  --
--                                Last revision :  12:47 19 Jun 2016  --
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

with Synchronization.Events;  use Synchronization.Events;

package GNAT.Sockets.SMTP.Client.Synchronous is
--
-- Send -- An E-mail
--
--    Server   - The connections server object
--    Host     - The host name or IP address
--    Message  - The message to send
--    User     - The user
--    Password - The password
--    Accepted - The authentication methods
--    Port     - The port number
--    Timeout  - Operation timeout
--
-- This procedure sends a mail  in a synchronous manner.  It waits until
-- the server is communicated and the mail either sent or rejected.
--
-- Exceptions :
--
--    Constraint_Error - Timeout is expired
--    Data_Error       - Mail send error
--    Host_Error       - Invalid host name
--    Socket_Error     - Socket error
--    Use_Error        - Invalid mail
--
   procedure Send
             (  Server   : in out Connections_Server;
                Host     : String;
                Message  : Mail;
                User     : String;
                Password : String;
                Accepted : SMTP_AUTH_Mechanism :=
                              SMTP_AUTH_Mechanism'Last;
                Port     : Port_Type := SMTP_Port;
                Timeout  : Duration  := Duration'Last
             );
private
   type SMTP_Client_Synchronous is new SMTP_Client with record
      Completed : Event;
   end record;
   procedure Send_Abandoned
             (  Client   : in out SMTP_Client_Synchronous;
                Messages : Mail_Array
             );
   procedure Send_Error
             (  Client  : in out SMTP_Client_Synchronous;
                Code    : Error_Code;
                Context : SMTP_Command;
                Reply   : String
             );
   procedure Send_Error
             (  Client  : in out SMTP_Client_Synchronous;
                Code    : Error_Code;
                Context : SMTP_Command;
                Reply   : String;
                Message : Mail
             );
   procedure Send_Success
             (  Client  : in out SMTP_Client_Synchronous;
                Message : Mail
             );

end GNAT.Sockets.SMTP.Client.Synchronous;
