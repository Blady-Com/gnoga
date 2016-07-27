--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.SMTP.Client.                   Luebeck            --
--        Synchronous                              Summer, 2016       --
--  Implementation                                                    --
--                                Last revision :  17:51 21 Jun 2016  --
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

with Ada.Exceptions;               use Ada.Exceptions;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;

package body GNAT.Sockets.SMTP.Client.Synchronous is

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
             )  is
      Reference : constant Handle :=
                     Ref
                     (  new SMTP_Client_Synchronous
                            (  Listener     => Server'Unchecked_Access,
                               Reply_Length => 1024,
                               Input_Size   => 80,
                               Output_Size  => 1024
                     )      );
      Client : SMTP_Client_Synchronous renames
               SMTP_Client_Synchronous (Ptr (Reference).all);
   begin
      Set_Credentials (Client, User, Password);
      Send (Client, Message);
      Connect (Server, Ptr (Reference), Host, Port);
      select
         Client.Completed.Wait;
      or delay Timeout;
         Shutdown (Ptr (Reference).all);
         Raise_Exception
         (  Constraint_Error'Identity,
            "Operation timed out"
         );
      end select;
      if Client.Code.Reply > 299 then -- Error
         Shutdown (Ptr (Reference).all);
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error sending mail: " & Image (Client.Code)
         );
      end if;
   end Send;

   procedure Send_Abandoned
             (  Client   : in out SMTP_Client_Synchronous;
                Messages : Mail_Array
             )  is
   begin
      Client.Completed.Signal;
   end Send_Abandoned;

   procedure Send_Error
             (  Client  : in out SMTP_Client_Synchronous;
                Code    : Error_Code;
                Context : SMTP_Command;
                Reply   : String
             )  is
   begin
      Client.Code := Code;
      Client.Completed.Signal;
   end Send_Error;

   procedure Send_Error
             (  Client  : in out SMTP_Client_Synchronous;
                Code    : Error_Code;
                Context : SMTP_Command;
                Reply   : String;
                Message : Mail
             )  is
   begin
      Client.Code := Code;
      Client.Completed.Signal;
   end Send_Error;

   procedure Send_Success
             (  Client  : in out SMTP_Client_Synchronous;
                Message : Mail
             )  is
   begin
      Client.Completed.Signal;
   end Send_Success;

end GNAT.Sockets.SMTP.Client.Synchronous;
