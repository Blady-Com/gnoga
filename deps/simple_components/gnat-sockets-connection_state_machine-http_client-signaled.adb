--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     HTTP_Client.Signaled                        Spring, 2015       --
--  Implementation                                                    --
--                                Last revision :  14:40 03 Apr 2020  --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Synchronization;    use Synchronization;

package body GNAT.Sockets.Connection_State_Machine.
             HTTP_Client.Signaled is

   procedure Cancel (Session : in out HTTP_Session_Signaled) is
   begin
      Session.Event.Cancel;
   end Cancel;

   procedure Connect
             (  Session        : in out HTTP_Session_Signaled;
                Host           : String;
                Port           : Port_Type := 80;
                Max_Connect_No : Positive  := Positive'Last;
                Timeout        : Duration  := Duration'Last
             )  is
      Deadline  : Time;
      Connected : Boolean := True;
   begin
      Shutdown (Session);
      Deadline := Clock + Timeout;
      select
         Session.Event.Released;
      or delay until Deadline;
         Raise_Exception
         (  Timeout_Error'Identity,
            "Shutdown timeout expired"
         );
      end select;
      Connect
      (  Session.Listener.all,
         Session'Unchecked_Access,
         Host,
         Port,
         Max_Connect_No
      );
      select
         Session.Event.Wait (Connected);
      or delay until Deadline;
         Raise_Exception
         (  Timeout_Error'Identity,
            "Connection timeout expired"
         );
      end select;
   exception
      when Time_Error =>
         Session.Event.Released;
         Connect
         (  Session.Listener.all,
            Session'Unchecked_Access,
            Host,
            Port,
            Max_Connect_No
         );
         Session.Event.Wait (Connected);
   end Connect;

   procedure Connected (Session : in out HTTP_Session_Signaled) is
   begin
      Connected (HTTP_Session (Session));
      Session.Event.Set;
   end Connected;

   procedure End_Of_Query (Session : in out HTTP_Session_Signaled) is
   begin
      End_Of_Query (HTTP_Session (Session));
      Session.Event.Set;
   end End_Of_Query;

   procedure Released (Session : in out HTTP_Session_Signaled) is
   begin
      Released (HTTP_Session (Session));
      Session.Event.Set;
   end Released;

   procedure Wait
             (  Session   : in out HTTP_Session_Signaled;
                Connected : Boolean;
                Timeout   : Duration := Duration'Last
             )  is
      Require : Boolean := Connected;
   begin
      select
         Session.Event.Wait (Require);
      or delay Timeout;
         Raise_Exception
         (  Timeout_Error'Identity,
            "Connection timeout expired"
         );
      end select;
   end Wait;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Event_Type
             )  is
   begin
      null;
   end Write;

   protected body Event_Type is
      entry Cancel when Event_Type.Released'Count = 0 and then
                        Event_Type.Wait'Count = 0 is
      begin
         null;
      end Cancel;

      entry Released when Down or else Event_Type.Cancel'Count > 0 is
      begin
         if Get_Session_State (Session.all) /= Session_Down then
            Down := False;
            if Event_Type.Cancel'Count > 0 then
               Raise_Exception (Cancel_Error'Identity, "Canceled");
            else
               requeue Released with abort;
            end if;
         end if;
      end Released;

      entry Wait (Connected : in out Boolean)
         when Ready or else Down or else Event_Type.Cancel'Count > 0 is
      begin
         case Get_Session_State (Session.all) is
            when Session_Down =>
               Ready := False;
               Down  := True;
               if Connected then
                  declare
                     Error : Exception_Occurrence;
                  begin
                     Get_Occurrence (Session.all, Error);
                     if Exception_Identity (Error) /= Null_Id then
                        Reraise_Occurrence (Error);
                     else
                        Raise_Exception
                        (  Status_Error'Identity,
                           "Unable to connect"
                        );
                     end if;
                  end;
               else
                  Connected := False;
               end if;
            when Session_Disconnected | Session_Connecting |
                 Session_Handshaking  | Session_Busy       =>
               Ready := False;
               Down  := False;
               if Event_Type.Cancel'Count > 0 then
                  Raise_Exception (Cancel_Error'Identity, "Canceled");
               else
                  requeue Wait with abort;
               end if;
            when Session_Active | Session_Connected =>
               Ready     := True;
               Down      := False;
               Connected := True;
         end case;
      end Wait;

      procedure Set is
      begin
         case Get_Session_State (Session.all) is
            when Session_Down =>
               Ready := False;
               Down  := True;
            when Session_Disconnected | Session_Connecting |
                 Session_Handshaking  | Session_Busy       =>
               Ready := False;
               Down  := False;
            when Session_Active | Session_Connected =>
               Ready := True;
               Down  := False;
         end case;
      end Set;

   end Event_Type;

end GNAT.Sockets.Connection_State_Machine.HTTP_Client.Signaled;
