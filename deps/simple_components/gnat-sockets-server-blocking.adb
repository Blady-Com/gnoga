--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Server.Blocking                Luebeck            --
--  Implementation                                 Winter, 2018       --
--                                                                    --
--                                Last revision :  13:37 23 Jun 2019  --
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

with Ada.Calendar;       use Ada.Calendar;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body GNAT.Sockets.Server.Blocking is
   use GNAT.Sockets.Server.Handles;

   procedure Cancel_IO
             (  Listener : in out Blocking_Server;
                Client   : in out Connection'Class
             )  is
   begin
      null;
   end Cancel_IO;

   procedure Close (Listener : in out Blocking_Server'Class) is
   begin
      if not Listener.Event.Is_Exiting then
         Listener.Event.Quit;
         begin
            Cancel_IO (Listener, Listener.Writer.Client.all);
         exception
            when Error : others =>
               Trace_Error
               (  Listener.Factory.all,
                  (  Get_Client_Name
                     (  Listener.Factory.all,
                        Listener.Writer.Client.all
                     )
                  &  " Canceling I/O upon finalization"
                  ),
                  Error
               );
         end;
      end if;
   end Close;

   procedure Connect
             (  Listener       : in out Blocking_Server;
                Client         : Connection_Ptr;
                Host           : String    := "";
                Port           : Port_Type := No_Port;
                Max_Connect_No : Positive  := Positive'Last;
                Overlapped     : Stream_Element_Count :=
                                 Stream_Element_Count'Last
             )  is
   begin
      if Client.Socket /= No_Socket then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Connection "
            &  Image (Client.Client_Address)
            &  " is already in use"
         )  );
      elsif Is_Valid (Listener.Peer) or Listener.Writer /= null then
         Raise_Exception
         (  Use_Error'Identity,
            "One connection is already handled by the server"
         );
      end if;
      Client.Session         := Session_Disconnected;
      Client.Client          := True;
      Client.Connect_No      := 0;
      Client.Max_Connect_No  := Max_Connect_No;
      Client.Socket_Listener := Listener'Unchecked_Access;
      Client.Overlapped_Read := Stream_Element_Count'Min
                                (  Overlapped,
                                   Client.Output_Size
                                );
      Set (Listener.Peer, Client);
      Listener.Clients := Listener.Clients + 1;
      Listener.Writer :=
         new Writer
             (  Listener'Unchecked_Access,
                Client.all'Unchecked_Access
             );
   end Connect;

   procedure Finalize (Listener : in out Blocking_Server) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Writer, Writer_Ptr);
      Started : constant Time := Clock;
   begin
      if Listener.Writer /= null then
         Listener.Finalizing := True;
         Close (Listener);
         while not Listener.Writer'Terminated loop
            if Clock - Started > Listener.IO_Timeout then
               Trace
               (  Listener.Factory.all,
                  (  Get_Client_Name
                     (  Listener.Factory.all,
                        Listener.Writer.Client.all
                     )
                  &  " Aborting writer"
               )  );
               abort Listener.Writer.all;
               exit;
            end if;
            delay 0.001;
         end loop;
         Free (Listener.Writer);
      end if;
      Invalidate (Listener.Peer);
      Finalize (Connections_Server (Listener));
   end Finalize;

   function Get_Clients_Count (Listener : Blocking_Server)
      return Natural is
   begin
      if Is_Valid (Listener.Peer) then
         return 1;
      else
         return 0;
      end if;
   end Get_Clients_Count;

   function Get_Peer (Listener : Blocking_Server)
      return GNAT.Sockets.Server.Handles.Handle is
   begin
      return Listener.Peer;
   end Get_Peer;

   function Get_Server_Address
            (  Listener : Blocking_Server
            )  return Sock_Addr_Type is
      Address : Sock_Addr_Type;
   begin
      Address.Addr := No_Inet_Addr;
      Address.Port := No_Port;
      return Address;
   end Get_Server_Address;

   procedure Initialize (Listener : in out Blocking_Server) is
   begin
      Listener.IO_Timeout := Get_IO_Timeout (Listener.Factory.all);
      Listener.Polling_Timeout :=
         Get_Polling_Timeout (Listener.Factory.all);
   end Initialize;

   procedure Receive_Socket
             (  Listener : in out Blocking_Server;
                Client   : in out Connection'Class;
                Data     : in out Stream_Element_Array;
                Last     : out Stream_Element_Offset
             )  is
   begin
      if Is_Empty (Listener.Input) then
         Listener.Event.Set_Empty (True);
         if Is_Empty (Listener.Input) then
            Listener.Event.Wait_Not_Empty;
         end if;
      end if;
      for Index in Data'Range loop
         Data (Index) := Get (Listener.Input);
         Last := Index;
         exit when Listener.Input.Is_Empty;
      end loop;
      if not Is_Full (Listener.Input) then
         Listener.Event.Set_Full (False);
      end if;
   end Receive_Socket;

   procedure Request_Disconnect
             (  Listener  : in out Blocking_Server;
                Client    : in out Connection'Class;
                Reconnect : Boolean
             )  is
   begin
      Raise_Exception
      (  Mode_Error'Identity,
         "The connection is permanent"
      );
   end Request_Disconnect;

   procedure Send_Socket
             (  Listener : in out Blocking_Server;
                Client   : in out Connection'Class;
                Data     : Stream_Element_Array;
                Last     : out Stream_Element_Offset
             )  is
   begin
      Write (Listener.Output_Stream.all, Data);
      Last := Data'Last;
   end Send_Socket;

   protected body IO_Buffer_Event is

      function Is_Exiting return Boolean is
      begin
         return Exiting;
      end Is_Exiting;

      procedure Quit is
      begin
         Exiting := True;
      end Quit;

      procedure Set_Empty (Value : Boolean) is
      begin
         Empty := Value;
      end Set_Empty;

      procedure Set_Full (Value : Boolean) is
      begin
         Full := Value;
      end Set_Full;

      entry Wait_Not_Empty when Exiting or else not Empty is
      begin
         if Exiting then
            raise Connection_Error;
         end if;
      end Wait_Not_Empty;

      entry Wait_Not_Full when Exiting or else not Full is
      begin
         if Exiting then
            raise Connection_Error;
         end if;
      end Wait_Not_Full;
   end IO_Buffer_Event;

   task body Writer is
      task Reader;

      procedure Trace (Text : String) is
      begin
         Trace
         (  Listener.Factory.all,
            (  Get_Client_Name (Listener.Factory.all, Client.all)
            &  " "
            &  Text
         )  );
      end Trace;

      task body Reader is
         Buffer : Stream_Element_Array (1..1);
         Last   : Stream_Element_Offset;
      begin
         Trace ("Reader task starting");
         while not (  Listener.Finalizing
                   or else
                      Listener.Shutdown_Request
                   or else
                      Listener.Event.Is_Exiting
                   )  loop
            if Is_Full (Listener.Input) then
               Listener.Event.Set_Full (True);
               Listener.Event.Wait_Not_Full;
            end if;
            Read (Listener.Input_Stream.all, Buffer, Last);
            if Last >= Buffer'First then
               Put (Listener.Input, Buffer (1));
               Listener.Event.Set_Empty (False);
            end if;
         end loop;
         Trace ("Reader task exiting");
      exception
         when End_Error | Connection_Error =>
            Trace ("Reader task exiting");
         when Error : others =>
            Trace_Error
            (  Listener.Factory.all,
               "Reader error",
               Error
            );
      end Reader;

      Data_Left : Boolean := False;
   begin
      Trace ("Writer task starting");
      On_Connected (Listener.all, Client.all);
      loop
         Trace_Service_Loop
         (  Listener.Factory.all,
            Service_Loop_Begin,
            Listener.all
         );
         exit when Listener.Finalizing
           or else Listener.Shutdown_Request
           or else Listener.Event.Is_Exiting;
         Trace_Service_Loop
         (  Listener.Factory.all,
            Service_Loop_Reading,
            Listener.all
         );
         if Client.Failed then
            if (  Client.Session /= Session_Down
               and then
                  Client.Action_Request = Keep_Connection
               and then
                  (  Exception_Identity (Client.Last_Error)
                  /= Connection_Error'Identity
               )  )
            then
               Trace_Error
               (  Listener.Factory.all,
                  "Preparing to receive",
                  Client.Last_Error
               );
            end if;
            exit;
         end if;
         if not Listener.Input.Is_Empty then
            begin
               Read (Client.all, Listener.Factory.all);
            exception
               when Connection_Error | End_Error =>
                  exit;
               when Error : others =>
                  Trace_Error
                  (  Listener.Factory.all,
                     "Read error",
                     Error
                  );
                  exit;
            end;
         end if;
         begin
            Process
            (  Listener.all,
               Client.all'Unchecked_Access,
               Data_Left
            );
         exception
            when Connection_Error =>
               exit;
            when Error : others =>
               Trace_Error
               (  Listener.Factory.all,
                  "Processing received",
                  Error
               );
               exit;
         end;
         Trace_Service_Loop
         (  Listener.Factory.all,
            Service_Loop_Writing,
            Listener.all
         );
         declare
            Block : Boolean;
         begin
            Write (Client.all, Listener.Factory.all, Block);
         end;
         exit when Listener.Finalizing
           or else Listener.Shutdown_Request
           or else Listener.Event.Is_Exiting;
         if Data_Left then
            Trace_Service_Loop
            (  Listener.Factory.all,
               Service_Loop_Postponed,
               Listener.all
            );
            begin
               Process
               (  Listener.all,
                  Client.all'Unchecked_Access,
                  Data_Left
               );
            exception
               when Connection_Error =>
                  exit;
               when Error : others =>
                  Trace_Error
                  (  Listener.Factory.all,
                     "Postponed service",
                     Error
                  );
                  exit;
            end;
         end if;
      end loop;
      if Client.Session in Session_Connected..Session_Busy then
         begin
            Disconnected (Listener.all, Client.all);
         exception
            when Connection_Error =>
               null;
            when Error : others =>
               Trace_Error
               (  Listener.Factory.all,
                  "Disconnected (server)",
                  Error
               );
         end;
         begin -- Disconnected
            Client.Session := Session_Disconnected;
            Disconnected (Client.all);
         exception
            when Connection_Error =>
               null;
            when Error : others =>
               Trace_Error
               (  Listener.Factory.all,
                  "Disconnected (client)",
                  Error
               );
         end;
      end if;
      Listener.Finalizing := True;
      Close (Listener.all);
      Listener.Clients      := Listener.Clients - 1;
      Client.Session        := Session_Down;
      Client.Failed         := False;
      Client.Action_Request := Keep_Connection;
      begin
         Downed (Listener.all, Client.all);
      exception
         when Error : others =>
            Trace_Error
            (  Listener.Factory.all,
               "Downed (server)",
               Error
            );
      end;
      begin
         Downed (Client.all);
      exception
         when Error : others =>
            Trace_Error
            (  Listener.Factory.all,
               "Downed (client)",
               Error
            );
      end;
      begin
         Released (Client.all);
      exception
         when others =>
            null;
      end;
      Trace ("Writer task exiting");
   exception
      when Connection_Error =>
         Trace ("Writer task exiting");
      when Error : others =>
         Trace_Error (Listener.Factory.all, "Writer task", Error);
   end Writer;

end GNAT.Sockets.Server.Blocking;
