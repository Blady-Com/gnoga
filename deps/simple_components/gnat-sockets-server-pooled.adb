--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Server.Pooled                  Luebeck            --
--  Implementation                                 Winter, 2013       --
--                                                                    --
--                                Last revision :  08:20 11 Jan 2015  --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

with Ada.Unchecked_Deallocation;

package body GNAT.Sockets.Server.Pooled is

   procedure Data_Sent
             (  Listener : in out Pooled_Server;
                Client   : Connection_Ptr
             )  is
      Enqueued : Boolean;
   begin
      if Queued_To_Send (Client.all) = 0 then
         Ref (Client.all);
         Listener.Active.Enqueue (Client, Enqueued);
         if not Enqueued then
            Unref (Client.all);
         end if;
      end if;
   end Data_Sent;

   procedure Disconnected
             (  Listener : in out Pooled_Server;
                Client   : in out Connection'Class
             )  is
      Dequeued : Boolean;
   begin
      Listener.Active.Delete (Client, Dequeued);
      if Dequeued then
         Unref (Client);
      end if;
   end Disconnected;

   procedure Finalize (Listener : in out Pooled_Server) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Packet_Handler,
                Packet_Handler_Ptr
             );
   begin
      Listener.Active.Finalize;
      loop
         declare
            Completed : Boolean := True;
         begin
            for Index in Listener.Workers'Range loop
               if Listener.Workers (Index) /= null then
                  if Listener.Workers (Index)'Terminated then
                     Free (Listener.Workers (Index));
                  else
                     Completed := False;
                  end if;
               end if;
            end loop;
            exit when Completed;
         end;
         delay 0.001;
      end loop;
      Finalize (Connections_Server (Listener));
      loop
         declare
            Client : Connection_Ptr;
         begin
            Listener.Active.Get (Client);
            exit when Client = null;
            Unref (Client.all);
         end;
      end loop;
   end Finalize;

   procedure Initialize (Listener : in out Pooled_Server) is
   begin
      Initialize (Connections_Server (Listener));
      for Index in Listener.Workers'Range loop
         Listener.Workers (Index) :=
            new Packet_Handler (Listener'Unchecked_Access);
      end loop;
   end Initialize;

   procedure Process
             (  Listener  : in out Pooled_Server;
                Client    : Connection_Ptr;
                Data_Left : out Boolean
             )  is
      Enqueued : Boolean;
   begin
      if Has_Data (Client.all) then
         Ref (Client.all);
         Listener.Active.Enqueue (Client, Enqueued);
         if not Enqueued then
            Unref (Client.all);
         end if;
      end if;
      Data_Left := False;
   end Process;

   procedure Ref (Client : in out Connection'Class) is
   begin
      Increment_Count (Client);
   end Ref;

   procedure Service_Postponed (Listener : in out Pooled_Server) is
   begin
      null;
   end Service_Postponed;

   procedure Unref (Client : in out Connection'Class) is
      Ptr : Object.Entity_Ptr := Client'Unchecked_Access;
   begin
      Object.Release (Ptr);
   end Unref;

   task body Packet_Handler is
      Data_Left : Boolean;
      Client    : Connection_Ptr;
   begin
      loop
         Listener.Active.Dequeue (Client);
         declare
            Enqueued : Boolean;
         begin
            if Client.Data_Sent then
               Data_Sent (Connections_Server (Listener.all), Client);
            end if;
            if (  Queued_To_Send (Client.all) = 0
               and then
                  Has_Data (Client.all)
               )
            then
               Process
               (  Connections_Server (Listener.all),
                  Client,
                  Data_Left
               );
            end if;
            Enqueued := Client.Data_Sent or else Has_Data (Client.all);
            Listener.Active.Release (Client, Enqueued);
            if not Enqueued then
               Unref (Client.all);
            end if;
         exception
            when Error : others =>
               Set_Failed (Client.all, Error);
               Unref (Client.all);
         end;
      end loop;
   exception
      when Pending_Finalization =>
         null;
      when Error : others =>
         Trace_Error
         (  Listener.Factory.all,
            "Packet handler task",
            Error
         );
   end Packet_Handler;

   protected body Queue is

      procedure Delete
                (  Client   : in out Connection'Class;
                   Dequeued : out Boolean
                )  is
      begin
         if Client.Successor /= null then
            Dequeued := True;
            Remove (Listener.Postponed, Client, Count);
         else
            Dequeued := False;
         end if;
      end Delete;

      entry Dequeue (Client : out Connection_Ptr)
         when Finalized or else Count > 0 is
      begin
         if Finalized then
            raise Pending_Finalization;
         end if;
         Client := Listener.Postponed;
         Client.External_Action := True;
         Remove (Listener.Postponed, Client.all, Count);
      end Dequeue;

      procedure Enqueue
                (  Client   : Connection_Ptr;
                   Enqueued : out Boolean
                )  is
      begin
         if not (  Finalized
                or else
                   Client.External_Action
                or else
                   Client.Socket = No_Socket
                )
         then
            Append (Listener.Postponed, Client, Count);
            Enqueued := True;
         else
            Enqueued := False;
         end if;
      end Enqueue;

      procedure Finalize is
      begin
         Finalized := True;
      end Finalize;

      procedure Get (Client : out Connection_Ptr) is
      begin
         Client := Listener.Postponed;
         if Client /= null then
            Remove (Listener.Postponed, Client.all, Count);
         end if;
      end Get;

      function Queued return Natural is
      begin
         return Count;
      end Queued;

      procedure Release
                (  Client   : Connection_Ptr;
                   Enqueued : in out Boolean
                )  is
      begin
         Client.External_Action := False;
         Enqueue (Client, Enqueued);
      end Release;

   end Queue;

end GNAT.Sockets.Server.Pooled;
