--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Server.Pooled                  Luebeck            --
--  Interface                                      Winter, 2013       --
--                                                                    --
--                                Last revision :  14:52 29 Feb 2020  --
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
--
--  This package  provides an  implementation of  server  with a pool of
--  worker tasks used to process incoming packets
--
package GNAT.Sockets.Server.Pooled is
--
-- Pooled_Server -- Server with a pool of tasks
--
--    Port      - The port to listen
--    Pool_Size - The number of worker tasks in the pool
--
-- The server rely on the tasks  from the pool  to parse and process the
-- packets.
--
   type Pooled_Server
        (  Factory   : access Connections_Factory'Class;
           Port      : Port_Type;
           Pool_Size : Positive
        )  is new Connections_Server with private;
--
-- Disconnected -- Connection destruction notification, overriding
--
   procedure Disconnected
             (  Listener : in out Pooled_Server;
                Client   : in out Connection'Class
             );
--
-- Finalize -- Destruction
--
--    Listener - The server object
--
-- The derived  type must  call this  procedure from  its implementation
-- when it replaces it.
--
   procedure Finalize (Listener : in out Pooled_Server);
--
-- Initialize -- Construction
--
--    Listener - The server object
--
-- The derived  type must  call this  procedure from  its implementation
-- when it replaces it.
--
   procedure Initialize (Listener : in out Pooled_Server);
--
-- On_Pooled_Server_Start -- Server task start notification
--
--    Listener - The server object
--
-- This procedure  is called  when the server  task starts.  The default
-- implementation does nothing.
--
   procedure On_Pooled_Server_Start (Listener : in out Pooled_Server);

private
   Pending_Finalization : exception;

   protected type Queue (Listener : access Pooled_Server'Class) is
      function Queued return Natural;
      entry Dequeue (Client : out Connection_Ptr);
      procedure Delete
                (  Client   : in out Connection'Class;
                   Dequeued : out Boolean
                );
      procedure Enqueue
                (  Client   : Connection_Ptr;
                   Enqueued : out Boolean
                );
      procedure Finalize;
      procedure Get (Client : out Connection_Ptr);
      procedure Release
                (  Client   : Connection_Ptr;
                   Enqueued : in out Boolean
                );
   private
      Count     : Natural := 0;
      Finalized : Boolean := False;
   end Queue;

   task type Packet_Handler (Listener : access Pooled_Server'Class);
   type Packet_Handler_Ptr is access Packet_Handler;
   type Packet_Handler_Ptr_Array is
      array (Positive range <>) of Packet_Handler_Ptr;
   type Pooled_Server
        (  Factory   : access Connections_Factory'Class;
           Port      : Port_Type;
           Pool_Size : Positive
        )  is new Connections_Server (Factory, Port) with
   record
      Active  : Queue (Pooled_Server'Unchecked_Access);
      Workers : Packet_Handler_Ptr_Array (1..Pool_Size);
   end record;

   procedure Data_Sent
             (  Listener : in out Pooled_Server;
                Client   : Connection_Ptr
             );
   procedure Process
             (  Listener  : in out Pooled_Server;
                Client    : Connection_Ptr;
                Data_Left : out Boolean
             );
   procedure Service_Postponed (Listener : in out Pooled_Server);
   procedure Ref (Client : in out Connection'Class);
   procedure Unref (Client : in out Connection'Class);

   pragma Inline (Ref);
   pragma Inline (Unref);

end GNAT.Sockets.Server.Pooled;
