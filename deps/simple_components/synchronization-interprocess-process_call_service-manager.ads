--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Process_Call_Service.Manager                Spring, 2018       --
--  Interface                                                         --
--                                Last revision :  22:08 06 Jan 2020  --
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

with Generic_Map;
with Synchronization.Mutexes;

package Synchronization.Interprocess.Process_Call_Service.Manager is
--
-- Call_Service_Manager -- A manger of call service instances
--
--    Size                 - The number of call service instances
--    Request_Queue_Size   - The number of pending requests - 1
--    Request_Stream_Size  - The request stream size
--    Response_Stream_Size - The response stream size
--
-- The  number  of call service instances determines the total number of
-- processes  which may simultaneously exchange RPC. A process can leave
-- the  manager  by  finalizing  it.  Then  another process can take its
-- place. Each instance has two streams.
--
   type Call_Service_Manager
        (  Size                 : Call_Service_ID;
           Request_Queue_Size   : Positive;
           Request_Stream_Size  : Stream_Element_Count;
           Response_Stream_Size : Stream_Element_Count
        )  is new Abstract_Shared_Object with private;
--
-- Finalize -- Destruction
--
--    Manager - The manager object
--
-- Upon  destruction  the  call  service instance used by the manager is
-- freed. When  overridded  this  procedure  must  be  called  from  the
-- override.
--
   procedure Finalize (Manager : in out Call_Service_Manager);
--
-- Get_Server -- The service used for the process
--
--    Manager - The manager object
--
-- Each process has  a dedicated service used  by the process  to accept
-- incoming calls. This function returns this service.
--
-- Returns :
--
--    A pointer to the call service that serves the process
--
-- Exceptions :
--
--    Status_Error - The manager is not initialized
--
   function Get_Server
            (  Manager : Call_Service_Manager
            )  return Call_Service_Ptr;
--
-- Get_Server_ID -- The service used for the process
--
--    Manager - The manager object
--
-- Each process has  a dedicated service used  by the process  to accept
-- incoming calls. This function returns this service.
--
-- Returns :
--
--    The ID of the call service that serves the process
--
-- Exceptions :
--
--    Status_Error - The manager is not initialized
--
   function Get_Server_ID
            (  Manager : Call_Service_Manager
            )  return Call_Service_ID;
--
-- Get_Service -- Get a service by its ID
--
--    Manager - The manager object
--    ID      - The service ID in 1..Manager.Size
--
-- Returns :
--
--    A pointer to the call service corresponding to ID
--
-- Exceptions :
--
--    Constraint_Error - Invalid ID (not in 1..Manager.Size)
--    Status_Error     - The manager is not initialized
--
   function Get_Service
            (  Manager : Call_Service_Manager;
               ID      : Call_Service_ID
            )  return Call_Service_Ptr;
--
-- Get_Service -- Get a service by its process ID
--
--    Manager - The manager object
--    ID      - The service's process ID
--
-- Returns :
--
--    A pointer to the call service corresponding to ID
--
-- Exceptions :
--
--    Constraint_Error - Invalid ID (there is no corresponding service)
--    Status_Error     - The manager is not initialized
--
   function Get_Service
            (  Manager : Call_Service_Manager;
               ID      : Process_ID
            )  return Call_Service_Ptr;
--
-- Initialize -- Construction
--
--    Manager - The manager object
--
-- When overridded this  procedure must  be  called  from  the override.
--
   procedure Initialize (Manager : in out Call_Service_Manager);
--
-- Wait_For_Initialization
--
--    Manager    - The manager object
--  [ Services ] - The services list
--  [ Timeout  ] - The timeout
--    Close      - Close environment
--
-- Wait for specified services to initialize.
--
-- Exceptions :
--
--    Constraint_Error - Invalid services in the list
--    Timeout_Error    - Timed out
--
   type Services_List is array (Positive range <>) of Call_Service_ID;
   procedure Wait_For_Initialization
             (  Manager  : in out Call_Service_Manager;
                Services : Services_List;
                Timeout  : Duration := Duration'Last
             );
   procedure Wait_For_Initialization
             (  Manager : in out Call_Service_Manager;
                Timeout : Duration := Duration'Last
             );
private
   package Process_Maps is
      new Generic_Map (Process_ID, Call_Service_ID);

   type Call_Service_Array is
      array (Call_Service_ID range <>) of Call_Service_Ptr;
   type Call_Service_Manager
        (  Size                 : Call_Service_ID;
           Request_Queue_Size   : Positive;
           Request_Stream_Size  : Stream_Element_Count;
           Response_Stream_Size : Stream_Element_Count
        )  is new Abstract_Shared_Object with
   record
      Lock      : aliased Synchronization.Mutexes.Mutex;
      Server    : Call_Service_ID := 0;
      Processes : Process_Maps.Map;
      Services  : Call_Service_Array (1..Size);
   end record;
   function Get_Size
            (  Manager : Call_Service_Manager
            )  return Storage_Count;
   procedure Enumerate
             (  Stream  : access Root_Stream_Type'Class;
                Manager : Call_Service_Manager
             );
   for Call_Service_Manager'Write use Enumerate;
   procedure Map
             (  Manager  : in out Call_Service_Manager;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
end Synchronization.Interprocess.Process_Call_Service.Manager;
