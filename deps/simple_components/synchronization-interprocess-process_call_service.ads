--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Generic_Process_Call_Service                Spring, 2018       --
--  Interface                                                         --
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
--  The call service provides remote procedure  calls between processes.
--  Each process  places  the same set of call service  objects  and the
--  same set of methods into the environment.  Each process can call any
--  method on any  other process.  A service object can be in either the
--  server or client mode.  Only one object per process is in the server
--  mode.  This is the object  responsible  for handling incoming calls.
--  A service object can be in the server mode in only one process:
--
--  .____________________. .____________________. .____________________.
--  |                    | |                    | |                    |
--  |   Process 1        | |   Process 2        | |   Process 3        |
--  |                    | |                    | |                    |
--  | .________________. | | .________________. | | .________________. |
--  | | Method A       | | | | Method A       | | | | Method A       | |
--  | |________________| | | |________________| | | |________________| |
--  | | Method B       | | | | Method B       | | | | Method B       | |
--  | |________________| | | |________________| | | |________________| |
--  |                    | |                    | |                    |
--  | .________________. | | .________________. | | .________________. |
--  | | Service master | | | | Service slave  | | | | Service slave  | |
--  | |________________| | | |________________| | | |________________| |
--  | | Service slave  | | | | Service master | | | | Service slave  | |
--  | |________________| | | |________________| | | |________________| |
--  | | Service slave  | | | | Service slave  | | | | Service master | |
--  | |________________| | | |________________| | | |________________| |
--  |                    | |                    | |                    |
--  |____________________| |____________________| |____________________|
--
--  The process 1  can call  method A on the process 2 or 3. For this it
--  must specify the  method object A and  the service in the slave mode
--  which  is in the master mode on the target process.  For example  to
--  the second service is used to call the method A on the process 2.
--
with Ada.Exceptions;   use Ada.Exceptions;

with Synchronization.Interprocess.Events;
use  Synchronization.Interprocess.Events;

with Synchronization.Interprocess.Mutexes;
use  Synchronization.Interprocess.Mutexes;

with Synchronization.Interprocess.Streams;
use  Synchronization.Interprocess.Streams;

with Synchronization.Interprocess.Generic_FIFO;

package Synchronization.Interprocess.Process_Call_Service is
--
-- Call_Service_ID -- Call service identification
--
   type Call_Service_ID is new Interfaces.Unsigned_32;
   Null_Call_Service_ID : constant := 0;
   function Image (ID : Call_Service_ID) return String;
--
-- Sequence_No -- Unique identification of a call
--
   type Sequence_No is new Unsigned_64;
--
-- Call_Service -- A process call service
--
--    Request_Queue_Size   - The number of pending requests - 1
--    Request_Stream_Size  - The request stream size
--    Response_Stream_Size - The response stream size
--
   type Call_Service
        (  Request_Queue_Size   : Positive;
           Request_Stream_Size  : Stream_Element_Count;
           Response_Stream_Size : Stream_Element_Count
        )  is new Abstract_Shared_Object with private;
   type Call_Service_Ptr is access all Call_Service'Class;
--
-- Get_ID -- The identity of the service
--
--    Service - The service
--
-- Returns :
--
--    The unique within shared environment ID of the service
--
-- Exceptions :
--
--    Status_Error - The service is not initialized
--
   function Get_ID (Service : Call_Service) return Call_Service_ID;
--
-- Get_Process_ID -- Of the service
--
--    Service - The service
--
-- Returns :
--
--    The process ID
--
-- Exceptions :
--
--    Status_Error - The service is not initialized
--
   function Get_Process_ID (Service : Call_Service) return Process_ID;
--
-- Is_Server -- Check the service mode
--
--    Service - The service
--
-- Returns :
--
--    True if the service is in the server mode
--
   function Is_Server (Service : Call_Service) return Boolean;
--
-- On_Start -- Service task start notification
--
--    Service - The service
--
-- This procedure  is called when  the service task starts.  The default
-- implementation does nothing.
--
   procedure On_Start (Service : in out Call_Service);
--
-- Set_Server -- Set the service mode
--
--    Service - The service
--
-- This procedure sets the mode to the server mode. Only one process may
-- run  a service in  the server mode.  This  procedure  must  be called
-- before creating or opening the sharing environment containing it.
--
-- Exceptions :
--
--    Status_Error - The service is already initialized
--
   procedure Set_Server (Service : in out Call_Service);
--
-- Method_ID -- Remote subroutine identification
--
   type Method_ID is new Interfaces.Unsigned_32;
   Null_Method_ID : constant := 0;
   function Image (ID : Method_ID) return String;
--
-- Abstract_Method -- The base type of all remote subroutines
--
-- Methods must be placed into  the same shared  environment as the call
-- service before it.
--
   type Abstract_Method is abstract
      new Abstract_Shared_Object with private;
--
-- Execute -- Run the subroutine at the callee side
--
--    Method     - The remote subroutine
--    Parameters - The stream to read subroutine parameters from
--    Results    - The stream to write subroutine parameters to
--    Caller     - The caller
--    No         - The sequence number of the call
--
-- The implementation  must read all parameters from the stream and then
-- execute its body and write all results into the corresponding stream.
-- If any  exceptions to propagate,  they must after  reading parameters
-- and writing the results.
--
-- Exceptions :
--
--    any exception is sent back to the caller
--
   procedure Execute
             (  Method     : in out Abstract_Method;
                Parameters : in out Root_Stream_Type'Class;
                Results    : in out Root_Stream_Type'Class;
                Caller     : in out Call_Service'Class;
                No         : Sequence_No
             )  is abstract;
--
-- Get_ID -- The identity of the method
--
--    Method - The remote subroutine
--
-- Returns :
--
--    The unique within shared environment ID of the method
--
-- Exceptions :
--
--    Status_Error - The method is not initialized
--
   function Get_ID (Method : Abstract_Method) return Method_ID;
--
-- Get_Service -- The service by its identity
--
--    Method - The remote subroutine
--  [ ID ]   - The service ID
--
-- Returns :
--
--    The ID of the method's service
--
-- Exceptions :
--
--    Constraint_Error - Wrong ID
--    Status_Error     - The method is not initialized
--
   function Get_Service
            (  Method : Abstract_Method;
               ID     : Call_Service_ID
            )  return Call_Service_Ptr;
   function Get_Service
            (  Method : Abstract_Method
            )  return Call_Service_Ptr;
--
-- Get_Service_ID -- The identity of the method's service
--
--    Method - The remote subroutine
--
-- Returns :
--
--    The ID of the method's service
--
-- Exceptions :
--
--    Status_Error - The method is not initialized
--
   function Get_Service_ID (Method : Abstract_Method)
      return Call_Service_ID;
--
-- Is_Synchronous -- Method executed synchronously
--
--    Method - The remote subroutine
--
-- This function returns  true if the  method is  executed synchronously
-- and delivers results immediately.  Alternatively if false is returned
-- the method execution is only initiated but completed later by calling
-- operation Complete from another task.
--
-- Returns :
--
--    True if the method is executed synchronously
--
   function Is_Synchronous (Method : Abstract_Method) return Boolean;
--
-- Generic_Complete -- Complete asynchronously executed call
--
--    Method - The remote subroutine
--    Callee - The callee
--    No     - The sequence number
--
-- This procedure  must  be  called  to complete  remote  call  executed
-- asynchronously.  That is when Is_Synchronous  returns false.  Execute
-- only initiates execution and the remote  caller remains blocked after
-- return  from Execute which does  not write any results yet.  Later on
-- this procedure  is called which  then calls  the actual procedure for
-- the generic formal  parameter  Complete  which writes the results and
-- that completes execution of the remote call.
--
   generic
      with procedure Complete
                     (  Method  : in out Abstract_Method'Class;
                        Results : in out Root_Stream_Type'Class;
                        Caller  : in out Call_Service'Class;
                        No      : Sequence_No
                     );
   procedure Generic_Complete
             (  Method : in out Abstract_Method'Class;
                Caller : in out Call_Service'Class;
                No     : Sequence_No
             );
--
-- Generic_Function_Call -- Remote synchronous call
--
--    Method - The remote subroutine
--    Callee - The callee
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    Status_Error  - Method of call service is not initialized
--    Timeout_Error - Timeout error
--    Use_Error     - Callee is same as the caller
--
   generic
      type Result_Type (<>) is private;
      with procedure Send_Parameters
                     (  Stream : in out Root_Stream_Type'Class
                     )  is <>;
   function Generic_Function_Call
            (  Method  : Abstract_Method'Class;
               Callee  : Call_Service'Class;
               Timeout : Duration := Duration'Last
            )  return Result_Type;
--
-- Generic_Procedure_Call -- Remote synchronous call
--
--    Method - The remote subroutine
--    Callee - The callee
--
-- Exceptions :
--
--    Status_Error  - Method of call service is not initialized
--    Timeout_Error - Timeout error
--    Use_Error     - Callee is same as the caller
--
   generic
      with procedure Receive_Results
                     (  Stream : in out Root_Stream_Type'Class
                     )  is <>;
      with procedure Send_Parameters
                     (  Stream : in out Root_Stream_Type'Class
                     )  is <>;
   procedure Generic_Procedure_Call
             (  Method  : Abstract_Method'Class;
                Callee  : Call_Service'Class;
                Timeout : Duration := Duration'Last
             );
--
-- Generic_Post -- Remote asynchronous call
--
--    Method - The remote subroutine
--    Callee - The callee
--
-- Exceptions :
--
--    Status_Error  - Method of call service is not initialized
--    Timeout_Error - Timeout error
--    Use_Error     - Callee is same as the caller
--
   generic
      with procedure Send_Parameters
                     (  Stream : in out Root_Stream_Type'Class
                     )  is <>;
   procedure Generic_Post
             (  Method  : Abstract_Method'Class;
                Callee  : Call_Service'Class;
                Timeout : Duration := Duration'Last
             );
--
-- Implementations
--
   function Get_Size (Object : Abstract_Method) return Storage_Count;
   procedure Map
             (  Object   : in out Abstract_Method;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
private
   task type Call_Server (Service : access Call_Service'Class) is
      entry Quit;
   end Call_Server;
   type Call_Server_Ptr is access all Call_Server;

   type Request_Type is
        (  Synchronous_Request,
           Asynchronous_Request,
           Success_Response,
           Failure_Response,
           Cancel_Response
        );
   type Request_Header is record
      Mode   : Request_Type;
      Method : Method_ID;       -- The method's number
      From   : Call_Service_ID; -- The sender's number
      No     : Sequence_No;
   end record;
   procedure Read
             (  Stream  : access Root_Stream_Type'Class;
                Request : out Request_Header
             );
   for Request_Header'Read use Read;
   procedure Write
             (  Stream  : access Root_Stream_Type'Class;
                Request : Request_Header
             );
   for Request_Header'Write use Write;
   function "=" (Left, Right : Request_Header) return Boolean;
   function Image
            (  Request   : Request_Header;
               Show_Mode : Boolean := True
            )  return String;

   package Request_FIFO is
      new Synchronization.Interprocess.Generic_FIFO (Request_Header);
   use Request_FIFO;

   protected type Response_Event is
      --
      -- Accept_Results -- Accepting results by the caller task
      --
      --    Header - The callee's response header
      --
      entry Accept_Results (Header : Request_Header);
      --
      -- Wait_For_Results -- Waiting for caller to respond
      --
      --    Header    - The caller's request header
      --    No_Header - No header read
      --
      entry Wait_For_Results
            (  Header    : in out Request_Header;
               No_Header : out Boolean
            );
      --
      -- Cancel -- Pending requests
      --
      --
      entry Cancel;
      --
      -- Complete -- The call is completed
      --
      procedure Complete;
      --
      -- Quit -- Abort all requests
      --
      procedure Quit;
   private
      entry Accept_Results_Lounge (Header : Request_Header);
      entry Incoming_Results (Boolean)
            (  Header    : in out Request_Header;
               No_Header : out Boolean
            );
      State   : Boolean := False;
      Pending : Boolean := False;
      Exiting : Boolean := False;
      Result  : Request_Header;
   end Response_Event;

   type Abstract_Method_Ptr is access all Abstract_Method'Class;
   type Method_Array is
      array (Method_ID range <>) of Abstract_Method_Ptr;
   type Method_Array_Ptr is access Method_Array;

   type Call_Service_Array is
      array (Call_Service_ID range <>) of Call_Service_Ptr;
   type Call_Service_Array_Ptr is access Call_Service_Array;

   type Service_Data is record
      Service : Call_Service_Ptr;       -- Valid when in Server_Mode
      Process : Process_ID := Null_Process;   -- The process ID
      Index   : aliased Call_Service_ID := 0; -- Index of the server
   end record;
   type Service_Data_Ptr is access all Service_Data;
------------------------------------------------------------------------
   type Channel (Size : Stream_Element_Count) is record
      Not_Full  : Event;
      Not_Empty : Event;
      Lock      : aliased Mutex;
      Stream    : aliased Universal_Stream (Size);
   end record;
   type Abstract_Shared_Environment_Ptr is
      access all Abstract_Shared_Environment'Class;
   type Mode_Type is (Server_Mode, Client_Mode);
   type Call_Service_Stop
        (  Parent : access Call_Service'Class
        )  is new Abstract_Shared_Object with null record;
   function Get_Size (Object : Call_Service_Stop) return Storage_Count;
   procedure Map
             (  Object   : in out Call_Service_Stop;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
   procedure Unmap
             (  Object : in out Call_Service_Stop;
                Shared : in out Abstract_Shared_Environment'Class;
                Owner  : Boolean
             );
   type Call_Service
        (  Request_Queue_Size   : Positive;
           Request_Stream_Size  : Stream_Element_Count;
           Response_Stream_Size : Stream_Element_Count
        )  is new Abstract_Shared_Object with
   record
      Parent     : Abstract_Shared_Environment_Ptr;
      Server_ID  : Call_Service_ID := 0;
      Total_Size : Storage_Count   := 0;
      Timeout    : Duration        := 0.5;
      Response   : Response_Event;
      Mode       : Mode_Type := Client_Mode;
      Server     : Call_Server_Ptr;
      Methods    : Method_Array_Ptr;
      Servers    : Call_Service_Array_Ptr;
      Data       : Service_Data_Ptr;
      Prev       : Call_Service_Ptr;
      Queue      : Universal_FIFO (Request_Queue_Size);
      Requests   : Channel (Request_Stream_Size);
      Responses  : Channel (Response_Stream_Size);
      Epilogue   : Call_Service_Stop (Call_Service'Unchecked_Access);
   end record;
   function Get_Size (Object : Call_Service) return Storage_Count;
   function Initiate (Caller : access Call_Service)
      return Sequence_No;
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Object : Call_Service
             );
   for Call_Service'Write use Enumerate;
   procedure Map
             (  Object   : in out Call_Service;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
   procedure Start
             (  Object : in out Call_Service;
                Shared : in out Abstract_Shared_Environment'Class;
                Owner  : Boolean
             );
--
-- Results_Stream -- The stream used to return results  to  the  caller.
--                   The  stream  seizes  the  mutex and then writes the
--                   response header before writing first elements. Upon
--                   finalization the mutex is released.
--
   type Results_Stream;
   type Lock_Holder (Stream : access Results_Stream'Class) is
      new Ada.Finalization.Limited_Controlled with
   record
      Owner : Boolean := False;
   end record;
   procedure Finalize (Lock : in out Lock_Holder);

   type Null_Stream is new Root_Stream_Type with null record;
   procedure Read
             (  Results : in out Null_Stream;
                Item    : out Stream_Element_Array;
                Last    : out Stream_Element_Offset
             );
   procedure Write
             (  Results : in out Null_Stream;
                Item    : Stream_Element_Array
             );

   type Results_Stream (Service : access Call_Service'Class) is
      new Root_Stream_Type with
   record
      Header : Request_Header;
      Lock   : Lock_Holder (Results_Stream'Access);
   end record;
   procedure Read
             (  Results : in out Results_Stream;
                Item    : out Stream_Element_Array;
                Last    : out Stream_Element_Offset
             );
   procedure Write
             (  Results : in out Results_Stream;
                Item    : Stream_Element_Array
             );
------------------------------------------------------------------------
   type Abstract_Method is abstract
      new Abstract_Shared_Object with
   record
      Index   : Method_ID := Null_Method_ID;
      Service : Call_Service_Ptr;
      Prev    : Abstract_Method_Ptr;
   end record;

   Last_ID : constant Call_Service_ID := 2**31 - 1;

   function Compare_And_Swap
            (  Target  : access Call_Service_ID;
               Old_Val : Call_Service_ID;
               New_Val : Call_Service_ID
            )  return Boolean;
   pragma Import
          (  Intrinsic,
             Compare_And_Swap,
             "__sync_bool_compare_and_swap_4"
          );

end Synchronization.Interprocess.Process_Call_Service;
