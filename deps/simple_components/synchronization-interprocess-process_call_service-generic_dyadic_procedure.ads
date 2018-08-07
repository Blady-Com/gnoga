--                                                                    --
--  package              .          Copyright (c)  Dmitry A. Kazakov  --
--     ...Generic_Dyadic_Procedure                 Luebeck            --
--                                                 Spring, 2018       --
--  Interface                                                         --
--                                Last revision :  20:28 27 May 2018  --
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

generic
   type Argument_1_Type (<>) is private;
   type Argument_2_Type (<>) is private;
package Synchronization.Interprocess.Process_Call_Service.
        Generic_Dyadic_Procedure is
--
-- Immutable_Implementation -- Of the remote procedure
--
--    Argument_1 - The first parameter
--    Argument_2 - The second parameter
--    Callee     - The local call service
--    Caller     - The remote call service
--
   type Immutable_Implementation is access procedure
        (  Argument_1 : Argument_1_Type;
           Argument_2 : Argument_2_Type;
           Callee     : in out Call_Service'Class;
           Caller     : in out Call_Service'Class
        );
--
-- Mutable_Implementation -- Of the remote procedure
--
--    Argument_1 - The first parameter  (in and out)
--    Argument_2 - The second parameter (in and out)
--    Callee     - The local call service
--    Caller     - The remote call service
--
   type Mutable_Implementation is access procedure
        (  Argument_1 : in out Argument_1_Type;
           Argument_2 : in out Argument_2_Type;
           Callee     : in out Call_Service'Class;
           Caller     : in out Call_Service'Class
        );
--
-- Asynchronous_Procedure -- A  remote  procedure  with  two   arguments
--                           called asynchronously
--
   type Asynchronous_Procedure is new Abstract_Method with private;
--
-- Synchronous_Immutable_Procedure -- A  remote   procedure   with   two
--                                    immutable      argument     called
--                                    synchronously
--
   type Synchronous_Immutable_Procedure is
      new Abstract_Method with private;
--
-- Synchronous_Mutable_Procedure -- A remote procedure with two  mutable
--                                  argument called synchronously
--
   type Synchronous_Mutable_Procedure is
      new Abstract_Method with private;
--
-- Call -- The remote procedure
--
--    Method     - The remote function
--    Callee     - The remote call service to call the procedure on
--    Argument_1 - The first parameter
--    Argument_2 - The second parameter
--    Timeout    - The call timeout
--
-- Exceptions :
--
--    Program_Error - Not implemented (the implementation is not set)
--    Status_Error  - Method of call service is not initialized
--    Timeout_Error - Timeout error
--    Use_Error     - Callee is same as the caller
--
   procedure Call
             (  Method     : Asynchronous_Procedure;
                Argument_1 : Argument_1_Type;
                Argument_2 : Argument_2_Type;
                Callee     : Call_Service'Class;
                Timeout    : Duration := Duration'Last
             );
   procedure Call
             (  Method     : Asynchronous_Procedure;
                Argument_1 : Argument_1_Type;
                Argument_2 : Argument_2_Type;
                Callee     : Call_Service_ID;
                Timeout    : Duration := Duration'Last
             );
   procedure Call
             (  Method     : Synchronous_Immutable_Procedure;
                Argument_1 : Argument_1_Type;
                Argument_2 : Argument_2_Type;
                Callee     : Call_Service'Class;
                Timeout    : Duration := Duration'Last
             );
   procedure Call
             (  Method     : Synchronous_Immutable_Procedure;
                Argument_1 : Argument_1_Type;
                Argument_2 : Argument_2_Type;
                Callee     : Call_Service_ID;
                Timeout    : Duration := Duration'Last
             );
   procedure Call
             (  Method     : Synchronous_Mutable_Procedure;
                Argument_1 : in out Argument_1_Type;
                Argument_2 : in out Argument_2_Type;
                Callee     : Call_Service'Class;
                Timeout    : Duration := Duration'Last
             );
   procedure Call
             (  Method     : Synchronous_Mutable_Procedure;
                Argument_1 : in out Argument_1_Type;
                Argument_2 : in out Argument_2_Type;
                Callee     : Call_Service_ID;
                Timeout    : Duration := Duration'Last
             );
--
-- Set -- The procedure implementation on the callee side
--
--    Method - The remote function
--    Hanler - The procedure implementation
--
-- This procedure must be called on  the service side  before  the first
-- incoming call.  The function  Handler  will be called  in response to
-- each incoming call.
--
   procedure Set
             (  Method  : in out Asynchronous_Procedure;
                Handler : Immutable_Implementation
             );
   procedure Set
             (  Method  : in out Synchronous_Immutable_Procedure;
                Handler : Immutable_Implementation
             );
   procedure Set
             (  Method  : in out Synchronous_Mutable_Procedure;
                Handler : Mutable_Implementation
             );
private
   type Asynchronous_Procedure is new Abstract_Method with record
      Handler : Immutable_Implementation;
   end record;
   procedure Execute
             (  Method     : in out Asynchronous_Procedure;
                Parameters : in out Root_Stream_Type'Class;
                Results    : in out Root_Stream_Type'Class;
                Caller     : in out Call_Service'Class;
                No         : Sequence_No
             );
   type Synchronous_Immutable_Procedure is new Abstract_Method with
   record
      Handler : Immutable_Implementation;
   end record;
   procedure Execute
             (  Method     : in out Synchronous_Immutable_Procedure;
                Parameters : in out Root_Stream_Type'Class;
                Results    : in out Root_Stream_Type'Class;
                Caller     : in out Call_Service'Class;
                No         : Sequence_No
             );
   type Synchronous_Mutable_Procedure is new Abstract_Method with record
      Handler : Mutable_Implementation;
   end record;
   procedure Execute
             (  Method     : in out Synchronous_Mutable_Procedure;
                Parameters : in out Root_Stream_Type'Class;
                Results    : in out Root_Stream_Type'Class;
                Caller     : in out Call_Service'Class;
                No         : Sequence_No
             );
end Synchronization.Interprocess.Process_Call_Service.
    Generic_Dyadic_Procedure;

