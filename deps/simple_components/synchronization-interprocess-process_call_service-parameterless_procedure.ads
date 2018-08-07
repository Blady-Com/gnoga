--                                                                    --
--  package              .          Copyright (c)  Dmitry A. Kazakov  --
--     ...Parameterless_Procedure                  Luebeck            --
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

package Synchronization.Interprocess.Process_Call_Service.
        Parameterless_Procedure is
--
-- Implementation -- Of the remote procedure
--
--    Callee - The local call service
--    Caller - The remote call service
--
   type Implementation is access procedure
        (  Callee : in out Call_Service'Class;
           Caller : in out Call_Service'Class
        );
--
-- Asynchronous_Procedure -- A remote procedure called asynchronously
--
   type Asynchronous_Procedure is new Abstract_Method with private;
--
-- Synchronous_Procedure -- A remote procedure called synchronously
--
   type Synchronous_Procedure is new Abstract_Method with private;
--
-- Call -- The remote procedure
--
--    Method  - The remote function
--    Callee  - The remote call service to call the procedure on
--    Timeout - The call timeout
--
-- Exceptions :
--
--    Program_Error - Not implemented (the implementation is not set)
--    Status_Error  - Method of call service is not initialized
--    Timeout_Error - Timeout error
--    Use_Error     - Callee is same as the caller
--
   procedure Call
             (  Method   : Asynchronous_Procedure;
                Callee   : Call_Service'Class;
                Timeout  : Duration := Duration'Last
             );
   procedure Call
             (  Method   : Asynchronous_Procedure;
                Callee   : Call_Service_ID;
                Timeout  : Duration := Duration'Last
             );
   procedure Call
             (  Method   : Synchronous_Procedure;
                Callee   : Call_Service'Class;
                Timeout  : Duration := Duration'Last
             );
   procedure Call
             (  Method   : Synchronous_Procedure;
                Callee   : Call_Service_ID;
                Timeout  : Duration := Duration'Last
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
                Handler : Implementation
             );
   procedure Set
             (  Method  : in out Synchronous_Procedure;
                Handler : Implementation
             );
private
   type Asynchronous_Procedure is new Abstract_Method with record
      Handler : Implementation;
   end record;
   procedure Execute
             (  Method     : in out Asynchronous_Procedure;
                Parameters : in out Root_Stream_Type'Class;
                Results    : in out Root_Stream_Type'Class;
                Caller     : in out Call_Service'Class;
                No         : Sequence_No
             );
   type Synchronous_Procedure is new Abstract_Method with
   record
      Handler : Implementation;
   end record;
   procedure Execute
             (  Method     : in out Synchronous_Procedure;
                Parameters : in out Root_Stream_Type'Class;
                Results    : in out Root_Stream_Type'Class;
                Caller     : in out Call_Service'Class;
                No         : Sequence_No
             );
end Synchronization.Interprocess.Process_Call_Service.
    Parameterless_Procedure;
