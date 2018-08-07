--                                                                    --
--  package              .          Copyright (c)  Dmitry A. Kazakov  --
--     ...Generic_Ternary_Function                 Luebeck            --
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
   type Argument_3_Type (<>) is private;
   type Result_Type   (<>) is private;
package Synchronization.Interprocess.Process_Call_Service.
        Generic_Ternary_Function is
--
-- Implementation -- Of the remote function
--
--    Argument_1 - The first parameter
--    Argument_2 - The second parameter
--    Argument_3 - The third parameter
--    Callee     - The local call service
--    Caller     - The remote call service
--
-- Returns :
--
--    The result
--
   type Implementation is access function
        (  Argument_1 : Argument_1_Type;
           Argument_2 : Argument_2_Type;
           Argument_3 : Argument_3_Type;
           Callee     : access Call_Service'Class;
           Caller     : access Call_Service'Class
        )  return Result_Type;
--
-- Remote_Function -- A remote fuction with one argument and result
--
   type Remote_Function is new Abstract_Method with private;
--
-- Call -- The remote function
--
--    Method     - The remote function
--    Callee     - The remote call service to call the function on
--    Argument_1 - The first parameter
--    Argument_2 - The second parameter
--    Argument_3 - The third parameter
--    Timeout    - The call timeout
--
-- Returns :
--
--     The result
--
-- Exceptions :
--
--    Program_Error - Not implemented (the implementation is not set)
--    Status_Error  - Method of call service is not initialized
--    Timeout_Error - Timeout error
--    Use_Error     - Callee is same as the caller
--
   function Call
            (  Method     : Remote_Function;
               Callee     : Call_Service'Class;
               Argument_1 : Argument_1_Type;
               Argument_2 : Argument_2_Type;
               Argument_3 : Argument_3_Type;
               Timeout    : Duration := Duration'Last
            )  return Result_Type;
   function Call
            (  Method     : Remote_Function;
               Callee     : Call_Service_ID;
               Argument_1 : Argument_1_Type;
               Argument_2 : Argument_2_Type;
               Argument_3 : Argument_3_Type;
               Timeout    : Duration := Duration'Last
            )  return Result_Type;
--
-- Set -- The function implementation on the callee side
--
--    Method - The remote function
--    Hanler - The function implementation
--
-- This procedure must be called on  the service side  before  the first
-- incoming call.  The function  Handler  will be called  in response to
-- each incoming call.
--
   procedure Set
             (  Method  : in out Remote_Function;
                Handler : Implementation
             );
private
   type Remote_Function is new Abstract_Method with record
      Handler : Implementation;
   end record;
   procedure Execute
             (  Method     : in out Remote_Function;
                Parameters : in out Root_Stream_Type'Class;
                Results    : in out Root_Stream_Type'Class;
                Caller     : in out Call_Service'Class;
                No         : Sequence_No
             );
end Synchronization.Interprocess.Process_Call_Service.
    Generic_Ternary_Function;
