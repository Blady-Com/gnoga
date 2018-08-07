--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Call_Services                          Luebeck            --
--  Call services example test                     Spring, 2018       --
--                                                                    --
--                                Last revision :  19:18 30 Apr 2018  --
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

with Synchronization.Interprocess;  use Synchronization.Interprocess;

with Synchronization.Interprocess.Process_Call_Service.
     Generic_Unary_Function;
with Synchronization.Interprocess.Process_Call_Service.
     Generic_Dyadic_Function;
with Synchronization.Interprocess.Process_Call_Service.
     Generic_Dyadic_Procedure;
with Synchronization.Interprocess.Process_Call_Service.Manager;

package Test_Call_Services is
   use Synchronization.Interprocess.Process_Call_Service;
   use Synchronization.Interprocess.Process_Call_Service.Manager;

   package Algebraic_Integer is
      new Synchronization.Interprocess.Process_Call_Service.
          Generic_Dyadic_Function
          (  Argument_1_Type => Integer,
             Argument_2_Type => Integer,
             Result_Type     => Integer
          );
   package Greeting_Message is
      new Synchronization.Interprocess.Process_Call_Service.
          Generic_Unary_Function
          (  Argument_Type => String,
             Result_Type   => String
          );
   package Baton_Message is
      new Synchronization.Interprocess.Process_Call_Service.
          Generic_Dyadic_Procedure
          (  Argument_1_Type => Positive,
             Argument_2_Type => String
          );

   type Shared_Data is new Abstract_Shared_Environment with record
      Add      : Algebraic_Integer.Remote_Function;
      Divide   : Algebraic_Integer.Remote_Function;
      Greeting : Greeting_Message.Remote_Function;
      Baton    : Baton_Message.Asynchronous_Procedure;
      Services : Call_Service_Manager (3, 10, 100, 100);
   end record;

   procedure Baton
             (  Count  : Positive;
                Text   : String;
                Callee : in out Call_Service'Class;
                Caller : in out Call_Service'Class
             );
   function Greet
            (  Text   : String;
               Callee : access Call_Service'Class;
               Caller : access Call_Service'Class
            )  return String;
   function Add
            (  Left   : Integer;
               Right  : Integer;
               Callee : access Call_Service'Class;
               Caller : access Call_Service'Class
            )  return Integer;
   function Divide
            (  Dividend : Integer;
               Divisor  : Integer;
               Callee   : access Call_Service'Class;
               Caller   : access Call_Service'Class
            )  return Integer;

   Data : Shared_Data;

end Test_Call_Services;
