--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Hello_RPC_Server_Data                  Luebeck            --
--                                                 Spring, 2018       --
--  Interface                                                         --
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

with Synchronization.Interprocess.Process_Call_Service.Process_String;
with Synchronization.Interprocess.Process_Call_Service.Manager;

package Test_Hello_RPC_Server_Data is
   use Synchronization.Interprocess;
   use Process_Call_Service;
   use Process_String;
   use Manager;

   type Shared_Data is new Abstract_Shared_Environment with record
      Greeting : Remote_Function;
      Services : Call_Service_Manager (2, 10, 100, 100);
   end record;

   Data : Shared_Data;

   function Hello
            (  Text   : String;
               Callee : access Call_Service'Class;
               Caller : access Call_Service'Class
            )  return String;

end Test_Hello_RPC_Server_Data;
