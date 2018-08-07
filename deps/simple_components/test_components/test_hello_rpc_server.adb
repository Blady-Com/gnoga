--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Hello_RPC_Client                       Luebeck            --
--                                                 Spring, 2018       --
--  Test client                                                       --
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

with Ada.Text_IO;                 use Ada.Text_IO;
with Test_Hello_RPC_Server_Data;  use Test_Hello_RPC_Server_Data;

with Synchronization.Interprocess.Process_Call_Service.Manager;

procedure Test_Hello_RPC_Server is
   use Synchronization.Interprocess.Process_Call_Service;
   use Manager;
begin
   Open (Data, "test_call_service", True);
   Put ("Waiting for the client ... ");
   Wait_For_Initialization (Data.Services);
   Put_Line (" OK");
   Put_Line ("Servicing for 5s");
   delay 5.0; -- Wait before exit
   Put_Line ("Done");
end Test_Hello_RPC_Server;
