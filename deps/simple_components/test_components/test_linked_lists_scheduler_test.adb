--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Linked_Lists_Scheduler_Test            Luebeck            --
--  Test                                           Summer, 2007       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2010  --
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

with Test_Linked_Lists_Scheduler;  use Test_Linked_Lists_Scheduler;

procedure Test_Linked_Lists_Scheduler_Test is
   W1 : Worker;
   W2 : Worker;
   W3 : Worker;
   W4 : Worker;
   W5 : Worker;
begin
   Submit (Have_To_Print ("The"));
   Submit (Have_To_Print ("quick"));
   Submit (Have_To_Print ("brown"));
   Submit (Have_To_Print ("fox"));
   Submit (Have_To_Print ("jumps"));
   Submit (Have_To_Print ("over"));
   Submit (Have_To_Print ("the"));
   Submit (Have_To_Print ("lazy"));
   Submit (Have_To_Print ("dog"));
   delay 10.0;
   Shut_Down;
end Test_Linked_Lists_Scheduler_Test;
