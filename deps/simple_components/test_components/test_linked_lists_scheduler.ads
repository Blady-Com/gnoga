--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Linked_Lists_Scheduler                 Luebeck            --
--  Test package interface                         Summer, 2007       --
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

with Ada.Finalization;
with Generic_Doubly_Linked;

package Test_Linked_Lists_Scheduler is
--
-- Job -- Abstract piece of work
--
   type Job is
      abstract new Ada.Finalization.Controlled with
         null record;
   procedure Do_It (Work : in out Job) is abstract;
   
   package Job_List is new Generic_Doubly_Linked (Job'Class);
   use Job_List.Doubly_Linked;
--
-- Worker -- A task doing jobs
--
   task type Worker;
--
-- Submit -- A new job for processing
--
   procedure Submit (Work : Item);
--
-- Shut_Down -- Purge the jobs queue and stop workers
--
   procedure Shut_Down;
--
-- Print_Me -- A concrete job, prints some text
--
   type Print_Me (Length : Natural) is new Job with record
      Text : String (1..Length);
   end record;
   procedure Do_It (Work : in out Print_Me);
   function Have_To_Print (Text : String) return Item;   

end Test_Linked_Lists_Scheduler;
