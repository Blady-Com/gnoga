--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Linked_Lists_Scheduler                 Luebeck            --
--  Test package implementation                    Summer, 2007       --
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

with Ada.Text_IO;  use Ada.Text_IO;

package body Test_Linked_Lists_Scheduler is

   Queue_Closed : exception;

   protected Waiting_Queue is
      entry Get_For_Service (Work : out Item);
      procedure Shut_Down;
      procedure Submit (Work : Item);
   private
      Queue  : List;
      Closed : Boolean;
   end Waiting_Queue;

   protected body Waiting_Queue is
      entry Get_For_Service (Work : out Item)
         when Closed or else Queue /= null is
      begin
         if Closed then
            raise Queue_Closed;
         else
            Take (Queue, Work);  -- The first in the list
         end if;
      end Get_For_Service;

      procedure Submit (Work : Item) is
      begin
         Append (Queue, Work); -- Add to the end
         if Closed then
            Erase (Queue);
         end if;
      end Submit;

      procedure Shut_Down is
      begin
         Closed := True;
         Erase (Queue);
      end Shut_Down;

   end Waiting_Queue;

   task body Worker is
      This : Item;
   begin
      loop
         Waiting_Queue.Get_For_Service (This);
         -- Now we are holding This, so be careful with exceptions,  the
         -- item must back to the queue in all cases
         begin
            Do_It (This.all);
               -- Item has been serviced, return it back
            Waiting_Queue.Submit (This);
         exception
            when Queue_Closed =>
               exit;
            when others =>
               Waiting_Queue.Submit (This);
         end;
      end loop;
   end Worker;

   procedure Submit (Work : Item) is
   begin
      Waiting_Queue.Submit (Work);
   end Submit;

   procedure Shut_Down is
   begin
      Waiting_Queue.Shut_Down;
   end Shut_Down;

   procedure Do_It (Work : in out Print_Me) is
   begin
      Put_Line (Work.Text);
   end Do_It;

   function Have_To_Print (Text : String) return Item is
   begin
      return
         new Print_Me'
             (  Job
             with
                Length => Text'Length,
                Text   => Text
             );
   end Have_To_Print;

end Test_Linked_Lists_Scheduler;
