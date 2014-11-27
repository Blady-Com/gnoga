--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Synchronization_Events                 Luebeck            --
--  Test                                           Spring, 2008       --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;

with Ada.Finalization;
with Synchronization.Mutexes;
with Test_Synchronization_Pulse_Events;
with Test_Synchronization_Events_Array;

procedure Test_Synchronization_Events is
begin
   Put_Line ("Array of events test");
   declare
      use Test_Synchronization_Events_Array;
      use Events_Arrays;
      Worker_State : Events_Array;

      task type Worker (ID : Worker_ID);
      task body Worker is
         Dice : Generator;
      begin
         Reset (Dice);
         for Index in 1..10 loop
            Put_Line
            (  Worker_ID'Image (ID)
            &  " doing things"
            &  Integer'Image (Index)
            );
            delay Duration (Random (Dice) * 0.100);
            Worker_State.Signal (ID, All_Signaled);
            Worker_State.Reset (ID);
         end loop;
         Put_Line (Worker_ID'Image (ID) & " finished");
      end Worker;

      T1 : Worker (A);
      T2 : Worker (B);
      T3 : Worker (C);
   begin
      null;
   end;
   Put_Line ("Pulse event test");
   declare
      use Test_Synchronization_Pulse_Events;

      Event : Pulse_Event;
      task type Waiter (Name : access String);
      task body Waiter is
         Value : Positive;
      begin
         for Index in Positive'Range loop
            select
               Event.Wait (Value);
               Put_Line (Name.all & " got" & Integer'Image (Value));
            or delay 1.0;
               exit;
            end select;
         end loop;
      end Waiter;
      N1 : aliased String := "A  |  |  |  |  |";
      N2 : aliased String := "|  B  |  |  |  |";
      N3 : aliased String := "|  |  C  |  |  |";
      N4 : aliased String := "|  |  |  D  |  |";
      N5 : aliased String := "|  |  |  |  E  |";
      N6 : aliased String := "|  |  |  |  |  F";
      T1 : Waiter (N1'Access);
      T2 : Waiter (N2'Access);
      T3 : Waiter (N3'Access);
      T4 : Waiter (N4'Access);
      T5 : Waiter (N5'Access);
      T6 : Waiter (N6'Access);
   begin
      for Index in 1..1000 loop
         Event.Pulse (Index);
         delay 0.0;
      end loop;
   end;
   Put_Line ("Mutex test");
   declare
      use Synchronization.Mutexes;
      Resource : aliased Mutex;
   begin
      declare
         Lock : Holder (Resource'Access);
      begin
         declare
            Lock : Holder (Resource'Access);
         begin
            Put_Line ("Got a mutex recursively");
            raise Constraint_Error;
         end;
      end;
   exception
      when Constraint_Error =>
         if Resource.Is_Owned then
            raise Program_Error;
         end if;
         Put_Line ("Mutex was released");
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Synchronization_Events;
