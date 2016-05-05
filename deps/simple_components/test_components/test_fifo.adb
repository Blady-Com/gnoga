--                                                                    --
--  procedure Test_FIFO             Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Autumn, 2006       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Text_IO;     use Ada.Text_IO;

with Test_Integer_FIFO;
with Test_Integer_Signaled_FIFO;
with Test_String_FIFO;
with Test_String_Signaled_FIFO;

procedure Test_FIFO is
begin
   Put_Line ("Plain String FIFO test---------------------------------");
   declare
      use Test_String_FIFO;
      Queue : FIFO (31);
   begin
      Put (Queue, "1234");
      Put (Queue, "5678");
      declare
         Item : constant String := Get (Queue);
      begin
         if "1234" /= Item then
            Raise_Exception
            (  Program_Error'Identity,
               "Indefinite FIFO error got '" & Item & "'/='1234'"
            );
         end if;
      end;
      if Is_Empty (Queue) then
         Raise_Exception
         (  Program_Error'Identity,
            "Indefinite FIFO is empty"
         );
      end if;
      declare
         Item : constant String := Get (Queue);
      begin
         if "5678" /= Item then
            Raise_Exception
            (  Program_Error'Identity,
               "Indefinite FIFO error got '" & Item & "'/='5678'"
            );
         end if;
      end;
      if not Is_Empty (Queue) then
         Raise_Exception
         (  Program_Error'Identity,
            "Indefinite FIFO is not empty"
         );
      end if;
   end;
   Put_Line ("Signaled String FIFO test------------------------------");
   declare
      use Test_String_Signaled_FIFO;
      Queue : Signaled_FIFO (80);

      task type Subscriber;

      task body Subscriber is
      begin
         loop
            Put_Line ("Received " & Get (Queue, 1.0));
         end loop;
      exception
         when End_Error =>
            null;
         when Constraint_Error =>
            Put_Line ("Subscriber exits due to time out");
      end Subscriber;

      Other : Subscriber;
   begin
      for Index in 1..600 loop
         Put (Queue, "package" & Integer'Image (Index), 1.0);
      end loop;
      delay 0.2;      -- Wait a bit, to get the subscriber blocked
      Cancel (Queue); -- and then terminate it
   exception
      when Constraint_Error =>
         Raise_Exception (Program_Error'Identity, "Publisher blocked");
   end;

   Put_Line ("Plain String FIFO test, buffer for just one element----");
   declare
      use Test_String_FIFO;
      Queue : FIFO (31);

      task type Subscriber is
         entry Shut_Down;
      end Subscriber;

      task body Subscriber is
      begin
         loop
            if Is_Empty (Queue) then
               select
                  accept Shut_Down;
                  exit;
               else
                  Put_Line ("Subscriber waits");
                  delay 0.001;  -- Wait for new elements to come
               end select;
            else
               Put_Line ("Received " & Get (Queue));
            end if;
         end loop;
      end Subscriber;

      Other : Subscriber;
   begin
      for Index in 1..200 loop
         loop
            begin
               Put (Queue, "packet" & Integer'Image (Index));
               exit;
            exception
               when Constraint_Error =>
                  Put_Line ("Publisher waits");
                  delay 0.001;  -- Wait for free space
            end;
         end loop;
      end loop;
      Other.Shut_Down;
   end;

   Put_Line ("Signaled FIFO test-------------------------------------");
   declare
      use Test_Integer_Signaled_FIFO;
      Queue : Signaled_FIFO (9);

      task type Subscriber;

      task body Subscriber is
      begin
         loop
            Put_Line ("Received " & Integer'Image (Get (Queue, 1.0)));
         end loop;
      exception
         when End_Error =>
            null;
         when Constraint_Error =>
            Put_Line ("Subscriber exits due to time out");
      end Subscriber;

      Other : Subscriber;
   begin
      for Index in 1..800 loop
         Put (Queue, Index, 1.0);
      end loop;
      delay 0.2;      -- Wait a bit, to get the subscriber blocked
      Cancel (Queue); -- and then terminate it
   end;

   Put_Line ("Plain FIFO test----------------------------------------");
   declare
      use Test_Integer_FIFO.Integer_FIFOs;
      Queue : FIFO (21);

      task type Subscriber is
         entry Shut_Down;
      end Subscriber;

      task body Subscriber is
      begin
         loop
            if Is_Empty (Queue) then
               select
                  accept Shut_Down;
                  exit;
               else
                  Put_Line ("Subscriber waits");
                  delay 0.001;  -- Wait for new elements to come
               end select;
            else
               Put_Line ("Received" & Integer'Image (Get (Queue)));
            end if;
         end loop;
      end Subscriber;

      Other : Subscriber;
   begin
      for Index in 1..200 loop
         while Is_Full (Queue) loop
            Put_Line ("Publisher waits");
            delay 0.001;  -- Wait for free space
         end loop;
         Put (Queue, Index);
      end loop;
      Other.Shut_Down;
   end;

   Put_Line ("Purge FIFO test----------------------------------------");
   declare
      use Test_Integer_FIFO;
      Queue : Integer_FIFO (11);
      Count : Natural;
   begin
      for Index in 1..10 loop
         Put (Queue, Index);
      end loop;
      Purge (Queue, Count);
      if Count /= 5 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong count" & Integer'Image (Count)
         );
      end if;
      Delete (Queue, 5);
      for Index in 11..20 loop
         Put (Queue, Index);
      end loop;
      if Count /= 5 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong count" & Integer'Image (Count)
         );
      end if;
      for Index in 1..5 loop
         Put_Line ("Preserved:" & Integer'Image (Get (Queue)));
      end loop;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_FIFO;
