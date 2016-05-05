--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Blackboard_Performance                 Luebeck            --
--  Test blackboard operations performance         Autumn, 2009       --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Real_Time;            use Ada.Real_Time;
with Ada.Text_IO;              use Ada.Text_IO;
with System.Storage_Elements;  use System.Storage_Elements;
with Test_Record_Blackboards;  use Test_Record_Blackboards;

procedure Test_Blackboard_Performance is

   Rounds : constant := 5_000_000;
   use Boards;

   procedure Report
             (  Text   : String;
                Span   : Time_Span;
                Rounds : Integer
             )  is
      type Nanoseconds is delta 0.1 range -100.0..1_000_000.0;
   begin
      Put_Line
      (  Text
      &  Nanoseconds'Image
         (  Nanoseconds
            (  Float (To_Duration (Span)) * 1.0E9
            /  Float (Rounds)
         )  )
      &  " ns"
      );
   end Report;

   type Blackboard_Ptr is access Blackboard;
   T1, T2, T3 : Time;
begin
   delay 0.001; -- GNAT bug workaround
   declare
      Data_Ptr : constant Blackboard_Ptr := -- Don't allocate it on the
                    new Blackboard          -- stack
                        (  (2 * Rounds + 1)
                        *  (  (  Element'Size
                              +  Storage_Element'Size
                              -  1
                              )
                           /  Storage_Element'Size
                        )  );
      Data : Blackboard renames Data_Ptr.all;
      Item : Element;
      This : Reference;
   begin
      T1 := Clock;
      for I in 1..Rounds loop
         Put (Data, (others => 0));
      end loop;
      T2 := Clock;
      for I in 1..Rounds loop
         Put (Data, (others => 0));
         Put (Data, (others => 0));
      end loop;
      T3 := Clock;
      Report ("writing blackboard", T3 - T2 - (T2 - T1), Rounds);
      T1 := Clock;

      This := First (Data);
      T1 := Clock;
      for I in 1..Rounds loop
         Item := Get (Data, This);
      end loop;
      This := First (Data);
      T2 := Clock;
      for I in 1..Rounds loop
         Item := Get (Data, This);
         Item := Get (Data, This);
      end loop;
      T3 := Clock;
      Report ("reading blackboard", T3 - T2 - (T2 - T1), Rounds);
      T1 := Clock;

      T1 := Clock;
      for I in 1..Rounds * 10 loop
         Item (1) := Item (I mod 10 + 1);
         Item := (others => 0);
      end loop;
      T2 := Clock;
      for I in 1..Rounds * 10 loop
         Item := (others => 0);
         Item (1) := Item (I mod 10 + 1);
         Item := (others => 1);
      end loop;
      T3 := Clock;
      Report ("raw writing", T3 - T2 - (T2 - T1), Rounds * 10);
      T1 := Clock;

   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Blackboard_Performance;
