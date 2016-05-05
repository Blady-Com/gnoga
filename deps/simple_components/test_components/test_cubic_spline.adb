--                                                                    --
--  procedure Test_Cubic_Spline     Copyright (c)  Dmitry A. Kazakov  --
--  Test program                                   Luebeck            --
--                                                 Spring, 2012       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of  the  GNU  Library  General  Public  --
--  License  as  published  by  the Free Software Foundation; either  --
--  version 2 of the License, or (at your option) any later version.  --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--                                                                    --
--  This  library is distributed in the hope that it will be useful,  --
--  but WITHOUT ANY WARRANTY; without even the implied  warranty  of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  Library General Public License for more details.                  --
--                                                                    --
--  You  should  have  received  a  copy  of the GNU Library General  --
--  Public  License  along  with  this library; if not, write to the  --
--  Free Software Foundation, Inc.,  675  Mass  Ave,  Cambridge,  MA  --
--  02139, USA.                                                       --
--____________________________________________________________________--

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Text_IO;              use Ada.Text_IO;
with Long_Float_Cubic_Spline;  use Long_Float_Cubic_Spline;

with Ada.Numerics.Long_Elementary_Functions;

procedure Test_Cubic_Spline is
   Spline : Cubic_Spline;

   procedure Check (X, Y : Long_Float; Text : String) is
      V : constant Long_Float := Value (Spline, X);
   begin
      if abs (Y - V) > abs (Y + V) * 0.01 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Y ("
            &  Long_Float'Image (X)
            &  " ) ="
            &  Long_Float'Image (V)
            &  " /="
            &  Long_Float'Image (Y)
            &  " (expected) "
            &  Text
         )  );
      end if;
   end Check;

   procedure Check_Acceleration (X, Y : Long_Float; Text : String) is
      A : constant Long_Float := Acceleration (Spline, X);
   begin
      if abs (Y - A) > abs (A + Y) * 0.01 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Y""("
            &  Long_Float'Image (X)
            &  " ) ="
            &  Long_Float'Image (A)
            &  " /="
            &  Long_Float'Image (Y)
            &  " (expected) "
            &  Text
         )  );
      end if;
   end Check_Acceleration;

   procedure Check_Velocity (X, Y : Long_Float; Text : String) is
      V : constant Long_Float := Velocity (Spline, X);
   begin
      if abs (Y - V) > abs (Y + V) * 0.01 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Y'("
            &  Long_Float'Image (X)
            &  " ) ="
            &  Long_Float'Image (V)
            &  " /="
            &  Long_Float'Image (Y)
            &  " (expected) "
            &  Text
         )  );
      end if;
   end Check_Velocity;

begin
   Set (Spline, (1 => (1.0, 5.0)));
   Check (0.0, 5.0, "one point test (1,5)");
   Check (1.0, 5.0, "one point test (1,5)");
   Check (3.0, 5.0, "one point test (1,5)");

   Check_Velocity (0.0, 0.0, "one point test (1,5)");
   Check_Velocity (1.0, 0.0, "one point test (1,5)");
   Check_Velocity (3.0, 0.0, "one point test (1,5)");

   Check_Acceleration (0.0, 0.0, "one point test (1,5)");
   Check_Acceleration (1.0, 0.0, "one point test (1,5)");
   Check_Acceleration (3.0, 0.0, "one point test (1,5)");

   Set (Spline, ((1.0, 1.0), (0.0, 0.0)));
   Check (-1.0, -1.0, "two points test (0,0)(1,1)");
   Check ( 0.0,  0.0, "two points test (0,0)(1,1)");
   Check ( 0.2,  0.2, "two points test (0,0)(1,1)");
   Check ( 0.8,  0.8, "two points test (0,0)(1,1)");
   Check ( 1.0,  1.0, "two points test (0,0)(1,1)");
   Check ( 3.0,  3.0, "two points test (0,0)(1,1)");

   Check_Velocity (-1.0, 1.0, "two points test (0,0)(1,1)");
   Check_Velocity ( 0.0, 1.0, "two points test (0,0)(1,1)");
   Check_Velocity ( 0.2, 1.0, "two points test (0,0)(1,1)");
   Check_Velocity ( 0.8, 1.0, "two points test (0,0)(1,1)");
   Check_Velocity ( 1.0, 1.0, "two points test (0,0)(1,1)");
   Check_Velocity ( 3.0, 1.0, "two points test (0,0)(1,1)");

   Check_Acceleration (-1.0, 0.0, "two points test (0,0)(1,1)");
   Check_Acceleration ( 0.0, 0.0, "two points test (0,0)(1,1)");
   Check_Acceleration ( 0.2, 0.0, "two points test (0,0)(1,1)");
   Check_Acceleration ( 0.8, 0.0, "two points test (0,0)(1,1)");
   Check_Acceleration ( 1.0, 0.0, "two points test (0,0)(1,1)");
   Check_Acceleration ( 3.0, 0.0, "two points test (0,0)(1,1)");

   Set (Spline, ((1.0, 1.0), (0.0, 0.0), (-2.0, -2.0)));
   Check (-0.7, -0.7, "3-points test (-2,-2)(0,0)(1,1)");
   Check ( 0.2,  0.2, "3-points test (-2,-2)(0,0)(1,1)");
   Check (-7.0, -7.0, "3-points test (-2,-2)(0,0)(1,1)");
   Check ( 4.0,  4.0, "3-points test (-2,-2)(0,0)(1,1)");

   Check_Velocity (-0.7, 1.0, "3-points test (-2,-2)(0,0)(1,1)");
   Check_Velocity ( 0.2, 1.0, "3-points test (-2,-2)(0,0)(1,1)");
   Check_Velocity (-7.0, 1.0, "3-points test (-2,-2)(0,0)(1,1)");
   Check_Velocity ( 4.0, 1.0, "3-points test (-2,-2)(0,0)(1,1)");

   Check_Acceleration (-0.7, 0.0, "3-points test (-2,-2)(0,0)(1,1)");
   Check_Acceleration ( 0.2, 0.0, "3-points test (-2,-2)(0,0)(1,1)");
   Check_Acceleration (-7.0, 0.0, "3-points test (-2,-2)(0,0)(1,1)");
   Check_Acceleration ( 4.0, 0.0, "3-points test (-2,-2)(0,0)(1,1)");

   Set (Spline, ((0.0, 0.0), (1.0, 1.0), (4.0, 4.0), (6.0, 6.0)));
   Check (-0.7, -0.7, "4-points test (0,0)(1,1)(4,4)(6,6)");
   Check ( 0.2,  0.2, "4-points test (0,0)(1,1)(4,4)(6,6)");
   Check (-7.0, -7.0, "4-points test (0,0)(1,1)(4,4)(6,6)");
   Check ( 4.0,  4.0, "4-points test (0,0)(1,1)(4,4)(6,6)");
   Check ( 5.0,  5.0, "4-points test (0,0)(1,1)(4,4)(6,6)");
   Check ( 2.0,  2.0, "4-points test (0,0)(1,1)(4,4)(6,6)");

   Check_Velocity (-0.7, 1.0, "4-points test (0,0)(1,1)(4,4)(6,6)");
   Check_Velocity ( 0.2, 1.0, "4-points test (0,0)(1,1)(4,4)(6,6)");
   Check_Velocity (-7.0, 1.0, "4-points test (0,0)(1,1)(4,4)(6,6)");
   Check_Velocity ( 4.0, 1.0, "4-points test (0,0)(1,1)(4,4)(6,6)");
   Check_Velocity ( 5.0, 1.0, "4-points test (0,0)(1,1)(4,4)(6,6)");
   Check_Velocity ( 2.0, 1.0, "4-points test (0,0)(1,1)(4,4)(6,6)");

   declare
      use Ada.Numerics.Long_Elementary_Functions;
      Pairs : Pairs_Array (1..10);
      X     : Long_Float;
   begin
      Pairs (1)  := (0.00, Sin (0.00));
      Pairs (2)  := (0.01, Sin (0.01));
      Pairs (3)  := (0.03, Sin (0.03));
      Pairs (4)  := (0.06, Sin (0.06));
      Pairs (5)  := (0.07, Sin (0.07));
      Pairs (6)  := (0.10, Sin (0.10));
      Pairs (7)  := (0.13, Sin (0.13));
      Pairs (8)  := (0.15, Sin (0.15));
      Pairs (9)  := (0.19, Sin (0.19));
      Pairs (10) := (0.23, Sin (0.23));
      Set (Spline, Pairs);
      for I in 0..23 loop
         X := Long_Float (I) * 0.01;
         Check (X, Sin (X), "sin test");
      end loop;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Cubic_Spline;
