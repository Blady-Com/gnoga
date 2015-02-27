--                                                                    --
--  package Generic_Cubic_Spline    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2012       --
--                                                                    --
--                                Last revision :  23:15 18 Feb 2015  --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Generic_Cubic_Spline is

   function Acceleration
            (  Offset       : Number;
               Coefficients : Spline_Coefficients
            )  return Number is
   begin
      return 6.0 * Coefficients.D * Offset + 2.0 * Coefficients.C;
   end Acceleration;

   function Acceleration (Spline : Cubic_Spline; Argument : Number)
      return Number is
      Intervals : Map renames Spline.Intervals;
      Index     : Integer := Find (Intervals, Argument);
   begin
      if Index > 0 then -- Exact hit
         return Get (Intervals, Index).C;
      else
         Index := - Index - 1;
         if Index = 0 then -- Left of the interval
            return 0.0;
         elsif Index >= Get_Size (Intervals) then
            return 0.0;
         else
            return Acceleration
                   (  Argument - Get_Key (Intervals, Index),
                      Get (Intervals, Index)
                   );
         end if;
      end if;
   end Acceleration;

   function Get
            (  Spline : Cubic_Spline;
               Index  : Positive
            )  return Pair is
   begin
      return
      (  X => Get_Key (Spline.Intervals, Index),
         Y => Get (Spline.Intervals, Index).A
      );
   end Get;

   function Get_Size (Spline : Cubic_Spline) return Natural is
   begin
      return Get_Size (Spline.Intervals);
   end Get_Size;

   procedure Solve (Data : in out Map) is
      procedure Set (Index : Positive; B, C, D : Number) is
      begin
         Replace (Data, Index, (Get (Data, Index).A, B, C, D));
      end Set;
      N     : constant Natural := Get_Size (Data);
      DX, D : array (1..N - 1) of Number;
      A     : array (2..N - 1) of Number;
      C     : array (1..N)     of Number;
   begin
   --
   -- Calculate interval differences of the arguments and values
   --
      for I in DX'Range loop
         DX (I) := Get_Key (Data, I + 1) - Get_Key (Data, I);
         D  (I) := (Get (Data, I + 1).A - Get (Data, I).A) / DX (I);
      end loop;
   --
   -- The system: to solve:
   --
   -- [Aij] [Si] = [Ci]                    Ci = 3 dYi/dXi - dYi-1/dXi-1
   --
   -- | 2(dX1+dX2)     dX2        0      ...     0          0         |
   -- |     dX2    2(dX2+dX3)    dX3     ...     0          0         |
   -- |      0         dX3    2(dX3+dX4) ...     0          0         |
   -- |     ...        ...       ...     ...    ...        ...        |
   -- |      0          0         0      ...    dXN-2  2(dXN-2+dXN-1) |
   --
      A (2) := 2.0 * (DX (2) + DX (1));  -- Diagonal element
      C (2) := 3.0 * ( D (2) -  D (1));  -- Right side
      for I in 2..N - 2 loop -- Excluding elements under the diagonal
         declare
            F : constant Number := DX (I) / A (I);
         begin
            A (I + 1) := 2.0 * (DX (I + 1) + DX (I)) - DX (I) * F;
            C (I + 1) := 3.0 * ( D (I + 1) -  D (I)) -  C (I) * F;
         end;
      end loop;
--        declare
--           function Image (X : Number) return String is
--              type Fixed is delta 0.01 digits 9;
--           begin
--              return Fixed'Image (Fixed (X));
--           end Image;
--           function Image (X : Natural) return String is
--              Result : String := Integer'Image (X);
--           begin
--              return Result (Result'First + 1..Result'Last);
--           end Image;
--        begin
--           for I in A'Range loop
--              Ada.Text_IO.Put_Line
--              (  "A" & Image (I) & Image (I) & " =" & Image (A (I))
--              &  "  B" & Image (I) & " =" & Image (C (I))
--              );
--           end loop;
--        end;
   --
   -- Now the matrix is this:
   --
   -- |  A(2)  dX2    0    ...   0      0     |    |  C(2)  |
   -- |   0    A(3)  dX3   ...   0      0     |    |  C(3)  |
   -- |   0     0    A(4)  ...   0      0     |    |  C(4)  |
   -- |  ...   ...    ...  ...  ...    ...    |    |  ...   |
   -- |   0     0     0    ...   0    A(XN-1) |    | C(N-1) |
   --
   -- Excluding elements above the diagonal
   --
      C (N - 1) := C (N - 1) / A (N - 1);
      for I in reverse 2..N - 2 loop
         C (I) := (C (I) - DX (I) * C (I + 1)) / A (I);
      end loop;
      C (1) := 0.0;
      C (N) := 0.0;
--        declare
--           function Image (X : Number) return String is
--              type Fixed is delta 0.01 digits 9;
--           begin
--              return Fixed'Image (Fixed (X));
--           end Image;
--           function Image (X : Natural) return String is
--              Result : String := Integer'Image (X);
--           begin
--              return Result (Result'First + 1..Result'Last);
--           end Image;
--        begin
--           for I in 1..N loop
--              Ada.Text_IO.Put_Line
--              (  "C" & Image (I) & " =" & Image (C (I))
--              );
--           end loop;
--        end;
   --
   -- Setting spline coefficients as:
   --
   --     Bi = dYi/dXi - dXi (2 Ci + Ci+1) / 3
   --     Ci = Bi
   --     Di = (Ci+1 - Ci) / 3 dXi
   --
      for I in 1..N - 1 loop
         Set
         (  I,
            D (I) - DX (I) * (2.0 * C (I) + C (I + 1)) / 3.0,
            C (I),
            (C (I + 1) - C (I)) / (3.0 * DX (I))
         );
      end loop;
      Set
      (  N,
         Velocity (DX (N - 1), Get (Data, N - 1)),
         0.0,
         0.0
      );
--        declare
--           function Image (X : Number) return String is
--              type Fixed is delta 0.01 digits 9;
--           begin
--              return Fixed'Image (Fixed (X));
--           end Image;
--           function Image (X : Natural) return String is
--              Result : String := Integer'Image (X);
--           begin
--              return Result (Result'First + 1..Result'Last);
--           end Image;
--        begin
--           for I in 1..N - 1 loop
--              declare
--                 This : Spline_Coefficients renames Get (Data, I);
--              begin
--                 Ada.Text_IO.Put_Line
--                 (  "X" & Image (I) & " =" & Image (Get_Key (Data, I))
--                 &  " dX" & Image (I) & " =" & Image (DX (I))
--                 &  "   A" & Image (I) & " =" & Image (This.A)
--                 &  " B" & Image (I) & " =" & Image (This.B)
--                 &  " C" & Image (I) & " =" & Image (This.C)
--                 &  " D" & Image (I) & " =" & Image (This.D)
--                 &  "   Y(R)=" & Image (Value (DX (I), This))
--                 &  " Y'(R)="  & Image (Velocity (DX (I), This))
--                 &  " Y""(R)=" & Image (Acceleration (DX (I), This))
--                 );
--              end;
--           end loop;
--        end;
   end Solve;

   procedure Set
             (  Spline : in out Cubic_Spline;
                Points : in out Map
             )  is
   begin
      case Get_Size (Points) is
         when 0 | 1 =>
            null;
         when 2 =>
            declare
               Left  : Spline_Coefficients := Get (Points, 1);
               Right : Spline_Coefficients := Get (Points, 2);
            begin
               Left.B :=
                  (  (Right.A - Left.A)
                  /  (Get_Key (Points, 2) - Get_Key (Points, 1))
                  );
               Right.B := Left.B;
               Replace (Points, 1, Left);
               Replace (Points, 2, Right);
            end;
         when others =>
            Solve (Points);
      end case;
      Spline.Intervals := Points;
   end Set;

   procedure Set
             (  Spline : in out Cubic_Spline;
                Pairs  : Pairs_Array
             )  is
      Data : Map;
   begin
      for Index in Pairs'Range loop
         begin
            Add
            (  Data,
               Pairs (Index).X,
               (Pairs (Index).Y, others => 0.0)
            );
         exception
            when Constraint_Error =>
               if Pairs (Index).Y /= Get (Data, Pairs (Index).X).A then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Conflicting (x,y)-pairs in the list"
                  );
               end if;
         end;
      end loop;
      Set (Spline, Data);
   end Set;

   procedure Set_From_Container
             (  Spline : in out Cubic_Spline;
                Pairs  : Abstract_Pairs_Container'Class
             )  is
      Data : Map;
      This : Pair;
   begin
      for Index in 1..Get_Size (Pairs) loop
         This := Get (Pairs, Index);
         begin
            Add (Data, This.X, (This.Y, others => 0.0));
         exception
            when Constraint_Error =>
               if This.Y /= Get (Data, This.X).A then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Conflicting (x,y)-pairs in the list"
                  );
               end if;
         end;
      end loop;
      Set (Spline, Data);
   end Set_From_Container;

   function Value
            (  Offset       : Number;
               Coefficients : Spline_Coefficients
            )  return Number is
   begin
      return
      (  (  (  Coefficients.D * Offset
            +  Coefficients.C
            )
          *  Offset
          +  Coefficients.B
          )
       *  Offset
       +  Coefficients.A
       );
   end Value;

   function Value (Spline : Cubic_Spline; Argument : Number)
      return Number is
      Intervals : Map renames Spline.Intervals;
      Index     : Integer := Find (Intervals, Argument);
   begin
      if Index > 0 then -- Exact hit
         return Get (Intervals, Index).A;
      else
         Index := - Index - 1;
         if Index = 0 then
            declare
               This : Spline_Coefficients renames Get (Intervals, 1);
            begin
               return
                  This.A + This.B * (Argument - Get_Key (Intervals, 1));
            end;
         elsif Index >= Get_Size (Spline.Intervals) then
            declare
               Size : constant Natural := Get_Size (Intervals);
               This : Spline_Coefficients renames Get (Intervals, Size);
            begin
               return This.A +
                      This.B * (Argument - Get_Key (Intervals, Size));
            end;
         end if;
         return Value
                (  Argument - Get_Key (Intervals, Index),
                   Get (Intervals, Index)
                );
      end if;
   end Value;

   function Velocity
            (  Offset       : Number;
               Coefficients : Spline_Coefficients
            )  return Number is
   begin
      return
      (  (  3.0 * Coefficients.D * Offset
         +  2.0 * Coefficients.C
         )
      *  Offset
      +  Coefficients.B
      );
   end Velocity;

   function Velocity (Spline : Cubic_Spline; Argument : Number)
      return Number is
      Intervals : Map renames Spline.Intervals;
      Index : Integer := Find (Intervals, Argument);
   begin
      if Index > 0 then -- Exact hit
         return Get (Intervals, Index).B;
      else
         Index := - Index - 1;
         if Index = 0 then -- Left of the interval
            return Get (Intervals, 1).B;
         elsif Index >= Get_Size (Intervals) then
            return Get (Intervals, Get_Size (Intervals)).B;
         else
            return Velocity
                   (  Argument - Get_Key (Intervals, Index),
                      Get (Intervals, Index)
                   );
         end if;
      end if;
   end Velocity;

end Generic_Cubic_Spline;
