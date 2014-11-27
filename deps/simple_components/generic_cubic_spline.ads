--                                                                    --
--  package Generic_Cubic_Spline    Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2012       --
--                                                                    --
--                                Last revision :  14:32 02 Apr 2012  --
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
--
--  The cubic spline is a piece-wise polynomial interpolation, that uses
--  3rd  order  polynom  on  each of the intervals. differently to other
--  methods  splines are numerically stable. The spline goes through all
--  specified points and has the first and second differential same left
--  and right of each inner interpolation point.
--
with Ada.Finalization;
with Generic_Map;

generic
   type Number is digits <>;
package Generic_Cubic_Spline is
--
-- Pair -- The argument and the value of the interpolated function
--
   type Pair is record
      X : Number;
      Y : Number;
   end record;
   type Pairs_Array is array (Positive range <>) of Pair;
--
-- Abstract_Points_Container -- A container of (x,y)-pairs
--
   type Abstract_Pairs_Container is
      abstract new Ada.Finalization.Limited_Controlled with null record;
--
-- Get -- A pair from the container
--
--    Container - Of pairs
--    Index     - Of the pair 1..
--
-- Returns :
--
--    The pair
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Get
            (  Container : Abstract_Pairs_Container;
               Index     : Positive
            )  return Pair is abstract;
--
-- Get_Size -- Get the number pairs
--
--    Container - Of pairs
--
-- Returns :
--
--    The number of pairs in the container
--
   function Get_Size (Container : Abstract_Pairs_Container)
      return Natural is abstract;
--
-- Qubic_Spline -- A qubic spline object
--
   type Cubic_Spline is new Abstract_Pairs_Container with private;
--
-- Acceleration -- 2nd differential value of the spline
--
--    Spline   - The object
--    Argument - The argument where spline is to be evaluated
--
-- Returns :
--
--    The spline's second differential
--
-- Exceptions :
--
--    Constraint_Error - Evaluation error
--
   function Acceleration (Spline : Cubic_Spline; Argument : Number)
      return Number;
--
-- Get -- Implementation of pairs container interface
--
   function Get
            (  Spline : Cubic_Spline;
               Index  : Positive
            )  return Pair;
--
-- Get_Size -- Implementation of pairs container interface
--
   function Get_Size (Spline : Cubic_Spline) return Natural;
--
-- Value -- Spline value
--
--    Spline   - The object
--    Argument - The argument where spline is to be evaluated
--
-- Returns :
--
--    The spline
--
-- Exceptions :
--
--    Constraint_Error - Evaluation error
--
   function Value (Spline : Cubic_Spline; Argument : Number)
      return Number;
--
-- Velocity -- 1st differential value
--
--    Spline   - The object
--    Argument - The argument where spline is to be evaluated
--
-- Returns :
--
--    The spline's first differential
--
-- Exceptions :
--
--    Constraint_Error - Evaluation error
--
   function Velocity (Spline : Cubic_Spline; Argument : Number)
      return Number;
--
-- Set -- Spline from an array of points
--
--    Spline - The spline to set
--    Pairs  - Array of pairs (X,Y), possibly unsorted
--
-- This procedure sets the spline to interpolate the  specified  set  of
-- pairs. Upon an error, the spline object is not changed.
--
-- Exceptions :
--
--    Constraint_Error - Evaluation error or illegal spline defintion
--
   procedure Set (Spline : in out Cubic_Spline; Pairs : Pairs_Array);
--
-- Set_From_Container -- Spline from a container of points
--
--    Spline - The spline to set
--    Pairs  - An abstract container of pairs (X,Y), possibly unsorted
--
-- This procedure sets the spline to interpolate the  specified  set  of
-- pairs. Upon an error, the spline object is not changed.
--
-- Exceptions :
--
--    Constraint_Error - Evaluation error or illegal spline defintion
--
   procedure Set_From_Container
             (  Spline : in out Cubic_Spline;
                Pairs  : Abstract_Pairs_Container'Class
             );
private
--
--     Si (X) = Ai + Bi (X - Xi) + Ci (X - Xi)**2 + Di (X - Xi)**3
--     Si'(X) = Bi + 2 Ci (X - Xi) + 3 Di (X - Xi)**2
--     Si"(X) = 2 Ci + 6 Di (X - Xi)
--
--     Si (Xi)= Yi => Ai = Yi
--
--     Si (Xi+1) = Si+1 (Xi+1)
--     Si'(Xi+1) = Si+1'(Xi+1)         C1 = CN-1 = 0  (zero acceleration
--     Si"(Xi+1) = Si+1"(Xi+1)                         at the ends)
--
--     dXi = Xi+1 - Xi
--     dYi = Yi+1 - Yi
--                                          x Y4     x Y5
--                  x Y2                    |        |
--       * Y1       |        x Y3           |        |
--       |    S1    |   S2   |      S3      |   S4   |
--     --+----------+--------+--------------+--------+------
--       X1         X2       X3             X4       X5
--           dX1
--
--  i=1..N-1                       Equality of values
--                                 Si (Xi+1) = Si+1 (Xi+1) <=>
--  <=>  Ai + Bi dXi + Ci dXi**2 + Di dXi**3 = Ai+1        <=>
--  <=>       Bi dXi + Ci dXi**2 + Di dXi**3 = dYi         <=>
--  <=>              Bi + Ci dXi + Di dXi**2 = dYi/dXi               (1)
--
--  i=1..N-2                       Equality of the first differntials
--                                 Si'(Xi+1) = Si+1'(Xi+1) <=>
--  <=>          Bi + 2 Ci dXi + 3 Di dXi**2 = Bi+1        <=>
--  <=>   Bi - Bi+1 + 2 Ci dXi + 3 Di dXi**2 = 0                     (2)
--
--  i=1..N-2                       Equality of the second differntials
--                                 Si"(Xi+1) = Si+1"(Xi+1) <=>
--  <=>                      2 Ci + 6 Di dXi = 2 Ci+1      <=>
--  <=>                        Ci + 3 Di dXi = Ci+1        <=>
--  <=>                 Ci - Ci+1 + 3 Di dXi = 0                     (3)
--
--  (2)-(3)*dXi
--               Bi - Bi+1 + (Ci + Ci+1) dXi = 0                     (4)
--  3*(1)-(3)*dXi
--                  3 Bi + (2 Ci + Ci+1) dXi = 3 dYi/dXi             (5)
--
--  3*(4)-(5)+(5>>1)
--         3 Bi - 3 Bi+1 + 3 Ci + 3 Ci+1 dXi = 0
---      - 3 Bi          - 2 Ci   - Ci+1 dXi = - 3 dYi/dXi
--        3 Bi+1 + 2 Ci+1 dXi+1 + Ci+2 dXi+1 = 3 dYi+1/dXi+1
--     -------------------------------------------------------
-- Ci dXi + 2 Ci+1 (dXi + dXi+1) + Ci+2 dXi+1 = 3(dYi+1/dXi+1 - dYi/dXi)
--
--  Which gives 3-band matrix: for C2..CN-2
--
--  |  2(dX1+dX2)   X2        0         0    ...    0         0        |
--  |     X2    2(dX2+dX3)   X3         0    ...    0         0        |
--  |      0        X3   2(dX3+dX4)     0    ...    0         0        |
--  |      0         0       X4   2(dX4+dX5) ...    0         0        |
--  |     ...       ...      ...       ...   ...   ...       ...       |
--  |      0         0        0         0    ... dXN-2  2(dXN-2+dXN-1) |
--
--  The right part of the system is 3(dYi/dXi - dYi-1/dXi-1)
--
--     Di = (Ci+1 - Ci) / 3 dXi               (from 3)
--     Bi = dYi/dXi - (Ci+1 + 2 Ci) dXi / 3   (from 5)
--
-- Numeric example:
--                               System:   C1 = 0   B1 =-1-(1.5)/3 =-1.5
--  X1 =-1  Y1 = 1  dY1/dX1 =-1  4 C2 = 6  C2 = 1.5 B2 = 1- 3/3 = 0
--  X2 = 0  Y2 = 0  dY2/dX2 = 1            C3 = 0   D1 = 1.5/3 = 0.5
--  X3 = 1  Y3 = 1                                  D2 =-1.5/3 =-0.5
--
   type Spline_Coefficients is record
      A, B, C, D  : Number;
   end record;

   package Spline_Coefficient_Maps is
      new Generic_Map (Number, Spline_Coefficients);
   use Spline_Coefficient_Maps;

   type Cubic_Spline is new Abstract_Pairs_Container with record
      Intervals : Map;
   end record;

   function Acceleration
            (  Offset       : Number;
               Coefficients : Spline_Coefficients
            )  return Number;
   function Value
            (  Offset       : Number;
               Coefficients : Spline_Coefficients
            )  return Number;
   function Velocity
            (  Offset       : Number;
               Coefficients : Spline_Coefficients
            )  return Number;

   pragma Inline (Acceleration);
   pragma Inline (Get);
   pragma Inline (Get_Size);
   pragma Inline (Value);
   pragma Inline (Velocity);

end Generic_Cubic_Spline;
