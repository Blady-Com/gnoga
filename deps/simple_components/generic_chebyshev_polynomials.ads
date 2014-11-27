--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Chebyshev_Polynomials               Luebeck            --
--  Interface                                      Spring, 2009       --
--                                                                    --
--                                Last revision :  15:03 28 Mar 2009  --
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
--  This package provides means to sum Chebyshev series:
--
--     Sum_T         - First kind Chebyshev series Tn
--     Sum_Even_T    - Even first kind Chebyshev series T2n
--     Sum_Odd_T     - Odd first kind Chebyshev series T2n+1
--     Sum_Shifted_T - Shifted first kind Chebyshev series T*n
--
generic
   type Number is digits <>;
package Generic_Chebyshev_Polynomials is
--
-- Coefficients -- Of a Chebyshev series
--
   type Coefficients is array (Natural range <>) of Number'Base;
--
-- Sum_T -- Sum of Chebyshev series Tn
--
--    X - The argument
--    A - The coefficients
--
-- Returns :
--
--    Sum An Tn(X)
--    n=0..N
--
   function Sum_T (X : Number'Base; A : Coefficients)
      return Number'Base;
--
-- Sum_Even_T -- Sum of Chebyshev series T2n
--
--    X - The argument
--    A - The coefficients
--
-- Returns :
--
--    Sum An T2n(X)
--    n=0..N
--
   function Sum_Even_T (X : Number'Base; A : Coefficients)
      return Number'Base;
--
-- T_Odd -- Sum of Chebyshev series T2n+1
--
--    X - The argument
--    A - The coefficients
--
-- Returns :
--
--    Sum An T2n+1(X)
--    n=0..N
--
   function Sum_Odd_T (X : Number'Base; A : Coefficients)
      return Number'Base;
--
-- T_Shifted -- Sum of shifted Chebyshev series T*2n
--
--    X - The argument
--    C - The coefficients
--
-- Returns :
--
--    Sum An T*n(X)
--    n=0..N
--
   function Sum_Shifted_T (X : Number'Base; A : Coefficients)
      return Number'Base;

end Generic_Chebyshev_Polynomials;
