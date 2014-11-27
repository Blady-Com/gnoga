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

package body Generic_Chebyshev_Polynomials is

   function Sum_T (X : Number'Base; A : Coefficients)
      return Number'Base is
      AN : constant Number'Base := 2.0 * X;
      B0 : Number'Base := 0.0;
      B1 : Number'Base := 0.0;
      BN : Number'Base;
   begin
      for N in reverse A'Range loop
         BN := AN * B0 - B1 + A (N);
         B1 := B0;
         B0 := BN;
      end loop;
      return B0 - X * B1;
   end Sum_T;

   function Sum_Even_T (X : Number'Base; A : Coefficients)
      return Number'Base is
      AN : constant Number'Base := 2.0 * (2.0 * X**2 - 1.0);
      B0 : Number'Base := 0.0;
      B1 : Number'Base := 0.0;
      BN : Number'Base;
   begin
      for N in reverse A'Range loop
         BN := AN * B0 - B1 + A (N);
         B1 := B0;
         B0 := BN;
      end loop;
      return B0 - (2.0 * X**2 - 1.0) * B1;
   end Sum_Even_T;

   function Sum_Odd_T (X : Number'Base; A : Coefficients)
      return Number'Base is
      AN : constant Number'Base := 2.0 * (2.0 * X**2 - 1.0);
      B0 : Number'Base := 0.0;
      B1 : Number'Base := 0.0;
      BN : Number'Base;
   begin
      for N in reverse A'Range loop
         BN := AN * B0 - B1 + A (N);
         B1 := B0;
         B0 := BN;
      end loop;
      return X * (B0 - B1);
   end Sum_Odd_T;

   function Sum_Shifted_T (X : Number'Base; A : Coefficients)
      return Number'Base is
      AN : constant Number'Base := 2.0 * (2.0 * X - 1.0);
      B0 : Number'Base := 0.0;
      B1 : Number'Base := 0.0;
      BN : Number'Base;
   begin
      for N in reverse A'Range loop
         BN := AN * B0 - B1 + A (N);
         B1 := B0;
         B0 := BN;
      end loop;
      return B0 - (2.0 * X - 1.0) * B1;
   end Sum_Shifted_T;

end Generic_Chebyshev_Polynomials;
