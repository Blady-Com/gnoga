--                                                                    --
--  function                        Copyright (c)  Dmitry A. Kazakov  --
--     Gamma                                       Luebeck            --
--  Instantiation                                  Spring, 2009       --
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

with Ada.Numerics.Long_Elementary_Functions;
with Long_Float_Chebyshev_Polynomials;
--
-- The implementation of Gamma function uses
--
--  ]-oo, 0[  Euler's reflection formula and Gamma(-x)
--    [0, 1[  Shifted first kind Chebyshev series of Gamma(x)/x
--    [1, 2]  Shifted first kind Chebyshev series of Gamma(x+1)
--    ]2, oo[ Shifted first kind Chebyshev series of ln Gamma (x)
--
-- Source: Mathematical functions and their approximations  by Yudell L.
-- Luke. 1975
--
function Gamma (X : Float) return Float is
   use Ada.Numerics;
   use Long_Elementary_Functions;
   use Long_Float_Chebyshev_Polynomials;

   Ln_2Pi_2 : constant :=                            -- ln 2Pi / 2
              (  (  1.1447_29885_84940_01741_43427   -- ln Pi
                 +  0.6931_47180_55994_53094_37232_1 -- ln 2
                 )
              /  2.0
              );
   C1 : constant Coefficients :=   -- gamma(x+1), T*n(x), 0 <= x <= 1
           (  0.94178_55977_95494_66571,  0.00441_53813_24841_00676,
              0.05685_04368_15993_63379, -0.00421_98353_96418_56050,
              0.00132_68081_81212_46022, -0.00018_93024_52979_88804,
              0.00003_60692_53274_41245, -0.00000_60567_61904_46086,
              0.00000_10558_29546_30228, -0.00000_01811_96736_55424,
              0.00000_00311_77249_64715, -0.00000_00053_54219_63902,
              0.00000_00009_19327_55199, -0.00000_00001_57794_12803,
              0.00000_00000_27079_80623, -0.00000_00000_04646_81865,
              0.00000_00000_00797_33502, -0.00000_00000_00136_80782,
              0.00000_00000_00023_47319, -0.00000_00000_00004_02743,
              0.00000_00000_00000_69101, -0.00000_00000_00000_11856,
              0.00000_00000_00000_02034  -0.00000_00000_00000_00349,
              0.00000_00000_00000_00060, -0.00000_00000_00000_00010,
              0.00000_00000_00000_00002
           );
   C2 : constant Coefficients := -- ln gamma(x), T2n, x > 1
           (  0.98575_15540_05098, -0.01357_51199_40355,
              0.00060_97577_84871, -0.00005_47947_47404,
              0.00000_72256_71298, -0.00000_12227_85636,
              0.00000_02472_06061, -0.00000_00571_76475,
              0.00000_00147_06251, -0.00000_00041_24183,
              0.00000_00012_43043, -0.00000_00003_98333,
              0.00000_00001_34579, -0.00000_00000_47620,
              0.00000_00000_17553, -0.00000_00000_06710,
              0.00000_00000_02651, -0.00000_00000_01079,
              0.00000_00000_00451, -0.00000_00000_00193,
              0.00000_00000_00085, -0.00000_00000_00040,
              0.00000_00000_00017, -0.00000_00000_00008,
              0.00000_00000_00004, -0.00000_00000_00002,
              0.00000_00000_00001
           );
   function Gamma (X : Long_Float) return Long_Float is
      pragma Inline (Gamma);
   begin
      if X <= 1.0 then
         return Sum_Shifted_T (X, C1) / X;
      elsif X <= 2.0 then
         return Sum_Shifted_T (X - 1.0, C1);
      else
         return
         (  Exp
            (  (X - 0.5) * Log (X)
            -  X
            +  Ln_2Pi_2
            +  Sum_Even_T (1.0 / X, C2) / (12.0 * X)
         )  );
      end if;
   end Gamma;
begin
   if X < 0.0 then
      return
         Float  -- Euler's reflection formula
         (  Pi
         /  (sin (Pi * Long_Float (X)) * Gamma (1.0 - Long_Float (X)))
         );
   else
      return Float (Gamma (Long_Float (X)));
   end if;
end Gamma;
