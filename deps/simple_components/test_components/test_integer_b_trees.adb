--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Integer_B_Trees                        Luebeck            --
--  Implementation                                 Summer, 2022       --
--                                                                    --
--                                Last revision :  18:00 18 Aug 2022  --
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

with Ada.Text_IO;          use Ada.Text_IO;
with Strings_Edit.Floats;  use Strings_Edit.Floats;

package body Test_Integer_B_Trees is

   procedure Complete
             (  State    : in out Indicator;
                Progress : Persistent.Memory_Pools.Streams.Advance
             )  is
   begin
      Put_Line (Image (Progress * 100.0, AbsSmall => -2));
   end Complete;

end Test_Integer_B_Trees;
