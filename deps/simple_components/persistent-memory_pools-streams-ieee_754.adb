--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Memory_Pools.Streams.            Luebeck            --
--        IEEE_754                                 Summer, 2022       --
--  Implementation                                                         --
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
--
with IEEE_754.Long_Floats;   use IEEE_754.Long_Floats;

with Ada.Unchecked_Conversion;

package body Persistent.Memory_Pools.Streams.IEEE_754 is

   function From_Byte_Index is
      new Ada.Unchecked_Conversion (Byte_Index, Float_64);
   function To_Byte_Index is
      new Ada.Unchecked_Conversion (Float_64, Byte_Index);

   function From_X (Value : Long_Float) return Byte_Index is
   begin
      return To_Byte_Index (To_IEEE (Value));
   end From_X;

   function From_Y (Value : Long_Float) return Byte_Index is
   begin
      return To_Byte_Index (To_IEEE (Value));
   end From_Y;

   function To_X (Value : Byte_Index) return Long_Float is
   begin
      return From_IEEE (From_Byte_Index (Value));
   end To_X;

   function To_Y (Value : Byte_Index) return Long_Float is
   begin
      return From_IEEE (From_Byte_Index (Value));
   end To_Y;

end Persistent.Memory_Pools.Streams.IEEE_754;
