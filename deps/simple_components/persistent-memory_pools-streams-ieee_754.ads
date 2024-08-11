--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Memory_Pools.Streams.            Luebeck            --
--        IEEE_754                                 Summer, 2022       --
--  Interface                                                         --
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
--  IEEE 754 integers for persistent storage
--
package Persistent.Memory_Pools.Streams.IEEE_754 is

   function From_X (Value : Long_Float) return Byte_Index;
   function From_Y (Value : Long_Float) return Byte_Index;

   function To_X   (Value : Byte_Index) return Long_Float;
   function To_Y   (Value : Byte_Index) return Long_Float;

private
   pragma Inline (From_X);
   pragma Inline (From_Y);
   pragma Inline (To_X);
   pragma Inline (To_Y);

end Persistent.Memory_Pools.Streams.IEEE_754;
