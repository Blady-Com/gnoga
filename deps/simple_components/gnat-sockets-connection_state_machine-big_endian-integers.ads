--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Connection_State_Machine.                   Luebeck            --
--     Big_Endian.Integers                         Winter, 2012       --
--  Interface                                                         --
--                                Last revision :  12:18 18 May 2019  --
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

with GNAT.Sockets.Connection_State_Machine.Big_Endian.Unsigneds;

package GNAT.Sockets.Connection_State_Machine.Big_Endian.Integers is
   use Interfaces;
--
-- Integer_{8|16|32|64}_Data_Item -- Big endian-encoded Integer number
--
   type Integer_8_Data_Item  is new Data_Item with private;
   type Integer_16_Data_Item is new Data_Item with private;
   type Integer_32_Data_Item is new Data_Item with private;
   type Integer_64_Data_Item is new Data_Item with private;
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Integer_8_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Feed
             (  Item    : in out Integer_16_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Feed
             (  Item    : in out Integer_32_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Feed
             (  Item    : in out Integer_64_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get -- Number from stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Value   - The result
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--
   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Integer_8
             );
   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Integer_16
             );
   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Integer_32
             );
   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Integer_64
             );
--
-- Put -- Number into stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to write
--    Value   - The value to encode
--
-- The parameter Pointer is advanced beyond the value output
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds
--
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Integer_8
             );
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Integer_16
             );
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Integer_32
             );
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Integer_64
             );
--
-- Value -- Content of an item
--
--    Item - The string data item
--
-- Returns :
--
--    The value contained by Item
--
   function Value (Item : Integer_8_Data_Item ) return Integer_8;
   function Value (Item : Integer_16_Data_Item) return Integer_16;
   function Value (Item : Integer_32_Data_Item) return Integer_32;
   function Value (Item : Integer_64_Data_Item) return Integer_64;

private
   use Connection_State_Machine.Big_Endian.Unsigneds;
   pragma Inline (Value);

   type Integer_8_Data_Item is
      new Unsigned_8_Data_Item with null record;
   type Integer_16_Data_Item is
      new Unsigned_16_Data_Item with null record;
   type Integer_32_Data_Item is
      new Unsigned_32_Data_Item with null record;
   type Integer_64_Data_Item is
      new Unsigned_64_Data_Item with null record;

end GNAT.Sockets.Connection_State_Machine.Big_Endian.Integers;
