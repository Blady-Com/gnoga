--                                                                    --
--  package GNAT.Sockets.           Copyright (c)  Dmitry A. Kazakov  --
--     Connection_State_Machine.                   Luebeck            --
--     Generic_Key                                 Winter, 2013       --
--  Interface                                                         --
--                                Last revision :  13:09 10 Mar 2013  --
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

with Tables;

generic
   with package Keys is new Tables (<>);
package GNAT.Sockets.Connection_State_Machine.Generic_Key is
   use Keys;
--
-- Key_Item -- Terminated keyed string
--
--    Dictionary - The table of keys
--    Terminator - String terminator, e.g. NUL
--
   type Key_Item
        (  Dictionary : access Table'Class;
           Terminator : Character
        )  is new Data_Item with
   record
      Offset : Natural := 0;
   end record;
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Key_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get_Key -- Get matched key
--
--    Item - Data item
--
-- Returns :
--
--    Matched item
--
-- Exception :
--
--    End_Error - No key matched
--
   function Get_Key (Item : Key_Item) return Tag;
--
-- Get -- String from stream element array
--
--    Data       - The stream element array
--    Pointer    - The first element to read
--    Dictionary - The table
--    Terminator - The string terminator
--    Key        - The key value
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    End_Error    - Not enough data (no terminator)
--    Layout_Error - Pointer is outside bounds
--
   procedure Get
             (  Data       : Stream_Element_Array;
                Pointer    : access Stream_Element_Offset;
                Dictionary : Table'Class;
                Terminator : Character;
                Key        : out Tag
             );
--
-- Put -- String into stream element array
--
--    Data       - The stream element array
--    Pointer    - The first element to write
--    Dictionary - The table
--    Terminator - The string terminator
--    Key        - The value to encode
--
-- The parameter Pointer is advanced beyond the value output
--
-- Exceptions :
--
--    Constraint_Error - Key not in Dictionaty
--    Data_Error       - The string contains the terminator
--    End_Error        - No room for output
--    Layout_Error     - Pointer is outside bounds
--
   procedure Put
             (  Data       : in out Stream_Element_Array;
                Pointer    : in out Stream_Element_Offset;
                Dictionary : Table'Class;
                Terminator : Character;
                Key        : Tag
             );
private
   pragma Assert (Stream_Element'Size = 8);

end GNAT.Sockets.Connection_State_Machine.Generic_Key;
