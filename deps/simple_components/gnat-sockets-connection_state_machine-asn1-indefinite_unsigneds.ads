--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Indefinite_Unsigned                    Spring, 2019       --
--  Interface                                                         --
--                                Last revision :  13:37 03 Aug 2019  --
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
--  ASN.1 encoding of indefinite unsigned integers
--
package GNAT.Sockets.Connection_State_Machine.ASN1.
        Indefinite_Unsigneds is
--
-- Indefinite_Unsigned_Data_Item -- ASN.1  encoded   unsigned   integer.
--                                  The actual  value  is  allocated in
-- the external string buffer.
--
   type Implicit_Indefinite_Unsigned_Data_Item is
      new ASN1_Data_Item with
   record
      Buffer : External_String_Buffer_Ptr;
      Start  : Positive := 1;
      Length : Natural  := 0; -- The actual length of the value
   end record;
   type Implicit_Indefinite_Unsigned_Data_Item_Ptr is
      access all Implicit_Indefinite_Unsigned_Data_Item'Class;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Implicit_Indefinite_Unsigned_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Enumerate -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Indefinite_Unsigned_Data_Item
             );
   for Implicit_Indefinite_Unsigned_Data_Item'Write use Enumerate;
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Implicit_Indefinite_Unsigned_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get_ASN1_Type -- Overrides ...Connection_State_Machine.ASN1...
--
   function Get_ASN1_Type
            (  Item : Implicit_Indefinite_Unsigned_Data_Item
            )  return ASN1_Type;
--
-- Get_Buffer -- Get the buffer used by the string data item
--
--    Item - The item
--
-- Returns :
--
--    A pointer to the buffer
--
   function Get_Buffer
            (  Item : Implicit_Indefinite_Unsigned_Data_Item
            )  return External_String_Buffer_Ptr;
--
-- Get_Length -- The current value length in bytes
--
--    Item - The item
--
-- Returns :
--
--    The current value length
--
   function Get_Length
            (  Item : Implicit_Indefinite_Unsigned_Data_Item
            )  return Natural;
--
-- Get_Value -- The current value in the form of a stream arrary
--
--    Item - The item
--
-- Returns :
--
--    The big-endian representation of the value
--
   function Get_Value
            (  Item : Implicit_Indefinite_Unsigned_Data_Item
            )  return Stream_Element_Array;
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit (Item : Implicit_Indefinite_Unsigned_Data_Item)
      return Boolean;
--
-- Set_Implicit_Tag -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Indefinite_Unsigned_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );
--
-- Set_Value -- Set value of the data object
--
--    Item  - The object
--    Value - The value to set in the form of a big-endian array
--
-- Exceptions :
--
--    Storage_Error - The value is too large to store
--    Use_Error     - The item was not initialized
--
   procedure Set_Value
             (  Item  : in out Implicit_Indefinite_Unsigned_Data_Item;
                Value : Stream_Element_Array
             );
------------------------------------------------------------------------
--
-- Indefinite_Unsigned_Data_Item -- Explicit encoding form
--
   type Indefinite_Unsigned_Data_Item is
      new Implicit_Indefinite_Unsigned_Data_Item with null record;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Indefinite_Unsigned_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Indefinite_Unsigned_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit (Item : Indefinite_Unsigned_Data_Item)
      return Boolean;
------------------------------------------------------------------------
--
-- Get -- From stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Value   - The result (big-endian encoded)
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
                Value   : out Stream_Element_Array
             );
--
-- Put -- Into stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to write
--    Value   - The value to encode
--
-- The parameter Pointer  is advanced beyond the value output.  The data
-- type is not output
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds
--
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Stream_Element_Array
             );
private
   function Self
            (  Item : Implicit_Indefinite_Unsigned_Data_Item'Class
            )  return Implicit_Indefinite_Unsigned_Data_Item_Ptr;

end GNAT.Sockets.Connection_State_Machine.ASN1.Indefinite_Unsigneds;
