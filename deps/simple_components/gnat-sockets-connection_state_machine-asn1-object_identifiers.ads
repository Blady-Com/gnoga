--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Object_Identifiers                             Spring, 2019       --
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

with Strings_Edit.Object_Identifiers;
use  Strings_Edit.Object_Identifiers;

package GNAT.Sockets.Connection_State_Machine.ASN1.Object_Identifiers is
--
-- Implicit_OID_Data_Item -- ASN.1 encoded OID
--
   type Implicit_OID_Data_Item (Size : Positive) is
      new ASN1_Data_Item with
   record
      Length : Natural := 0;
      Value  : Object_Identifier (1..Size);
   end record;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Implicit_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Implicit_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get_ASN1_Type -- Overrides ...Connection_State_Machine.ASN1...
--
   function Get_ASN1_Type
            (  Item : Implicit_OID_Data_Item
            )  return ASN1_Type;
--
-- Get_Value -- Get value of the data object
--
--    Item - The object
--
-- Returns :
--
--    The current value
--
   function Get_Value
            (  Item : Implicit_OID_Data_Item
            )  return Object_Identifier;
--
-- Set_Implicit_Tag -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_OID_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );
--
-- Set_Value -- Set value of the data object
--
--    Item  - The object
--    Value - The value to set
--
-- Exceptions :
--
--    Constraint_Error - The identifier is too large
--
   procedure Set_Value
             (  Item  : in out Implicit_OID_Data_Item;
                Value : Object_Identifier
             );
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit (Item : Implicit_OID_Data_Item)
      return Boolean;
--
-- OID_Data_Item -- Explicit form of encoding
--
   type OID_Data_Item is new Implicit_OID_Data_Item with null record;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit
            (  Item : OID_Data_Item
            )  return Boolean;
------------------------------------------------------------------------
--
-- Implicit_Relative_OID_Data_Item -- ASN.1 encoded relative OID
--
   type Implicit_Relative_OID_Data_Item is
      new Implicit_OID_Data_Item with null record;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Implicit_Relative_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Implicit_Relative_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get_ASN1_Type -- Overrides ...Connection_State_Machine.ASN1...
--
   function Get_ASN1_Type
            (  Item : Implicit_Relative_OID_Data_Item
            )  return ASN1_Type;
--
-- Relative_OID_Data_Item -- Explicit form of encoding
--
   type Relative_OID_Data_Item is
      new Implicit_Relative_OID_Data_Item
         with null record;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Relative_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Relative_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit
            (  Item : Relative_OID_Data_Item
            )  return Boolean;
------------------------------------------------------------------------
--
-- Implicit_External_OID_Data_Item -- ASN.1 object identifier
--
-- The objects of  these  types can share  the same  buffer to store the
-- bit string contents. E.g. if an ASN.1 choice consists of  many string
-- alternatives it might be reasonable to use single buffer for them all
-- since only one alternative is realized. Other cases is to specify the
-- upper memory limit for all strings in a packet.
--
   type Subindentifier_Ptr is access all Subindentifier_Type;
   type Implicit_External_OID_Data_Item is
      new ASN1_Data_Item with
   record
      Buffer : External_String_Buffer_Ptr;
      Length : Natural;
      Value  : Subindentifier_Ptr;
   end record;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Implicit_External_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Enumerate -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_External_OID_Data_Item
             );
   for Implicit_External_OID_Data_Item'Write use Enumerate;
--
-- Expand -- Expand the identifier
--
--    Item - The object identifier
--
-- The identifier must be on top of the arena pool
--
   procedure Expand (Item : in out Implicit_External_OID_Data_Item);
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Implicit_External_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get_ASN1_Type -- Overrides ...Connection_State_Machine.ASN1...
--
   function Get_ASN1_Type
            (  Item : Implicit_External_OID_Data_Item
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
            (  Item : Implicit_External_OID_Data_Item
            )  return External_String_Buffer_Ptr;
--
-- Get_Length -- The current string value length
--
--    Item - The item
--
-- Returns :
--
--    The current value length
--
   function Get_Length
            (  Item : Implicit_External_OID_Data_Item
            )  return Natural;
--
-- Get_Value -- The current string value
--
--    Item - The item
--
-- Returns :
--
--    The current value
--
   function Get_Value
            (  Item : Implicit_External_OID_Data_Item
            )  return Object_Identifier;
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit
            (  Item : Implicit_External_OID_Data_Item
            )  return Boolean;
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_External_OID_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );
--
-- Set_Value -- Set the string value
--
--    Item  - The item
--    Value - The new value to set
--
-- Exceptions :
--
--    Storage_Error - The value is too large to store
--    Use_Error     - The item was not initialized
--
   procedure Set_Value
             (  Item  : in out Implicit_External_OID_Data_Item;
                Value : Object_Identifier
             );
--
-- External_OID_Data_Item -- Explicit encoding
--
   type External_OID_Data_Item is
      new Implicit_External_OID_Data_Item with null record;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : External_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out External_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit
            (  Item : External_OID_Data_Item
            )  return Boolean;
------------------------------------------------------------------------
--
-- Implicit_External_Relative_OID_Data_Item -- ASN.1  encoded   relative
--                                             OID
--
   type Implicit_External_Relative_OID_Data_Item is
      new Implicit_External_OID_Data_Item with null record;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Implicit_External_Relative_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out
                          Implicit_External_Relative_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- External_Relative_OID_Data_Item -- Explicit encoding form
--
   type External_Relative_OID_Data_Item is
      new Implicit_External_Relative_OID_Data_Item
         with null record;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : External_Relative_OID_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out External_Relative_OID_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit
            (  Item : External_Relative_OID_Data_Item
            )  return Boolean;
------------------------------------------------------------------------
--
-- Get -- ASN.1 object identifier from stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--    Value   - The result
--    Last    - The subidentifier stored
--
-- Note that the data type is  not included.  The parameter  Pointer  is
-- advanced beyond the value obtained.  Value (Value'First..Last) is the
-- result
--
-- Exceptions :
--
--    Constraint_Error - The value is too large too store
--    End_Error        - Not enough data
--    Layout_Error     - Pointer is outside bounds
--
   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Object_Identifier;
                Last    : out Integer
             );
--
-- Get -- ASN.1 object identifier from stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--
   function Get
            (  Data    : Stream_Element_Array;
               Pointer : access Stream_Element_Offset;
               Length  : Stream_Element_Offset
            )  return Object_Identifier;
--
-- Get_Relative -- ASN.1 object identifier from stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--    Value   - The result
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    Constraint_Error - The value is too large too store
--    End_Error        - Not enough data
--    Layout_Error     - Pointer is outside bounds
--
   procedure Get_Relative
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Object_Identifier;
                Last    : out Integer
             );
--
-- Get_Relative -- ASN.1 object identifier from stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--
   function Get_Relative
            (  Data    : Stream_Element_Array;
               Pointer : access Stream_Element_Offset;
               Length  : Stream_Element_Offset
            )  return Object_Identifier;
--
-- Put -- ASN.1 object identifier into stream element array
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
                Value   : Object_Identifier
             );
--
-- Put_Relative -- ASN.1 object identifier into stream element array
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
   procedure Put_Relative
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Object_Identifier
             );
private
   type Implicit_External_OID_Data_Item_Ptr is
      access all Implicit_External_OID_Data_Item'Class;
   function Self
            (  Item : Implicit_External_OID_Data_Item'Class
            )  return Implicit_External_OID_Data_Item_Ptr;

end GNAT.Sockets.Connection_State_Machine.ASN1.Object_Identifiers;

