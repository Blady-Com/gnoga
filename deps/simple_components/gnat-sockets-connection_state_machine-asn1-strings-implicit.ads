--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Strings.Implicit                       Spring, 2019       --
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

package GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit is
--
-- Implicit_String_Data_Item -- ASN.1 encoded string. The string accepts
--                              any string type recoding it into UTF-8
--
   type Implicit_String_Data_Item is
      new Public_String_Data_Item with private;
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit (Item : Implicit_String_Data_Item)
      return Boolean;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Implicit_String_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Implicit_String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
------------------------------------------------------------------------
-- Implicit_External_String_Data_Item -- ASN.1   encoded   string.   The
--                                       string   accepts   any   string
--                                       type   recoding  it into UTF-8.
-- The objects of  these  types can share  the same  buffer to store the
-- string contents.  E.g.  if an ASN.1 choice  consists  of  many string
-- alternatives it might be reasonable to use single buffer for them all
-- since only one alternative is realized. Other cases is to specify the
-- upper memory limit for all strings in a packet.
--
   type Implicit_External_String_Data_Item is
      new ASN1_Data_Item with private;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Implicit_External_String_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Implicit_External_String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get_ASN1_Type -- Overrides ...Connection_State_Machine.ASN1...
--
   function Get_ASN1_Type
            (  Item : Implicit_External_String_Data_Item
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
            (  Item : Implicit_External_String_Data_Item
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
            (  Item : Implicit_External_String_Data_Item
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
            (  Item : Implicit_External_String_Data_Item
            )  return String;
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit (Item : Implicit_External_String_Data_Item)
      return Boolean;
--
-- Set_Value -- Set the string value
--
--    Item  - The item
--    Value - The new value to set
--
-- Exceptions :
--
--    Storage_Error - The value is too large to store
--    Use_Error     - The object was not initialized
--
   procedure Set_Value
             (  Item  : in out Implicit_External_String_Data_Item;
                Value : String
             );
------------------------------------------------------------------------
   function Get_Data
            (  Item : Implicit_String_Data_Item
            )  return String_Data_Ptr;
   function Get_Data
            (  Item : Implicit_External_String_Data_Item
            )  return String_Data_Ptr;
private
   type Implicit_String_Data_Item is
      new Public_String_Data_Item with
   record
      Driver : aliased Embedded_String_Data
                       (  Implicit_String_Data_Item'Access
                       );
   end record;
--
-- Set_Implicit_Tag -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_String_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );

   type Implicit_String_Data_Item_Ptr is
      access all Implicit_String_Data_Item'Class;
   function Self
            (  Item : Implicit_String_Data_Item'Class
            )  return Implicit_String_Data_Item_Ptr;
------------------------------------------------------------------------
   type Implicit_External_String_Data_Item is
      new ASN1_Data_Item with
   record
      Driver : aliased External_String_Data;
   end record;
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_External_String_Data_Item
             );
   for Implicit_External_String_Data_Item'Write use Enumerate;
--
-- Set_Implicit_Tag -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_External_String_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );

   type Implicit_External_String_Data_Item_Ptr is
      access all Implicit_External_String_Data_Item'Class;
   function Self
            (  Item : Implicit_External_String_Data_Item'Class
            )  return Implicit_External_String_Data_Item_Ptr;

end GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit;
