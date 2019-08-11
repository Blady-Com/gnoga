--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Choices                                Summer, 2019       --
--  Interface                                                         --
--                                Last revision :  18:41 01 Aug 2019  --
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
--  ASN.1 encoding of choices
--
with Generic_Map;

with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;

package GNAT.Sockets.Connection_State_Machine.ASN1.Choices is
--
-- Choice_Data_Item -- ASN.1  encoded  choice.  A  choice  contains  any
--                     number of alternatives, other ASN.1 objects.  The
-- input determines the  alternative  by its tag.  The  output uses  the
-- selected  alternative for  encoding.  The choice object  has no ASN.1
-- type.  The data type  implements an empty choice.  A non-empty choice
-- is  declared  by deriving  a  new  type from  it  and  placing  items
-- derived from ASN1_Data_Item into it:
--
--    type Alternatives_Record is new Choice_Data_Item with record
--       Name : Implicit_String_Data_Item (20);
--       ID   : Implicit_Integer_Data_Item;
--    end record;
--
-- This is equivalent to ASN.1
--
--    CHOICE
--    {  Name [0] IMPLICIT OCTET STRING,
--       ID   [1] IMPLICIT INTEGER
--    }
--
-- Note that  universally  tagged  alternatives  in  the choice  must be
-- implicit because the universal tag already contains the object type.
--
   type Choice_Data_Item is new Abstract_ASN1_Data_Item with private;
   type Choice_Data_Item_Ptr is access all Choice_Data_Item'Class;
--
-- Always_Constructed -- Overrides ...Connection_State_Machine.ASN1...
--
   function Always_Constructed
            (  Item : Choice_Data_Item
            )  return Boolean;
--
-- Enable_Unsolicited -- Enable or disable ignoring unsolicited tags
--
--    Item   - The ASN.1 choice object
--    Enable - If objects with an unsolicited tag must be skipped
--
-- By default when an unknown tag appears, it is treated as an error. If
-- Enable is set to True,  the tagged object is skipped and Get_Selected
-- will return 0.
--
-- Exceptions :
--
--    Use_Error - The choice was not initialized yet
--
   procedure Enable_Unsolicited
             (  Item   : in out Choice_Data_Item;
                Enable : Boolean
             );
--
-- Enumerate -- Fake stream I/O procedure
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Choice_Data_Item
             );
   for Choice_Data_Item'Write use Enumerate;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Choice_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Choice_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Finalize -- Destruction
--
--    Item - Being finalized
--
-- To be called the from derived type's Finalize if overridden
--
   procedure Finalize
             (  Item : in out Choice_Data_Item
             );
--
-- Get_Children -- Overrides ...Connection_State_Machine...
--
   function Get_Children
            (  Item : Choice_Data_Item
            )  return Data_Item_Ptr_Array;
--
-- Get_Length -- The number of alternatives
--
--    Item - The ASN.1 choice object
--
-- Returns :
--
--    The number of alternative elements
--
-- Exceptions :
--
--    Use_Error - The choice was not initialized yet
--
   function Get_Length
            (  Item : Choice_Data_Item
            )  return Natural;
--
-- Get_Selected -- Get selected choice alternative
--
--    Item - The choice
--
-- Returns :
--
--    The index of or pointer to the alternative or 0 if none selected
--
-- Exceptions :
--
--    Use_Error - The choice was not initialized yet
--
   function Get_Selected
            (  Item  : Choice_Data_Item
            )  return Natural;
   function Get_Selected
            (  Item  : Choice_Data_Item
            )  return Abstract_ASN1_Data_Item_Ptr;
--
-- Get_Size -- Overrides ...Connection_State_Machine...
--
   function Get_Size
            (  Item : Choice_Data_Item
            )  return Natural;
--
-- Get_Tag -- Get tag of a choice element
--
--    Item  - The Choice
--    Index - The number of the choice element 1..Get_Length (Item)
--
-- Returns :
--
--    The element's tag
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--    Use_Error        - The choice was not initialized yet
--
   function Get_Tag
            (  Item  : Choice_Data_Item;
               Index : Positive
            )  return Tag_Type;
--
-- Initialized -- Notification of object initialization
--
--    Item - The object
--
-- This procedure is called when all components of  the object have been
-- enumerated,  so that some additional initialization could be finished
-- with all components known. The default implementation does nothing.
--
   procedure Initialized (Item : in out Choice_Data_Item);
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit
            (  Item : Choice_Data_Item
            )  return Boolean;
--
-- Is_Unsolicited_Enabled -- If unsolicited tags are enabled
--
--    Item - The ASN.1 choice object
--
-- Returns :
--
--    True if choices of objects tagged by unknown tags are ignored
--
-- Exceptions :
--
--    Use_Error - The choice was not initialized yet
--
   function Is_Unsolicited_Enabled
            (  Item : Choice_Data_Item
            )  return Boolean;
--
-- Set_Implicit_Tag -- Overrides ...Connection_State_Machine.ASN1...
--
-- This procedure  select  the item  using  the specified tag. S ee also
-- Set_Selected which does it rather by the alternative index.
--
   procedure Set_Implicit_Tag
             (  Item   : in out Choice_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );
--
-- Set_Selected -- Set selected choice alternative
--
--    Item  - The choice
--    Index - The choice item to select 1..Get_Length
--
-- Exceptions :
--
--    Constraint_Error - Selectec is out of range
--    Use_Error        - The choice was not initialized yet
--
   procedure Set_Selected
             (  Item  : in out Choice_Data_Item;
                Index : Positive
             );
--
-- Set_Tag -- Set tag of a choice alternative
--
--    Item  - The choice
--    Index - The number of the choice element 1..Get_Length (Item)
--    Tag   - The tag to assign
--
-- This procedure changes the alternative's tag. By default the tags are
-- assigned  context-specific  in ascending order starting from 0.  When
-- tag is universal the corresponding  object must be implicit otherwise
-- Mode_Error is propagated. It is also propagated when the tag value of
-- universal tag differs  from the object's ASN.1  type or  when the tag
-- does  not indicate optional choice  (all choice alternatives  must be
-- optional).
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range or the tag is in use
--    Mode_Error       - Wrongly tagged object
--    Use_Error        - The choice was not initialized yet
--
   procedure Set_Tag
             (  Item  : in out Choice_Data_Item;
                Index : Positive;
                Tag   : Tag_Type
             );
------------------------------------------------------------------------
--
-- Resolve_Selected -- Resolve the ultimate object
--
--    Item - The object
--
-- When  the object  is  choice  the function  recursively  returns  the
-- selected item.
--
-- Returns :
--
--    The selected object
--
   function Resolve_Selected
            (  Item : Abstract_ASN1_Data_Item'Class
            )  return Abstract_ASN1_Data_Item_Ptr;

private
   type Tag_Type_Array is array (Positive range <>) of Tag_Type;
   type Tag_Type_Array_Ptr is access Tag_Type_Array;
   type Element_Descriptor is record
      Position : Natural := 0;
      Item     : Data_Item_Ptr;
   end record;
   package Element_Maps is
      new Generic_Map (Tag_Type, Element_Descriptor);
   use Element_Maps;

   type Current_Element is new Measured_Data_Item with record
      Position : Natural  := 0;
      Tag      : Tag_Type := (Universal_Tag, 0, False);
   end record;
   type Choice_Data_Item is new Abstract_ASN1_Data_Item with record
      List        : Tag_Type_Array_Ptr;
      Map         : Element_Maps.Map;
      Initialized : Boolean := False;
      Unsolicited : Boolean := False;
      Current     : Current_Element;
--      Length      : Stream_Element_Offset := 0;
   end record;
   procedure Set_Untagged (Item : in out Choice_Data_Item);
--     procedure End_Of_Subsequence
--               (  Item    : in out Choice_Data_Item;
--                  Data    : Stream_Element_Array;
--                  Pointer : Stream_Element_Offset;
--                  Client  : in out State_Machine'Class;
--                  State   : in out Stream_Element_Offset
--               );
   function Self
            (  Item : Choice_Data_Item'Class
            )  return Choice_Data_Item_Ptr;

end GNAT.Sockets.Connection_State_Machine.ASN1.Choices;
