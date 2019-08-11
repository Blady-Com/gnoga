--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Sets.Implicit                          Summer, 2019       --
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
--  ASN.1 encoding of sets
--
with Generic_Map;
with Generic_Unbounded_Array;

package GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Implicit is
--
-- Implicit_Set_Data_Item -- ASN.1  encoded  set.  A  set  contains  any
--                           number  of other  ASN.1  objects  input  in
-- any order.  The objects must be  tagged  to determine the order.  The
-- data type  implements  an empty set.  A non-empty set is  declared by
-- deriving  a  new   type  from  it  and  placing  items  derived  from
-- ASN1_Data_Item into it:
--
--    type Client_Record is new Set_Data_Item with record
--       Name      : String_Data_Item (20);
--       Street    : String_Data_Item (80);
--       Post_Code : String_Data_Item (Numeric_String_Tag, 80);
--    end record;
--
-- This is equivalent to ASN.1
--
--    SET
--    {  Name      [0] OCTET STRING,
--       Street    [1] OCTET STRING,
--       Post_Code [2] NUMERIC STRING
--    }
--
-- The elements can be  marked optional in which case  they  are ignored
-- when missing in the input and are not output by Encode. The same type
-- implementation is used for both ASN.1 SET and SET OF.
--
   type Implicit_Set_Data_Item is
      new ASN1_Tagged_List_Data_Item with private;
--
-- [Explicit_]Feed -- Implementation of the feed operation
--
   procedure Explicit_Feed
             (  Item    : in out Implicit_Set_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Feed
             (  Item    : in out Implicit_Set_Data_Item;
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
             (  Item : in out Implicit_Set_Data_Item
             );
--
-- Initialized -- Notification of object initialization
--
--    Item - The object
--
-- This procedure is called when all components of  the object have been
-- enumerated,  so that some additional initialization could be finished
-- with all components known. The default implementation does nothing.
--
   procedure Initialized (Item : in out Implicit_Set_Data_Item);
--
-- Reset -- Change a set element to unset
--
--    Item  - The set
--    Index - The number of the sequence element 1..Get_Length (Item)
--    Unset - True if the sequence element is not set
--
-- These procedures marks the element set or unset.
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--    Status_Error     - The element is untagged (universal)
--    Use_Error        - The set was not initialized yet
--
   procedure Reset
             (  Item  : in out Implicit_Set_Data_Item;
                Index : Positive;
                Unset : Boolean
             );
--
-- Set_Optional -- Change tag of a set element
--
--    Item     - The set
--    Index    - The number of the set element 1..Get_Length (Item)
--    Optional - True if the set element is optional
--
-- This procedure  changes the element's tag to make  element  optional.
-- The element is marked optional and unset.
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--    Use_Error        - The set was not initialized yet
--
   procedure Set_Optional
             (  Item     : in out Implicit_Set_Data_Item;
                Index    : Positive;
                Optional : Boolean
             );
--
-- Set_Tag -- Set tag of a set element
--
--    Item  - The set
--    Index - The number of the set element 1..Get_Length (Item)
--    Tag   - The tag to assign
--    Unset - Mark unset, e.g. for Encode
--
-- These procedures change  the element's tag.  By default the tags  are
-- assigned  context-specific  in ascending order  starting from 0.  The
-- parameter Unset marks the element unset.  Unset elements  are skipped
-- by Encode. Upon input Feed marks absent optional elements unset. When
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
--    Use_Error        - The set was not initialized yet
--
   procedure Set_Tag
             (  Item  : in out Implicit_Set_Data_Item;
                Index : Positive;
                Tag   : Tag_Type;
                Unset : Boolean := False
             );
------------------------------------------------------------------------
--
-- Implicit_External_Set_Of_Data_Item -- ASN.1   encoded  set.   A   set
--                                       contains  any  number  of other
-- ASN.1 objects  of same type.  The objects  are kept externally  in  a
-- container object. The object itself must contain no items.
--
   type Implicit_External_Set_Of_Data_Item is
      abstract new ASN1_List_Data_Item with private;
--
-- Append -- A new element to the set
--
--    Item - The set
--
-- Exceptions :
--
--    Storage_Error - No room in the external buffer
--    Use_Error     - Uinitialized
--
   procedure Append
             (  Item : in out Implicit_External_Set_Of_Data_Item
             );
--
-- Create -- A new element for the set
--
--    Item - The set
--
-- The derived  type  must implement  this function  in order  to have a
-- specific data type to allocate. The element must  be allocated in the
-- pool obtained by Get_Container.
--
-- Returns :
--
--    A new element for the set
--
-- Exceptions :
--
--    Storage_Error - No room in the external buffer
--
   function Create
            (  Item : access Implicit_External_Set_Of_Data_Item
            )  return Abstract_ASN1_Data_Item_Ptr is abstract;
--
-- [Explicit_]Feed -- Implementation of the feed operation
--
   procedure Explicit_Feed
             (  Item    : in out Implicit_External_Set_Of_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Feed
             (  Item    : in out Implicit_External_Set_Of_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get -- Get a set element
--
--    Item  - The set
--    Index - The number of the set element 1..Get_Length (Item)
--
-- Returns :
--
--    A pointer to the element
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--    Use_Error        - Uinitialized
--
   function Get
            (  Item  : Implicit_External_Set_Of_Data_Item;
               Index : Positive
            )  return Abstract_ASN1_Data_Item_Ptr;
--
-- Get_Container -- Get the container used to store set elements
--
--    Item - The item
--
-- Returns :
--
--    A pointer to the container
--
   function Get_Container
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return External_String_Buffer_Ptr;
--
-- Initialized -- Notification of object initialization
--
--    Item - The object
--
-- This procedure is called when all components of  the object have been
-- enumerated,  so that some additional initialization could be finished
-- with all components known. The default implementation does nothing.
--
   procedure Initialized
             (  Item : in out Implicit_External_Set_Of_Data_Item
             );
--
-- Implementations of primitive operations
--
   function Always_Constructed
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return Boolean;
   procedure Encode
             (  Item    : Implicit_External_Set_Of_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   function Get_ASN1_Type
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return ASN1_Type;
   function Get_Children
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return Data_Item_Ptr_Array;
   function Get_Length
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return Natural;
   function Get_Size
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return Natural;
   function Is_Implicit
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return Boolean;
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_External_Set_Of_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );
private
   type Tag_Type_Array is array (Positive range <>) of Tag_Type;
   type Tag_Type_Array_Ptr is access Tag_Type_Array;
   type Element_Descriptor is record
      Unset    : Boolean := True;
      Position : Positive;
      Item     : Data_Item_Ptr;
   end record;
   package Element_Maps is
      new Generic_Map (Tag_Type, Element_Descriptor);
   use Element_Maps;

   type Current_Element is new Measured_Data_Item with record
      Position : Natural  := 0;
      Tag      : Tag_Type := (Universal_Tag, 0, False);
   end record;
   ---------------------------------------------------------------------
   type ASN1_Data_Item_Ptr_Array is
      array (Positive range <>) of Abstract_ASN1_Data_Item_Ptr;
   package Element_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Positive,
             Object_Type       => Abstract_ASN1_Data_Item_Ptr,
             Object_Array_Type => ASN1_Data_Item_Ptr_Array,
             Null_Element      => null
          );
   type Unbounded_Element_Array is
      new Element_Arrays.Unbounded_Array with
   record
      Length : Natural := 0;
   end record;
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Unbounded_Element_Array
             );
   for Unbounded_Element_Array'Write use Write;

   type Implicit_Set_Data_Item is
      new ASN1_Tagged_List_Data_Item with
   record
      Initialized    : Boolean := False;
      Definite       : Boolean := False;
      Elements_Count : Natural := 0;
      Optional_Count : Natural := 0;
      List           : Tag_Type_Array_Ptr;
      Map            : Element_Maps.Map;
      Total_Length   : Stream_Element_Offset;
      Current        : Current_Element;
   end record;
   type Implicit_Set_Data_Item_Ptr is
      access all Implicit_Set_Data_Item'Class;
   function Always_Constructed
            (  Item : Implicit_Set_Data_Item
            )  return Boolean;
   procedure Encode
             (  Item    : Implicit_Set_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure End_Of_Subsequence
             (  Item    : in out Implicit_Set_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Set_Data_Item
             );
   for Implicit_Set_Data_Item'Write use Enumerate;
   function Get_ASN1_Type
            (  Item : Implicit_Set_Data_Item
            )  return ASN1_Type;
   function Get_Children
            (  Item : Implicit_Set_Data_Item
            )  return Data_Item_Ptr_Array;
   function Get_Length
            (  Item : Implicit_Set_Data_Item
            )  return Natural;
   function Get_Size
            (  Item : Implicit_Set_Data_Item
            )  return Natural;
   function Get_Tag
            (  Item  : Implicit_Set_Data_Item;
               Index : Positive
            )  return Tag_Type;
   function Is_Implicit
            (  Item : Implicit_Set_Data_Item
            )  return Boolean;
   function Is_Set
            (  Item  : Implicit_Set_Data_Item;
               Index : Positive
            )  return Boolean;
   function Is_Untagged
            (  Item  : Implicit_Set_Data_Item;
               Index : Positive
            )  return Boolean;
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Set_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );
   function Self
            (  Item : Implicit_Set_Data_Item'Class
            )  return Implicit_Set_Data_Item_Ptr;
   ---------------------------------------------------------------------
   type Implicit_External_Set_Of_Data_Item is
      abstract new ASN1_List_Data_Item with
   record
      Map          : Unbounded_Element_Array;
      Definite     : Boolean := False;
      Registered   : Boolean := False;
      Total_Length : Stream_Element_Offset;
      Fed_Count    : Unsigned_64;
      Container    : External_String_Buffer_Ptr;
      Current      : aliased Sequence (1);
      Notifier     : aliased Allocator_Data
                     (  Implicit_External_Set_Of_Data_Item'Access
                     );
   end record;
   type Implicit_External_Set_Of_Data_Item_Ptr is
      access all Implicit_External_Set_Of_Data_Item'Class;
   procedure End_Of_Subsequence
             (  Item    : in out Implicit_External_Set_Of_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_External_Set_Of_Data_Item
             );
   for Implicit_External_Set_Of_Data_Item'Write use Enumerate;
   procedure Finalize
             (  Item : in out Implicit_External_Set_Of_Data_Item
             );
   procedure Freed (Item : in out Implicit_External_Set_Of_Data_Item);
   procedure Register_Allocator
             (  Item : in out Implicit_External_Set_Of_Data_Item
             );
   function Self
            (  Item : Implicit_External_Set_Of_Data_Item'Class
            )  return Implicit_External_Set_Of_Data_Item_Ptr;

end GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Implicit;
