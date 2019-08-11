--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Sequences.Implicit                     Spring, 2019       --
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
--  ASN.1 encoding of sequences
--
with GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values;

with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;

with GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Implicit;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Implicit;

package GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Implicit is
--
-- Implicit_Sequence_Data_Item -- ASN.1  encoded  sequence.  A  sequence
--                                contains  any  number of  other  ASN.1
-- objects input in the order they appear in the sequence. The data type
-- implements an empty sequence.  A non-empty  sequence  is  declared by
-- deriving  a  new  type   from  it  and  placing  items  derived  from
-- ASN1_Data_Item into it:
--
--    type Name_And_Check is new Sequence_Data_Item with record
--       Name  : String_Data_Item (200);
--       Check : Boolean_Data_Item;
--    end record;
--
-- This is equivalent to ASN.1
--
--    SEQUENCE
--    {  Name  OCTET STRING,
--       Check BOOLEAN
--    }
--
-- Note  that  all items in  the sequence  are  non-tagged.  See  tagged
-- sequence for having tags and optional elements in the sequence.
--
   type Implicit_Sequence_Data_Item is
      new ASN1_List_Data_Item with private;
--
-- [Explicit_]Feed -- Implementation of the feed operation
--
   procedure Explicit_Feed
             (  Item    : in out Implicit_Sequence_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Feed
             (  Item    : in out Implicit_Sequence_Data_Item;
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
   procedure Finalize (Item : in out Implicit_Sequence_Data_Item);
--
-- Initialized -- Notification of object initialization
--
--    Item - The object
--
-- This procedure is called when all components of  the object have been
-- enumerated,  so that some additional initialization could be finished
-- with all components known. The default implementation does nothing.
--
   procedure Initialized (Item : in out Implicit_Sequence_Data_Item);
------------------------------------------------------------------------
--
-- Implicit_Tagged_Sequence_Data_Item -- ASN.1   encoded   sequence.   A
--                                       sequence  contains  any  number
-- of  tagged  ASN.1  objects  input  in the  order  they  appear in the
-- sequence.  The objects can be optional  which is recognized using the
-- tags.  The  data  type  implements  an  empty  sequence.  A non-empty
-- sequence is declared by deriving  a new  type  from  it  and  placing
-- items  derived  from ASN1_Data_Item into it:
--
--    type Four_Integers is new Tagged_Sequence_Data_Item with record
--       A : Integer_Data_Item;
--       B : Implicit_Integer_Data_Item;
--       C : Implicit_Integer_Data_Item;
--       D : Implicit_Integer_Data_Item;
--    end record;
--
-- This is equivalent to ASN.1
--
--    SEQUENCE
--    {  A [0] INTEGER,
--       B [1] IMPLICIT INTEGER,
--       C [2] IMPLICIT INTEGER,
--       D [3] IMPLICIT INTEGER
--    }
--
-- The tags by-default are set context-specific in ascending order.  The
-- tags can be modified in order to have other classes, values or having
-- elements optional.  Tagged elements can be marked optional, then they
-- are skipped if the tag does not match.  This behavior  corresponds to
-- ASN.1 sequences like
--
--    SEQUENCE
--    {  A [0] INTEGER,
--       B [1] IMPLICIT INTEGER OPTIONAL,
--       C [2] IMPLICIT INTEGER,
--       D [3] IMPLICIT INTEGER
--    }
--
   type Implicit_Tagged_Sequence_Data_Item is
      new ASN1_Tagged_List_Data_Item with private;
--
-- [Explicit_]Feed -- Implementation of the feed operation
--
   procedure Explicit_Feed
             (  Item    : in out Implicit_Tagged_Sequence_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Feed
             (  Item    : in out Implicit_Tagged_Sequence_Data_Item;
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
             (  Item : in out Implicit_Tagged_Sequence_Data_Item
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
   procedure Initialized
             (  Item : in out Implicit_Tagged_Sequence_Data_Item
             );
--
-- Is_Universal -- Check if a sequence element is universal
--
--    Item  - The sequence
--    Index - The number of the sequence element 1..Get_Length (Item)
--
-- Returns :
--
--    True if the element is untagged
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--    Use_Error        - The sequence was not initialized yet
--
   function Is_Universal
            (  Item  : Implicit_Tagged_Sequence_Data_Item;
               Index : Positive
            )  return Boolean;
--
-- Reset -- Change a sequence element to unset
--
--    Item  - The sequence
--    Index - The number of the sequence element 1..Get_Length (Item)
--    Unset - True if the sequence element is not set
--
-- This procedure marks the element set or unset.
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--    Use_Error        - The sequence was not initialized yet
--
   procedure Reset
             (  Item  : in out Implicit_Tagged_Sequence_Data_Item;
                Index : Positive;
                Unset : Boolean
             );
--
-- Set_Optional -- Change tag of a sequence element
--
--    Item     - The sequence
--    Index    - The number of the sequence element 1..Get_Length (Item)
--    Optional - True if the sequence element is optional
--
-- This procedure  changes the element's tag to make  element  optional.
-- The element is marked optional and unset.
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--    Status_Error     - The element is untagged, cannot be optional
--    Use_Error        - The sequence was not initialized yet
--
   procedure Set_Optional
             (  Item     : in out Implicit_Tagged_Sequence_Data_Item;
                Index    : Positive;
                Optional : Boolean
             );
--
-- Set_Tag -- Set tag of a sequence element
--
--    Item      - The sequence
--    Index     - The number of the sequence element 1..Get_Length
--    Tag       - The tag to assign
--    Unset     - Mark unset, e.g. for Encode
--    Universal - Matches any universal tag
--
-- This procedure  changes the element's tag.  By default  the tags  are
-- assigned  context-specific  in ascending order  starting from 0.  The
-- parameter Unset marks the element unset.  Unset elements  are skipped
-- by Encode.  Upon input Feed  marks absent  optional  elements  unset.
-- When Tag is set universal it matches any actual tag  of the universal
-- class.
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--    Use_Error        - The sequence was not initialized yet
--
   procedure Set_Tag
             (  Item      : in out Implicit_Tagged_Sequence_Data_Item;
                Index     : Positive;
                Tag       : Tag_Type;
                Unset     : Boolean := False;
                Universal : Boolean := False
             );
--
-- Set_Untagged -- Set sequence element untagged
--
--    Item  - The sequence
--    Index - The number of the sequence element 1..Get_Length (Item)
--
-- This procedure removes the element's tag.
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--    Use_Error        - The sequence was not initialized yet
--
   procedure Set_Untagged
             (  Item  : in out Implicit_Tagged_Sequence_Data_Item;
                Index : Positive
             );
------------------------------------------------------------------------
--
-- Implicit_Sequence_Of_Data_Item -- ASN.1   encoded  sequence  of  same
--                                   types. A sequence should contain an
-- array of  ASN.1 objects.  The sequence inputs  objects  an fills  the
-- array. The number of objects is returned by the function Get_Length.
--
--    type Integer_Array is
--       array (Positive range <>) of Integer_Data_Item;
--    type Array_Of_Integers (Size : Positive) is
--       new Sequence_Of_Data_Item with
--    record
--       Values : Integer_Array (1..Size);
--    end record;
--
-- This is equivalent to ASN.1
--
--    SEQUENCE OF INTEGER
--
-- Note that Integer_Array  in the example does  not have  Write stream
-- attribute generated. This must be done explicitly, e.g.
--
--    procedure Enumerate
--              (  Stream : access Root_Stream_Type'Class;
--                 Item   : Integer_Data_Array
--              );
--    for Integer_Data_Array'Write use Enumerate;
--
-- The implementation must walk all array elements and write them:
--
--    procedure Enumerate
--              (  Stream : access Root_Stream_Type'Class;
--                 Item   : Integer_Data_Array
--              )  is
--    begin
--       for Index in Item'Range loop
--          Integer_Data_Item'Write (Stream, Item (Index));
--       end loop;
--    end Enumerate;
--
   type Implicit_Sequence_Of_Data_Item is
      new Public_Sequence_Of_Data_Item with private;
--
-- [Explicit_]Feed -- Implementation of the feed operation
--
   procedure Explicit_Feed
             (  Item    : in out Implicit_Sequence_Of_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Feed
             (  Item    : in out Implicit_Sequence_Of_Data_Item;
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
   procedure Finalize (Item : in out Implicit_Sequence_Of_Data_Item);
--
-- Get_Length -- The number of elements contained by the sequence
--
--    Item - The ASN.1 sequence object
--
-- Returns :
--
--    The number of sequence elements
--
-- Exceptions :
--
--    Use_Error - The sequence was not initialized yet
--
   function Get_Length
            (  Item : Implicit_Sequence_Of_Data_Item
            )  return Natural;
------------------------------------------------------------------------
--
-- Implicit_External_Sequence_Of_Data_Item -- ASN.1  encoded   sequence.
--                                            A  sequence  contains  any
-- number  of other  ASN.1  objects of same type.  The objects  are kept
-- externally in  a container object.  The object itself must contain no
-- items.  As a matter of fact, there is no other difference between set
-- and sequence except for the type.
--
   type Implicit_External_Sequence_Of_Data_Item is
      abstract new Implicit_External_Set_Of_Data_Item with null record;
--
-- Get_ASN1_Type -- Overrides ...Connection_State_Machine.ASN1...
--
   function Get_ASN1_Type
            (  Item : Implicit_External_Sequence_Of_Data_Item
            )  return ASN1_Type;
private
   type Implicit_Sequence_Data_Item is
      new ASN1_List_Data_Item with
   record
      Data        : Sequence_Ptr;
      Initialized : Boolean := False;
   end record;
   type Implicit_Sequence_Data_Item_Ptr is
      access all Implicit_Sequence_Data_Item'Class;
   function Always_Constructed
            (  Item : Implicit_Sequence_Data_Item
            )  return Boolean;
   procedure Encode
             (  Item    : Implicit_Sequence_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Sequence_Data_Item
             );
   for Implicit_Sequence_Data_Item'Write use Enumerate;
   function Get_ASN1_Type
            (  Item : Implicit_Sequence_Data_Item
            )  return ASN1_Type;
   function Get_Children
            (  Item : Implicit_Sequence_Data_Item
            )  return Data_Item_Ptr_Array;
   function Get_Length
            (  Item : Implicit_Sequence_Data_Item
            )  return Natural;
   function Get_Size
            (  Item : Implicit_Sequence_Data_Item
            )  return Natural;
   function Is_Implicit
            (  Item : Implicit_Sequence_Data_Item
            )  return Boolean;

   function Self (Item : Implicit_Sequence_Data_Item'Class)
      return Implicit_Sequence_Data_Item_Ptr;

   type Implicit_Sequence_Of_Data_Item is
      new Public_Sequence_Of_Data_Item with
   record
      Initialized  : Boolean := False;
      Definite     : Boolean := False;
      List         : Data_Item_Ptr_Array_Ptr;
      Fed_Count    : Unsigned_64;
      Total_Length : Stream_Element_Offset;
      Current_Item : aliased Sequence (1);
   end record;
   type Implicit_Sequence_Of_Data_Item_Ptr is
      access all Implicit_Sequence_Of_Data_Item'Class;
   procedure Encode
             (  Item    : Implicit_Sequence_Of_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure End_Of_Subsequence
             (  Item    : in out Implicit_Sequence_Of_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Sequence_Of_Data_Item
             );
   for Implicit_Sequence_Of_Data_Item'Write use Enumerate;
   function Get_Children
            (  Item : Implicit_Sequence_Of_Data_Item
            )  return Data_Item_Ptr_Array;
   function Is_Implicit
            (  Item : Implicit_Sequence_Of_Data_Item
            )  return Boolean;
   function Get_Size
            (  Item : Implicit_Sequence_Of_Data_Item
            )  return Natural;
   procedure Next_Item
             (  Item    : in out Implicit_Sequence_Of_Data_Item;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Sequence_Of_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );
   function Self
            (  Item : Implicit_Sequence_Of_Data_Item'Class
            )  return Implicit_Sequence_Of_Data_Item_Ptr;

   type Tagged_Data_Item is record
      Tag       : Tag_Type := (Universal_Tag, 0, False);
      Unset     : Boolean  := True;
      Untagged  : Boolean  := False;
      Universal : Boolean  := False; -- Matches any universal tag
      Item      : Data_Item_Ptr;
   end record;
   type Tagged_Data_Item_Array is
      array (Positive range <>) of Tagged_Data_Item;
   type Tagged_Data_Item_Array_Ptr is
      access Tagged_Data_Item_Array;
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Tagged_Data_Item_Array,
             Tagged_Data_Item_Array_Ptr
          );
   type Current_Element is new Measured_Data_Item with record
      Index       : Natural  := 0;
      Tag         : Tag_Type := (Universal_Tag, 0, False);
      Constructed : Boolean;
   end record;
   type Implicit_Tagged_Sequence_Data_Item is
      new ASN1_Tagged_List_Data_Item with
   record
      Initialized  : Boolean := False;
      Definite     : Boolean := False;
      List         : Tagged_Data_Item_Array_Ptr;
      Total_Length : Stream_Element_Offset;
      Current      : Current_Element;
   end record;
   type Implicit_Tagged_Sequence_Data_Item_Ptr is
      access all Implicit_Tagged_Sequence_Data_Item'Class;
   function Always_Constructed
            (  Item : Implicit_Tagged_Sequence_Data_Item
            )  return Boolean;
   procedure Encode
             (  Item    : Implicit_Tagged_Sequence_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Tagged_Sequence_Data_Item
             );
   for Implicit_Tagged_Sequence_Data_Item'Write use Enumerate;
   procedure End_Of_Subsequence
             (  Item    : in out Implicit_Tagged_Sequence_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type
            (  Item : Implicit_Tagged_Sequence_Data_Item
            )  return ASN1_Type;
   function Get_Children
            (  Item : Implicit_Tagged_Sequence_Data_Item
            )  return Data_Item_Ptr_Array;
   function Get_Length
            (  Item : Implicit_Tagged_Sequence_Data_Item
            )  return Natural;
   function Get_Size
            (  Item : Implicit_Tagged_Sequence_Data_Item
            )  return Natural;
   function Get_Tag
            (  Item  : Implicit_Tagged_Sequence_Data_Item;
               Index : Positive
            )  return Tag_Type;
   function Is_Implicit
            (  Item : Implicit_Tagged_Sequence_Data_Item
            )  return Boolean;
   function Is_Set
            (  Item  : Implicit_Tagged_Sequence_Data_Item;
               Index : Positive
            )  return Boolean;
   function Is_Untagged
            (  Item  : Implicit_Tagged_Sequence_Data_Item;
               Index : Positive
            )  return Boolean;
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Tagged_Sequence_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );
   function Self
            (  Item : Implicit_Tagged_Sequence_Data_Item'Class
            )  return Implicit_Tagged_Sequence_Data_Item_Ptr;

end GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Implicit;
