--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine       Luebeck            --
--  Interface                                      Winter, 2012       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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
--  This package provides an implementation of server's  side connection
--  object that implements a state  machine  to receive packets from the
--  client side.  The structure of a packet is described by the contents
--  of connection object itself.  Fields of the  object  derived  from a
--  special abstract  type (Data_Item) fed with the input received  from
--  the client  in the order  they are declared in the object.  Once all
--  fields are received  a primitive operation is called to process  the
--  packet. After that the cycle repeats.
--     Enumeration of the fields (introspection) is  based on Ada stream
--  attributes. See ARM 13.13.2(9) for the legality of the approach.
--
with Ada.Finalization;     use Ada.Finalization;
with Ada.Streams;          use Ada.Streams;
with GNAT.Sockets.Server;  use GNAT.Sockets.Server;

with Generic_Unbounded_Array;
with Interfaces;

package GNAT.Sockets.Connection_State_Machine is
--
-- State_Machine -- Connection  client driven  by the contents.  This is
--                  the  base type  to derive  from.  The  derived  type
-- should contain a set of fields with the types derived from Data_Item.
-- These objects will be read automatically in their order.
--
   type State_Machine is abstract new Connection with private;
--
-- Connected -- Overrides Connections_Server...
--
-- If the derived type overrides this procedure, it should call this one
-- from new implemetation.
--
   procedure Connected (Client : in out State_Machine);
--
-- Finalize -- Destruction
--
--    Client - The connection client
--
-- This procedure is to be called from implementation when overridden.
--
   procedure Finalize (Client : in out State_Machine);
--
-- Received -- Overrides GNAT.Sockets.Server...
--
   procedure Received
             (  Client  : in out State_Machine;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Enumerate -- Fake stream I/O procedure
--
-- This procedure  is used internally in order to enumerate the contents
-- of the record type.
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : State_Machine
             );
   for State_Machine'Write use Enumerate;
------------------------------------------------------------------------
--
-- Data_Item -- Base type of a data item to read
--
   type Data_Item is abstract
      new Ada.Finalization.Limited_Controlled with null record;
--
-- Feed -- Incoming data
--
--    Item    - The data item
--    Data    - The array of stream elements containing incoming data
--    Pointer - The first element in the array
--    State   - Of the data item processing
--
-- This procedure  is called  when data  become  available  to  get item
-- contents.  The stream  elements  are  Data (Pointer..Data'Last).  The
-- procedure consumes data and advances Pointer beyond consumed elements
-- The parameter  State indicates processing state.  It  is initially 0.
-- When Item contents is read in full State is set to 0.  When State  is
-- not 0 then Pointer must be set to Data'Last + 1, indicating that more
-- data required.  Feed will be  called  again on the item when new data
-- come with the value of State returned from the last call.
--
   procedure Feed
             (  Item    : in out Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is abstract;
--
-- Get_Size -- Size of the data item in data items
--
--    Item - The data item
--
-- Returns :
--
--    The default implementation is returns 1
--
   function Get_Size (Item : Data_Item) return Positive;
--
-- Enumerate -- Fake stream I/O procedure
--
-- This procedure  is used internally in order to enumerate the contents
-- of the record type, a descendant of Connection.  The elements  of the
-- record  type derived  from Data_Item are  ones which will be fed with
-- data received from the socket.
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Item
             );
   for Data_Item'Write use Enumerate;
------------------------------------------------------------------------
-- Data_Block -- Base type  of  a sequence  of data items.  Derived data
--               types contain fields derived from Data_Item.
--
   type Data_Block is abstract new Data_Item with private;
--
-- Feed -- Implementation
--
   procedure Feed
             (  Item    : in out Data_Block;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Enumerate -- Fake stream I/O procedure
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Block
             );
   for Data_Block'Write use Enumerate;
--
-- Finalize -- Destruction
--
--    Item - Being finalized
--
-- To be called the from derived type's Finalize if overridden
--
   procedure Finalize (Item : in out Data_Block);
--
-- Get_Length -- The number of items contained by the block item
--
--    Item - The block item
--
-- Exceptions :
--
--    Use_Error - The block was not initialized yet
--
   function Get_Length (Item : Data_Block) return Positive;
--
-- Get_Size -- Overriding
--
   function Get_Size (Item : Data_Block) return Positive;
------------------------------------------------------------------------
-- Data_Null -- No data item, used where a data item is required
--
   type Data_Null is new Data_Item with null record;
--
-- Feed -- Implementation
--
   procedure Feed
             (  Item    : in out Data_Null;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
------------------------------------------------------------------------
-- Data_Selector -- Base  type  of  a  list  of  data   items   selected
--                  alternatively.  A  derived  type  has fields derived
-- from Data_Item. One  of the  fields  is used at a time.  So  the type
-- acts as a variant record.  The  field  to select  is set  by  calling
-- Set_Alternative.  Usually  it is done  from  Feed  of some descendant
-- derived from Data_Item,  placed after the field controlling selection
-- of the alternative. When an alternative should enclose several fields
-- a Data_Block descendant is used.
--
   type Data_Selector is new Data_Item with private;
--
-- Feed -- Implementation
--
   procedure Feed
             (  Item    : in out Data_Selector;
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
   procedure Finalize (Item : in out Data_Selector);
--
-- Get_Alternative -- The currently selected alternative
--
--    Item - Selector item
--
-- Returns :
--
--    The alternative number 1..
--
   function Get_Alternative (Item : Data_Selector) return Positive;
--
-- Get_Alternatives_Number -- The number of alternatives
--
--    Item - Selector item
--
-- Returns :
--
--    The total number of alternatives
--
-- Exceptions :
--
--    Use_Error - The selector was not initialized yet
--
   function Get_Alternatives_Number (Item : Data_Selector)
      return Positive;
--
-- Enumerate -- Fake stream I/O procedure
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Selector
             );
   for Data_Selector'Write use Enumerate;
--
-- Set_Alternative -- Select an alternative
--
--    Item        - Selector item
--    Alternative - To select (number 1..Get_Alternatives_Number (Item))
--
-- Exceptions :
--
--    Constraint_Error - Invalid alternative number
--    Use_Error        - The selector was not initialized yet
--
   procedure Set_Alternative
             (  Item        : in out Data_Selector;
                Alternative : Positive
             );
--
-- Get_Size -- Overriding
--
   function Get_Size (Item : Data_Selector) return Positive;
private
   use Interfaces;

   type Data_Item_Ptr is access all Data_Item'Class;
   type Data_Item_Offset is new Integer;
   subtype Data_Item_Address is
      Data_Item_Offset range 1..Data_Item_Offset'Last;
   type Data_Item_Ptr_Array is
      array (Data_Item_Address range <>) of Data_Item_Ptr;
   package Data_Item_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Data_Item_Address,
             Object_Type       => Data_Item_Ptr,
             Object_Array_Type => Data_Item_Ptr_Array,
             Null_Element      => null
          );
   use Data_Item_Arrays;

   type Sequence;
   type Sequence_Ptr is access all Sequence;
   type Sequence (Length : Data_Item_Address) is record
      Caller  : Sequence_Ptr;
      Current : Data_Item_Offset := 1;
      List    : Data_Item_Ptr_Array (1..Length);
   end record;

   type State_Machine is abstract new Connection with record
      State : Stream_Element_Offset := 0;
      Data  : Sequence_Ptr;
   end record;
   procedure Call (Client : in out State_Machine; Data : Sequence_Ptr);

   type Initialization_Stream is new Root_Stream_Type with record
      Count  : Data_Item_Offset := 0;
      Ignore : Data_Item_Offset := 0;
      Data   : Unbounded_Array;
   end record;

   procedure Read
             (  Stream : in out Initialization_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );

   procedure Write
             (  Stream : in out Initialization_Stream;
                Item   : Stream_Element_Array
             );

   function To_Integer (Value : Unsigned_8)  return Integer_8;
   function To_Integer (Value : Unsigned_16) return Integer_16;
   function To_Integer (Value : Unsigned_32) return Integer_32;
   function To_Integer (Value : Unsigned_64) return Integer_64;
   pragma Inline (To_Integer);

   function To_Unsigned (Value : Integer_8)  return Unsigned_8;
   function To_Unsigned (Value : Integer_16) return Unsigned_16;
   function To_Unsigned (Value : Integer_32) return Unsigned_32;
   function To_Unsigned (Value : Integer_64) return Unsigned_64;
   pragma Inline (To_Unsigned);

   function Self (Item : Data_Item'Class) return Data_Item_Ptr;

   type Data_Block_Ptr is access all Data_Block'Class;
   type Data_Block is new Data_Item with record
      Data        : Sequence_Ptr;
      Initialized : Boolean := False;
   end record;
   function Self (Item : Data_Block'Class) return Data_Block_Ptr;

   type Sequence_Array is array (Positive range <>) of Sequence_Ptr;
   type Alternatives_List (Size : Natural) is record
      List : Sequence_Array (1..Size);
   end record;
   type Alternatives_List_Ptr is access Alternatives_List;

   type Data_Selector_Ptr is access all Data_Selector'Class;
   type Data_Selector is new Data_Item with record
      Alternatives : Alternatives_List_Ptr;
      Current      : Positive := 1;
      Initialized  : Boolean  := False;
   end record;
   function Self (Item : Data_Selector'Class) return Data_Selector_Ptr;

end GNAT.Sockets.Connection_State_Machine;
