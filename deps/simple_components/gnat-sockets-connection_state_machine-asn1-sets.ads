--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Sets                                   Summer, 2019       --
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
with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;

package GNAT.Sockets.Connection_State_Machine.ASN1.Sets is
--
-- Reference_Data_Item -- ASN.1 encoded object of any type.  The  target
--                        object  is  kept  externally  in  a  container
-- object. The object itself must contain no items.
--
   type Reference_Data_Item is abstract new ASN1_Data_Item with private;
--
-- Always_Constructed -- Overrides ...Connection_State_Machine.ASN1...
--
   function Always_Constructed
            (  Item : Reference_Data_Item
            )  return Boolean;
--
-- Create -- A new element for the set
--
--    Item  - The set
--
-- The derived  type  must implement  this function  in order  to have a
-- a specific data  type to allocate.  The element must  be allocated in
-- the pool obtained by Get_Container.
--
-- Returns :
--
--    A new element for the set
--
   function Create
            (  Item : access Reference_Data_Item
            )  return Abstract_ASN1_Data_Item_Ptr is abstract;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Reference_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Reference_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get_ASN1_Type -- Overrides ...Connection_State_Machine.ASN1...
--
   function Get_ASN1_Type
            (  Item : Reference_Data_Item
            )  return ASN1_Type;
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
            (  Item : Reference_Data_Item
            )  return External_String_Buffer_Ptr;
--
-- Get_Size -- Overrides ...Connection_State_Machine...
--
   function Get_Size
            (  Item : Reference_Data_Item
            )  return Natural;
--
-- Get -- Get the actual element
--
--    Item     - The referential item
--    Allocate - If true the target is created
--
-- Returns :
--
--    A pointer to the element
--
-- Exceptions :
--
--    Storage_Error - No room in the external buffer
--    Use_Error     - Uinitialized
--
   function Get
            (  Item     : Reference_Data_Item;
               Allocate : Boolean := False
            )  return Abstract_ASN1_Data_Item_Ptr;
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit
            (  Item : Reference_Data_Item
            )  return Boolean;

private
   Invalid_EOC : constant String := "Invalid indefinite ASN.1 " &
                                    "set termination";
   Untagged    : constant String := "ASN.1 set element is untagged";
   Unset       : constant String := "ASN.1 reference is unset";
   ---------------------------------------------------------------------
   type Reference_Data_Item is abstract new ASN1_Data_Item with record
      List       : aliased Sequence (1);
      Target     : Abstract_ASN1_Data_Item_Ptr;
      Container  : External_String_Buffer_Ptr;
      Registered : Boolean := False;
      Notifier   : aliased Allocator_Data (Reference_Data_Item'Access);
   end record;
   type Reference_Data_Item_Ptr is
      access all Reference_Data_Item'Class;
   function Get_Children
            (  Item : Reference_Data_Item
            )  return Data_Item_Ptr_Array;
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Reference_Data_Item
             );
   for Reference_Data_Item'Write use Enumerate;
   procedure Finalize (Item : in out Reference_Data_Item);
   procedure Freed (Item : in out Reference_Data_Item);
   procedure Register_Allocator (Item : in out Reference_Data_Item);
   function Self
            (  Item : Reference_Data_Item'Class
            )  return Reference_Data_Item_Ptr;

   procedure Uninitialized (Item : ASN1_Data_Item'Class);

end GNAT.Sockets.Connection_State_Machine.ASN1.Sets;
