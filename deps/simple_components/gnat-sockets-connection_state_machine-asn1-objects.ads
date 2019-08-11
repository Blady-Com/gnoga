--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Objects                                Summer, 2019       --
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
--  ASN.1 encoding of dynamic objects
--
with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;

with GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Explicit;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Explicit;

with GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Implicit;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Implicit;

with GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Explicit;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Explicit;

with GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Implicit;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Implicit;

package GNAT.Sockets.Connection_State_Machine.ASN1.Objects is
--
-- Any_Data_Item -- ASN.1 encoded object of any type
--
   type Any_Data_Item is new ASN1_Data_Item with private;
--
-- Get -- Get the actual element
--
--    Item - The referential item
--
-- Returns :
--
--    A pointer to the element
--
-- Exceptions :
--
--    Use_Error - Uinitialized
--
   function Get
            (  Item : Any_Data_Item
            )  return Abstract_ASN1_Data_Item_Ptr;
--
-- Get_Container -- Get the container used to store objects
--
--    Item - The item
--
-- Returns :
--
--    A pointer to the container
--
   function Get_Container
            (  Item : Any_Data_Item
            )  return External_String_Buffer_Ptr;
--
-- Get_Tag -- Get the actual element's tag
--
--    Item - The referential item
--
-- Returns :
--
--    The object tag
--
-- Exceptions :
--
--    Use_Error - Uinitialized
--
   function Get_Tag (Item : Any_Data_Item) return Tag_Type;

private
   type Dynamic_Set_Data_Item (Parent : access Any_Data_Item'Class) is
      new Implicit_External_Set_Of_Data_Item with null record;
   function Create
            (  Item : access Dynamic_Set_Data_Item
            )  return Abstract_ASN1_Data_Item_Ptr;

   type Dynamic_Sequence_Data_Item
        (  Parent : access Any_Data_Item'Class
        )  is new Implicit_External_Sequence_Of_Data_Item with
      null record;
   function Create
            (  Item : access Dynamic_Sequence_Data_Item
            )  return Abstract_ASN1_Data_Item_Ptr;

   type Any_Data_Item is new ASN1_Data_Item with record
      Tag         : Tag_Type := (Universal_Tag, 0, False);
      Length      : Stream_Element_Offset := 0;
      Target      : Abstract_ASN1_Data_Item_Ptr;
      Container   : External_String_Buffer_Ptr;
      Current     : Measured_Data_Item;
      Registered  : Boolean := False;
      Notifier    : aliased Allocator_Data (Any_Data_Item'Access);
   end record;
   type Any_Data_Item_Ptr is
      access all Any_Data_Item'Class;
   function Always_Constructed (Item : Any_Data_Item) return Boolean;
   function Create
            (  Item : access Any_Data_Item;
               Tag  : Tag_Type
            )  return Abstract_ASN1_Data_Item_Ptr;
   procedure Encode
             (  Item    : Any_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Any_Data_Item
             );
   for Any_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out Any_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Finalize (Item : in out Any_Data_Item);
   procedure Freed (Item : in out Any_Data_Item);
   function Get_ASN1_Type (Item : Any_Data_Item) return ASN1_Type;
   function Get_Children
            (  Item : Any_Data_Item
            )  return Data_Item_Ptr_Array;
   function Is_Implicit (Item : Any_Data_Item) return Boolean;
   procedure Register_Allocator (Item : in out Any_Data_Item);
   function Self
            (  Item : Any_Data_Item'Class
            )  return Any_Data_Item_Ptr;

end GNAT.Sockets.Connection_State_Machine.ASN1.Objects;
