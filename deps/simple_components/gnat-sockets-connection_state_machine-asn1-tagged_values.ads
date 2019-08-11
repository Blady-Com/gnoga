--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Tagged_Values                          Spring, 2019       --
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
--  ASN.1 encoding of tagged values
--
with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;

package GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values is
--
-- Tagged_Data_Item -- ASN.1  encoded  tagged  value.  A  tagged  object
--                     contains another object for which it provides the
--                     tag.
--
--    type Tagged_Name is new Tagged_Data_Item with record
--       Value : Implicit_String_Data_Item (100);
--    end record;
--
-- After input, the component Tag contains  the actual value  of the tag
-- and Value does the input string. In ASN.1 notation it could be put as
--
--    [x] IMPLICIT OCTET STRING
--
   type Public_Tagged_Data_Item is
      abstract new Abstract_ASN1_Data_Item with
   record
      Tag : Tag_Type;
   end record;

   type Tagged_Data_Item is new Public_Tagged_Data_Item with private;
--
-- Enumerate -- Fake stream I/O procedure
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Tagged_Data_Item
             );
   for Tagged_Data_Item'Write use Enumerate;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Tagged_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Tagged_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get_Size -- Overriding
--
   function Get_Size (Item : Tagged_Data_Item) return Natural;
--
-- Initialized -- Notification of object initialization
--
--    Item - The object
--
-- This procedure is called when all components of  the object have been
-- enumerated,  so that some additional initialization could be finished
-- with all components known. The default implementation does nothing.
--
   procedure Initialized (Item : in out Tagged_Data_Item);
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit (Item : Tagged_Data_Item) return Boolean;

private
   type Tagged_Data_Item is new Public_Tagged_Data_Item with record
      Selected     : Measured_Data_Item;
      Value        : Abstract_ASN1_Data_Item_Ptr;
      Initialized  : Boolean := False;
   end record;
   type Tagged_Data_Item_Ptr is access all Tagged_Data_Item'Class;
   function Always_Constructed (Item : Tagged_Data_Item) return Boolean;
   function Self (Item : Tagged_Data_Item'Class)
      return Tagged_Data_Item_Ptr;

end GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values;
