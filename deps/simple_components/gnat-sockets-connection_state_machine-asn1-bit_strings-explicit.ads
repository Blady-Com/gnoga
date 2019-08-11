--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Bit_Strings.Explicit                   Spring, 2019       --
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
--  ASN.1 encoding of bit strings.  The implementation  of Feed supports
--  constructed bit strings.
--
with GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.Implicit;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.Implicit;

package GNAT.Sockets.Connection_State_Machine.ASN1.
        Bit_Strings.Explicit is
--
-- Bit_String_Data_Item -- ASN.1 encoded bit string
--
   type Bit_String_Data_Item is
      new Implicit_Bit_String_Data_Item with null record;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Bit_String_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Bit_String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit (Item : Bit_String_Data_Item) return Boolean;
------------------------------------------------------------------------
--
-- External_Bit_String_Data_Item -- ASN.1 bit string
--
-- The objects of  these  types can share  the same  buffer to store the
-- bit string contents. E.g. if an ASN.1 choice consists of  many string
-- alternatives it might be reasonable to use single buffer for them all
-- since only one alternative is realized. Other cases is to specify the
-- upper memory limit for all strings in a packet.
--
   type External_Bit_String_Data_Item is
      new Implicit_External_Bit_String_Data_Item with null record;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : External_Bit_String_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out External_Bit_String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit (Item : External_Bit_String_Data_Item)
      return Boolean;

end GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.Explicit;
