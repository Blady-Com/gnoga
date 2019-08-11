--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Sets.Generic_Reference                 Summer, 2019       --
--  Implementation                                                    --
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

package body GNAT.Sockets.Connection_State_Machine.ASN1.Sets.
             Generic_Reference is

   function Create
            (  Item : access Reference_To
            )  return Abstract_ASN1_Data_Item_Ptr is
      type Element_Type_Ptr is access Element_Type;
      for Element_Type_Ptr'Storage_Pool use
          Get_Container (Item.all).Pool;
      Element : constant Element_Type_Ptr := new Element_Type;
   begin
      return Element.all'Unchecked_Access;
   end Create;

   function Get
            (  Item     : Reference_To;
               Allocate : Boolean := False
            )  return Value_Type is
      Element : constant Abstract_ASN1_Data_Item_Ptr :=
                         Get (Item, Allocate);
   begin
      return Get (Element_Type (Element.all));
   end Get;

   function Get
            (  Item     : Reference_To;
               Allocate : Boolean := False
            )  return Element_Type_Ptr is
      Element : constant Abstract_ASN1_Data_Item_Ptr :=
                         Get (Item, Allocate);
   begin
      return Element_Type (Element.all)'Unchecked_Access;
   end Get;

   procedure Set
             (  Item     : in out Reference_To;
                Value    : Value_Type;
                Allocate : Boolean := False
             )  is
      Element : constant Abstract_ASN1_Data_Item_Ptr :=
                         Get (Item, Allocate);
   begin
      Set (Element_Type (Element.all), Value);
   end Set;

end GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Generic_Reference;

