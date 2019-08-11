--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Strings                                Spring, 2019       --
--  Implementation                                                    --
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

package body GNAT.Sockets.Connection_State_Machine.ASN1.Strings.
             Implicit.Constrained is

   procedure Encode
             (  Item :
                   Implicit_Constrained_External_String_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Check_Value
      (  Item,
         Item.Driver,
         Get_ASN1_Type (ASN1_Data_Item'Class (Item))
      );
      Encode
      (  Implicit_External_String_Data_Item (Item),
         Data,
         Pointer
      );
   end Encode;

   procedure Encode
             (  Item    : Implicit_Constrained_String_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Check_Value
      (  Item,
         Item.Driver,
         Get_ASN1_Type (ASN1_Data_Item'Class (Item))
      );
      Encode (Implicit_String_Data_Item (Item), Data, Pointer);
   end Encode;

   function Get_ASN1_Type
            (  Item : Implicit_Constrained_External_String_Data_Item
            )  return ASN1_Type is
   begin
      return Item.String_Type;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Implicit_Constrained_String_Data_Item
            )  return ASN1_Type is
   begin
      return Item.String_Type;
   end Get_ASN1_Type;

   procedure Initialize
             (  Item : in out Implicit_Constrained_String_Data_Item
             )  is
   begin
      Item.Driver.Constraint := Item.String_Type;
      Initialize (Implicit_String_Data_Item (Item));
   end Initialize;

   procedure Initialize
             (  Item : in out
                       Implicit_Constrained_External_String_Data_Item
             )  is
   begin
      Item.Driver.Constraint := Item.String_Type;
      Initialize (Implicit_External_String_Data_Item (Item));
   end Initialize;

   procedure Set_Value
             (  Item  : in out
                        Implicit_Constrained_External_String_Data_Item;
                Value : String
             )  is
   begin
      Check_Value (Value, Item.String_Type);
      Set_Value
      (  Implicit_External_String_Data_Item (Item),
         Value
      );
   end Set_Value;

   procedure Set_Value
             (  Item  : in out
                        Implicit_Constrained_String_Data_Item;
                Value : String
             )  is
   begin
      Check_Value (Value, Item.String_Type);
      Set_Value
      (  Implicit_String_Data_Item (Item),
         Value
      );
   end Set_Value;

end GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit.
    Constrained;
