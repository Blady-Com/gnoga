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

with Ada.Exceptions;              use Ada.Exceptions;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Ada.Tags;                    use Ada.Tags;
with Strings_Edit.Integers;       use Strings_Edit.Integers;
with Strings_Edit.UTF8.Handling;  use Strings_Edit.UTF8.Handling;

with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;

with Ada.Unchecked_Conversion;
with Strings_Edit.UTF8.ITU_T61;
with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Connection_State_Machine.ASN1.Strings.
             Implicit is

   procedure Encode
             (  Item    : Implicit_External_String_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      case Get_ASN1_Type (ASN1_Data_Item'Class (Item)) is
         when Universal_String_Tag =>
            Put_Universal (Data, Pointer, Get_Value (Item));
         when Character_String_Tag =>
            Put_Latin1 (Data, Pointer, Get_Value (Item));
         when BMP_String_Tag =>
            Put_BMP (Data, Pointer, Get_Value (Item));
         when others =>
            Put (Data, Pointer, Get_Value (Item));
      end case;
   end Encode;

   procedure Encode
             (  Item    : Implicit_String_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      case Get_ASN1_Type (ASN1_Data_Item'Class (Item)) is
         when Universal_String_Tag =>
            Put_Universal (Data, Pointer, Item.Value (1..Item.Length));
         when Character_String_Tag =>
            Put_Latin1 (Data, Pointer, Item.Value (1..Item.Length));
         when BMP_String_Tag =>
            Put_BMP (Data, Pointer, Item.Value (1..Item.Length));
         when others =>
            Put (Data, Pointer, Item.Value (1..Item.Length));
      end case;
   end Encode;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_External_String_Data_Item
             )  is
      procedure Find
                (  Object : in out
                            Implicit_External_String_Data_Item'Class;
                   Shared : Shared_Data_Item_Ptr
                )  is
         This : Shared_Data_Item_Ptr := Shared;
      begin
         loop
            if This = null then
               Raise_Exception
               (  Use_Error'Identity,
                  (  "Missing buffer for externally buffered "
                  &  "ASN.1 string"
               )  );
            end if;
            exit when This.all in External_String_Buffer'Class;
            This := This.Previous;
         end loop;
         Object.Driver.Buffer :=
            External_String_Buffer'Class (This.all)'Unchecked_Access;
      end Find;
   begin
      Check_Initialization_Stream (Item, Stream.all);
      declare
         This : Initialization_Stream'Class renames
                Initialization_Stream'Class (Stream.all);
      begin
         if Item.Driver.Buffer = null then
            if This.Parent = null then
               Find (Self (Item).all, This.Shared);
            else
               Find (Self (Item).all, This.Parent.all);
            end if;
         end if;
         Enumerate (Stream, ASN1_Data_Item (Item));
      end;
   end Enumerate;

   procedure Feed
             (  Item    : in out Implicit_External_String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if Item.Driver.Buffer = null then
         Uninitialized (Item);
      end if;
      Feed (Item, Item.Driver, Data, Pointer, Client, State);
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed (Item, Item.Driver, Data, Pointer, Client, State);
   end Feed;

   function Get_ASN1_Type
            (  Item : Implicit_External_String_Data_Item
            )  return ASN1_Type is
   begin
      return UTF8_String_Tag;
   end Get_ASN1_Type;

   function Get_Buffer
            (  Item : Implicit_External_String_Data_Item
            )  return External_String_Buffer_Ptr is
   begin
      return Item.Driver.Buffer;
   end Get_Buffer;

   function Get_Data
            (  Item : Implicit_String_Data_Item
            )  return String_Data_Ptr is
   begin
      return Self (Item).Driver'Unchecked_Access;
   end Get_Data;

   function Get_Data
            (  Item : Implicit_External_String_Data_Item
            )  return String_Data_Ptr is
   begin
      return Self (Item).Driver'Unchecked_Access;
   end Get_Data;

   function Get_Length
            (  Item : Implicit_External_String_Data_Item
            )  return Natural is
   begin
      if Item.Driver.Buffer = null then
         return 0;
      else
         return Item.Driver.Length;
      end if;
   end Get_Length;

   function Get_Value
            (  Item : Implicit_External_String_Data_Item
            )  return String is
   begin
      if Item.Driver.Buffer = null then
         return "";
      else
         return Item.Driver.Buffer.Buffer
                (  Item.Driver.Start
                .. Item.Driver.Start + Item.Driver.Length - 1
                );
      end if;
   end Get_Value;

   function Is_Implicit (Item : Implicit_External_String_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit (Item : Implicit_String_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Self
            (  Item : Implicit_External_String_Data_Item'Class
            )  return Implicit_External_String_Data_Item_Ptr is
      package From_Address is
         new System.Address_To_Access_Conversions
             (  Implicit_External_String_Data_Item'Class
             );
      use From_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   function Self
            (  Item : Implicit_String_Data_Item'Class
            )  return Implicit_String_Data_Item_Ptr is
      package From_Address is
         new System.Address_To_Access_Conversions
             (  Implicit_String_Data_Item'Class
             );
      use From_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_String_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length >= 0 then
         Item.Driver.Last     := Length;
         Item.Driver.Definite := True;
      else
         Item.Driver.Last     := 0;
         Item.Driver.Definite := False;
      end if;
   end Set_Implicit_Tag;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_External_String_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length >= 0 then
         Item.Driver.Last     := Length;
         Item.Driver.Definite := True;
      else
         Item.Driver.Last     := 0;
         Item.Driver.Definite := False;
      end if;
   end Set_Implicit_Tag;

   procedure Set_Value
             (  Item  : in out Implicit_External_String_Data_Item;
                Value : String
             )  is
      Driver : External_String_Data renames Item.Driver;
   begin
      if Driver.Buffer = null then
         Uninitialized (Item);
      elsif (  Value'Length
            >  Driver.Buffer.Size - Driver.Buffer.Length
            )  then
         Raise_Exception (Storage_Error'Identity, Too_Large);
      else
         Driver.Start  := Driver.Buffer.Length + 1;
         Driver.Length := Value'Length;
         Driver.Buffer.Buffer
         (  Driver.Start
         ..  Driver.Start + Value'Length - 1
         )  := Value;
         Driver.Buffer.Length := Driver.Buffer.Length + Value'Length;
      end if;
   end Set_Value;

end GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit;
