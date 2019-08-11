--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Sets                                   Summer, 2019       --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Ada.Tags;           use Ada.Tags;

with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Connection_State_Machine.ASN1.Sets is

   procedure Uninitialized (Item : ASN1_Data_Item'Class) is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         (  "ASN.1 "
         &  Image (Get_ASN1_Type (Item))
         &  " "
         &  Expanded_Name (Item'Tag)
         &  " was not properly initialized"
      )  );
   end Uninitialized;

   function Always_Constructed
            (  Item : Reference_Data_Item
            )  return Boolean is
   begin
      if Item.Container = null or else Item.Target = null then
         return False;
      else
         return Always_Constructed (Item.Target.all);
      end if;
   end Always_Constructed;

   procedure Check
             (  Container : ASN1_Data_Item'Class;
                Item      : Data_Item'Class
             )  is
   begin
      if Item not in Abstract_ASN1_Data_Item'Class then
         Raise_Exception
         (  Use_Error'Identity,
            (  "ASN.1 "
            &  Image (Get_ASN1_Type (Container))
            &  " contains non ASN.1 object "
            &  Expanded_Name (Item'Tag)
         )  );
      end if;
   end Check;

   procedure Encode
             (  Item    : Reference_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      if Item.Container = null then
         Raise_Exception (Data_Error'Identity, Unset);
      elsif Item.Target = null then
         null;
      else
         Encode (Item.Target.all, Data, Pointer);
      end if;
   end Encode;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Reference_Data_Item
             )  is
      procedure Find
                (  Object : in out Reference_Data_Item'Class;
                   Shared : Shared_Data_Item_Ptr
                )  is
         This : Shared_Data_Item_Ptr := Shared;
      begin
         loop
            if This = null then
               Raise_Exception (Use_Error'Identity, No_Container);
            end if;
            exit when This.all in External_String_Buffer'Class;
            This := This.Previous;
         end loop;
         Object.Container :=
            External_String_Buffer'Class (This.all)'Unchecked_Access;
      end Find;
   begin
      Check_Initialization_Stream (Item, Stream.all);
      declare
         Count : Data_Item_Offset := 0;
         This  : Initialization_Stream'Class renames
                 Initialization_Stream'Class (Stream.all);
      begin
         if Item.Container = null then
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
             (  Item    : in out Reference_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      procedure Call_Item (Element : Abstract_ASN1_Data_Item_Ptr) is
         List : Sequence renames Item.List;
      begin
         List.Caller   := null;
         List.State    := 0;
         List.Current  := 1;
         List.List (1) := Element.all'Unchecked_Access;
         Call (Client, Pointer, List'Unchecked_Access);
         State := 0;
      end Call_Item;
   begin
      if State = 0 then
         if Item.Container = null then
            Uninitialized (Item);
         end if;
         Item.Target :=
            Create (Reference_Data_Item'Class (Item)'Unchecked_Access);
         Register_Allocator (Item);
         declare
            Stream  : aliased Initialization_Stream;
         begin
            Stream.Shared := Item.Container.all'Unchecked_Access;
            Abstract_ASN1_Data_Item'Class'Write
            (  Stream'Access,
               Item.Target.all
            );
         end;
         Call_Item (Item.Target);
      else
         Feed
         (  Item    => Item.Target.all,
            Data    => Data,
            Pointer => Pointer,
            Client  => Client,
            State   => State
         );
      end if;
   end Feed;

   procedure Finalize (Item : in out Reference_Data_Item) is
   begin
      Freed (Item);
      Finalize (ASN1_Data_Item (Item));
   end Finalize;

   procedure Freed (Item : in out Reference_Data_Item) is
   begin
      if Item.Registered then
         declare
            type Element_Type_Ptr is
               access Abstract_ASN1_Data_Item'Class;
            for Element_Type_Ptr'Storage_Pool use Item.Container.Pool;
            function Convert is
               new Ada.Unchecked_Conversion
                   (  Abstract_ASN1_Data_Item_Ptr,
                      Element_Type_Ptr
                   );
            procedure Free is
               new Ada.Unchecked_Deallocation
                   (  Abstract_ASN1_Data_Item'Class,
                      Element_Type_Ptr
                   );
            This : Element_Type_Ptr;
         begin
            Item.Registered := False;
            This := Convert (Item.Target);
            Free (This);
         end;
      end if;
      Item.Target := null;
   end Freed;

   function Get
            (  Item     : Reference_Data_Item;
               Allocate : Boolean := False
            )  return Abstract_ASN1_Data_Item_Ptr is
   begin
      if Item.Container = null then
         Uninitialized (Item);
      elsif Item.Target = null then
         if not Allocate then
            Raise_Exception (Data_Error'Identity, Unset);
         end if;
         declare
            Stream : aliased Initialization_Stream;
            Object : Reference_Data_Item'Class renames Self (Item).all;
         begin
            Register_Allocator (Object);
            Object.Target := Create (Object'Access);
            Stream.Shared := Item.Container.all'Unchecked_Access;
            Abstract_ASN1_Data_Item'Class'Write
            (  Stream'Access,
               Item.Target.all
            );
         end;
      end if;
      return Item.Target;
   end Get;

   function Get_ASN1_Type
            (  Item : Reference_Data_Item
            )  return ASN1_Type is
   begin
      if Item.Container = null or else Item.Target = null then
         Raise_Exception (Data_Error'Identity, Unset);
      else
         return Get_ASN1_Type
                (  ASN1.ASN1_Data_Item'Class (Item.Target.all)
                );
      end if;
   end Get_ASN1_Type;

   function Get_Children
            (  Item : Reference_Data_Item
            )  return Data_Item_Ptr_Array is
   begin
      if Item.Container = null then
         Raise_Exception (Data_Error'Identity, Unset);
      elsif Item.Target = null then
         return (1..0 => null);
      else
         return Get_Children
                (  ASN1.ASN1_Data_Item'Class (Item.Target.all)
                );
      end if;
   end Get_Children;

   function Get_Container
            (  Item : Reference_Data_Item
            )  return External_String_Buffer_Ptr is
   begin
      return Item.Container;
   end Get_Container;

   function Get_Size
            (  Item : Reference_Data_Item
            )  return Natural is
   begin
      if Item.Target = null then
         return 1;
      else
         return Get_Size (Item.Target.all);
      end if;
   end Get_Size;

   function Is_Implicit
            (  Item : Reference_Data_Item
            )  return Boolean is
   begin
      if Item.Container = null or else Item.Target = null then
         Raise_Exception (Data_Error'Identity, Unset);
      else
         return Is_Implicit (Item.Target.all);
      end if;
   end Is_Implicit;

   procedure Register_Allocator (Item : in out Reference_Data_Item) is
   begin
      if not Item.Registered then
         Item.Registered := True;
         Item.Notifier.Previous := Item.Container.Allocators;
         Item.Container.Allocators := Item.Notifier'Unchecked_Access;
      end if;
   end Register_Allocator;

   function Self
            (  Item : Reference_Data_Item'Class
            )  return Reference_Data_Item_Ptr is
      package From_Set_Address is
         new System.Address_To_Access_Conversions
             (  Reference_Data_Item'Class
             );
      use From_Set_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

end GNAT.Sockets.Connection_State_Machine.ASN1.Sets;
