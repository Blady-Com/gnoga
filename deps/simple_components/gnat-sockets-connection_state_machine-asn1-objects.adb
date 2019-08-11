--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Objects                                Summer, 2019       --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Ada.Tags;               use Ada.Tags;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with GNAT.Sockets.Connection_State_Machine.ASN1.Booleans;
with GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.Implicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Dates;
with GNAT.Sockets.Connection_State_Machine.ASN1.Distinguished_Names;
with GNAT.Sockets.Connection_State_Machine.ASN1.Indefinite_Unsigneds;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers_64;
with GNAT.Sockets.Connection_State_Machine.ASN1.Long_Floats;
with GNAT.Sockets.Connection_State_Machine.ASN1.Nulls;
with GNAT.Sockets.Connection_State_Machine.ASN1.Object_Identifiers;
with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit.
     Constrained;

with GNAT.Sockets.Connection_State_Machine.ASN1.Choices;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Choices;

with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Connection_State_Machine.ASN1.Objects is

   Unset : constant String := "ASN.1 object is unset";

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
            (  Item : Any_Data_Item
            )  return Boolean is
   begin
      if Item.Container = null or else Item.Target = null then
         return False;
      else
         return Always_Constructed (Item.Target.all);
      end if;
   end Always_Constructed;

   function Create
            (  Item : access Dynamic_Set_Data_Item
            )  return Abstract_ASN1_Data_Item_Ptr is
      Pool : Arena_Pool renames Item.Parent.Container.Pool;
      type Ptr_Type is access Any_Data_Item;
      for Ptr_Type'Storage_Pool use Pool;
      Ptr : constant Ptr_Type := new Any_Data_Item;
   begin
      External_Initialize
      (  Ptr.all,
         Item.Parent.Container.all'Unchecked_Access
      );
      return Ptr.all'Unchecked_Access;
   end Create;

   function Create
            (  Item : access Dynamic_Sequence_Data_Item
            )  return Abstract_ASN1_Data_Item_Ptr is
      Pool : Arena_Pool renames Item.Parent.Container.Pool;
      type Ptr_Type is access Any_Data_Item;
      for Ptr_Type'Storage_Pool use Pool;
      Ptr : constant Ptr_Type := new Any_Data_Item;
   begin
      External_Initialize
      (  Ptr.all,
         Item.Parent.Container.all'Unchecked_Access
      );
      return Ptr.all'Unchecked_Access;
   end Create;

   function Create
            (  Item : access Any_Data_Item;
               Tag  : Tag_Type
            )  return Abstract_ASN1_Data_Item_Ptr is
      Pool   : Arena_Pool renames Item.Container.Pool;
      Result : Abstract_ASN1_Data_Item_Ptr;
   begin
      case Tag.Class is
         when Application_Tag      |
              Context_Specific_Tag |
              Private_Tag          =>
            declare
               type Ptr_Type is access Any_Data_Item;
               for Ptr_Type'Storage_Pool use Pool;
               Ptr : constant Ptr_Type := new Any_Data_Item;
            begin
               Ptr.Tag := Tag;
               Result := Ptr.all'Unchecked_Access;
            end;
         when Universal_Tag =>
            case Tag.Value is
               when Boolean_Tag =>
                  declare
                     use ASN1.Booleans;
                     type Ptr_Type is access Implicit_Boolean_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                                    new Implicit_Boolean_Data_Item;
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Integer_Tag =>
                  declare
                     use ASN1.Indefinite_Unsigneds;
                     type Ptr_Type is
                        access Implicit_Indefinite_Unsigned_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                           new Implicit_Indefinite_Unsigned_Data_Item;
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Bit_String_Tag =>
                  declare
                     use ASN1.Bit_Strings.Implicit;
                     type Ptr_Type is
                        access Implicit_External_Bit_String_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                        new Implicit_External_Bit_String_Data_Item;
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Octet_String_Tag     |
                    UTF8_String_Tag      |
                    Printable_String_Tag |
                    Teletext_String_Tag  |
                    Videotext_String_Tag |
                    IA5_String_Tag       |
                    Graphic_String_Tag   |
                    ISO646_String_Tag    |
                    General_String_Tag   |
                    Universal_String_Tag |
                    Character_String_Tag |
                    Numeric_String_Tag   |
                    BMP_String_Tag       =>
                  declare
                     use ASN1.Strings.Implicit.Constrained;
                     type Ptr_Type is access
                        Implicit_Constrained_External_String_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type := new
                        Implicit_Constrained_External_String_Data_Item
                        (  Tag.Value
                        );
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Null_Tag =>
                  declare
                     use ASN1.Nulls;
                     type Ptr_Type is
                        access Implicit_Null_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type := new Implicit_Null_Data_Item;
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Object_Identifier_Tag =>
                  declare
                     use ASN1.Object_Identifiers;
                     type Ptr_Type is
                        access Implicit_External_OID_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                                    new Implicit_External_OID_Data_Item;
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Object_Descriptor_Tag =>
                  declare
                     use ASN1.Distinguished_Names;
                     type Ptr_Type is
                        access Implicit_External_DN_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                                    new Implicit_External_DN_Data_Item;
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Real_Tag =>
                  declare
                     use ASN1.Long_Floats;
                     type Ptr_Type is
                        access Implicit_Real_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                                    new Implicit_Real_Data_Item;
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Enumerated_Tag =>
                  declare
                     use ASN1.Integers_64;
                     type Ptr_Type is
                        access Implicit_Integer_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                                    new Implicit_Integer_Data_Item;
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Relative_OID_Tag =>
                  declare
                     use ASN1.Object_Identifiers;
                     type Ptr_Type is
                        access Implicit_External_Relative_OID_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                        new Implicit_External_Relative_OID_Data_Item;
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Sequence_Tag =>
                  declare
                     type Ptr_Type is access Dynamic_Sequence_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                                    new Dynamic_Sequence_Data_Item
                                        (  Item.all'Unchecked_Access
                                        );
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Set_Tag =>
                  declare
                     type Ptr_Type is access Dynamic_Set_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                                    new Dynamic_Set_Data_Item
                                        (  Item.all'Unchecked_Access
                                        );
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Date_Tag =>
                  declare
                     use ASN1.Dates;
                     type Ptr_Type is access Implicit_Date_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                                    new Implicit_Date_Data_Item;
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Time_Tag =>
                  declare
                     use ASN1.Dates;
                     type Ptr_Type is access Implicit_Time_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                                    new Implicit_Time_Data_Item;
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when UTC_Time_Tag =>
                  declare
                     use ASN1.Dates;
                     type Ptr_Type is access Implicit_UTC_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                                    new Implicit_UTC_Data_Item;
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when Generalized_Time_Tag =>
                  declare
                     use ASN1.Dates;
                     type Ptr_Type is
                        access Implicit_Generalized_Time_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type :=
                           new Implicit_Generalized_Time_Data_Item;
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
               when others =>
                  declare
                     use ASN1.Strings.Implicit.Constrained;
                     type Ptr_Type is access
                        Implicit_Constrained_External_String_Data_Item;
                     for Ptr_Type'Storage_Pool use Pool;
                     Ptr : constant Ptr_Type := new
                        Implicit_Constrained_External_String_Data_Item
                        (  Octet_String_Tag
                        );
                  begin
                     Result := Ptr.all'Unchecked_Access;
                  end;
            end case;
      end case;
      External_Initialize
      (  Result.all,
         Item.Container.all'Unchecked_Access
      );
      return Result;
   end Create;

   procedure Encode
             (  Item    : Any_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      if Item.Container = null or else Item.Target = null then
         Raise_Exception (Data_Error'Identity, Unset);
      else
         Put
         (  Data,
            Pointer,
            Item.Tag,
            (  Always_Constructed (ASN1_Data_Item'Class (Item))
            or else
               (  Item.Tag.Class /= Universal_Tag
               and then
                  not Is_Implicit (ASN1_Data_Item'Class (Item))
         )  )  );
         Encode (Item.Target.all, Data, Pointer);
      end if;
   end Encode;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Any_Data_Item
             )  is
      procedure Find
                (  Object : in out Any_Data_Item'Class;
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
             (  Item    : in out Any_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         case ASN1_Tag (Data (Pointer)) and 2#1100_0000# is
            when Application_Class =>
               Item.Tag.Class := Application_Tag;
            when Context_Specific_Class =>
               Item.Tag.Class := Context_Specific_Tag;
            when Private_Class =>
               Item.Tag.Class := Private_Tag;
            when others =>
               Item.Tag.Class := Universal_Tag;
         end case;
         Item.Tag.Optional := False;
         Item.Tag.Value    := ASN1_Type (Data (Pointer) and 16#1F#);
         Pointer           := Pointer + 1;
         if Item.Tag.Value < 31 then
            Item.Target := Create (Item'Access, Item.Tag);
            Call
            (  Item    => Item.Current,
               Callee  => Item.Target.all,
               Tag     => Item.Tag,
               Pointer => Pointer,
               Client  => Client,
               State   => State
            );
            return;
         end if;
         State          := 1;
         Item.Tag.Value := 0;
         if Pointer > Data'Last then
            return;
         end if;
      end if;
      loop
         begin
            if Data (Pointer) > 127 then
               Item.Tag.Value :=
                  (  Item.Tag.Value * 128
                  +  (ASN1_Type (Data (Pointer)) - 128)
                  );
               Pointer := Pointer + 1;
            else
               Item.Tag.Value :=
                  Item.Tag.Value * 128 + ASN1_Type (Data (Pointer));
               Pointer := Pointer + 1;
               State := 0;
               Item.Target := Create (Item'Access, Item.Tag);
               Call
               (  Item    => Item.Current,
                  Callee  => Item.Target.all,
                  Tag     => Item.Tag,
                  Pointer => Pointer,
                  Client  => Client,
                  State   => State
               );
               return;
            end if;
         exception
            when Constraint_Error =>
               Raise_Exception (Data_Error'Identity, Tag_Too_Large);
         end;
         exit when Pointer > Data'Last;
      end loop;
   end Feed;

   procedure Finalize (Item : in out Any_Data_Item) is
   begin
      Freed (Item);
      Finalize (ASN1_Data_Item (Item));
   end Finalize;

   procedure Freed (Item : in out Any_Data_Item) is
   begin
      if Item.Registered then
         declare
            type Element_Type_Ptr is access Abstract_ASN1_Data_Item'Class;
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

   function Get_ASN1_Type (Item : Any_Data_Item) return ASN1_Type is
   begin
      if Item.Container = null or else Item.Target = null then
         Raise_Exception (Data_Error'Identity, Unset);
      elsif Item.Tag.Class = Universal_Tag then
         return Item.Tag.Value;
      else
         return Get_ASN1_Type (ASN1_Data_Item'Class (Item.Target.all));
      end if;
   end Get_ASN1_Type;

   function Get
            (  Item : Any_Data_Item
            )  return Abstract_ASN1_Data_Item_Ptr is
   begin
      if Item.Container = null or else Item.Target = null then
         Uninitialized (Item);
      end if;
      return Item.Target;
   end Get;

   function Get_Children
            (  Item : Any_Data_Item
            )  return Data_Item_Ptr_Array is
   begin
      if Item.Container = null or else Item.Target = null then
         Raise_Exception (Data_Error'Identity, Unset);
      else
         return Get_Children
                (  ASN1.ASN1_Data_Item'Class (Item.Target.all)
                );
      end if;
   end Get_Children;

   function Get_Container
            (  Item : Any_Data_Item
            )  return External_String_Buffer_Ptr is
   begin
      return Item.Container;
   end Get_Container;

   function Get_Tag (Item : Any_Data_Item) return Tag_Type is
   begin
      if Item.Container = null or else Item.Target = null then
         Uninitialized (Item);
      end if;
      return Item.Tag;
   end Get_Tag;

   function Is_Implicit (Item : Any_Data_Item) return Boolean is
   begin
      if Item.Container = null or else Item.Target = null then
         Raise_Exception (Data_Error'Identity, Unset);
      else
         return Is_Implicit (Item.Target.all);
      end if;
   end Is_Implicit;

   procedure Register_Allocator (Item : in out Any_Data_Item) is
   begin
      if not Item.Registered then
         Item.Registered := True;
         Item.Notifier.Previous := Item.Container.Allocators;
         Item.Container.Allocators := Item.Notifier'Unchecked_Access;
      end if;
   end Register_Allocator;

   function Self
            (  Item : Any_Data_Item'Class
            )  return Any_Data_Item_Ptr is
      package From_Set_Address is
         new System.Address_To_Access_Conversions
             (  Any_Data_Item'Class
             );
      use From_Set_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

end GNAT.Sockets.Connection_State_Machine.ASN1.Objects;
