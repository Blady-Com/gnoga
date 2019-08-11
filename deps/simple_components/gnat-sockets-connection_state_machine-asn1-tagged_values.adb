--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Tagged_Values                              Spring, 2019       --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Ada.Tags;               use Ada.Tags;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with GNAT.Sockets.Connection_State_Machine.ASN1.Choices;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Choices;

with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values is

   Invalid_EOC    : constant String := "Invalid indefinite ASN.1 " &
                                       "sequence termination";
   Invalid_Length : constant String := "Invalid ASN.1 sequence length";
   Wrong_Class    : constant String := "Wrong ASN.1 value class";
   Type_Error     : constant String := "ASN.1 tag does not match " &
                                       "the object type";

   procedure Check (Item : Tagged_Data_Item) is
   begin
      if not Item.Initialized then
         Raise_Exception
         (  Use_Error'Identity,
            (  "ASN.1 sequence "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " was not properly initialized"
         )  );
      end if;
   end Check;

   function Always_Constructed (Item : Tagged_Data_Item)
      return Boolean is
   begin
      Check (Item);
      return Always_Constructed (Item.Value.all);
   end Always_Constructed;

   procedure Encode
             (  Item    : Tagged_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      Index : Stream_Element_Offset := Pointer;
   begin
      Check (Item);
      if Item.Tag.Class = Universal_Tag and then Item.Tag.Value = 0 then
         declare
            Selected : Abstract_ASN1_Data_Item'Class renames
                       Resolve_Selected (Item.Value.all).all;
         begin
            Put
            (  Data,
               Index,
               (  Universal_Tag,
                  Get_ASN1_Type (ASN1_Data_Item'Class (Selected)),
                  False
               ),
               Always_Constructed (Selected)
            );
         end;
      else
         Put
         (  Data,
            Index,
            Item.Tag,
            (  Always_Constructed (Item.Value.all)
            or else
               (  Item.Tag.Class /= Universal_Tag
               and then
                  not Is_Implicit (Item.Value.all)
         )  )  );
      end if;
      Definite_Encode (Item.Value.all, Data, Index);
      Pointer := Index;
   end Encode;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Tagged_Data_Item
             )  is
   begin
      Check_Initialization_Stream (Item, Stream.all);
      declare
         Count : Data_Item_Offset := 0;
         This  : Initialization_Stream'Class renames
                 Initialization_Stream'Class (Stream.all);
      begin
         if not Item.Initialized then
            declare
               Counter : aliased Initialization_Stream;
               Object  : Tagged_Data_Item'Class renames
                         Self (Item).all;
            begin
               if This.Parent = null then
                  Counter.Parent := This.Shared'Access;
               else
                  Counter.Parent := This.Parent;
               end if;
               Object.Initialized := True;
               Tagged_Data_Item'Class'Write
               (  Counter'Access,
                  Tagged_Data_Item'Class (Item)
               );
               if Counter.Count <= 1 then
                  Raise_Exception
                  (  Use_Error'Identity,
                     (  "ASN.1 tagged value "
                     &  Expanded_Name
                        (  Tagged_Data_Item'Class (Item)'Tag
                        )
                     &  " contains no values"
                  )  );
               elsif Counter.Count > 2 then
                  Raise_Exception
                  (  Use_Error'Identity,
                     (  "ASN.1 tagged value "
                     &  Expanded_Name
                        (  Tagged_Data_Item'Class (Item)'Tag
                        )
                     &  " contains more than one value"
                  )  );
               elsif Counter.Data.Vector (2).all not in
                     Abstract_ASN1_Data_Item'Class then
                  Raise_Exception
                  (  Use_Error'Identity,
                     (  "ASN.1 tagged value "
                     &  Expanded_Name
                        (  Counter.Data.Vector (2)'Tag
                        )
                     &  " is not an ASN.1 object"
                  )  );
               end if;
               Object.Value :=
                  Abstract_ASN1_Data_Item'Class
                  (  Counter.Data.Vector (2).all
                  ) 'Unchecked_Access;
               Count := Data_Item_Offset (Get_Size (Item));
               Initialized (Object);
            end;
         end if;
         Add (This, Item, Count);
      end;
   end Enumerate;

   procedure Feed
             (  Item    : in out Tagged_Data_Item;
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
            Call
            (  Item    => Item.Selected,
               Callee  => Item.Value.all,
               Tag     => Item.Tag,
               Pointer => Pointer,
               Client  => Client,
               State   => State
            );
            State := 0;
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
               Call
               (  Item    => Item.Selected,
                  Callee  => Item.Value.all,
                  Tag     => Item.Tag,
                  Pointer => Pointer,
                  Client  => Client,
                  State   => State
               );
               State := 0;
               return;
            end if;
         exception
            when Constraint_Error =>
               Raise_Exception (Data_Error'Identity, Tag_Too_Large);
         end;
         exit when Pointer > Data'Last;
      end loop;
   end Feed;

   function Get_Size (Item : Tagged_Data_Item) return Natural is
   begin
      Check (Item);
      return Get_Size (Item.Value.all);
   end Get_Size;

   procedure Initialized (Item : in out Tagged_Data_Item) is
   begin
      null;
   end Initialized;

   function Is_Implicit (Item : Tagged_Data_Item) return Boolean is
   begin
      return False;
   end Is_Implicit;

   function Self (Item : Tagged_Data_Item'Class)
      return Tagged_Data_Item_Ptr is
      package From_Sequence_Address is
         new System.Address_To_Access_Conversions
             (  Tagged_Data_Item'Class
             );
      use From_Sequence_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

end GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values;
