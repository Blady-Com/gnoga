--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Sets.Implicit                                   Summer, 2019       --
--  Implementation                                                    --
--                                Last revision :  10:13 29 Nov 2020  --
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

with GNAT.Sockets.Connection_State_Machine.ASN1.Choices;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Choices;

with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Implicit is

   function Always_Constructed
            (  Item : Implicit_Set_Data_Item
            )  return Boolean is
   begin
      return True;
   end Always_Constructed;

   function Always_Constructed
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return Boolean is
   begin
      return True;
   end Always_Constructed;

   procedure Append
             (  Item : in out Implicit_External_Set_Of_Data_Item
             )  is
   begin
      if Item.Container = null then
         Uninitialized (Item);
      end if;
      declare
         Stream  : aliased Initialization_Stream;
         Element : constant Abstract_ASN1_Data_Item_Ptr :=
                            Create
                            (  Implicit_External_Set_Of_Data_Item'Class
                               (  Item
                               ) 'Unchecked_Access
                            );
      begin
         Register_Allocator (Item);
         Stream.Shared := Item.Container.all'Unchecked_Access;
         Abstract_ASN1_Data_Item'Class'Write
         (  Stream'Access,
            Element.all
         );
         Put (Item.Map, Item.Map.Length + 1, Element);
         Item.Map.Length := Item.Map.Length + 1;
      end;
   end Append;

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
             (  Item    : Implicit_Set_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      elsif Data'Last - Pointer < 0 then
         Raise_Exception (End_Error'Identity, No_Room);
      elsif not Item.Initialized then
         Uninitialized (Item);
      end if;
      declare
         Index : Stream_Element_Offset := Pointer;
      begin
         for Position in Item.List'Range loop
            declare
               Tag        : constant Tag_Type := Item.List (Position);
               Descriptor : constant Element_Descriptor :=
                                     Get (Item.Map, Tag);
               Value      : Abstract_ASN1_Data_Item'Class renames
                            Abstract_ASN1_Data_Item'Class
                            (  Descriptor.Item.all
                            );
            begin
               if not Descriptor.Unset then
                  if (  Tag.Class = Universal_Tag
                     and then
                        Tag.Value = 0
                     )  then
                     declare
                        Selected : Abstract_ASN1_Data_Item'Class renames
                                   Resolve_Selected (Value).all;
                     begin
                        Put
                        (  Data,
                           Index,
                           (  Universal_Tag,
                              Get_ASN1_Type
                              (  ASN1_Data_Item'Class (Selected)
                              ),
                              False
                           ),
                           Always_Constructed (Selected)
                        );
                     end;
                  else
                     Put
                     (  Data,
                        Index,
                        Tag,
                        (  Always_Constructed (Value)
                        or else
                           (  Tag.Class /= Universal_Tag
                           and then
                              not Is_Implicit (Value)
                     )  )  );
                  end if;
                  Definite_Encode (Value, Data, Index);
               end if;
            end;
         end loop;
         Pointer := Index;
      end;
   end Encode;

   procedure Encode
             (  Item    : Implicit_External_Set_Of_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      Index : Stream_Element_Offset := Pointer;
   begin
      for Position in 1..Item.Map.Length loop
         Encode (Get (Item.Map, Position).all, Data, Index);
      end loop;
      Pointer := Index;
   end Encode;

   procedure End_Of_Subsequence
             (  Item    : in out Implicit_Set_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if Item.Definite then
         if Item.Total_Length = Get_Length (Item.Current) then
            if (  (  Item.Elements_Count -- Only optionals left
                  +  Item.Optional_Count
                  )
               >= Item.List'Length
               )  then
               State := 0;
               return;
            end if;
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         elsif Item.Total_Length < Get_Length (Item.Current) then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
         Item.Total_Length :=
            Item.Total_Length - Get_Length (Item.Current);
      end if;
   end End_Of_Subsequence;

   procedure End_Of_Subsequence
             (  Item    : in out Implicit_External_Set_Of_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      Count : constant Stream_Element_Offset :=
              Stream_Element_Offset (Client.Fed - Item.Fed_Count);
   begin
      if Item.Definite then
         if Item.Total_Length = Count then
            State := 0;
            return;
         elsif Item.Total_Length < Count then
            Raise_Exception (Data_Error'Identity, Invalid_Length);
         end if;
         Item.Total_Length := Item.Total_Length - Count;
      end if;
   end End_Of_Subsequence;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Set_Data_Item
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
               Object  : Implicit_Set_Data_Item'Class
                         renames Self (Item).all;
            begin
               if This.Parent = null then
                  Counter.Parent := This.Shared'Access;
               else
                  Counter.Parent := This.Parent;
               end if;
               Object.Initialized := True;
               Object.Elements_Count := 0;
               Implicit_Set_Data_Item'Class'Write
               (  Counter'Access,
                  Object
               );
               if Counter.Count <= 1 then
                  Raise_Exception
                  (  Use_Error'Identity,
                     (  "ASN.1 set "
                     &  Expanded_Name (Object'Tag)
                     &  " is empty (contains no items)"
                  )  );
               end if;
               Object.List :=
                  new Tag_Type_Array
                      (  1
                      .. Natural (Counter.Count - 1)
                      );
               for Index in 2..Counter.Count loop
                  declare
                     Position : constant Positive :=
                                         Positive (Index - 1);
                  begin
                     Check (Item, Counter.Data.Vector (Index).all);
                     Object.List (Position) :=
                        (  Class    => Context_Specific_Tag,
                           Value    => ASN1_Type (Position - 1),
                           Optional => False
                        );
                     Add
                     (  Object.Map,
                        Object.List (Position),
                        (  Unset    => True,
                           Position => Position,
                           Item     => Counter.Data.Vector (Index)
                     )  );
                  end;
               end loop;
               Count := Data_Item_Offset (Get_Size (Item)) - 1;
               Initialized (Object);
            end;
         end if;
         Add (This, Item, Count);
      end;
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_External_Set_Of_Data_Item
             )  is
      procedure Find
                (  Object : in out
                            Implicit_External_Set_Of_Data_Item'Class;
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
         Initialized (Self (Item).all);
      end;
   end Enumerate;

   procedure Feed_Initialize
             (  Item : in out Implicit_Set_Data_Item'Class
             )  is
   begin
      Item.Elements_Count := 0;
      Item.Optional_Count := 0;
      for Index in 1..Get_Size (Item.Map) loop
         declare
            Element : Element_Descriptor := Get (Item.Map, Index);
         begin
            Element.Unset := True;
            if Item.List (Index).Optional then
               Item.Optional_Count := Item.Optional_Count + 1;
            end if;
            Replace (Item.Map, Index, Element);
         end;
      end loop;
   end Feed_Initialize;

   Get_Element   : constant := 1;
   Get_Tag_Value : constant := 2;
   Select_By_Tag : constant := 3;
   First_Zero    : constant := 4;
   Second_Zero   : constant := 5;

   procedure Explicit_Feed
             (  Item    : in out Implicit_Set_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Check
         (  Data (Pointer),
            (1 => Get_ASN1_Type (Implicit_Set_Data_Item'Class (Item)))
         );
         Pointer := Pointer + 1;
         State   := Start_Length;
         Feed_Initialize (Item);
         if Pointer > Data'Last then
            return;
         end if;
      end if;
      if State < 0 then
         while not Is_Length_Ready (State) loop
            if Pointer > Data'Last then
               return;
            end if;
            Embedded_Feed (Data, Pointer, State);
         end loop;
         Item.Definite  := not Is_Indefinite (State);
         if Item.Definite then
            Item.Total_Length := Get_Length (State);
            if Item.Total_Length = 0 then
               if (  (  Item.Elements_Count -- Only optionals left
                     +  Item.Optional_Count
                     )
                  >= Item.List'Length
                  )  then
                  State := 0;
                  return;
               else
                  Raise_Exception
                  (  Data_Error'Identity,
                     Invalid_Length
                  );
               end if;
            end if;
         else
            Item.Total_Length := 0;
         end if;
         State := Get_Element;
      end if;
      Feed
      (  Item    => Item,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
   end Explicit_Feed;

   procedure Explicit_Feed
             (  Item    : in out Implicit_External_Set_Of_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         if Item.Container = null then
            Uninitialized (Item);
         end if;
         Check
         (  Data (Pointer),
            (  1 => Get_ASN1_Type
                    (  Implicit_External_Set_Of_Data_Item'Class (Item)
         )  )       );
         Item.Map.Length := 0;
         State   := Start_Length;
         Pointer := Pointer + 1;
         if Pointer > Data'Last then
            return;
         end if;
      end if;
      if State < 0 then
         while not Is_Length_Ready (State) loop
            if Pointer > Data'Last then
               return;
            end if;
            Embedded_Feed (Data, Pointer, State);
         end loop;
         Item.Definite := not Is_Indefinite (State);
         if Item.Definite then
            Item.Total_Length := Get_Length (State);
            if Item.Total_Length = 0 then
               State := 0;
               return;
            end if;
         else
            Item.Total_Length := 0;
         end if;
         State := Get_Element;
      end if;
      Feed
      (  Item    => Item,
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
   end Explicit_Feed;

   procedure Feed
             (  Item    : in out Implicit_Set_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      procedure Call_Item (Element : Element_Descriptor) is
      begin
         State := Get_Element;
         Call
         (  Item    => Item.Current,
            Callee  => Element.Item.all,
            Tag     => Item.Current.Tag,
            Pointer => Pointer,
            Client  => Client,
            State   => State
         );
         Item.Current.Position := Element.Position;
         State := 0;
      end Call_Item;

      procedure Begin_Tag_Header is
      begin
         Item.Elements_Count := Item.Elements_Count + 1;
         case ASN1_Tag (Data (Pointer)) and 2#1100_0000# is
            when Application_Class =>
               Item.Current.Tag.Class := Application_Tag;
            when Context_Specific_Class =>
               Item.Current.Tag.Class := Context_Specific_Tag;
            when Private_Class =>
               Item.Current.Tag.Class := Private_Tag;
            when others =>
               Item.Current.Tag.Class := Universal_Tag;
         end case;
         Item.Current.Tag.Value :=
            ASN1_Type (Data (Pointer) and 16#1F#);
         if Item.Current.Tag.Value < 31 then
            State := Select_By_Tag;
         else
            State := Get_Tag_Value;
         end if;
         Pointer := Pointer + 1;
         if Item.Definite then
            if Item.Total_Length = 0 then
               Raise_Exception (Data_Error'Identity, Invalid_Length);
            end if;
            Item.Total_Length := Item.Total_Length - 1;
         end if;
      end Begin_Tag_Header;
   begin
      if State = 0 then
         Feed_Initialize (Item);
         State := Start_Length;
      end if;
      while Pointer <= Data'Last loop
         case State is
            when Get_Element =>
               if Item.Elements_Count >= Item.List'Length then
                  if Item.Definite then
                     if Item.Total_Length /= 0 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           Invalid_Length
                        );
                     end if;
                     State := 0;
                     return;
                  else
                     State := First_Zero;
                  end if;
               elsif (  (  Item.Elements_Count -- Only optionals left
                        +  Item.Optional_Count
                        )
                     >= Item.List'Length
                     )  then
                  if Item.Definite then
                     if Item.Total_Length = 0 then
                        State := 0;
                        return;
                     end if;
                     if Data (Pointer) = 0 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           "ASN.1 set element tag is expected"
                        );
                     else
                        Begin_Tag_Header;
                     end if;
                  else
                     if Data (Pointer) = 0 then
                        Pointer := Pointer + 1;
                        State   := Second_Zero;
                     else
                        Begin_Tag_Header;
                     end if;
                  end if;
               else
                  if Data (Pointer) = 0 then
                     Pointer := Pointer + 1;
                     State   := Second_Zero;
                  else
                     Begin_Tag_Header;
                  end if;
               end if;
            when Get_Tag_Value =>
               while Pointer <= Data'Last loop
                  declare
                     This : constant ASN1_Type :=
                                     ASN1_Type (Data (Pointer));
                     Tag  : Tag_Type renames Item.Current.Tag;
                  begin
                     Pointer := Pointer + 1;
                     if This > 127 then
                        Tag.Value := Tag.Value * 128 + (This - 128);
                     else
                        Tag.Value := Tag.Value * 128 + This;
                        State     := Select_By_Tag;
                        exit;
                     end if;
                     if Item.Definite then
                        if Item.Total_Length = 0 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              Invalid_Length
                           );
                        end if;
                        Item.Total_Length := Item.Total_Length - 1;
                     end if;
                  exception
                     when Constraint_Error =>
                        Raise_Exception
                        (  Data_Error'Identity,
                           Tag_Too_Large
                        );
                  end;
               end loop;
            when Select_By_Tag =>
               declare
                  Element : Element_Descriptor;
                  Offset  : constant Integer :=
                                     Find (Item.Map, Item.Current.Tag);
               begin
                  if Offset <= 0 then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "ASN.1 unexpected set tag "
                        &  Image (Item.Current.Tag)
                     )  );
                  end if;
                  Element := Get (Item.Map, Offset);
                  if not Element.Unset then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "ASN.1 duplicated set element tagged by "
                        &  Image (Item.Current.Tag)
                     )  );
                  end if;
                  Element.Unset := False;
                  if Item.List (Element.Position).Optional then
                     Item.Optional_Count := Item.Optional_Count - 1;
                  end if;
                  Replace (Item.Map, Offset, Element);
                  Call_Item (Element);
                  return;
               end;
            when First_Zero =>
               if Data (Pointer) /= 0 then
                  Raise_Exception (Data_Error'Identity, Invalid_EOC);
               end if;
               State   := Second_Zero;
               Pointer := Pointer + 1;
            when others =>
               if Data (Pointer) /= 0 then
                  Raise_Exception (Data_Error'Identity, Invalid_EOC);
               end if;
               State   := 0;
               Pointer := Pointer + 1;
               return;
         end case;
      end loop;
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_External_Set_Of_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      procedure Call_Item is
         List    : Sequence renames Item.Current;
         Stream  : aliased Initialization_Stream;
         Element : constant Abstract_ASN1_Data_Item_Ptr :=
                            Create
                            (  Implicit_External_Set_Of_Data_Item'Class
                               (  Item
                               ) 'Unchecked_Access
                            );
      begin
         Register_Allocator (Item);
         Stream.Shared := Item.Container.all'Unchecked_Access;
         Abstract_ASN1_Data_Item'Class'Write
         (  Stream'Access,
            Element.all
         );
         Put (Item.Map, Item.Map.Length + 1, Element);
         Item.Map.Length := Item.Map.Length + 1;
         List.Caller   := null;
         List.State    := 0;
         List.Current  := 1;
         List.List (1) := Element.all'Unchecked_Access;
         Call (Client, Pointer, List'Unchecked_Access, Get_Element);
         Item.Fed_Count := Client.Fed;
         State := 0;
      end Call_Item;
   begin
      if State = 0 then
         if Item.Container = null then
            Uninitialized (Item);
         end if;
         Item.Map.Length := 0;
         State := Get_Element;
      end if;
      while Pointer <= Data'Last loop
         case State is
            when Get_Element =>
               if Item.Definite then
                  if Item.Total_Length = 0 then
                     State := 0;
                     return;
                  end if;
                  if Data (Pointer) = 0 then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "ASN.1 "
                        &  Image (Get_ASN1_Type (Item))
                        &  " element tag is expected"
                     )  );
                  else
                     Call_Item;
                     return;
                  end if;
               else
                  if Data (Pointer) = 0 then
                     Pointer := Pointer + 1;
                     State   := Second_Zero;
                  else
                     Call_Item;
                     return;
                  end if;
               end if;
            when First_Zero =>
               if Data (Pointer) /= 0 then
                  Raise_Exception (Data_Error'Identity, Invalid_EOC);
               end if;
               State   := Second_Zero;
               Pointer := Pointer + 1;
            when others =>
               if Data (Pointer) /= 0 then
                  Raise_Exception (Data_Error'Identity, Invalid_EOC);
               end if;
               State   := 0;
               Pointer := Pointer + 1;
               return;
         end case;
      end loop;
   end Feed;

   procedure Finalize
             (  Item : in out Implicit_Set_Data_Item
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Tag_Type_Array,
                Tag_Type_Array_Ptr
             );
   begin
      Free (Item.List);
      Finalize (ASN1_Data_Item (Item));
   end Finalize;

   procedure Finalize
             (  Item : in out Implicit_External_Set_Of_Data_Item
             )  is
   begin
      Freed (Item);
      Finalize (ASN1_Data_Item (Item));
   end Finalize;

   procedure Freed (Item : in out Implicit_External_Set_Of_Data_Item) is
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
            for Index in 1..Item.Map.Length loop
               This := Convert (Get (Item.Map, Index));
               Free (This);
            end loop;
         end;
      end if;
      Item.Map.Length := 0;
   end Freed;

   function Get
            (  Item  : Implicit_External_Set_Of_Data_Item;
               Index : Positive
            )  return Abstract_ASN1_Data_Item_Ptr is
   begin
      if Item.Container = null then
         Uninitialized (Item);
      elsif Index > Item.Map.Length then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      end if;
      return Get (Item.Map, Index);
   end Get;

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
            (  Item : Implicit_Set_Data_Item
            )  return ASN1_Type is
   begin
      return ASN1.Set_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return ASN1_Type is
   begin
      return ASN1.Set_Tag;
   end Get_ASN1_Type;

   function Get_Children
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return Data_Item_Ptr_Array is
   begin
      if Item.Container = null then
         Uninitialized (Item);
      end if;
      declare
         Result : Data_Item_Ptr_Array
                  (  1
                  .. Data_Item_Offset (Item.Map.Length)
                  );
      begin
         for Index in Result'Range loop
            Result (Index) :=
               Get (Item.Map, Integer (Index)).all'Unchecked_Access;
         end loop;
         return Result;
      end;
   end Get_Children;

   function Get_Children
            (  Item : Implicit_Set_Data_Item
            )  return Data_Item_Ptr_Array is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      declare
         Result : Data_Item_Ptr_Array (1..Item.List'Length);
      begin
         for Index in Result'Range loop
            Result (Index) := Get (Item.Map, Integer (Index)).Item;
         end loop;
         return Result;
      end;
   end Get_Children;

   function Get_Container
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return External_String_Buffer_Ptr is
   begin
      return Item.Container;
   end Get_Container;

   function Get_Length
            (  Item : Implicit_Set_Data_Item
            )  return Natural is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      return Item.List'Length;
   end Get_Length;

   function Get_Length
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return Natural is
   begin
      return Item.Map.Length;
   end Get_Length;

   function Get_Size
            (  Item : Implicit_Set_Data_Item
            )  return Natural is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      declare
         Result : Natural := 1;
      begin
         for Index in 1..Get_Size (Item.Map) loop
            Result :=
               Result + Get_Size (Get (Item.Map, Index).Item.all);
         end loop;
         return Result;
      end;
   end Get_Size;

   function Get_Size
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return Natural is
      Result : Natural := 1;
   begin
      if Item.Container /= null then
         for Index in 1..Get_Size (Item.Container.all) loop
            Result := Result + Item.Map.Length;
         end loop;
      end if;
      return Result;
   end Get_Size;

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

   function Get_Tag
            (  Item  : Implicit_Set_Data_Item;
               Index : Positive
            )  return Tag_Type is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      if Index > Item.List'Last then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      else
         return Item.List (Index);
      end if;
   end Get_Tag;

   procedure Initialized (Item : in out Implicit_Set_Data_Item) is
   begin
      null;
   end Initialized;

   procedure Initialized
             (  Item : in out Implicit_External_Set_Of_Data_Item
             )  is
   begin
      null;
   end Initialized;

   function Is_Implicit
            (  Item : Implicit_Set_Data_Item
            )  return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit
            (  Item : Implicit_External_Set_Of_Data_Item
            )  return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Set
            (  Item  : Implicit_Set_Data_Item;
               Index : Positive
            )  return Boolean is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      if Index > Item.List'Last then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      end if;
      return not Get (Item.Map, Item.List (Index)).Unset;
   end Is_Set;

   function Is_Untagged
            (  Item  : Implicit_Set_Data_Item;
               Index : Positive
            )  return Boolean is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      if Index > Item.List'Last then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      end if;
      return False;
   end Is_Untagged;

   procedure Register_Allocator
             (  Item : in out Implicit_External_Set_Of_Data_Item
             )  is
   begin
      if not Item.Registered then
         Item.Registered := True;
         Item.Notifier.Previous := Item.Container.Allocators;
         Item.Container.Allocators := Item.Notifier'Unchecked_Access;
      end if;
   end Register_Allocator;

   procedure Reset
             (  Item  : in out Implicit_Set_Data_Item;
                Index : Positive;
                Unset : Boolean
             )  is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      elsif Index > Get_Length (Item) then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      else
         declare
            Location : constant Tag_Type  := Item.List (Index);
            This     : Element_Descriptor := Get (Item.Map, Location);
         begin
            This.Unset := Unset;
            Replace (Item.Map, Location, This);
         end;
      end if;
   end Reset;

   function Self
            (  Item : Implicit_Set_Data_Item'Class
            )  return Implicit_Set_Data_Item_Ptr is
      package From_Set_Address is
         new System.Address_To_Access_Conversions
             (  Implicit_Set_Data_Item'Class
             );
      use From_Set_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   function Self
            (  Item : Implicit_External_Set_Of_Data_Item'Class
            )  return Implicit_External_Set_Of_Data_Item_Ptr is
      package From_Set_Address is
         new System.Address_To_Access_Conversions
             (  Implicit_External_Set_Of_Data_Item'Class
             );
      use From_Set_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Set_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length >= 0 then
         Item.Total_Length := Length;
         Item.Definite     := True;
      else
         Item.Total_Length := 0;
         Item.Definite     := False;
      end if;
   end Set_Implicit_Tag;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_External_Set_Of_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length >= 0 then
         Item.Total_Length := Length;
         Item.Definite     := True;
      else
         Item.Total_Length := 0;
         Item.Definite     := False;
      end if;
   end Set_Implicit_Tag;

   procedure Set_Optional
             (  Item     : in out Implicit_Set_Data_Item;
                Index    : Positive;
                Optional : Boolean
             )  is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      if Index > Item.List'Last then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      end if;
      declare
         Element : Element_Descriptor;
         Offset  : Integer;
         Tag     : Tag_Type renames Item.List (Index);
      begin
         Tag.Optional := Optional;
         Offset  := Find (Item.Map, Tag);
         Element := Get (Item.Map, Offset);
         Element.Unset := Optional;
         Replace (Item.Map, Offset, Element);
      end;
   end Set_Optional;

   procedure Set_Tag
             (  Item  : in out Implicit_Set_Data_Item;
                Index : Positive;
                Tag   : Tag_Type;
                Unset : Boolean := False
             )  is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      if Index > Item.List'Last then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      end if;
      declare
         Element : Element_Descriptor;
         Offset  : Integer := Find (Item.Map, Tag);
      begin
         if Offset > 0 then
            Element := Get (Item.Map, Offset);
            if Element.Position /= Index then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "ASN.1 set already contains element tagged by "
                  &  Image (Tag)
               )  );
            end if;
            Element.Unset := Unset;
            Replace (Item.Map, Offset, Element);
         else
            Offset  := Find (Item.Map, Item.List (Index));
            Element := Get (Item.Map, Offset);
            if Tag.Class = Universal_Tag then
               declare
                  Object : Abstract_ASN1_Data_Item'Class renames
                           Abstract_ASN1_Data_Item'Class
                           (  Element.Item.all
                           );
               begin
                  if not Is_Implicit (Object) then
                     Raise_Exception
                     (  Mode_Error'Identity,
                        "Universally tagged object is not implicit"
                     );
                  elsif Object not in ASN1_Data_Item'Class then
                     Raise_Exception
                     (  Mode_Error'Identity,
                        "Universally tagged object has no definite type"
                     );
                  elsif (  Tag.Value
                        /= Get_ASN1_Type (ASN1_Data_Item'Class (Object))
                        )  then
                     Raise_Exception
                     (  Mode_Error'Identity,
                        (  "The universal tag value differs "
                        &  "from the object type"
                     )  );
                  end if;
               end;
            end if;
            Element.Unset := Unset;
            Remove (Item.Map, Offset);
            Add (Item.Map, Tag, Element);
            Item.List (Index) := Tag;
         end if;
      end;
   end Set_Tag;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Unbounded_Element_Array
             )  is
   begin
      null;
   end Write;

end GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Implicit;
