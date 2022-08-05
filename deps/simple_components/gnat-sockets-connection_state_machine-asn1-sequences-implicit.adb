--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Sequences.Implicit                     Spring, 2019       --
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

package body GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.
             Implicit is

   Invalid_EOC : constant String := "Invalid indefinite ASN.1 " &
                                    "sequence termination";
   Untagged    : constant String := "An untagged sequence element " &
                                    "cannot be optional";
   Get_Element   : constant := 1;
   Get_Tag_Value : constant := 2;
   Select_By_Tag : constant := 3;
   First_Zero    : constant := 4;
   Second_Zero   : constant := 5;

   function Always_Constructed
            (  Item : Implicit_Sequence_Data_Item
            )  return Boolean is
   begin
      return True;
   end Always_Constructed;

   function Always_Constructed
            (  Item : Implicit_Tagged_Sequence_Data_Item
            )  return Boolean is
   begin
      return True;
   end Always_Constructed;

   procedure Check (Item : Data_Item'Class) is
   begin
      if Item not in Abstract_ASN1_Data_Item'Class then
         Raise_Exception
         (  Use_Error'Identity,
            (  "ASN.1 sequence contains a non ASN.1 object "
            &  Expanded_Name (Item'Tag)
         )  );
      end if;
   end Check;

   procedure Uninitialized (Item : ASN1_Data_Item'Class) is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         (  "ASN.1 sequence "
         &  Expanded_Name (Item'Tag)
         &  " was not properly initialized"
      )  );
   end Uninitialized;

   procedure Encode
             (  Item    : Implicit_Sequence_Data_Item;
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
      elsif not Item.Initialized or else Item.Data = null then
         Uninitialized (Item);
      end if;
      declare
         Index : Stream_Element_Offset := Pointer;
      begin
         for No in Item.Data.List'Range loop
            declare
               Element : Abstract_ASN1_Data_Item'Class renames
                         Abstract_ASN1_Data_Item'Class
                         (  Item.Data.List (No).all
                         );
            begin
               Encode (Element, Data, Index);
            end;
         end loop;
         Pointer := Index;
      end;
   end Encode;

   procedure Encode
             (  Item    : Implicit_Sequence_Of_Data_Item;
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
      elsif not Item.Initialized or else Item.List = null then
         Uninitialized (Item);
      end if;
      declare
         Index : Stream_Element_Offset := Pointer;
      begin
         for Element in 1..Item.Length loop
            Encode
            (  Abstract_ASN1_Data_Item'Class
               (  Item.List
                  (  Item.List'First
                  +  Data_Item_Offset (Element)
                  -  1
                  ) .all
               ),
               Data,
               Index
            );
         end loop;
         Pointer := Index;
      end;
   end Encode;

   procedure Encode
             (  Item    : Implicit_Tagged_Sequence_Data_Item;
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
      elsif not Item.Initialized or else Item.List = null then
         Uninitialized (Item);
      end if;
      declare
         Index : Stream_Element_Offset := Pointer;
      begin
         for No in Item.List'Range loop
            declare
               This    : Tagged_Data_Item renames Item.List (No);
               Element : Abstract_ASN1_Data_Item'Class renames
                         Abstract_ASN1_Data_Item'Class (This.Item.all);
            begin
               if not This.Unset then
                  if This.Untagged then
                     Encode (Element, Data, Index);
                  else
                     if (  This.Tag.Class = Universal_Tag
                        and then
                           This.Tag.Value = 0
                        )  then
                        declare
                           Selected : Abstract_ASN1_Data_Item'Class
                              renames Resolve_Selected (Element).all;
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
                           This.Tag,
                           (  Always_Constructed (Element)
                           or else
                              (  This.Tag.Class /= Universal_Tag
                              and then
                                 not Is_Implicit (Element)
                        )  )  );
                     end if;
                     Definite_Encode (Element, Data, Index);
                  end if;
               end if;
            end;
         end loop;
         Pointer := Index;
      end;
   end Encode;

   procedure End_Of_Subsequence
             (  Item    : in out Implicit_Sequence_Of_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Item.Length := Item.Length + 1;
      if Item.Definite then
         declare
            Count : constant Unsigned_64 := Client.Fed - Item.Fed_Count;
         begin
            if Unsigned_64 (Item.Total_Length) = Count then
               State := 0;
            elsif Unsigned_64 (Item.Total_Length) < Count then
               Raise_Exception (Data_Error'Identity, Invalid_Length);
            end if;
            Item.Total_Length :=
               Item.Total_Length - Stream_Element_Offset (Count);
         end;
      end if;
   end End_Of_Subsequence;

   procedure End_Of_Subsequence
             (  Item    : in out Implicit_Tagged_Sequence_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if Item.Definite then
         if Item.Total_Length = Get_Length (Item.Current) then
            while (  Item.Current.Index < Item.List'Last
                  and then
                     Item.List (Item.Current.Index + 1).Tag.Optional
                  )  loop
               Item.Current.Index := Item.Current.Index + 1;
            end loop;
            if Item.Current.Index = Item.List'Last then
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

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Sequence_Data_Item
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
               Object  : Implicit_Sequence_Data_Item'Class
                         renames Self (Item).all;
            begin
               if This.Parent = null then
                  Counter.Parent := This.Shared'Access;
               else
                  Counter.Parent := This.Parent;
               end if;
               Object.Initialized := True;
               Data_Item'Class'Write
               (  Counter'Access,
                  Data_Item'Class (Item)
               );
               if Counter.Count <= 1 then
                  Raise_Exception
                  (  Use_Error'Identity,
                     (  "ASN.1 sequence "
                     &  Expanded_Name (Object'Tag)
                     &  " is empty (contains no items)"
                  )  );
               end if;
               for Element in 2..Counter.Count loop
                  Check (Counter.Data.Vector (Element).all);
               end loop;
               Object.Data :=
                  new Sequence'
                      (  Length  => Counter.Count - 1,
                         Caller  => null,
                         State   => 0,
                         Current => 1,
                         List    => Counter.Data.Vector
                                    (  2
                                    .. Counter.Count
                      )             );
               Count := Data_Item_Offset (Get_Size (Item)) - 1;
               Initialized (Object);
            end;
         end if;
         Add (This, Item, Count);
      end;
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Sequence_Of_Data_Item
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
               Object  : Implicit_Sequence_Of_Data_Item'Class
                         renames Self (Item).all;
            begin
               if This.Parent = null then
                  Counter.Parent := This.Shared'Access;
               else
                  Counter.Parent := This.Parent;
               end if;
               Object.Initialized := True;
               Data_Item'Class'Write
               (  Counter'Access,
                  Data_Item'Class (Item)
               );
               if Counter.Count <= 1 then
                  Raise_Exception
                  (  Use_Error'Identity,
                     (  "ASN.1 sequence "
                     &  Expanded_Name (Object'Tag)
                     &  " contains no values"
                  )  );
               elsif Counter.Count > 2 then
                  Check (Counter.Data.Vector (2).all);
                  for Index in 3..Counter.Count loop
                     if (  Counter.Data.Vector (2).all'Tag
                        /= Counter.Data.Vector (Index).all'Tag
                        )  then
                        Raise_Exception
                        (  Use_Error'Identity,
                           (  "ASN.1 sequence "
                           &  Expanded_Name (Object'Tag)
                           &  " contains differently typed values"
                        )  );
                     end if;
                  end loop;
               end if;
               Object.List :=
                  new Data_Item_Ptr_Array'
                      (  Counter.Data.Vector (2..Counter.Count)
                      );
               Count := Data_Item_Offset (Get_Size (Item)) - 1;
               Initialized (Object);
            end;
         end if;
         Add (This, Item, Count);
      end;
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Tagged_Sequence_Data_Item
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
               Object  : Implicit_Tagged_Sequence_Data_Item'Class
                         renames Self (Item).all;
            begin
               if This.Parent = null then
                  Counter.Parent := This.Shared'Access;
               else
                  Counter.Parent := This.Parent;
               end if;
               Object.Initialized := True;
               Data_Item'Class'Write
               (  Counter'Access,
                  Data_Item'Class (Item)
               );
               if Counter.Count <= 1 then
                  Raise_Exception
                  (  Use_Error'Identity,
                     (  "ASN.1 sequence "
                     &  Expanded_Name (Object'Tag)
                     &  " is empty (contains no items)"
                  )  );
               end if;
               Object.List :=
                  new Tagged_Data_Item_Array
                      (  1
                      .. Integer (Counter.Count - 1)
                      );
               for Index in Object.List'Range loop
                  Check
                  (  Counter.Data.Vector
                     (  Data_Item_Offset (Index) + 1
                     ) .all
                  );
                  Object.List (Index).Tag :=
                     (  Context_Specific_Tag,
                        ASN1_Type (Index - 1),
                        False
                     );
                  Object.List (Index).Item :=
                     Counter.Data.Vector
                     (  Data_Item_Offset (Index) + 1
                     );
               end loop;
               Count := Data_Item_Offset (Get_Size (Item)) - 1;
               Initialized (Object);
            end;
         end if;
         Add (This, Item, Count);
      end;
   end Enumerate;

   procedure Explicit_Feed
             (  Item    : in out Implicit_Sequence_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Check
         (  Data (Pointer),
            (  1 => Get_ASN1_Type
                    (  Implicit_Sequence_Data_Item'Class (Item)
         )  )       );
         Pointer := Pointer + 1;
         State   := Start_Length;
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
         if Is_Indefinite (State) then
            Call (Client, Pointer, Item.Data, 1);
         else
            Call (Client, Pointer, Item.Data);
         end if;
         State := 0;
         return;
      end if;
      if State = 1 then
         if Data (Pointer) /= 0 then
            Raise_Exception (Data_Error'Identity, Invalid_EOC);
         end if;
         State   := 2;
         Pointer := Pointer + 1;
         if Pointer > Data'Last then
            return;
         end if;
      end if;
      if Data (Pointer) /= 0 then
         Raise_Exception (Data_Error'Identity, Invalid_EOC);
      end if;
      State   := 0;
      Pointer := Pointer + 1;
   end Explicit_Feed;

   procedure Explicit_Feed
             (  Item    : in out Implicit_Sequence_Of_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Check
         (  Data (Pointer),
            (  1 => Get_ASN1_Type
                    (  Implicit_Sequence_Of_Data_Item'Class (Item)
         )  )       );
         State       := Start_Length;
         Pointer     := Pointer + 1;
         Item.Length := 0;
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
         Item.Fed_Count := 0;
         Item.Definite  := not Is_Indefinite (State);
         if Item.List'Length > 0 then
            if Item.Definite then
               Item.Total_Length := Get_Length (State);
               if Item.Total_Length = 0 then
                  State := 0;
               else
                  Next_Item (Item, Pointer, Client, State);
               end if;
               return;
            else
               Item.Total_Length := 0;
               State := 1;
            end if;
         elsif Item.Definite then -- Definite empty sequence
            if Get_Length (State) /= 0 then
               Raise_Exception (Data_Error'Identity, Invalid_Length);
            end if;
            State := 0;
            return;
         else
            State := 2; -- Expecting terminating sequence
         end if;
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
             (  Item    : in out Implicit_Tagged_Sequence_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         for Index in Item.List'Range loop
            Item.List (Index).Unset := True;
         end loop;
         Check
         (  Data (Pointer),
            (  1 => Get_ASN1_Type
                    (  Implicit_Tagged_Sequence_Data_Item'Class (Item)
         )  )       );
         Pointer := Pointer + 1;
         State   := Start_Length;
         Item.Current.Index := 0;
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
             (  Item    : in out Implicit_Sequence_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Call (Client, Pointer, Item.Data);
         State := 0;
         return;
      end if;
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_Sequence_Of_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Item.Length := 0;
         if Item.Total_Length = 0 then
            State := 0;
         else
            Next_Item (Item, Pointer, Client, State);
         end if;
         return;
      end if;
      while Pointer <= Data'Last loop
         if State = 1 then
            if Item.Definite or else Data (Pointer) /= 0 then
               Next_Item (Item, Pointer, Client, State);
               return;
            end if;
            Pointer := Pointer + 1;
            State   := 3;
         elsif State = 2 then
            if Data (Pointer) /= 0 then
               Raise_Exception (Data_Error'Identity, Invalid_EOC);
            end if;
            State   := 3;
            Pointer := Pointer + 1;
         else
            if Data (Pointer) /= 0 then
               Raise_Exception (Data_Error'Identity, Invalid_EOC);
            end if;
            State   := 0;
            Pointer := Pointer + 1;
            return;
         end if;
      end loop;
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_Tagged_Sequence_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      procedure Call_Item (This : in out Tagged_Data_Item) is
      begin
         This.Unset := False;
         Set_Untagged (Item.Current, This.Untagged);
         State := Get_Element;
         Call
         (  Item    => Item.Current,
            Callee  => This.Item.all,
            Tag     => Item.Current.Tag,
            Pointer => Pointer,
            Client  => Client,
            State   => State
         );
      end Call_Item;
   begin
      if State = 0 then
         for Index in Item.List'Range loop
            Item.List (Index).Unset := True;
         end loop;
         State := Get_Element;
         Item.Current.Index := 0;
      end if;
      while Pointer <= Data'Last loop
         case State is
            when Get_Element =>
               if Item.Current.Index >= Item.List'Last then
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
               else
                  Item.Current.Index := Item.Current.Index + 1;
                  if Item.List (Item.Current.Index).Untagged then
                     State := Select_By_Tag;
                  else
                     Item.Current.Tag.Value :=
                        ASN1_Type (Data (Pointer) and 16#1F#);
                     case ASN1_Tag (Data (Pointer) and 2#1100_0000#) is
                        when Application_Class =>
                           Item.Current.Tag.Class := Application_Tag;
                        when Context_Specific_Class =>
                           Item.Current.Tag.Class :=
                              Context_Specific_Tag;
                        when Private_Class =>
                           Item.Current.Tag.Class := Private_Tag;
                        when others =>
                           Item.Current.Tag.Class := Universal_Tag;
                     end case;
                     if Item.Current.Tag.Value < 31 then
                        State := Select_By_Tag;
                     else
                        State := Get_Tag_Value;
                     end if;
                     Pointer := Pointer + 1;
                     if Item.Definite then
                        if Item.Total_Length = 0 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              Invalid_Length
                           );
                        end if;
                        Item.Total_Length := Item.Total_Length - 1;
                     end if;
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
               while Item.Current.Index <= Item.List'Last loop
                  declare
                     This : Tagged_Data_Item renames
                            Item.List (Item.Current.Index);
                  begin
                     if (  This.Untagged
                        or else
                           Item.Current.Tag = This.Tag
                        or else
                           (  This.Universal
                           and then
                              Item.Current.Tag.Class = Universal_Tag
                           )
                        or else
                           This.Item.all in Choice_Data_Item'Class
                        )   then
                        Call_Item (This);
                        return;
                     elsif This.Tag.Optional then
                        Item.Current.Index := Item.Current.Index + 1;
                     else
                        Raise_Exception
                        (  Data_Error'Identity,
                           (  "ASN.1 sequence tag "
                           &  Image (Item.Current.Tag)
                           &  " does not match expected "
                           &  Image (This.Tag)
                        )  );
                     end if;
                  end;
               end loop;
               Raise_Exception
               (  Data_Error'Identity,
                  (  "ASN.1 unexpected sequence tag "
                  &  Image (Item.Current.Tag)
               )  );
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

   procedure Finalize (Item : in out Implicit_Sequence_Data_Item) is
   begin
      Free (Item.Data);
   end Finalize;

   procedure Finalize (Item : in out Implicit_Sequence_Of_Data_Item) is
   begin
      Free (Item.List);
   end Finalize;

   procedure Finalize
             (  Item : in out Implicit_Tagged_Sequence_Data_Item
             )  is
   begin
      Free (Item.List);
   end Finalize;

   function Get_ASN1_Type
            (  Item : Implicit_External_Sequence_Of_Data_Item
            )  return ASN1_Type is
   begin
      return Sequence_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Implicit_Sequence_Data_Item
            )  return ASN1_Type is
   begin
      return Sequence_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : Implicit_Tagged_Sequence_Data_Item
            )  return ASN1_Type is
   begin
      return Sequence_Tag;
   end Get_ASN1_Type;

   function Get_Children
            (  Item : Implicit_Sequence_Data_Item
            )  return Data_Item_Ptr_Array is
   begin
      if not Item.Initialized or else Item.Data = null then
         Uninitialized (Item);
      end if;
      return Item.Data.List;
   end Get_Children;

   function Get_Children
            (  Item : Implicit_Tagged_Sequence_Data_Item
            )  return Data_Item_Ptr_Array is
   begin
      if not Item.Initialized or else Item.List = null then
         Uninitialized (Item);
      end if;
      declare
         Result : Data_Item_Ptr_Array (1..Item.List'Length);
      begin
         for Index in Item.List'Range loop
            Result (Data_Item_Address (Index)) :=
               Item.List (Index).Item;
         end loop;
         return Result;
      end;
   end Get_Children;

   function Get_Children
            (  Item : Implicit_Sequence_Of_Data_Item
            )  return Data_Item_Ptr_Array is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      if Item.List = null then
         return (1..0 => null);
      else
         return Item.List.all;
      end if;
   end Get_Children;

   function Get_Length
            (  Item : Implicit_Sequence_Data_Item
            )  return Natural is
   begin
      if not Item.Initialized or else Item.Data = null then
         Uninitialized (Item);
      end if;
      return Integer (Item.Data.Length);
   end Get_Length;

   function Get_Length
            (  Item : Implicit_Sequence_Of_Data_Item
            )  return Natural is
   begin
      if not Item.Initialized or else Item.List = null then
         Uninitialized (Item);
      end if;
      return Item.Length;
   end Get_Length;

   function Get_Length
            (  Item : Implicit_Tagged_Sequence_Data_Item
            )  return Natural is
   begin
      if not Item.Initialized or else Item.List = null then
         Uninitialized (Item);
      end if;
      return Item.List'Length;
   end Get_Length;

   function Get_Size
            (  Item : Implicit_Sequence_Data_Item
            )  return Natural is
   begin
      if not Item.Initialized or else Item.Data = null then
         Uninitialized (Item);
      end if;
      declare
         Result : Natural := 1;
         List   : Data_Item_Ptr_Array renames Item.Data.List;
      begin
         for Index in List'Range loop
            Result := Result + Get_Size (List (Index).all);
         end loop;
         return Result;
      end;
   end Get_Size;

   function Get_Size
            (  Item : Implicit_Sequence_Of_Data_Item
            )  return Natural is
   begin
      if not Item.Initialized or else Item.List = null then
         Uninitialized (Item);
      end if;
      declare
         Result : Natural := 1;
      begin
         for Index in Item.List'Range loop
            Result := Result + Get_Size (Item.List (Index).all);
         end loop;
         return Result;
      end;
   end Get_Size;

   function Get_Size
            (  Item : Implicit_Tagged_Sequence_Data_Item
            )  return Natural is
   begin
      if not Item.Initialized or else Item.List = null then
         Uninitialized (Item);
      end if;
      declare
         Result : Natural := 1;
         List   : Tagged_Data_Item_Array renames Item.List.all;
      begin
         for Index in List'Range loop
            Result := Result + Get_Size (List (Index).Item.all);
         end loop;
         return Result;
      end;
   end Get_Size;

   function Get_Tag
            (  Item  : Implicit_Tagged_Sequence_Data_Item;
               Index : Positive
            )  return Tag_Type is
   begin
      if Index > Get_Length (Item) then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      else
         return Item.List (Index).Tag;
      end if;
   end Get_Tag;

   procedure Initialized (Item : in out Implicit_Sequence_Data_Item) is
   begin
      null;
   end Initialized;

   procedure Initialized
             (  Item : in out Implicit_Tagged_Sequence_Data_Item
             )  is
   begin
      null;
   end Initialized;

   function Is_Implicit
            (  Item : Implicit_Sequence_Data_Item
            )  return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit
            (  Item : Implicit_Sequence_Of_Data_Item
            )  return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit
            (  Item : Implicit_Tagged_Sequence_Data_Item
            )  return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Set
            (  Item  : Implicit_Tagged_Sequence_Data_Item;
               Index : Positive
            )  return Boolean is
   begin
      if Index > Get_Length (Item) then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      else
         return not Item.List (Index).Unset;
      end if;
   end Is_Set;

   function Is_Universal
            (  Item  : Implicit_Tagged_Sequence_Data_Item;
               Index : Positive
            )  return Boolean is
   begin
      if Index > Get_Length (Item) then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      else
         return Item.List (Index).Universal;
      end if;
   end Is_Universal;

   function Is_Untagged
            (  Item  : Implicit_Tagged_Sequence_Data_Item;
               Index : Positive
            )  return Boolean is
   begin
      if Index > Get_Length (Item) then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      else
         return Item.List (Index).Untagged;
      end if;
   end Is_Untagged;

   procedure Next_Item
             (  Item    : in out Implicit_Sequence_Of_Data_Item;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if Item.Length >= Item.List'Length then
         Raise_Exception
         (  Data_Error'Identity,
            (  "ASN.1 sequence of values is longer than "
            &  Image (Integer (Item.List'Length))
            &  " values"
         )  );
      end if;
      Item.Current_Item.Caller   := null;
      Item.Current_Item.State    := 0;
      Item.Current_Item.Current  := 1;
      Item.Current_Item.List (1) := Item.List
                                    (  Item.List'First
                                    +  Data_Item_Offset (Item.Length)
                                    );
      Call (Client, Pointer, Item.Current_Item'Unchecked_Access, 1);
      Item.Fed_Count := Client.Fed;
      State := 0;
   end Next_Item;

   procedure Reset
             (  Item  : in out Implicit_Tagged_Sequence_Data_Item;
                Index : Positive;
                Unset : Boolean
             )  is
   begin
      if Index > Get_Length (Item) then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      else
         declare
            This : Tagged_Data_Item renames Item.List (Index);
         begin
            This.Unset := Unset;
         end;
      end if;
   end Reset;

   function Self
            (  Item : Implicit_Sequence_Data_Item'Class
            )  return Implicit_Sequence_Data_Item_Ptr is
      package From_Sequence_Address is
         new System.Address_To_Access_Conversions
             (  Implicit_Sequence_Data_Item'Class
             );
      use From_Sequence_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   function Self
            (  Item : Implicit_Sequence_Of_Data_Item'Class
            )  return Implicit_Sequence_Of_Data_Item_Ptr is
      package From_Sequence_Address is
         new System.Address_To_Access_Conversions
             (  Implicit_Sequence_Of_Data_Item'Class
             );
      use From_Sequence_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   function Self
            (  Item : Implicit_Tagged_Sequence_Data_Item'Class
            )  return Implicit_Tagged_Sequence_Data_Item_Ptr is
      package From_Sequence_Address is
         new System.Address_To_Access_Conversions
             (  Implicit_Tagged_Sequence_Data_Item'Class
             );
      use From_Sequence_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Sequence_Of_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length < 0 then
         Item.Total_Length := 0;
         Item.Definite     := False;
      else
         Item.Total_Length := Length;
         Item.Definite     := True;
      end if;
   end Set_Implicit_Tag;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Tagged_Sequence_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length < 0 then
         Item.Total_Length := 0;
         Item.Definite     := False;
      else
         Item.Total_Length := Length;
         Item.Definite     := True;
      end if;
   end Set_Implicit_Tag;

   procedure Set_Optional
             (  Item     : in out Implicit_Tagged_Sequence_Data_Item;
                Index    : Positive;
                Optional : Boolean
             )  is
   begin
      if Index > Get_Length (Item) then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      else
         declare
            This : Tagged_Data_Item renames Item.List (Index);
         begin
            if This.Untagged then
               if Optional then
                  Raise_Exception (Status_Error'Identity, Untagged);
               end if;
            else
               This.Tag.Optional := Optional;
               This.Unset        := Optional;
            end if;
         end;
      end if;
   end Set_Optional;

   procedure Set_Tag
             (  Item      : in out Implicit_Tagged_Sequence_Data_Item;
                Index     : Positive;
                Tag       : Tag_Type;
                Unset     : Boolean := False;
                Universal : Boolean := False
             )  is
   begin
      if Index > Get_Length (Item) then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      else
         declare
            This : Tagged_Data_Item renames Item.List (Index);
         begin
            This.Tag := Tag;
            if Tag.Class = Universal_Tag then
               if This.Item.all in ASN1_Data_Item'Class then
                  This.Tag.Value :=
                     Get_ASN1_Type
                     (  ASN1_Data_Item'Class (This.Item.all)
                     );
               end if;
            end if;
            This.Unset     := Unset;
            This.Untagged  := False;
            This.Universal := Universal;
         end;
      end if;
   end Set_Tag;

   procedure Set_Untagged
             (  Item  : in out Implicit_Tagged_Sequence_Data_Item;
                Index : Positive
             )  is
   begin
      if Index > Get_Length (Item) then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      else
         declare
            This : Tagged_Data_Item renames Item.List (Index);
         begin
            This.Tag := (Universal_Tag, 0, False);
            This.Unset     := False;
            This.Universal := False;
            This.Untagged  := True;
         end;
      end if;
   end Set_Untagged;

end GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Implicit;
