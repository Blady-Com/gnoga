--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Choices                                Summer, 2019       --
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

with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Connection_State_Machine.ASN1.Choices is

   Invalid_EOC : constant String := "Invalid indefinite ASN.1 " &
                                    "choice termination";
   function Always_Constructed
            (  Item : Choice_Data_Item
            )  return Boolean is
   begin
      return True;
   end Always_Constructed;

   procedure Check (Item : Data_Item'Class) is
   begin
      if Item not in ASN1_Data_Item'Class then
         Raise_Exception
         (  Use_Error'Identity,
            Wrong_Choice & Expanded_Name (Item'Tag)
         );
      end if;
   end Check;

   procedure Uninitialized (Item : Abstract_ASN1_Data_Item'Class) is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         (  "ASN.1 Set "
         &  Expanded_Name (Item'Tag)
         &  " was not properly initialized"
      )  );
   end Uninitialized;

   procedure Enable_Unsolicited
             (  Item   : in out Choice_Data_Item;
                Enable : Boolean
             )  is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      Item.Unsolicited := Enable;
   end Enable_Unsolicited;

   procedure Encode
             (  Item    : Choice_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      Tag : Tag_Type;
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
      elsif Item.Current.Position = 0 then
         Raise_Exception (Use_Error'Identity, Nothing_Selected);
      end if;
      Tag := Item.List (Item.Current.Position);
      declare
         This : Abstract_ASN1_Data_Item'Class renames
                Abstract_ASN1_Data_Item'Class
                (  Get (Item.Map, Tag).Item.all
                );
      begin
         Put (Data, Pointer, Tag, Always_Constructed (This));
         Definite_Encode (This, Data, Pointer);
      end;
   end Encode;

   Get_Element      : constant := 1;
   Get_Tag_Value    : constant := 2;
   Select_By_Tag    : constant := 3;
   Skip_Until_Zero  : constant := 4;
   Skip_Second_Zero : constant := 5;
   Skip_Definite    : constant := 6;
--  --     First_Zero       : constant := 7;
--  --     Second_Zero      : constant := 8;

--     procedure End_Of_Subsequence
--               (  Item    : in out Choice_Data_Item;
--                  Data    : Stream_Element_Array;
--                  Pointer : Stream_Element_Offset;
--                  Client  : in out State_Machine'Class;
--                  State   : in out Stream_Element_Offset
--               )  is
--     begin
--        if Item.Length >= 0 then
--           if Item.Length = Get_Length (Item.Current) then
--              State := 0;
--           else
--              Raise_Exception (Data_Error'Identity, Invalid_Length);
--           end if;
--        else
--           State := First_Zero;
--        end if;
--     end End_Of_Subsequence;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Choice_Data_Item
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
               Object  : Choice_Data_Item'Class
                         renames Self (Item).all;
            begin
               if This.Parent = null then
                  Counter.Parent := This.Shared'Access;
               else
                  Counter.Parent := This.Parent;
               end if;
               Object.Initialized := True;
               Choice_Data_Item'Class'Write (Counter'Access, Object);
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
                     Check (Counter.Data.Vector (Index).all);
                     Object.List (Position) :=
                        (  Class    => Context_Specific_Tag,
                           Value    => ASN1_Type (Position - 1),
                           Optional => False
                        );
                     Add
                     (  Object.Map,
                        Object.List (Position),
                        (  Position => Position,
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

   procedure Feed
             (  Item    : in out Choice_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Item.Current.Position := 0;
         State := Get_Element;
      end if;
      while Pointer <= Data'Last loop
         case State is
            when Get_Element =>
--                 if Item.Length = 0 then
--                    Raise_Exception
--                    (  Data_Error'Identity,
--                       Invalid_Length
--                    );
--                 end if;
               declare
                  Tag : Tag_Type renames Item.Current.Tag;
               begin
                  Tag.Value := ASN1_Type (Data (Pointer) and 16#1F#);
                  case ASN1_Tag (Data (Pointer) and 2#1100_0000#) is
                     when Application_Class =>
                        Tag.Class := Application_Tag;
                     when Context_Specific_Class =>
                        Tag.Class := Context_Specific_Tag;
                     when Private_Class =>
                        Tag.Class := Private_Tag;
                     when others =>
                        Tag.Class := Universal_Tag;
                  end case;
                  if Tag.Value < 31 then
                     State := Select_By_Tag;
                  else
                     State := Get_Tag_Value;
                  end if;
               end;
               Pointer := Pointer + 1;
--                 if Item.Length > 0 then
--                    Item.Length := Item.Length - 1;
--                 end if;
            when Get_Tag_Value =>
               while Pointer <= Data'Last loop
--                    if Item.Length = 0 then
--                       Raise_Exception
--                       (  Data_Error'Identity,
--                          Invalid_Length
--                       );
--                    end if;
                  declare
                     This  : constant ASN1_Type :=
                                      ASN1_Type (Data (Pointer));
                     Value : ASN1_Type renames Item.Current.Tag.Value;
                  begin
                     Pointer := Pointer + 1;
--                       if Item.Length > 0 then
--                          Item.Length := Item.Length - 1;
--                       end if;
                     if This > 127 then
                        Value := Value * 128 + (This - 128);
                     else
                        Value := Value * 128 + This;
                        State := Select_By_Tag;
                        exit;
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
                  Offset : constant Integer :=
                                    Find (Item.Map, Item.Current.Tag);
               begin
                  if Offset > 0 then
                     declare
                        Element : constant Element_Descriptor :=
                                           Get (Item.Map, Offset);
                     begin
                        Item.Current.Position := Element.Position;
--                          State := First_Zero;
                        State := 0;
                        Call
                        (  Item    => Item.Current,
                           Callee  => Element.Item.all,
                           Tag     => Item.Current.Tag,
                           Pointer => Pointer,
                           Client  => Client,
                           State   => State
                        );
                        return;
                     end;
                  elsif Item.Unsolicited then
                     State := Start_Length;
                  else
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "ASN.1 unexpected choice tag "
                        &  Image (Item.Current.Tag)
                     )  );
                  end if;
               end;
            when Skip_Until_Zero => -- Skip indefinite length object
               while Data (Pointer) /= 0 loop
                  Pointer := Pointer + 1;
                  if Pointer > Data'Last then
                     return;
                  end if;
               end loop;
               State := Skip_Second_Zero;
            when Skip_Second_Zero =>
               if Data (Pointer) = 0 then
                  State := 0;
                  return;
               else
                  State := Skip_Until_Zero;
               end if;
            when Stream_Element_Offset'First..-1 => -- Skipped length
               while not Is_Length_Ready (State) loop
                  if Pointer > Data'Last then
                     return;
                  end if;
                  Embedded_Feed (Data, Pointer, State);
               end loop;
               if Is_Indefinite (State) then
                  State := Skip_Until_Zero;
               else
                  State := Skip_Definite + Get_Length (State);
                  if State = Skip_Definite then
                     State := 0;
                     return;
                  end if;
               end if;
            when others => -- Skip definite length object
               loop
                  Pointer := Pointer + 1;
                  State   := State   - 1;
                  if State <= Skip_Definite then
                     State := 0;
                     return;
                  elsif Pointer > Data'Last then
                     return;
                  end if;
               end loop;
--              when First_Zero =>
--                 if Data (Pointer) /= 0 then
--                    Raise_Exception (Data_Error'Identity, Invalid_EOC);
--                 end if;
--                 State   := Second_Zero;
--                 Pointer := Pointer + 1;
--              when others =>
--                 if Data (Pointer) /= 0 then
--                    Raise_Exception (Data_Error'Identity, Invalid_EOC);
--                 end if;
--                 State   := 0;
--                 Pointer := Pointer + 1;
--                 return;
         end case;
      end loop;
   end Feed;

--     procedure Feed
--               (  Item    : in out Choice_Data_Item;
--                  Data    : Stream_Element_Array;
--                  Pointer : in out Stream_Element_Offset;
--                  Client  : in out State_Machine'Class;
--                  State   : in out Stream_Element_Offset
--               )  is
--        Skip_Until_Zero : constant := 1;
--        Skip_Definite   : constant := 2;
--     begin
--        if Item.Position = 0 then
--           while Pointer <= Data'Last loop
--  ada.Text_IO.Put_Line("     feed ***IGNORED*** choice "
--  &" pointer="&stream_element_offset'image(pointer)
--  &" data=%"&image(integer(data(pointer)),base=>16)
--  &" state="&stream_element_offset'image(State)
--  &" length="&stream_element_offset'image(item.Length)
--  &" "&Image(data(data'first..pointer-1))&"|"&Image(data(pointer..data'last))
--  );
--              case State is
--                 when 0 => -- Get ignored item length
--                    if not Item.Unsolicited then
--                       Raise_Exception
--                       (  Data_Error'Identity,
--                          (  "ASN.1 unexpected choice tag "
--                          &  Image (Item.Tag)
--                       )  );
--                    end if;
--                    if Item.Length < 0 then
--                       State := Skip_Until_Zero;
--                    elsif Item.Length = 0 then -- Zero length
--                       return;
--                    else
--                       State := Skip_Definite;
--                    end if;
--                 when Skip_Definite => -- Skip definite length object
--                    loop
--                       Pointer     := Pointer     + 1;
--                       Item.Length := Item.Length - 1;
--                       if Item.Length <= 0 then
--                          State := 0;
--                          return;
--                       elsif Pointer > Data'Last then
--                          return;
--                       end if;
--                    end loop;
--                 when others => -- Skip indefinite length object
--                    loop
--                       if Data (Pointer) = 0 then -- The first zero, leave
--                          State := 0;             -- it be
--                          return;
--                       end if;
--                       Pointer := Pointer + 1;
--                       if Pointer > Data'Last then
--                          return;
--                       end if;
--                    end loop;
--              end case;
--           end loop;
--        else
--  ada.Text_IO.Put_Line("     feed choice "
--  &" pointer="&stream_element_offset'image(pointer)
--  &" data=%"&image(integer(data(pointer)),base=>16)
--  &" state="&stream_element_offset'image(State)
--  &" length="&stream_element_offset'image(item.Length)
--  &" "&Image(data(data'first..pointer-1))&"|"&Image(data(pointer..data'last))
--  );
--           Feed
--           (  Get (Item.Map, Item.Position).Item.all,
--              Data,
--              Pointer,
--              Client,
--              State
--           );
--        end if;
--     end Feed;

   procedure Finalize
             (  Item : in out Choice_Data_Item
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Tag_Type_Array,
                Tag_Type_Array_Ptr
             );
   begin
      Free (Item.List);
   end Finalize;

   function Get_Children
            (  Item : Choice_Data_Item
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

   function Get_Length
            (  Item : Choice_Data_Item
            )  return Natural is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      return Item.List'Length;
   end Get_Length;

   function Get_Selected
            (  Item  : Choice_Data_Item
            )  return Natural is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      return Item.Current.Position;
   end Get_Selected;

   function Get_Selected
            (  Item  : Choice_Data_Item
            )  return Abstract_ASN1_Data_Item_Ptr is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      if Item.Current.Position = 0 then
         return null;
      else
         return Abstract_ASN1_Data_Item'Class
                (  Get
                   (  Item.Map,
                      Item.List (Item.Current.Position)
                   ) .Item.all
                ) 'Unchecked_Access;
      end if;
   end Get_Selected;

   function Get_Size (Item : Choice_Data_Item) return Natural is
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

   function Get_Tag
            (  Item  : Choice_Data_Item;
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

   procedure Initialized (Item : in out Choice_Data_Item) is
   begin
      null;
   end Initialized;

   function Is_Implicit (Item : Choice_Data_Item) return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Unsolicited_Enabled
            (  Item : Choice_Data_Item
            )  return Boolean is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      end if;
      return Item.Unsolicited;
   end Is_Unsolicited_Enabled;

   function Resolve_Selected
            (  Item : Abstract_ASN1_Data_Item'Class
            )  return Abstract_ASN1_Data_Item_Ptr is
      Current : Abstract_ASN1_Data_Item_Ptr :=
                Abstract_ASN1_Data_Item'Class
                (  Self (Item).all
                ) 'Unchecked_Access;
   begin
      loop
         if Current.all in ASN1_Data_Item'Class then
            return Current;
         elsif Current.all not in Choice_Data_Item'Class then
            Raise_Exception
            (  Data_Error'Identity,
                Wrong_Choice & Expanded_Name (Current.all'Tag)
            );
         end if;
         Current := Get_Selected (Choice_Data_Item'Class (Current.all));
         if Current = null then
            Raise_Exception (Data_Error'Identity, Nothing_Selected);
         end if;
      end loop;
   end Resolve_Selected;

   function Self
            (  Item : Choice_Data_Item'Class
            )  return Choice_Data_Item_Ptr is
      package From_Set_Address is
         new System.Address_To_Access_Conversions
             (  Choice_Data_Item'Class
             );
      use From_Set_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   procedure Set_Implicit_Tag
             (  Item   : in out Choice_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      if Length = 0 then
         Raise_Exception (Data_Error'Identity, Invalid_Length);
      end if;
   end Set_Implicit_Tag;

   procedure Set_Selected
             (  Item  : in out Choice_Data_Item;
                Index : Positive
             )  is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      elsif Index > Item.List'Last then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      else
         Item.Current.Position := Index;
      end if;
   end Set_Selected;

   procedure Set_Tag
             (  Item  : in out Choice_Data_Item;
                Index : Positive;
                Tag   : Tag_Type
             )  is
   begin
      if not Item.Initialized then
         Uninitialized (Item);
      elsif Index > Item.List'Last then
         Raise_Exception (Constraint_Error'Identity, Wrong_Index);
      elsif not Tag.Optional then
          Raise_Exception
          (  Mode_Error'Identity,
             "ASN.1 set element is not otpional"
          );
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
            Remove (Item.Map, Offset);
            Add (Item.Map, Tag, Element);
            Item.List (Index) := Tag;
         end if;
      end;
   end Set_Tag;

   procedure Set_Untagged (Item : in out Choice_Data_Item) is
   begin
      null;
   end Set_Untagged;

end GNAT.Sockets.Connection_State_Machine.ASN1.Choices;
