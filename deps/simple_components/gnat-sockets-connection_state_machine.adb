--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine       Luebeck            --
--  Implementation                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Connection_State_Machine is

   procedure Call
             (  Client : in out State_Machine;
                Data   : Sequence_Ptr
             )  is
   begin
      Data.Caller  := Client.Data;
      Client.Data  := Data;
      Data.Current := 0;
   end Call;

   procedure Connected (Client : in out State_Machine) is
      Stream : aliased Initialization_Stream;
   begin
      Initialize (Connection (Client));
      State_Machine'Class'Write
      (  Stream'Access,
         State_Machine'Class (Client)
      );
      Client.Data :=
         new Sequence'
             (  Length  => Stream.Count,
                Caller  => null,
                Current => 1,
                List    => Stream.Data.Vector (1..Stream.Count)
             );
   end Connected;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : State_Machine
             )  is
   begin
      null;
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Block
             )  is
   begin
      if Stream.all in Initialization_Stream'Class then
         declare
            Count : Data_Item_Offset := 0;
            This  : Initialization_Stream'Class renames
                    Initialization_Stream'Class (Stream.all);
         begin
            if not Item.Initialized then
               declare
                  Counter : aliased Initialization_Stream;
               begin
                  Self (Item).Initialized := True;
                  Data_Block'Class'Write
                  (  Counter'Access,
                     Data_Block'Class (Item)
                  );
                  if Counter.Count <= 1 then
                     Raise_Exception
                     (  Use_Error'Identity,
                        (  "Block "
                        &  Expanded_Name (Data_Item'Class (Item)'Tag)
                        &  " is empty (contains no items)"
                     )  );
                  end if;
                  Self (Item).Data :=
                     new Sequence'
                         (  Length  => Counter.Count - 1,
                            Caller  => null,
                            Current => 1,
                            List    => Counter.Data.Vector
                                       (  2
                                       .. Counter.Count
                         )             );
                  Count := Data_Item_Offset (Get_Size (Item)) - 1;
               end;
            end if;
            if This.Ignore > 0 then
               This.Ignore := This.Ignore - 1;
            else
               This.Count := This.Count + 1;
               Put (This.Data, This.Count, Self (Item));
            end if;
            This.Ignore := This.Ignore + Count;
         end;
      else
         Raise_Exception
         (  Use_Error'Identity,
            (  "Stream attribute of "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " used with a non-initialization stream "
            &  Expanded_Name (Stream'Tag)
         )  );
      end if;
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Item
             )  is
   begin
      if Stream.all in Initialization_Stream'Class then
         declare
            This : Initialization_Stream'Class renames
                   Initialization_Stream'Class (Stream.all);
         begin
            if This.Ignore > 0 then
               This.Ignore := This.Ignore - 1;
            else
               This.Count := This.Count + 1;
               Put (This.Data, This.Count, Self (Item));
            end if;
         end;
      else
         Raise_Exception
         (  Use_Error'Identity,
            (  "Stream attribute of "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " used with a non-initialization stream "
            &  Expanded_Name (Stream'Tag)
         )  );
      end if;
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Selector
             )  is
   begin
      if Stream.all in Initialization_Stream'Class then
         declare
            Selector : Data_Selector'Class renames
                       Self (Data_Selector'Class (Item)).all;
            Count    : Data_Item_Offset := 0;
            This     : Initialization_Stream'Class renames
                       Initialization_Stream'Class (Stream.all);
         begin
            if not Selector.Initialized then
               Selector.Initialized := True;
               declare
                  Counter : aliased Initialization_Stream;
               begin
                  Selector.Initialized := True;
                  Data_Selector'Class'Write (Counter'Access, Selector);
                  if Counter.Count <= 1 then
                     Raise_Exception
                     (  Use_Error'Identity,
                        (  "Selector "
                        &  Expanded_Name (Data_Item'Class (Item)'Tag)
                        &  " contains no alternatives"
                     )  );
                  end if;
                  Selector.Alternatives :=
                     new Alternatives_List
                         (  Natural (Counter.Count)
                         -  1
                         );
                  declare
                     This : Data_Item_Ptr;
                     List : Sequence_Array renames
                            Item.Alternatives.List;
                  begin
                     for Index in List'Range loop
                        This :=
                           Get
                           (  Counter.Data,
                              Data_Item_Offset (Index) + 1
                           );
                        List (Index) :=
                           new Sequence'
                               (  Length  => 1,
                                  Caller  => null,
                                  Current => 1,
                                  List    => (1 => This)
                               );
                     end loop;
                  end;
                  Count := Data_Item_Offset (Get_Size (Item)) - 1;
               end;
            end if;
            if This.Ignore > 0 then
               This.Ignore := This.Ignore - 1;
            else
               This.Count := This.Count + 1;
               Put (This.Data, This.Count, Self (Item));
            end if;
            This.Ignore := This.Ignore + Count;
         end;
      else
         Raise_Exception
         (  Use_Error'Identity,
            (  "Stream attribute of "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " used with a non-initialization stream "
            &  Expanded_Name (Stream'Tag)
         )  );
      end if;
   end Enumerate;

   procedure Free is
      new Ada.Unchecked_Deallocation (Sequence, Sequence_Ptr);

   procedure Finalize (Client : in out State_Machine) is
   begin
      if Client.Data /= null then
         while Client.Data.Caller /= null loop
            Client.Data := Client.Data.Caller;
         end loop;
         Free (Client.Data);
      end if;
      Finalize (Connection (Client));
   end Finalize;

   procedure Finalize (Item : in out Data_Block) is
   begin
      Free (Item.Data);
   end Finalize;

   procedure Finalize (Item : in out Data_Selector) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Alternatives_List,
                Alternatives_List_Ptr
             );
   begin
      if Item.Alternatives /= null then
         declare
            List : Sequence_Array renames Item.Alternatives.List;
         begin
            for Index in List'Range loop
               Free (List (Index));
            end loop;
         end;
         Free (Item.Alternatives);
      end if;
   end Finalize;

   procedure Feed
             (  Item    : in out Data_Block;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Call (Client, Item.Data);
      State := 0;
   end Feed;

   procedure Feed
             (  Item    : in out Data_Null;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      State := 0;
   end Feed;

   procedure Feed
             (  Item    : in out Data_Selector;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Call (Client, Item.Alternatives.List (Item.Current));
      State := 0;
   end Feed;

   package From_Block_Address is
      new System.Address_To_Access_Conversions (Data_Block'Class);

   package From_Item_Address is
      new System.Address_To_Access_Conversions (Data_Item'Class);

   package From_Selector_Address is
      new System.Address_To_Access_Conversions (Data_Selector'Class);

   function Get_Alternative (Item : Data_Selector) return Positive is
   begin
      return Item.Current;
   end Get_Alternative;

   function Get_Alternatives_Number (Item : Data_Selector)
      return Positive is
   begin
      if not Item.Initialized or else Item.Alternatives = null then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Selector "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " was not properly initialized"
         )  );
      end if;
      return Positive (Item.Alternatives.Size);
   end Get_Alternatives_Number;

   function Get_Length (Item : Data_Block) return Positive is
   begin
      if not Item.Initialized or else Item.Data = null then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Block "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " was not properly initialized"
         )  );
      end if;
      return Positive (Item.Data.Length);
   end Get_Length;

   function Get_Size (Item : Data_Block) return Positive is
   begin
      if not Item.Initialized or else Item.Data = null then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Block "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " was not properly initialized"
         )  );
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

   function Get_Size (Item : Data_Item) return Positive is
   begin
      return 1;
   end Get_Size;

   function Get_Size (Item : Data_Selector) return Positive is
   begin
      if not Item.Initialized or else Item.Alternatives = null then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Selector "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
            &  " was not properly initialized"
         )  );
      end if;
      declare
         Result   : Natural := 1;
         Sequence : Sequence_Array renames Item.Alternatives.List;
      begin
         for Index in Sequence'Range loop
            declare
               List : Data_Item_Ptr_Array renames Sequence (Index).List;
            begin
               for Index in List'Range loop
                  Result := Result + Get_Size (List (Index).all);
               end loop;
            end;
         end loop;
         return Result;
      end;
   end Get_Size;

   procedure Read
             (  Stream : in out Initialization_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      Last := Item'Last;
   end Read;

   procedure Received
             (  Client  : in out State_Machine;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Pointer := Data'First;
      loop
         Feed
         (  Client.Data.List (Client.Data.Current).all,
            Data,
            Pointer,
            Client,
            Client.State
         );
         if Client.State = 0 then -- Done with this item
            Client.Data.Current := Client.Data.Current + 1;
            while Client.Data.Current > Client.Data.Length loop
               if Client.Data.Caller = null then
                  Client.Data.Current := 1;
                  Process_Packet (State_Machine'Class (Client));
                  exit;
               end if;
               Client.Data := Client.Data.Caller;
               Client.Data.Current := Client.Data.Current + 1;
            end loop;
         else
            exit when Pointer > Data'Last; -- All data consumed
            Raise_Exception
            (  Status_Error'Identity,
               (  "Unprocessed data left when after return from "
               &  Expanded_Name
                  (  Client.Data.List
                     (  Client.Data.Current
                     ) .all'Tag
            )  )  );
         end if;
      end loop;
   end Received;

   function Self (Item : Data_Block'Class) return Data_Block_Ptr is
      use From_Block_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   function Self (Item : Data_Item'Class) return Data_Item_Ptr is
      use From_Item_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   function Self (Item : Data_Selector'Class)
      return Data_Selector_Ptr is
      use From_Selector_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   procedure Set_Alternative
             (  Item        : in out Data_Selector;
                Alternative : Positive
             )  is
   begin
      if Alternative > Get_Alternatives_Number (Item) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Invalid alternative number of selector "
            &  Expanded_Name (Data_Item'Class (Item)'Tag)
         )  );
      end if;
      Item.Current := Alternative;
   end Set_Alternative;

   function To_Integer (Value : Unsigned_8) return Integer_8 is
   begin
      if Value < 2**7 then
         return Integer_8 (Value);
      else
         return Integer_8 (not Value) - 1;
      end if;
   end To_Integer;

   function To_Integer (Value : Unsigned_16) return Integer_16 is
   begin
      if Value < 2**15 then
         return Integer_16 (Value);
      else
         return Integer_16 (not Value) - 1;
      end if;
   end To_Integer;

   function To_Integer (Value : Unsigned_32) return Integer_32 is
   begin
      if Value < 2**31 then
         return Integer_32 (Value);
      else
         return Integer_32 (not Value) - 1;
      end if;
   end To_Integer;

   function To_Integer (Value : Unsigned_64) return Integer_64 is
   begin
      if Value < 2**63 then
         return Integer_64 (Value);
      else
         return Integer_64 (not Value) - 1;
      end if;
   end To_Integer;

   function To_Unsigned (Value : Integer_8) return Unsigned_8 is
   begin
      if Value >= 0 then
         return Unsigned_8 (Value);
      else
         return not Unsigned_8 (-(Value + 1));
      end if;
   end To_Unsigned;

   function To_Unsigned (Value : Integer_16) return Unsigned_16 is
   begin
      if Value >= 0 then
         return Unsigned_16 (Value);
      else
         return not Unsigned_16 (-(Value + 1));
      end if;
   end To_Unsigned;

   function To_Unsigned (Value : Integer_32) return Unsigned_32 is
   begin
      if Value >= 0 then
         return Unsigned_32 (Value);
      else
         return not Unsigned_32 (-(Value + 1));
      end if;
   end To_Unsigned;

   function To_Unsigned (Value : Integer_64) return Unsigned_64 is
   begin
      if Value >= 0 then
         return Unsigned_64 (Value);
      else
         return not Unsigned_64 (-(Value + 1));
      end if;
   end To_Unsigned;

   procedure Write
             (  Stream : in out Initialization_Stream;
                Item   : Stream_Element_Array
             )  is
   begin
      null;
   end Write;

end GNAT.Sockets.Connection_State_Machine;
