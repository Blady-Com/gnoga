--                                                                    --
--  package Test_Data_Servers       Copyright (c)  Dmitry A. Kazakov  --
--  Test server                                    Luebeck            --
--  Implementation                                 Winter, 2012       --
--                                                                    --
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

with Ada.Text_IO;            use Ada.Text_IO;
with Interfaces;             use Interfaces;
with Strings_Edit.Floats;    use Strings_Edit.Floats;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

package body Test_Data_Servers is

   function Create
            (  Factory  : access Data_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr is
      Result : Connection_Ptr;
   begin
      Put_Line ("Connected client at " & Image (From));
      Result := new Data_Connection (80, 120);
      Data_Connection (Result.all).From := From;
      return Result;
   end Create;

   function Create
            (  Set : access Integer_Set
            )  return ASN1.Abstract_ASN1_Data_Item_Ptr is
      use GNAT.Sockets.Connection_State_Machine.ASN1.Integers_32;
      type Ptr is access Integer_Data_Item;
      for Ptr'Storage_Pool use Get_Container (Set.all).Pool;
      This : constant Ptr := new Integer_Data_Item;
   begin
      return This.all'Unchecked_Access;
   end Create;

   function Create
            (  Reference : access Integer_Reference
            )  return ASN1.Abstract_ASN1_Data_Item_Ptr is
      use GNAT.Sockets.Connection_State_Machine.ASN1.Integers_32;
      type Ptr is access Integer_Data_Item;
      for Ptr'Storage_Pool use Get_Container (Reference.all).Pool;
      This : constant Ptr := new Integer_Data_Item;
   begin
      return This.all'Unchecked_Access;
   end Create;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Integer_Data_Array
             )  is
      use GNAT.Sockets.Connection_State_Machine.ASN1.Integers_32;
   begin
      for Index in Item'Range loop
         Integer_Data_Item'Write (Stream, Item (Index));
      end loop;
   end Enumerate;

   procedure Feed
             (  Item    : in out String_Length_Setter;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      use Big_Endian.Integers;
      Length : constant Integer_16 := Value (Item.Parent.Len);
   begin
      if Length not in 1..Item.Parent.S2.Value'Length then
         Raise_Exception
         (  Data_Error'Identity,
            (  "String length"
            &  Integer_16'Image (Length)
            &  " is not in range 1 .."
            &  Integer'Image (Item.Parent.S2.Value'Length)
         )  );
      end if;
      Item.Parent.S2.Last := Integer (Length);
      Set_Alternative
      (  Item.Parent.V,
         Integer (Item.Parent.N8.Value mod 4) + 1
      );
   end Feed;

   procedure Feed
             (  Client : in out Null_Machine;
                Title  : String;
                Data   : Stream_Element_Array
             )  is
      Old     : Stream_Element_Offset := Data'First;
      Pointer : Stream_Element_Offset := Data'First;
   begin
      Put_Line ("--------" & Title & " feeding |" & Image (Data) & "|");
      while Pointer <= Data'Last loop
         Client.Completed := False;
         Old := Pointer;
         Received
         (  Client  => Client,
            Data    => Data (Pointer..Pointer),
            Pointer => Pointer
         );
         if Pointer /= Old + 1 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Feed overrun Pointer"
               &  Stream_Element_Offset'Image (Pointer)
               &  " /="
               &  Stream_Element_Offset'Image (Old + 1)
               &  " (expected) Last"
               &  Stream_Element_Offset'Image (Old + 1)
            )  );
         end if;
      end loop;
      if not Client.Completed then
         Raise_Exception
         (  Status_Error'Identity,
            Title & " not completed at the source end"
         );
      end if;
   end Feed;

   procedure Feed_Flat
             (  Client : in out Null_Machine;
                Title  : String;
                Data   : Stream_Element_Array
             )  is
      Pointer : Stream_Element_Offset := Data'First;
   begin
      Put_Line ("--------" & Title & " feeding |" & Image (Data) & "|");
      Client.Completed := False;
      Received
      (  Client  => Client,
         Data    => Data,
         Pointer => Pointer
      );
      if Pointer /= Data'Last + 1 then
         Raise_Exception
         (  Status_Error'Identity,
            (  Title
            &  " Feed overrun Pointer"
            &  Stream_Element_Offset'Image (Pointer)
            &  " /="
            &  Stream_Element_Offset'Image (Data'Last + 1)
            &  " (expected) Last"
            &  Stream_Element_Offset'Image (Data'Last + 1)
         )  );
      end if;
      if not Client.Completed then
         Raise_Exception
         (  Status_Error'Identity,
            Title & " not completed at the source end"
         );
      end if;
   end Feed_Flat;

   procedure Finalize (Client : in out Data_Connection) is
   begin
      Put_Line ("Disconnected client " & Image (Client.From));
      Finalize (Connection (Client));
   end Finalize;

   function Get (Item : Sequence_Of_Integers) return Integer_32_Array is
      use GNAT.Sockets.Connection_State_Machine.ASN1.Sequences;
      Result : Integer_32_Array (1..Item.Length);
   begin
      for Index in 1..Item.Length loop
         Result (Index) := Item.Values (Index).Value;
      end loop;
      return Result;
   end Get;

   function Get (Set : Integer_Set) return Integer_32_Array is
      Result : Integer_32_Array (1..Get_Length (Set));
   begin
      for Index in Result'Range loop
         Result (Index) := Get (Set, Index);
      end loop;
      return Result;
   end Get;

   function Get
            (  Set   : Integer_Set;
               Index : Positive
            )  return Interfaces.Integer_32 is
      use GNAT.Sockets.Connection_State_Machine.ASN1.Integers_32;
      This : constant ASN1.Abstract_ASN1_Data_Item_Ptr :=
                      Get (Set, Index);
   begin
      return Implicit_Integer_Data_Item (This.all).Value;
   end Get;

   function Get
            (  Reference : Integer_Reference
            )  return Interfaces.Integer_32 is
      use GNAT.Sockets.Connection_State_Machine.ASN1.Integers_32;
      This : constant ASN1.Abstract_ASN1_Data_Item_Ptr :=
                      Get (Reference);
   begin
      return Integer_Data_Item (This.all).Value;
   end Get;

   function Image (Value : Integer_32_Array) return String is
      use Strings_Edit;
      Size : Natural := 1024 * 8;
   begin
      loop
         declare
            Result  : String (1..Size);
            Pointer : Integer := 1;
         begin
            for Index in Value'Range loop
               Put (Result, Pointer, Integer_32'Image (Value (Index)));
            end loop;
            return Result (1..Pointer - 1);
         exception
            when Layout_Error =>
               Size := (Size * 3) / 2;
         end;
      end loop;
   end Image;

   procedure Initialize (Client : in out Null_Machine) is
   begin
      Connected (Client);
   end Initialize;

   procedure Process_Packet (Client : in out Null_Machine) is
   begin
      Client.Completed := True;
   end Process_Packet;

   procedure Process_Packet (Client : in out Data_Connection) is
      Packet  : Stream_Element_Array (1..200);
      Pointer : Stream_Element_Offset := Packet'First;
      Index   : Stream_Element_Offset := Packet'First;
   begin
      Big_Endian.Unsigneds.Put (Packet, Pointer, Client.Blk.N1.Value);
      Big_Endian.Unsigneds.Put (Packet, Pointer, Client.Blk.N2.Value);
      Big_Endian.Unsigneds.Put (Packet, Pointer, Client.Blk.N3.Value);
      Big_Endian.Unsigneds.Put (Packet, Pointer, Client.Blk.N4.Value);
      Little_Endian.Unsigneds.Put (Packet, Pointer, Client.N5.Value);
      Little_Endian.Unsigneds.Put (Packet, Pointer, Client.N6.Value);
      Little_Endian.Unsigneds.Put (Packet, Pointer, Client.N7.Value);
      Little_Endian.Unsigneds.Put (Packet, Pointer, Client.N8.Value);
      Big_Endian.IEEE_754_Floats.Put (Packet, Pointer, Client.F1.Value);
      Big_Endian.IEEE_754_Long_Floats.Put
      (  Packet,
         Pointer,
         Client.F2.Value
      );
      Little_Endian.IEEE_754_Floats.Put
      (  Packet,
         Pointer,
         Client.F3.Value
      );
      Little_Endian.IEEE_754_Long_Floats.Put
      (  Packet,
         Pointer,
         Client.F4.Value
      );
      Terminated_Strings.Put
      (  Packet,
         Pointer,
         Client.S1.Value (1..Client.S1.Last),
         Character'Val (0)
      );
      Terminated_Strings.Put
      (  Packet,
         Pointer,
         Client.S2.Value (1..Client.S2.Last),
         Character'Val (0)
      );
      Chain_Code.Naturals.Put (Packet, Pointer, Client.N9.Value);
      Chain_Code.Integers.Put (Packet, Pointer, Client.N10.Value);

      Put ("Packet:");
      Put (Unsigned_8'Image (Client.Blk.N1.Value));
      Put (Unsigned_16'Image (Client.Blk.N2.Value));
      Put (Unsigned_32'Image (Client.Blk.N3.Value));
      Put (Unsigned_64'Image (Client.Blk.N4.Value));
      Put (Unsigned_8'Image (Client.N5.Value));
      Put (Unsigned_16'Image (Client.N6.Value));
      Put (Unsigned_32'Image (Client.N7.Value));
      Put (Unsigned_64'Image (Client.N8.Value));
      Put
      (  " "
      &  Image
         (  Big_Endian.IEEE_754_Floats.Get_Value (Client.F1)
      )  );
      Put
      (  " "
      &  Image
         (  Float
            (  Big_Endian.IEEE_754_Long_Floats.Get_Value (Client.F2)
      )  )  );
      Put
      (  " "
      &  Image
         (  Little_Endian.IEEE_754_Floats.Get_Value (Client.F3)
      )  );
      Put
      (  " "
      &  Image
         (  Float
            (  Little_Endian.IEEE_754_Long_Floats.Get_Value (Client.F4)
      )  )  );
      Put (" " & Quote (Client.S1.Value (1..Client.S1.Last)));
      Put (" " & Quote (Client.S2.Value (1..Client.S2.Last)));
      Put (" " & Image (Client.N9.Value));
      Put (" " & Image (Client.N10.Value));
      Put (" SELECTED=" & Image (Get_Alternative (Client.V)));
      case Get_Alternative (Client.V) is
         when 1 =>
            Little_Endian.Integers.Put
            (  Packet,
               Pointer,
               Little_Endian.Integers.Value (Client.V.A1.N)
            );
            Put
            (  Integer_16'Image
               (  Little_Endian.Integers.Value (Client.V.A1.N)
            )  );
         when 2 =>
            Chain_Code.Naturals.Put
            (  Packet,
               Pointer,
               Client.V.A2.L.Value
            );
            Chain_Code.Naturals.Put
            (  Packet,
               Pointer,
               Client.V.A2.M.Value
            );
            Put (Integer'Image (Client.V.A2.L.Value));
            Put (Integer'Image (Client.V.A2.M.Value));
         when 3 =>
            Terminated_Strings.Put
            (  Packet,
               Pointer,
               Client.V.S.Value (1..Client.V.S.Last),
               Character'Val (0)
            );
            Put (" " & Quote (Client.V.S.Value (1..Client.V.S.Last)));
         when others =>
            null;
      end case;
      New_Line;
      Send (Client, Packet (Packet'First..Pointer - 1), Index);
      if Index /= Pointer then
         Put_Line ("Error queueing packet");
         raise Connection_Error;
      end if;
   end Process_Packet;

   procedure Trace
             (  Factory    : in out Data_Factory;
                Context    : String;
                Occurrence : Exception_Occurrence
             )  is
   begin
      Put_Line (Context & ':' & Exception_Information (Occurrence));
   end Trace;

end Test_Data_Servers;
