--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     MODBUS_Client                               Spring, 2015       --
--  Implementation                                                    --
--                                Last revision :  08:55 08 Apr 2022  --
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
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Synchronization;        use Synchronization;

package body GNAT.Sockets.Connection_State_Machine.MODBUS_Client is
   use Stream_Element_Offset_Edit;

   function "abs" (Timeout : Duration) return Time is
   begin
      if Timeout = Duration'Last then
         return Time_Last;
      elsif Timeout <= 0.0 then
         return Clock;
      else
         return Clock + To_Time_Span (Timeout);
      end if;
   exception
      when Constraint_Error =>
         return Time_Last;
   end "abs";

   procedure Accumulate
             (  CRC  : in out MODBUS_Checksum;
                Data : Stream_Element_Array
             )  is
      Value : Unsigned_16 := CRC.Value;
   begin
      for Index in Data'Range loop
         Value := Value xor Unsigned_16 (Data (Index));
         for Bit in 1..8 loop
            if (Value and 1) /= 0 then
               Value := Shift_Right (Value, 1) xor 16#A001#;
            else
               Value := Shift_Right (Value, 1);
            end if;
         end loop;
      end loop;
      CRC.Value := Value;
   end Accumulate;

   procedure Accumulate
             (  CRC  : in out MODBUS_Checksum;
                Data : Unsigned_8
             )  is
      Value : Unsigned_16 := CRC.Value;
   begin
      Value := Value xor Unsigned_16 (Data);
      for Bit in 1..8 loop
         if (Value and 1) /= 0 then
            Value := Shift_Right (Value, 1) xor 16#A001#;
         else
            Value := Shift_Right (Value, 1);
         end if;
      end loop;
      CRC.Value := Value;
   end Accumulate;

   procedure Bits_Read
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Values    : Bit_Array;
                Code      : Function_Code;
                Unit      : Unit_No
             )  is
      function Image (Values : Bit_Array) return String is
         Result  : String (1..Values'Length + (Values'Length - 1) / 5);
         Pointer : Integer := Result'First;
      begin
        for Index in Values'Range loop
           if Pointer > 1 and then (Pointer mod 5) = 1 then
              Result (Pointer) := ' ';
              Pointer := Pointer + 1;
           end if;
           if Values (Index) then
              Result (Pointer) := '1';
           else
              Result (Pointer) := '0';
           end if;
           Pointer := Pointer + 1;
        end loop;
        return Result;
      end Image;
   begin
      Trace
      (  MODBUS_Client'Class (Client),
         (  "FC" & Image (Integer (Code))
         &  " bits read "
         &  Image (Values)
         &  " Ref:"
         &  Image (Integer (Reference))
         &  " Unit:"
         &  Image (Integer (Unit))
      )  );
   end Bits_Read;

   procedure Bits_Written
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Bit_Address;
                To        : Bit_Address;
                Code      : Function_Code;
                Unit      : Unit_No
             )  is
   begin
      Trace
      (  MODBUS_Client'Class (Client),
         (  "FC" & Image (Integer (Code))
         &  " bits written at "
         &  Image (Integer (From))
         &  ".."
         &  Image (Integer (To))
         &  " Ref:"
         &  Image (Integer (Reference))
         &  " Unit:"
         &  Image (Integer (Unit))
      )  );
   end Bits_Written;

   function Can_Send
            (  Client : MODBUS_Client;
               Code   : Function_Code;
               Count  : Natural := 0
            )  return Boolean is
   begin
      return
      (  Available_To_Send (Client)
      >= Get_Request_Length (Client, Code, Count)
      );
   end Can_Send;

   procedure Clear (Client : in out MODBUS_Client) is
   begin
      Clear (State_Machine (Client));
      Client.Payload_Data.Offset := 0;
      Client.Payload_Data.Last   := 0;
   end Clear;

   function Error_Text (Code : Exception_Code) return String is
   begin
      case Code is
         when 1 =>
            return "Function code received in the query is not "     &
                   "recognized or allowed by slave";
         when 2 =>
            return "Data address of some or all the required "       &
                   "entities are not allowed or do not exist in slave";
         when 3 =>
            return "Value is not accepted by slave";
         when 4 =>
            return "Unrecoverable error occurred while slave was "   &
                   "attempting to perform requested action";
         when 5 =>
            return "Slave has accepted request and is processing "   &
                   "it, but a long duration of time is required. "   &
                   "This response is returned to prevent a timeout " &
                   "error from occurring in the master. Master can " &
                   "next issue a Poll Program Complete message to "  &
                   "determine if processing is completed";
         when 6 =>
            return "Slave is engaged in processing a long-duration " &
                   "command. Master should retry later";
         when 7 =>
            return "Slave cannot perform the programming "           &
                   "functions. Master should request diagnostic or " &
                   "error information from slave";
         when 8 =>
            return "Slave detected a parity error in memory. "       &
                   "Master can retry the request, but service may "  &
                   "be required on the slave device";
         when 10 =>
            return "Specialized for Modbus gateways. Indicates "     &
                   "a misconfigured gateway";
         when 11 =>
            return "Specialized for Modbus gateways. Sent when "     &
                   "slave fails to respond";
         when others =>
            return "Unknown exception code " & Image (Integer (Code));
      end case;
   end Error_Text;

   procedure Exception_Status_Received
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Status    : Unsigned_8;
                Unit      : Unit_No
             )  is
   begin
      Trace
      (  MODBUS_Client'Class (Client),
         (  "FC7 status "
         &  Image (Integer (Status))
         &  " Ref:"
         &  Image (Integer (Reference))
         &  " Unit:"
         &  Image (Integer (Unit))
      )  );
   end Exception_Status_Received;

   procedure Failed
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Error     : Exception_Code;
                Code      : Function_Code;
                Unit      : Unit_No
             )  is
   begin
      Trace
      (  MODBUS_Client'Class (Client),
         (  "FC" & Image (Integer (Code))
         &  " failed: "
         &  Error_Text (Error)
         &  " Ref:"
         &  Image (Integer (Reference))
         &  " Unit:"
         &  Image (Integer (Unit))
      )  );
   end Failed;

   procedure Feed
             (  Item    : in out Payload_And_Checksum_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         State     := Item.Last;
         Item.Last := Item.Offset;
         if State = 0 and then not Item.Enable then -- No checksum
            return;
         end if;
      end if;
      if State > 0 then
         loop
            Item.Last := Item.Last + 1;
            State     := State - 1;
            Item.Value (Item.Last) := Data (Pointer);
            Pointer   := Pointer + 1;
            exit when State = 0;
            if Pointer > Data'Last then
               return;
            end if;
         end loop;
         if not Item.Enable then -- No checksum, we are done
            return;
         end if;
         State := -2;
         if Pointer > Data'Last then
            return;
         end if;
      end if;
      loop
         Item.Data (3 + State) := Data (Pointer);
         Pointer := Pointer + 1;
         State   := State   + 1;
         exit when State = 0 or else Pointer > Data'Last;
      end loop;
   end Feed;

   procedure Feed
             (  Item    : in out RTU_Length;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Machine : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if Item.Client.Payload_Data.Enable then
         declare
            Client  : MODBUS_Client'Class renames Item.Client.all;
            Payload : Payload_And_Checksum_Item renames
                      Client.Payload_Data;
         begin
            case Client.Function_Code.Value is
               when 1 | 2 | 3 | 4 | 23 => -- Bits or words read
                  declare
                     Length : constant Stream_Element_Offset :=
                        Stream_Element_Offset (Data (Pointer)) + 1;
                  begin
                     if Length <= 0 then
                        Payload.Last := 0;
                     else
                        Payload.Last := Length;
                     end if;
                  end;
               when 5 | 6 | 15 | 16 => -- Bits or words written
                  Payload.Last := 4;
               when 7 | 128..255 => -- Exception status
                  Payload.Last := 1;
               when 22 => -- Word masked
                  Payload.Last := 6;
               when 24 => -- Words from FIFO read
                  loop
                     if State = 0 then
                        Payload.Value (1) := Data (Pointer);
                        Payload.Last :=
                           Stream_Element_Offset (Data (Pointer)) * 256;
                        State   := 1;
                        Pointer := Pointer + 1;
                        exit when Pointer > Data'Last;
                     else
                        Payload.Value (2) := Data (Pointer);
                        Payload.Last :=
                           (  Payload.Last
                           +  Stream_Element_Offset (Data (Pointer))
                           );
                        State   := 0;
                        Pointer := Pointer + 1;
                        exit;
                     end if;
                  end loop;
                  Payload.Last := Payload.Last + 2;
                  if Payload.Last > Payload.Size then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "The frame payload data exceeds "
                        &  Image (Payload.Size)
                        &  " bytes"
                     )  );
                  end if;
               when others =>
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Unsupported function code "
                     &  Image (Integer (Client.Function_Code.Value))
                  )  );
            end case;
         end;
      else
         State := 0;
      end if;
   end Feed;

   procedure Feed
             (  Item    : in out TCP_Head;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Machine : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if Item.Client.Payload_Data.Enable then
         State := 0;
      else
         Feed (Data_Block (Item), Data, Pointer, Machine, State);
      end if;
   end Feed;

   procedure Feed
             (  Item    : in out Set_TCP_Data_Length;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Machine : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      Client : MODBUS_Client'Class renames Item.Head.Client.all;
      Length : constant Stream_Element_Offset :=
               Stream_Element_Offset (Item.Head.Length_Field.Value) - 2;
   begin
      if Length > Client.Payload_Data.Size then
         Raise_Exception
         (  Data_Error'Identity,
            (  "The frame payload data exceeds "
            &  Image (Client.Payload_Data.Size)
            &  " bytes"
         )  );
      elsif Length <= 0 then
         Client.Payload_Data.Last := 0;
      else
         Client.Payload_Data.Last := Length;
      end if;
   end Feed;

   function Get (CRC : MODBUS_Checksum) return Stream_Element_Array is
   begin
      return
      (  Stream_Element (CRC.Value and 16#FF#),
         Stream_Element (Shift_Right (CRC.Value, 8) and 16#FF#)
      );
   end Get;

   function Get_Request_Length
            (  Client : MODBUS_Client;
               Code   : Function_Code;
               Count  : Natural := 0
            )  return Stream_Element_Count is
   begin
      case Code is
         when 1..6 =>
            return 10 + 2;
         when 7 =>
            return 8;
         when 15 =>
            return 10 + 3 + (Stream_Element_Count (Count) + 7) / 8;
         when 16 =>
            return 10 + 3 + Stream_Element_Count (Count) * 2;
         when 22 =>
            return 10 + 4;
         when 23 =>
            return 10 + 7 + Stream_Element_Count (Count) * 2;
         when 24 =>
            return 10;
         when others =>
            Raise_Exception
            (  Constraint_Error'Identity,
               "FC" & Image (Integer (Code)) & " is not supported"
            );
      end case;
   end Get_Request_Length;

   function Get_RTU_Checksum_Mode
            (  Client : MODBUS_Client
            )  return Boolean is
   begin
      return Client.Payload_Data.Enable;
   end Get_RTU_Checksum_Mode;

   function Get_RTU_Silence_Time
            (  Client : MODBUS_Client
            )  return Duration is
   begin
      return To_Duration (Client.Payload_Data.Event.Get);
   end Get_RTU_Silence_Time;

   procedure Prepare_To_Send
             (  Client : in out MODBUS_Client;
                Length : Stream_Element_Count
             )  is
   begin
      if not Is_Connected (Client) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      end if;
      if Client.Payload_Data.Enable then
         if Available_To_Send (Client) >= Length - 4 then
            if Client.Payload_Data.Event.Get > Time_Span_Zero then
               if Queued_To_Send (Client) > 0 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Silence time violation, "
                     &  Image (Queued_To_Send (Client))
                     &  " elements still queued to send when"
                     &  " sending new request"
                  )  );
               elsif Clock < Client.Payload_Data.Event.Get_Next then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Silence time violation, "
                     &  Image (Queued_To_Send (Client))
                     &  " sending new request before silence time"
                     &  " expired"
                  )  );
               end if;
            end if;
            return;
         end if;
         Raise_Exception
         (  Data_Error'Identity,
            (  "Output buffer overrun, "
            &  Image (Queued_To_Send (Client))
            &  " elements queued, space for at least more "
            &  Image (Length - 4)
            &  " required (available "
            &  Image (Available_To_Send (Client))
            &  ")"
         )  );
      else
         if Available_To_Send (Client) >= Length then
            return;
         end if;
         Raise_Exception
         (  Data_Error'Identity,
            (  "Output buffer overrun, "
            &  Image (Queued_To_Send (Client))
            &  " elements queued, space for at least more "
            &  Image (Length)
            &  " required (available "
            &  Image (Available_To_Send (Client))
            &  ")"
         )  );
      end if;
   end Prepare_To_Send;

   procedure Process_Packet (Client : in out MODBUS_Client) is
      Data : Stream_Element_Array  renames Client.Payload_Data.Value;
      Last : Stream_Element_Offset renames Client.Payload_Data.Last;
   begin
      if Client.Payload_Data.Enable then -- Calculate CRC
         declare
            CRC : MODBUS_Checksum;
         begin
            Accumulate (CRC, Client.Unit_ID.Value);
            Accumulate (CRC, Client.Function_Code.Value);
            Accumulate (CRC, Data (Data'First..Last));
            declare
               Sum : constant Stream_Element_Array := Get (CRC);
            begin
               if Sum /= Client.Payload_Data.Data then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Invalid checksum "
                     &  Image (Client.Payload_Data.Data)
                     &  " (calculated was "
                     &  Image (Sum)
                     &  ")"
                  )  );
               end if;
            end;
         end;
      end if;
      case Client.Function_Code.Value is
         when 1 | 2 => -- Bits read
            if (  Last < 1
               or else
                  Last /= Stream_Element_Count (Data (1)) + 1
               )
            then
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid payload data length for FC1 or FC2"
               );
            end if;
            declare
               Values : Bit_Array (1..Bit_Address ((Last - 1) * 8));
               Bit_No : Bit_Address := Values'First;
            begin
               for Index in 2..Last loop
                  declare
                     Byte : constant Stream_Element := Data (Index);
                  begin
                     Values (Bit_No    ) := (Byte and   1) /= 0;
                     Values (Bit_No + 1) := (Byte and   2) /= 0;
                     Values (Bit_No + 2) := (Byte and   4) /= 0;
                     Values (Bit_No + 3) := (Byte and   8) /= 0;
                     Values (Bit_No + 4) := (Byte and  16) /= 0;
                     Values (Bit_No + 5) := (Byte and  32) /= 0;
                     Values (Bit_No + 6) := (Byte and  64) /= 0;
                     Values (Bit_No + 7) := (Byte and 128) /= 0;
                     Bit_No := Bit_No + 8;
                  end;
               end loop;
               Bits_Read
               (  MODBUS_Client'Class (Client), -- Dispatching call
                  Reference_ID (Client.Head.Transaction_ID.Value),
                  Values,
                  Function_Code (Client.Function_Code.Value),
                  Unit_No (Client.Unit_ID.Value)
               );
            end;
         when 3 | 4 | 23 => -- Words read
            if (  Last < 1
               or else
                  Last /= Stream_Element_Count (Data (1)) + 1
               )
            then
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid payload data length for FC3, FC4 or FC23"
               );
            end if;
            declare
               Values  : Word_Array (1..Word_Address ((Last - 1) / 2));
               Pointer : Stream_Element_Offset := Data'First + 1;
            begin
               for Index in Values'Range loop
                  Get (Data, Pointer, Values (Index));
               end loop;
               Words_Read
               (  MODBUS_Client'Class (Client), -- Dispatching call
                  Reference_ID (Client.Head.Transaction_ID.Value),
                  Values,
                  Function_Code (Client.Function_Code.Value),
                  Unit_No (Client.Unit_ID.Value)
               );
            end;
         when 5 => -- Bits written
            if Last /= 4 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid payload data length for FC5"
               );
            end if;
            declare
               Address : Unsigned_16;
               Pointer : Stream_Element_Offset := Data'First;
            begin
               Get (Data, Pointer, Address);
               Bits_Written
               (  MODBUS_Client'Class (Client), -- Dispatching call
                  Reference_ID (Client.Head.Transaction_ID.Value),
                  Bit_Address (Address),
                  Bit_Address (Address),
                  Function_Code (Client.Function_Code.Value),
                  Unit_No (Client.Unit_ID.Value)
               );
            end;
         when 6 => -- Word written
            if Last /= 4 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid payload data length for FC6"
               );
            end if;
            declare
               Address : Unsigned_16;
               Pointer : Stream_Element_Offset := Data'First;
            begin
               Get (Data, Pointer, Address);
               Words_Written
               (  MODBUS_Client'Class (Client), -- Dispatching call
                  Reference_ID (Client.Head.Transaction_ID.Value),
                  Word_Address (Address),
                  Word_Address (Address),
                  Function_Code (Client.Function_Code.Value),
                  Unit_No (Client.Unit_ID.Value)
               );
            end;
         when 7 => -- Exception status
            if Last /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid payload data length for FC7"
               );
            end if;
            Exception_Status_Received
            (  MODBUS_Client'Class (Client), -- Dispatching call
               Reference_ID (Client.Head.Transaction_ID.Value),
               Unsigned_8 (Client.Payload_Data.Value (1)),
               Unit_No (Client.Unit_ID.Value)
            );
         when 15 => -- Bits written
            if Last /= 4 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid payload data length for FC15"
               );
            end if;
            declare
               From    : Unsigned_16;
               To      : Unsigned_16;
               Pointer : Stream_Element_Offset := Data'First;
            begin
               Get (Data, Pointer, From);
               Get (Data, Pointer, To);
               Bits_Written
               (  MODBUS_Client'Class (Client), -- Dispatching call
                  Reference_ID (Client.Head.Transaction_ID.Value),
                  Bit_Address (From),
                  Bit_Address (From + To - 1),
                  Function_Code (Client.Function_Code.Value),
                  Unit_No (Client.Unit_ID.Value)
               );
            end;
         when 16 => -- Words written
            if Last /= 4 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid payload data length for FC15"
               );
            end if;
            declare
               From    : Unsigned_16;
               To      : Unsigned_16;
               Pointer : Stream_Element_Offset := Data'First;
            begin
               Get (Data, Pointer, From);
               Get (Data, Pointer, To);
               Words_Written
               (  MODBUS_Client'Class (Client), -- Dispatching call
                  Reference_ID (Client.Head.Transaction_ID.Value),
                  Word_Address (From),
                  Word_Address (From + To - 1),
                  Function_Code (Client.Function_Code.Value),
                  Unit_No (Client.Unit_ID.Value)
               );
            end;
         when 22 => -- Word masked
            if Last /= 6 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid payload data length for FC22"
               );
            end if;
            declare
               Address : Unsigned_16;
               Pointer : Stream_Element_Offset := Data'First;
            begin
               Get (Data, Pointer, Address);
               Words_Written
               (  MODBUS_Client'Class (Client), -- Dispatching call
                  Reference_ID (Client.Head.Transaction_ID.Value),
                  Word_Address (Address),
                  Word_Address (Address),
                  Function_Code (Client.Function_Code.Value),
                  Unit_No (Client.Unit_ID.Value)
               );
            end;
         when 24 => -- Words from FIFO read
            if Last < 4 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid payload data length for FC24"
               );
            end if;
            declare
               Bytes   : Unsigned_16;
               Words   : Unsigned_16;
               Pointer : Stream_Element_Offset := Data'First;
            begin
               Get (Data, Pointer, Bytes);
               Get (Data, Pointer, Words);
               if Bytes /= Data'Length - 4 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Invalid bytes count in FC24"
                  );
               elsif Words /= Bytes * 2 + 2 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Invalid words count in FC24"
                  );
               end if;
               declare
                  Values  : Word_Array (1..Word_Address (Words));
                  Pointer : Stream_Element_Offset := Data'First + 5;
               begin
                  for Index in Values'Range loop
                     Get (Data, Pointer, Values (Index));
                  end loop;
                  Words_Read
                  (  MODBUS_Client'Class (Client), -- Dispatching call
                     Reference_ID (Client.Head.Transaction_ID.Value),
                     Values,
                     Function_Code (Client.Function_Code.Value),
                     Unit_No (Client.Unit_ID.Value)
                  );
               end;
            end;
         when 128..255 =>
            if Last /= 1 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid payload data length for exception response"
               );
            end if;
            Failed
            (  MODBUS_Client'Class (Client), -- Dispatching call
               Reference_ID (Client.Head.Transaction_ID.Value),
               Exception_Code (Client.Payload_Data.Value (1)),
               Function_Code (Client.Function_Code.Value - 128),
               Unit_No (Client.Unit_ID.Value)
            );
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               (  "Unsupported function code "
               &  Image (Integer (Client.Function_Code.Value))
            )  );
      end case;
   end Process_Packet;

   procedure Reset (CRC : in out MODBUS_Checksum) is
   begin
      CRC.Value := 16#FFFF#;
   end Reset;

   procedure Send
             (  Client    : in out MODBUS_Client'Class;
                Reference : Reference_ID;
                Unit      : Unit_No;
                Code      : Stream_Element;
                Address   : Unsigned_16;
                Data      : Stream_Element_Array
             )  is
      Header  : Stream_Element_Array (1..10);
      Pointer : Stream_Element_Offset := Header'First;
   begin
      Prepare_To_Send (Client, Header'Length + Data'Length);
      Put (Header, Pointer, Unsigned_16 (Reference));
      Header (Pointer)     := 0;
      Header (Pointer + 1) := 0;
      Pointer := Pointer + 2;
      Put (Header, Pointer, Unsigned_16 (Data'Length + 4));
      Header (Pointer)     := Stream_Element (Unit);
      Header (Pointer + 1) := Code;
      Pointer := Pointer + 2;
      Put (Header, Pointer, Address);
      if Client.Payload_Data.Enable then
         Client.Head.Transaction_ID.Value := Unsigned_16 (Reference);
         Pointer := Header'First + 6;
         Send (Client, Header, Pointer);
         if Pointer > Header'Last then
            declare
               CRC : MODBUS_Checksum;
            begin
               Accumulate (CRC, Header (Header'First + 6..Header'Last));
               Pointer := Data'First;
               Send (Client, Data, Pointer);
               if Pointer > Data'Last then
                  Accumulate (CRC, Data);
                  declare
                     Sum : constant Stream_Element_Array := Get (CRC);
                  begin
                     Pointer := Sum'First;
                     Send (Client, Sum, Pointer);
                     if Pointer > Sum'Last then
                        return;
                     end if;
                  end;
               end if;
            end;
         end if;
      else
         Pointer := Header'First;
         Send (Client, Header, Pointer);
         if Pointer > Header'Last then
            Pointer := Data'First;
            Send (Client, Data, Pointer);
            if Pointer > Data'Last then
               return;
            end if;
         end if;
      end if;
      Raise_Exception
      (  Data_Error'Identity,
         (  "Output buffer overrun, "
         &  Image (Queued_To_Send (Client))
         &  " elements queued, space for at least more "
         &  Image (Data'Last - Pointer + 1)
         &  " requred (available "
         &  Image (Available_To_Send (Client))
         &  ")"
      )  );
   end Send;

   procedure Send
             (  Client    : in out MODBUS_Client'Class;
                Reference : Reference_ID;
                Unit      : Unit_No;
                Code      : Stream_Element;
                Address   : Unsigned_16
             )  is
      Header  : Stream_Element_Array (1..10);
      Pointer : Stream_Element_Offset := Header'First;
   begin
      Prepare_To_Send (Client, Header'Length);
      Put (Header, Pointer, Unsigned_16 (Reference));
      Header (3..8) := (0, 0, 0, 4, Stream_Element (Unit), Code);
      Pointer := 9;
      Put (Header, Pointer, Address);
      if Client.Payload_Data.Enable then
         Client.Head.Transaction_ID.Value := Unsigned_16 (Reference);
         Pointer := Header'First + 6;
         Send (Client, Header, Pointer);
         if Pointer > Header'Last then
            declare
               CRC : MODBUS_Checksum;
            begin
               Accumulate (CRC, Header (Header'First + 6..Header'Last));
               declare
                  Sum : constant Stream_Element_Array := Get (CRC);
               begin
                  Pointer := Sum'First;
                  Send (Client, Sum, Pointer);
                  if Pointer > Sum'Last then
                     return;
                  end if;
               end;
            end;
         end if;
      else
         Pointer := Header'First;
         Send (Client, Header, Pointer);
         if Pointer > Header'Last then
            return;
         end if;
      end if;
      Raise_Exception
      (  Data_Error'Identity,
         (  "Output buffer overrun, "
         &  Image (Queued_To_Send (Client))
         &  " elements queued, space for at least more "
         &  Image (Header'Last - Pointer + 1)
         &  " requred (available "
         &  Image (Available_To_Send (Client))
         &  ")"
      )  );
   end Send;

   procedure Send
             (  Client    : in out MODBUS_Client'Class;
                Reference : Reference_ID;
                Unit      : Unit_No;
                Code      : Stream_Element
             )  is
      Header  : Stream_Element_Array (1..8);
      Pointer : Stream_Element_Offset := Header'First;
   begin
      Prepare_To_Send (Client, Header'Length);
      Put (Header, Pointer, Unsigned_16 (Reference));
      Header (3..8) := (0, 0, 0, 4, Stream_Element (Unit), Code);
      Pointer := Header'First;
      Send (Client, Header, Pointer);
      if Client.Payload_Data.Enable then
         Client.Head.Transaction_ID.Value := Unsigned_16 (Reference);
         Pointer := Header'First + 6;
         Send (Client, Header, Pointer);
         if Pointer > Header'Last then
            declare
               CRC : MODBUS_Checksum;
            begin
               Accumulate (CRC, Header (Header'First + 6..Header'Last));
               declare
                  Sum : constant Stream_Element_Array := Get (CRC);
               begin
                  Pointer := Sum'First;
                  Send (Client, Sum, Pointer);
                  if Pointer > Sum'Last then
                     return;
                  end if;
               end;
            end;
         end if;
      else
         Send (Client, Header, Pointer);
         if Pointer > Header'Last then
            return;
         end if;
      end if;
      Raise_Exception
      (  Data_Error'Identity,
         (  "Output buffer overrun, "
         &  Image (Queued_To_Send (Client))
         &  " elements queued, space for at least more "
         &  Image (Header'Last - Pointer + 1)
         &  " requred (available "
         &  Image (Available_To_Send (Client))
         &  ")"
      )  );
   end Send;

   procedure Send_FC1
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Bit_Address;
                To        : Bit_Address;
                Unit      : Unit_No := 255
             )  is
      Data    : Stream_Element_Array (1..2);
      Pointer : Stream_Element_Offset := Data'First;
   begin
      if From > To then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid bits range"
         );
      elsif To - From + 1 > 255 * 8 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "More than 2040 bits to read"
         );
      end if;
      Put (Data, Pointer, Unsigned_16 (To - From + 1));
      Send (Client, Reference, Unit, 1, Unsigned_16 (From), Data);
   end Send_FC1;

   procedure Send_FC2
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Bit_Address;
                To        : Bit_Address;
                Unit      : Unit_No := 255
             )  is
      Data    : Stream_Element_Array (1..2);
      Pointer : Stream_Element_Offset := Data'First;
   begin
      if From > To then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid bits range"
         );
      elsif To - From + 1 > 255 * 8 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "More than 2040 bits to read"
         );
      end if;
      Put (Data, Pointer, Unsigned_16 (To - From + 1));
      Send (Client, Reference, Unit, 2, Unsigned_16 (From), Data);
   end Send_FC2;

   procedure Send_FC3
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Word_Address;
                To        : Word_Address;
                Unit      : Unit_No := 255
             )  is
      Data    : Stream_Element_Array (1..2);
      Pointer : Stream_Element_Offset := Data'First;
   begin
      if From > To then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid words range"
         );
      elsif To - From + 1 > 127 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "More than 127 words to read"
         );
      end if;
      Put (Data, Pointer, Unsigned_16 (To - From + 1));
      Send (Client, Reference, Unit, 3, Unsigned_16 (From), Data);
   end Send_FC3;

   procedure Send_FC4
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Word_Address;
                To        : Word_Address;
                Unit      : Unit_No := 255
             )  is
      Data    : Stream_Element_Array (1..2);
      Pointer : Stream_Element_Offset := Data'First;
   begin
      if From > To then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid words range"
         );
      elsif To - From + 1 > 127 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "More than 127 words to read"
         );
      end if;
      Put (Data, Pointer, Unsigned_16 (To - From + 1));
      Send (Client, Reference, Unit, 4, Unsigned_16 (From), Data);
   end Send_FC4;

   procedure Send_FC5
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Address   : Bit_Address;
                Value     : Boolean;
                Unit      : Unit_No := 255
             )  is
      Data : Stream_Element_Array (1..2) := (0, 0);
   begin
      if Value then
         Data (1) := 16#FF#;
      end if;
      Send (Client, Reference, Unit, 5, Unsigned_16 (Address), Data);
   end Send_FC5;

   procedure Send_FC6
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Address   : Word_Address;
                Value     : Unsigned_16;
                Unit      : Unit_No := 255
             )  is
      Data    : Stream_Element_Array (1..2);
      Pointer : Stream_Element_Offset := Data'First;
   begin
      Put (Data, Pointer, Value);
      Send (Client, Reference, Unit, 6, Unsigned_16 (Address), Data);
   end Send_FC6;

   procedure Send_FC7
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Unit      : Unit_No := 255
             )  is
   begin
      Send (Client, Reference, Unit, 7);
   end Send_FC7;

   procedure Send_FC15
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Values    : Bit_Array;
                Unit      : Unit_No := 255
             )  is
   begin
      if Values'Length > 255 * 8 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "More than 2040 bits to write"
         );
      end if;
      declare
         Bytes   : constant Stream_Element_Offset :=
                      (Values'Length + 7) / 8;
         Data    : Stream_Element_Array (1..3 + Bytes) := (others => 0);
         Pointer : Stream_Element_Offset := Data'First;
         Power   : Stream_Element := 1;
      begin
         Put (Data, Pointer, Unsigned_16 (Values'Length));
         Data (Pointer) := Stream_Element (Bytes);
         Pointer := Pointer + 1;
         for Index in Values'Range loop
            if Values (Index) then
               Data (Pointer) := Data (Pointer) or Power;
            end if;
            if Power = 2#1000_0000# then
               Power   := 1;
               Pointer := Pointer + 1;
            else
               Power := Power * 2;
            end if;
         end loop;
         Send
         (  Client    => Client,
            Reference => Reference,
            Unit      => Unit,
            Code      => 15,
            Address   => Unsigned_16 (Values'First),
            Data      => Data
         );
      end;
   end Send_FC15;

   procedure Send_FC16
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Values    : Word_Array;
                Unit      : Unit_No := 255
             )  is
   begin
      if Values'Length > 127 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "More than 127 words to write"
         );
      end if;
      declare
         Bytes   : constant Stream_Element_Offset := Values'Length * 2;
         Data    : Stream_Element_Array (1..3 + Bytes);
         Pointer : Stream_Element_Offset := Data'First;
      begin
         Put (Data, Pointer, Unsigned_16 (Values'Length));
         Data (Pointer) := Stream_Element (Bytes);
         Pointer := Pointer + 1;
         for Index in Values'Range loop
            Put (Data, Pointer, Values (Index));
         end loop;
         Send
         (  Client    => Client,
            Reference => Reference,
            Unit      => Unit,
            Code      => 16,
            Address   => Unsigned_16 (Values'First),
            Data      => Data
         );
      end;
   end Send_FC16;

   procedure Send_FC22
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Address   : Word_Address;
                And_Mask  : Unsigned_16;
                Or_Mask   : Unsigned_16;
                Unit      : Unit_No := 255
             )  is
      Data    : Stream_Element_Array (1..4);
      Pointer : Stream_Element_Offset := Data'First;
   begin
      Put (Data, Pointer, And_Mask);
      Put (Data, Pointer, Or_Mask);
      Send (Client, Reference, Unit, 22, Unsigned_16 (Address), Data);
   end Send_FC22;

   procedure Send_FC23
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Word_Address;
                To        : Word_Address;
                Values    : Word_Array;
                Unit      : Unit_No := 255
             )  is
   begin
      if From > To then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid words range"
         );
      elsif To - From + 1 > 127 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "More than 127 words to read"
         );
      elsif Values'Length > 127 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "More than 127 words to write"
         );
      end if;
      declare
         Bytes   : constant Stream_Element_Offset := Values'Length * 2;
         Data    : Stream_Element_Array (1..7 + Bytes);
         Pointer : Stream_Element_Offset := Data'First;
      begin
         Put (Data, Pointer, Unsigned_16 (To - From + 1));
         Put (Data, Pointer, Unsigned_16 (Values'First));
         Put (Data, Pointer, Unsigned_16 (Values'Length));
         Data (Pointer) := Stream_Element (Bytes);
         Pointer := Pointer + 1;
         for Index in Values'Range loop
            Put (Data, Pointer, Values (Index));
         end loop;
         Send (Client, Reference, Unit, 23, Unsigned_16 (From), Data);
      end;
   end Send_FC23;

   procedure Send_FC24
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Address   : Word_Address;
                Unit      : Unit_No := 255
             )  is
   begin
      Send (Client, Reference, Unit, 24, Unsigned_16 (Address));
   end Send_FC24;

   procedure Sent (Client : in out MODBUS_Client) is
   begin
      if Queued_To_Send (Client) = 0 then
         Client.Payload_Data.Event.Set_Last (Clock);
      end if;
      Sent (State_Machine (Client));
   end Sent;

   procedure Set_RTU_Checksum_Mode
             (  Client : in out MODBUS_Client;
                Enable : Boolean
             )  is
   begin
      Client.Payload_Data.Enable := Enable;
   end Set_RTU_Checksum_Mode;

   procedure Set_RTU_Silence_Time
             (  Client  : in out MODBUS_Client;
                Silence : Duration
             )  is
   begin
      if Silence <= 0.0 then
         Client.Payload_Data.Event.Set (Time_Span_Zero);
      else
         Client.Payload_Data.Event.Set (To_Time_Span (Silence));
      end if;
   end Set_RTU_Silence_Time;

   procedure Wait_RTU_Silence_Time
             (  Client  : in out MODBUS_Client;
                Timeout : Duration := Duration'Last
             )  is
   begin
      Wait_Until_RTU_Silence_Time (Client, abs Timeout);
   end Wait_RTU_Silence_Time;

   procedure Wait_Until_RTU_Silence_Time
             (  Client   : in out MODBUS_Client;
                Deadline : Time
             )  is
      CRC : Payload_And_Checksum_Item renames Client.Payload_Data;
   begin
      if CRC.Enable and then CRC.Event.Get > Time_Span_Zero then
         if Queued_To_Send (Client) > 0 then
            select
               CRC.Event.Wait_For_Empty_Queue;
            or delay until Deadline;
               Raise_Exception (Timeout_Error'Identity, Expired_Text);
            end select;
         end if;
         begin
            declare
               Next : constant Time := CRC.Event.Get_Next;
            begin
               if Next > Deadline then
                  Raise_Exception
                  (  Timeout_Error'Identity,
                     Expired_Text
                  );
               end if;
               if Clock < Next then
                  delay until Next;
               end if;
            end;
         exception
            when Constraint_Error =>
               Raise_Exception (Timeout_Error'Identity, Expired_Text);
         end;
      end if;
   end Wait_Until_RTU_Silence_Time;

   procedure Words_Read
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Values    : Word_Array;
                Code      : Function_Code;
                Unit      : Unit_No
             )  is
      function Image (Values : Word_Array) return String is
         Result  : String (1..Values'Length * 5 - 1);
         Pointer : Integer := Result'First;
      begin
        for Index in Values'Range loop
           if Pointer /= 1 then
              Result (Pointer) := ' ';
              Pointer := Pointer + 1;
           end if;
           Put
           (  Destination => Result,
              Pointer     => Pointer,
              Value       => Integer (Values (Index)),
              Base        => 16,
              Field       => 4,
              Justify     => Strings_Edit.Right,
              Fill        => '0'
           );
        end loop;
        return Result;
      end Image;
   begin
      Trace
      (  MODBUS_Client'Class (Client),
         (  "FC" & Image (Integer (Code))
         &  " words read "
         &  Image (Values)
         &  " Ref:"
         &  Image (Integer (Reference))
         &  " Unit:"
         &  Image (Integer (Unit))
      )  );
   end Words_Read;

   procedure Words_Written
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Word_Address;
                To        : Word_Address;
                Code      : Function_Code;
                Unit      : Unit_No
             )  is
   begin
      Trace
      (  MODBUS_Client'Class (Client),
         (  "FC" & Image (Integer (Code))
         &  " words written at "
         &  Image (Integer (From))
         &  ".."
         &  Image (Integer (To))
         &  " Ref:"
         &  Image (Integer (Reference))
         &  " Unit:"
         &  Image (Integer (Unit))
      )  );
   end Words_Written;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Silence_Event
             )  is
   begin
      null;
   end Write;

   procedure Trace
             (  Client  : in out MODBUS_Client;
                Message : String
             )  is
   begin
      Trace
      (  Client.Listener.Factory.all,
         (  Get_Client_Name (Client.Listener.Factory.all, Client)
         &  ' '
         &  Message
      )  );
   end Trace;

   protected body Silence_Event is

      function Get return Time_Span is
      begin
         return Silence;
      end Get;

      function Get_Next return Time is
      begin
         return Last + Silence;
      end Get_Next;

      procedure Set (Silence_Time : Time_Span) is
      begin
         Silence := Silence_Time;
      end Set;

      procedure Set_Last (Last_Time : Time) is
      begin
         Empty := Queued_To_Send (Parent.Client.all) = 0;
         Last  := Last_Time;
      end Set_Last;

      entry Wait_For_Empty_Queue when Empty is
      begin
         if Queued_To_Send (Parent.Client.all) > 0 then
            Empty := False;
            requeue Wait_For_Empty_Queue with abort;
         end if;
      end Wait_For_Empty_Queue;

   end Silence_Event;

end GNAT.Sockets.Connection_State_Machine.MODBUS_Client;
