--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     MODBUS_Client.Synchronous                   Spring, 2015       --
--  Implementation                                                    --
--                                Last revision :  09:02 17 Oct 2020  --
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
with Ada.Real_Time;      use Ada.Real_Time;
with Synchronization;    use Synchronization;

package body GNAT.Sockets.Connection_State_Machine.MODBUS_Client.
             Synchronous is

   procedure Bits_Read
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Values    : Bit_Array;
                Code      : Function_Code;
                Unit      : Unit_No
             )  is
   begin
      if Client.Result.Mode = Bits then
         declare
            Result : Bits_Result renames Client.Result.Bits.all;
         begin
            Result.Length := Values'Length;
            Result.Data (1..Values'Length) := Values;
         end;
      else
         Client.Result := (Mode => None);
      end if;
      Client.Event.Complete;
   end Bits_Read;

   procedure Bits_Written
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                From      : Bit_Address;
                To        : Bit_Address;
                Code      : Function_Code;
                Unit      : Unit_No
             )  is
   begin
      Client.Result := (Mode => None);
      Client.Event.Complete;
   end Bits_Written;

   procedure Cancel (Client : in out MODBUS_Synchronous_Client) is
   begin
      Client.Event.Cancel;
   end Cancel;

   procedure Connect
             (  Client         : in out MODBUS_Synchronous_Client;
                Host           : String;
                Port           : Port_Type := MODBUS_Port;
                Max_Connect_No : Positive  := Positive'Last;
                Timeout        : Duration  := Duration'Last
             )  is
      Lock     : Holder (Client'Unchecked_Access);
      Deadline : constant Time := abs Timeout;
   begin
      Shutdown (Client);
      select
         Client.Event.Wait_For_Release;
      or delay until Deadline;
         Raise_Exception
         (  Timeout_Error'Identity,
            "Shutdown timeout expired"
         );
      end select;
      Connect
      (  Client.Listener.all,
         Client'Unchecked_Access,
         Host,
         Port,
         Max_Connect_No
      );
      select
         Client.Event.Wait_For_Connection;
      or delay until Deadline;
         Raise_Exception
         (  Timeout_Error'Identity,
            "Connection timeout expired"
         );
      end select;
   end Connect;

   procedure Connected (Client : in out MODBUS_Synchronous_Client) is
   begin
       Connected (MODBUS_Client (Client));
       Client.Event.Set;
   end Connected;

   procedure Exception_Status_Received
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Status    : Unsigned_8;
                Unit      : Unit_No
             )  is
   begin
      Client.Result := (Exception_Status, Status);
      Client.Event.Complete;
   end Exception_Status_Received;

   procedure Failed
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Error     : Exception_Code;
                Code      : Function_Code;
                Unit      : Unit_No
             )  is
   begin
      Client.Result := (Failure, Error);
      Client.Event.Complete;
   end Failed;

   function FC1
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               From      : Bit_Address;
               To        : Bit_Address;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Bit_Array is
      Deadline : constant Time := abs Timeout;
      Lock     : Holder (Client);
      Result   : aliased Bits_Result;
   begin
      Client.Result := (Bits, Result'Unchecked_Access);
      Wait_Until_RTU_Silence_Time (Client.all, Deadline);
      Send_FC1 (Client.all, Reference, From, To, Unit);
      Wait (Client.all, Bits, Deadline);
      return Result.Data (1..Result.Length);
   end FC1;

   function FC2
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               From      : Bit_Address;
               To        : Bit_Address;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Bit_Array is
      Deadline : constant Time := abs Timeout;
      Lock     : Holder (Client);
      Result   : aliased Bits_Result;
   begin
      Client.Result := (Bits, Result'Unchecked_Access);
      Wait_Until_RTU_Silence_Time (Client.all, Deadline);
      Send_FC2 (Client.all, Reference, From, To, Unit);
      Wait (Client.all, Bits, Deadline);
      return Result.Data (1..Result.Length);
   end FC2;

   function FC3
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               From      : Word_Address;
               To        : Word_Address;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Word_Array is
      Deadline : constant Time := abs Timeout;
      Lock     : Holder (Client);
      Result   : aliased Words_Result;
   begin
      Client.Result := (Words, Result'Unchecked_Access);
      Wait_Until_RTU_Silence_Time (Client.all, Deadline);
      Send_FC3 (Client.all, Reference, From, To, Unit);
      Wait (Client.all, Words, Deadline);
      return Result.Data (1..Result.Length);
   end FC3;

   function FC4
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               From      : Word_Address;
               To        : Word_Address;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Word_Array is
      Deadline : constant Time := abs Timeout;
      Lock     : Holder (Client);
      Result   : aliased Words_Result;
   begin
      Client.Result := (Words, Result'Unchecked_Access);
      Wait_Until_RTU_Silence_Time (Client.all, Deadline);
      Send_FC4 (Client.all, Reference, From, To, Unit);
      Wait (Client.all, Words, Deadline);
      return Result.Data (1..Result.Length);
   end FC4;

   procedure FC5
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Address   : Bit_Address;
                Value     : Boolean;
                Unit      : Unit_No  := 255;
                Timeout   : Duration := Duration'Last
             )  is
      Deadline : constant Time := abs Timeout;
      Lock     : Holder (Client'Unchecked_Access);
   begin
      Wait_Until_RTU_Silence_Time (Client, Deadline);
      Send_FC5 (Client, Reference, Address, Value, Unit);
      Wait (Client, None, Deadline);
   end FC5;

   procedure FC6
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Address   : Word_Address;
                Value     : Unsigned_16;
                Unit      : Unit_No  := 255;
                Timeout   : Duration := Duration'Last
             )  is
      Deadline : constant Time := abs Timeout;
      Lock     : Holder (Client'Unchecked_Access);
   begin
      Wait_Until_RTU_Silence_Time (Client, Deadline);
      Send_FC6 (Client, Reference, Address, Value, Unit);
      Wait (Client, None, Deadline);
   end FC6;

   function FC7
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Unsigned_8 is
      Deadline : constant Time := abs Timeout;
      Lock     : Holder (Client);
   begin
      Wait_Until_RTU_Silence_Time (Client.all, Deadline);
      Send_FC7 (Client.all, Reference, Unit);
      Wait (Client.all, Exception_Status, Deadline);
      return Client.Result.Status;
   end FC7;

   procedure FC15
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Values    : Bit_Array;
                Unit      : Unit_No  := 255;
                Timeout   : Duration := Duration'Last
             )  is
      Deadline : constant Time := abs Timeout;
      Lock     : Holder (Client'Unchecked_Access);
   begin
      Wait_Until_RTU_Silence_Time (Client, Deadline);
      Send_FC15 (Client, Reference, Values, Unit);
      Wait (Client, None, Deadline);
   end FC15;

   procedure FC16
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Values    : Word_Array;
                Unit      : Unit_No  := 255;
                Timeout   : Duration := Duration'Last
             )  is
      Deadline : constant Time := abs Timeout;
      Lock     : Holder (Client'Unchecked_Access);
   begin
      Wait_Until_RTU_Silence_Time (Client, Deadline);
      Send_FC16 (Client, Reference, Values, Unit);
      Wait (Client, None, Deadline);
   end FC16;

   procedure FC22
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Address   : Word_Address;
                And_Mask  : Unsigned_16;
                Or_Mask   : Unsigned_16;
                Unit      : Unit_No  := 255;
                Timeout   : Duration := Duration'Last
             )  is
      Deadline : constant Time := abs Timeout;
      Lock     : Holder (Client'Unchecked_Access);
   begin
      Wait_Until_RTU_Silence_Time (Client, Deadline);
      Send_FC22 (Client, Reference, Address, And_Mask, Or_Mask, Unit);
      Wait (Client, None, Deadline);
   end FC22;

   function FC23
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               From      : Word_Address;
               To        : Word_Address;
               Values    : Word_Array;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Word_Array is
      Deadline : constant Time := abs Timeout;
      Lock     : Holder (Client);
      Result   : aliased Words_Result;
   begin
      Client.Result := (Words, Result'Unchecked_Access);
      Wait_Until_RTU_Silence_Time (Client.all, Deadline);
      Send_FC23 (Client.all, Reference, From, To, Values, Unit);
      Wait (Client.all, Words, Deadline);
      return Result.Data (1..Result.Length);
   end FC23;

   function FC24
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               Address   : Word_Address;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Word_Array is
      Deadline : constant Time := abs Timeout;
      Lock     : Holder (Client);
      Result   : aliased Words_Result;
   begin
      Client.Result := (Words, Result'Unchecked_Access);
      Wait_Until_RTU_Silence_Time (Client.all, Deadline);
      Send_FC24 (Client.all, Reference, Address, Unit);
      Wait (Client.all, Words, Deadline);
      return Result.Data (1..Result.Length);
   end FC24;

   procedure Finalize (Object : in out Holder) is
   begin
      Object.Client.Event.Release;
   end Finalize;

   function Get_RTU_Checksum_Mode
            (  Client : MODBUS_Synchronous_Client
            )  return Boolean is
   begin
      return Get_RTU_Checksum_Mode (MODBUS_Client (Client));
   end Get_RTU_Checksum_Mode;

   function Get_RTU_Silence_Time
            (  Client : MODBUS_Synchronous_Client
            )  return Duration is
   begin
      return Get_RTU_Silence_Time (MODBUS_Client (Client));
   end Get_RTU_Silence_Time;

   procedure Initialize (Object : in out Holder) is
   begin
      Object.Client.Event.Seize;
   end Initialize;

   procedure Released (Client : in out MODBUS_Synchronous_Client) is
   begin
      Released (MODBUS_Client (Client));
      Client.Event.Set;
   end Released;

   procedure Set_RTU_Checksum_Mode
             (  Client : in out MODBUS_Synchronous_Client;
                Enable : Boolean
             )  is
   begin
      Set_RTU_Checksum_Mode (MODBUS_Client (Client), Enable);
   end Set_RTU_Checksum_Mode;

   procedure Set_RTU_Silence_Time
             (  Client  : in out MODBUS_Synchronous_Client;
                Silence : Duration
             )  is
   begin
      Set_RTU_Silence_Time (MODBUS_Client (Client), Silence);
   end Set_RTU_Silence_Time;

   procedure Wait
             (  Client   : in out MODBUS_Synchronous_Client;
                Expected : Result_Mode;
                Deadline : Time
             )  is
   begin
      select
         Client.Event.Wait_For_Completion;
      or delay until Deadline;
         Raise_Exception (Timeout_Error'Identity, Expired_Text);
      end select;
      if Client.Result.Mode /= Expected then
         if Client.Result.Mode = Failure then
            Raise_Exception
            (  MODBUS_Error'Identity,
               Error_Text (Client.Result.Error)
            );
         else
            Raise_Exception
            (  Program_Error'Identity,
               "Unexpected response result"
            );
         end if;
      end if;
   end Wait;

   procedure Words_Read
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Values    : Word_Array;
                Code      : Function_Code;
                Unit      : Unit_No
             )  is
   begin
      if Client.Result.Mode = Words then
         declare
            Result : Words_Result renames Client.Result.Words.all;
         begin
            Result.Length := Values'Length;
            Result.Data (1..Values'Length) := Values;
         end;
      else
         Client.Result := (Mode => None);
      end if;
      Client.Event.Complete;
   end Words_Read;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Object : Event_Type
             )  is
   begin
      null;
   end Write;

   procedure Words_Written
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                From      : Word_Address;
                To        : Word_Address;
                Code      : Function_Code;
                Unit      : Unit_No
             )  is
   begin
      Client.Result := (Mode => None);
      Client.Event.Complete;
   end Words_Written;

   protected body Event_Type is

      entry Cancel when Wait_For_Completion'Count = 0
               and then Wait_For_Connection'Count = 0
               and then Wait_For_Release'Count = 0 is
      begin
         null;
      end Cancel;

      entry Lounge when Owner = Null_Task_ID is
      begin
         Owner := Lounge'Caller;
         Count := 1;
         Done  := False;
      end Lounge;

      entry Seize when True is
      begin
         if Seize'Caller = Owner then
            Count := Count + 1;
            Done  := False;
         else
            requeue Lounge with abort;
         end if;
      end Seize;

      procedure Release is
      begin
         if Owner = Current_Task then
            Count := Count - 1;
            if Count = 0 then
               Owner := Null_Task_ID;
            end if;
         else
            Raise_Exception
            (  Program_Error'Identity,
               "Mutex ownership error"
            );
         end if;
      end Release;

      procedure Complete is
      begin
         Done := True;
      end Complete;

      procedure Set is
      begin
         case Get_Session_State (Client.all) is
            when Session_Down =>
               Ready := False;
               Down  := True;
            when Session_Disconnected | Session_Connecting |
                 Session_Handshaking  | Session_Busy       =>
               Ready := False;
               Down  := False;
            when Session_Active | Session_Connected =>
               Ready := True;
               Down  := False;
         end case;
      end Set;

      entry Wait_For_Completion
         when Done or else Down or else Event_Type.Cancel'Count > 0 is
      begin
         if Done then
            return;
         end if;
         case Get_Session_State (Client.all) is
            when Session_Down =>
               Ready := False;
               Down  := True;
               declare
                  Error : Exception_Occurrence;
               begin
                  Get_Occurrence (Client.all, Error);
                  if Exception_Identity (Error) /= Null_Id then
                     Reraise_Occurrence (Error);
                  else
                     Raise_Exception
                     (  Status_Error'Identity,
                        "Unable to connect"
                     );
                  end if;
               end;
            when Session_Disconnected | Session_Connecting |
                 Session_Handshaking  | Session_Busy       =>
               Ready := False;
               Down  := False;
            when Session_Active | Session_Connected =>
               Ready := True;
               Down  := False;
         end case;
         Raise_Exception
         (  Status_Error'Identity,
            "Connection was lost during execution of the request"
         );
      end Wait_For_Completion;

      entry Wait_For_Release
         when Down or else Event_Type.Cancel'Count > 0 is
      begin
         if Get_Session_State (Client.all) /= Session_Down then
            Down := False;
            if Event_Type.Cancel'Count > 0 then
               Raise_Exception (Cancel_Error'Identity, "Canceled");
            else
               requeue Wait_For_Release with abort;
            end if;
         end if;
      end Wait_For_Release;

      entry Wait_For_Connection
         when Ready or else Down or else Event_Type.Cancel'Count > 0 is
      begin
         case Get_Session_State (Client.all) is
            when Session_Down =>
               Ready := False;
               Down  := True;
               declare
                  Error : Exception_Occurrence;
               begin
                  Get_Occurrence (Client.all, Error);
                  if Exception_Identity (Error) /= Null_Id then
                     Reraise_Occurrence (Error);
                  else
                     Raise_Exception
                     (  Status_Error'Identity,
                        "Unable to connect"
                     );
                  end if;
               end;
            when Session_Disconnected | Session_Connecting |
                 Session_Handshaking  | Session_Busy       =>
               Ready := False;
               Down  := False;
               if Event_Type.Cancel'Count > 0 then
                  Raise_Exception (Cancel_Error'Identity, "Canceled");
               else
                  requeue Wait_For_Connection with abort;
               end if;
            when Session_Active | Session_Connected =>
               Ready := True;
               Down  := False;
         end case;
      end Wait_For_Connection;

   end Event_Type;

end GNAT.Sockets.Connection_State_Machine.MODBUS_Client.Synchronous;
