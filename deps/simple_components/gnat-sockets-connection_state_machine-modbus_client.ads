--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     MODBUS_Client                               Spring, 2015       --
--  Interface                                                         --
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

with Ada.Exceptions;  use Ada.Exceptions;

with GNAT.Sockets.Connection_State_Machine.Big_Endian.Unsigneds;
with GNAT.Sockets.Connection_State_Machine.Variable_Length_Arrays;

package GNAT.Sockets.Connection_State_Machine.MODBUS_Client is
   use Interfaces;
--
-- MODBUS_Port -- The TCP/IP port used
--
   MODBUS_Port : constant Port_Type := 502;
--
-- Exception_Code
--
   type Exception_Code is new Unsigned_8;
   Illegal_Function         : constant Exception_Code := 1;
   Illegal_Data_Address     : constant Exception_Code := 2;
   Illegal_Data_Value       : constant Exception_Code := 3;
   Slave_Device_Failure     : constant Exception_Code := 4;
   Acknowledge              : constant Exception_Code := 5;
   Slave_Device_Busy        : constant Exception_Code := 6;
   Negative_Acknowledge     : constant Exception_Code := 7;
   Memory_Parity_Error      : constant Exception_Code := 8;
   Gateway_Path_Unavailable : constant Exception_Code := 10;
   Target_Failed_To_Respond : constant Exception_Code := 11;
--
-- Function_Code
--
   type Function_Code  is new Unsigned_8;
   type Unit_No        is new Unsigned_8;
--
-- Bit_Address -- Of a bit input or coil
--
   type Bit_Address is new Unsigned_16;
--
-- Word_Address -- Of a word input or data
--
   type Word_Address is new Unsigned_16;
   type Reference_ID is new Unsigned_16;
   type Bit_Array    is array (Bit_Address range <>)  of Boolean;
   type Word_Array   is array (Word_Address range <>) of Unsigned_16;
--
-- MODBUS_Client -- An object implementing MODBUS client
--
--    Listener    - The connections server object
--    Output_Size - The size of the output buffer
--
-- The  Output_Size can  be limited to 140 elements or so,  if  only one
-- request will be sent at a time.  If the client wanted  to queue  more
-- than one request it should be increased correspondingly.
--
   type MODBUS_Client
        (  Listener    : access Connections_Server'Class;
           Output_Size : Buffer_Length
        )  is new State_Machine with private;
--
-- Bits_Read -- Completion notification
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Values    - Read values
--    Code      - Function code
--    Unit      - Identifies the target slave, if many connected
--
-- This procedure  is called when some bits were successfully read.  The
-- read values are set into  the parameter Values.  Note that  the array
-- indices do not correspond to the actual bit addresses.  Reference has
-- the value passed to the server with the request. Code is the function
-- code 1 or 2.  Unit  is  the slave responding  if  more than one.  The
-- default implementation traces.
--
   procedure Bits_Read
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Values    : Bit_Array;
                Code      : Function_Code;
                Unit      : Unit_No
             );
--
-- Bits_Written -- Completion notification
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    From      - The first bit address
--    To        - The last bit address
--    Code      - Function code
--    Unit      - Identifies the target slave, if many connected
--
-- This procedure  is called when  some bits  were successfully written.
-- From..To is the written range of addresses.  Reference  has the value
-- passed to the server with the request. Code is the function code 5 or
-- 15.  Unit  is the slave responding  if  more  than one.  The  default
-- implementation traces.
--
   procedure Bits_Written
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Bit_Address;
                To        : Bit_Address;
                Code      : Function_Code;
                Unit      : Unit_No
             );
--
-- Can_Send -- Check if a request can be sent
--
--    Client - The MODBUS client
--    Code   - The function code
--    Count  - Number of data elements, e.g. bits in FC15
--
-- The parameter Count is only relevant to FC15, FC16 and FC23. For FC15
-- it is the number of bits written.  For FC16 and FC23 it is the number
-- of words.
--
-- Returns :
--
--    True if there is space in the output buffer to send the request
--
-- Exceptions :
--
--    Constraint_Error - Unsupported function code
--
   function Can_Send
            (  Client : MODBUS_Client;
               Code   : Function_Code;
               Count  : Natural := 0
            )  return Boolean;
--
-- Exception_Status_Received -- Completion notification
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Status    - The exception status code
--    Unit      - Identifies the target slave, if many connected
--
-- This procedure is called when FC7 request is completed. Status is the
-- the exception  status  (a predefined range of coils).  Reference  has
-- the value passed to  the server with  the request. Unit  is the slave
-- responding  if  more   than one.  The  default implementation traces.
--
   procedure Exception_Status_Received
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Status    : Unsigned_8;
                Unit      : Unit_No
             );
--
-- Failed -- Exception response notification
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Error     - The exception code reported
--    Code      - Function code
--    Unit      - Identifies the target slave, if many connected
--
-- This procedure  is  called  when  server  responds with an exception.
-- Error is the exception code.  Reference  has  the value passed to the
-- server with the request.  Code is the function  code that  caused the
-- exception.  Unit  is  the  slave responding  if  more  than one.  The
-- default implementation traces.
--
   procedure Failed
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Error     : Exception_Code;
                Code      : Function_Code;
                Unit      : Unit_No
             );
--
-- Get_Request_Length -- Get total length of a request
--
--    Client - The MODBUS client
--    Code   - The function code
--    Count  - Number of data elements, e.g. bits in FC15
--
-- The parameter Count is only relevant to FC15, FC16 and FC23. For FC15
-- it is the number of bits written.  For FC16 and FC23 it is the number
-- of words.
--
-- Returns :
--
--    Number of stream elements needed to send the request
--
-- Exceptions :
--
--    Constraint_Error - Unsupported function code
--
   function Get_Request_Length
            (  Client : MODBUS_Client;
               Code   : Function_Code;
               Count  : Natural := 0
            )  return Stream_Element_Count;
--
-- Send_FC1 -- Read Coils
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    From      - Address of the first bit to read
--    To        - Address of the last bit to read
--    Unit      - Identifies the target slave, if many connected
--
-- Exceptions :
--
--    Data_Error - No room in the output buffer
--    Use_Error  - Not connected
--
   procedure Send_FC1
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Bit_Address;
                To        : Bit_Address;
                Unit      : Unit_No := 255
             );
--
-- Send_FC2 -- Read Discrete Inputs
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    From      - Address of the first bit to read
--    To        - Address of the last bit to read
--    Unit      - Identifies the target slave, if many connected
--
-- Exceptions :
--
--    Data_Error - No room in the output buffer
--    Use_Error  - Not connected
--
   procedure Send_FC2
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Bit_Address;
                To        : Bit_Address;
                Unit      : Unit_No := 255
             );
--
-- Send_FC3 -- Read Holding Registers
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    From      - Address of the first word to read
--    To        - Address of the last word to read
--    Unit      - Identifies the target slave, if many connected
--
-- Exceptions :
--
--    Data_Error - No room in the output buffer
--    Use_Error  - Not connected
--
   procedure Send_FC3
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Word_Address;
                To        : Word_Address;
                Unit      : Unit_No := 255
             );
--
-- Send_FC4 -- Read Input Registers
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    From      - Address of the first word to read
--    To        - Address of the last word to read
--    Unit      - Identifies the target slave, if many connected
--
-- Exceptions :
--
--    Data_Error - No room in the output buffer
--    Use_Error  - Not connected
--
   procedure Send_FC4
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Word_Address;
                To        : Word_Address;
                Unit      : Unit_No := 255
             );
--
-- Send_FC5 -- Write Single Coil
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Address   - Of the bit to write
--    Value     - The value to write
--    Unit      - Identifies the target slave, if many connected
--
-- Exceptions :
--
--    Data_Error - No room in the output buffer
--    Use_Error  - Not connected
--
   procedure Send_FC5
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Address   : Bit_Address;
                Value     : Boolean;
                Unit      : Unit_No := 255
             );
--
-- Send_FC6 -- Write Single Holding Register
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Address   - Of the word to write
--    Value     - The value to write
--    Unit      - Identifies the target slave, if many connected
--
-- Exceptions :
--
--    Data_Error - No room in the output buffer
--    Use_Error  - Not connected
--
   procedure Send_FC6
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Address   : Word_Address;
                Value     : Unsigned_16;
                Unit      : Unit_No := 255
             );
--
-- Send_FC7 -- Read Exception Status
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Unit      - Identifies the target slave, if many connected
--
-- Exceptions :
--
--    Data_Error - No room in the output buffer
--    Use_Error  - Not connected
--
   procedure Send_FC7
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Unit      : Unit_No := 255
             );
--
-- Send_FC15 -- Write Multiple Coils
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Values    - Bits to write
--    Unit      - Identifies the target slave, if many connected
--
-- The bit  addresses to be written are identified by the array indices.
-- E.g. if 1,0,1 has to be written at 3..5, Values must be:
--
--    (3=>True, 4=>False, 5=>True)
--
-- Exceptions :
--
--    Data_Error - No room in the output buffer
--    Use_Error  - Not connected
--
   procedure Send_FC15
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Values    : Bit_Array;
                Unit      : Unit_No := 255
             );
--
-- Send_FC16 -- Write Multiple Holding Registers
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Values    - Words to write
--    Unit      - Identifies the target slave, if many connected
--
-- The word addresses to be written are identified by the array indices.
-- E.g. if 1,2,3 has to be written at 3..5, Values must be:
--
--    (3=>1, 4=>2, 5=>3)
--
-- Exceptions :
--
--    Data_Error - No room in the output buffer
--    Use_Error  - Not connected
--
   procedure Send_FC16
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Values    : Word_Array;
                Unit      : Unit_No := 255
             );
--
-- Send_FC22 -- Mask Write Register
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Address   - Address of the word to mask
--    And_Mask  - The mask to apply using AND
--    Or_Mask   - The mask to apply using OR
--    Unit      - Identifies the target slave, if many connected
--
-- Exceptions :
--
--    Data_Error - No room in the output buffer
--    Use_Error  - Not connected
--
   procedure Send_FC22
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Address   : Word_Address;
                And_Mask  : Unsigned_16;
                Or_Mask   : Unsigned_16;
                Unit      : Unit_No := 255
             );
--
-- Send_FC23 -- Read/Write Multiple Registers
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    From      - Address of the first word to read
--    To        - Address of the last word to read
--    Values    - Words to write
--    Unit      - Identifies the target slave, if many connected
--
-- The word addresses to be written are identified by the array indices.
-- E.g. if 1,2,3 has to be written at 3..5, Values must be:
--
--    (3=>1, 4=>2, 5=>3)
--
-- Exceptions :
--
--    Data_Error - No room in the output buffer
--    Use_Error  - Not connected
--
   procedure Send_FC23
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Word_Address;
                To        : Word_Address;
                Values    : Word_Array;
                Unit      : Unit_No := 255
             );
--
-- Send_FC24 -- Read FIFO queue
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Address   - Address of the FIFO to read
--    Unit      - Identifies the target slave, if many connected
--
-- Exceptions :
--
--    Data_Error - No room in the output buffer
--    Use_Error  - Not connected
--
   procedure Send_FC24
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Address   : Word_Address;
                Unit      : Unit_No := 255
             );
--
-- Words_Read -- Completion notification
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Values    - Read values
--    Code      - Function code
--    Unit      - Identifies the target slave, if many connected
--
-- This procedure is called when some words were successfully read.  The
-- read values are in parameter Values.  Note that  the array indices do
-- not correspond to the actual word addresses.  Reference has the value
-- passed to the server with the request.  Code  is the function code 3,
-- 4 or  23.  Unit  is  the  slave  responding  if  more  than one.  The
-- default implementation traces.
--
   procedure Words_Read
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                Values    : Word_Array;
                Code      : Function_Code;
                Unit      : Unit_No
             );
--
-- Words_Written -- Completion notification
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    From      - The first word address
--    To        - The last word address
--    Code      - Function code
--    Unit      - Identifies the target slave, if many connected
--
-- This procedure  is called when  some words were successfully written.
-- From..To is the written range of addresses.  Reference  has the value
-- passed to the server with the request. Code is the function code 6 or
-- 16.  Unit  is the slave responding  if  more  than one.  The  default
-- implementation traces.
--
   procedure Words_Written
             (  Client    : in out MODBUS_Client;
                Reference : Reference_ID;
                From      : Word_Address;
                To        : Word_Address;
                Code      : Function_Code;
                Unit      : Unit_No
             );
--
-- Trace -- Tracing
--
--    Client  - The connection object
--    Message - To write into the trace
--
   procedure Trace
             (  Client  : in out MODBUS_Client;
                Message : String
             );
--
-- Error_Text -- Exception code mnemonic
--
--    Code - The exception code
--
-- Returns :
--
--    The corresponding error text
--
   function Error_Text (Code : Exception_Code) return String;

private
   use GNAT.Sockets.Connection_State_Machine.Big_Endian.Unsigneds;
   use GNAT.Sockets.Connection_State_Machine.Variable_Length_Arrays;

   pragma Assert (Stream_Element'Size = 8);

   type Set_Data_Length (Client : access MODBUS_Client'Class) is
      new Data_Item with null record;
   procedure Feed
             (  Item    : in out Set_Data_Length;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Machine : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   type MODBUS_Client
        (  Listener    : access Connections_Server'Class;
           Output_Size : Buffer_Length
        )  is new State_Machine (128 + 16, Output_Size) with
   record
         -- Response fields
      Transaction_ID : Unsigned_16_Data_Item;
      Protocol_ID    : Unsigned_16_Data_Item;
      Length_Field   : Unsigned_16_Data_Item;
      Data_Length    : Set_Data_Length (MODBUS_Client'Unchecked_Access);
      Unit_ID        : Unsigned_8_Data_Item;
      Function_Code  : Unsigned_8_Data_Item;
      Payload_Data   : Array_Data_Item (128);
   end record;

   procedure Prepare_To_Send
             (  Client : in out MODBUS_Client;
                Length : Stream_Element_Count
             );
   procedure Process_Packet (Client : in out MODBUS_Client);

end GNAT.Sockets.Connection_State_Machine.MODBUS_Client;
