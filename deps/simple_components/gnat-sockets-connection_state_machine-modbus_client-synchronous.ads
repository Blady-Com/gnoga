--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     MODBUS_Client.Synchronous                   Spring, 2015       --
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

with Ada.Task_Identification;  use Ada.Task_Identification;

package GNAT.Sockets.Connection_State_Machine.
        MODBUS_Client.Synchronous is

   MODBUS_Error  : exception;
   Timeout_Error : exception;
   Cancel_Error  : exception;
--
-- MODBUS_Synchronous_Client -- A synchronous MODBUS client
--
--    Listener - The connections server object
--
-- The client provides  synchronous  operations to connect to the server
-- and to issue MODBUS requests with awaiting for completion.
--
   type MODBUS_Synchronous_Client
        (  Listener : access Connections_Server'Class
        )  is new Connection with private;
--
-- Cancel -- Waiting
--
--    Client - The connection object
--
-- This  procedure  is used to cancel a pending operation.  It does  not
-- influence  the communication  itself.  The operation  will  propagate
-- Cancel_Error if not yet completed.
--
   procedure Cancel (Client : in out MODBUS_Synchronous_Client);
--
-- Connect -- Connect to a server
--
--    Client         - The connection object
--    Host           - The host name or IP address
--    Port           - The port number
--    Max_Connect_No - Maximal number of connection attempts
--    Timeout        - For the operation
--
-- This procedure  is used  to connect to a remote server.  When already
-- connected  the current connection  is dropped  and another initiated.
-- The procedure ends when the host is connected.
--
-- Exceptions :
--
--    Cancel_Error  - Waiting canceled by a call to Cancel
--    Socket_Error  - Socket error
--    Status_Error  - Connection failure, e.g. attempts exhausted
--    Timeout_Error - Timeout expired
--
   procedure Connect
             (  Client         : in out MODBUS_Synchronous_Client;
                Host           : String;
                Port           : Port_Type := MODBUS_Port;
                Max_Connect_No : Positive  := Positive'Last;
                Timeout        : Duration  := Duration'Last
             );
--
-- FC1 -- Read Coils
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    From      - Address of the first bit to read
--    To        - Address of the last bit to read
--    Unit      - Identifies the target slave, if many connected
--    Timeout   - For the operation
--
-- Exceptions :
--
--    Cancel_Error  - Waiting was canceled by another task called Cancel
--    Data_Error    - No room in the output buffer (should not happen)
--    MODBUS_Error  - MODBUS server responded with an exception code
--    Status_Error  - Connection was lost during operation
--    Timeout_Error - Timeout expired
--    Use_Error     - Not connected
--
   function FC1
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               From      : Bit_Address;
               To        : Bit_Address;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Bit_Array;
--
-- FC2 -- Read Discrete Inputs
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    From      - Address of the first bit to read
--    To        - Address of the last bit to read
--    Unit      - Identifies the target slave, if many connected
--    Timeout   - For the operation
--
-- Exceptions :
--
--    Cancel_Error  - Waiting was canceled by another task called Cancel
--    Data_Error    - No room in the output buffer (should not happen)
--    MODBUS_Error  - MODBUS server responded with an exception code
--    Status_Error  - Connection was lost during operation
--    Timeout_Error - Timeout expired
--    Use_Error     - Not connected
--
   function FC2
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               From      : Bit_Address;
               To        : Bit_Address;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Bit_Array;
--
-- FC3 -- Read Holding Registers
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    From      - Address of the first word to read
--    To        - Address of the last word to read
--    Unit      - Identifies the target slave, if many connected
--    Timeout   - For the operation
--
-- Exceptions :
--
--    Cancel_Error  - Waiting was canceled by another task called Cancel
--    Data_Error    - No room in the output buffer (should not happen)
--    MODBUS_Error  - MODBUS server responded with an exception code
--    Status_Error  - Connection was lost during operation
--    Timeout_Error - Timeout expired
--    Use_Error     - Not connected
--
   function FC3
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               From      : Word_Address;
               To        : Word_Address;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Word_Array;
--
-- FC4 -- Read Input Registers
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    From      - Address of the first word to read
--    To        - Address of the last word to read
--    Unit      - Identifies the target slave, if many connected
--    Timeout   - For the operation
--
-- Exceptions :
--
--    Cancel_Error  - Waiting was canceled by another task called Cancel
--    Data_Error    - No room in the output buffer (should not happen)
--    MODBUS_Error  - MODBUS server responded with an exception code
--    Status_Error  - Connection was lost during operation
--    Timeout_Error - Timeout expired
--    Use_Error     - Not connected
--
   function FC4
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               From      : Word_Address;
               To        : Word_Address;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Word_Array;
--
-- FC5 -- Write Single Coil
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Address   - Of the bit to write
--    Value     - The value to write
--    Unit      - Identifies the target slave, if many connected
--    Timeout   - For the operation
--
-- Exceptions :
--
--    Cancel_Error  - Waiting was canceled by another task called Cancel
--    Data_Error    - No room in the output buffer (should not happen)
--    MODBUS_Error  - MODBUS server responded with an exception code
--    Status_Error  - Connection was lost during operation
--    Timeout_Error - Timeout expired
--    Use_Error     - Not connected
--
   procedure FC5
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Address   : Bit_Address;
                Value     : Boolean;
                Unit      : Unit_No  := 255;
                Timeout   : Duration := Duration'Last
             );
--
-- FC6 -- Write Single Holding Register
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Address   - Of the word to write
--    Value     - The value to write
--    Unit      - Identifies the target slave, if many connected
--    Timeout   - For the operation
--
-- Exceptions :
--
--    Cancel_Error  - Waiting was canceled by another task called Cancel
--    Data_Error    - No room in the output buffer (should not happen)
--    MODBUS_Error  - MODBUS server responded with an exception code
--    Status_Error  - Connection was lost during operation
--    Timeout_Error - Timeout expired
--    Use_Error     - Not connected
--
   procedure FC6
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Address   : Word_Address;
                Value     : Unsigned_16;
                Unit      : Unit_No  := 255;
                Timeout   : Duration := Duration'Last
             );
--
-- FC7 -- Read Exception Status
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Unit      - Identifies the target slave, if many connected
--    Timeout   - For the operation
--
-- Exceptions :
--
--    Use_Error - Not connected
--
   function FC7
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Unsigned_8;
--
-- FC15 -- Write Multiple Coils
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Values    - Bits to write
--    Unit      - Identifies the target slave, if many connected
--    Timeout   - For the operation
--
-- The bit  addresses to be written are identified by the array indices.
-- E.g. if 1,0,1 has to be written at 3..5, Values must be:
--
--    (3=>True, 4=>False, 5=>True)
--
-- Exceptions :
--
--    Cancel_Error  - Waiting was canceled by another task called Cancel
--    Data_Error    - No room in the output buffer (should not happen)
--    MODBUS_Error  - MODBUS server responded with an exception code
--    Status_Error  - Connection was lost during operation
--    Timeout_Error - Timeout expired
--    Use_Error     - Not connected
--
   procedure FC15
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Values    : Bit_Array;
                Unit      : Unit_No  := 255;
                Timeout   : Duration := Duration'Last
             );
--
-- FC16 -- Write Multiple Holding Registers
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Values    - Words to write
--    Unit      - Identifies the target slave, if many connected
--    Timeout   - For the operation
--
-- The word addresses to be written are identified by the array indices.
-- E.g. if 1,2,3 has to be written at 3..5, Values must be:
--
--    (3=>1, 4=>2, 5=>3)
--
-- Exceptions :
--
--    Cancel_Error  - Waiting was canceled by another task called Cancel
--    Data_Error    - No room in the output buffer (should not happen)
--    MODBUS_Error  - MODBUS server responded with an exception code
--    Status_Error  - Connection was lost during operation
--    Timeout_Error - Timeout expired
--    Use_Error     - Not connected
--
   procedure FC16
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Values    : Word_Array;
                Unit      : Unit_No  := 255;
                Timeout   : Duration := Duration'Last
             );
--
-- FC22 -- Mask Write Register
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Address   - Address of the word to mask
--    And_Mask  - The mask to apply using AND
--    Or_Mask   - The mask to apply using OR
--    Unit      - Identifies the target slave, if many connected
--    Timeout   - For the operation
--
-- Exceptions :
--
--    Cancel_Error  - Waiting was canceled by another task called Cancel
--    Data_Error    - No room in the output buffer (should not happen)
--    MODBUS_Error  - MODBUS server responded with an exception code
--    Status_Error  - Connection was lost during operation
--    Timeout_Error - Timeout expired
--    Use_Error     - Not connected
--
   procedure FC22
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Address   : Word_Address;
                And_Mask  : Unsigned_16;
                Or_Mask   : Unsigned_16;
                Unit      : Unit_No  := 255;
                Timeout   : Duration := Duration'Last
             );
--
-- FC23 -- Read/Write Multiple Registers
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    From      - Address of the first word to read
--    To        - Address of the last word to read
--    Values    - Words to write
--    Unit      - Identifies the target slave, if many connected
--    Timeout   - For the operation
--
-- The word addresses to be written are identified by the array indices.
-- E.g. if 1,2,3 has to be written at 3..5, Values must be:
--
--    (3=>1, 4=>2, 5=>3)
--
-- Exceptions :
--
--    Cancel_Error  - Waiting was canceled by another task called Cancel
--    Data_Error    - No room in the output buffer (should not happen)
--    MODBUS_Error  - MODBUS server responded with an exception code
--    Status_Error  - Connection was lost during operation
--    Timeout_Error - Timeout expired
--    Use_Error     - Not connected
--
   function FC23
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               From      : Word_Address;
               To        : Word_Address;
               Values    : Word_Array;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Word_Array;
--
-- FC24 -- Read FIFO queue
--
--    Client    - The MODBUS client
--    Reference - User-defined value repeated by the server in response
--    Address   - Address of the FIFO to read
--    Unit      - Identifies the target slave, if many connected
--    Timeout   - For the operation
--
-- Exceptions :
--
--    Cancel_Error  - Waiting was canceled by another task called Cancel
--    Data_Error    - No room in the output buffer (should not happen)
--    MODBUS_Error  - MODBUS server responded with an exception code
--    Status_Error  - Connection was lost during operation
--    Timeout_Error - Timeout expired
--    Use_Error     - Not connected
--
   function FC24
            (  Client    : access MODBUS_Synchronous_Client;
               Reference : Reference_ID;
               Address   : Word_Address;
               Unit      : Unit_No  := 255;
               Timeout   : Duration := Duration'Last
            )  return Word_Array;
private
   type Bits_Result is record
      Length : Bit_Address := 0;
      Data   : Bit_Array (1..256 * 8);
   end record;
   type Bits_Result_Ptr is access all Bits_Result;

   type Words_Result is record
      Length : Word_Address := 0;
      Data   : Word_Array (1..256);
   end record;
   type Words_Result_Ptr is access all Words_Result;

   type Result_Mode is (None, Bits, Words, Exception_Status, Failure);
   type Result_Type (Mode : Result_Mode := None) is record
      case Mode is
         when None =>
            null;
         when Bits =>
            Bits : Bits_Result_Ptr;
         when Words =>
            Words : Words_Result_Ptr;
         when Exception_Status =>
            Status : Unsigned_8;
         when Failure =>
            Error : Exception_Code;
      end case;
   end record;

   protected type Event_Type
                  (  Client : access MODBUS_Synchronous_Client'Class
                  )  is
      entry Cancel;
      entry Seize;
      procedure Release;
      procedure Complete;
      procedure Set;
      entry Wait_For_Completion;
      entry Wait_For_Connection;
      entry Wait_For_Release;
   private
      entry Lounge;

      Owner : Task_ID := Null_Task_ID;
      Count : Natural := 0;
      Ready : Boolean := False;
      Done  : Boolean := False;
      Down  : Boolean := True;
   end Event_Type;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Object : Event_Type
             );
   for Event_Type'Write use Write;

   type Holder (Client : access MODBUS_Synchronous_Client'Class) is
      new Ada.Finalization.Limited_Controlled with null record;
   procedure Finalize (Object : in out Holder);
   procedure Initialize (Object : in out Holder);

   type MODBUS_Synchronous_Client
        (  Listener : access Connections_Server'Class
        )  is new MODBUS_Client (Listener, 140) with
   record
      Result : Result_Type;
      Event  : Event_Type (MODBUS_Synchronous_Client'Unchecked_Access);
   end record;

   procedure Bits_Read
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Values    : Bit_Array;
                Code      : Function_Code;
                Unit      : Unit_No
             );
   procedure Bits_Written
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                From      : Bit_Address;
                To        : Bit_Address;
                Code      : Function_Code;
                Unit      : Unit_No
             );
   procedure Connected (Client : in out MODBUS_Synchronous_Client);
   procedure Exception_Status_Received
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Status    : Unsigned_8;
                Unit      : Unit_No
             );
   procedure Failed
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Error     : Exception_Code;
                Code      : Function_Code;
                Unit      : Unit_No
             );
   procedure Released (Client : in out MODBUS_Synchronous_Client);
   procedure Wait
             (  Client   : in out MODBUS_Synchronous_Client;
                Expected : Result_Mode;
                Timeout  : Duration
             );
   procedure Words_Read
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                Values    : Word_Array;
                Code      : Function_Code;
                Unit      : Unit_No
             );
   procedure Words_Written
             (  Client    : in out MODBUS_Synchronous_Client;
                Reference : Reference_ID;
                From      : Word_Address;
                To        : Word_Address;
                Code      : Function_Code;
                Unit      : Unit_No
             );

end GNAT.Sockets.Connection_State_Machine.MODBUS_Client.Synchronous;
