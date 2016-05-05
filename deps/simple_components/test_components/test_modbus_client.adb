--                                                                    --
--  procedure Test_MODBUS_Client    Copyright (c)  Dmitry A. Kazakov  --
--  MODBUS client test                             Luebeck            --
--                                                 Spring, 2015       --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;     use Ada.Text_IO.Text_Streams;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;
with Interfaces;                   use Interfaces;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Strings_Edit.Streams;         use Strings_Edit.Streams;

with GNAT.Sockets.Connection_State_Machine.MODBUS_Client.Synchronous;

procedure Test_MODBUS_Client is
   Timeout : constant Duration := 1.0;

   function Image
            (  Values : GNAT.Sockets.Connection_State_Machine.
                        MODBUS_Client.Bit_Array
            )  return String is
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

   function Image
            (  Values : GNAT.Sockets.Connection_State_Machine.
                        MODBUS_Client.Word_Array
            )  return String is
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

   procedure Test_Asynchronous is
      use GNAT.Sockets.Connection_State_Machine.MODBUS_Client;
      Factory   : aliased Connections_Factory;
      Server    : aliased Connections_Server (Factory'Access, 0);
      Reference : Handle;
   begin
      Trace_On
      (  Factory  => Factory,
         Received => GNAT.Sockets.Server.Trace_Decoded,
         Sent     => GNAT.Sockets.Server.Trace_Decoded
      );
      Set
      (  Reference,
         new MODBUS_Client (Server'Unchecked_Access, 140)
      );
      declare
         Client : MODBUS_Client renames
                  MODBUS_Client (Ptr (Reference).all);
      begin
         Connect
         (  Server,
            Client'Unchecked_Access,
            "192.168.2.100", -- "127.0.0.1",
            MODBUS_Port
         );
         while not Is_Connected (Client) loop -- Busy waiting
            delay Timeout;
         end loop;
         Put_Line ("MODBUS client connected");
         Send_FC5 (Client, 100, 1, True);
         delay Timeout;
         Send_FC15 (Client, 101, (5..15=>True, 16=>False, 17=>True));
         delay Timeout;
         Send_FC1 (Client, 102, 1, 3);
         delay Timeout;
         Send_FC2 (Client, 103, 10, 20);
         delay Timeout;
         Send_FC6 (Client, 104, 0, 16#100#);
         delay Timeout;
         Send_FC16 (Client, 105, (2=>16#101#,3=>16#102#,4=>16#103#));
         delay Timeout;
         Send_FC23
         (  Client,
            105,
            0, 4,
            (5=>16#201#,6=>16#202#,7=>16#203#)
         );
         delay Timeout;
         Send_FC24 (Client, 106, 16#6000#);
         delay Timeout;
         Send_FC7 (Client, 106);
         delay Timeout;
      end;
   end Test_Asynchronous;

   procedure Test_Synchronous is
      use GNAT.Sockets.Connection_State_Machine.
          MODBUS_Client.Synchronous;
      Factory   : aliased Connections_Factory;
      Server    : aliased Connections_Server (Factory'Access, 0);
      Reference : Handle;
   begin
      Trace_On
      (  Factory  => Factory,
         Received => GNAT.Sockets.Server.Trace_Decoded,
         Sent     => GNAT.Sockets.Server.Trace_Decoded
      );
      Set
      (  Reference,
         new MODBUS_Synchronous_Client (Server'Unchecked_Access)
      );
      declare
         Client : MODBUS_Synchronous_Client renames
                  MODBUS_Synchronous_Client (Ptr (Reference).all);
      begin
         Connect (Client, "192.168.2.100");
         Put_Line ("MODBUS client connected");
         FC5 (Client, 100, 1, True);
         Put_Line ("FC1 > OK");
         FC15 (Client, 101, (5..15=>True, 16=>False, 17=>True));
         Put_Line ("FC15 > OK");
         Put_Line ("FC1 > " & Image (FC1 (Client'Access, 102, 1, 3)));
         Put_Line ("FC2 > " & Image (FC2 (Client'Access, 103, 10, 20)));
         FC6 (Client, 104, 0, 16#100#);
         Put_Line ("FC6 > OK");
         FC16 (Client, 105, (2=>16#101#,3=>16#102#,4=>16#103#));
         Put_Line
         (  "FC23 > "
         &  Image
            (  FC23
               (  Client'Access,
                  105,
                  0, 4,
                  (5=>16#201#,6=>16#202#,7=>16#203#)
         )  )  );
         Put_Line
         (  "FC24 > "
         &  Image (FC24 (Client'Access, 106, 16#6000#))
         );
         Put_Line
         (  "FC7 > " & Unsigned_8'Image (FC7 (Client'Access, 106))
         );
      end;
   end Test_Synchronous;
begin
--   Test_Asynchronous;
   Test_Synchronous;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_MODBUS_Client;
