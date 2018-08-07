--                                                                    --
--  package GNAT.Sockets.NTP        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2017       --
--                                                                    --
--                                Last revision :  23:22 29 Sep 2017  --
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

with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Ada.Streams;              use Ada.Streams;
with Interfaces;               use Interfaces;

package body GNAT.Sockets.NTP is

   NTP_Packet_Size : constant := 48;
   --
   -- RFC 5905: Official NTP era begins at 1 Jan 1900. We cannot have it
   -- in  Ada.Calendar.Time,  so taking a later time. Note Time_Zone = 0
   -- in order to have it UTC
   --
   Era : constant Time := Time_Of (1999, 12, 31, Time_Zone => 0);
   --
   -- RFC 5905: seconds since 1 Jan 1900 to 31 Dec 1999
   --
   Era_Offset : constant := 3_155_587_200;

   function To_Addr (Host : String) return Inet_Addr_Type is
   begin
      for Index in Host'Range loop
         case Host (Index) is
            when '.' | '0'..'9' =>
               null;
            when others =>
               return Addresses (Get_Host_By_Name (Host), 1);
         end case;
      end loop;
      return Inet_Addr (Host);
   end To_Addr;

   function Get_Time
            (  Server  : String;
               Timeout : Timeval_Duration := 10.0;
               Adjust  : Boolean := True
            )  return Time is
      Socket   : Socket_Type;
      Address  : Sock_Addr_Type;
      Offset   : Duration := 0.0;  -- Round-trip time
      Seconds  : Unsigned_32;
      Fraction : Unsigned_32;
      Last     : Stream_Element_Offset;
      Data     : Stream_Element_Array (1..NTP_Packet_Size) :=
                    (  1  => 2#1110_0011#, -- LI, Version, Mode
                       2  => 0,            -- Stratum, or type of clock
                       3  => 0,            -- Polling Interval
                       4  => 16#EC#,       -- Peer Clock Precision
                       13 => 49,
                       14 => 16#4E#,
                       15 => 49,
                       16 => 52,
                       others => 0
                    );
   begin
      Address.Addr := To_Addr (Server);
      Address.Port := 123; -- NTP port
      Create_Socket (Socket, Family_Inet, Socket_Datagram);
      Set_Socket_Option
      (  Socket,
         Socket_Level,
         (Receive_Timeout, Timeout)
      );
      if Adjust then
         declare
            Start : constant Time := Clock; -- Exchange begin
         begin
            Send_Socket    (Socket, Data, Last, Address);
            Receive_Socket (Socket, Data, Last, Address);
            Offset := (Clock - Start) / 2.0;
         end;
      else
         Send_Socket    (Socket, Data, Last, Address);
         Receive_Socket (Socket, Data, Last, Address);
      end if;
      if Last /= Data'Last then
         Raise_Exception (Data_Error'Identity, "Mangled NTP response");
      end if;
      Seconds := (  Unsigned_32 (Data (41)) * 2**24
                 +  Unsigned_32 (Data (42)) * 2**16
                 +  Unsigned_32 (Data (43)) * 2**8
                 +  Unsigned_32 (Data (44))
                 -  Era_OFfset
                 );
      Fraction := (  Unsigned_32 (Data (45)) * 2**24
                  +  Unsigned_32 (Data (46)) * 2**16
                  +  Unsigned_32 (Data (47)) * 2**8
                  +  Unsigned_32 (Data (48))
                  );
      return (  Era
             +  Duration (Seconds)
             +  Duration (Long_Float (Fraction) / 2.0**32)
             -  Offset
             );
   end Get_Time;

end GNAT.Sockets.NTP;
