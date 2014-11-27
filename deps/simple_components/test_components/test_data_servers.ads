--                                                                    --
--  package Test_Data_Servers       Copyright (c)  Dmitry A. Kazakov  --
--  Test server                                    Luebeck            --
--  Interface                                      Winter, 2012       --
--                                                                    --
--                                Last revision :  13:09 10 Mar 2013  --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Streams;               use Ada.Streams;
with GNAT.Sockets;              use GNAT.Sockets;
with GNAT.Sockets.Server;       use GNAT.Sockets.Server;

with GNAT.Sockets.Connection_State_Machine.Big_Endian.IEEE_754_Floats;
with GNAT.Sockets.Connection_State_Machine.Big_Endian.
                                           IEEE_754_Long_Floats;
with GNAT.Sockets.Connection_State_Machine.Big_Endian.Integers;
with GNAT.Sockets.Connection_State_Machine.Big_Endian.Unsigneds;
with GNAT.Sockets.Connection_State_Machine.Chain_Code.Naturals;
with GNAT.Sockets.Connection_State_Machine.Little_Endian.
                                           IEEE_754_Floats;
with GNAT.Sockets.Connection_State_Machine.Little_Endian.
                                           IEEE_754_Long_Floats;
with GNAT.Sockets.Connection_State_Machine.Chain_Code.Integers;
with GNAT.Sockets.Connection_State_Machine.Little_Endian.Integers;
with GNAT.Sockets.Connection_State_Machine.Little_Endian.Unsigneds;
with GNAT.Sockets.Connection_State_Machine.Terminated_Strings;
with GNAT.Sockets.Connection_State_Machine.Variable_Length_Strings;

package Test_Data_Servers is
   use GNAT.Sockets.Connection_State_Machine;

   type Data_Connection;
   type String_Length_Setter
        (  Parent : access Data_Connection
        )  is new Data_Item with null record;
   procedure Feed
             (  Item    : in out String_Length_Setter;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );

   type Nested is new Data_Block with record
      N1 : Little_Endian.Unsigneds.Unsigned_8_Data_Item;
      N2 : Little_Endian.Unsigneds.Unsigned_16_Data_Item;
      N3 : Little_Endian.Unsigneds.Unsigned_32_Data_Item;
      N4 : Little_Endian.Unsigneds.Unsigned_64_Data_Item;
   end record;

   type Alternative_1 is new Data_Block with record
      N : Little_Endian.Integers.Integer_16_Data_Item;
   end record;

   type Alternative_2 is new Data_Block with record
      L : Chain_Code.Naturals.Unsigned_Data_Item;
      M : Chain_Code.Naturals.Unsigned_Data_Item;
   end record;

   type Variant is new Data_Selector with record
      A1   : Alternative_1;
      A2   : Alternative_2;
      S    : Terminated_Strings.String_Data_Item (9, Character'Val (0));
      None : Data_Null;
   end record;

   type Data_Connection is new State_Machine with record
      From : Sock_Addr_Type;
      Blk  : Nested;
      N5   : Big_Endian.Unsigneds.Unsigned_8_Data_Item;
      N6   : Big_Endian.Unsigneds.Unsigned_16_Data_Item;
      N7   : Big_Endian.Unsigneds.Unsigned_32_Data_Item;
      N8   : Big_Endian.Unsigneds.Unsigned_64_Data_Item;
      F1   : Big_Endian.IEEE_754_Floats.IEEE_754_Data_Item;
      F2   : Big_Endian.IEEE_754_Long_Floats.IEEE_754_Data_Item;
      F3   : Little_Endian.IEEE_754_Floats.IEEE_754_Data_Item;
      F4   : Little_Endian.IEEE_754_Long_Floats.IEEE_754_Data_Item;
      S1   : Terminated_Strings.String_Data_Item (9, Character'Val (0));
      Len  : Big_Endian.Integers.Integer_16_Data_Item;
      Fix  : String_Length_Setter (Data_Connection'Unchecked_Access);
      S2   : Variable_Length_Strings.String_Data_Item (80);
      N9   : Chain_Code.Naturals.Unsigned_Data_Item;
      N10  : Chain_Code.Integers.Integer_Data_Item;
      V    : Variant;
   end record;
   procedure Finalize (Client : in out Data_Connection);
   procedure Process_Packet (Client : in out Data_Connection);

   type Data_Factory is new Connections_Factory with null record;
   function Create
            (  Factory  : access Data_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;
   procedure Trace
             (  Factory    : in out Data_Factory;
                Context    : String;
                Occurrence : Exception_Occurrence
             );
end Test_Data_Servers;
