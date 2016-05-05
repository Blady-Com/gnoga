--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     Little_Endian.Integers                      Winter, 2012       --
--  Implementation                                                    --
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

package body GNAT.Sockets.Connection_State_Machine.
             Little_Endian.Integers is

   procedure Feed
             (  Item    : in out Integer_8_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed (Unsigned_8_Data_Item (Item), Data, Pointer, Client, State);
   end Feed;

   procedure Feed
             (  Item    : in out Integer_16_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed (Unsigned_16_Data_Item (Item), Data, Pointer, Client, State);
   end Feed;

   procedure Feed
             (  Item    : in out Integer_32_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed (Unsigned_32_Data_Item (Item), Data, Pointer, Client, State);
   end Feed;

   procedure Feed
             (  Item    : in out Integer_64_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed (Unsigned_64_Data_Item (Item), Data, Pointer, Client, State);
   end Feed;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Integer_8
             )  is
      Result : Unsigned_8;
   begin
      Get (Data, Pointer, Result);
      Value := To_Integer (Result);
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Integer_16
             )  is
      Result : Unsigned_16;
   begin
      Get (Data, Pointer, Result);
      Value := To_Integer (Result);
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Integer_32
             )  is
      Result : Unsigned_32;
   begin
      Get (Data, Pointer, Result);
      Value := To_Integer (Result);
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Integer_64
             )  is
      Result : Unsigned_64;
   begin
      Get (Data, Pointer, Result);
      Value := To_Integer (Result);
   end Get;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Integer_8
             )  is
   begin
      Put (Data, Pointer, To_Unsigned (Value));
   end Put;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Integer_16
             )  is
   begin
      Put (Data, Pointer, To_Unsigned (Value));
   end Put;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Integer_32
             )  is
   begin
      Put (Data, Pointer, To_Unsigned (Value));
   end Put;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Integer_64
             )  is
   begin
      Put (Data, Pointer, To_Unsigned (Value));
   end Put;

   function Value (Item : Integer_8_Data_Item) return Integer_8 is
   begin
      return To_Integer (Item.Value);
   end Value;

   function Value (Item : Integer_16_Data_Item) return Integer_16 is
   begin
      return To_Integer (Item.Value);
   end Value;

   function Value (Item : Integer_32_Data_Item) return Integer_32 is
   begin
      return To_Integer (Item.Value);
   end Value;

   function Value (Item : Integer_64_Data_Item) return Integer_64 is
   begin
      return To_Integer (Item.Value);
   end Value;

end GNAT.Sockets.Connection_State_Machine.Little_Endian.Integers;
