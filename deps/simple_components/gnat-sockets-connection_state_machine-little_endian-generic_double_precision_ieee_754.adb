--                                                                    --
--  package GNAT.Sockets.           Copyright (c)  Dmitry A. Kazakov  --
--     Connection_State_Machine.Little_Endian.     Luebeck            --
--     Generic_Double_Precision_IEEE_754           Winter, 2012       --
--  Implementation                                                    --
--                                Last revision :  10:13 29 Nov 2020  --
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

package body GNAT.Sockets.Connection_State_Machine.Little_Endian.
             Generic_Double_Precision_IEEE_754 is
   use IEEE_754;

   procedure Feed
             (  Item    : in out IEEE_754_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State > 8 then
         Raise_Exception
         (  Data_Error'Identity,
            "Protocol error reading 64-bit little endian IEEE 754"
         );
      elsif State = 0 then
         State := 8;
      end if;
      while Pointer <= Data'Last and then State > 0 loop
         Item.Value (Integer (State)) := Byte (Data (Pointer));
         State   := State - 1;
         Pointer := Pointer + 1;
      end loop;
   end Feed;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Float_64
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )
      then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      elsif Pointer + 7 > Data'Last then
         Raise_Exception (End_Error'Identity, "End of data");
      end if;
      Value :=
         (  Byte (Data (Pointer + 7)),
            Byte (Data (Pointer + 6)),
            Byte (Data (Pointer + 5)),
            Byte (Data (Pointer + 4)),
            Byte (Data (Pointer + 3)),
            Byte (Data (Pointer + 2)),
            Byte (Data (Pointer + 1)),
            Byte (Data (Pointer))
         );
      Pointer := Pointer + 8;
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Number
             )  is
      IEEE : Float_64;
   begin
      Get (Data, Pointer, IEEE);
      Value := From_IEEE (IEEE);
   end Get;

   function Get_Value (Item : IEEE_754_Data_Item) return Number is
   begin
      return From_IEEE (Item.Value);
   end Get_Value;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Float_64
             )  is
   begin
      if Pointer < Data'First or else Data'Last - Pointer < 7 then
         if Pointer >= Data'First and then Pointer - 1 <= Data'Last then
            Raise_Exception (End_Error'Identity, No_Room);
         else
            Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
         end if;
      end if;
      Data (Pointer..Pointer + 7) :=
         (  Stream_Element (Value (8)),
            Stream_Element (Value (7)),
            Stream_Element (Value (6)),
            Stream_Element (Value (5)),
            Stream_Element (Value (4)),
            Stream_Element (Value (3)),
            Stream_Element (Value (2)),
            Stream_Element (Value (1))
         );
      Pointer := Pointer + 8;
   end Put;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Number
             )  is
   begin
      Put (Data, Pointer, To_IEEE (Value));
   end Put;

end GNAT.Sockets.Connection_State_Machine.Little_Endian.
    Generic_Double_Precision_IEEE_754;
