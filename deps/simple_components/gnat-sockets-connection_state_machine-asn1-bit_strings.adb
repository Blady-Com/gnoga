--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Bit_Strings                            Spring, 2019       --
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

package body GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings is

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Boolean_Array
             )  is
      Length : constant Stream_Element_Offset := (Value'Length + 7) / 8;
      Mask   : Stream_Element;
      Octet  : Stream_Element;
   begin
      if Data'Last - Pointer < Length then
         Raise_Exception (End_Error'Identity, Non_Terminated);
      end if;
      if Data (Pointer) /= Stream_Element ((-Value'Length) mod 8) then
         Raise_Exception (Data_Error'Identity, Wrong_Unused_Count);
      end if;
      Pointer := Pointer + 1;
      Mask  := 0;
      for Bit in Value'Range loop
         if Mask = 0 then
            Mask    := 2**7;
            Octet   := Data (Pointer);
            Pointer := Pointer + 1;
         end if;
         Value (Bit) := 0 /= (Octet and Mask);
         Mask := Mask / 2;
      end loop;
   end Get;

   function Get_ASN1_Type
            (  Item : Public_Bit_String_Data_Item
            )  return ASN1_Type is
   begin
      return Bit_String_Tag;
   end Get_ASN1_Type;

   function Get_Length
            (  Item : Public_Bit_String_Data_Item
            )  return Natural is
   begin
      return Item.Length;
   end Get_Length;

   function Get_Value
            (  Item : Public_Bit_String_Data_Item
            )  return Boolean_Array is
   begin
      return Item.Value (1..Item.Length);
   end Get_Value;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Boolean_Array
             )  is
      Length : constant Stream_Element_Offset := (Value'Length + 7) / 8;
      Mask   : Stream_Element;
      Octet  : Stream_Element;
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      end if;
      if Data'Last - Pointer < Length then
         Raise_Exception (End_Error'Identity, No_Room);
      end if;
      Data (Pointer) := Stream_Element ((-Value'Length) mod 8);
      Pointer := Pointer + 1;
      Mask  := 2**7;
      Octet := 0;
      for Index in Value'Range loop
         if Value (Index) then
            Octet := Octet or Mask;
         end if;
         Mask := Mask / 2;
         if Mask = 0 then
            Data (Pointer) := Octet;
            Pointer := Pointer + 1;
            Mask  := 2**7;
            Octet := 0;
         end if;
      end loop;
      if Mask /= 2**7 then
         Data (Pointer) := Octet;
         Pointer := Pointer + 1;
      end if;
   end Put;

   procedure Set_Value
             (  Item  : in out Public_Bit_String_Data_Item;
                Value : Boolean_Array
             )  is
   begin
      if Item.Size < Value'Length then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      else
         Item.Length := Value'Length;
         Item.Value (1..Value'Length) := Value;
      end if;
   end Set_Value;

end GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings;
