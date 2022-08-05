--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Nulls                                  Spring, 2019       --
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

with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;

package body GNAT.Sockets.Connection_State_Machine.ASN1.Nulls is

   procedure Encode
             (  Item    : Implicit_Null_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      null;
   end Encode;

   procedure Encode
             (  Item    : Null_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      elsif Data'Last - Pointer < 1 then
         Raise_Exception (End_Error'Identity, No_Room);
      end if;
      Data (Pointer    ) := Stream_Element (Null_Tag);
      Data (Pointer + 1) := 0;
      Pointer := Pointer + 2;
   end Encode;

   procedure Feed
             (  Item    : in out Null_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Check (Data (Pointer), (1 => Null_Tag), True);
         Pointer := Pointer + 1;
         State   := 1;
         if Pointer > Data'Last then
            return;
         end if;
      end if;
      if Data (Pointer) /= 0 then
         Raise_Exception (Data_Error'Identity, Invalid_Length);
      end if;
      Pointer := Pointer + 1;
      State   := 0;
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_Null_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      State := 0;
   end Feed;

   function Get_ASN1_Type
            (  Item : Implicit_Null_Data_Item
            )  return ASN1_Type is
   begin
       return Null_Tag;
   end Get_ASN1_Type;

   function Is_Implicit (Item : Implicit_Null_Data_Item)
      return Boolean is
   begin
      return True;
   end Is_Implicit;

   function Is_Implicit (Item : Null_Data_Item) return Boolean is
   begin
      return False;
   end Is_Implicit;

   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Null_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             )  is
   begin
      null;
   end Set_Implicit_Tag;

   procedure Set_Untagged (Item : in out Implicit_Null_Data_Item) is
   begin
      null;
   end Set_Untagged;

end GNAT.Sockets.Connection_State_Machine.ASN1.Nulls;
