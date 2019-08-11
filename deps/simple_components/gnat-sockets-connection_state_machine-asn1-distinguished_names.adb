--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Distinguished_Names                    Summer, 2019       --
--  Implementation                                                    --
--                                Last revision :  13:37 03 Aug 2019  --
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

package body GNAT.Sockets.Connection_State_Machine.ASN1.
             Distinguished_Names is
   use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;

   No_Name : constant String := "ASN.1 DN is empty";

   procedure Feed
             (  Item    : in out External_DN_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  External_String_Data_Item (Item),
         Data,
         Pointer,
         Client,
         State
      );
      if State = 0 then
         begin
            declare
               Name : constant Distinguished_Name := Get_Name (Item);
            begin
               null; -- Checked name OK
            end;
         exception
            when End_Error =>
               Raise_Exception (Data_Error'Identity, No_Name);
         end;
      end if;
   end Feed;

   procedure Feed
             (  Item    : in out DN_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  String_Data_Item (Item),
         Data,
         Pointer,
         Client,
         State
      );
      if State = 0 then
         begin
            declare
               Name : constant Distinguished_Name := Get_Name (Item);
            begin
               null; -- Checked name OK
            end;
         exception
            when End_Error =>
               Raise_Exception (Data_Error'Identity, No_Name);
         end;
      end if;
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_DN_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Implicit_String_Data_Item (Item),
         Data,
         Pointer,
         Client,
         State
      );
      if State = 0 then
         begin
            declare
               Name : constant Distinguished_Name := Get_Name (Item);
            begin
               null; -- Checked name OK
            end;
         exception
            when End_Error =>
               Raise_Exception (Data_Error'Identity, No_Name);
         end;
      end if;
   end Feed;

   procedure Feed
             (  Item    : in out Implicit_External_DN_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      Feed
      (  Implicit_External_String_Data_Item (Item),
         Data,
         Pointer,
         Client,
         State
      );
      if State = 0 then
         begin
            declare
               Name : constant Distinguished_Name := Get_Name (Item);
            begin
               null; -- Checked name OK
            end;
         exception
            when End_Error =>
               Raise_Exception (Data_Error'Identity, No_Name);
         end;
      end if;
   end Feed;

   function Get
            (  Data    : Stream_Element_Array;
               Pointer : access Stream_Element_Offset;
               Length  : Stream_Element_Offset
            )  return Distinguished_Name is
      Index : Stream_Element_Offset := Pointer.all;
      Text  : String (1..Integer (Length));
   begin
      Get (Data, Index, Text);
      declare
         Result : constant Distinguished_Name := Value (Text);
      begin
         Pointer.all := Index;
         return Result;
      end;
   exception
      when Error : End_Error =>
         Raise_Exception (Data_Error'Identity, No_Name);
   end Get;

   function Get_Name
            (  Item : External_DN_Data_Item
            )  return Distinguished_Name is
   begin
      return Value (Get_Value (Item));
   end Get_Name;

   function Get_Name
            (  Item : DN_Data_Item
            )  return Distinguished_Name is
   begin
      return Value (Get_Value (Item));
   end Get_Name;

   function Get_Name
            (  Item : Implicit_DN_Data_Item
            )  return Distinguished_Name is
   begin
      return Value (Get_Value (Item));
   end Get_Name;

   function Get_Name
            (  Item : Implicit_External_DN_Data_Item
            )  return Distinguished_Name is
   begin
      return Value (Get_Value (Item));
   end Get_Name;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Name    : Distinguished_Name
             )  is
   begin
      Put (Data, Pointer, Image (Name));
   end Put;

   procedure Set_Name
             (  Item : in out DN_Data_Item;
                Name : Distinguished_Name
             )  is
   begin
      Set_Value (Item, Image (Name));
   end Set_Name;

   procedure Set_Name
             (  Item : in out Implicit_DN_Data_Item;
                Name : Distinguished_Name
             )  is
   begin
      Set_Value (Item, Image (Name));
   end Set_Name;

   procedure Set_Name
             (  Item : in out External_DN_Data_Item;
                Name : Distinguished_Name
             )  is
   begin
      Set_Value (Item, Image (Name));
   end Set_Name;

   procedure Set_Name
             (  Item : in out Implicit_External_DN_Data_Item;
                Name : Distinguished_Name
             )  is
   begin
      Set_Value (Item, Image (Name));
   end Set_Name;

end GNAT.Sockets.Connection_State_Machine.ASN1.Distinguished_Names;
