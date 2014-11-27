--                                                                    --
--  package GNAT.Sockets.           Copyright (c)  Dmitry A. Kazakov  --
--     Connection_State_Machine.                   Luebeck            --
--     Generic_Key                                 Winter, 2013       --
--  Implementation                                                    --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body GNAT.Sockets.Connection_State_Machine.Generic_Key is

   procedure Feed
             (  Item    : in out Key_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      procedure Raise_Not_Found is
      begin
         Item.Offset := 0;
         Raise_Exception (Data_Error'Identity, "Key not matched");
      end Raise_Not_Found;

      Dictionary : Table'Class renames Item.Dictionary.all;
      This       : Character;

      function Get_Name return String is
      begin
         if State = 0 then
            return (1 => This);
         else
            declare
               Result : String := GetName (Dictionary, Item.Offset);
            begin
               return
               (  Result
                  (  Result'First
                  .. Result'First + Natural (State) - 1
                  )
               &  This
               );
            end;
         end if;
      end Get_Name;
   begin
      if State = 0 then
         Item.Offset := 1;
      end if;
      while Pointer <= Data'Last loop
         This := Character'Val (Data (Pointer));
         if This = Item.Terminator then
            declare
               Name : String := GetName (Dictionary, Item.Offset);
            begin
               if Name'Length /= Natural (State) then
                  Raise_Not_Found;
               end if;
            end;
            Pointer := Pointer + 1;
            State   := 0;
            return;
         end if;
         declare
            Name : String := Get_Name;
         begin
  Advance : loop
               if Item.Offset > GetSize (Dictionary) then
                  Raise_Not_Found;
               end if;
               declare
                  Token : String  := GetName (Dictionary, Item.Offset);
                  Index : Integer := Name'First;
               begin
                  for Pointer in Token'Range loop
                     exit Advance when Index > Name'Last;
                     if Token (Pointer) /= Name (Index) then
                        exit when Token (Pointer) < Name (Index);
                        Raise_Not_Found;
                     end if;
                     Index := Index + 1;
                  end loop;
                  exit when Index > Name'Last;
               end;
               Item.Offset := Item.Offset;
            end loop Advance;
            State   := State   + 1;
            Pointer := Pointer + 1;
         end;
      end loop;
   end Feed;

   procedure Get
             (  Data       : Stream_Element_Array;
                Pointer    : access Stream_Element_Offset;
                Dictionary : Table'Class;
                Terminator : Character;
                Key        : out Tag
             )  is
      Next : Stream_Element_Offset := Pointer.all;
   begin
      if (  Next < Data'First
         or else
            (  Next > Data'Last
            and then
               Next > Data'Last + 1
         )  )
      then
         Raise_Exception
         (  Layout_Error'Identity,
            "Pointer is out of bounds"
         );
      end if;
      while Next <= Data'Last loop
         if Character'Val (Data (Next)) = Terminator then
            declare
               Name : String (1..Natural (Next - Pointer.all));
            begin
               Next := Pointer.all;
               for Index in Name'Range loop
                  Name (Index) := Character'Val (Data (Next));
                  Next := Next + 1;
               end loop;
               Key := Find (Dictionary, Name);
               Pointer.all := Next + 1;
               return;
            exception
               when End_Error =>
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Key not matched"
                  );
            end;
         else
            Next := Next + 1;
         end if;
      end loop;
      Raise_Exception
      (  End_Error'Identity,
         "Missing string terminator"
      );
   end Get;

   function Get_Key (Item : Key_Item) return Tag is
   begin
      return GetTag (Item.Dictionary.all, Item.Offset);
   end Get_Key;

   procedure Put
             (  Data       : in out Stream_Element_Array;
                Pointer    : in out Stream_Element_Offset;
                Dictionary : Table'Class;
                Terminator : Character;
                Key        : Tag
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer > Data'Last + 1
         )  )
      then
         Raise_Exception
         (  Layout_Error'Identity,
            "Pointer is out of bounds"
         );
      end if;
      for Index in 1..GetSize (Dictionary) loop
         if GetTag (Dictionary, Index) = Key then
            declare
               Name : String := GetName (Dictionary, Index);
            begin
               if Data'Last - Pointer < Name'Length then
                  Raise_Exception
                  (  End_Error'Identity,
                     "No room for output"
                  );
               end if;
               for Index in Name'Range loop
                  if Name (Index) = Terminator then
                     Raise_Exception
                     (  Data_Error'Identity,
                        "The string body contains the terminator"
                     );
                  end if;
               end loop;
               for Index in Name'Range loop
                  Data (Pointer) := Character'Pos (Name (Index));
                  Pointer := Pointer + 1;
               end loop;
               Data (Pointer) := Character'Pos (Terminator);
               Pointer := Pointer + 1;
               return;
            end;
         end if;
      end loop;
      Raise_Exception
      (  Constraint_Error'Identity,
         "Key is not in the dictionary"
      );
   end Put;

end GNAT.Sockets.Connection_State_Machine.Generic_Key;
