--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     Chain_Code.Generic_Integer                  Winter, 2012       --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body GNAT.Sockets.Connection_State_Machine.Chain_Code.
             Generic_Integer is
   procedure Feed
             (  Item    : in out Integer_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      This : Stream_Element;
   begin
      if State = 0 then
         Item.Value := 0;
         State      := 1; -- Only 6 bits available in the first octet
      end if;
      if State = 1 then -- The first octet
         if Pointer > Data'Last then
            return;
         end if;
         This    := Data (Pointer);
         Pointer := Pointer + 1;
         begin
            Item.Value := Number ((This and 2#0111_1110#) / 2);
            if 0 = (This and 16#80#) then
               if 0 /= (This and 1) then
                  Item.Value := -Item.Value - 1;
               end if;
               State := 0;
               return;
            end if;
         exception
            when Constraint_Error =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Number is too large"
               );
         end;
         if 0 = (This and 1) then
            State := 6;
         else
            State := -6;
         end if;
      end if;
      if State > 0 then
         while Pointer <= Data'Last loop
            This    := Data (Pointer);
            Pointer := Pointer + 1;
            begin
               Item.Value :=
                  (  Item.Value
                  +  2 ** Natural (State) * Number (This and 16#7F#)
                  );
            exception
               when Constraint_Error =>
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Number is too large"
                  );
            end;
            if 0 = (This and 16#80#) then
               State := 0;
               return;
            end if;
            State := State + 7;
         end loop;
      else
         while Pointer <= Data'Last loop
            This    := Data (Pointer);
            Pointer := Pointer + 1;
            begin
               Item.Value :=
                  (  Item.Value
                  +  2 ** Natural (-State) * Number (This and 16#7F#)
                  );
            exception
               when Constraint_Error =>
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Number is too large"
                  );
            end;
            if 0 = (This and 16#80#) then
               begin
                  Item.Value := -Item.Value - 1;
               exception
                  when Constraint_Error =>
                     Raise_Exception
                     (  Data_Error'Identity,
                        "Number is too large"
                     );
               end;
               State := 0;
               return;
            end if;
            State := State - 7;
         end loop;
      end if;
   end Feed;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Number
             )  is
      Result   : Number;
      Power    : Natural := 6;
      Negative : Boolean;
      This     : Stream_Element;
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
      if Pointer <= Data'Last then
         This    := Data (Pointer);
         Pointer := Pointer + 1;
         begin
            Result := Number ((This and 2#0111_1110#) / 2);
            Negative := 0 /= (This and 1);
            if 0 = (This and 16#80#) then
               if Negative then
                  Value := -Result - 1;
               else
                  Value := Result;
               end if;
               return;
            end if;
         exception
            when Constraint_Error =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Number is too large"
               );
         end;
         while Pointer <= Data'Last loop
            This    := Data (Pointer);
            Pointer := Pointer + 1;
            begin
               Result := Result + 2**Power * Number (This and 16#7F#);
               if 0 = (This and 16#80#) then
                  if Negative then
                     Value := -Result - 1;
                  else
                     Value := Result;
                  end if;
                  return;
               end if;
            exception
               when Constraint_Error =>
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Number is too large"
                  );
            end;
            Power := Power + 7;
         end loop;
      end if;
      Raise_Exception
      (  End_Error'Identity,
         "Non-terminated chain sequence"
      );
   end Get;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Number
             )  is
      Item : Number := Value;
   begin
      if Pointer not in Data'Range then
         Raise_Exception
         (  Layout_Error'Identity,
            "Pointer is out of bounds"
         );
      end if;
      if Item >= 0 then
         Item := Value;
         Data (Pointer) := Stream_Element (Item mod (16#40#)) * 2;
      else
         Item := -(Value + 1);
         Data (Pointer) := Stream_Element (Item mod (16#40#)) * 2 + 1;
      end if;
      Item := Item / 16#40#;
      if Item = 0 then
         Pointer := Pointer + 1;
         return;
      end if;
      Data (Pointer) := Data (Pointer) or 16#80#;
      Pointer := Pointer + 1;
      while Pointer <= Data'Last loop
         Data (Pointer) := Stream_Element (Item mod 16#80#) or 16#80#;
         Item := Item / 16#80#;
         if Item = 0 then
            Data (Pointer) := Data (Pointer) and 16#7F#;
            Pointer := Pointer + 1;
            return;
         end if;
         Pointer := Pointer + 1;
      end loop;
      Raise_Exception (Layout_Error'Identity, "No room for output");
   end Put;

end GNAT.Sockets.Connection_State_Machine.Chain_Code.Generic_Integer;
