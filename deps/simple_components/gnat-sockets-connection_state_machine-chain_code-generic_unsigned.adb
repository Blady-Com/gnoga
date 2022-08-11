--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     Chain_Code.Generic_Unsigned                 Winter, 2012       --
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

package body GNAT.Sockets.Connection_State_Machine.Chain_Code.
             Generic_Unsigned is
   procedure Feed
             (  Item    : in out Unsigned_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         Item.Value := 0;
         State      := 1;
      end if;
      while Pointer <= Data'Last loop
         begin
            Item.Value :=
               (  Item.Value
               +  (  2 ** Natural (State - 1)
                  *  Number (Data (Pointer) and 16#7F#)
               )  );
         exception
            when Constraint_Error =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Number is too large"
               );
         end;
         if 0 = (Data (Pointer) and 16#80#) then
            State   := 0;
            Pointer := Pointer + 1;
            return;
         end if;
         Pointer := Pointer + 1;
         State   := State + 7;
      end loop;
   end Feed;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Number
             )  is
      Result : Number  := 0;
      Power  : Natural := 0;
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )
      then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      end if;
      while Pointer <= Data'Last loop
         begin
            Result :=
               Result + 2**Power * Number (Data (Pointer) and 16#7F#);
         exception
            when Constraint_Error =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Number is too large"
               );
         end;
         if 0 = (Data (Pointer) and 16#80#) then
            Pointer := Pointer + 1;
            Value   := Result;
            return;
         end if;
         Power   := Power + 7;
         Pointer := Pointer + 1;
      end loop;
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
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      elsif Item < 0 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Negative value"
         );
      end if;
      while Pointer <= Data'Last loop
         Data (Pointer) := Stream_Element (Item mod 16#80#) or 16#80#;
         Pointer := Pointer + 1;
         Item := Item / 16#80#;
         if Item = 0 then
            Data (Pointer - 1) := Data (Pointer - 1) and 16#7F#;
            return;
         end if;
      end loop;
      Raise_Exception (Layout_Error'Identity, "No room for output");
   end Put;

end GNAT.Sockets.Connection_State_Machine.Chain_Code.Generic_Unsigned;
