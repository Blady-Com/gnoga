--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Object_Identifiers             Luebeck            --
--                                                 Spring, 2019       --
--  Implementation                                                    --
--                                Last revision :  18:40 01 Aug 2019  --
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

with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Fields;    use Strings_Edit.Fields;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body Strings_Edit.Object_Identifiers is

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : in out Object_Identifier;
                Last    : out Integer
             )  is
      Index  : Integer := Pointer;
      Item   : Integer := 0;
      Length : Integer := 0;
   begin
      while Index <= Source'Last loop
         begin
            Get
            (  Source  => Source,
               Pointer => Index,
               Value   => Item,
               First   => 0
            );
         exception
            when End_Error =>
               if Length = 0 then
                  raise;
               else
                  raise Data_Error;
               end if;
            when others =>
               raise Data_Error;
         end;
         if Length >= Value'Length then
            raise Constraint_Error;
         end if;
         Value (Value'First + Length) := Subindentifier_Type (Item);
         Length := Length + 1;
         exit when Index > Source'Last or else Source (Index) /= '.';
         Index := Index + 1;
      end loop;
      Pointer := Index;
      Last    := Value'First + Length - 1;
   end Get;

   function Get
            (  Source  : String;
               Pointer : access Integer
            )  return Object_Identifier is
      Size : Natural := 127;
   begin
      loop
         declare
            Last   : Integer;
            Index  : Integer := Pointer.all;
            Result : Object_Identifier (1..Size);
         begin
            Get (Source, Index, Result, Last);
            Pointer.all := Index;
            return Result (1..Last);
         exception
            when Constraint_Error =>
               Size := (Size * 3) / 2;
         end;
      end loop;
   end Get;

   function Image (Value : Object_Identifier) return String is
      Size : Integer := 512;
   begin
      loop
         declare
            Result  : String (1..Size);
            Pointer : Integer := 1;
         begin
            Put (Result, Pointer, Value);
            return Result (1..Pointer - 1);
         exception
            when Layout_Error =>
               Size := (Size * 3) / 2;
         end;
      end loop;
   end Image;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Object_Identifier;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   Out_Field : constant Natural :=
      Get_Output_Field (Destination, Pointer, Field);
   subtype Output is String (Pointer..Pointer + Out_Field - 1);
   Index : Integer := Pointer;
   Text  : Output renames
                  Destination (Pointer..Pointer + Out_Field - 1);
   begin
      for Node in Value'Range loop
         if Node > Value'First then
            Put (Text, Index, ".");
         end if;
         Put (Text, Index, Integer (Value (Node)));
      end loop;
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   end Put;

   function Value (Source : String) return Object_Identifier is
      Size : Integer := 512;
   begin
      loop
         declare
            Value   : Object_Identifier (1..Size);
            Pointer : Integer := Source'First;
            Last    : Integer;
         begin
            Get (Source, Pointer, SpaceAndTab);
            Get (Source, Pointer, Value, Last);
            Get (Source, Pointer, SpaceAndTab);
            if Pointer /= Source'Last  + 1 then
               raise Data_Error;
            end if;
            return Value (1..Last);
         exception
            when Constraint_Error =>
               Size := (Size * 3) / 2;
         end;
      end loop;
   end Value;

   function Compare
            (  Left, Right : Object_Identifier
            )  return Precedence is
      This : Integer := Right'First;
   begin
      if Right'Length = 0 then
         if Left'Length = 0 then
            return Equal;
         else
            return Greater;
         end if;
      end if;
      for That in Left'Range loop
         if This > Right'Last then
            return Greater;
         elsif Left (That) /= Right (This) then
            if Left (That) < Right (This) then
               return Less;
            else
               return Greater;
            end if;
         end if;
         This := This + 1;
      end loop;
      if This > Right'Last then
         return Equal;
      else
         return Less;
      end if;
   end Compare;

   function "<" (Left, Right : Object_Identifier) return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<=" (Left, Right : Object_Identifier) return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function ">" (Left, Right : Object_Identifier) return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">=" (Left, Right : Object_Identifier) return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

end Strings_Edit.Object_Identifiers;
