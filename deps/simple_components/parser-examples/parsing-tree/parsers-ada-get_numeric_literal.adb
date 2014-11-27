--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Ada.Get_Numeric_Literal             Luebeck            --
--  Separate body implementation                   Winter, 2004       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2010  --
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

separate (Parsers.Ada) 
   procedure Get_Numeric_Literal
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is
   function To_Digit (Symbol : Character) return Integer;
   pragma Inline (To_Digit);

   function To_Digit (Symbol : Character) return Integer is
   begin
      case Symbol is
         when '0'     => return 0;
         when '1'     => return 1;
         when '2'     => return 2;
         when '3'     => return 3;
         when '4'     => return 4;
         when '5'     => return 5;
         when '6'     => return 6;
         when '7'     => return 7;
         when '8'     => return 8;
         when '9'     => return 9;
         when 'a'|'A' => return 10;
         when 'b'|'B' => return 11;
         when 'c'|'C' => return 12;
         when 'd'|'D' => return 13;
         when 'e'|'E' => return 14;
         when 'f'|'F' => return 15;
         when others  => return 16;
      end case;
   end To_Digit;

   procedure Get_Numeral
             (  Line      : String;
                Pointer   : in out Integer;
                Base      : Integer;
                Length    : out Natural;
                Malformed : in out Boolean
             )  is
      Underline : Boolean := False;
      Symbol    : Character;
   begin
      Length := 0;
      while Pointer <= Line'Last loop
         Symbol := Line (Pointer);
         if To_Digit (Symbol) < Base then
            Length    := Length + 1;
            Underline := False;
         elsif '_' = Symbol then
            Malformed := Malformed or Underline;
            Underline := True;
         else
            exit;
         end if;
         Pointer := Pointer + 1;
      end loop;
      Malformed := Malformed or Underline;
   end Get_Numeral;

   function To_Unsigned (Line : String; Upper : Integer)
      return Integer is
      Result : Integer := 0;
      Symbol : Character;
   begin
      for Index in Line'Range loop
         Symbol := Line (Index);
         if '_' /= Symbol then
            Result := Result * 10 + To_Digit (Symbol);
            exit when Result > Upper;
         end if;
      end loop;
      return Result;
   end To_Unsigned;

   Max_Exponent : constant := Integer'Last / 10 - 10;
   Base      : Integer := 10;
   Exponent  : Integer := 0;
   Mantissa  : Integer := Pointer;
   Fore      : Natural;
   Aft       : Natural := 0;
   Malformed : Boolean := False;
   Real      : Boolean := False;
   Based     : Boolean := False;
   Index     : Integer := Pointer;
begin
   Get_Numeral (Line, Index, 10, Fore, Malformed);
   if Index <= Line'Last and then '#' = Line (Index) then
      Base := To_Unsigned (Line (Pointer..Index - 1), 16);
      if Base < 2 or else Base > 16 then
         Set_Pointer (Code, Index);
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            (  "The base at "
            &  Image (Link (Code))
            &  " of a numeric literal shall be in 2..16"
         )  );
      end if;
      Based    := True;
      Index    := Index + 1;
      Mantissa := Index;
      Get_Numeral (Line, Index, Base, Fore, Malformed);
   end if;
   if (  Index <= Line'Last
      and then
         '.' = Line (Index)
      and then
         (Index >= Line'Last or else '.' /= Line (Index + 1))
      )
   then
      Real  := True;
      Index := Index + 1;
      Get_Numeral (Line, Index, Base, Aft, Malformed);
      Malformed := Malformed or Aft = 0;
   end if;
   if Based then
      if Index > Line'Last or else '#' /= Line (Index) then
         Set_Pointer (Code, Index);
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            (  "Missing # in the numeric literal at "
            &  Image (Link (Code))
         )  );
      end if;
      Index := Index + 1;
   end if;
   if (  Index <= Line'Last
      and then
         (  'e' = Line (Index)
         or else
            'E' = Line (Index)
      )  )
   then
      Index := Index + 1;
      declare
         Pointer  : Integer := Index;
         Length   : Natural := 0;
         Negative : Boolean := False;
      begin
         if Index <= Line'Last then
            if '-' = Line (Index) then
               Pointer  := Pointer + 1;
               Index    := Index   + 1;
               Negative := True;
            elsif '+' = Line (Index) then
               Pointer  := Pointer + 1;
               Index    := Index   + 1;
            end if;
            Get_Numeral (Line, Index, 10, Length, Malformed);
            Exponent :=
               To_Unsigned
               (  Line (Pointer..Index - 1),
                  Max_Exponent
               );
            if Exponent > Max_Exponent then
               if Negative then
                  Exponent := Integer'First;
               else
                  Exponent := Integer'Last;
               end if;
            else
               if Negative then
                  Exponent := - Exponent - Aft;
               else
                  Exponent := Exponent - Aft;
               end if;
            end if;
         end if;
         if Length = 0 then
            Set_Pointer (Code, Index);
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "Missing exponent part in the numeric literal at "
               &  Image (Link (Code))
            )  );
         end if;
      end;
   else
      Exponent := -Aft;
   end if;
   Malformed :=
      (  Malformed
      or else
         (  Index <= Line'Last
         and then
            Is_Letter (Line (Index))
      )  );
   if Real then
      Argument.Value := new Real_Literal (Fore + Aft);
   else
      Malformed := Malformed or Exponent < 0;
      Argument.Value := new Integer_Literal (Fore + Aft);
   end if;
   declare
      This   : Numeric_Literal renames
                  Numeric_Literal (Argument.Value.all);
      Symbol : Character;
   begin
      This.Malformed := Malformed;
      This.Base      := Base;
      This.Exponent  := Exponent;
      for Index in This.Value'Range loop
         Symbol := Line (Mantissa);
         while '.' = Symbol or else '_' = Symbol loop
            Mantissa := Mantissa + 1;
            Symbol   := Line (Mantissa);
         end loop;
         This.Value (Index) := Symbol;    
         Mantissa := Mantissa + 1;
      end loop;  
   end;
   Set_Pointer (Code, Index);
   Argument.Location := Link (Code);
   Term (Argument.Value.all).Location := Argument.Location;   
end Get_Numeric_Literal;

