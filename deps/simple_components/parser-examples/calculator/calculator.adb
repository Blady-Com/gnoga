--                                                                    --
--  package Calculator              Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2004       --
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

with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.IO_Exceptions;              use Ada.IO_Exceptions;
with Strings_Edit;                   use Strings_Edit;
with Strings_Edit.Floats;            use Strings_Edit.Floats;

with Ada.Numerics.Elementary_Functions;
use  Ada.Numerics.Elementary_Functions;

package body Calculator is 

   function "and" (Left, Right : Operations) return Boolean is
   begin
      return True;
   end "and";

   function Is_Commutative (Left, Right : Operations) return Boolean is
   begin
      return False;
   end Is_Commutative;

   function Is_Inverse (Operation : Operations) return Boolean is
   begin
      return False;
   end Is_Inverse;

   function Group_Inverse (Operation : Operations) return Operations is
   begin
      return Minus;
   end Group_Inverse;

   procedure Check_Spelling (Name : String) is
   begin
      null;
   end Check_Spelling;

   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean is
   begin
      return
      (  not Is_Alphanumeric (Source (Pointer))
      or else
         not Is_Alphanumeric (Source (Pointer - 1))
      );
   end Check_Matched;

   function Call
            (  Context   : access Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      Result : Float;
   begin
      case Operation.Operation is
         when Abs_Value =>
            Result := abs List (List'First).Value;
         when Add =>
            Result := List (List'First).Value + List (List'Last).Value;
         when Sub =>
            Result := List (List'First).Value - List (List'Last).Value;
         when Mul =>
            Result := List (List'First).Value * List (List'Last).Value;
         when Div =>
            Result := List (List'First).Value / List (List'Last).Value;
         when Pow =>
            Result :=
               exp (log (List (List'First).Value) * List (List'Last).Value);
         when Plus =>
            Result := List (List'First).Value;
         when Minus =>
            Result := -List (List'First).Value;
         when others =>
            raise Program_Error;
      end case;
      if Result'Valid then
         return (Result, Operation.Location & Link (List));
      else
         Raise_Exception
         (  Numeric_Error'Identity,
            (  "Numeric error in "
            &  Operations'Image (Operation.Operation)
            &  " at " & Image (Operation.Location)
         )  );
      end if;
   exception
      when Program_Error =>
         raise;
      when others =>
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Numeric error in "
            &  Operations'Image (Operation.Operation)
            &  " at " & Image (Operation.Location)
         )  );
   end Call;

   function Enclose
            (  Context : access Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
   begin
      return
      (  List (List'First).Value,
         Left.Location & Right.Location
      );
   end Enclose;

   procedure Get_Operand
             (  Context  : in out Expression;
                Code     : in out Source;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             )  is
      Line    : String renames Get_Line (Code);
      Pointer : Integer := Get_Pointer (Code);
      Value   : Float;
   begin
      Get (Line, Pointer, Value);
      Set_Pointer (Code, Pointer);
      Argument := (Value, Link (Code));
      Got_It := True;
   exception
      when End_Error =>
         Got_It := False;
      when Constraint_Error =>
         Set_Pointer (Code, Pointer);
         Raise_Exception
         (  Numeric_Error'Identity,
            "Too large number at " & Image (Link (Code))
         );      
      when Data_Error =>
         Set_Pointer (Code, Pointer);
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Wrong number at " & Image (Link (Code))
         );      
   end Get_Operand;

   Reckoner : Expression;

   function Calculate (Formula : String) return Float is
      Copy   : aliased String := Formula;
      Code   : Source (Copy'Access); 
      Result : Tokens.Argument_Token;
   begin
      Lexers.Parse (Reckoner, Code, Result);
      if Get_Pointer (Code) <= Copy'Last then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            (  "Unrecognized '"
            &  Copy (Get_Pointer (Code)..Copy'Last)
            &  "'"
         )  );
      end if;
      return Result.Value;
   end Calculate;

begin
   Add_Operator (Prefixes, "abs", Abs_Value,  8, 7);
   Add_Operator (Prefixes, "+",   Plus,       8, 7);
   Add_Operator (Prefixes, "-",   Minus,      8, 7);
   Add_Bracket  (Prefixes, "(",   Left_Bracket);

   Add_Operator (Infixes, "+",  Add, 1, 2);
   Add_Operator (Infixes, "-",  Sub, 1, 3);
   Add_Operator (Infixes, "*",  Mul, 3, 4);
   Add_Operator (Infixes, "/",  Div, 3, 4);
   Add_Operator (Infixes, "**", Pow, 9, 5);
   
   Add_Bracket  (Postfixes, ")", Right_Bracket);  
end Calculator;
