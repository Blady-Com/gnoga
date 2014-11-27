--                                                                    --
--  package Parsers.Ada             Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  13:43 19 Mar 2011  --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;

package body Parsers.Ada is

   function "and" (Left, Right : Operations) return Boolean is
   begin
      case Right is
         when Logical_And =>
            case Left is
               when Logical_Or | Logical_Xor | And_Then | Or_Else =>
                  return False;
               when others =>
                  return True;
            end case;
         when Logical_Or =>
            case Left is
               when Logical_And | Logical_Xor | And_Then | Or_Else =>
                  return False;
               when others =>
                  return True;
            end case;
         when Logical_Xor =>
            case Left is
               when Logical_And | Logical_Or | And_Then | Or_Else =>
                  return False;
               when others =>
                  return True;
            end case;
         when And_Then =>
            case Left is
               when Logical_And | Logical_Or | Logical_Xor | Or_Else =>
                  return False;
               when others =>
                  return True;
            end case;
         when Or_Else =>
            case Left is
               when Logical_And | Logical_Or | Logical_Xor | And_Then =>
                  return False;
               when others =>
                  return True;
            end case;
         when Unary =>
            case Left is
               when Additive | Unary | Multiplying | Highest =>
                  return False;
               when Attribute =>
                  return False;
               when others =>
                  return True;
            end case;
         when Highest =>
            case Left is
               when Highest =>
                  return False;
               when others =>
                  return True;
            end case;
          when Extend =>
            return Left /= Left_Index;
         when others =>
            return True;
      end case;
   end "and";

   function Is_Commutative (Left, Right : Operations) return Boolean is
   begin
      case Left is
         when Logical_And =>
            return Right = Logical_And;
         when Logical_Or =>
            return Right = Logical_Or;
         when Logical_Xor =>
            return Right = Logical_Xor;
         when Add | Sub =>
            case Right is
              when Add | Sub =>
                 return True;
              when others =>
                 return False;
            end case;
         when Mul | Div =>
            case Right is
              when Mul | Div =>
                 return True;
              when others =>
                 return False;
            end case;
         when Alternative | Associate | Component =>
            return Left = Right;
         when others =>
            return False;
      end case;
   end Is_Commutative;

   function Is_Inverse (Operation : Operations) return Boolean is
   begin
      case Operation is
         when Sub | Div =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Inverse;

   function Group_Inverse (Operation : Operations) return Operations is
   begin
      case Operation is
         when Add | Sub =>
            return Add_Inv;
         when Mul | Div =>
            return Mul_Inv;
         when others =>
            raise Program_Error;
      end case;
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
            (  Context   : access Ada_Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      Result : Node_Ptr := new Expression (List'Length);
   begin
      declare
         This : Expression renames Expression (Result.all);
      begin
         This.Operation := Operation.Operation;
         This.Location  := Operation.Location;
         for Argument in List'Range loop
            This.Operands (Integer (Argument)) :=
               List (Argument).Value;
         end loop;
      end;
      return (Result, Operation.Location & Link (List));
   end Call;

   function Enclose
            (  Context : access Ada_Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      Result : Node_Ptr := new Expression (List'Length);
   begin
      declare
         This : Expression renames Expression (Result.all);
      begin
         This.Operation := Left.Operation;
         This.Location  := Left.Location & Right.Location;
         for Argument in List'Range loop
            This.Operands (Integer (Argument)) :=
               List (Argument).Value;
         end loop;
      end;
      return (Result, Left.Location & Right.Location & Link (List));
   end Enclose;

   procedure Get_Character_Literal
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Identifier
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Numeric_Literal
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_String_Literal
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Operand
             (  Context  : in out Ada_Expression;
                Code     : in out Source'Class;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             )  is
      Line    : String renames Get_Line (Code);
      Pointer : Integer   := Get_Pointer (Code);
      Symbol  : Character := Line (Pointer);
   begin
      Got_It := True;
      if '"' = Symbol then
         Get_String_Literal (Code, Line, Pointer, Argument);
      elsif ''' = Symbol then
         Get_Character_Literal (Code, Line, Pointer, Argument);
      elsif Is_Decimal_Digit (Symbol) then
         Get_Numeric_Literal (Code, Line, Pointer, Argument);
      elsif Is_Letter (Symbol) then
         Get_Identifier (Code, Line, Pointer, Argument);
      else
         Got_It := False;
      end if;
   exception
      when End_Error =>
         Got_It := False;
   end Get_Operand;

   function Image (Item : Mark) return String is
   begin
      return "";
   end Image;

   function Image (Item : Numeric_Literal) return String is
   begin
      if Item.Malformed then
         return "<malformed>";
      elsif Item.Exponent = Integer'First then
         return "<underflown>";
      elsif Item.Exponent = Integer'Last then
         return "<overflown>";
      elsif Item.Base = 10 then
         if Item.Exponent = 0 then
            return Item.Value;
         else
            return Item.Value & "E" & Image (Item.Exponent);
         end if;
      else
         if Item.Exponent = 0 then
            return Image (Item.Base) & '#' & Item.Value & '#';
         else
            return
            (  Image (Item.Base)
            & '#' & Item.Value & '#'
            & "E" & Image (Item.Exponent)
            );
         end if;
      end if;
   end Image;

   function Image (Item : Integer_Literal) return String is
   begin
      return
      (  "Universal_Integer ("
      &  Image (Numeric_Literal (Item))
      &  ")"
      );
   end Image;

   function Image (Item : Real_Literal) return String is
   begin
      return
      (  "Universal_Real ("
      &  Image (Numeric_Literal (Item))
      &  ")"
      );
   end Image;

   function Image (Item : String_Literal) return String is
   begin
      return Quote (Item.Value);
   end Image;

   function Image (Item : Character_Literal) return String is
   begin
      return ''' & Item.Value & ''';
   end Image;

   function Image (Item : Identifier) return String is
   begin
      if Item.Malformed then
         return "<malformed>";
      else
         return Item.Value;
      end if;
   end Image;

   function Image (Item : Missing_Operand) return String is
   begin
      return "<missing>";
   end Image;

   function Image (Item : Expression) return String is
      function Image (List : Argument_List) return String is
      begin
         if List'Length = 0 then
            return ")";
         else
            return
            (  ", "
            &  Image (List (List'First).all)
            &  Image (List (List'First + 1..List'Last))
            );
         end if;
      end Image;
   begin
      return
      (  Operations'Image (Item.Operation)
      &  "("
      &  Image (Item.Operands (1).all)
      &  Image (Item.Operands (2..Item.Count))
      );
   end Image;

   use type Tokens.Descriptors.Descriptor_Class;
   use Lexers.Lexical_Descriptors.Operation;
   use Lexers.Lexical_Arguments;

   procedure On_Missing_Operation
             (  Context  : in out Ada_Expression;
                Code     : in out Source'Class;
                Modifier : Tokens.Operation_Token;
                Token    : out Lexers.Token_Lexer.Implementation.
                                  Lexical_Token;
                Got_It   : out Boolean
             )  is
   begin
      case Modifier.Operation is
         when Logical_And | Logical_Or =>
            Token  := (Operator, Modifier, 2, 2);
            Got_It := True;
         when Logical_Not =>
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "'in' is expected at "
               &  Image (Link (Code))
               &  " after 'not' at "
               &  Image (Modifier.Location)
            )  );
         when others =>
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "Unknown error after a modifier at "
               &  Image (Modifier.Location)
            )  );
      end case;
   end On_Missing_Operation;

   procedure On_Postmodifier
             (  Context  : in out Ada_Expression;
                Code     : in out Source'Class;
                Argument : in out Tokens.Argument_Token;
                Modifier : Tokens.Operation_Token;
                Got_It   : out Boolean
             )  is
   begin
      case Modifier.Operation is
         when Keyword_Record =>
            if (  Argument.Value.all in Identifier'Class
               and then
                  Identifier'Class (Argument.Value.all).Value = "null"
               )
            then
               Free (Argument.Value);
               Argument.Value := new Identifier (11);
               Identifier (Argument.Value.all).Value := "null record";
               Argument.Location :=
                  Argument.Location & Modifier.Location;
               Got_It := True;
               return;
            end if;
         when others =>
            null;
      end case;
      Reset_Pointer (Code);
      Got_It := False;
   end On_Postmodifier;

   procedure On_Premodifier
             (  Context  : in out Ada_Expression;
                Code     : in out Source'Class;
                Token    : in out Lexers.Token_Lexer.Implementation.
                                     Lexical_Token;
                Modifier : Tokens.Operation_Token;
                Got_It   : out Boolean
             )  is
   begin
      case Modifier.Operation is
         when Logical_And =>
            if (  Token.Class = Postmodifier
               and then
                  Token.Operation.Operation = And_Then
               )
            then
               Token :=
                  (  Operator,
                     (  And_Then,
                        Token.Operation.Location & Modifier.Location
                     ),
                     2,
                     2
                  );
               Got_It := True;
               return;
            else
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  (  "'then' is expected at "
                  &  Image (Token.Operation.Location)
                  &  " after 'and' at "
                  &  Image (Modifier.Location)
               )  );
            end if;
         when Logical_Or =>
            if (  Token.Class = Postmodifier
               and then
                  Token.Operation.Operation = Or_Else
               )
            then
               Token :=
                  (  Operator,
                     (  Or_Else,
                        Token.Operation.Location & Modifier.Location
                     ),
                     2,
                     2
                  );
               Got_It := True;
               return;
            else
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  (  "'else' is expected at "
                  &  Image (Token.Operation.Location)
                  &  " after 'or' at "
                  &  Image (Modifier.Location)
               )  );
            end if;
         when Logical_Not =>
            if (  Token.Class = Operator
               and then
                  Token.Operation.Operation = Member
               )
            then
               Token :=
                  (  Operator,
                     (  Not_Member,
                        Token.Operation.Location & Modifier.Location
                     ),
                     3,
                     3
                  );
               Got_It := True;
               return;
            else
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  (  "'in' is expected at "
                  &  Image (Token.Operation.Location)
                  &  " after 'not' at "
                  &  Image (Modifier.Location)
               )  );
            end if;
         when others =>
            null;
      end case;
      Reset_Pointer (Code);
      Got_It := False;
   end On_Premodifier;

begin
   Add_Operator (Infixes, "|",        Alternative, 0, 0);
   Add_Operator (Infixes, "..",       Ellipsis,    1, 1);

   Add_Operator (Infixes, "xor",      Logical_Xor, 2, 2);

   Add_Operator (Infixes, "=",        EQ,          3, 3);
   Add_Operator (Infixes, "/=",       NE,          3, 3);
   Add_Operator (Infixes, "<",        LT,          3, 3);
   Add_Operator (Infixes, "<=",       LE,          3, 3);
   Add_Operator (Infixes, ">",        GT,          3, 3);
   Add_Operator (Infixes, ">=",       GE,          3, 3);
   Add_Operator (Infixes, "in",       Member,      3, 3);

   Add_Operator (Infixes, "+",        Add,         4, 4);
   Add_Operator (Infixes, "-",        Sub,         4, 4);
   Add_Operator (Infixes, "&",        Concatenate, 4, 4);

   Add_Operator (Prefixes, "+",       Plus,        5, 5);
   Add_Operator (Prefixes, "-",       Minus,       5, 5);

   Add_Operator (Infixes,  "*",       Mul,         6, 6);
   Add_Operator (Infixes,  "/",       Div,         6, 6);
   Add_Operator (Infixes,  "mod",     Modulus,     6, 6);
   Add_Operator (Infixes,  "rem",     Remainder,   6, 6);

   Add_Operator (Prefixes, "abs",     Abs_Value,   7, 7);
   Add_Operator (Prefixes, "not",     Logical_Not, 7, 7);
   Add_Operator (Infixes,  "**",      Pow,         7, 7);

   Add_Operator (Prefixes, "new",     Allocator,   8,  8);
   Add_Operator (Infixes,  "'",       Attribute,   9,  9);
   Add_Operator (Infixes,  ".",       Component,  10, 10);
   Add_Index    (Infixes,  "(",       Left_Index,  9);

   Add_Comma    (Infixes,  ",",       Comma);
   Add_Ligature (Infixes,  "=>",      Associate);
   Add_Bracket  (Prefixes, "(",       Left_Bracket);
   Add_Bracket  (Postfixes, ")",      Right_Bracket);

   Add_Premodifier  (Postfixes, "and",  Logical_And);
   Add_Premodifier  (Postfixes, "or",   Logical_Or);
   Add_Premodifier  (Postfixes, "not",  Logical_Not);
   Add_Postmodifier (Infixes,   "then", And_Then);
   Add_Postmodifier (Infixes,   "else", Or_Else);

   Add_Postmodifier (Postfixes, "record", Keyword_Record);

   Add_Semicolon (Infixes, "with", Extend, Parsers.Sublist_Open, 1);

   Add_Postmodifier (Infixes, "is",   Reserved);
   Add_Postmodifier (Infixes, "loop", Reserved);
   Add_Postmodifier (Infixes, "do",   Reserved);

end Parsers.Ada;
