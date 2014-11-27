--                                                                    --
--  package Calculator              Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
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

with Parsers.String_Source;          use Parsers.String_Source;
with Parsers.Generic_Lexer.Blanks;
with Parsers.Generic_Token.Segmented_Lexer;
with Tables.Names;

package Calculator is
--
-- Calculate -- A primitive floating-point calculator
--
--    Formula - To be evaluated
--
-- Returns :
--
--    The result of Formula
--
-- Exceptions :
--
--    Syntax_Error  - Any syntax error
--    Numeric_Error - Any numeric error
--
   function Calculate (Formula : String) return Float;

private
--
-- Operations -- All the operations supported
--
   type Operations is
        (  Add, Sub, Mul, Div, Pow,      -- Infix operators
           Abs_Value, Plus, Minus,       -- Prefix operators
           Left_Bracket, Right_Bracket   -- Brackets
        );
--
-- "and" -- Checks operation associations, always True (Ok)
--
   function "and" (Left, Right : Operations) return Boolean;
--
-- Is_Commutative -- No commutative operations, always False
--
   function Is_Commutative (Left, Right : Operations) return Boolean;
--
-- Is_Inverse -- No commutative operations, always False
--
   function Is_Inverse (Operation : Operations) return Boolean;
--
-- Group_Inverse -- No commutative operations, never called
--
   function Group_Inverse (Operation : Operations) return Operations;
--
-- Priorities -- The levels of association
--
   type Priorities is mod 10;
--
-- Tokens -- The lexical tokens
--
   package Tokens is
      new Parsers.Generic_Token
          (  Operation_Type => Operations,
             Argument_Type  => Float,
             Priority_Type  => Priorities,
             Sources        => Code
          );
   use Tokens;
--
-- Check_Spelling -- Of a name, no checks
--
   procedure Check_Spelling (Name : String);
--
-- Check_Matched -- Check if no broken keyword matched
--
   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean;
--
-- Token_Tables -- Case-insensitive tables of tokens
--
   package Token_Tables is new Tokens.Vocabulary.Names;
--
-- The tables of prefix, infix and postfix operations
--
   Prefixes  : aliased Token_Tables.Dictionary;
   Infixes   : aliased Token_Tables.Dictionary;
   Postfixes : aliased Token_Tables.Dictionary;
--
-- Lexers -- Table driven lexers
--
   package Lexers is new Tokens.Segmented_Lexer;
--
-- Blank_Skipping_Lexers -- Ones that skip blanks
--
   package Blank_Skipping_Lexers is
      new Lexers.Token_Lexer.Implementation.Blanks (Lexers.Lexer);
--
-- Expression -- The lexer using our tables
--
   type Expression is
      new Blank_Skipping_Lexers.Lexer
          (  Prefixes  => Prefixes'Access,
             Infixes   => Infixes'Access,
             Postfixes => Postfixes'Access
          )  with null record;
--
-- Call -- Evaluates an operator 
--
   function Call
            (  Context   : access Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token;
--
-- Enclose -- Evaluates an expression in brackets
--
   function Enclose
            (  Context : access Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token;
--
-- Get_Operand -- Recognizes an operand (float number)
--
   procedure Get_Operand
             (  Context  : in out Expression;
                Code     : in out Source;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             );
end Calculator;
