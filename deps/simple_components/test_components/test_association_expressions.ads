--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Association_Expression                 Luebeck            --
--  Test package interface                         Winter, 2004       --
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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Parsers.Multiline_Source;  use Parsers.Multiline_Source;

with Parsers.Generic_Lexer.Cpp_Blanks;
with Parsers.Generic_Token.Segmented_Lexer;
with Tables.Names;

package Test_Association_Expressions is

   type Operations is
        (  Add, Sub, Mul, Div, Pow,
           Abs_Value, Plus, Minus, Preinc, Predec, Postinc, Postdec,
           Inv,
           Pre_Query, Post_At,
           Left_Bracket, Right_Bracket, Left_Index, Comma,
           Colon, Bar, Sharp, Dollar,
           Extension, Closure, Colon_Open, Colon_Close
        );
   function "and" (Left, Right : Operations) return Boolean;
   function Is_Commutative (Left, Right : Operations) return Boolean;
   function Is_Inverse (Operation : Operations) return Boolean;
   function Group_Inverse (Operation : Operations) return Operations;

   type Priorities is mod 20;

   procedure Check_Spelling (Name : String);
   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean;

   package Tokens is
      new Parsers.Generic_Token
          (  Operation_Type => Operations,
             Argument_Type  => Unbounded_String,
             Priority_Type  => Priorities,
             Sources        => Parsers.Multiline_Source.Code
          );
   use Tokens;

   package Token_Tables is new Tokens.Vocabulary.Names;

   Prefixes  : aliased Token_Tables.Dictionary;
   Infixes   : aliased Token_Tables.Dictionary;
   Postfixes : aliased Token_Tables.Dictionary;

   package Lexers is new Tokens.Segmented_Lexer;
   package Blank_Skipping_Lexers is
      new Lexers.Token_Lexer.Implementation.Cpp_Blanks (Lexers.Lexer);

   type Test_Expression is
      new Blank_Skipping_Lexers.Lexer
          (  Prefixes  => Prefixes'Access,
             Infixes   => Infixes'Access,
             Postfixes => Postfixes'Access
          )  with null record;

   function Call
            (  Context   : access Test_Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token;
   function Enclose
            (  Context : access Test_Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token;
   procedure Get_Operand
             (  Context  : in out Test_Expression;
                Code     : in out Source'Class;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             );
end Test_Association_Expressions;
