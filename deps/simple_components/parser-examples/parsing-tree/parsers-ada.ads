--                                                                    --
--  package Parsers.Ada             Copyright (c)  Dmitry A. Kazakov  --
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
--
--  This package provides a full Ada 95 expression parser. The result of
--  parsing is stored in a parsing tree allocated on a stack pool.
--
with Ada.Unchecked_Deallocation;
with Parsers.Multiline_Source;          use Parsers.Multiline_Source;
with Parsers.Generic_Lexer.Ada_Blanks;
with Parsers.Generic_Token.Segmented_Lexer;
with Stack_Storage;
with Tables.Names;

package Parsers.Ada is
--
-- Operations -- All the operations supported
--
   type Operations is
        (     -- Operators according to ARM 4.5
           Logical_And, Logical_Or, Logical_Xor, -- Logical operators
           And_Then, Or_Else,                    -- Short-circuit
           EQ, NE, LT, LE, GE, GT,               -- Relational
           Member, Not_Member,                   -- Membership tests
           Add, Sub, Concatenate,                -- Binary adding
           Plus, Minus,                          -- Unary adding
           Mul, Div, Modulus, Remainder,         -- Multiplying
           Pow, Abs_Value, Logical_Not,          -- Highest precedence
              -- Hard-wired operators
           Allocator,           -- Allocator "new"
           Alternative,         -- Alternative separator "|"
           Attribute,           -- Attribute specification "'"
           Ellipsis,            -- Range ".."
           Component,           -- Component extraction "."
              -- Order and aggregate brackets
           Left_Bracket, Right_Bracket,          -- Brackets ()
              -- Index brackets
           Left_Index,                           -- Brackets f()
              -- Commas and ligatures
           Comma, Associate, Extend,             -- ",", "=>", "with"
              -- Inverses
           Add_Inv, Mul_Inv,                     -- 0-x, 1/x
              -- Keywords
           Keyword_Record,                       -- "record"
           Reserved                              -- "is", "loop", "do"
        );
   subtype Logical     is Operations range Logical_And..Or_Else;
   subtype Relational  is Operations range EQ..Not_Member;
   subtype Additive    is Operations range Add..Concatenate;
   subtype Unary       is Operations range Plus..Minus;
   subtype Multiplying is Operations range Mul..Remainder;
   subtype Highest     is Operations range Pow..Logical_Not;
--
-- "and" -- Checks operation associations
--
--     Left  - The operation on the left
--     Right - The operation on the right
--
-- Returns :
--
--     True if Left is compatible with Right
--
   function "and" (Left, Right : Operations) return Boolean;
--
-- Is_Commutative -- Commutative operations
--
--     Left  - The operation on the left
--     Right - The operation on the right
--
-- Commutative groups:
--
--     {+, -}, {*, /}, {and}, {or}, {xor}, {.}, {|}
--
-- Though  A.B  is  not commutative, it makes sense to treat it as if it
-- were commutative to parse A.B.C as "."(A,B,C).
--
-- Returns :
--
--     True if Left and Right are from a group
--
   function Is_Commutative (Left, Right : Operations) return Boolean;
--
-- Is_Inverse -- Of a group
--
--     Operation - To be tested
--
-- Returns :
--
--     True if - or /
--
   function Is_Inverse (Operation : Operations) return Boolean;
--
-- Group_Inverse -- Of a group
--
--     Operation - An operation of either {+, -} or {*, /}
--
-- Returns :
--
--     Add_Inv for + or -
--     Mul_Inv for * or /
--
   function Group_Inverse (Operation : Operations) return Operations;
--
-- Priorities -- The levels of association
--
   type Priorities is mod 12;
--
-- Parsing  tree.  To  make  it  efficient  the  nodes  of  the tree are
-- allocated  on  a  stack.  The stack is provided by a stack pool. This
-- allows  to  remove the whole tree by deallocating its first allocated
-- node or any other pool object allocated before it. Tree_Pool  is  the
-- stack storage pool used for this.
--
   Tree_Pool : Stack_Storage.Pool (2048, 128);
--
-- Node -- Of a parsing tree
--
   type Node is abstract tagged limited null record;
--
-- Image -- To be used for tree output
--
--    Item - The node
--
-- Returns :
--
--    The string representation of the node
--
   function Image (Item : Node) return String is abstract;
--
-- Node_Ptr -- Pointer to a node, class-wide, Tree_Pool specific
--
   type Node_Ptr is access Node'Class;
   for Node_Ptr'Storage_Pool use Tree_Pool;
   procedure Free is
      new Standard.Ada.Unchecked_Deallocation (Node'Class, Node_Ptr);
   --
   -- Mark -- Marks the pool state for quick tree removal
   --
   type Mark is new Node with null record;
   function Image (Item : Mark) return String;
   --
   -- Term -- Expression term, abstract base type
   --
   type Term is abstract new Node with record
      Location : Parsers.Multiline_Source.Location;
   end record;
   --
   -- Literal -- Expression literal, abstract base type
   --
   type Literal (Length : Natural) is abstract new Term with record
      Value : String (1..Length);
   end record;
   --
   -- Numeric_Literal -- A numeric literal, abstract base type
   --
   -- The  field  Malformed is set to true to indicate a syntax error in
   -- the  literal, which was detected and corrected. The field Exponent
   -- is set to Integer'First or Integer'Last  when  the  exponent  part
   -- cannot be represented (it is too big). The  field  Value  contains
   -- the mantissa, which is always whole.
   --
   subtype Number_Base is Integer range 2..16;
   type Numeric_Literal is abstract new Literal with record
      Malformed : Boolean     := False;
      Base      : Number_Base := 10;
      Exponent  : Integer;
   end record;
   function Image (Item : Numeric_Literal) return String;
   --
   -- Integer_Literal -- Represents integer literals
   --
   type Integer_Literal is new Numeric_Literal with null record;
   function Image (Item : Integer_Literal) return String;
   --
   -- Real_Literal -- Represents real literals
   --
   type Real_Literal is new Numeric_Literal with null record;
   function Image (Item : Real_Literal) return String;
   --
   -- String_Literal -- Represents string literals
   --
   type String_Literal is new Literal with null record;
   function Image (Item : String_Literal) return String;
   --
   -- Character_Literal -- Represents character literals
   --
   type Character_Literal is new Term with record
      Value : Character;
   end record;
   function Image (Item : Character_Literal) return String;
   --
   -- Identifier -- Represents identifiers
   --
   type Identifier is new Literal with record
      Malformed : Boolean := False;
   end record;
   function Image (Item : Identifier) return String;
   --
   -- Missing operand -- Represents an assumed operand
   --
   type Missing_Operand is new Term with null record;
   function Image (Item : Missing_Operand) return String;
   --
   -- Expression -- Non-terminal node
   --
   type Argument_List is array (Positive range <>) of Node_Ptr;
   type Expression (Count : Positive) is new Node with record
      Operation : Operations;
      Location  : Parsers.Multiline_Source.Location;
      Operands  : Argument_List (1..Count);
   end record;
   function Image (Item : Expression) return String;
--
-- Tokens -- The lexical tokens
--
   package Tokens is
      new Parsers.Generic_Token
          (  Operation_Type => Operations,
             Argument_Type  => Node_Ptr,
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
      new Lexers.Token_Lexer.Implementation.Ada_Blanks (Lexers.Lexer);
--
-- Expression -- The lexer using our tables
--
   type Ada_Expression is
      new Blank_Skipping_Lexers.Lexer
          (  Prefixes  => Prefixes'Access,
             Infixes   => Infixes'Access,
             Postfixes => Postfixes'Access
          )  with null record;
--
-- Call -- Evaluates an operator
--
   function Call
            (  Context   : access Ada_Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token;
--
-- Enclose -- Evaluates an expression in brackets
--
   function Enclose
            (  Context : access Ada_Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token;
--
-- Get_Operand -- Recognizes an operand (float number)
--
   procedure Get_Operand
             (  Context  : in out Ada_Expression;
                Code     : in out Source'Class;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             );
--
-- On_Postmodifier -- Overrides the default handling of modifiers
--
   procedure On_Postmodifier
             (  Context  : in out Ada_Expression;
                Code     : in out Source'Class;
                Argument : in out Tokens.Argument_Token;
                Modifier : Tokens.Operation_Token;
                Got_It   : out Boolean
             );
--
-- On_Premodifier -- Overrides the default handling of modifiers
--
   procedure On_Premodifier
             (  Context  : in out Ada_Expression;
                Code     : in out Source'Class;
                Token    : in out Lexers.Token_Lexer.Implementation.
                                     Lexical_Token;
                Modifier : Tokens.Operation_Token;
                Got_It   : out Boolean
             );
--
-- On_Missing_Operation -- To deal with "and", "or" etc
--
   procedure On_Missing_Operation
             (  Context  : in out Ada_Expression;
                Code     : in out Source'Class;
                Modifier : Tokens.Operation_Token;
                Token    : out Lexers.Token_Lexer.Implementation.
                                  Lexical_Token;
                Got_It   : out Boolean
             );
end Parsers.Ada;
