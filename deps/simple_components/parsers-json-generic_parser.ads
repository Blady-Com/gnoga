--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.JSON.Generic_Parser                 Luebeck            --
--  Interface                                      Autumn, 2019       --
--                                                                    --
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
--
--  This  generic package  is an JSON parser. It can be instantiated for
--  any  type  of  sources,  but  usually  it makes sense for multi-line
--  sources only. The JSON format is governed by RFC 7159.
--
with System.Storage_Pools;  use System.Storage_Pools;

with Parsers.Generic_Source;
with Parsers.Generic_Token.Segmented_Lexer;

generic
   with package Sources is new Parsers.Generic_Source (<>);
package Parsers.JSON.Generic_Parser is
--
-- Parse -- A JSON value
--
--    Code  - To be parsed
--    Arena - The arena storage pool to allocate JSON data
--
-- Returns :
--
--    The JSON value with parts allocated in Arena
--
-- Exceptions :
--
--    Storage_Error - No room in Arena to allocate parts of the value
--    Syntax_Error  - Any syntax error
--
   function Parse
            (  Code  : access Sources.Source_Type;
               Arena : access Root_Storage_Pool'Class
            )  return JSON_Value;
private
   type Argument_Item is record
      Nothing : Boolean;
      Data    : JSON_Argument;
   end record;

   package Tokens is
      new Parsers.Generic_Token
          (  Operation_Type => Operations,
             Argument_Type  => Argument_Item,
             Priority_Type  => Priorities,
             Sources        => Sources
          );
   use Tokens;
--
-- The tables of prefix, infix and postfix operations
--
   Prefixes  : aliased Tokens.Vocabulary.Table;
   Infixes   : aliased Tokens.Vocabulary.Table;
   Postfixes : aliased Tokens.Vocabulary.Table;
--
-- Lexers -- Table driven lexers
--
   package Lexers is new Tokens.Segmented_Lexer;
--
-- Expression -- The lexer using our tables
--
   type Expression
        (  Arena : access Root_Storage_Pool'Class
        )  is new Lexers.Lexer
                  (  Prefixes  => Prefixes'Access,
                     Infixes   => Infixes'Access,
                     Postfixes => Postfixes'Access
                  )  with null record;
   function Call
            (  Context   : access Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return  Tokens.Argument_Token;
   function Enclose
            (  Context : access Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token;
   procedure Get_Blank
             (  Context : in out Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                Got_It  : out Boolean
             );
   procedure Get_Operand
             (  Context  : in out Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             );overriding
   procedure On_Missing_Operand
             (  Context   : in out Expression;
                Code      : in out Lexers.Lexer_Source_Type;
                Operation : Tokens.Operation_Token;
                Argument  : out Tokens.Argument_Token
             );

end Parsers.JSON.Generic_Parser;
