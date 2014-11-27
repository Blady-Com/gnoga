--                                                                    --
--  procedure Test_Ada_Parser       Copyright (c)  Dmitry A. Kazakov  --
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

with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Text_IO;                   use Ada.Text_IO;
with Parsers.Ada;                   use Parsers.Ada;
with Parsers.Multiline_Source;      use Parsers.Multiline_Source;

with Parsers.Multiline_Source.Text_IO;

procedure Test_Ada_Parser is
   use Lexers;
   use Tokens;

   File   : aliased File_Type;
   Parser : Ada_Expression;
   Result : Argument_Token;
   Stub   : Node_Ptr;
begin
   Open (File, In_File, "test_ada_parser.txt");
   declare
      Code : Parsers.Multiline_Source.Text_IO.Source (File'Access);
   begin
      loop
         Stub := new Mark; -- Mark the tree stack
         begin
            Parse (Parser, Code, Result);
            Put_Line
            (  Image (Result.Location)
            &  ": "
            &  Image (Result.Value.all)
            &  " ends at "
            &  Image (Link (Code))
            );
         exception
            when Error : Parsers.Syntax_Error =>
               Put_Line ("Error : " & Exception_Message (Error));
         end;
         Free (Stub);      -- Release the stack
         Next_Line (Code);
      end loop;
   exception
      when End_Error =>
         Put_Line ("----Normal completion----");
   end;
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
end Test_Ada_Parser;
