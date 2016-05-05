--                                                                    --
--  procedure Test_Association      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2004       --
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
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Text_IO;                   use Ada.Text_IO;
with Strings_Edit.Quoted;           use Strings_Edit.Quoted;
with Parsers.Multiline_Source;      use Parsers.Multiline_Source;
with Test_Association_Expressions;  use Test_Association_Expressions;

with Parsers.Generic_Source.Get_Text;
with Parsers.Multiline_Source.Text_IO;

procedure Test_Association is
   use Lexers;
   use Tokens;

   procedure Get_Text is new Parsers.Multiline_Source.Code.Get_Text;

   File   : aliased File_Type;
   Parser : Test_Expression;
   Result : Argument_Token;
   Got_It : Boolean;

begin
   Open (File, In_File, "test_association.txt");
   declare
      Code : Text_IO.Source (File'Access);
   begin
      while not End_Of (Code) loop
         Parse (Parser, Code, Result);
         Get_Blank (Parser, Code, Got_It);
         Get_Text (Code, ":=", Got_It);
         if Got_It then
            Get_Blank (Parser, Code, Got_It);
            declare
               Line    : String renames Get_Line (Code);
               Pointer : aliased Integer := Get_Pointer (Code);
            begin
               if Line (Pointer) = '"' then
                  declare
                     Text : constant String :=
                            Get_Quoted (Line, Pointer'Access);
                  begin
                     if Text /= To_String (Result.Value) then
                        Put_Line ("Expected:'" & Text & "'");
                        Put_Line
                        (  "     Got:'"
                        &  To_String (Result.Value)
                        &  "'"
                        );
                        return;
                     end if;
                     Set_Pointer (Code, Line'Last + 1);
                  end;
               else
                  Put_Line
                  (  "Expected nothing, got:'"
                  &  To_String (Result.Value)
                  &  "'"
                  );
                  return;
               end if;
            end;
         else
            Put_Line ("Got:" & To_String (Result.Value));
            Get_Text (Code, ";", Got_It);
         end if;
         Get_Blank (Parser, Code, Got_It);
      end loop;
      Put ("It looks all right!");
   end;
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
end Test_Association;
