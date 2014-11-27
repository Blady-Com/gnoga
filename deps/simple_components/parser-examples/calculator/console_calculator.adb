--                                                                    --
--  procedure Console_Calculator    Copyright (c)  Dmitry A. Kazakov  --
--  A simple calculator                            Luebeck            --
--  Implementation                                 Winter, 2004       --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Text_IO;          use Ada.Text_IO;
with Strings_Edit.Floats;  use Strings_Edit.Floats;
with Calculator;           use Calculator;
with Parsers;

procedure Console_Calculator is 
   Text : String (1..120);
   Last : Integer;
begin
   Put_Line ("Enter an expression to calculate and hit <enter>");
   Put_Line ("The operations supported are +, -, /, *, **, abs, ()");
   Put_Line ("   (to exit enter an empty string)");
   loop
      Put (">");
      Get_Line (Text, Last);
      exit when Last < Text'First;
      begin
         Put_Line ("=" & Image (Calculate (Text (1..Last))));
      exception
         when Error : Numeric_Error | Parsers.Syntax_Error =>
            Put_Line (Exception_Message (Error));
      end;
   end loop; 
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
end Console_Calculator;
