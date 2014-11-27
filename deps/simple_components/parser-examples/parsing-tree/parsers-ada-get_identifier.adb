--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Ada.Get_Identifier                  Luebeck            --
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
   procedure Get_Identifier
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is
   Index     : Integer := Pointer + 1;
   Malformed : Boolean := False;
   Underline : Boolean := False;
   Symbol    : Character;
begin
   while Index <= Line'Last loop
      Symbol := Line (Index);
      if Is_Alphanumeric (Symbol) then
         Underline := False;
      elsif '_' = Symbol then
         Malformed := Malformed or Underline;
         Underline := True;
      else
         exit;
      end if;
      Index := Index + 1;
   end loop;
   Malformed := Malformed or Underline;
   Set_Pointer (Code, Index);
   Argument.Location := Link (Code);
   Argument.Value := new Identifier (Index - Pointer);
   declare
      This : Identifier renames Identifier (Argument.Value.all);
   begin
      This.Location  := Argument.Location;
      This.Malformed := Malformed;
      This.Value     := Line (Pointer..Index - 1);
   end;
end Get_Identifier;

