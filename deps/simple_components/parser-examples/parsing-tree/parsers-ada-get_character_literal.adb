--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Ada.Get_Character_Literal           Luebeck            --
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
   procedure Get_Character_Literal
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is
begin
   if Pointer + 2 <= Line'Last and then ''' = Line (Pointer + 2) then
      Set_Pointer (Code, Pointer + 3);
      Argument.Location := Link (Code);
      Argument.Value    := new Character_Literal;
      Character_Literal (Argument.Value.all).Value :=
         Line (Pointer + 1);
      return;
   end if;
   Set_Pointer (Code, Pointer + 1);
   Raise_Exception
   (  Parsers.Syntax_Error'Identity,
      (  "Missing ' in the character literal at "
      &  Image (Link (Code))
   )  );
end Get_Character_Literal;
