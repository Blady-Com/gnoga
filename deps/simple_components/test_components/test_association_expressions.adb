--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Association_Expressions                Luebeck            --
--  Test package implementation                    Winter, 2004       --
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

package body Test_Association_Expressions is 

   function "and" (Left, Right : Operations) return Boolean is
   begin
      return True;
   end "and";

   function Is_Commutative (Left, Right : Operations) return Boolean is
   begin
      case Left is
         when Add | Sub =>
            return Right = Add or Right = Sub;
         when Mul | Div =>
            return Right = Mul or Right = Div;
         when others =>
            return False;
      end case;
   end Is_Commutative;

   function Is_Inverse (Operation : Operations) return Boolean is
   begin
      return Operation = Sub or else Operation = Div;
   end Is_Inverse;

   function Group_Inverse (Operation : Operations) return Operations is
   begin
      case Operation is
         when Add | Sub =>
            return Minus;
         when Mul | Div =>
            return Inv;
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

   function Arg_List (List : Tokens.Arguments.Frame)
      return Unbounded_String is
      Result : Unbounded_String;
   begin
      for Argument in List'Range loop
         if Length (Result) > 0 then
            Append (Result, ", ");
         end if;
         Append (Result, List (Argument).Value);
      end loop;
      return "(" & Result & ")";
   end Arg_List;

   function Call
            (  Context   : access Test_Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
   begin
      return
      (  Operations'Image (Operation.Operation) & Arg_List (List),
         Operation.Location & Link (List)
      );
   end Call;

   function Enclose
            (  Context : access Test_Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      function Img (Bracket : Operations) return String is
      begin
         case Bracket is
            when Left_Bracket  => return "";
            when Right_Bracket => return "";
            when others        => return Operations'Image (Bracket);
         end case;
      end Img;
   begin
      return
      (  Img (Left.Operation) & Arg_List (List) & Img (Right.Operation),
         Left.Location & Right.Location & Link (List)
      );
   end Enclose;

   procedure Get_Operand
             (  Context  : in out Test_Expression;
                Code     : in out Source'Class;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             )  is
      Line    : String renames Get_Line (Code);
      Pointer : Integer := Get_Pointer (Code);
      Value   : Unbounded_String;
   begin
      while (  Pointer <= Line'Last
            and then
               Is_Alphanumeric (Line (Pointer))
            )
      loop
         Append (Value, Line (Pointer));
         Pointer := Pointer + 1;
      end loop;
      if Length (Value) > 0 then
         Set_Pointer (Code, Pointer);
         Argument := (Value, Link (Code));
         Got_It := True;
      else
         Got_It := False;
      end if;
   end Get_Operand;

begin
   Add_Operator (Prefixes, "abs", Abs_Value,  10, 9);
   Add_Operator (Prefixes, "++",  Preinc,     10, 9);
   Add_Operator (Prefixes, "--",  Predec,     10, 9);
   Add_Operator (Prefixes, "+",   Plus,       10, 9);
   Add_Operator (Prefixes, "-",   Minus,      10, 9);
   Add_Bracket  (Prefixes, "(",   Left_Bracket);
   Add_Operator (Prefixes, "?",   Pre_Query,  1,  2);

   Add_Operator (Infixes, "+",  Add,  1, 2);
   Add_Operator (Infixes, "-",  Sub,  1, 3);
   Add_Operator (Infixes, "*",  Mul,  3, 4);
   Add_Operator (Infixes, "/",  Div,  3, 4);
   Add_Operator (Infixes, "**", Pow, 11, 5);
   Add_Index    (Infixes, "(",  Left_Index, 4);
   Add_Comma    (Infixes, ",",  Comma);
   
   Add_Bracket  (Postfixes, ")",  Right_Bracket);  
   Add_Operator (Postfixes, "++", Postinc, 7, 8);
   Add_Operator (Postfixes, "--", Postdec, 7, 8);
   Add_Operator (Postfixes, "@",  Post_at, 2, 1);
   
   Add_Semicolon (Infixes, "|", Bar,    Parsers.Sublist_Separator, 1);
   Add_Semicolon (Infixes, ":", Colon,  Parsers.Sublist_Separator, 2);
   Add_Semicolon (Infixes, "#", Sharp,  Parsers.Sublist_Separator, 3);
   Add_Semicolon (Infixes, "$", Dollar, Parsers.Sublist_Separator, 1);
   Add_Semicolon (Infixes, "with", Extension, Parsers.Sublist_Open,  0);
   Add_Semicolon (Infixes, "out",  Closure, Parsers.Sublist_Close,   0);
   Add_Semicolon (Infixes, "<:", Colon_Open,  Parsers.Sublist_Open,  2);
   Add_Semicolon (Infixes, ":>", Colon_Close, Parsers.Sublist_Close, 2);
end Test_Association_Expressions;
