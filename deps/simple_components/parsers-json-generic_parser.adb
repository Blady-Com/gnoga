--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.JSON.Generic_Parser                 Luebeck            --
--  Implementation                                 Autumn, 2019       --
--                                                                    --
--                                Last revision :  18:40 23 Oct 2021  --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Strings_Edit.Integers;     use Strings_Edit.Integers;
with Strings_Edit.Long_Floats;  use Strings_Edit.Long_Floats;
with Strings_Edit.UTF8;         use Strings_Edit.UTF8;

package body Parsers.JSON.Generic_Parser is
   use Sources;

   function Call
               (  Context   : access Expression;
                  Operation : Tokens.Operation_Token;
                  List      : Tokens.Arguments.Frame
               )  return Tokens.Argument_Token is
   begin
      case Operation.Operation is
         when Colon =>
            declare
               Left  : Tokens.Argument_Token renames List (List'First);
               Right : Tokens.Argument_Token renames List (List'Last);
            begin
               if Left.Value.Nothing then
                  Raise_Exception
                  (  Parsers.Syntax_Error'Identity,
                     (  "JSON object is missing at "
                     &  Image (Left.Location)
                  )  );
               elsif Left.Value.Data.Value.JSON_Type /= JSON_String then
                  Raise_Exception
                  (  Parsers.Syntax_Error'Identity,
                     (  "A string literal is expected left of "
                     &  "the colon at "
                     &  Image (Left.Location)
                  )  );
               end if;
               return
               (  (  Nothing => False,
                     Data    => (  Name  => Left.Value.Data.Value.Text,
                                   Value => Right.Value.Data.Value
                  )             ),
                  Left.Location & Right.Location
               );
            end;
         when others =>
            raise Program_Error; -- No operations in JSON to call
      end case;
   end Call;

   function Enclose
            (  Context : access Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      use Tokens.Arguments;
   begin
      case Left.Operation is
         when Left_Brace =>
            declare
               type Ptr_Type is access JSON_Pair_Array;
               for Ptr_Type'Storage_Pool use Context.Arena.all;
               Ptr : Ptr_Type;
            begin
               if (  List'Length = 1
                  and then
                     List (List'First).Value.Nothing
                  )  then -- Empty object
                  Ptr := new JSON_Pair_Array (1..0);
               else
                  Ptr := new JSON_Pair_Array (1..List'Length);
                  for Index in List'Range loop
                     declare
                        This : Argument_Item renames List (Index).Value;
                     begin
                        if This.Nothing then
                           Raise_Exception
                           (  Parsers.Syntax_Error'Identity,
                              (  "JSON object element is missing at "
                              &  Image (List (Index).Location)
                           )  );
                        elsif This.Data.Name = null then
                           Raise_Exception
                           (  Parsers.Syntax_Error'Identity,
                              (  "Unnamed JSON element is "
                              &  "found at "
                              &  Image (List (Index).Location)
                           )  );
                        end if;
                        Ptr (Integer (Index - List'First) + 1) :=
                           (This.Data.Name, This.Data.Value);
                     end;
                  end loop;
               end if;
               return
               (  (  Nothing => False,
                     Data    => (  Name  => null,
                                   Value => (  JSON_Object,
                                               Ptr.all'Unchecked_Access
                  )             )           ),
                  Left.Location & Right.Location
               );
            end;
         when Left_Bracket =>
            declare
               type Ptr_Type is access JSON_Sequence;
               for Ptr_Type'Storage_Pool use Context.Arena.all;
               Ptr : Ptr_Type;
            begin
               if (  List'Length = 1
                  and then
                     List (List'First).Value.Nothing
                  )  then -- Empty array
                  Ptr := new JSON_Sequence (1..0);
               else
                  Ptr := new JSON_Sequence (1..List'Length);
                  for Index in List'Range loop
                     declare
                        This : Argument_Item renames List (Index).Value;
                     begin
                        if This.Nothing then
                           Raise_Exception
                           (  Parsers.Syntax_Error'Identity,
                              (  "JSON array element is missing at "
                              &  Image (List (Index).Location)
                           )  );
                        elsif This.Data.Name /= null then
                           Raise_Exception
                           (  Parsers.Syntax_Error'Identity,
                              (  "Named JSON array element is found at "
                              &  Image (List (Index).Location)
                           )  );
                        end if;
                        Ptr (Integer (Index - List'First) + 1) :=
                          This.Data.Value;
                     end;
                  end loop;
               end if;
               return
               (  (  Nothing => False,
                     Data    => (  Name  => null,
                                   Value => (  JSON_Array,
                                               Ptr.all'Unchecked_Access
                  )             )           ),
                  Left.Location & Right.Location
               );
            end;
         when others =>
            raise Program_Error;
      end case;
   end Enclose;

   procedure Get_Operand
             (  Context  : in out Expression;
                Code     : in out Lexers.Lexer_Source_Type;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             )  is
      Ptr     : Line_Ptr_Type;
      Last    : Integer;
      Pointer : Integer;
   begin
      Get_Line (Code, Ptr, Pointer, Last);
      Got_It := False;
      if Pointer > Last then
         return;
      end if;
      declare
         Line : String renames Ptr.all;
      begin
         case Line (Pointer) is
            when 'f' =>
               if Strings_Edit.Is_Prefix
                  (  "false",
                     Line (Pointer..Last),
                     Pointer
                  )  then
                  Pointer := Pointer + 5;
                  Set_Pointer (Code, Pointer);
                  Argument :=
                     (  (  Nothing =>
                              False,
                           Data =>
                              (  Name  => null,
                                 Value => (JSON_Boolean, False)
                        )     ),
                        Link (Code)
                     );
                  Got_It := True;
               end if;
            when 't' =>
               if Strings_Edit.Is_Prefix
                  (  "true",
                     Line (Pointer..Last),
                     Pointer
                  )  then
                  Pointer := Pointer + 4;
                  Set_Pointer (Code, Pointer);
                  Argument :=
                     (  (  Nothing =>
                              False,
                           Data =>
                              (  Name  => null,
                                 Value => (JSON_Boolean, True)
                        )     ),
                        Link (Code)
                     );
                  Got_It := True;
               end if;
            when 'n' =>
               if Strings_Edit.Is_Prefix
                  (  "null",
                     Line (Pointer..Last),
                     Pointer
                  )  then
                  Pointer := Pointer + 4;
                  Set_Pointer (Code, Pointer);
                  Argument :=
                     (  (  Nothing =>
                              False,
                           Data =>
                              (  Name  => null,
                                 Value => (JSON_Type => JSON_Null)
                        )     ),
                        Link (Code)
                     );
                  Got_It := True;
               end if;
            when '0'..'9' | '+' | '-' | '.' =>
               declare
                  Value : Long_Float;
               begin
                  Get (Line (Pointer..Last), Pointer, Value);
                  Set_Pointer (Code, Pointer);
                  Argument :=
                     (  (  Nothing =>
                              False,
                           Data =>
                              (  Name  => null,
                                 Value => (JSON_Number, Value)
                        )     ),
                        Link (Code)
                     );
                  Got_It := True;
               exception
                  when Constraint_Error =>
                     Set_Pointer (Code, Pointer);
                     Raise_Exception
                     (  Parsers.Syntax_Error'Identity,
                        "Too large number at " & Image (Link (Code))
                     );
                  when others =>
                     Set_Pointer (Code, Pointer);
                     Raise_Exception
                     (  Parsers.Syntax_Error'Identity,
                        "Wrong number at " & Image (Link (Code))
                     );
               end;
            when '"' =>
               Set_Pointer (Code, Pointer);
               declare
                  Length : Integer := 0;
                  Index  : Integer;
                  This   : UTF8_Code_Point;
               begin
                  Pointer := Pointer + 1;
                  Index   := Pointer;
                  while Index <= Last loop
                     case Line (Index) is
                        when '\' =>
                           if Index >= Last then
                              Set_Pointer (Code, Index);
                              Set_Pointer (Code, Index);
                              Raise_Exception
                              (  Parsers.Syntax_Error'Identity,
                                 (  "Non-terminated escape sequence "
                                 &  "at "
                                 &  Image (Link (Code))
                              )  );
                           end if;
                           Index := Index + 1;
                           case Line (Index) is
                              when '"' | '\' | '/' | 'b' | 'f' |
                                   'n' | 'r' | 't' =>
                                 Index  := Index  + 1;
                                 Length := Length + 1;
                              when 'u' =>
                                 Index := Index + 1;
                                 if Index + 3 > Last then
                                    Set_Pointer (Code, Index - 1);
                                    Set_Pointer (Code, Last  + 1);
                                    Raise_Exception
                                    (  Parsers.Syntax_Error'Identity,
                                       (  "Non-terminated numeric "
                                       &  "escape sequence at "
                                       &  Image (Link (Code))
                                    )  );
                                 end if;
                                 begin
                                    This :=
                                       UTF8_Code_Point
                                       (  Integer'
                                          (  Value
                                             (  Line (Index..Index + 3),
                                                16
                                       )  )  );
                                    Index  := Index  + 4;
                                    declare
                                       Text : constant String :=
                                                       Image (This);
                                    begin
                                       Length := Length + Text'Length;
                                    end;
                                 exception
                                    when others =>
                                       Set_Pointer (Code, Index + 4);
                                       Raise_Exception
                                       (  Parsers.Syntax_Error'Identity,
                                          (  "Invalid numeric "
                                          &  "escape sequence at "
                                          &  Image (Link (Code))
                                       )  );
                                 end;
                              when others =>
                                 Set_Pointer (Code, Index);
                                 Set_Pointer (Code, Index + 1);
                                 Raise_Exception
                                 (  Parsers.Syntax_Error'Identity,
                                    (  "Invalid escape sequence "
                                    &  "at "
                                    &  Image (Link (Code))
                                 )  );
                           end case;
                        when '"' =>
                           Got_It := True;
                           exit;
                        when others =>
                           declare
                              Start : constant Integer := Index;
                           begin
                              Get (Line, Index, This);
                              Length := Length + (Index - Start);
                           exception
                              when others =>
                                 Set_Pointer (Code, Index);
                                 Raise_Exception
                                 (  Parsers.Syntax_Error'Identity,
                                    (  "Invalid UTF-8 sequence at "
                                    &  Image (Link (Code))
                                 )  );
                           end;
                     end case;
                  end loop;
                  if not Got_It then
                     Set_Pointer (Code, Last + 1);
                     Raise_Exception
                     (  Parsers.Syntax_Error'Identity,
                        (  "Non-terminated string at "
                        &  Image (Link (Code))
                     )  );
                  end if;
                  declare
                     type Ptr_Type is access String;
                     for Ptr_Type'Storage_Pool use Context.Arena.all;
                     Ptr  : constant Ptr_Type := new String (1..Length);
                     Text : String renames Ptr.all;
                  begin
                     Index := Text'First;
                     while Pointer <= Last loop
                        case Line (Pointer) is
                           when '\' =>
                              Pointer := Pointer + 1;
                              case Line (Pointer) is
                                 when '"' | '\' | '/' =>
                                    Text (Index) := Line (Pointer);
                                    Index   := Index   + 1;
                                    Pointer := Pointer + 1;
                                 when 'b' =>
                                    Text (Index) := Character'Val (8);
                                    Index   := Index   + 1;
                                    Pointer := Pointer + 1;
                                 when 'f' =>
                                    Text (Index) := Character'Val (12);
                                    Index   := Index   + 1;
                                    Pointer := Pointer + 1;
                                 when 'n' =>
                                    Text (Index) := Character'Val (10);
                                    Index   := Index   + 1;
                                    Pointer := Pointer + 1;
                                 when 'r' =>
                                    Text (Index) := Character'Val (13);
                                    Index   := Index   + 1;
                                    Pointer := Pointer + 1;
                                 when 't' =>
                                    Text (Index) := Character'Val (9);
                                    Index   := Index   + 1;
                                    Pointer := Pointer + 1;
                                 when 'u' =>
                                    Pointer := Pointer + 1;
                                    declare
                                       This : constant String :=
                                              Image
                                              (  UTF8_Code_Point
                                                 (  Integer'
                                                    (  Value
                                                       (  Line
                                                          (  Pointer
                                                          .. Pointer + 3
                                                          ),
                                                          16
                                              )  )  )  );
                                    begin
                                       Pointer := Pointer + 4;
                                       Text
                                       (  Index
                                       .. Index + This'Length - 1
                                       )  := This;
                                       Index := Index + This'Length;
                                    end;
                                 when others =>
                                    raise Program_Error;
                              end case;
                           when '"' =>
                              Pointer := Pointer + 1;
                              exit;
                           when others =>
                              Length := Pointer;
                              Skip (Line, Pointer);
                              Length := Pointer - Length;
                              Text (Index..Index + Length - 1) :=
                                 Line (Pointer - Length..Pointer - 1);
                              Index := Index + Length;
                        end case;
                     end loop;
                     Set_Pointer (Code, Pointer);
                     Argument :=
                        (  (  Nothing =>
                                 False,
                              Data =>
                                  (  Name =>
                                        null,
                                     Value =>
                                        (  JSON_String,
                                           Ptr.all'Unchecked_Access
                           )      )     ),
                           Link (Code)
                        );
                  end;
               end;
            when others =>
               Got_It := False;
         end case;
      end;
   end Get_Operand;

   procedure Get_Blank
             (  Context : in out Expression;
                Code    : in out Lexers.Lexer_Source_Type;
                Got_It  : out Boolean
             )  is
      Buffer  : Line_Ptr_Type;
      Pointer : Integer;
      Last    : Integer;
   begin
      loop
         Get_Line (Code, Buffer, Pointer, Last);
         declare
            Line : String renames Buffer (Buffer'First..Last);
         begin
            Strings_Edit.Get (Line, Pointer, WS);
            Set_Pointer (Code, Pointer);
            if Pointer <= Line'Last then
               Got_It := True;
               return;
            end if;
         end;
         Next_Line (Code);
      end loop;
   exception
      when others =>
         Got_It := False;
   end Get_Blank;

   procedure On_Missing_Operand
             (  Context   : in out Expression;
                Code      : in out Lexers.Lexer_Source_Type;
                Operation : Tokens.Operation_Token;
                Argument  : out Tokens.Argument_Token
             )  is
   begin
      case Operation.Operation is
         when Left_Bracket | Left_Brace =>
            Argument :=
               (  (  Nothing => True,
                     Data    => (null, (JSON_Type => JSON_Null))
                  ),
                  Link (Code)
               );
         when others =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Operand is expected at "
               &  Sources.Image (Sources.Link (Code))
            )  );
      end case;
   end On_Missing_Operand;

   function Parse
            (  Code  : access Source_Type;
               Arena : access Root_Storage_Pool'Class
            )  return JSON_Value is
      Context : Expression (Arena.all'Unchecked_Access);
      Result  : Tokens.Argument_Token;
   begin
      Lexers.Parse (Context, Code.all, Result);
      if Result.Value.Data.Name /= null then
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            (  "Named JSON value is found at "
            &  Image (Result.Location)
         )  );
      end if;
      return Result.Value.Data.Value;
   end Parse;

begin
   Add_Bracket  (Prefixes, "[", Left_Bracket);
   Add_Bracket  (Prefixes, "{", Left_Brace);

   Add_Comma    (Infixes, ",", Comma);
   Add_Ligature (Infixes, ":", Colon);

   Add_Bracket  (Postfixes, "]", Right_Bracket);
   Add_Bracket  (Postfixes, "}", Right_Brace);
end Parsers.JSON.Generic_Parser;
