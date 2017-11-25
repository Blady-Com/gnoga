------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                            G N O G A _ D O C                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;

with Gnoga;
with Gnoga.Server.Template_Parser.Simple;
with Gnoga.Types;

with Gnoga_Doc.Token; use Gnoga_Doc.Token;

package body Gnoga_Doc is

   Comments : Ada.Strings.Unbounded.Unbounded_String;

   procedure Parse_Elements (S : String);

   procedure Parse_Elements (S : String) is
      P : Integer := S'First;

      Names : Gnoga.Types.Data_Array_Type;
   begin
      while P <= S'Last loop

         while S (P) /= ':' loop
            Get_To_EOS (S, P);
            Names.Prepend (Token_Name (S, P));
            Get_To_EOT (S, P);
            Get_To_EOS (S, P);

            if S (P) = ',' then
               P := P + 1;
            end if;
         end loop;

         P := P + 1;
         Get_To_EOS (S, P);

         loop
            declare
               Type_Info : constant String := Token_Name (S, P);
            begin
               if Type_Info = "in" or
                 Type_Info = "out" or
                 Type_Info = "access"
               then
                  Put (Type_Info & " ");
                  Get_To_Next_Token (S, P);
               else
                  for i in Names.First_Index .. Names.Last_Index loop
                     Put_Line (Names.Element (i) & " : " & Type_Info);
                  end loop;

                  Names.Clear;
                  exit;
               end if;
            end;
         end loop;

         Get_To_Semicolon (S, P);
         P := P + 1;
         Get_To_EOS (S, P);
      end loop;
   end Parse_Elements;

   procedure Parse (File_Name : String) is
      use Ada.Characters;
      use Ada.Strings.Unbounded;

      Location : File_Loc := Pre_Package;
   begin
      Gnoga.Server.Template_Parser.Set_Template_Directory ("");
      Put_Line ("Parsing file : " & File_Name);

      declare
         S : constant String := Gnoga.Server.Template_Parser.Simple.Load_View
           (File_Name);
         P : Integer := S'First;
         T : Integer;
         L : Integer;
      begin
         while P <= S'Last loop
            if Is_Prefix ("--", S, P) then
               P := P + String'("--")'Length;

               if Is_EOL (S, P) then
                  if Location /= Pre_Package then
                     Comments := Comments & Latin_1.LF;
                  end if;

                  P := P + 1;
               else
                  Get_To_EOS (S, P);
                  L := P;
                  Get_To_EOL (S, P);

                  if Location /= Pre_Package then
                     Comments := Comments & S (L .. P);
                  end if;

                  P := P + 1;
               end if;

            elsif Is_Token ("procedure", S, P) then
               L := P;
               Get_To_Next_Token (S, P);

               Put_Line ("Procedure Name : " & Token_Name (S, P));

               Get_To_EOT (S, P);
               Get_To_EOS (S, P);
               T := P + 1;
               Get_To_Character (')', S, P);
               Parse_Elements (S (T .. P - 1) & ";");

               P := L;
               Get_To_Semicolon (S, P);
               Put_Line ("Procedure : " & S (L .. P));

               Get_To_EOL (S, P);
               P := P + 1;

               Location := Post_Block;
            elsif Is_Token ("function", S, P) then
               L := P;
               Get_To_Next_Token (S, P);

               Put_Line ("Function Name : " & Token_Name (S, P));

               Get_To_EOT (S, P);
               Get_To_EOS (S, P);
               T := P + 1;
               Get_To_Character (')', S, P);
               Parse_Elements (S (T .. P - 1) & ";");

               P := L;
               Get_To_Semicolon (S, P);
               Put_Line ("Function : " & S (L .. P));

               Get_To_EOL (S, P);
               P := P + 1;

               Location := Post_Block;
            elsif Is_Token ("type", S, P) then
               L := P;
               Get_To_Next_Token (S, P);

               Put_Line ("Type Name : " & Token_Name (S, P));

               --  Check if a record type
               Get_To_Next_Token (S, P);
               if Is_Token ("is", S, P) then
                  Get_To_Next_Token (S, P);

                  if Is_Token ("record", S, P) then
                     Get_To_Next_Token (S, P);

                     T := P;
                     Get_To_EOR (S, P);
                     Parse_Elements (S (T .. P - 1));

                     Get_To_Semicolon (S, P);
                     P := P + 1;

                     Put_Line ("Type : " & S (L .. P));
                  else
                     P := L;
                     Get_To_Semicolon (S, P);
                     Put_Line ("Type : " & S (L .. P));
                  end if;
               else
                  Put_Line ("Type : Forward decleration of type.");
               end if;

               Get_To_EOL (S, P);
               P := P + 1;

               Location := Post_Block;
            elsif Location = Pre_Package and then Is_Token ("package", S, P)
            then
               P := P + String'("package")'Length + 1;
               Get_To_EOS (S, P);

               L := P;
               Get_To_EOT (S, P);
               Put_Line ("Package Name : " & S (L .. P - 1));
               New_Line;

               Location := In_Package;
            elsif Is_Token ("private", S, P) then
               return;
            elsif S (P) = Latin_1.CR or S (P) = Latin_1.LF then
               if Location = Post_Block then
                  if Comments /= Null_Unbounded_String then
                     Put_Line ("Comments : " & To_String (Comments));
                  end if;

                  Location := In_Package;
               end if;

               Comments := Null_Unbounded_String;
               P := P + 1;
            else
               P := P + 1;
            end if;
         end loop;
      end;
   end Parse;

end Gnoga_Doc;
