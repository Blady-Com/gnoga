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
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;

with Gnoga;
with Gnoga.Server.Template_Parser.Simple;

with Strings_Edit;

with Gnoga_Doc.Token; use Gnoga_Doc.Token;

package body Gnoga_Doc is

   Comments : Ada.Strings.Unbounded.Unbounded_String;

   procedure Parse (File_Name : String) is
      use Ada.Characters;
      use Ada.Strings.Maps;
      use Ada.Strings.Maps.Constants;
      use Ada.Strings.Unbounded;

      TabAndSpace : Ada.Strings.Maps.Character_Set :=
        To_Set (Sequence => Latin_1.Space &
                  Latin_1.HT &
                  Latin_1.CR &
                  Latin_1.LF);

      Location : File_Loc := Pre_Package;
   begin
      Gnoga.Server.Template_Parser.Set_Template_Directory ("");
      Put_Line ("Parsing file : " & File_Name);

      declare
         S : String := Gnoga.Server.Template_Parser.Simple.Load_View
           (File_Name);
         P : Integer := S'First;
         L : Integer;
         T : Integer;
      begin
         while P <= S'Last loop
            if Strings_Edit.Is_Prefix (Prefix  => "--",
                                       Source  => S,
                                       Pointer => P,
                                       Map     => Lower_Case_Map)
            then
               P := P + String'("--")'Length + 1;
               Strings_Edit.Get (S, P, TabAndSpace);

               L := P;
               Get_To_EOL (S, P);

               if Location /= Pre_Package then
                  Comments := Comments & S (L .. P);
               end if;

               P := P + 1;
            elsif Is_Token ("procedure", S, P) then
               L := P;

               P := P + String'("procedure")'Length + 1;
               T := P;
               Strings_Edit.Get (S, P, TabAndSpace);
               Get_To_EOT (S, P);
               Put_Line ("Procedure Name : " & S (T .. P - 1));

               P := L;
               Get_To_Semicolon (S, P);
               Put_Line ("Procedure : " & S (L .. P));

               P := P + 1;
               Strings_Edit.Get (S, P, TabAndSpace);

               Location := Post_Block;
            elsif Is_Token ("function", S, P) then
               L := P;

               P := P + String'("function")'Length + 1;
               T := P;
               Strings_Edit.Get (S, P, TabAndSpace);
               Get_To_EOT (S, P);
               Put_Line ("Function Name : " & S (T .. P - 1));

               P := L;
               Get_To_Semicolon (S, P);
               Put_Line ("Function : " & S (L .. P));

               P := P + 1;
               Strings_Edit.Get (S, P, TabAndSpace);

               Location := Post_Block;
            elsif Is_Token ("type", S, P) then
               L := P;

               P := P + String'("type")'Length + 1;
               T := P;
               Strings_Edit.Get (S, P, TabAndSpace);
               Get_To_EOT (S, P);
               Put_Line ("Type Name : " & S (T .. P - 1));

               P := L;
               Get_To_Semicolon (S, P);
               Put_Line ("Type : " & S (L .. P));

               P := P + 1;
               Strings_Edit.Get (S, P, TabAndSpace);

               Location := Post_Block;
            elsif Location = Pre_Package and then Is_Token ("package", S, P)
            then
               P := P + String'("package")'Length + 1;
               Strings_Edit.Get (S, P, TabAndSpace);
               L := P;
               Get_To_EOT (S, P);
               Put_Line ("Package Name : " & S (L .. P - 1));

               Location := In_Package;
            elsif Is_Token ("private", S, P) then
               return;
            elsif S (P) = Latin_1.CR or S (P) = Latin_1.LF then
               if Location = Post_Block then
                  if Comments /= Null_Unbounded_String then
                     Put_Line ("Comments :" & To_String (Comments));
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
