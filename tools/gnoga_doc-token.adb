------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                       G N O G A _ D O C . T O K E N                      --
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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Strings.Maps.Constants;
with Strings_Edit;

package body Gnoga_Doc.Token is

   function Is_EOT_Character (C : Character) return Boolean is
      use Ada.Characters;
   begin
      if C = Latin_1.Space or
        C = Latin_1.HT or
        C = '-' or
        C = ':' or
        C = ';' or
        C = ',' or
        C = Latin_1.CR or
        C = Latin_1.LF
      then
         return True;
      else
         return False;
      end if;
   end Is_EOT_Character;

   function Is_EOL (S : in String; P : Integer) return Boolean is
      use Ada.Characters;

      C : constant Character := S (P);
   begin
      if C = Latin_1.CR or C = Latin_1.LF then
         return True;
      else
         return False;
      end if;
   end Is_EOL;

   function Is_Prefix (Prefix, S : in String; P : Integer) return Boolean is
      use Ada.Strings.Maps.Constants;
   begin
      return Strings_Edit.Is_Prefix (Prefix  => Prefix,
                                     Source  => S,
                                     Pointer => P,
                                     Map     => Lower_Case_Map);
   end Is_Prefix;

   procedure Get_To_Character (C : in     Character;
                               S : in     String;
                               P : in out Integer)
   is
   begin
      while P <= S'Last loop
         if S (P) = C then
            exit;
         else
            P := P + 1;
         end if;
      end loop;
   end Get_To_Character;

   procedure Get_To_Semicolon (S : in String; P : in out Integer) is
      Par_Count : Natural := 0;
   begin
      while P <= S'Last loop
         if S (P) = '(' then
            Par_Count := Par_Count + 1;
         elsif S (P) = ')' then
            Par_Count := Par_Count - 1;
         end if;

         P := P + 1;

         if Par_Count = 0 then
            exit when S (P) = ';';
         end if;
      end loop;
   end Get_To_Semicolon;

   procedure Get_To_EOS (S : in String; P : in out Integer) is
      use Ada.Characters;
      use Ada.Strings.Maps;
      use Ada.Strings.Maps.Constants;

      TabAndSpace : constant Ada.Strings.Maps.Character_Set :=
        To_Set (Sequence => Latin_1.Space &
                  Latin_1.HT &
                  Latin_1.CR &
                  Latin_1.LF);
   begin
      Strings_Edit.Get (S, P, TabAndSpace);
   end Get_To_EOS;

   procedure Get_To_EOL (S : String; P : in out Integer) is
      use Ada.Characters;
   begin
      while P <= S'Last and S (P) /= Latin_1.LF loop
         P := P + 1;
      end loop;
   end Get_To_EOL;

   procedure Get_To_EOT (S : String; P : in out Integer) is
      use Ada.Characters;
      --  (' ', HT, ':', ';', ',', CR, LF)
   begin
      while P <= S'Last and not Is_EOT_Character (S (P)) loop
         P := P + 1;
      end loop;
   end Get_To_EOT;

   procedure Get_To_EOR (S : in String; P : in out Integer) is
   begin
      while P <= S'Last loop
         if Is_Token ("end", S, P) then
            exit;
         end if;

         P := P + 1;
      end loop;
   end Get_To_EOR;

   procedure Get_To_Next_Token (S : in String; P : in out Integer) is
   begin
      Get_To_EOT (S, P);
      Get_To_EOS (S, P);
   end Get_To_Next_Token;

   function Token_Name (S : String; P : Integer) return String is
      T : Integer := P;
   begin
      Get_To_EOT (S, T);
      return S (P .. T - 1);
   end Token_Name;

   function Is_Token (Token, S : String; P : Integer) return Boolean is
      use Ada.Characters;
      use Ada.Strings.Maps.Constants;

      L : Integer;
   begin
      if not Strings_Edit.Is_Prefix (Prefix  => Token,
                                     Source  => S,
                                     Pointer => P,
                                     Map     => Lower_Case_Map)
      then
         return False;
      end if;

      L := P + Token'Length;

      if not Is_EOT_Character (S (L)) then
         return False;
      else
         if S'First = P then
            return True;
         else
            L := P - 1;

            return Is_EOT_Character (S (L));
         end if;
      end if;
   end Is_Token;

end Gnoga_Doc.Token;
