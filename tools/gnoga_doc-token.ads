------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                       G N O G A _ D O C . T O K E N                      --
--                                                                          --
--                                 S p e c                                  --
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

package Gnoga_Doc.Token is
   type File_Loc is (Pre_Package, In_Package, Post_Block);

   function Is_EOT_Character (C : Character) return Boolean;

   function Is_Prefix (Prefix, S : in String; P : Integer) return Boolean;
   --  Case insensitive check if S (P .. Prefix'Length) = Prefix

   function Is_EOL (S : in String; P : Integer) return Boolean;
   --  Return true if S (P) = CR or LF

   procedure Get_To_Semicolon (S : in String; P : in out Integer);
   --  Move P to semicolon, ignore semicolon with in parenthesis
   --  ? ignore ',"

   procedure Get_To_Character (C : in     Character;
                               S : in     String;
                               P : in out Integer);

   procedure Get_To_EOS (S : in String; P : in out Integer);
   --  Move P to end of Space, Tab, CR and LF

   procedure Get_To_EOL (S : in String; P : in out Integer);
   --  Move P to end of line (LF)

   procedure Get_To_EOT (S : in String; P : in out Integer);
   --  Move P to end of token

   procedure Get_To_EOR (S : in String; P : in out Integer);
   --  Move P to end of record, P at "end record;"

   procedure Get_To_Next_Token (S : in String; P : in out Integer);
   --  Move P to next token

   function Token_Name (S : String; P : Integer) return String;
   --  Return S (P .. EOT)

   function Is_Token (Token, S : String; P : Integer) return Boolean;
   --  check if S at P is Token

end Gnoga_Doc.Token;
