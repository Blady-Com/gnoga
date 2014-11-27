--                                                                    --
--  procedure Test_Echo_Client      Copyright (c)  Dmitry A. Kazakov  --
--  Test echo client                               Luebeck            --
--                                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  13:09 10 Mar 2013  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Text_IO;            use Ada.Text_IO;
with GNAT.Sockets;           use GNAT.Sockets;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Ada.Numerics.Discrete_Random;

procedure Test_Echo_Client is
   Port           : constant := 5876;
   Session_Length : constant := 500;

   subtype String_Length is Integer range 1..80;
   package Random_Length is
      new Ada.Numerics.Discrete_Random (String_Length);
   use Random_Length;
   subtype Printable is Character range '!'..'~';
   package Random_Text is
      new Ada.Numerics.Discrete_Random (Printable);
   use Random_Text;
   Length_Dice    : Random_Length.Generator;
   Character_Dice : Random_Text.Generator;
   Socket         : Socket_Type;
   Address        : Sock_Addr_Type;
begin
   Reset (Length_Dice);
   Reset (Character_Dice);
   Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
   Address.Port := Port;
   Create_Socket (Socket);
   Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
   Connect_Socket (Socket, Address);
   for Packet in 1..Session_Length loop
      declare
         Text     : String (1..Random (Length_Dice));
         Response : String (Text'Range);
      begin
         for Index in Text'Range loop
            Text (Index) := Random (Character_Dice);
         end loop;
         String'Write (Stream (Socket), Text);
         Put_Line ("<" & Text);
         String'Read (Stream (Socket), Response);
         Put_Line (">" & Response);
      end;
   end loop;
   Shutdown_Socket (Socket);
   Close_Socket (Socket);
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Echo_Client;
