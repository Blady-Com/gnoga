--                                                                    --
--  package Test_Echo_Clients       Copyright (c)  Dmitry A. Kazakov  --
--  Test echo client                               Luebeck            --
--  Implementation                                 Winter, 2015       --
--                                                                    --
--                                Last revision :  12:25 15 May 2015  --
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

with Ada.Text_IO;            use Ada.Text_IO;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Ada.Numerics.Discrete_Random;

package body Test_Echo_Clients is
   subtype String_Length is Integer range 1..80;
   package Random_Length is
      new Ada.Numerics.Discrete_Random (String_Length);
   use Random_Length;
   subtype Printable is Character range '0'..'9';
   package Random_Text is
      new Ada.Numerics.Discrete_Random (Printable);
   use Random_Text;

   Length_Dice    : Random_Length.Generator;
   Character_Dice : Random_Text.Generator;

   function Random return String is
      Text : String (1..Random (Length_Dice));
   begin
      for Index in Text'Range loop
         Text (Index) := Random (Character_Dice);
      end loop;
      return Text;
   end Random;

   procedure Connected (Client : in out Echo_Connection) is
   begin
      Reset (Length_Dice);
      Reset (Character_Dice);
      Sent (Client);
   end Connected;

   procedure Received
             (  Client  : in out Echo_Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Pointer := Data'Last + 1;
      Put_Line (">" & To_String (Data));
      Client.Data_Count := Client.Data_Count - Data'Length;
      if Client.Data_Count <= 0 then
         Put_Line ("Done");
         Shutdown (Client);
      end if;
   end Received;

   procedure Sent (Client : in out Echo_Connection) is
      Text    : String := Random;
      Pointer : Integer := Text'First;
   begin
      if Client.Messages_Count > 0 then
         Client.Messages_Count := Client.Messages_Count - 1;
         Send (Client, Text, Pointer);
         if Pointer /= Text'Last + 1 then
            Put_Line
            (  "Error sending "
            &  Image (Text'Last - Pointer + 1)
            &  " elements, "
            &  Image (Pointer - Text'First)
            &  " sent"
            );
         end if;
         Put_Line ("<" & Text);
         Client.Data_Count := Client.Data_Count + Text'Length;
      end if;
   end Sent;

   procedure Trace
             (  Factory    : in out Echo_Factory;
                Context    : String;
                Occurrence : Exception_Occurrence
             )  is
   begin
      Put_Line (Context & ':' & Exception_Information (Occurrence));
   end Trace;

end Test_Echo_Clients;
