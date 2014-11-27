--                                                                    --
--  package Test_Echo_Servers       Copyright (c)  Dmitry A. Kazakov  --
--  Test echo server                               Luebeck            --
--  Implementation                                 Winter, 2012       --
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

with Ada.Text_IO;  use Ada.Text_IO;

package body Test_Echo_Servers is

   function Create
            (  Factory  : access Echo_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr is
      Result : Connection_Ptr;
   begin
      Put_Line ("Connected client at " & Image (From));
      Result := new Echo_Connection (80, 120);
      Echo_Connection (Result.all).From := From;
      return Result;
   end Create;

   procedure Finalize (Client : in out Echo_Connection) is
   begin
      Put_Line ("Disconnected client " & Image (Client.From));
      Finalize (Connection (Client));
   end Finalize;

   procedure Received
             (  Client  : in out Echo_Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Pointer := Data'First;
      Send (Client, Data, Pointer);
      if Pointer /= Data'Last + 1 then
         Put_Line
         (  "Error sending"
         &  Stream_Element_Offset'Image (Data'Last - Pointer + 1)
         &  " elements,"
         &  Stream_Element_Offset'Image (Pointer - Data'First)
         &  " sent"
         );
      end if;
   end Received;

   procedure Trace
             (  Factory    : in out Echo_Factory;
                Context    : String;
                Occurrence : Exception_Occurrence
             )  is
   begin
      Put_Line (Context & ':' & Exception_Information (Occurrence));
   end Trace;

end Test_Echo_Servers;
