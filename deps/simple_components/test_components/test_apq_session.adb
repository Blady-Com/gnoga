--                                                                    --
--  package Test_APQ_Session        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  11:37 13 Oct 2007  --
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
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Text_IO;            use Ada.Text_IO;
with Persistent.APQ;         use Persistent.APQ;
with APQ;

package body Test_APQ_Session is

   First_Time  : Boolean := True;
   Data_Base   : Unbounded_String;
   Server_Name : Unbounded_String := To_Unbounded_String ("localhost");
   Server_Type : APQ.Database_Type;
   User_Name   : Unbounded_String;
   Password    : Unbounded_String;
   Trace_File  : Unbounded_String;

   procedure Query is   
      Text : String (1..256);
      procedure Get (Prompt : String; Name : in out Unbounded_String) is
         Last : Integer;
      begin
         Put ("   " & Prompt & " [" & To_String (Name) & "]: ");
         Get_Line (Text, Last);
         if Last >= 1 then
            Name := To_Unbounded_String (Text (1..Last));
         end if;
      end Get;
   begin
      First_Time := False;
      loop
         declare
            Server : Unbounded_String;
         begin
            if Server_Type'Valid then
               declare
                  Default : constant String :=
                               APQ.Database_Type'Image (Server_Type);
               begin
                  Append
                  (  Server,
                     Default (Default'First + 7..Default'Last)
                  );
               end;
            end if;
            Get ("The server (postgreSQL, mySQL)", Server);
            Server_Type :=
               APQ.Database_Type'Value ("Engine_" & To_String (Server));
            exit when Server_Type'Valid;
         exception
            when others =>
               null;
         end;
         Put_Line ("Server type should be either PostgreSQL or MySQL");
      end loop;
      Get ("The server name",            Server_Name);
      Get ("The data base name",         Data_Base);
      Get ("The user name",              User_Name);
      Get ("The password",               Password);
      Get ("Trace file (empty if none)", Trace_File);
   end Query;

   function Open return Storage_Handle is
      Erase : Boolean := First_Time;
   begin
      if First_Time then
         Query;
      end if;
      loop
         declare
            Result : Storage_Handle;
         begin
            Result :=
               Create
               (  Server_Type    => Server_Type,
                  Host_Name      => To_String (Server_Name),
                  Data_Base_Name => To_String (Data_Base),
                  User_Name      => To_String (User_Name),
                  Password       => To_String (Password),
                  Erase          => Erase
               );
               if Length (Trace_File) /= 0 then
               Persistent.APQ.Enable_Tracing
               (  Result,
                  To_String (Trace_File)
               );
            end if;
            return Result;
         exception
            when Error : Use_Error =>
               Put_Line ("Failed to connect to the data base, reason:");
               Put_Line (Exception_Message (Error));
               Put_Line
               (  "Ensure that data base server is configured "
               &  "and re-type"
               );
               Query;
            when Error : others =>
               Put_Line ("Error connecting to the data base:");
               Put_Line (Exception_Message (Error));
               raise;
         end;
      end loop;
   end Open;

end Test_APQ_Session;
