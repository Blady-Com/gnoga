--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Call_Service_Example                   Luebeck            --
--  Helper test package                            Spring, 2018       --
--                                                                    --
--                                Last revision :  19:18 30 Apr 2018  --
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

with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Text_IO;                   use Ada.Text_IO;
with Strings_Edit.Integers;         use Strings_Edit.Integers;
with Synchronization.Interprocess;  use Synchronization.Interprocess;
with Test_Call_Services;            use Test_Call_Services;

with Synchronization.Interprocess.Process_Call_Service.Manager;
with GNAT.Exception_Traces;

procedure Test_Call_Service_Instance is
   use Algebraic_Integer;
   use Baton_Message;
   use Greeting_Message;
   use Synchronization.Interprocess.Process_Call_Service;
   use Synchronization.Interprocess.Process_Call_Service.Manager;
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   Set (Data.Add,      Add'Access);  -- Set handler
   Set (Data.Divide,   Divide'Access);
   Set (Data.Greeting, Greet'Access);
   Set (Data.Baton,    Baton'Access);
   Open (Data, "test_call_service", True);
   Wait_For_Initialization (Data.Services);
   Close (Data); -- No more participants
   for Index in 1..Data.Services.Size loop
      if Index /= Get_Server_ID (Data.Services) then
         Put_Line
         (  "Response from"
         &  Call_Service_ID'Image (Index)
         &  ": "
         &  Call
            (  Method   => Data.Greeting,
               Callee   => Index,
               Argument => "Greeting!"
         )  );
      end if;
   end loop;
   case Get_Server_ID (Data.Services) is
      when 1 =>
         Put_Line
         (  "Divide 10 by 2 at 2 ="
         &  Integer'Image (Call (Data.Divide, 2, 10, 2))
         );
         begin
            Put_Line
            (  "Divide 10 by 0 at 2 ="
            &  Integer'Image (Call (Data.Divide, 2, 10, 0))
            );
         exception
            when Error : others =>
               Put_Line
               (  "Divide 10 by 0 at 2 = "
               &  Exception_Message (Error)
               );
         end;
      when others =>
         null;
   end case;
   if Get_Server_ID (Data.Services) = 1 then -- Initiate passing the baton
      Call
      (  Method     => Data.Baton,
         Argument_1 => 1,
         Argument_2 => "",
         Callee     => Get_Service (Data.Baton, 2).all
      );
   end if;
   Put_Line ("Wait for 1s");
   delay 1.0;
   declare
      function "+" (Increment : Call_Service_ID)
         return Call_Service_ID is
         ID : Call_Service_ID :=
              Get_Server_ID (Data.Services) + Increment;
      begin
         if ID > 3 then
            ID := ID - 3;
         end if;
         return ID;
      end "+";
      task A; task B; task C;
      task body A is
         ID : constant Call_Service_ID := +1;
         K  : Integer;
      begin
         for I in 1..10 loop
            for J in 1..10 loop
               Put_Line
               (  "A "
               &  Image (I)
               &  "+"
               &  Image (J)
               &  " on"
               &  Call_Service_ID'Image (ID)
               );
               K := Call (Data.Add, ID, I, J);
               Put_Line ("A = " & Image (K));
               if K /= I + J then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Error get"
                     &  Integer'Image (K)
                     &  ", expected"
                     &  Integer'Image (I + J)
                  )  );
               end if;
            end loop;
         end loop;
         Put_Line ("Quit A");
      exception
         when Error : others =>
            Put_Line ("Fault in A: " & Exception_Information (Error));
      end A;
      task body B is
         ID : constant Call_Service_ID := +1;
         K  : Integer;
      begin
         for I in 1..10 loop
            for J in 1..10 loop
               Put_Line
               (  "B "
               &  Image (I)
               &  "+"
               &  Image (J)
               &  " on"
               &  Call_Service_ID'Image (ID)
               );
               K := Call (Data.Add, ID, I, J);
               Put_Line ("B = " & Image (K));
               if K /= I + J then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Error get"
                     &  Integer'Image (K)
                     &  ", expected"
                     &  Integer'Image (I + J)
                  )  );
               end if;
            end loop;
         end loop;
         Put_Line ("Quit B");
      exception
         when Error : others =>
            Put_Line ("Fault in B: " & Exception_Information (Error));
      end B;
      task body C is
         ID : constant Call_Service_ID := +2;
         K  : Integer;
      begin
         for I in 1..10 loop
            for J in 1..10 loop
               Put_Line
               (  "C "
               &  Image (I)
               &  "+"
               &  Image (J)
               &  " on"
               &  Call_Service_ID'Image (ID)
               );
               K := Call (Data.Add, ID, I, J);
               Put_Line ("C = " & Image (K));
               if K /= I + J then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Error get"
                     &  Integer'Image (K)
                     &  ", expected"
                     &  Integer'Image (I + J)
                  )  );
               end if;
            end loop;
         end loop;
         Put_Line ("Quit C");
      exception
         when Error : others =>
            Put_Line ("Fault in C: " & Exception_Information (Error));
      end C;
   begin
      null;
   exception
      when Error : others =>
         Put_Line ("Call fault: " & Exception_Information (Error));
   end;
   Put_Line ("Wait for 5s");
   delay 5.0;
exception
   when Error : others =>
      Put_Line ("Fault: " & Exception_Information (Error));
end Test_Call_Service_Instance;
