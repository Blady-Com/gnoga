--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Interprocess_Synchronization           Luebeck            --
--  Test                                           Spring, 2018       --
--                                                                    --
--                                Last revision :  14:26 25 Apr 2024  --
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
--
--  This is a test procedure for interprocess synchronization
--
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Text_IO;              use Ada.Text_IO;
with System.Storage_Elements;  use System.Storage_Elements;
with Test_Interprocess_Data;   use Test_Interprocess_Data;

with Synchronization.Interprocess.Events;
use  Synchronization.Interprocess.Events;


with Synchronization.Interprocess.Mutexes;
use  Synchronization.Interprocess.Mutexes;

with Synchronization.Interprocess.Pulse_Events;
use  Synchronization.Interprocess.Pulse_Events;

with Synchronization.Interprocess.Streams;
use  Synchronization.Interprocess.Streams;

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;
with Synchronization.Interprocess.Memory_Pools;
--with System.Exception_Traces;

procedure Test_Interprocess_Synchronization is
   use Shared_Integer;
   use Shared_Integer_Queue;
   Name   : constant String := "sync_test";
   Master : Boolean;

   package Y_Of is
      new System.Address_To_Access_Conversions (Integer);

   procedure Put_Pool_Statistics
             (  Pool : Synchronization.Interprocess.Memory_Pools.
                       Interprocess_Pool;
                Text : String
             )  is
      use Synchronization.Interprocess.Memory_Pools;
      Free_Blocks : Natural;
      Used_Blocks : Natural;
      Free_Space  : Storage_Count;
      Used_Space  : Storage_Count;
   begin
      Get_Statistics
      (  Pool        => Pool,
         Free_Blocks => Free_Blocks,
         Used_Blocks => Used_Blocks,
         Free_Space  => Free_Space,
         Used_Space  => Used_Space
      );
      Put_Line (Text);
      Put_Line ("   Free blocks:" & Integer'Image (Free_Blocks));
      Put_Line ("   Used blocks:" & Integer'Image (Used_Blocks));
      Put_Line ("   Free space: " & Storage_Count'Image (Free_Space));
      Put_Line ("   Used_Space: " & Storage_Count'Image (Used_Space));
   end Put_Pool_Statistics;
begin
--     System.Exception_Traces.Trace_On
--     (  System.Exception_Traces.Every_Raise
--     );
   if Argument_Count = 1 then
      declare
         Mode : constant String := To_Lower (Argument (1));
      begin
         if Mode = "master" or else Mode = "m" then
            Master := True;
         elsif Mode = "slave" or else Mode = "s" then
            Master := False;
         else
            Put_Line
            (  "Usage: test_interprocess_synchronization "
            &  "{m[aster]|s[lave]}"
            );
            return;
         end if;
      end;
   else
      Put_Line
      (  "Usage: test_interprocess_synchronization "
      &  "{m[aster]|s[lave]}"
      );
      return;
   end if;
   if Master then
      Put_Line ("Testing MASTER interprocess synchronization ...");
      declare
         Data : Shared_Data_Master;
         type String_Ptr is access all String;
         for String_Ptr'Storage_Pool use Data.Pool;
         type Integer_Ptr is access all Integer;
         for Integer_Ptr'Storage_Pool use Data.Pool;
         procedure Free is
            new Ada.Unchecked_Deallocation (String, String_Ptr);
      begin
         Create (Data, Name);
         Put_Line
         (  "Shared size"
         &  Storage_Count'Image (Get_Size (Data))
         );
         if Is_Signaled (Data.Event_1) then
            Raise_Exception
            (  Data_Error'Identity,
               "Unset event is signaled"
            );
         end if;
         declare
            Signaled : Boolean;
         begin
            Wait (Data.Event_1, 0.1, Signaled);
            if Signaled then
               Raise_Exception
               (  Data_Error'Identity,
                  "Unset event is signaled in Wait procedure"
               );
            end if;
         end;
         Put_Line ("Signal manual event");
         Signal (Data.Event_1);
         if not Is_Signaled (Data.Event_1) then
            Raise_Exception
            (  Data_Error'Identity,
               "Set event is not signaled"
            );
         end if;
         declare
            Signaled : Boolean;
         begin
            Wait (Data.Event_1, 0.5, Signaled);
            if not Signaled then
               Raise_Exception
               (  Data_Error'Identity,
                  "Set event is not signaled in Wait procedure"
               );
            end if;
         end;
         if not Is_Empty (Data.Queue) then
            Raise_Exception
            (  Data_Error'Identity,
               "Empty Queue is not empty"
            );
         end if;
         if Is_Full (Data.Queue) then
            Raise_Exception
            (  Data_Error'Identity,
               "Empty Queue is full"
            );
         end if;
         declare
            Empty : Boolean;
         begin
            Wait_For_Not_Empty (Data.Queue, 0.1, Empty);
            if not Empty then
               Raise_Exception
               (  Data_Error'Identity,
                  "Empty Queue is not empty"
               );
            end if;
         end;
         Put_Line ("Wait for response event");
         Wait (Data.Event_2);
         Put_Line
         (  "Master has the mutex: "
         &  Boolean'Image (Is_Mine (Data.Mutex_1))
         );
         Put_Line
         (  "Mutex is owned: "
         &  Boolean'Image (Is_Owned (Data.Mutex_1))
         );
         Put_Line ("Seizing mutex");
         Seize (Data.Mutex_1);
         Put_Line
         (  "Master has the mutex: "
         &  Boolean'Image (Is_Mine (Data.Mutex_1))
         );
         Put_Line
         (  "Mutex is owned: "
         &  Boolean'Image (Is_Owned (Data.Mutex_1))
         );
         Put_Line ("Releasing mutex");
         Release (Data.Mutex_1);
         Put_Line ("Writing shared object concurrently");
         for Try in 1..100 loop
            Set (Data.Int_1, 1);
            Put_Line ("  Read" & Integer'Image (Get (Data.Int_1)));
         end loop;
         Put_Line ("Writing queue");
         for Item in 1..100 loop
            Put (Data.Queue, Item);
         end loop;
         Put_Line ("Writing stream");
         Reset (Data.Not_Full_Event);
         Reset (Data.Not_Empty_Event);
         declare
            File   : File_Type;
            Buffer : String (1..2048);
            Last   : Integer;
         begin
            Open
            (  File,
               In_File,
               "test_interprocess_synchronization.adb"
            );
            loop
               Get_Line (File, Buffer, Last);
               String'Output (Data.Stream'Access, Buffer (1..Last));
            end loop;
         exception
            when End_Error =>
               Close (File);
               Close (Data.Stream);
         end;
         Put_Line ("Testing memory pool");
         Put_Pool_Statistics (Data.Pool, "Pool initialized:");
         declare
            use Synchronization.Interprocess.Memory_Pools;
            P1, P2, P3, P4, P5, P6, P7 : String_Ptr;
            I1 : Integer_Ptr;
         begin
            P1 := new String'("Hello pool");
            Put_Pool_Statistics (Data.Pool, "Pool string allocated:");
            Free (P1);
            Put_Pool_Statistics (Data.Pool, "Pool string freed:");
            P1 := new String'((1..376 => ' '));
            Put_Pool_Statistics (Data.Pool, "All pool allocated:");
            Free (P1);
            Put_Pool_Statistics (Data.Pool, "All pool free:");
            P1 := new String'("abc");
            P2 := new String'("cde");
            P3 := new String'("fgh");
            P4 := new String'("ijk");
            P5 := new String'("lmn");
            P6 := new String'("opq");
            P7 := new String'("rst");
            Put_Pool_Statistics (Data.Pool, "7 strings:");
            Free (P2);
            Put_Pool_Statistics (Data.Pool, "2nd deallocated:");
            Free (P3);
            Put_Pool_Statistics (Data.Pool, "3rd deallocated:");
            Free (P6);
            Put_Pool_Statistics (Data.Pool, "6th deallocated:");
            Free (P5);
            Put_Pool_Statistics (Data.Pool, "5th deallocated:");
            Free (P4);
            Put_Pool_Statistics (Data.Pool, "4th deallocated:");
            Free (P7);
            Put_Pool_Statistics (Data.Pool, "7th deallocated:");
            I1 := new Integer'(12345);
            Shared_Reference.Set
            (  Data.Reference_1,
               To_Reference (Data.Pool, I1.all'Address)
            );
            Signal (Data.Event_1);
         end;
         Put_Line ("Testing blackboard");
         declare
            use Test_Boards;
            Item    : Reference := First (Data.Board);
            Sequent : Boolean;
         begin
     Scan : for Index in 1..200 loop
               while Data.Board < Item loop
                  Put ("*");
                  delay 0.001; -- Not yet there
               end loop;
               declare
                  This : constant String :=
                                  "packet" & Integer'Image (Index);
               begin
                  declare
                     That : constant String := Get (Data.Board, Item);
                  begin
                     if This /= That then
                        New_Line;
                        Put_Line
                        (  That
                        &  " is read "
                        &  This
                        &  " is expected, "
                        &  Image (Item)
                        &  " ["
                        &  Image (First (Data.Board))
                        &  ".."
                        &  Image (Upper (Data.Board))
                        &  "["
                        );
                        exit Scan;
                     end if;
                  end;
               exception
                  when Constraint_Error =>
                     New_Line;
                     Put_Line
                     (  This
                     &  " is lost, "
                     &  Image (Item)
                     &  " ["
                     &  Image (First (Data.Board))
                     &  ".."
                     &  Image (Upper (Data.Board))
                     &  "["
                     );
                     exit Scan;
               end;
               exit when Index = 200;
               declare
                  Old : constant Reference := Item;
               begin
                  loop
                     Next (Data.Board, Item, Sequent);
                     if Sequent then
                        exit when Item /= Old;
                        Put ("*");
                        delay 0.001; -- Not yet there
                     else
                        New_Line;
                        Put_Line
                        (  "Some items are lost, "
                        &  Image (Item)
                        &  " ["
                        &  Image (First (Data.Board))
                        &  ".."
                        &  Image (Upper (Data.Board))
                        &  "["
                        );
                        exit Scan;
                     end if;
                  end loop;
               end;
            end loop Scan;
         end;
      end;
   else
      Put_Line ("Testing SLAVE interprocess synchronization ...");
      declare
         Data : Shared_Data_Slave;
      begin
         Open (Data, Name);
         Put_Line
         (  "Shared size"
         &  Storage_Count'Image (Get_Size (Data))
         );
         Put_Line ("Wait for manual event");
         Wait (Data.Event_1);
         Reset (Data.Event_1);
         Put_Line ("OK, got it, pulsing response");
         Pulse (Data.Event_2);
         Put_Line
         (  "Slave has the mutex: "
         &  Boolean'Image (Is_Mine (Data.Mutex_1))
         );
         Put_Line
         (  "Mutex is owned: "
         &  Boolean'Image (Is_Owned (Data.Mutex_1))
         );
         Put_Line ("Seizing mutex");
         Seize (Data.Mutex_1);
         Put_Line
         (  "Slave has the mutex: "
         &  Boolean'Image (Is_Mine (Data.Mutex_1))
         );
         Put_Line
         (  "Mutex is owned: "
         &  Boolean'Image (Is_Owned (Data.Mutex_1))
         );
         Put_Line ("Releasing mutex");
         Release (Data.Mutex_1);
         for Try in 1..100 loop
            Set (Data.Int_1, 2);
            Put_Line ("  Read" & Integer'Image (Get (Data.Int_1)));
         end loop;
         Put_Line ("Reading queue");
         for Item in 1..100 loop
            declare
               Value : constant Integer := Get (Data.Queue);
            begin
               if Item /= Value then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "  Read" & Integer'Image (Value)
                     &  ", expected" & Integer'Image (Item)
                  )  );
               end if;
            end;
         end loop;
         Put_Line ("Reading stream");
         Reset (Data.Not_Full_Event);
         Reset (Data.Not_Empty_Event);
         declare
            File   : File_Type;
            Buffer : String (1..2048);
            Last   : Integer;
         begin
            Open
            (  File,
               In_File,
               "test_interprocess_synchronization.adb"
            );
            loop
               Get_Line (File, Buffer, Last);
               declare
                  Other : constant String :=
                                   String'Input (Data.Stream'Access);
               begin
                  if Buffer (1..Last) /= Other then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "  Read    :" & Other
                        &  ", expected:" & Buffer (1..Last)
                     )  );
                  end if;
               end;
            end loop;
         exception
            when End_Error =>
               Close (File);
               if not End_Of_Stream (Data.Stream) then
                  -- There is a race condition here
                  delay 1.0;
                  if not End_Of_Stream (Data.Stream) then
                     Raise_Exception
                     (  Data_Error'Identity,
                        "End of stream not reached"
                     );
                  end if;
               end if;
         end;
         Put_Line ("Testing memory pool");
         Put_Pool_Statistics (Data.Pool, "Pool initialized:");
         Wait (Data.Event_1);
         declare
            use Synchronization.Interprocess.Memory_Pools;
            Pointer : Reference :=
                      Shared_Reference.Get (Data.Reference_1);
         begin
            Put_Line
            (  "Integer from other process:"
            &  Integer'Image
               (  Y_Of.To_Pointer
                  (  To_Address (Data.Pool, Pointer)
                  ) .all
            )  );
            Free (Data.Pool, Pointer);
         end;
         Put_Pool_Statistics (Data.Pool, "Shared integer deallocated:");
         Put_Line ("Testing blackboard");
         declare
            use Test_Boards;
            Pointer : Reference;
         begin
            for Index in 1..200 loop
               Put
               (  Data.Board,
                  "packet" & Integer'Image (Index),
                  Pointer
               );
               Put_Line
               (  "Put package"
               &  Integer'Image (Index)
               &  ", at "
               &  Image (Pointer)
               );
               delay 0.02;
            end loop;
         end;
      end;
   end if;
   Put_Line ("... Done");
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Interprocess_Synchronization;
