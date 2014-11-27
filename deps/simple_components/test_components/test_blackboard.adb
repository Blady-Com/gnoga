--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Blackboard                             Luebeck            --
--  Test                                           Autumn, 2006       --
--                                                                    --
--                                Last revision :  21:31 21 Dec 2011  --
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
with System.Storage_Elements;       use System.Storage_Elements;
with Test_Record_Blackboards;       use Test_Record_Blackboards;
with Test_String_Blackboards;       use Test_String_Blackboards;
with Test_Safe_String_Blackboards;  use Test_Safe_String_Blackboards;

procedure Test_Blackboard is
   Data : Shared_Blackboard (256);

   task type Subscriber (Name : access String) is
      entry Shut_Down;
   end Subscriber;

   task body Subscriber is
      Element : Reference;
      Sequent : Boolean;
   begin
      Element := First (Data); -- The first element
      loop
         if not (Element > Data) then
            begin
               Put_Line (Name.all & " received " & Get (Data, Element));
            exception
               when Constraint_Error =>
                  Put_Line (Name.all & " missed an element");
            end;
            Next (Data, Element, Sequent);
            if not Sequent then
               Put_Line (Name.all & " missed some elements");
            end if;
         else
            select
               accept Shut_Down;
               exit;
            else
               Put_Line (Name.all & " waits for more");
               delay 0.010;  -- Wait for new elements to come
            end select;
         end if;
      end loop;
   end Subscriber;

begin
   declare
      use Boards;
      Board   : Test_String_Blackboards.Blackboard (100);
      Pointer : Test_String_Blackboards.Reference := First (Board);
      procedure Check (Text : String) is
         Item    : Test_String_Blackboards.Reference;
         Sequent : Boolean := True;
      begin
         Put (Board, Text, Item);
         if Text /= Get (Board, Item) then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in string black board Put '"
               &  Text
               &  "' /= '"
               &  Get (Board, Item)
               &  "'"
            )  );
         end if;
         if Item /= Pointer then
            Next (Board, Pointer, Sequent);
            if Item /= Pointer or else not Sequent then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Error in string black board Next '" & Text & '''
               );
            end if;
         end if;
      end Check;
   begin
      Check ("abc");
      Check ("d");
      Check ("efgh");
      Check ("ijklmn");
      Check ("opq");
      Check ("r");
      Check ("stu");
      Check ("vwx");
      Check ("yz");
   end;

   declare
      use Boards;
      Board   : Test_Record_Blackboards.Boards.Blackboard
                (  Test_Record_Blackboards.Element_Size
                +  Test_Record_Blackboards.Head_Size
                );
      Pointer : Boards.Reference;
      Sequent : Boolean;
      Count   : Natural := 0;
   begin
      Put (Board, (others => 0));
      Put (Board, (others => 1));
      Put (Board, (others => 2));
      Pointer := First (Board);
      while Is_Valid (Board, Pointer) loop
         Count := Count + 1;
         Put_Line (Integer'Image (Integer (Get (Board, Pointer) (1))));
         Next (Board, Pointer, Sequent);
         exit when not Sequent;
      end loop;
      if Count /= 1 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in 1x-bytes test Count" & Integer'Image (Count)
         );
      end if;
   exception
      when Error : Storage_Error =>
         Raise_Exception
         (  Storage_Error'Identity,
            (  "Storage error in 1x-bytes test, item size"
            &  Integer'Image (Test_Record_Blackboards.Element'Size)
            &  ": "
            &  Exception_Message (Error)
         )  );
   end;

   declare
      use Boards;
      Board   : Test_Record_Blackboards.Boards.Blackboard
                (  (  Test_Record_Blackboards.Element_Size
                   +  Test_Record_Blackboards.Head_Size
                   )
                *  2
                );
      Pointer : Boards.Reference;
      Sequent : Boolean;
      Count   : Natural := 0;
   begin
      Put (Board, (others => 0));
      Put (Board, (others => 1));
      Put (Board, (others => 2));
      Pointer := First (Board);
      while Is_Valid (Board, Pointer) loop
         Count := Count + 1;
         Put_Line (Integer'Image (Integer (Get (Board, Pointer) (1))));
         Next (Board, Pointer, Sequent);
         exit when not Sequent;
      end loop;
      if Count /= 2 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in 2x-bytes test"
         );
      end if;
   exception
      when Error : Storage_Error =>
         Raise_Exception
         (  Storage_Error'Identity,
            (  "Storage error in 2x-bytes test, item size"
            &  Integer'Image (Test_Record_Blackboards.Element'Size)
            &  ": "
            &  Exception_Message (Error)
         )  );
   end;

   declare
      use Boards;
      Board   : Test_Record_Blackboards.Boards.Blackboard
                (  (  (  Test_Record_Blackboards.Element_Size
                      +  Test_Record_Blackboards.Head_Size
                      )
                   *  3
                   )
                /  2
                );
      Pointer : Boards.Reference;
      Sequent : Boolean;
      Count   : Natural := 0;
   begin
      Put (Board, (others => 0));
      Put (Board, (others => 1));
      Put (Board, (others => 2));
      Pointer := First (Board);
      while Is_Valid (Board, Pointer) loop
         Count := Count + 1;
         Put_Line (Integer'Image (Integer (Get (Board, Pointer) (1))));
         Next (Board, Pointer, Sequent);
         exit when not Sequent;
      end loop;
      if Count /= 1 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in 1.5x-bytes test"
         );
      end if;
   exception
      when Error : Storage_Error =>
         Raise_Exception
         (  Storage_Error'Identity,
            (  "Storage error in 1.5x-bytes test, item size"
            &  Integer'Image (Test_Record_Blackboards.Element'Size)
            &  ": "
            &  Exception_Message (Error)
         )  );
   end;

   declare
      Item    : Reference;
      Sequent : Boolean;
   begin
      for Index in 1..200 loop
         Put (Data, "packet" & Integer'Image (Index), Item);
         Put_Line ("Put " & Get (Data, Item));
      end loop;
      Item := First (Data);
      while not (Item > Data) loop
         Put_Line ("Scan " & Get (Data, Item));
         Next (Data, Item, Sequent);
      end loop;
   end;
   declare
      S1_Name : aliased String := "Subscriber 1";
      S2_Name : aliased String := "   Subscriber 2";
      S3_Name : aliased String := "      Subscriber 3";
      S1 : Subscriber (S1_Name'Access);
      S2 : Subscriber (S2_Name'Access);
      S3 : Subscriber (S3_Name'Access);
   begin
      -- Start flooding the blackboard
      for Index in 200..2_000 loop
         Put (Data, "packet" & Integer'Image (Index));
         delay 0.010;
      end loop;
      S1.Shut_Down;
      S2.Shut_Down;
      S3.Shut_Down;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Blackboard;
