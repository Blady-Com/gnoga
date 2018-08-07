--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Streams                                     Spring, 2018       --
--  Implementation                                                    --
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

with Ada.Calendar;       use Ada.Calendar;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Ada.Tags;           use Ada.Tags;

with Ada.Unchecked_Conversion;

package body Synchronization.Interprocess.Streams is

   In_End_Init  : constant String := "The in-end of the stream is " &
                                     "not initialized " &
                                     "or already finalized";
   Out_End_Init : constant String := "The out-end of the stream is " &
                                     "not initialized " &
                                     "or already finalized";
   Read_Output  : constant String := "Reading from an output stream";
   Skip_Output  : constant String := "Skipping in an output stream";
   Stream_Init  : constant String := "The stream is not initialized " &
                                     "or already finalized";
   Closed       : constant String := "The stream is closed";
   Write_Input  : constant String := "Writing into an input stream";

   procedure Close (Stream : in out Interprocess_Stream) is
   begin
      if Stream.Data = null then
         Raise_Exception (Status_Error'Identity, Stream_Init);
      end if;
      Stream.Data.Closed := True;
      Signal (Stream.Not_Full.all);
      Signal (Stream.Not_Empty.all);
   end Close;

   function End_Of_Stream (Stream : Interprocess_Stream)
      return Boolean is
   begin
      if Stream.Data = null then
         Raise_Exception (Status_Error'Identity, Stream_Init);
      end if;
      declare
         Data : Stream_Data renames Stream.Data.all;
      begin
         return Data.First = Data.Free and then Data.Closed;
      end;
   end End_Of_Stream;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Object : Input_Stream
             )  is
   begin
       Enumerate (Stream, Object.Object);
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Object : Output_Stream
             )  is
   begin
       Enumerate (Stream, Object.Object);
   end Enumerate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Object : Universal_Stream
             )  is
   begin
       Enumerate (Stream, Object.Object);
   end Enumerate;

   procedure Finalize (Stream : in out Interprocess_Stream) is
   begin
      if Stream.Data /= null then
         Close (Stream);
      end if;
   end Finalize;

   procedure Finalize (Stream : in out Universal_Stream) is
   begin
      case Stream.Mode is
         when In_End | Out_End =>
            if Stream.Data /= null then
               Close (Stream);
            end if;
         when Multiplexed_In_End | Multiplexed_Out_End =>
            null;
      end case;
   end Finalize;

   function Get_Mode (Stream : Universal_Stream) return Stream_End is
   begin
      return Stream.Mode;
   end Get_Mode;

   function Get_Offset
            (  Stream : Input_Stream
            )  return Storage_Offset is
   begin
      return Stream.Object.Offset;
   end Get_Offset;

   function Get_Offset
            (  Stream : Output_Stream
            )  return Storage_Offset is
   begin
      return Stream.Object.Offset;
   end Get_Offset;

   function Get_Offset
            (  Stream : Universal_Stream
            )  return Storage_Offset is
   begin
      return Stream.Object.Offset;
   end Get_Offset;

   function Get_Signature (Object : Shared_Object) return Unsigned_16 is
   begin
      return Get_Signature (External_Tag (Interprocess_Stream'Tag));
   end Get_Signature;

   function Get_Size (Object : Shared_Input_Object)
      return Storage_Count is
      subtype Data is Stream_Data (Positive (Object.Stream.Size));
   begin
      return Round (Data'Max_Size_In_Storage_Elements);
   end Get_Size;

   function Get_Size (Object : Shared_Output_Object)
      return Storage_Count is
      subtype Data is Stream_Data (Positive (Object.Stream.Size));
   begin
      return Round (Data'Max_Size_In_Storage_Elements);
   end Get_Size;

   function Get_Size (Object : Shared_Universal_Object)
      return Storage_Count is
      subtype Data is Stream_Data (Positive (Object.Stream.Size));
   begin
      return Round (Data'Max_Size_In_Storage_Elements);
   end Get_Size;

   function Get_Timeout (Stream : Interprocess_Stream)
      return Duration is
   begin
      return Stream.Timeout;
   end Get_Timeout;

   function Is_Closed (Stream : Interprocess_Stream) return Boolean is
   begin
      if Stream.Data = null then
         Raise_Exception (Status_Error'Identity, Stream_Init);
      else
         return Stream.Data.Closed;
      end if;
   end Is_Closed;

   function Is_Empty (Stream : Interprocess_Stream)
      return Boolean is
   begin
      if Stream.Data = null then
         Raise_Exception (Status_Error'Identity, Stream_Init);
      end if;
      declare
         Data : Stream_Data renames Stream.Data.all;
      begin
         return Data.First = Data.Free;
      end;
   end Is_Empty;

   function Is_Full (Stream : Interprocess_Stream)
      return Boolean is
   begin
      if Stream.Data = null then
         Raise_Exception (Status_Error'Identity, Stream_Init);
      end if;
      declare
         Data  : Stream_Data renames Stream.Data.all;
         Count : constant Integer := Data.First - Data.Free;
      begin
         return Count = 1 or else Count = 1 - Data.Size;
      end;
   end Is_Full;

   procedure Map
             (  Object   : in out Shared_Input_Object;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
      Stream : Input_Stream'Class renames
               Object.Stream.all;
      subtype Data is Stream_Data (Positive (Stream.Size));
      package Mapper is new Generic_Memory_Mapper (Data);
      This : Abstract_Shared_Object_Ptr := Shared.First;
   begin
      while (  This /= null
            and then
               This /= Stream.Object'Unchecked_Access
            )  loop
         if This.all in Event'Class then
            Stream.Not_Full  := Stream.Not_Empty;
            Stream.Not_Empty := Event'Class (This.all)'Unchecked_Access;
         end if;
         This := This.Next;
      end loop;
      if Stream.Not_Full = null then
         Raise_Exception
         (  Data_Error'Identity,
            (  "The environment contains no not-full event "
            &  "record member appearing before the shared object"
         )  );
      end if;
      Stream.Data := Mapper.Map (Location, Owner).all'Unchecked_Access;
      if not Compare_And_Swap
             (  Target  => Object.Stream.Data.Has_In'Access,
                Old_Val => 0,
                New_Val => 1
             )
      then
         Raise_Exception
         (  Mode_Error'Identity,
            "The queue already has the in-end"
         );
      end if;
   end Map;

   procedure Map
             (  Object   : in out Shared_Output_Object;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
      Stream : Output_Stream'Class renames
               Object.Stream.all;
      subtype Data is Stream_Data (Positive (Stream.Size));
      package Mapper is new Generic_Memory_Mapper (Data);
      This : Abstract_Shared_Object_Ptr := Shared.First;
   begin
      while (  This /= null
            and then
               This /= Stream.Object'Unchecked_Access
            )  loop
         if This.all in Event'Class then
            Stream.Not_Full  := Stream.Not_Empty;
            Stream.Not_Empty := Event'Class (This.all)'Unchecked_Access;
         end if;
         This := This.Next;
      end loop;
      if Stream.Not_Full = null then
         Raise_Exception
         (  Data_Error'Identity,
            (  "The environment contains no not-full event "
            &  "record member appearing before the shared object"
         )  );
      end if;
      Stream.Data := Mapper.Map (Location, Owner).all'Unchecked_Access;
      if not Compare_And_Swap
             (  Target  => Object.Stream.Data.Has_Out'Access,
                Old_Val => 0,
                New_Val => 1
             )
      then
         Raise_Exception
         (  Mode_Error'Identity,
            "The queue already has the out-end"
         );
      end if;
   end Map;

   procedure Map
             (  Object   : in out Shared_Universal_Object;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
      Stream : Universal_Stream'Class renames
               Object.Stream.all;
      subtype Data is Stream_Data (Positive (Stream.Size));
      package Mapper is new Generic_Memory_Mapper (Data);
      This : Abstract_Shared_Object_Ptr := Shared.First;
   begin
      while (  This /= null
            and then
               This /= Stream.Object'Unchecked_Access
            )  loop
         if This.all in Event'Class then
            Stream.Not_Full  := Stream.Not_Empty;
            Stream.Not_Empty := Event'Class (This.all)'Unchecked_Access;
         end if;
         This := This.Next;
      end loop;
      if Stream.Not_Full = null then
         Raise_Exception
         (  Data_Error'Identity,
            (  "The environment contains no not-full event "
            &  "record member appearing before the shared object"
         )  );
      end if;
      Stream.Data := Mapper.Map (Location, Owner).all'Unchecked_Access;
      case Stream.Mode is
         when In_End =>
            if not Compare_And_Swap
                   (  Target  => Object.Stream.Data.Has_In'Access,
                      Old_Val => 0,
                      New_Val => 1
                   )
            then
               Raise_Exception
               (  Mode_Error'Identity,
                  "The queue already has an in-end"
               );
            end if;
         when Multiplexed_In_End =>
            if (  not Compare_And_Swap
                      (  Target  => Object.Stream.Data.Has_In'Access,
                         Old_Val => 0,
                         New_Val => 2
                      )
               and then
                  Object.Stream.Data.Has_In = 1
               )
            then
               Raise_Exception
               (  Mode_Error'Identity,
                  "The queue already has an exclusive in-end"
               );
            end if;
         when Out_End =>
            if not Compare_And_Swap
                   (  Target  => Object.Stream.Data.Has_Out'Access,
                      Old_Val => 0,
                      New_Val => 1
                   )
            then
               Raise_Exception
               (  Mode_Error'Identity,
                  "The queue already has an out-end"
               );
            end if;
         when Multiplexed_Out_End =>
            if (  not Compare_And_Swap
                      (  Target  => Object.Stream.Data.Has_Out'Access,
                         Old_Val => 0,
                         New_Val => 2
                      )
               and then
                  Object.Stream.Data.Has_Out = 1
               )
            then
               Raise_Exception
               (  Mode_Error'Identity,
                  "The queue has an exclusive out-end"
               );
            end if;
      end case;
   end Map;

   procedure Open (Stream : in out Interprocess_Stream) is
   begin
      if Stream.Data = null then
         Raise_Exception (Status_Error'Identity, Stream_Init);
      end if;
      Stream.Data.Closed := False;
   end Open;

   procedure Read
             (  Stream : in out Interprocess_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
      Started : constant Time := Clock;
   begin
      if Stream.Data = null then
         Raise_Exception (Status_Error'Identity, In_End_Init);
      end if;
      declare
         Free   : Positive renames Stream.Data.Free;
         First  : Positive renames Stream.Data.First;
         Buffer : Element_Array renames Stream.Data.Buffer;
      begin
         Last := Item'First - 1;
         while Item'Last /= Last loop
            declare
               Tail  : constant Stream_Element_Offset :=
                                Item'Last - Last;
               Count : Integer := Free - First;
            begin
               if Count = 0 then -- Empty
                  Reset (Stream.Not_Empty.all); -- Prepare wait event
                  if First = Free then          -- Buffer is still empty
                     if Stream.Data.Closed then
                        Raise_Exception (End_Error'Identity, Closed);
                     end if;
                     Wait
                     (  Stream.Not_Empty.all,
                        Stream.Timeout - (Clock - Started)
                     );
                  end if;
               else -- Not empty
                  if Count < 0 then
                     -- |///|            |///|
                     --      |            |
                     --     Free         First
                     --
                     Count := Buffer'Last + 1 - First;
                  else
                     -- |   |////////////|   |
                     --      |            |
                     --     First        Free
                     --
                     null;
                  end if;
                  if Tail <= Stream_Element_Offset (Count) then
                     -- Complete reading
                     declare
                        Next : constant Positive :=
                                        First + Integer (Tail);
                     begin
                        Item (Last + 1..Item'Last) :=
                           Stream_Element_Array
                           (  Buffer (First..Next - 1)
                           );
                        if Next > Buffer'Last then
                           First := 1;
                        else
                           First := Next;
                        end if;
                        Last := Item'Last;
                        Signal (Stream.Not_Full.all);
                        return;
                     end;
                  end if;
                  -- More data needed
                  declare
                     Next : constant Positive := First + Count;
                  begin
                     Item
                     (  Last + 1
                     .. Last + Stream_Element_Offset (Count)
                     )  := Stream_Element_Array
                           (  Buffer (First..Next - 1)
                           );
                     if Next > Buffer'Last then
                        First := 1;
                     else
                        First := Next;
                     end if;
                     Last := Last + Stream_Element_Offset (Count);
                     Signal (Stream.Not_Full.all);
                  end;
               end if;
            end;
         end loop;
      end;
   exception
      when Timeout_Error =>
         null;
   end Read;

   procedure Read
             (  Stream : in out Output_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      Raise_Exception (Mode_Error'Identity, Read_Output);
   end Read;

   procedure Read
             (  Stream : in out Universal_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      case Stream.Mode is
         when In_End | Multiplexed_In_End =>
            Read (Interprocess_Stream (Stream), Item, Last);
         when others =>
            Raise_Exception (Mode_Error'Identity, Read_Output);
      end case;
   end Read;

   procedure Set_Mode
             (  Stream : in out Universal_Stream;
                Mode   : Stream_End
             )  is
   begin
      if Stream.Data /= null then
         Raise_Exception
         (  Status_Error'Identity,
            "The stream is already initialized"
         );
      end if;
      Stream.Mode := Mode;
   end Set_Mode;

   procedure Set_Timeout
             (  Stream  : in out Interprocess_Stream;
                Timeout : Duration
             )  is
   begin
      Stream.Timeout := Timeout;
   end Set_Timeout;

   procedure Skip
             (  Stream  : in out Interprocess_Stream;
                Count   : in out Stream_Element_Count;
                Timeout : Duration
             )  is
      Started : constant Time := Clock;
   begin
      if Stream.Data = null then
         Raise_Exception (Status_Error'Identity, In_End_Init);
      end if;
      declare
         Free   : Positive renames Stream.Data.Free;
         First  : Positive renames Stream.Data.First;
         Buffer : Element_Array renames Stream.Data.Buffer;
      begin
         while Count > 0 loop
            declare
               Available : Integer := Free - First;
            begin
               if Available = 0 then -- Empty
                  Reset (Stream.Not_Empty.all); -- Prepare wait event
                  if First = Free then          -- Buffer is still empty
                     if Stream.Data.Closed then
                        Raise_Exception (End_Error'Identity, Closed);
                     end if;
                     Wait
                     (  Stream.Not_Empty.all,
                        Timeout - (Clock - Started)
                     );
                  end if;
               else -- Not empty
                  if Available < 0 then
                     -- |///|            |///|
                     --      |            |
                     --     Free         First
                     --
                     Available := Buffer'Last + 1 - First;
                  else
                     -- |   |////////////|   |
                     --      |            |
                     --     First        Free
                     --
                     null;
                  end if;
                  if Count <= Stream_Element_Count (Available) then
                     -- Complete skipping
                     declare
                        Next : constant Positive :=
                                        First + Integer (Count);
                     begin
                        Count := 0;
                        if Next > Buffer'Last then
                           First := 1;
                        else
                           First := Next;
                        end if;
                        Signal (Stream.Not_Full.all);
                        return;
                     end;
                  end if;
                  -- More data needed
                  declare
                     Next : constant Positive := First + Available;
                  begin
                     Count := Count - Stream_Element_Count (Available);
                     if Next > Buffer'Last then
                        First := 1;
                     else
                        First := Next;
                     end if;
                     Signal (Stream.Not_Full.all);
                  end;
               end if;
            end;
         end loop;
      end;
   exception
      when Timeout_Error =>
         null;
   end Skip;

   procedure Skip
             (  Stream : in out Interprocess_Stream;
                Count  : in out Stream_Element_Count
             )  is
   begin
      Skip (Stream, Count, Stream.Timeout);
   end Skip;

   procedure Skip
             (  Stream : in out Output_Stream;
                Count  : in out Stream_Element_Count
             )  is
   begin
      Raise_Exception (Mode_Error'Identity, Skip_Output);
   end Skip;

   procedure Skip
             (  Stream  : in out Output_Stream;
                Count   : in out Stream_Element_Count;
                Timeout : Duration
             )  is
   begin
      Raise_Exception (Mode_Error'Identity, Skip_Output);
   end Skip;

   procedure Skip
             (  Stream  : in out Universal_Stream;
                Count   : in out Stream_Element_Count;
                Timeout : Duration
             )  is
   begin
      case Stream.Mode is
         when In_End | Multiplexed_In_End =>
            Skip (Interprocess_Stream (Stream), Count, Timeout);
         when others =>
            Raise_Exception (Mode_Error'Identity, Skip_Output);
      end case;
   end Skip;

   procedure Skip
             (  Stream : in out Universal_Stream;
                Count  : in out Stream_Element_Count
             )  is
   begin
      Skip (Stream, Count, Stream.Timeout);
   end Skip;

   procedure Wait_For_Not_Empty
             (  Stream  : in out Interprocess_Stream;
                Timeout : Duration
             )  is
   begin
      if Is_Empty (Stream) then
         Reset (Stream.Not_Empty.all);
         if Is_Empty (Stream) then -- Still empty, enter wait
            if Stream.Data.Closed then
               Raise_Exception (End_Error'Identity, Closed);
            end if;
            Wait (Stream.Not_Empty.all, Timeout);
            if Is_Empty (Stream) and then Stream.Data.Closed then
               Raise_Exception (End_Error'Identity, Closed);
            end if;
         end if;
      end if;
   end Wait_For_Not_Empty;

   procedure Wait_For_Not_Empty
             (  Stream  : in out Interprocess_Stream;
                Timeout : Duration;
                Empty   : out Boolean
             )  is
      Signaled : Boolean;
   begin
      if Is_Empty (Stream) then
         Reset (Stream.Not_Empty.all);
         if Is_Empty (Stream) then -- Still empty, enter wait
            if Stream.Data.Closed then
               Raise_Exception (End_Error'Identity, Closed);
            end if;
            Wait (Stream.Not_Empty.all, Timeout, Signaled);
         end if;
         Empty := Is_Empty (Stream);
         if Empty and then Stream.Data.Closed then
            Raise_Exception (End_Error'Identity, Closed);
         end if;
      else
         Empty := False;
      end if;
   end Wait_For_Not_Empty;

   procedure Wait_For_Not_Full
             (  Stream  : in out Interprocess_Stream;
                Timeout : Duration
             )  is
   begin
      if Is_Full (Stream) then
         Reset (Stream.Not_Full.all);
         if Is_Full (Stream) then -- Still full, enter wait
            if Stream.Data.Closed then
               Raise_Exception (End_Error'Identity, Closed);
            end if;
            Wait (Stream.Not_Full.all, Timeout);
            if Is_Full (Stream) and then Stream.Data.Closed then
               Raise_Exception (End_Error'Identity, Closed);
            end if;
         end if;
      end if;
   end Wait_For_Not_Full;

   procedure Wait_For_Not_Full
             (  Stream  : in out Interprocess_Stream;
                Timeout : Duration;
                Full    : out Boolean
             )  is
      Signaled : Boolean;
   begin
      if Is_Full (Stream) then
         Reset (Stream.Not_Full.all);
         if Is_Full (Stream) then -- Still full, enter wait
            if Stream.Data.Closed then
               Raise_Exception (End_Error'Identity, Closed);
            end if;
            Wait (Stream.Not_Full.all, Timeout, Signaled);
         end if;
         Full := Is_Full (Stream);
         if Full and then Stream.Data.Closed then
            Raise_Exception (End_Error'Identity, Closed);
         end if;
      else
         Full := False;
      end if;
   end Wait_For_Not_Full;

   procedure Write
             (  Stream : in out Input_Stream;
                Item   : Stream_Element_Array
             )  is
   begin
      Raise_Exception (Mode_Error'Identity, Write_Input);
   end Write;

   procedure Write
             (  Stream : in out Interprocess_Stream;
                Item   : Stream_Element_Array
             )  is
      Started : constant Time := Clock;
   begin
      if Stream.Data = null then
         Raise_Exception (Status_Error'Identity, Out_End_Init);
      elsif Stream.Data.Closed then
         Raise_Exception (End_Error'Identity, Closed);
      end if;
      declare
         Free   : Positive renames Stream.Data.Free;
         First  : Positive renames Stream.Data.First;
         Buffer : Element_Array renames Stream.Data.Buffer;
         Index  : Stream_Element_Offset := Item'First;
      begin
         while Index <= Item'Last loop
            declare
               Tail  : constant Stream_Element_Offset :=
                                Item'Last - Index + 1;
               Space : Integer := First - Free - 1;
            begin
               if Space = 0 or else Space = -Buffer'Length then -- Full
                  Reset (Stream.Not_Full.all);
                  Space := First - Free;
                  if Space = 1 or else Space = 1 - Buffer'Length then
                     Wait
                     (  Stream.Not_Full.all,
                        Stream.Timeout - (Clock - Started)
                     );
                  end if;
               else -- Not full
                  if Space < 0 then
                     --
                     -- |   |////////////|   |
                     --      |            |
                     --     First        Free
                     --
                     Space := Integer'Min
                              (  Space + Buffer'Length,
                                 Buffer'Last + 1 - Free
                              );
                  else
                     --
                     -- |///|            |///|
                     --      |            |
                     --     Free         First
                     --
                     Space := Integer'Min (Space, Free);
                  end if;
                  if Stream_Element_Offset (Space) >= Tail then
                     -- Complete writing
                     declare
                        Next : constant Positive :=
                                        Free + Integer (Tail);
                        subtype Target is
                                Element_Array (Free..Next - 1);
                     begin
                        Buffer (Free..Next - 1) :=
                           Target (Item (Index..Item'Last));
                        if Next > Buffer'Last then
                           Free := 1;
                        else
                           Free := Next;
                        end if;
                        Signal (Stream.Not_Empty.all);
                        return;
                     end;
                  end if;
                  -- More space needed
                  declare
                     Next  : constant Positive := Free + Space;
                     Count : constant Stream_Element_Offset :=
                                      Stream_Element_Offset (Space);
                     subtype Target is Element_Array (Free..Next - 1);
                  begin
                     Buffer (Free..Next - 1) :=
                        Target (Item (Index..Index + Count - 1));
                     if Next > Buffer'Last then
                        Free := 1;
                     else
                        Free := Next;
                     end if;
                     Index := Index + Count;
                  end;
                  Signal (Stream.Not_Empty.all);
               end if;
            end;
            if Stream.Data.Closed then
               Raise_Exception (End_Error'Identity, Closed);
            end if;
         end loop;
      end;
   end Write;

   procedure Write
             (  Stream : in out Universal_Stream;
                Item   : Stream_Element_Array
             )  is
   begin
      case Stream.Mode is
         when Out_End | Multiplexed_Out_End =>
            Write (Interprocess_Stream (Stream), Item);
         when others =>
            Raise_Exception (Mode_Error'Identity, Write_Input);
      end case;
   end Write;

end Synchronization.Interprocess.Streams;
