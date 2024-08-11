--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Blocking_Files.                  Luebeck            --
--        Transactional                            Spring, 2014       --
--  Implementation                                                    --
--                                Last revision :  18:00 18 Aug 2022  --
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

package body Persistent.Blocking_Files.Transactional is

   Min_Map_Size : constant := 16;

   No_File_Open    : constant String := "No file open";
   Invalid_Segment : constant String := "Invalid block number of " &
                                        "a map segment";

   function Allocate
            (  Container : Persistent_Transactional_Array
            )  return Block_Index is
      Raw : Persistent_Array renames
            Persistent_Array (Container.Self.all);
      Physical : Block_Count := Block_Count (Container.Size);
   begin
      if Container.Length < Physical then -- Have unused file blocks
         Container.Self.Length := Container.Length + 1;
         return Container.Length;
      end if;
      while Container.Free.Length = 0 loop -- Empty free blocks segment
         if Container.Free.Next = 0 then -- The list is empty
            declare -- Allocate a new block in the file
               Ptr : Block_Type_Ptr;
            begin
               Physical := Physical + 1;
               Ptr := Get (Raw'Unchecked_Access, Compose (Physical, 0));
               Container.Self.Length := Physical;
               return Physical;
            end;
         end if;
         if Container.Free.Location /= 0 then
            Dispose (Container.Self.all, Container.Free.Location);
         end if;
         Load
         (  Container.Self.all,
            Container.Free.Next,
            Container.Self.Free
         );
      end loop;
      Physical := Container.Free.List (Container.Free.Length);
      Container.Self.Free.Length := Container.Free.Length - 1;
      return Physical;
   end Allocate;

   procedure Close
             (  Container : in out Persistent_Transactional_Array
             )  is
   begin
      Close (Persistent_Array (Container));
   end Close;

   procedure Commit
             (  Container : in out Persistent_Transactional_Array
             )  is
      Raw  : Persistent_Array renames
             Persistent_Array (Container.Self.all);
      Free : Block_Count := 0; -- The head of the free block list
   begin
      --
      -- Allocate place for the heads  of the block lists. We must do it
      -- in adavance because allocation has an effect on the list of
      -- free blocks
      --
      if Container.Disposed.Length > 0 then -- Have disposed blocks
         if Container.Disposed.Location = 0 then -- Allocate segment
            Container.Disposed.Location := Allocate (Container);
         end if;
      end if;
      if Container.Free.Length > 0 then -- Have free blocks
         if Container.Free.Location = 0 then -- Allocate segment
            Container.Free.Location := Allocate (Container);
         end if;
         Free := Container.Free.Location;
      end if;
      if Free > 0 then -- Store free blocks list head
         Store (Container, Container.Free);
      end if;
      if Container.Disposed.Length > 0 then -- Have disposed blocks
         if Free > 0 then -- Free blocks to prepend the disposed list
            if Container.First = 0 then -- Single list element
               Container.Disposed.Next := Free;
            else -- The list end is stored away
               declare
                  First : Free_Segment;
               begin
                  Load (Container, Container.First, First);
                  First.Next := Free;
                  Store (Container, First);
               end;
            end if;
         end if;
         Container.Free := Container.Disposed; -- Replace free list head
         Free := Container.Free.Location;
         Store (Container, Container.Free);
      end if;
      Container.First             := 0; -- The disposed list is now
      Container.Disposed.Location := 0; -- empty
      Container.Disposed.Next     := 0;
      Container.Disposed.Length   := 0;
      for Index in Container.List'First..Container.List'Last - 1 loop
         declare
            Segment : Map_Segment renames Container.List (Index);
         begin
            if Segment.Used then
               Store (Container, Segment, True);
               Segment.Modified  := False;
               Segment.Relocated := False;
               Segment.Updated   := (others => False);
            end if;
         end;
      end loop;
      Flush (Raw);
      declare
         Root : Map_Segment renames
                Container.List (Container.List'Last);
      begin
         Store (Container, Root, Free);
         Root.Modified  := False;
         Root.Relocated := False;
         Root.Updated   := (others => False);
      end;
      Flush (Raw);
      Container.Sequence := Container.Sequence + 1;
   end Commit;

   function Count
            (  Container : Persistent_Transactional_Array;
               Segment   : Free_Segment
            )  return Block_Count is
      Result   : Block_Count := 0;
      Physical : Block_Count := Segment.Next;
   begin
      Result := Result + Block_Count (Segment.Length);
      while Physical /= 0 loop
         declare
            This : Free_Segment;
         begin
            Load (Container.Self.all, Physical, This);
            Result   := Result + Block_Count (This.Length);
            Physical := This.Next;
         end;
      end loop;
      return Result;
   end Count;

   procedure Dispose
             (  Container : in out Persistent_Transactional_Array;
                Physical  : Block_Index
             )  is
      Disposed : Free_Segment renames Container.Disposed;
   begin
      if Disposed.Length = Disposed.List'Last then
         if Disposed.Location = 0 then
            Disposed.Location := Allocate (Container);
         end if;
         Store (Container.Self.all, Disposed);
         if Container.First = 0 then
            Container.First := Disposed.Location;
         end if;
         Disposed.Next     := Disposed.Location;
         Disposed.Location := 0;
         Disposed.Length   := 0;
      end if;
      Disposed.Length := Disposed.Length + 1;
      Disposed.List (Disposed.Length) := Physical;
   end Dispose;

   function Expand
            (  Container : Persistent_Transactional_Array
            )  return Block_Index is
      Last     : constant Block_Count := Container.Width;
      Next     : constant Block_Index := Last + 1;
      Physical : constant Block_Index := Allocate (Container);
      Current  : Block_Index := Physical;
      Offset   : Map_Index;
      Root     : Map_Segment renames
                 Container.List (Container.List'Last);
   begin
      for Height in 0..Root.Height loop
         Offset := Get_Offset (Next, Height);
         if Height = Root.Height then -- Root segment
            if Offset >= Root_Size then -- No place in the root segment
               declare
                  This : Map_Segment renames
                         Container.Self.List
                         (  Get_Segment_No (Container, Next)
                         );
               begin
                  if This.Used then
                     Store (Container.Self.all, This, False);
                  end if;
                  This.Location         := Allocate (Container);
                  This.Lower            := Get_Lower (Next);
                  This.Height           := Height;
                  This.Used             := True;
                  This.Modified         := True;
                  This.Relocated        := True;
                  This.Map              := Root.Map;
                  This.Map (Offset)     := Current;
                  This.Updated          := Root.Updated;
                  This.Updated (Offset) := True;
                  Current               := This.Location;

                  Root.Height      := Root.Height + 1;
                  Root.Modified    := True;
                  Root.Map         := (others => 0);
                  Root.Updated     := (others => False);
                  Root.Map (0)     := Current;
                  Root.Updated (0) := True;
               end;
            else
               Root.Modified := True;
               Root.Map (Offset) := Current;
               Root.Updated (Offset) := True;
            end if; -- Have place in the root segment
            exit;
         elsif Offset = 0 then
            -- Have to allocate a new segment
            declare
               This : Map_Segment renames
                      Container.Self.List
                      (  Get_Segment_No (Container, Next)
                      );
            begin
               if This.Used then
                  Store (Container.Self.all, This, False);
               end if;
               This.Location    := Allocate (Container);
               This.Lower       := Get_Lower (Next);
               This.Height      := Height;
               This.Used        := True;
               This.Modified    := True;
               This.Relocated   := True;
               This.Map (0)     := Current;
               This.Updated (0) := True;
               Current := This.Location;
            end;
         else -- There is place in the last segment at this height
            declare
               Segment : constant Map_Segment_Ptr :=
                         Find (Container, Last, Height);
            begin
               if Segment = null then
                  Raise_Exception
                  (  Data_Error'Identity,
                     Invalid_Segment
                  );
               end if;
               Segment.Modified := True;
               Segment.Map (Offset) := Current;
               Segment.Updated (Offset) := True;
            end;
            exit;
         end if;
      end loop;
      Container.Self.Width := Last + 1;
      return Physical;
   end Expand;

   procedure Finalize
             (  Container : in out Persistent_Transactional_Array
             )  is
   begin
      Close (Container);
      Free (Container.List);
      Finalize (Persistent_Array (Container));
   end Finalize;

   function Find
            (  Container : Persistent_Transactional_Array;
               Virtual   : Block_Index;
               Height    : Map_Level
            )  return Map_Segment_Ptr is
   begin
      if Height = Container.List (Container.List'Last).Height then
         --
         -- The segment is the root segment
         --
         return Container.List (Container.List'Last)'Unchecked_Access;
      end if;
      declare
         Lower : constant Block_Index := Get_Lower (Virtual);
         This  : Map_Segment renames
                 Container.Self.List
                 (  Get_Segment_No (Container, Virtual)
                 );
      begin
         if This.Used then
            if Height = This.Height and then Lower = This.Lower then
               return This'Unchecked_Access;
            end if;
            Store (Container.Self.all, This, False);
         end if;
         declare
            Parent : Map_Segment_Ptr;
         begin
            Parent := Find (Container, Virtual, Height + 1);
            if Parent = null then
               return null;
            end if;
            Load
            (  Container => Container.Self.all,
               Lower     => Lower,
               Height    => Height,
               Segment   => This,
               Physical  => Parent.Map
                            (  Get_Offset
                               (  Virtual,
                                  Height + 1
            )               )  );
            return This'Unchecked_Access;
         end;
      end;
   end Find;

   function Fletcher_16 (Data : Byte_Array) return Byte_Array is
      Sum_1 : Unsigned_16 := 0;
      Sum_2 : Unsigned_16 := 0;
   begin
      for Index in Data'Range loop
         Sum_1 := (Sum_1 + Unsigned_16 (Data (Index))) and 16#FF#;
         Sum_2 := (Sum_1 + Sum_2) and 16#FF#;
      end loop;
      return
      (  0 => Unsigned_8 (Sum_2 and 16#FF#),
         1 => Unsigned_8 (Sum_1 and 16#FF#)
      );
   end Fletcher_16;

   procedure Flush
             (  Container : in out Persistent_Transactional_Array
             )  is
   begin
      if (  Container.Is_Open
         and then
            Container.Buffer /= null
         and then
            Mode (Container.File) /= In_File
         )
      then
         Commit (Container);
      end if;
   end Flush;

   function Get
            (  Container : access Persistent_Transactional_Array;
               Index     : Byte_Index
            )  return Block_Type_Ptr is
      Virtual  : constant Block_Index := Get_Index (Index);
      Physical : Block_Count;
   begin
      if not Container.Is_Open then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      elsif Mode (Container.File) = In_File then
         Raise_Exception (Use_Error'Identity, "File is read-only");
      end if;
      while Virtual > Container.Width loop
         Physical := Expand (Container.all);
      end loop;
      Physical := Update (Container.all, Virtual, False);
      if Physical = 0 then
         Raise_Exception
         (  Data_Error'Identity,
            "Internal mapping error in Get"
         );
      end if;
      return Update
             (  Persistent_Array (Container.all)'Unchecked_Access,
                Compose (Physical, 0)
             );
   end Get;

   function Get
            (  Block  : Byte_Array;
               Offset : Block_Offset
            )  return Unsigned_16 is
   begin
      return
      (              Unsigned_16 (Block (Offset    ))
      or Shift_Left (Unsigned_16 (Block (Offset + 1)), 8)
      );
   end Get;

   function Get
            (  Block  : Byte_Array;
               Offset : Block_Offset
            )  return Unsigned_64 is
   begin
      return
      (              Unsigned_64 (Block (Offset    ))
      or Shift_Left (Unsigned_64 (Block (Offset + 1)),  8)
      or Shift_Left (Unsigned_64 (Block (Offset + 2)), 16)
      or Shift_Left (Unsigned_64 (Block (Offset + 3)), 24)
      or Shift_Left (Unsigned_64 (Block (Offset + 4)), 32)
      or Shift_Left (Unsigned_64 (Block (Offset + 5)), 40)
      or Shift_Left (Unsigned_64 (Block (Offset + 6)), 48)
      or Shift_Left (Unsigned_64 (Block (Offset + 7)), 56)
      );
   end Get;

   function Get
            (  Block  : Byte_Array;
               Offset : Block_Offset
            )  return Block_Count is
      Value : constant Unsigned_64 := Get (Block, Offset);
   begin
      return Block_Count (Value);
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            "Illegal file block count" & Unsigned_64'Image (Value)
         );
   end Get;

   function Get
            (  Block  : Byte_Array;
               Offset : Block_Offset
            )  return Free_Count is
      Value : constant Unsigned_16 := Get (Block, Offset);
   begin
      return Free_Count (Value);
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            "Illegal free block count" & Unsigned_16'Image (Value)
         );
   end Get;

   function Get_Allocated_Size
            (  Container : Persistent_Transactional_Array
            )  return Byte_Index is
   begin
      return Get_Size (Persistent_Array (Container));
   end Get_Allocated_Size;

   function Get_Block_Size
            (  Container : Persistent_Transactional_Array
            )  return Block_Count is
   begin
      if Container.Is_Open then
         return Container.Width;
      else
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
   end Get_Block_Size;

   function Get_Disposed_Blocks
            (  Container : Persistent_Transactional_Array
            )  return Block_Count is
   begin
      return Count (Container, Container.Disposed);
   end Get_Disposed_Blocks;

   function Get_Free_Blocks
            (  Container : Persistent_Transactional_Array
            )  return Block_Count is
   begin
      return Count (Container, Container.Free);
   end Get_Free_Blocks;

   function Get_Lower (Block : Block_Index) return Block_Index is
   begin
      return Block - (Block - 1) mod Map_Size;
   end Get_Lower;

   function Get_Map_Depth
            (  Container : Persistent_Transactional_Array
            )  return Natural is
   begin
      if not Container.Is_Open then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      else
         return
            Natural (Container.List (Container.List'Last).Height) + 1;
      end if;
   end Get_Map_Depth;

   function Get_Map_Size (Size : Byte_Index) return Natural is
      Count  : Byte_Index := 0;
      Blocks : Byte_Index :=
                  (Size + Block_Byte_Size - 1) / Block_Byte_Size;
   begin
      while Blocks > Root_Size loop
         Blocks := (Blocks + Map_Size - 1) / Map_Size;
         Count  := Count + Blocks;
      end loop;
      return Natural (Count + 1);
   end Get_Map_Size;

   function Get_Offset (Block : Block_Index) return Map_Index is
   begin
      return Map_Index ((Block - 1) mod Map_Size);
   end Get_Offset;

   function Get_Offset
            (  Block  : Block_Index;
               Height : Map_Level
            )  return Map_Index is
   begin
      return
         Map_Index
         (  ((Block_Count (Block) - 1) / Map_Size ** Integer (Height))
         mod
            Map_Size
         );
   end Get_Offset;

   function Get_Parent (Block : Block_Index) return Block_Index is
   begin
      return (Block - 1) / Map_Size + 1;
   end Get_Parent;

   function Get_Parent
            (  Block  : Block_Index;
               Height : Map_Level
            )  return Block_Index is
   begin
      return (Block - 1) / Map_Size ** Integer (Height + 1) + 1;
   end Get_Parent;

   function Get_Physical
            (  Container : Persistent_Transactional_Array;
               Virtual   : Block_Index
            )  return Block_Count is
      Segment : constant Map_Segment_Ptr := Find (Container, Virtual, 0);
   begin
      if Segment = null then
         return 0;
      else
         return Segment.Map (Get_Offset (Virtual));
      end if;
   end Get_Physical;

   function Get_Segment_No
            (  Container : Persistent_Transactional_Array;
               Virtual   : Block_Index
            )  return Map_Segment_No is
   begin
      return
         Map_Segment_No
         (  ((Virtual - 1) / Map_Size) mod Container.List'Length
         );
   end Get_Segment_No;

   function Get_Sequence_No
            (  Container : Persistent_Transactional_Array
            )  return Unsigned_64 is
   begin
      if Container.Is_Open then
         return Container.Sequence;
      else
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
   end Get_Sequence_No;

   function Get_Size
            (  Container : Persistent_Transactional_Array
            )  return Byte_Index is
   begin
      if Container.Is_Open then
         return Byte_Index (Container.Width) * Block_Byte_Size;
      else
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
   end Get_Size;

   function Get_Used_Size
            (  Container : Persistent_Transactional_Array
            )  return Byte_Index is
   begin
      if Container.Is_Open then
         return Byte_Index (Container.Length) * Block_Byte_Size;
      else
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
   end Get_Used_Size;

   function Is_Resident
            (  Container : Persistent_Transactional_Array;
               Index     : Block_Index
            )  return Boolean is
   begin
      if Container.Is_Open then
         declare
            Physical : constant Block_Count := Map (Container, Index);
         begin
            if Physical /= 0 then
               return Is_Resident
                      (  Persistent_Array (Container),
                         Physical
                      );
            end if;
         end;
      end if;
      return False;
   end Is_Resident;

   function Is_Resident
            (  Container : Persistent_Transactional_Array;
               Index     : Byte_Index
            )  return Boolean is
   begin
      return Is_Resident (Container, Get_Index (Index));
   end Is_Resident;

   function Is_Updated
            (  Container : Persistent_Transactional_Array;
               Virtual   : Block_Index
            )  return Boolean is
      Segment : constant Map_Segment_Ptr :=
                Find (Container, Virtual, 0);
   begin
      if Segment = null then
         return False;
      else
         return Segment.Updated (Get_Offset (Virtual));
      end if;
   end Is_Updated;

   procedure Load
             (  Container : in out Persistent_Transactional_Array;
                Physical  : Block_Index;
                Segment   : out Free_Segment
             )  is
      Ptr : constant Block_Type_Ref :=
               Load
               (  Persistent_Array (Container)'Access,
                  Compose (Physical, 0)
               );
   begin
      if Ptr = null then
         Raise_Exception
         (  Data_Error'Identity,
            "Invalid block number of a free block list segment"
         );
      end if;
      declare
         Block  : Block_Type renames Ptr.all;
         Offset : Block_Offset := 0;
      begin
         Segment.Location := Physical;
         Segment.Next     := Get (Block, Offset); Offset := Offset + 8;
         Segment.Length   := Get (Block, Offset); Offset := Offset + 2;
         for Index in 1..Segment.Length loop
            Segment.List (Index) := Get (Block, Offset);
            Offset := Offset + 8;
         end loop;
         for Index in Segment.Length + 1..Segment.List'Last loop
            Segment.List (Index) := 0;
         end loop;
      end;
   end Load;

   procedure Load
             (  Container : in out Persistent_Transactional_Array;
                Physical  : Block_Index;
                Lower     : Block_Index;
                Height    : Map_Level;
                Segment   : out Map_Segment
             )  is
      Ptr : constant Block_Type_Ref :=
               Load
               (  Persistent_Array (Container)'Access,
                  Compose (Physical, 0)
               );
   begin
      if Ptr = null then
         Raise_Exception
         (  Data_Error'Identity,
            Invalid_Segment
         );
      end if;
      declare
         Block  : Block_Type renames Ptr.all;
         Byte   : Unsigned_8;
         Mask   : Unsigned_8;
         Offset : Block_Offset;
      begin
         Segment.Location  := Physical;
         Segment.Lower     := Lower;
         Segment.Height    := Height;
         Segment.Used      := True;
         Segment.Modified  := False;
         Segment.Relocated := Container.Sequence = Get (Block, 0);
         Offset := 8;
         for Index in Segment.Map'Range loop
            Segment.Map (Index) := Get (Block, Offset);
            Offset := Offset + 8;
         end loop;
         if Segment.Relocated then
            for Index in Segment.Updated'Range loop
               if Index mod 8 = 0 then
                  Byte   := Block (Offset);
                  Mask   := 1;
                  Offset := Offset + 1;
               else
                  Mask := Mask + 2;
               end if;
               Segment.Updated (Index) := 0 /= (Byte and Mask);
            end loop;
         else -- Old segment refers to old blocks
            Segment.Updated := (others => False);
         end if;
      end;
   end Load;

   procedure Load
             (  Container : in out Persistent_Transactional_Array;
                Physical  : Block_Index;
                Data      : out Master_Block_Data
             )  is
      Ptr : constant Block_Type_Ref :=
               Load
               (  Persistent_Array (Container)'Access,
                  Compose (Physical, 0)
               );
   begin
      Data.Valid := False;
      if Ptr = null then
         Raise_Exception
         (  Data_Error'Identity,
            Invalid_Segment
         );
      end if;
      declare
         Block  : Block_Type renames Ptr.all;
         Offset : Block_Offset := 2;
      begin
         if Fletcher_16 (Block (2..Block'Last)) /= Block (0..1) then
            Raise_Exception
            (  Status_Error'Identity,
               "Wrong check sum"
            );
         end if;
         Data.Segment.Location  := (Physical mod 2) + 1; -- Alternate
         Data.Segment.Lower     := 1;
         Data.Segment.Used      := True;
         Data.Segment.Modified  := False;
         Data.Segment.Relocated := False;
         Data.Sequence := Get (Block, Offset) + 1; -- Next
         Offset := Offset + 8;
         Data.Length := Get (Block, Offset);
         Offset := Offset + 8;
         Data.Size := Get (Block, Offset);
         Offset := Offset + 8;
         Data.Free := Get (Block, Offset);
         Offset := Offset + 8;
         Data.Segment.Height := Map_Level (Block (Offset));
         Offset := Offset + 1;
         for Index in Root_Map_Index'Range loop
            Data.Segment.Map (Index) := Get (Block, Offset);
            Offset := Offset + 8;
         end loop;
         for Index in Root_Map_Index'Last + 1..Map_Index'Last loop
            Data.Segment.Map (Index) := 0;
         end loop;
         Data.Segment.Updated := (others => False);
      exception
         when Error : Constraint_Error =>
            Raise_Exception
            (  Status_Error'Identity,
               Exception_Message (Error)
            );
      end;
      Data.Valid := True;
   end Load;

   function Load
            (  Container : access Persistent_Transactional_Array;
               Index     : Byte_Index
            )  return Block_Type_Ref is
      Virtual  : constant Block_Index := Get_Index (Index);
      Physical : Block_Count;
   begin
      if not Container.Is_Open then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      elsif Virtual > Container.Width then -- Outside the virtual file
         return null;
      else
         Physical := Map (Container.all, Virtual);
         if Physical = 0 then
            Raise_Exception
            (  Data_Error'Identity,
               "Internal mapping error in Load"
            );
         end if;
         return Load
                (  Persistent_Array (Container.all)'Unchecked_Access,
                   Compose (Physical, 0)
                );
      end if;
   end Load;

   function Map
            (  Container : Persistent_Transactional_Array;
               Virtual   : Block_Index
            )  return Block_Count is
      Segment : constant Map_Segment_Ptr :=
                Find (Container, Virtual, 0);
   begin
      if Segment = null then
         return 0;
      else
         return Segment.Map (Get_Offset (Virtual));
      end if;
   end Map;

   procedure Open
             (  Container : in out Persistent_Transactional_Array;
                Hash_Size : Positive := 256;
                Form      : String   := ""
             )  is
   begin
      Open (Container, "", Create_Mode, Hash_Size, 256, Form);
   end Open;

   procedure Open
             (  Container : in out Persistent_Transactional_Array;
                Hash_Size : Positive := 256;
                Map_Size  : Positive;
                Form      : String   := ""
             )  is
   begin
      Open (Container, "", Create_Mode, Hash_Size, Map_Size, Form);
   end Open;

   procedure Open
             (  Container : in out Persistent_Transactional_Array;
                Name      : String;
                Mode      : Access_Mode := Read_Mode;
                Hash_Size : Positive    := 256;
                Form      : String      := ""
             )  is
   begin
      Open (Container, Name, Mode, Hash_Size, 256, Form);
   end Open;

   procedure Open
             (  Container : in out Persistent_Transactional_Array;
                Name      : String;
                Mode      : Access_Mode := Read_Mode;
                Hash_Size : Positive    := 256;
                Map_Size  : Positive;
                Form      : String      := ""
             )  is
      Size : constant Positive := Positive'Max (Map_Size, Min_Map_Size);
   begin
      Close (Container);
      Open (Persistent_Array (Container), Name, Mode, Hash_Size, Form);
      if Container.List = null then
         Container.List :=
            new Map_Segments_Array (0..Map_Segment_No (Size));
      elsif Container.List'Length < Size then
         Free (Container.List);
         Container.List :=
            new Map_Segments_Array (0..Map_Segment_No (Size));
      end if;
      Rollback (Container);
   end Open;

   procedure Put
             (  Block  : in out Byte_Array;
                Offset : Block_Offset;
                Value  : Unsigned_16
             )  is
   begin
      Block (Offset..Offset + 1) :=
         (  1 => Unsigned_8 (16#FF# and Value),
            2 => Unsigned_8 (16#FF# and Shift_Right (Value, 8))
         );
   end Put;

   procedure Put
             (  Block  : in out Byte_Array;
                Offset : Block_Offset;
                Value  : Unsigned_64
             )  is
   begin
      Block (Offset..Offset + 7) :=
         (  1 => Unsigned_8 (16#FF# and Value),
            2 => Unsigned_8 (16#FF# and Shift_Right (Value, 8)),
            3 => Unsigned_8 (16#FF# and Shift_Right (Value, 16)),
            4 => Unsigned_8 (16#FF# and Shift_Right (Value, 24)),
            5 => Unsigned_8 (16#FF# and Shift_Right (Value, 32)),
            6 => Unsigned_8 (16#FF# and Shift_Right (Value, 40)),
            7 => Unsigned_8 (16#FF# and Shift_Right (Value, 48)),
            8 => Unsigned_8 (16#FF# and Shift_Right (Value, 56))
         );
   end Put;

   procedure Put
             (  Block  : in out Byte_Array;
                Offset : Block_Offset;
                Value  : Block_Count
             )  is
   begin
      Put (Block, Offset, Unsigned_64 (Value));
   end Put;

   procedure Put
             (  Block  : in out Byte_Array;
                Offset : Block_Offset;
                Value  : Free_Count
             )  is
   begin
      Put (Block, Offset, Unsigned_16 (Value));
   end Put;

   procedure Read
             (  Container : in out Persistent_Transactional_Array;
                Index     : Byte_Index;
                Block     : out Block_Type
             )  is
      Virtual  : constant Block_Index := Get_Index (Index);
      Physical : Block_Count;
   begin
      if not Container.Is_Open then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      elsif Virtual > Container.Width then -- Outside the virtual file
         Raise_Exception (End_Error'Identity, "Out of file");
      end if;
      Physical := Map (Container, Virtual);
      if Physical = 0 then
         Raise_Exception
         (  Data_Error'Identity,
            "Reading not mapped block"
         );
      else
         Read
         (  Container => Persistent_Array (Container),
            Index     => Compose (Physical, 0),
            Block     => Block
         );
      end if;
   end Read;

   procedure Rollback
             (  Container : in out Persistent_Transactional_Array
             )  is
   begin
      Container.Length := Block_Count (Container.Size);
      if Container.Length <= 2 then -- First time
         Container.Sequence      := 0;
         Container.Length        := 2;
         Container.Width         := 0;
         Container.Free.Location := 0;
         Container.Free.Length   := 0;
         Container.Free.Next     := 0;
         declare -- Allocate file header
            Block : Block_Type_Ptr;
         begin
            Block :=
               Get
               (  Persistent_Array (Container)'Unchecked_Access,
                  Compose (2, 0)
               );
         end;
         declare
            Root : Map_Segment renames
                   Container.List (Container.List'Last);
         begin
            Root.Location := 1;
            Store (Container, Root, 0);
            Root.Location := 2;
            Store (Container, Root, 0);
            Container.Sequence := 1;
         end;
      else
         declare
            Last : Positive;
            Data : array (1..2) of Master_Block_Data;
         begin
            begin
               Load (Container, 1, Data (1));
               Last := 1;
            exception
               when Status_Error | Data_Error =>
                  null;
            end;
            begin
               Load (Container, 2, Data (2));
               if Data (1).Sequence < Data (2).Sequence then
                  Last := 2;
               end if;
            exception
               when Error : Status_Error | Data_Error =>
                  if not Data (1).Valid then
                     Raise_Exception
                     (  Data_Error'Identity,
                        Exception_Message (Error)
                     );
                  end if;
            end;
            Container.List (Container.List'Last) := Data (Last).Segment;
            Container.Sequence := Data (Last).Sequence;
            Container.Length   := Data (Last).Length;
            Container.Width    := Data (Last).Size;
            if Data (Last).Free = 0 then
               Container.Free.Location := 0;
               Container.Free.Next     := 0;
               Container.Free.Length   := 0;
            else
               Load (Container, Data (Last).Free, Container.Free);
            end if;
         end;
      end if;
      for Index in Container.List'First..Container.List'Last - 1 loop
         Container.List (Index).Used := False;
      end loop;
      Container.Disposed.Location := 0;
      Container.Disposed.Length := 0;
      Container.Disposed.Next := 0;
      Container.First := 0;
   end Rollback;

   procedure Store
             (  Container : in out Persistent_Transactional_Array;
                Segment   : in out Map_Segment;
                Recurse   : Boolean
             )  is
      Relocate : Boolean := False;
      Ptr      : Block_Type_Ptr;
   begin
      if not Segment.Used or else not Segment.Modified then
         return;
      end if;
      if not Segment.Relocated then
         declare
            Physical : constant Block_Index := Segment.Location;
         begin
            Relocate          := True;
            Segment.Location  := Allocate (Container);
            Segment.Relocated := True;
            Dispose (Container, Physical);
         end;
      end if;
      Ptr := Update
             (  Persistent_Array (Container)'Unchecked_Access,
                Compose (Segment.Location, 0)
             );
      if Ptr = null then
         Raise_Exception
         (  Data_Error'Identity,
            Invalid_Segment
         );
      end if;
      Segment.Modified := False;
      declare
         Block  : Block_Type renames Ptr.all;
         Offset : Block_Offset := 0;
         Byte   : Unsigned_8   := 0;
         Mask   : Unsigned_8   := 1;
      begin
         Put (Block, Offset, Container.Sequence);
         Offset := Offset + 8;
         for Index in Map_Index'Range loop
            Put (Block, Offset, Segment.Map (Index));
            Offset := Offset + 8;
         end loop;
         for Index in Map_Index'Range loop
            if Segment.Updated (Index) then
               Byte := Byte + Mask;
            end if;
            if Index mod 8 = 7 then
               Block (Offset) := Byte;
               Offset := Offset + 1;
               Byte   := 0;
               Mask   := 1;
            else
               Mask := Mask * 2;
            end if;
         end loop;
         if Map_Index'Last mod 8 /= 7 then
            Block (Offset) := Byte;
         end if;
      end;
      if Relocate then
         declare
            Parent   : Map_Segment_Ptr;
            Physical : constant Block_Index := Segment.Location;
            Offset   : constant Map_Index :=
                       Get_Offset (Segment.Lower, Segment.Height + 1);
         begin
            Parent :=
               Find (Container, Segment.Lower, Segment.Height + 1);
            if Parent = null then
               Raise_Exception
               (  Data_Error'Identity,
                  Invalid_Segment
               );
            end if;
            Parent.Map (Offset) := Physical;
            Parent.Updated (Offset) := True;
            if (  Recurse
               and then
                  (  Parent.Height
                  <  Container.List (Container.List'Last).Height
               )  )
            then
               Store (Container, Parent.all, True);
            end if;
         end;
      end if;
   end Store;

   procedure Store
             (  Container : in out Persistent_Transactional_Array;
                Segment   : Map_Segment;
                Free      : Block_Count
             )  is
      Ptr : constant Block_Type_Ptr :=
               Update
               (  Persistent_Array (Container)'Unchecked_Access,
                  Compose (Segment.Location, 0)
               );
   begin
      if Ptr = null then
         Raise_Exception
         (  Data_Error'Identity,
            Invalid_Segment
         );
      end if;
      declare
         Block  : Block_Type renames Ptr.all;
         Offset : Block_Offset := 2;
         Byte   : Unsigned_8   := 0;
         Mask   : Unsigned_8   := 1;
      begin
         Offset := 2;
         Put (Block, Offset, Container.Sequence);
         Offset := Offset + 8;
         Put (Block, Offset, Container.Length);
         Offset := Offset + 8;
         Put (Block, Offset, Container.Width);
         Offset := Offset + 8;
         Put (Block, Offset, Free);
         Offset := Offset + 8;
         Block (Offset) := Unsigned_8 (Segment.Height);
         Offset := Offset + 1;
         for Index in Root_Map_Index'Range loop
            Put (Block, Offset, Segment.Map (Index));
            Offset := Offset + 8;
         end loop;
         for Index in Root_Map_Index'Range loop
            if Segment.Updated (Index) then
               Byte := Byte + Mask;
            end if;
            if Index mod 8 = 7 then
               Block (Offset) := Byte;
               Offset := Offset + 1;
               Byte   := 0;
               Mask   := 1;
            else
               Mask := Mask * 2;
            end if;
         end loop;
         if Root_Map_Index'Last mod 8 /= 7 then
            Block (Offset) := Byte;
         end if;
         Block (0..1) := Fletcher_16 (Block (2..Block'Last));
      end;
   end Store;

   procedure Store
             (  Container : in out Persistent_Transactional_Array;
                Segment   : Free_Segment
             )  is
      Ptr : constant Block_Type_Ptr :=
               Update
               (  Persistent_Array (Container)'Access,
                  Compose (Segment.Location, 0)
               );
   begin
      if Ptr = null then
         Raise_Exception
         (  Data_Error'Identity,
            "Invalid block number of a free block list segment"
         );
      end if;
      declare
         Block  : Block_Type renames Ptr.all;
         Offset : Block_Offset := 0;
      begin
         Put (Block, Offset, Segment.Next);
         Offset := Offset + 8;
         Put (Block, Offset, Segment.Length);
         Offset := Offset + 2;
         for Index in 1..Segment.Length loop
            Put (Block, Offset, Segment.List (Index));
            Offset := Offset + 8;
         end loop;
         for Index in Offset..Block'Last loop
            Block (Index) := 0;
         end loop;
      end;
   end Store;

   function Update
            (  Container : access Persistent_Transactional_Array;
               Index     : Byte_Index
            )  return Block_Type_Ptr is
      Virtual : constant Block_Index := Get_Index (Index);
   begin
      if not Container.Is_Open then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      elsif Mode (Container.File) = In_File then
         Raise_Exception (Use_Error'Identity, "File is read-only");
      elsif Virtual > Container.Width then -- Outside the virtual file
         return null;
      else
         return Update
                (  Persistent_Array (Container.all)'Unchecked_Access,
                   Compose (Update (Container.all, Virtual, False), 0)
                );
      end if;
   end Update;

   function Update
            (  Container : Persistent_Transactional_Array;
               Virtual   : Block_Index;
               Replace   : Boolean
            )  return Block_Index is
      Segment  : constant Map_Segment_Ptr :=
                 Find (Container, Virtual, 0);
      Index    : constant Map_Index := Get_Offset (Virtual);
      Physical : Block_Count := Segment.Map (Index);
   begin
      if Segment = null then
         Raise_Exception
         (  Data_Error'Identity,
            (  "No map segment found while updating virtual block"
            &  Block_Index'Image (Virtual)
         )  );
      end if;
      if not Segment.Updated (Index) then
         Physical := Segment.Map (Index);
         if Physical = 0 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Updating an unmapped vitual block"
               &  Block_Index'Image (Virtual)
            )  );
         end if;
         declare
            Parent : Persistent_Array renames
                     Persistent_Array (Container.Self.all);
         begin
            Segment.Map (Index) := Allocate (Container);
            Segment.Updated (Index) := True;
            Segment.Modified := True;
            if not Replace then
               Write
               (  Parent,
                  Compose (Segment.Map (Index), 0),
                  Load (Parent'Access, Compose (Physical, 0)).all
               );
            end if;
            Dispose (Container.Self.all, Physical);
         end;
      end if;
      return Segment.Map (Index);
   end Update;

   procedure Write
             (  Container : in out Persistent_Transactional_Array;
                Index     : Byte_Index;
                Block     : Block_Type
             )  is
      Virtual  : constant Block_Index := Get_Index (Index);
      Physical : Block_Count;
   begin
      if not Container.Is_Open then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      elsif Mode (Container.File) = In_File then
         Raise_Exception (Use_Error'Identity, "File is read-only");
      end if;
      while Virtual < Container.Width loop
         Physical := Expand (Container);
      end loop;
      Write
      (  Persistent_Array (Container),
         Compose (Update (Container, Virtual, True), 0),
         Block
      );
   end Write;

end Persistent.Blocking_Files.Transactional;
