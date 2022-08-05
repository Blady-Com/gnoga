--
--  Copyright  2021  cbb software  GmbH.  This  is a part of proprietary
--  software. All rights reserved.
--
--  Purpose :      Persistent streams (implementation)
--
--  Author :       Dmitry A. Kazakov
--
--  Language :     Ada 2005
--
--  Environment :  Any
--
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Interfaces;         use Interfaces;

package body Persistent.Streams is

   No_File_Open : constant String := "No file open";

   procedure Close (Stream : in out Persistent_Stream) is
      File : Persistent_Transactional_Array renames Stream.File;
   begin
      if Is_Open (Stream.File) then
         if Is_Writable (File) then
            Flush (Stream);
         end if;
         Close  (File);
      end if;
   end Close;

   procedure Dispose (Stream : in out Persistent_Stream) is
   begin
      Stream.Dispose (Stream.Read_Position);
   end Dispose;

   procedure Dispose
             (  Stream : in out Persistent_Stream;
                Upto   : Stream_Position
             )  is
      File     : Persistent_Transactional_Array renames Stream.File;
      Free     : Byte_Index      := Stream.Free;
      Index    : Byte_Index      := Stream.Out_Index;
      Position : Stream_Position := Stream.Out_Position;
      No       : Block_Index;
   begin
      if Upto > Stream.In_Position or else Upto < Position then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The position is ouside the stream"
         );
      end if;
      while Upto > Position loop
         No := Get_Index (Index);
         if No = 1 then -- Within the first block
            exit when
                 (  Upto - Position
                 <  Block_Byte_Size - Header_Size - Payload_Offset
                 );
         else
            exit when
                 (  Upto - Position
                 <  Block_Byte_Size - Payload_Offset
                 );
         end if;
         declare
            Block : Block_Type renames Get (File'Access, Index).all;
            Next  : constant Byte_Index := Get_Next (Block, Index);
         begin
            Put_Next (Block, Index, Free);
            if No = 1 then -- Within the first block
               Free := Compose (No, Header_Size + Payload_Offset);
            else
               Free := Compose (No, Payload_Offset);
            end if;
            Position :=
               (  Position
               +  Stream_Position (Block'Last - Get_Offset (Index))
               +  1
               );
            Index := Next;
         end;
      end loop;
      Stream.Free         := Free;
      Stream.Out_Index    := Index + Byte_Index (Upto - Position);
      Stream.Out_Position := Upto;
   end Dispose;

   function End_Of (Stream : Persistent_Stream) return Boolean is
   begin
      return Stream.Read_Position >= Stream.In_Position;
   end End_Of;

   procedure Erase (Stream : in out Persistent_Stream) is
   begin
      Dispose (Stream, Stream.In_Position);
   end Erase;

   procedure Flush (Stream : in out Persistent_Stream) is
      File  : Persistent_Transactional_Array renames Stream.File;
      Block : Block_Type renames Get (File'Access, 1).all;
   begin
      Put (Block, 0 * 8, Stream.In_Index);
      Put (Block, 1 * 8, Stream.In_Position);
      Put (Block, 2 * 8, Stream.Out_Index);
      Put (Block, 3 * 8, Stream.Out_Position);
      Put (Block, 4 * 8, Stream.Free);
      Commit (File);
   end Flush;

   function Get
            (  Block : Block_Type;
               Index : Block_Offset
            )  return Byte_Index is
   begin
      return
      (  Byte_Index (Block (Index    )) * 2**56
      +  Byte_Index (Block (Index + 1)) * 2**48
      +  Byte_Index (Block (Index + 2)) * 2**40
      +  Byte_Index (Block (Index + 3)) * 2**32
      +  Byte_Index (Block (Index + 4)) * 2**24
      +  Byte_Index (Block (Index + 5)) * 2**16
      +  Byte_Index (Block (Index + 6)) * 2**8
      +  Byte_Index (Block (Index + 7))
      );
   end Get;

   function Get
            (  Block : Block_Type;
               Index : Block_Offset
            )  return Stream_Position is
   begin
      return
      (  Stream_Position (Block (Index    )) * 2**56
      +  Stream_Position (Block (Index + 1)) * 2**48
      +  Stream_Position (Block (Index + 2)) * 2**40
      +  Stream_Position (Block (Index + 3)) * 2**32
      +  Stream_Position (Block (Index + 4)) * 2**24
      +  Stream_Position (Block (Index + 5)) * 2**16
      +  Stream_Position (Block (Index + 6)) * 2**8
      +  Stream_Position (Block (Index + 7))
      );
   end Get;

   procedure Get_Blocks
             (  Stream          : Persistent_Stream;
                Total_Blocks    : out Block_Count;
                Disposed_Blocks : out Block_Count;
                Free_Blocks     : out Block_Count;
                Logical_Blocks  : out Block_Count;
                Writable_Blocks : out Block_Count
             )  is
      File  : Persistent_Transactional_Array renames Stream.Self.File;
      Free  : Byte_Index  := Stream.Free;
      Count : Block_Count := 0;
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
      while Free /= 0 loop
         Count := Count + 1;
         Free  := Get_Next (Load (File'Access, Free).all, Free);
      end loop;
      Total_Blocks    := Get_Block_Size (Persistent_Array (File));
      Disposed_Blocks := Get_Disposed_Blocks (File);
      Free_Blocks     := Get_Free_Blocks (File);
      Logical_Blocks  := Get_Block_Size (File);
      Writable_Blocks := Count;
   end Get_Blocks;

   function Get_Block_Size
            (  Stream : Persistent_Stream
            )  return Block_Count is
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
      return Get_Block_Size (Stream.File);
   end Get_Block_Size;

   function Get_First_Index (Stream : Persistent_Stream)
      return Byte_Index is
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
      return Stream.Out_Index;
   end Get_First_Index;

   function Get_First_Position (Stream : Persistent_Stream)
      return Stream_Position is
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
      return Stream.Out_Position;
   end Get_First_Position;

   function Get_Name (Stream : Persistent_Stream) return String is
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
      return Get_Name (Stream.File);
   end Get_Name;

   function Get_Next
            (  Block : Block_Type;
               Index : Byte_Index
            )  return Byte_Index is
   begin
      if Get_Index (Index) = 1 then
         return Get (Block, Header_Size);
      else
         return Get (Block, 0);
      end if;
   end Get_Next;

   function Get_Write_Index (Stream : Persistent_Stream)
      return Byte_Index is
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
      return Stream.In_Index;
   end Get_Write_Index;

   function Get_Write_Position (Stream : Persistent_Stream)
      return Stream_Position is
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
      return Stream.In_Position;
   end Get_Write_Position;

   function Get_Size (Stream : Persistent_Stream)
      return Stream_Position is
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
      return Stream.In_Position - Stream.Out_Position;
   end Get_Size;

   function Get_Read_Index (Stream : Persistent_Stream)
      return Byte_Index is
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
      return Stream.Read_Index;
   end Get_Read_Index;

   function Get_Read_Position (Stream : Persistent_Stream)
      return Stream_Position is
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
      return Stream.Read_Position;
   end Get_Read_Position;

   function Is_Empty (Stream : Persistent_Stream) return Boolean is
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
      return Stream.In_Position = Stream.Out_Position;
   end Is_Empty;

   function Is_Open (Stream : Persistent_Stream) return Boolean is
   begin
      return Is_Open (Stream.File);
   end Is_Open;

   procedure Open
             (  Stream    : in out Persistent_Stream;
                Name      : String;
                Mode      : Access_Mode := Read_Mode;
                Hash_Size : Positive    := 256;
                Form      : String      := ""
             )  is
      File : Persistent_Transactional_Array renames Stream.File;
   begin
      Close (Stream);
      Open (File, Name, Mode, Hash_Size, Form);
      if Get_Block_Size (File) = 0 then
         declare
            Block : Block_Type renames Get (File'Access, 1).all;
         begin
            Put (Block, 0 * 8, Stream.In_Index);
            Put (Block, 1 * 8, Stream.In_Position);
            Put (Block, 2 * 8, Stream.Out_Index);
            Put (Block, 3 * 8, Stream.Out_Position);
            Put (Block, 4 * 8, Stream.Free);
            Put (Block, 5 * 8, Byte_Index'(0));
            Commit (File);
         end;
      else
         declare
            Block : Block_Type renames Load (File'Access, 1).all;
         begin
            Stream.In_Index     := Get (Block, 0 * 8);
            Stream.In_Position  := Get (Block, 1 * 8);
            Stream.Out_Index    := Get (Block, 2 * 8);
            Stream.Out_Position := Get (Block, 3 * 8);
            Stream.Free         := Get (Block, 4 * 8);
         end;
      end if;
      Rewind (Stream);
   end Open;

   procedure Put
             (  Block : in out Block_Type;
                Index : Block_Offset;
                Value : Byte_Index
             )  is
   begin
      Block (Index    ) := Unsigned_8 ((Value / 2**56) and 16#FF#);
      Block (Index + 1) := Unsigned_8 ((Value / 2**48) and 16#FF#);
      Block (Index + 2) := Unsigned_8 ((Value / 2**40) and 16#FF#);
      Block (Index + 3) := Unsigned_8 ((Value / 2**32) and 16#FF#);
      Block (Index + 4) := Unsigned_8 ((Value / 2**24) and 16#FF#);
      Block (Index + 5) := Unsigned_8 ((Value / 2**16) and 16#FF#);
      Block (Index + 6) := Unsigned_8 ((Value / 2**8 ) and 16#FF#);
      Block (Index + 7) := Unsigned_8 ((Value        ) and 16#FF#);
   end Put;

   procedure Put
             (  Block : in out Block_Type;
                Index : Block_Offset;
                Value : Stream_Position
             )  is
   begin
      Block (Index    ) := Unsigned_8 ((Value / 2**56) and 16#FF#);
      Block (Index + 1) := Unsigned_8 ((Value / 2**48) and 16#FF#);
      Block (Index + 2) := Unsigned_8 ((Value / 2**40) and 16#FF#);
      Block (Index + 3) := Unsigned_8 ((Value / 2**32) and 16#FF#);
      Block (Index + 4) := Unsigned_8 ((Value / 2**24) and 16#FF#);
      Block (Index + 5) := Unsigned_8 ((Value / 2**16) and 16#FF#);
      Block (Index + 6) := Unsigned_8 ((Value / 2**8 ) and 16#FF#);
      Block (Index + 7) := Unsigned_8 ((Value        ) and 16#FF#);
   end Put;

   procedure Put_Next
             (  Block : in out Block_Type;
                Index : Byte_Index;
                Next  : Byte_Index
             )  is
   begin
      if Get_Index (Index) = 1 then
         Put (Block, Header_Size, Next);
      else
         Put (Block, 0, Next);
      end if;
   end Put_Next;

   procedure Read
             (  Stream : in out Persistent_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
      File     : Persistent_Transactional_Array renames Stream.File;
      From     : Block_Offset;
      Size     : Block_Offset; -- Chunk size - 1
      To       : Stream_Element_Offset;
      Index    : Byte_Index      := Stream.Read_Index;
      Position : Stream_Position := Stream.Read_Position;
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      elsif Item'Length = 0 then
         Last := Item'First - 1;
         return;
      end if;
      To := Item'First;
      while Position < Stream.In_Position loop
         declare
            Block : Block_Type renames Load (File'Access, Index).all;
         begin
            From := Get_Offset (Index);
            Size := Block'Last - From;
            if Stream_Element_Offset (Size) >= Item'Last - To then
               -- The last chunk of data in this block
               Size := Block_Offset (Item'Last - To);
               loop
                  Item (To) := Stream_Element (Block (From));
                  From := From + 1;
                  if To = Item'Last then
                     if From = 0 then -- Next block (offset overrun)
                        Stream.Read_Index :=
                           Get_Next (Block, Index);
                     else
                        Stream.Read_Index :=
                           Index + Byte_Index (Size) + 1;
                     end if;
                     Stream.Read_Position :=
                        Position + Stream_Position (Size) + 1;
                     Last := Item'Last;
                     return;
                  end if;
                  To := To + 1;
               end loop;
            else
               -- More chunks to follow
               for Element in From..Block'Last loop
                  Item (To) := Stream_Element (Block (Element));
                  To := To + 1;
               end loop;
               Position := Position + Stream_Position (Size) + 1;
               Index    := Get_Next (Block, Index);
            end if;
         end;
      end loop;
      raise Data_Error with "Reading beyond the file end";
   end Read;

   procedure Rewind (Stream : in out Persistent_Stream) is
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      end if;
      Stream.Read_Index    := Stream.Out_Index;
      Stream.Read_Position := Stream.Out_Position;
   end Rewind;

   procedure Write
             (  Stream : in out Persistent_Stream;
                Item   : Stream_Element_Array
             )  is
      File     : Persistent_Transactional_Array renames Stream.File;
      Index    : Byte_Index    := Stream.In_Index;
      Position : Stream_Position := Stream.In_Position;
      From     : Stream_Element_Offset := Item'First;
      Size     : Block_Offset; -- Chunk size - 1
      procedure Allocate_Next is
         Next : Byte_Index;
      begin
         if Stream.Free = 0 then -- Allocate a new block
            declare
               New_Block : constant Block_Count :=
                                    Get_Block_Size (File) + 1;
            begin
               if New_Block = 1 then
                  Next := Compose (1, Header_Size + Payload_Offset);
               else
                  Next := Compose (New_Block, Payload_Offset);
               end if;
            end;
         else -- Get a block from the list of free blocks
            Next        := Stream.Free;
            Stream.Free := Get_Next(Get (File'Access, Next).all, Next);
         end if;
         Put_Next (Get (File'Access, Index).all, Index, Next);
         Index := Next;
      end Allocate_Next;
   begin
      if not Is_Open (Stream.File) then
         Raise_Exception (Use_Error'Identity, No_File_Open);
      elsif Item'Length = 0 then
         return;
      end if;
      loop
         declare
            Block : Block_Type renames Get (File'Access, Index).all;
            To    : Block_Offset := Get_Offset (Index);
         begin
            Size := Block'Last - To;
            if Stream_Element_Offset (Size) >= Item'Last - From then
               -- The last chunk is in this block
               Size := Block_Offset (Item'Last - From);
               for Element in From..Item'Last loop
                  Block (To) := Unsigned_8 (Item (Element));
                  To := To + 1;
               end loop;
               Stream.In_Position :=
                  Position + Stream_Position (Size) + 1;
               if To = 0 then -- Next block (offset overrun)
                  Allocate_Next;
                  Stream.In_Index := Index;
               else
                  Stream.In_Index := Index + Byte_Index (Size) + 1;
               end if;
               return;
            end if;
            -- More chunks to follow
            Size := Block'Last - To;
            for Element in To..Block'Last loop
               Block (Element) := Unsigned_8 (Item (From));
               From := From + 1;
            end loop;
         end;
         Position := Position + Stream_Position (Size) + 1;
         Allocate_Next;
      end loop;
   end Write;

end Persistent.Streams;
