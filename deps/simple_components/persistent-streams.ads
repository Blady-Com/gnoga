--
--  Copyright  2021  cbb software  GmbH.  This  is a part of proprietary
--  software. All rights reserved.
--
--  Purpose :      Persistent streams (interface)
--
--  Author :       Dmitry A. Kazakov
--
--  Language :     Ada 2005
--
--  Environment :  Any
--
with Ada.Streams;                use Ada.Streams;
with Persistent.Blocking_Files;  use Persistent.Blocking_Files;

with Persistent.Blocking_Files.Transactional;
use  Persistent.Blocking_Files.Transactional;

package Persistent.Streams is
--
-- Stream_Position -- Monotonically ascending Position within the stream
--
   type Stream_Position is new Byte_Index;
--
-- Persistent_Stream -- The stream backed by a file. When written  items
--                      are added  at end  of the file.  When read items
-- are taken from the beginning.  When  Dispose is called the items read
-- from  the stream  are removed from  the file  and  the freed space is
-- reused.
--
   type Persistent_Stream is
      new Ada.Streams.Root_Stream_Type with private;
--
-- Close -- Close the file (if any)
--
--    Stream - The persistent stream
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Close (Stream : in out Persistent_Stream);
--
-- Dispose -- Remove all read content
--
--    Stream   - The FIFO
--  [ Upto   ] - The first position to keep in the stream
--
-- When Position is not specified it the current read position.
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Dispose (Stream : in out Persistent_Stream);
   procedure Dispose
             (  Stream : in out Persistent_Stream;
                Upto   : Stream_Position
             );
--
-- End_Of -- Check for stream end
--
--    Stream - The persistent stream
--
-- Returns :
--
--    True if reading the stream reached the end
--
   function End_Of (Stream : Persistent_Stream) return Boolean;
--
-- Erase -- Remove all conntent
--
--    Stream - The persistent stream
--
   procedure Erase (Stream : in out Persistent_Stream);
--
-- Flush -- Write file updates
--
--    Stream - The persistent stream
--
-- This procedure  commits latest changes made by Dispose and Write.  If
-- the file is not closed the changes are discarded.
--
   procedure Flush (Stream : in out Persistent_Stream);
--
-- Get_Block_Size -- Size of the file in blocks
--
--    Stream - The persistent stream
--
-- This function  returns  the number  of allocated blocks.  It includes
-- unused blocks but not the blocks reserved for maintaining redundancy.
--
-- Returns :
--
--    Number of FIFO blocks
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Get_Block_Size
            (  Stream : Persistent_Stream
            )  return Block_Count;
--
-- Get_Blocks -- The number of free blocks
--
--    Stream          - The FIFO
--    Total_Blocks    - The total number of physical blocks used
--    Disposed_Blocks - Physical blocks usable in a next transaction
--    Free_Blocks     - Physical blocks available to reuse
--    Logical_Blocks  - Logical blocks allocated for the stream
--    Writable_Blocks - Logical stream blocks available to rewrite
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   procedure Get_Blocks
             (  Stream          : Persistent_Stream;
                Total_Blocks    : out Block_Count;
                Disposed_Blocks : out Block_Count;
                Free_Blocks     : out Block_Count;
                Logical_Blocks  : out Block_Count;
                Writable_Blocks : out Block_Count
             );
--
-- Get_Name -- File name
--
--    Stream - The persistent stream
--
-- Returns :
--
--    The file name
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Get_Name (Stream : Persistent_Stream) return String;
--
-- Get_First_Index -- The logical index of the first element in
--
--    Stream - The persistent stream
--
-- Returns :
--
--    The first index of the FIFO
--
-- Exceptions :
--
--    Use_Error - No file open
--
   function Get_First_Index (Stream : Persistent_Stream)
      return Byte_Index;
--
-- Get_First_Position -- The position of the first element in
--
--    Stream - The persistent stream
--
-- Returns :
--
--    The first position of the FIFO
--
-- Exceptions :
--
--    Use_Error - No file open
--
   function Get_First_Position (Stream : Persistent_Stream)
      return Stream_Position;
--
-- Get_Read_Index -- The logical index of the first element to read
--
--    Stream - The persistent stream
--
-- Returns :
--
--    The first index of to reat at
--
-- Exceptions :
--
--    Use_Error - No file open
--
   function Get_Read_Index (Stream : Persistent_Stream)
      return Byte_Index;
--
-- Get_Read_Position -- The position of the first element to read
--
--    Stream - The persistent stream
--
-- Returns :
--
--    The first position to read at
--
-- Exceptions :
--
--    Use_Error - No file open
--
   function Get_Read_Position (Stream : Persistent_Stream)
      return Stream_Position;
--
-- Get_Size -- The number of element in the stream
--
--    Stream - The persistent stream
--
-- Returns :
--
--    The number stream elements in the stream
--
-- Exceptions :
--
--    Use_Error - No file open
--
   function Get_Size (Stream : Persistent_Stream)
      return Stream_Position;
--
-- Get_Write_Index -- The logical index of the first element to write
--
--    Stream - The persistent stream
--
-- Returns :
--
--    The first index to write
--
-- Exceptions :
--
--    Use_Error - No file open
--
   function Get_Write_Index (Stream : Persistent_Stream)
      return Byte_Index;
--
-- Get_Write_Position -- The position of the first element to write
--
--    Stream - The persistent stream
--
-- Returns :
--
--    The first position to write
--
-- Exceptions :
--
--    Use_Error - No file open
--
   function Get_Write_Position (Stream : Persistent_Stream)
      return Stream_Position;
--
-- Is_Empty -- The stream is empty
--
--    Stream - The persistent stream
--
-- Returns :
--
--    True if the stream is empty
--
-- Exceptions :
--
--    Use_Error - No file open
--
   function Is_Empty (Stream : Persistent_Stream) return Boolean;
--
-- Is_Open -- Check if the file is open
--
--    Stream - The persistent stream
--
-- Returns :
--
--    True if the file is open
--
   function Is_Open (Stream : Persistent_Stream) return Boolean;
--
-- Open -- Open file
--
--    Stream    - The persistent stream
--    Name      - File name
--    Mode      - File access mode
--    Hash_Size - Number of blocks kept stored in the memory
--    Form      - Additional system-dependent string for open
--
-- This  procedure  opens  a file.  Name is  of the  file to open.  When
-- missing  a temp file is created. Has_Size  specifies  the  number  of
-- blocks kept resident in the memory. Mode is the access mode.
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Open
             (  Stream    : in out Persistent_Stream;
                Name      : String;
                Mode      : Access_Mode := Read_Mode;
                Hash_Size : Positive    := 256;
                Form      : String      := ""
             );
--
-- Rewind -- Bring reading position back to the FIFO start
--
--    Stream - The persistent stream
--
-- Exceptions :
--
--    Use_Error - The file is not open
--
   procedure Rewind (Stream : in out Persistent_Stream);

private
   Header_Size    : constant := 5 * 8;
   Payload_Offset : constant := 8;

   type Persistent_Stream_Ptr is access all Persistent_Stream'Class;
   type Persistent_Stream is
      new Ada.Streams.Root_Stream_Type with
   record
      Self          : Persistent_Stream_Ptr :=
                         Persistent_Stream'Unchecked_Access;
      File          : aliased Persistent_Transactional_Array;
      In_Index      : Byte_Index      := Header_Size + Payload_Offset;
      In_Position   : Stream_Position := 0;
      Out_Index     : Byte_Index      := Header_Size + Payload_Offset;
      Out_Position  : Stream_Position := 0;
      Free          : Byte_Index      := 0;
      Read_Index    : Byte_Index      := 0;
      Read_Position : Stream_Position := 0;
   end record;
   procedure Read
             (  Stream : in out Persistent_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
   procedure Write
             (  Stream : in out Persistent_Stream;
                Item   : Stream_Element_Array
             );

   function Get
            (  Block : Block_Type;
               Index : Block_Offset
            )  return Byte_Index;
   function Get
            (  Block : Block_Type;
               Index : Block_Offset
            )  return Stream_Position;
   function Get_Next
            (  Block : Block_Type;
               Index : Byte_Index
            )  return Byte_Index;

   procedure Put
             (  Block : in out Block_Type;
                Index : Block_Offset;
                Value : Byte_Index
             );
   procedure Put
             (  Block : in out Block_Type;
                Index : Block_Offset;
                Value : Stream_Position
             );
   procedure Put_Next
             (  Block : in out Block_Type;
                Index : Byte_Index;
                Next  : Byte_Index
             );
end Persistent.Streams;
