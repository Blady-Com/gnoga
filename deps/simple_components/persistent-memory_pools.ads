--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Memory_Pools                     Luebeck            --
--  Interface                                      Winter, 2014       --
--                                                                    --
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
--
--  This  package provides  a memory pool  resident in a blocking access
--  file based on Persistent_Array.
--
with Persistent.Blocking_Files;  use Persistent.Blocking_Files;

with Ada.Finalization;
with Ada.Task_Identification;
with Interfaces;

package Persistent.Memory_Pools is
--
-- Byte_Count -- Bytes to allocate
--
   Min_Bits : constant := 5;
   Min_Size : constant := 2 ** Min_Bits; -- 32, minimal block size
   Max_Size : constant := Block_Byte_Size;
   subtype Byte_Count is Block_Offset range 0..Block_Offset'Last - 3;
--
-- Root_Index -- Root block indices
--
   type Root_Index is range 1..16;
--
-- Persistent_Pool -- Memory pool resident in a file
--
--    File - The file where the pool resides
--
   type Persistent_Pool (File : access Persistent_Array'Class) is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Allocate -- Memory block
--
--    Pool - Memory pool
--    Size - Bytes to allocate
--
-- Returns :
--
--    Index of the first byte of the allocated block
--
-- Exceptions :
--
--    Use_Error - No file open, the file is open read-only
--    I/O exceptions
--
   function Allocate
            (  Pool : Persistent_Pool;
               Size : Byte_Count
            )  return Byte_Index;
--
-- Commit -- Transaction
--
--    Container - Memory pool
--
-- This procedure commits and thus opens a new transaction.
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Commit (Container : in out Persistent_Pool);
--
-- Deallocate -- Memory block
--
--    Pool  - Memory pool
--    Index - Of the memory block to free
--
-- Exceptions :
--
--    Use_Error - No file open, the file is open read-only
--    I/O exceptions
--
   procedure Deallocate
             (  Pool  : in out Persistent_Pool;
                Index : Byte_Index
             );
--
-- Expand -- Memory block
--
--    Pool  - Memory pool
--    Index - Of the memory block to expand
--
-- This function expands  the memory block at  Index  when there is free
-- space beyond the block.  The result  is the number  of bytes by which
-- the block was expanded.
--
-- Returns :
--
--    Number of bytes obtained
--
-- Exceptions :
--
--    Use_Error - No file open, the file is open read-only
--    I/O exceptions
--
   function Expand
            (  Pool  : Persistent_Pool;
               Index : Byte_Index
            )  return Byte_Count;
--
-- Fetch -- Allocate a memory block eagerly
--
--    Pool  - Memory pool
--    Size  - Least size to allocate
--
-- This  function allocates at least Size bytes. It tries to allocate as
-- much space as possible. When a fitting block is found it is allocated
-- full.  The  actual  size of the allocated block can be obtained using
-- Get_Size.
--
-- Returns :
--
--    Index of the first byte of the allocated block
--
-- Exceptions :
--
--    Use_Error - No file open, the file is open read-only
--    I/O exceptions
--
   function Fetch
            (  Pool : Persistent_Pool;
               Size : Byte_Count
            )  return Byte_Index;
--
-- Finalize -- Destructor
--
--    Pool - Memory pool
--
   procedure Finalize (Pool : in out Persistent_Pool);
--
-- Flush -- Write all updated blocks
--
--    Pool - Memory pool
--
-- Write all updated blocks and other cached data into the file.
--
   procedure Flush (Pool : in out Persistent_Pool);
--
-- Get_Block_Size -- Total byte size of a memory block
--
--    Pool  - Memory pool
--    Index - Of the memory block
--
-- Returns :
--
--    Byte size of the block including its margins
--
-- Exceptions :
--
--    Use_Error - No file open, wrong index
--    I/O exceptions
--
   function Get_Block_Size
            (  Pool  : Persistent_Pool;
               Index : Byte_Index
            )  return Byte_Count;
--
-- Get_Blocks_Free -- Blocks free
--
--    Pool - Memory pool
--
-- Returns :
--
--   Number of unused memory blocks in the pool
--
   function Get_Blocks_Free (Pool : Persistent_Pool) return Block_Count;
--
-- Get_Blocks_Used -- Blocks containing allocated data
--
--    Pool - Memory pool
--
-- Returns :
--
--   Number of allocated blocks in the pool
--
   function Get_Blocks_Used (Pool : Persistent_Pool) return Block_Count;
--
-- Get_Bytes_Free -- Bytes free in the pool
--
--    Pool - Memory pool
--
-- Returns :
--
--   Number of free bytes in the pool
--
   function Get_Bytes_Free (Pool : Persistent_Pool) return Byte_Index;
--
-- Get_Bytes_Used -- Bytes allocated in the pool
--
--    Pool - Memory pool
--
-- Returns :
--
--   Number of bytes allocated in the pool
--
   function Get_Bytes_Used (Pool : Persistent_Pool) return Byte_Index;
--
-- Get_Name -- File name
--
--    Pool - Memory pool
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
   function Get_Name (Pool : Persistent_Pool) return String;
--
-- Get_Root_Index -- File name
--
--    Pool  - Memory pool
--    Index - Of the root index to get
--
-- Root indices are used to keep user information, e.g. the index of the
-- master block allocated in the pool.
--
-- Returns :
--
--    The root index
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Get_Root_Index
            (  Pool  : Persistent_Pool;
               Index : Root_Index
            )  return Byte_Index;
--
-- Get_Size -- Byte size of a memory block
--
--    Pool  - Memory pool
--    Index - Of the memory block
--
-- This function  returns  the number of bytes  available  to use in the
-- block pointed by Index.
--
-- Returns :
--
--    Byte size of the block excluding its margins
--
-- Exceptions :
--
--    Use_Error - No file open, wrong index
--    I/O exceptions
--
   function Get_Size
            (  Pool  : Persistent_Pool;
               Index : Byte_Index
            )  return Byte_Count;
--
-- Get_Space -- Byte space in the file
--
--    Pool - Memory pool
--
-- Returns :
--
--    Byte space available to allocate in the file
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Get_Space (Pool : Persistent_Pool) return Byte_Index;
--
-- Initialize -- Constructor
--
--    Pool - Memory pool
--
-- This procedure must be called when overridden.
--
-- Exceptions :
--
--    Use_Error - The file is not open
--    I/O exceptions
--
   procedure Initialize (Pool : in out Persistent_Pool);
--
-- Is_Open -- Check if the file is open
--
--    Pool - Memory pool
--
-- Returns :
--
--    True if the file is open
--
   function Is_Open (Pool : Persistent_Pool) return Boolean;
--
-- Set_Root_Index -- File name
--
--    Pool  - Memory pool
--    Index - Of the root index to set
--    Value - The value of the root index
--
-- Root indices are used to keep user information, e.g. the index of the
-- master block allocated in the pool.
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   procedure Set_Root_Index
             (  Pool  : in out Persistent_Pool;
                Index : Root_Index;
                Value : Byte_Index
             );
--
-- Truncate -- Memory block
--
--    Pool  - Memory pool
--    Index - Of the memory block to free
--    Size  - New usable space of the block
--
-- This procedure  truncates the memory block pointed by Index to lesser
-- size. Nothing happens if the new size is greater than the current one
-- or when the freed space is less than minimal possible size.
--
-- Exceptions :
--
--    Use_Error - No file open, wrong index, the file is open read-only
--    I/O exceptions
--
   procedure Truncate
             (  Pool  : in out Persistent_Pool;
                Index : Byte_Index;
                Size  : Byte_Count
             );
------------------------------------------------------------------------
--
-- Holder -- A pool  holder  object.  This  is  a  helper  object  which
--           ensures  safe  concurrent  access to the file used with the
--           pool.
--
--    Pool - A pointer to the pool
--
   type Holder (Pool : access Persistent_Pool'Class) is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Finalize -- Destruction
--
-- This  procedure releases the file access mutex. It shall be called by
-- the derived type if overridden.
--
   procedure Finalize (Object : in out Holder);
--
-- Initialize -- Construction
--
-- This  procedure  seizes  the file access mutex. It shall be called by
-- the derived type if overridden.
--
   procedure Initialize (Object : in out Holder);

private
   use Ada.Task_Identification;
   use Interfaces;

   pragma Inline (Get_Blocks_Free);
   pragma Inline (Get_Blocks_Used);
   pragma Inline (Get_Bytes_Free);
   pragma Inline (Get_Bytes_Used);
   pragma Inline (Get_Root_Index);
   pragma Inline (Get_Size);
   pragma Inline (Is_Open);
   pragma Inline (Set_Root_Index);
   pragma Assert
          (  (Integer (Byte_Count'Last) + Min_Size - 1) / Min_Size
          <= 16#3FFF#
          );
   type Block_Size_Index is
      range 1..(Max_Size + Min_Size - 1) / Min_Size;

   type Holder (Pool : access Persistent_Pool'Class) is
      new Ada.Finalization.Limited_Controlled with null record;

   protected type Persistent_Mutex
                  (  Pool : access Persistent_Pool'Class
                  )  is
      function Get_Blocks_Free return Byte_Index;
      function Get_Blocks_Used return Byte_Index;
      function Get_Bytes_Free  return Byte_Index;
      function Get_Bytes_Used  return Byte_Index;
      function Get_Root_Index (Index : Root_Index) return Byte_Index;
      procedure Release;
      procedure Set_Root_Index (Index : Root_Index; Value : Byte_Index);
      entry Seize;
   private
      entry Lounge;
      Owner : Task_ID := Null_Task_ID;
      Count : Natural := 0;
   end Persistent_Mutex;

   type Free_Block_Lists is array (Block_Size_Index) of Byte_Index;
   type Root_Indices is array (Root_Index) of Byte_Index;
   type Persistent_Pool (File : access Persistent_Array'Class) is
      new Ada.Finalization.Limited_Controlled with
   record
      Bytes_Used  : Byte_Index := 0;
      Bytes_Free  : Byte_Index := 0;
      Blocks_Used : Byte_Index := 0;
      Blocks_Free : Byte_Index := 0;
      Mutex : aliased Persistent_Mutex
                      (  Persistent_Pool'Unchecked_Access
                      );
      Free  : Free_Block_Lists := (others => 0);
      Root  : Root_Indices     := (others => 0);
   end record;
--
-- Add -- Add an unused memory block to the list of free blocks
--
--    Pool  - Memory pool
--    Index - Index of the block
--    Size  - Memory block size (including margins)
--
-- The block is merged with adjacent free blocks if necessary.
--
   procedure Add
             (  Pool  : in out Persistent_Pool;
                Index : Byte_Index;
                Size  : Unsigned_16
             );
--
-- Fetch -- Remove block from the list of free blocks
--
--    Pool   - Memory pool
--    Block  - The file block
--    Size   - Memory block size (including the margins)
--    Offset - To the block
--
-- The block  margins are not touched.  The counters  of free  bytes and
-- free blocks are decreased.
--
-- !!WARNING!! Block may become invalid
--
   procedure Fetch
             (  Pool   : in out Persistent_Pool;
                Block  : in out Block_Type;
                Size   : Unsigned_16;
                Offset : Block_Offset
             );
--
-- Insert -- Add an unused memory block to the list of free blocks
--
--    Pool  - Memory pool
--    List  - Of free blocks
--    Index - The memory block
--
   procedure Insert
             (  Pool  : in out Persistent_Pool;
                List  : in out Byte_Index;
                Index : Byte_Index
             );
--
-- Remove -- Remove block from the list of free blocks
--
--    Pool   - Memory pool
--    Block  - The file block
--    List   - Of free blocks
--    Offset - To the block
--
-- !!WARNING!! Block may become invalid
--
   procedure Remove
             (  Pool   : in out Persistent_Pool;
                Block  : in out Block_Type;
                List   : in out Byte_Index;
                Offset : Block_Offset
             );
--
-- Get -- Get value at the offset
--
--    Block  - The file block
--    Offset - The offset to the memory block
--
-- Returns :
--
--    The value
--
   function Get
            (  Block  : Byte_Array;
               Offset : Block_Offset
            )  return Byte_Index;
   function Get
            (  Block  : Byte_Array;
               Offset : Block_Offset
            )  return Unsigned_16;
   function Get
            (  Block  : Byte_Array;
               Offset : Block_Offset
            )  return Unsigned_64;
--
-- Get_Size -- Get block size
--
--    Block  - The file block
--    Offset - The offset to the memory block
--
-- Returns :
--
--    The block size (including margins)
--
   function Get_Size
            (  Block  : Block_Type;
               Offset : Block_Offset
            )  return Unsigned_16;
--
-- Mark -- Set memory block margins
--
--    Block  - The file block
--    Offset - To the first used byte of the block
--    Size   - The block size (including margins)
--    Free   - True if the block is free
--
   procedure Mark
             (  Block  : in out Block_Type;
                Offset : Block_Offset;
                Size   : Unsigned_16;
                Free   : Boolean
             );
--
-- Mark_Chained -- Mark block chained in its margins
--
--    Block  - The file block
--    Offset - To the first used byte of the block
--    Size   - The block size (including margins)
--
   procedure Mark_Chained
             (  Block  : in out Block_Type;
                Offset : Block_Offset;
                Size   : Unsigned_16
             );
--
-- Mark_Unchained -- Mark block not chained in its margins
--
--    Block  - The file block
--    Offset - To the first used byte of the block
--    Size   - The block size (including margins)
--
   procedure Mark_Unchained
             (  Block  : in out Block_Type;
                Offset : Block_Offset;
                Size   : Unsigned_16
             );
--
-- Is_Chained -- Check if the memory block is chained
--
--    Block  - The file block
--    Offset - The offset to the memory block
--
-- Returns :
--
--    True if the block is in a list of blocks
--
   function Is_Chained
            (  Block  : Block_Type;
               Offset : Block_Offset
            )  return Boolean;
--
-- Is_Free -- Check if the memory block is free
--
--    Block  - The file block
--    Offset - The offset to the memory block
--
-- Returns :
--
--    True if the block is marked free
--
   function Is_Free
            (  Block  : Block_Type;
               Offset : Block_Offset
            )  return Boolean;
--
-- Put -- Put value at the offset
--
--    Block  - The file block
--    Offset - The offset to the memory block
--    Value  - The value
--
   procedure Put
             (  Block  : in out Byte_Array;
                Offset : Block_Offset;
                Value  : Byte_Index
             );
   procedure Put
             (  Block  : in out Byte_Array;
                Offset : Block_Offset;
                Value  : Unsigned_16
             );
   procedure Put
             (  Block  : in out Byte_Array;
                Offset : Block_Offset;
                Value  : Unsigned_64
             );
--
-- Unchecked_Allocate -- Memory block
--
--    Pool - Memory pool
--    Size - Bytes to allocate
--
-- Returns :
--
--    Index of the first byte of the allocated block
--
-- Exceptions :
--
--    Use_Error - No file open, the file is open read-only
--    I/O exceptions
--
   function Unchecked_Allocate
            (  Pool : Persistent_Pool;
               Size : Byte_Count
            )  return Byte_Index;
--
-- Unchecked_Close -- Close the file (if any)
--
--    Pool - Memory pool
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Unchecked_Close (Pool : in out Persistent_Pool);
--
-- Unchecked_Deallocate -- Memory block
--
--    Pool  - Memory pool
--    Index - Of the memory block to free
--
-- Exceptions :
--
--    Use_Error - No file open, the file is open read-only
--    I/O exceptions
--
   procedure Unchecked_Deallocate
             (  Pool  : in out Persistent_Pool;
                Index : Byte_Index
             );
--
-- Unchecked_Expand -- Memory block
--
--    Pool  - Memory pool
--    Index - Of the memory block to expand
--
-- This function expands  the memory block at  Index  when there is free
-- space beyond the block.  The result  is the number  of bytes by which
-- the block was expanded.
--
-- Returns :
--
--    Number of bytes obtained
--
-- Exceptions :
--
--    Use_Error - No file open, the file is open read-only
--    I/O exceptions
--
   function Unchecked_Expand
            (  Pool  : Persistent_Pool;
               Index : Byte_Index
            )  return Byte_Count;
--
-- Unchecked_Fetch -- Allocate a memory block eagerly
--
--    Pool  - Memory pool
--    Size  - Least size to allocate
--
-- This  function allocates at least Size bytes. It tries to allocate as
-- much space as possible. When a fitting block is found it is allocated
-- full.  The  actual  size of the allocated block can be obtained using
-- Get_Size.
--
-- Returns :
--
--    Index of the first byte of the allocated block
--
-- Exceptions :
--
--    Use_Error - No file open, the file is open read-only
--    I/O exceptions
--
   function Unchecked_Fetch
            (  Pool : Persistent_Pool;
               Size : Byte_Count
            )  return Byte_Index;
--
-- Unchecked_Flush -- Write all updated blocks
--
--    Pool - Memory pool
--
-- Write all updated blocks and other cached data into the file.
--
   procedure Unchecked_Flush (Pool : in out Persistent_Pool);
--
-- Unchecked_Truncate -- Memory block
--
--    Pool  - Memory pool
--    Index - Of the memory block to free
--    Size  - New usable space of the block (without margins)
--
-- This procedure  truncates the memory block pointed by Index to lesser
-- size. Nothing happens if the new size is greater than the current one
-- or when the freed space is less than minimal possible size.
--
-- Exceptions :
--
--    Use_Error - No file open, wrong index, the file is open read-only
--    I/O exceptions
--
   procedure Unchecked_Truncate
             (  Pool  : in out Persistent_Pool;
                Index : Byte_Index;
                Size  : Byte_Count
             );

   pragma Inline (Allocate);
   pragma Inline (Deallocate);
   pragma Inline (Expand);
   pragma Inline (Fetch);
   pragma Inline (Get);
   pragma Inline (Get_Size);
   pragma Inline (Insert);
   pragma Inline (Is_Chained);
   pragma Inline (Is_Free);
   pragma Inline (Mark);
   pragma Inline (Mark_Chained);
   pragma Inline (Mark_Unchained);
   pragma Inline (Put);
   pragma Inline (Remove);
   pragma Inline (Truncate);

   Tail_Size   : constant := (  (  (  Block_Byte_Size
                                   -  (  40
                                      +  8 * Free_Block_Lists'Length
                                      +  8 * Root_Indices'Length
                                   )  )
                                /  Min_Size
                                )
                             *  Min_Size
                             );
   Head_Size   : constant := Block_Byte_Size - Tail_Size;
   First_Block : constant := Head_Size + 2;

end Persistent.Memory_Pools;
