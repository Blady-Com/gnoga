--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Blocking_Files                   Luebeck            --
--  Interface                                      Winter, 2014       --
--                                                                    --
--                                Last revision :  10:05 22 Nov 2014  --
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
--  This  package provides the type Persistent_Array which is a proxy to
--  a direct I/O file of fixed size blocks. The blocks are read into the
--  memory on demand using byte index Block_Index.
--
--  Note that the implementation is not task-safe.
--
with Interfaces;  use Interfaces;

with Ada.Direct_IO;
with Ada.Finalization;

package Persistent.Blocking_Files is
   Byte_Offset_Bits : constant := 10 + 3; -- 8K blocks
   Block_Byte_Size  : constant := 2 ** Byte_Offset_Bits;
--
-- Byte_Index -- Specifies a byte within the file
--
   type Byte_Index is mod 2 ** 64;
--
-- Block_Offset -- Specifies byte within a block
--
   type Block_Offset is mod Block_Byte_Size;
--
-- Block_Type -- Array element
--
   type Byte_Array is array (Block_Offset range <>) of Unsigned_8;
   for Byte_Array'Component_Size use 8;
   subtype Block_Type is Byte_Array (Block_Offset'Range);
   pragma Assert (Block_Type'Size = Block_Byte_Size);

   type Block_Type_Ptr is access all Block_Type;
   type Block_Type_Ref is access constant Block_Type;

   package File_IO is new Ada.Direct_IO (Block_Type);
--
-- Block_Count -- File block count
--
   type Block_Count is new File_IO.Count;
   subtype Block_Index is Block_Count range 1..Block_Count'Last;
--
-- Access_Mode -- File access mode
--
--    Read_Mode       - Read only access
--    Read_Write_Mode - Mutable access
--    Create_Mode     - Creating new file if none present
--
   type Access_Mode is (Read_Mode, Read_Write_Mode, Create_Mode);
--
-- Persistent_Array -- Array of blocks stored in a file
--
   type Persistent_Array is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Close -- Close the file (if any)
--
--    Container - The array
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Close (Container : in out Persistent_Array);
--
-- Commit -- Store all changes made since last commit
--
--    Container - The array
--
-- The implementation does nothing
--
   procedure Commit (Container : in out Persistent_Array);
--
-- Compose -- Byte index from block index and offset
--
--    Block  - The number of the block
--    Offset - The offset within the block
--
-- Returns :
--
--    Byte index
--
   function Compose
            (  Block  : Block_Index;
               Offset : Block_Offset
            )  return Byte_Index;
--
-- Finalize -- Destructor
--
--    Container - The array
--
   procedure Finalize (Container : in out Persistent_Array);
--
-- Flush -- Write all updated blocks
--
--    Container - The array
--
-- Write all updated blocks to the file.
--
   procedure Flush (Container : in out Persistent_Array);
--
-- Get -- An existing block or allocate a new one
--
--    Container - The array
--    Index     - Byte index (within the block)
--
-- This function  returns a pointer to the block corresponding to Index.
-- When the block already is in the  file  the function is equivalent to
-- Update. Otherwise it allocates a new block.
--
-- Returns :
--
--    The block loaded into the memory
--
-- Exceptions :
--
--    Use_Error - No file open, read-only file
--    I/O exceptions
--
   function Get
            (  Container : access Persistent_Array;
               Index     : Byte_Index
            )  return Block_Type_Ptr;
--
-- Get_Block_Size -- Size of the file in blocks
--
--    Container - The array
--
-- Returns :
--
--    Number of file blocks
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Get_Block_Size
            (  Container : Persistent_Array
            )  return Block_Count;
--
-- Get_First -- Byte index of the first block's byte
--
--    Index - Byte index (within the block)
--
-- Returns :
--
--    Index to the block's beginning
--
   function Get_First (Index : Byte_Index) return Byte_Index;
--
-- Get_Index -- Block index
--
--    Index - Byte index (within the block)
--
-- Returns :
--
--    The file block index corresponding to Index
--
   function Get_Index (Index : Byte_Index) return Block_Index;
--
-- Get_Name -- File name
--
--    Container - The array
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
   function Get_Name (Container : Persistent_Array) return String;
--
-- Get_Offset -- Byte offset within a block
--
--    Index - Byte index (within the block)
--
-- Returns :
--
--    The offset within the block when loaded into the memory
--
   function Get_Offset (Index : Byte_Index) return Block_Offset;
--
-- Get_Size -- Byte size of the file
--
--    Container - The array
--
-- Returns :
--
--    Usable size of the file in bytes
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Get_Size (Container : Persistent_Array) return Byte_Index;
--
-- Is_Open -- Check if the file is open
--
--    Container - The array
--
-- Returns :
--
--    True if the file is open
--
   function Is_Open (Container : Persistent_Array) return Boolean;
--
-- Is_Resident -- Check if the block is memory-resident
--
--    Container - The array
--    Index     - Of the element or block
--
-- Returns :
--
--    True if the block is in the memory
--
   function Is_Resident
            (  Container : Persistent_Array;
               Index     : Byte_Index
            )  return Boolean;
   function Is_Resident
            (  Container : Persistent_Array;
               Index     : Block_Index
            )  return Boolean;
--
-- Is_Writable -- Check if the file is writable
--
--    Container - The array
--
-- Returns :
--
--    True if the file can be written
--
   function Is_Writable (Container : Persistent_Array) return Boolean;
--
-- Load -- Load a block into memory
--
--    Container - The array
--    Index     - Byte index (within the block)
--
-- This function  returns a pointer to the block corresponding to Index.
-- If the file  does not have  a block containing  Index,  the result is
-- null.
--
-- Returns :
--
--    The block loaded into the memory or null
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Load
            (  Container : access Persistent_Array;
               Index     : Byte_Index
            )  return Block_Type_Ref;
--
-- Open -- Open file
--
--    Container - The array
--  [ Name      - File name
--    Mode ]    - File access mode
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
             (  Container : in out Persistent_Array;
                Name      : String;
                Mode      : Access_Mode := Read_Mode;
                Hash_Size : Positive    := 256;
                Form      : String      := ""
             );
   procedure Open
             (  Container : in out Persistent_Array;
                Hash_Size : Positive := 256;
                Form      : String   := ""
             );
--
-- Read -- A block
--
--    Container - The array
--    Index     - Byte index (within the block)
--    Block     - The block contents
--
-- This procedure  reads a block corresponding to Index.  When the block
-- is memory resident,  its contents is taken from  the memory  and  the
-- block is removed from there in order to prevent duplicity.  End_Error
-- is propagated if the file does not have a block containing Index.
--
-- Exceptions :
--
--    End_Error - Out of file
--    Use_Error - No file open
--    I/O exceptions
--
   procedure Read
             (  Container : in out Persistent_Array;
                Index     : Byte_Index;
                Block     : out Block_Type
             );
--
-- Rollback -- Drop any changes made since last commit
--
--    Container - The array
--
-- The implementation does nothing
--
   procedure Rollback (Container : in out Persistent_Array);
--
-- Update -- Load a block into memory
--
--    Container - The array
--    Index     - Byte index (within the block)
--
-- This function  returns a pointer to the block corresponding to Index.
-- If the container does not have it, the result is null.
--
-- Returns :
--
--    The block loaded into the memory or null
--
-- Exceptions :
--
--    Use_Error - No file open, read-only file update attempted
--    I/O exceptions
--
   function Update
            (  Container : access Persistent_Array;
               Index     : Byte_Index
            )  return Block_Type_Ptr;
--
-- Write -- Write a block into memory
--
--    Container - The array
--    Index     - Byte index (within the block)
--    Block     - The new contents
--
-- This procedure  replaces the block corresponding to Index with Block.
-- If the block  was memory  resident  it  is  removed  from there.  The
-- procedure can be used to add new blocks.
--
-- Exceptions :
--
--    Use_Error - No file open, read-only file update attempted
--    I/O exceptions
--
   procedure Write
             (  Container : in out Persistent_Array;
                Index     : Byte_Index;
                Block     : Block_Type
             );
private
   pragma Inline (Compose);
   pragma Inline (Get_First);
   pragma Inline (Get_Index);
   pragma Inline (Get_Offset);
   pragma Inline (Get_Size);
   pragma Inline (Is_Open);
   pragma Inline (Is_Resident);
   pragma Inline (Is_Writable);
   pragma Inline (Load);
   pragma Inline (Update);

   use File_IO;

   type Cashed_Block is record
      Index   : Positive_Count;
      Used    : Boolean := False;
      Updated : Boolean := False;
      Data    : aliased Block_Type;
   end record;
   type Cashed_Block_Ptr is access all Cashed_Block;
   type Blocks_Array is array (Count range <>) of aliased Cashed_Block;
   type Blocks_Array_Ptr is access Blocks_Array;

   type Persistent_Array is
      new Ada.Finalization.Limited_Controlled with
   record
      File    : File_Type;
      Is_Open : Boolean := False;
      Size    : Count   := 0;
      Buffer  : Blocks_Array_Ptr; -- Memory-resident blocks
   end record;
--
-- Load -- Read block into the buffer
--
--    Container  - The array
--    No         - Of the file block
--    Block      - Null if the block is not in the file
--
-- This procedure reads block into the memory if it is not alread read.
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   procedure Load
             (  Container : in out Persistent_Array;
                No        : Count;
                Block     : out Cashed_Block_Ptr
             );

end Persistent.Blocking_Files;
