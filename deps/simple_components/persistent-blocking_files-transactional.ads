--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Blocking_Files.                  Luebeck            --
--        Transactional                            Spring, 2014       --
--  Interface                                                         --
--                                Last revision :  11:02 11 Apr 2021  --
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
--  This  package   provides  the  type  Persistent_Transactional_Array.
--  It is derived from  Persistent_Array and  provides transactional I/O
--  safe against computer crash.
--
--  Note that the implementation is not task-safe.
--
with Ada.Unchecked_Deallocation;

package Persistent.Blocking_Files.Transactional is
--
-- Persistent_Transactional_Array -- Array with transactions
--
   type Persistent_Transactional_Array is
      new Persistent_Array with private;
--
-- Close -- Close the file (if any)
--
--    Container - The array
--
-- Any non-committed changes are discarded.
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Close
             (  Container : in out Persistent_Transactional_Array
             );
--
-- Commit -- Transaction
--
--    Container - The array
--
-- This procedure commits and thus opens a new transaction.
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Commit (Container : in out Persistent_Transactional_Array);
--
-- Finalize -- Destructor
--
--    Container - The array
--
-- If the file still open it is closed.
--
   procedure Finalize
             (  Container : in out Persistent_Transactional_Array
             );
--
-- Flush -- Write all updated blocks
--
--    Container - The array
--
-- Write all updated blocks to the file. All changes made are committed.
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Flush (Container : in out Persistent_Transactional_Array);
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
            (  Container : access Persistent_Transactional_Array;
               Index     : Byte_Index
            )  return Block_Type_Ptr;
--
-- Get_Allocated_Size -- Total byte size of the file
--
--    Container - The array
--
-- Returns :
--
--    The total file size in bytes
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Get_Allocated_Size
            (  Container : Persistent_Transactional_Array
            )  return Byte_Index;
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
            (  Container : Persistent_Transactional_Array
            )  return Block_Count;
--
-- Get_Disposed_Blocks -- The number of disposed blocks
--
--    Container - The array
--
-- Disposed blocks  cannot be reused  during current  transaction.  They
-- become  available  when  the  transaction  is  committed.   When  the
-- transaction is rolled back the disposed blocks become used.
--
-- Returns :
--
--    Disposed blocks number
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Get_Disposed_Blocks
            (  Container : Persistent_Transactional_Array
            )  return Block_Count;
--
-- Get_Free_Blocks -- The number of free blocks
--
--    Container - The array
--
-- Free blocks can be reused during current transaction.  When there  is
-- no free blocks new blocks are acquired by expanding the file.
--
-- Returns :
--
--    Free blocks number
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Get_Free_Blocks
            (  Container : Persistent_Transactional_Array
            )  return Block_Count;
--
-- Get_Map_Depth -- The depth of the virtual to physical block map
--
--    Container - The array
--
-- Returns :
--
--    The depth of the map
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Get_Map_Depth
            (  Container : Persistent_Transactional_Array
            )  return Natural;
--
-- Get_Map_Size -- The number of segments need to map all address space
--
--    Size - The maximal address
--
-- Returns :
--
--    The number of mapping segments needed
--
   function Get_Map_Size (Size : Byte_Index) return Natural;
--
-- Get_Physical -- Map virtual to physical block
--
--    Container - The array
--    Virtual   - The virtual block number 1..
--
-- Returns :
--
--    The corresponding physical block number in the file or 0
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Get_Physical
            (  Container : Persistent_Transactional_Array;
               Virtual   : Block_Index
            )  return Block_Count;
--
-- Get_Sequence_No -- The transaction sequence number
--
--    Container - The array
--
-- Each transaction has an unique sequence number.
--
-- Returns :
--
--    The sequence number
--
-- Exceptions :
--
--    Use_Error - No file open
--
   function Get_Sequence_No
            (  Container : Persistent_Transactional_Array
            )  return Unsigned_64;
--
-- Get_Size -- Byte size of the file
--
--    Container - The array
--
-- This function returns the virtual file size.  This is  the  number of
-- bytes that can be used to store data without extending the file.
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
   function Get_Size
            (  Container : Persistent_Transactional_Array
            )  return Byte_Index;
--
-- Get_Used_Size -- Used byte size of the file
--
--    Container - The array
--
-- This function returns the total number of used file bytes.  It can be
-- less than  the total file size returned by Get_Allocated_Size.  It is
-- greater than the virtual file size returned by Get_Size.
--
-- Returns :
--
--    The used file size in bytes
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Get_Used_Size
            (  Container : Persistent_Transactional_Array
            )  return Byte_Index;
--
-- Is_Resident -- Check if the block is memory-resident
--
--    Container - The array
--    Index     - Of the element
--
-- Returns :
--
--    True if the block is in the memory
--
   function Is_Resident
            (  Container : Persistent_Transactional_Array;
               Index     : Byte_Index
            )  return Boolean;
   function Is_Resident
            (  Container : Persistent_Transactional_Array;
               Index     : Block_Index
            )  return Boolean;
--
-- Is_Updated -- Check if the block is updated
--
--    Container - The array
--    Virtual   - The virtual block number 1..
--
-- Updated  blocks  are   committed  or  rolled  back  upon  transaction
-- completion.
--
-- Returns :
--
--    True if the block is updated
--
-- Exceptions :
--
--    Use_Error - No file open
--    I/O exceptions
--
   function Is_Updated
            (  Container : Persistent_Transactional_Array;
               Virtual   : Block_Index
            )  return Boolean;
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
            (  Container : access Persistent_Transactional_Array;
               Index     : Byte_Index
            )  return Block_Type_Ref;
--
-- Open -- Open file
--
--    Container - The array
--  [ Name      - File name
--    Mode ]    - File access mode
--    Hash_Size - Number of blocks kept stored in the memory
--    Map_Size  - Number of virtual block map stored in the memory
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
             (  Container : in out Persistent_Transactional_Array;
                Name      : String;
                Mode      : Access_Mode := Read_Mode;
                Hash_Size : Positive    := 256;
                Map_Size  : Positive;
                Form      : String      := ""
             );
   procedure Open
             (  Container : in out Persistent_Transactional_Array;
                Name      : String;
                Mode      : Access_Mode := Read_Mode;
                Hash_Size : Positive    := 256;
                Form      : String      := ""
             );
   procedure Open
             (  Container : in out Persistent_Transactional_Array;
                Hash_Size : Positive := 256;
                Map_Size  : Positive;
                Form      : String   := ""
             );
   procedure Open
             (  Container : in out Persistent_Transactional_Array;
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
-- This procedure reads a block corresponding to Index.  When the  block
-- is memory  resident,  its contents  is taken from the memory  and the
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
             (  Container : in out Persistent_Transactional_Array;
                Index     : Byte_Index;
                Block     : out Block_Type
             );
--
-- Rollback -- Drop any changes made since last commit
--
--    Container - The array
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Rollback
             (  Container : in out Persistent_Transactional_Array
             );
--
-- Update -- Load a block into memory
--
--    Container - The array
--    Index     - Byte index (within the block)
--
-- This function returns a pointer  to the block corresponding to Index.
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
            (  Container : access Persistent_Transactional_Array;
               Index     : Byte_Index
            )  return Block_Type_Ptr;
--
-- Write -- Load a block into memory
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
             (  Container : in out Persistent_Transactional_Array;
                Index     : Byte_Index;
                Block     : Block_Type
             );
private
   Map_Size  : constant := (Block_Byte_Size - 8) / 9;
   Root_Size : constant := Map_Size - 3;

   type Map_Index is mod Map_Size;
   subtype Root_Map_Index is Map_Index range 0..Map_Index'Last - 4;
   type Block_Map    is array (Map_Index) of Block_Count;
   type Block_Update is array (Map_Index) of Boolean;
   type Map_Level is range 0..127;
--
-- Get_Lower -- Get the least block number in the same segment
--
--    Block - The virtual block number
--
-- Returns :
--
--    The least block number in the same segment
--
   function Get_Lower (Block : Block_Index) return Block_Index;
--
-- Get_Offset -- Get block offset in a map segment
--
--    Block    - The virtual block number
--  [ Height ] - The height of the map segment
--
-- Returns :
--
--    Index in the map
--
   function Get_Offset (Block : Block_Index) return Map_Index;
   function Get_Offset
            (  Block  : Block_Index;
               Height : Map_Level
            )  return Map_Index;
--
-- Get_Parent -- Get virtual block number of the parent map segment
--
--    Block    - The virtual block number
--  [ Height ] - The height of the map segment
--
-- Returns :
--
--    Virtual block number of the parent segment
--
   function Get_Parent (Block : Block_Index) return Block_Index;
   function Get_Parent
            (  Block  : Block_Index;
               Height : Map_Level
            )  return Block_Index;
--
-- Map_Segment -- Segment of the virtual to physical block map
--
--    Offset  Length Field    [ segments ]
--    ------  ------ -----
--     0000      8   Sequence number
--     0008      N   List of physical blocks
--      N*8     N/8  List of bits, 1 if virtual block is relocated
--
--    Offset  Length Field    [ root segment ]
--    ------  ------ -----
--     0000      2   Fletcher-16 check sum
--     0002      8   Sequence number
--     0010      8   Physical file size, number of physical blocks
--     0018      8   Virtual file size, number of virtual blocks
--     0020      8   Head of the list of free blocks (physical block)
--     0028      1   Height
--     0029   K=N-3  List of physical blocks
--    29+K*8   K/8   List of bits, 1 if virtual block is relocated
--
--  Virtual block index
--  [ ... | 0..Map_Size - 1 | ... | 0..Map_Size - 1 | 0..Map_Size - 1 ]
--            /                                        \
--  Root segment index                     Leaf segment index
--
   type Map_Segment is record
      Location  : Block_Index  := 1;     -- In the file (physical index)
      Lower     : Block_Index  := 1;     -- Lowest block index mapped
      Height    : Map_Level    := 0;     -- 0 - leaf, > 0 - tree node
      Used      : Boolean      := False; -- The segment is in use
      Modified  : Boolean      := False; -- The segment was modified
      Relocated : Boolean      := False; -- The segment was relocated
      Map       : Block_Map    := (others => 0);     -- Physical blocks
      Updated   : Block_Update := (others => False); -- True if block
   end record;                                       -- was relocated
   type Map_Segment_Ptr is access all Map_Segment;
   type Map_Segment_No is new Natural;
   type Map_Segments_Array is
      array (Map_Segment_No range <>) of aliased Map_Segment;
   type Map_Segments_Array_Ptr is access Map_Segments_Array;
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Map_Segments_Array,
             Map_Segments_Array_Ptr
          );

   Free_Size : constant := (Block_Byte_Size - 2 - 1 - 8 - 8 - 1) / 8;

   type Free_Count is range 0..Free_Size;
   subtype Free_Index is Free_Count range 1..Free_Count'Last;
   type Block_List is array (Free_Index) of Block_Count;
--
-- Free_Segment -- Segment of the list of free blocks
--
--    Offset  Length Field
--    ------  ------ -----
--     0000     8   Physical block index of the next element
--     0008     2   Length of the list
--     0010    rest  List of 8-bytes items
--
   type Free_Segment is record
      Location  : Block_Count := 0; -- In the file (physical index)
      Next      : Block_Count := 0; -- Next segment of the list
      Length    : Free_Count  := 0; -- List length
      List      : Block_List;       -- List of blocks
   end record;

   type Ptr is access all Persistent_Transactional_Array'Class;

   type Persistent_Transactional_Array is
      new Persistent_Array with
   record
      Self     : Ptr := Persistent_Transactional_Array'Unchecked_Access;
      Sequence : Unsigned_64 := 0;
      Length   : Block_Count := 0;       -- Physical file size
      Width    : Block_Count := 0;       -- Virtual file size
      First    : Block_Count := 0;       -- First disposed block
      Free     : Free_Segment;           -- Free blocks list head
      Disposed : Free_Segment;           -- Newly freed blocks list head
      List     : Map_Segments_Array_Ptr; -- Memory-resident segments
   end record;
--
-- Allocate -- Get a new physical block
--
--    Container - The array
--
-- Returns :
--
--    The physical block number
--
-- Exceptions :
--
--    Use_Error - No file open, read-only file update attempted
--    I/O exceptions
--
   function Allocate
            (  Container : Persistent_Transactional_Array
            )  return Block_Index;
--
-- Dispose -- Mark a physical block as free for the next transaction
--
--    Container - The array
--    Physical  - The block number to free
--
-- Exceptions :
--
--    Use_Error - No file open, read-only file update attempted
--    I/O exceptions
--
   procedure Dispose
             (  Container : in out Persistent_Transactional_Array;
                Physical  : Block_Index
             );
--
-- Expand -- Add a new virtual block
--
--    Container - The array
--
-- Returns :
--
--    The physical block number
--
-- Exceptions :
--
--    Use_Error - No file open, read-only file update attempted
--    I/O exceptions
--
   function Expand
            (  Container : Persistent_Transactional_Array
            )  return Block_Index;
--
-- Find -- Virtual block index in the map
--
--    Container - The array
--    Virtual   - The virtual block number
--    Height    - The map tree height, 0 corresponds to the leaf
--
-- Returns :
--
--    The map segment
--
   function Find
            (  Container : Persistent_Transactional_Array;
               Virtual   : Block_Index;
               Height    : Map_Level
            )  return Map_Segment_Ptr;
--
-- Get -- Get value the offset
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
            )  return Unsigned_16;
   function Get
            (  Block  : Byte_Array;
               Offset : Block_Offset
            )  return Unsigned_64;
   function Get
            (  Block  : Byte_Array;
               Offset : Block_Offset
            )  return Block_Count;
   function Get
            (  Block  : Byte_Array;
               Offset : Block_Offset
            )  return Free_Count;
--
-- Get_Segment_No -- Get map segment index by block number
--
--    Container - The array
--    Virtual   - The virtual block number
--
-- Returns :
--
--    Index in the map
--
   function Get_Segment_No
            (  Container : Persistent_Transactional_Array;
               Virtual   : Block_Index
            )  return Map_Segment_No;
--
-- Load -- Load free blocks list segment
--
--    Container - The array
--    Physical  - The block where the segment is located
--    Segment   - The free block list's segment to load
--
-- Exceptions :
--
--    Data_Error   - The block is not mapped
--    Status_Error - Wrong check sum
--
   procedure Load
             (  Container : in out Persistent_Transactional_Array;
                Physical  : Block_Index;
                Segment   : out Free_Segment
             );
--
-- Load -- Load segment of the virtual to physical block map
--
--    Container - The array
--    Physical  - The block where the segment is located
--    Lower     - The least mapped block in the segment
--    Height    - The map tree height, 0 corresponds to the leaf
--    Segment   - The segment to read the block into
--
-- Exceptions :
--
--    Data_Error - The block is not mapped
--
   procedure Load
             (  Container : in out Persistent_Transactional_Array;
                Physical  : Block_Index;
                Lower     : Block_Index;
                Height    : Map_Level;
                Segment   : out Map_Segment
             );
--
-- Master_Block_Data -- Root segment data
--
   type Master_Block_Data is record
      Valid    : Boolean := False;
      Segment  : Map_Segment;
      Sequence : Unsigned_64;
      Free     : Block_Count;
      Size     : Block_Count;
      Length   : Block_Count;
   end record;
--
-- Load -- Root segment of the virtual to physical block map
--
--    Container - The array
--    Physical  - The block where the segment is located
--    Data      - The master block data
--
-- Exceptions :
--
--    Data_Error   - The block is not mapped
--    Status_Error - Wrong check sum or corrupted
--
   procedure Load
             (  Container : in out Persistent_Transactional_Array;
                Physical  : Block_Index;
                Data      : out Master_Block_Data
             );
--
-- Map -- Virtual to physical block number conversion
--
--    Container - The array
--    Virtual   - The virtual block number
--
-- Returns :
--
--    The physical block number or 0 if not mapped
--
   function Map
            (  Container : Persistent_Transactional_Array;
               Virtual   : Block_Index
            )  return Block_Count;
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
                Value  : Unsigned_16
             );
   procedure Put
             (  Block  : in out Byte_Array;
                Offset : Block_Offset;
                Value  : Unsigned_64
             );
   procedure Put
             (  Block  : in out Byte_Array;
                Offset : Block_Offset;
                Value  : Block_Count
             );
   procedure Put
             (  Block  : in out Byte_Array;
                Offset : Block_Offset;
                Value  : Free_Count
             );
--
-- Store -- Segment of the virtual to physical block map
--
--    Container - The array
--    Segment   - The map segment to store
--    Recurse   - Store all updated segments
--
   procedure Store
             (  Container : in out Persistent_Transactional_Array;
                Segment   : in out Map_Segment;
                Recurse   : Boolean
             );
--
-- Store -- Root segment of the virtual to physical block map
--
--    Container - The array
--    Segment   - The map segment to store
--    Free      - The head of the joint list of free blocks
--
-- When storing  the root  segment the  list of disposed block should be
-- merged with the list of free blocks. The first block of disposed list
-- will  point to the head  of free list.  The head of the disposed list
-- is the head of the new free list as stored.
--
   procedure Store
             (  Container : in out Persistent_Transactional_Array;
                Segment   : Map_Segment;
                Free      : Block_Count
             );
--
-- Store -- Free blocks segment
--
--    Container - The array
--    Segment   - The free block list's segment to store
--
   procedure Store
             (  Container : in out Persistent_Transactional_Array;
                Segment   : Free_Segment
             );
--
-- Update -- Prepare a block for update
--
--    Container - The array
--    Virtual   - The virtual block index
--    Replace   - Drop old contents of the block
--
-- Returns :
--
--    The physical block number
--
-- Exceptions :
--
--    Data_Error - The specified block is not in the map
--
   function Update
            (  Container : Persistent_Transactional_Array;
               Virtual   : Block_Index;
               Replace   : Boolean
            )  return Block_Index;

   pragma Inline (Get);
   pragma Inline (Get_Lower);
   pragma Inline (Get_Offset);
   pragma Inline (Get_Parent);
   pragma Inline (Get_Segment_No);
   pragma Inline (Map);
   pragma Inline (Put);

   function Fletcher_16 (Data : Byte_Array) return Byte_Array;

end Persistent.Blocking_Files.Transactional;
