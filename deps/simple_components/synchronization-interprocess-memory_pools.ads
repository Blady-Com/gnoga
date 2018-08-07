--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Memory_Pools                                Spring, 2018       --
--  Interface                                                         --
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

with Synchronization.Interprocess.Mutexes;
use  Synchronization.Interprocess.Mutexes;

package Synchronization.Interprocess.Memory_Pools is
--
-- Interprocess_Pool -- A pool
--
--    Size - The total space used for the pool
--
-- The pool requires a mutex used when dealing with the pool. The  mutex
-- must be placed in the shared environment object before the queue. For
-- example:
--
--     type Shared is new Abstract_Shared_Environment with record
--        Lock : Mutex;  <------------------------------.
--        Pool : Interprocess_Pool; -- Uses this mutex _/
--     end record;
--
   type Reference is private;
   Null_Reference : constant Reference;
   subtype Pool_Storage_Count is Storage_Count
      range 32..Storage_Count'Last;
   type Interprocess_Pool (Size : Pool_Storage_Count) is
      new Root_Storage_Pool with private;
--
-- Allocate -- Pool memory
--
--    Pool      - The pool
--    Address   - Of the allocated block
--    Size      - Of the block
--    Alignment - Of the allocated block
--
-- Exceptions :
--
--    Status_Error  - The pool is not initialized
--    Storage_Error - No space in the pool
--    Timeout_Error - Timeout
--
   procedure Allocate
             (  Pool      : in out Interprocess_Pool;
                Address   : out System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             );
--
-- Deallocate -- Pool memory
--
--    Pool      - The pool
--    Address   - Of the allocated block
--    Size      - Of the block
--    Alignment - Of the allocated block
--
-- Exceptions :
--
--    Status_Error  - The pool is not initialized
--    Storage_Error - No space in the pool
--    Timeout_Error - Timeout
--
   procedure Deallocate
             (  Pool      : in out Interprocess_Pool;
                Address   : System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             );
--
-- Free -- Pool memory
--
--    Pool    - The pool
--    Pointer - To the allocated object
--
-- Exceptions :
--
--    Status_Error  - The pool is not initialized
--    Storage_Error - Invalid reference
--    Timeout_Error - Timeout
--
   procedure Free
             (  Pool    : in out Interprocess_Pool;
                Pointer : in out Reference
             );
--
-- Get_Statistics -- Pool statistics
--
--    Pool        - Memory pool
--    Free_Blocks - The number of free blocks in the pool
--    Used_Blocks - The number of allocated blocks in the pool
--    Free_Space  - The total space avaialbale in the free blocks
--    Used_Space  - The total space of the allocated blocks
--
-- Exceptions :
--
--    Status_Error  - The pool is not initialized
--    Timeout_Error - Timeout
--
   procedure Get_Statistics
             (  Pool        : Interprocess_Pool;
                Free_Blocks : out Natural;
                Used_Blocks : out Natural;
                Free_Space  : out Storage_Count;
                Used_Space  : out Storage_Count
             );
--
-- Get_Offset -- The offset in the shared memory
--
--    Pool - The pool
--
-- Returns :
--
--    The offset to the object data in the shared memory
--
   function Get_Offset
            (  Pool : Interprocess_Pool
            )  return Storage_Offset;
--
-- Get_Timeout -- The timeout
--
--    Pool - The pool
--
-- Returns :
--
--    The timeout used in the Seize
--
   function Get_Timeout (Pool : Interprocess_Pool) return Duration;
--
-- Set_Timeout -- The I/O timeout
--
--    Pool    - The pool
--    Timeout - The timeout to be used in the Seize
--
   procedure Set_Timeout
             (  Pool    : in out Interprocess_Pool;
                Timeout : Duration
             );
--
-- Storage_Size -- Pool memory
--
--    Pool - The pool
--
-- Returns :
--
--    The pool space
--
-- Exceptions :
--
--    Status_Error - The pool is not initialized
--
   function Storage_Size
            (  Pool : Interprocess_Pool
            )  return Storage_Count;
--
-- To_Address -- Convert reference to address
--
--    Pool    - The pool
--    Pointer - Reference
--
-- Returns :
--
--    The address corresponding to the reference
--
-- Exceptions :
--
--    Status_Error  - The pool is not initialized
--    Storage_Error - An invalid reference
--
   function To_Address
            (  Pool    : Interprocess_Pool;
               Pointer : Reference
            )  return System.Address;
--
-- To_Reference -- Convert address to reference
--
--    Pool    - The pool
--    Address - Memory address
--
-- Returns :
--
--    The reference corresponding to the address
--
-- Exceptions :
--
--    Status_Error  - The pool is not initialized
--    Storage_Error - An invalid address
--
   function To_Reference
            (  Pool    : Interprocess_Pool;
               Address : System.Address
            )  return Reference;
private
   type Reference is new Integer;
   Null_Reference : constant Reference := 0;

   type Pool_Array is array (Reference range <>) of aliased Reference;
   type Pool_Data (Size : Reference) is record
      First_Free  : Reference := 1;
      Free_Blocks : Reference := 1;
      Used_Blocks : Reference := 0;
      Used_Count  : Reference := 0;
      Free_Count  : Reference := Size - 2;
      Core        : Pool_Array (1..Size);
   end record;
   type Pool_Data_Ptr is access all Pool_Data;
   procedure Check (Data : Pool_Data; Index : Reference);
   function Invariant (Data : Pool_Data) return Boolean;
   function To_Reference
            (  Data    : Pool_Data;
               Address : System.Address
            )  return Reference;

   type Shared_Object (Pool : access Interprocess_Pool'Class) is
      new Abstract_Shared_Object with null record;
   function Get_Size (Object : Shared_Object) return Storage_Count;
   procedure Map
             (  Object   : in out Shared_Object;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );

   type Mutex_Ptr is access all Mutex'Class;
   type Interprocess_Pool (Size : Pool_Storage_Count) is
      new Root_Storage_Pool with
   record
      Lock    : Mutex_Ptr;
      Data    : Pool_Data_Ptr;
      Timeout : Duration := 10.0;
      Object  : aliased Shared_Object
                        (  Interprocess_Pool'Unchecked_Access
                        );
   end record;
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Pool   : Interprocess_Pool
             );
   for Interprocess_Pool'Write use Enumerate;

   function To_Reference (Count : Storage_Offset) return Reference;

   pragma Inline (Check);
   pragma Inline (Invariant);
   pragma Inline (To_Reference);

end Synchronization.Interprocess.Memory_Pools;

