--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Generic_Blackboard                          Spring, 2018       --
--  Interface                                                         --
--                                Last revision :  17:44 21 Jul 2018  --
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
--     type Shared is new Abstract_Shared_Environment with record
--        Lock  : Mutex;
--        Board : Blackboard (...);
--     end record;
--
--     Data    : Shared;
--     Item    : Reference;
--     Sequent : Boolean;
--     ...
--     Item := First (Data.Board);
--     while not (Item > Data.Board) loop
--        begin
--           ... Get (Data.Board, Item) ... -- Use Item
--        exception
--           when Constraint_Error =>
--              ... -- The element is lost
--        end;
--        Next (Data.Board, Item, Sequent);
--        if not Sequent then
--           ... -- Some elements were lost
--        end if;
--     end loop;
--
-- This implementation uses GCC built-in primitives.  It should  be used
-- with GNAT compiler,  in the cases when pragma Atomic is not supported
-- for 64-bit types.
------------------------------------------------------------------------
with System.Storage_Elements;  use System.Storage_Elements;
with System.Storage_Pools;     use System.Storage_Pools;

with Synchronization.Interprocess.Events;
use  Synchronization.Interprocess.Events;

with Synchronization.Interprocess.Mutexes;
use  Synchronization.Interprocess.Mutexes;

generic
   type Element_Type (<>) is private;
package Synchronization.Interprocess.Generic_Blackboard is
--
-- Blackboard -- The blackboard
--
--    Size - The size of, in storage elements
--
-- The  blackboard  size detemines how long survives an element put into
-- the  balckboard, after consequent placing other elements into it. The
-- blackboard requires a mutex used when putting data into it. The mutex
-- must be placed in the shared environment object before the queue. For
-- example:
--
--     type Shared is new Abstract_Shared_Environment with record
--        Lock : Mutex;  <-----------------------.
--        Data : Blackboard; -- Uses this mutex _/
--     end record;
--
   type Blackboard (Size : Storage_Count) is
      new Abstract_Shared_Object with private;
   type Reference is private;
--
-- First_Reference -- The first reference
--
   First_Reference : constant Reference;
--
-- First -- Get a reference of the first element in
--
--    Storage - The blackboard
--
-- Note  that  this  function  can return an invalid reference when used
-- concurrently. It that case the caller should call  it  again,  unless
-- the  result  is  greater than Storage according to >. That would mean
-- that the blackboard is empty.
--
-- Exceptions :
--
--    Status_Error - The blackboard is not initialized
--
   function First (Storage : Blackboard) return Reference;
--
-- Get -- Dereferencing
--
--    Storage - The blackboard
--    Pointer - A reference to an item in Storage
--
-- Returns :
--
--    The item referenced by Pointer
--
-- Exceptions :
--
--    Constraint_Error - The reference is no longer valid
--    Status_Error     - The blackboard is not initialized
--
   function Get (Storage : Blackboard; Pointer : Reference)
      return Element_Type;
--
-- Image -- String representation of a reference
--
--    Pointer - A reference to an item in Storage
--
-- Returns :
--
--    Text representation of Pointer
--
   function Image (Pointer : Reference) return String;
--
-- Is_Valid -- Validity check
--
--    Storage - The blackboard
--    Pointer - A reference to an item in Storage
--
-- Is_Valid (B, R) is equivalent to not (B < R or B > R)
--
-- Returns :
--
--    True if Pointer is a valid reference
--
-- Exceptions :
--
--    Status_Error - The blackboard is not initialized
--
   function Is_Valid (Storage : Blackboard; Pointer : Reference)
      return Boolean;
--
-- Next -- Advance reference to a next element
--
--    Storage - The blackboard
--    Pointer - A reference
--    Sequent - Set to True if the result is next to the argument
--
-- The  procedure  advances Pointer to the next element. When Pointer is
-- valid and there is a next element then Sequent is  set  to  True  and
-- Pointer will  refer  to  that  element. When Pointer refers to a lost
-- element then it is set to the first available element and Sequent  is
-- set to False. When Pointer refers to a not yet available element,  it
-- is not changed and Sequent is set to True.
--
-- Exceptions :
--
--    Status_Error - The blackboard is not initialized
--
   procedure Next
             (  Storage : Blackboard;
                Pointer : in out Reference;
                Sequent : out Boolean
             );
--
-- Put -- A new item into the blackboard
--
--    Storage   - The blackboard
--    Element   - The item to put
--  [ Pointer ] - A reference to the item
--  [ Timeout ] - Time before Timeout_Error is propagated
--
-- This  procedure  puts Element into Storage and returns a reference to
-- it. The operation overrides the most outdated items in the blackboard
-- making references to them invalid.
--
-- Exceptions :
--
--    Status_Error  - The blackboard is not initialized
--    Storage_Error - Element is too large to fit into
--    Timeout_Error - Timed out
--
   procedure Put
             (  Storage : in out Blackboard;
                Element : Element_Type;
                Pointer : out Reference;
                Timeout : Duration := Duration'Last
             );
   procedure Put
             (  Storage : in out Blackboard;
                Element : Element_Type;
                Timeout : Duration := Duration'Last
             );
--
-- Put -- A new item into the blackboard
--
--    Storage   - The blackboard
--    Element   - The item to put
--    Preserve  - A reference to an non-overriddable item
--  [ Pointer ] - A reference to the item
--    Success   - The outcome of the operation
--  [ Timeout ] - Time before Timeout_Error is propagated
--
-- This  procedure  puts Element into Storage and returns a reference to
-- it. The operation overrides the most outdated items in the blackboard
-- except  for  ones  indicated  by Preserve or any later. The parameter
-- Success is True when Element was put into the blackboard. It is False
-- when putting Element there would require removing items references by
-- Preserve.
--
-- Exceptions :
--
--    Status_Error  - The blackboard is not initialized
--    Storage_Error - Element is too large to fit into
--    Timeout_Error - Timed out
--
   procedure Put
             (  Storage  : in out Blackboard;
                Element  : Element_Type;
                Preserve : Reference;
                Pointer  : out Reference;
                Success  : out Boolean;
                Timeout  : Duration := Duration'Last
             );
   procedure Put
             (  Storage  : in out Blackboard;
                Element  : Element_Type;
                Preserve : Reference;
                Success  : out Boolean;
                Timeout  : Duration := Duration'Last
             );
--
-- Upper -- Get a reference of the least upper bound of the blackboard
--
--    Storage - The blackboard
--
-- The  function  returns  the least upper bound of Storage. That is the
-- reference to the element which will be put next into.
--
-- Returns :
--
--    The least upper bound
--
-- Exceptions :
--
--    Status_Error  - The blackboard is not initialized
--
   function Upper (Storage : Blackboard) return Reference;
--
-- <, > -- Reference validity checks
--
-- When the blackboard is compared to  reference,  it  is  less  than  a
-- reference if the latter refer to a not  yet  written  object.  It  is
-- greater than a reference if the object is already lost (overwritten).
--
-- Returns :
--
--    Comparison result
--
-- Exceptions :
--
--    Status_Error  - The blackboard is not initialized
--
   function "<" (Storage : Blackboard; Pointer : Reference)
      return Boolean;
   function ">" (Storage : Blackboard; Pointer : Reference)
      return Boolean;
   function "<" (Pointer : Reference; Storage : Blackboard)
      return Boolean;
   function ">" (Pointer : Reference; Storage : Blackboard)
      return Boolean;
--
-- <, <=, >= , > -- Reference comparisons
--
   function "<"  (Left, Right : Reference) return Boolean;
   function ">"  (Left, Right : Reference) return Boolean;
   function "<=" (Left, Right : Reference) return Boolean;
   function ">=" (Left, Right : Reference) return Boolean;

private
   pragma Inline (First);
   pragma Inline (Upper);
   pragma Inline (Get);
   pragma Inline (Is_Valid);
   pragma Inline (Put);

   type Modular is mod 2**64;
   function LT (Left, Right : Modular) return Boolean renames "<";
   function GT (Left, Right : Modular) return Boolean renames ">";
   function LE (Left, Right : Modular) return Boolean renames "<=";
   function GE (Left, Right : Modular) return Boolean renames ">=";
--
-- Reference -- Is a flat index, the lower part modulus  Blackboard.Size
--              is the offset in the buffer. The upper part is the frame
-- number. When the buffer is wrapped the frame number is incremented.
--
   type Reference is new Modular;

   pragma Inline (">", "<", "<=", ">=");

   First_Reference : constant Reference := 0;

   type Buffer is array (Storage_Count range <>) of Storage_Element;
   pragma Atomic_Components (Buffer);

   type Blackboard_Data (Size : Storage_Count) is record
      Offset : Storage_Offset := 0; -- To the "official" address
      Upper  : Reference := 0;
      Lower  : Reference := 0;
      Data   : Buffer (1..Size);
      pragma Atomic (Offset);
      pragma Atomic (Upper);
      pragma Atomic (Lower);
   end record;
   type Blackboard_Data_Ptr is access all Blackboard_Data;
   function Get_Block_Size
            (  Storage : Blackboard_Data;
               Offset  : Storage_Offset
            )  return Storage_Offset;
   pragma Inline (Get_Block_Size);
   function Get_Offset
            (  Storage : Blackboard_Data;
               Pointer : Reference
            )  return Storage_Offset;
   pragma Inline (Get_Offset);

   type Blackboard_Pool (Data : access Blackboard_Data) is
      new Root_Storage_Pool with
   record
      Pointer   : Reference;     -- The reference to the last element
      Index     : Storage_Count; -- The index of the last element
      Length    : Storage_Count; -- The length of the last element
      Preserved : Reference;
      Address   : System.Address;
   end record;
   procedure Allocate
             (  Storage   : in out Blackboard_Pool;
                Address   : out System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             );
   procedure Deallocate
             (  Storage   : in out Blackboard_Pool;
                Address   : System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             );
   function Storage_Size (Storage : Blackboard_Pool)
      return Storage_Count;

   type Mutex_Ptr is access all Mutex'Class;
   type Blackboard (Size : Storage_Count) is
      new Abstract_Shared_Object with
   record
      Lock : Mutex_Ptr;
      Data : Blackboard_Data_Ptr;
   end record;
   function Get_Signature (Storage : Blackboard) return Unsigned_16;
   function Get_Size (Storage : Blackboard) return Storage_Count;
   procedure Map
             (  Storage  : in out Blackboard;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );

end Synchronization.Interprocess.Generic_Blackboard;
