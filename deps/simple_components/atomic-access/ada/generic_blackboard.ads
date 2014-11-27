--                                                                    --
--  package Generic_Blackboard      Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Autumn, 2007       --
--                                                                    --
--                                Last revision :  10:25 26 Dec 2009  --
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
--  Blackboard  is  a container which can be used for creating lock-free
--  notification queueus and storage pools. The elements are  stored  in
--  the blackboard temporarily. Newly coming elements override the  most
--  elder  ones.  The  elements  in  the blackboard are accessed through
--  references which know if the destination is still valid. Because the
--  elements in the blackboard are destructed automatically, it  is  not
--  allowed  to use elements of controlled types. Otherwise, indefinite,
--  tagged  and  class-wide  elements can be used. The generic parameter
--  is:
--
--      Element_Type - Of the elements in the blackboard
--
--  Typically a blackboard is  read  in  a  lock-free  way  using  plain
--  subprograms.  The  function  Get dereferences a blackboard reference
--  and returns the corresponding element if  the  reference  is  valid.
--  Othwerwise   Constraint_Error   is  propagated.  This  can  be  done
--  concurrently to other tasks doing Get  or  Put.  The  procedure  Put
--  shall  be  used in mutually exclusive way with other concurrent Put.
--  Thus if several tasks should call it, that must be done from  inside
--  a protected action or else through a monitor  task.  See  the  child
--  package Generic_Task_Safe which provides blackboards with  task-safe
--  operations.
--
--  Blackboard  elements can be iterated as follows:
--
--     Data : Blackboard;
--     ...
--     Item    : Reference;
--     Sequent : Boolean;
--     ...
--     Item := First (Data);
--     while not (Item > Data) loop
--        begin
--           ... Get (Data, Item) ... -- Use Item
--        exception
--           when Constraint_Error =>
--              ... -- The element is lost
--        end;
--        Next (Data, Item, Sequent);
--        if not Sequent then
--           ... -- Some elements were lost
--        end if;
--     end loop;
--
with System.Storage_Elements;  use System.Storage_Elements;
with System.Storage_Pools;     use System.Storage_Pools;

with Ada.Finalization;

generic
   type Element_Type (<>) is private;
package Generic_Blackboard is
--
-- Blackboard -- The blackboard
--
--    Size - The size of, in storage elements
--
-- The  blackboard  size detemines how long survives an element put into
-- the balckboard, after consequent placing other elements into it.
--
   type Blackboard (Size : Storage_Count) is
      new Ada.Finalization.Limited_Controlled with private;
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
--
   function Get (Storage : Blackboard; Pointer : Reference)
      return Element_Type;
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
--
-- This  procedure  puts Element into Storage and returns a reference to
-- it. The operation overrides the most outdated items in the blackboard
-- making references to them invalid.
--
-- Exceptions :
--
--    Storage_Error - Element is too large to fit into
--
   procedure Put
             (  Storage : in out Blackboard;
                Element : Element_Type;
                Pointer : out Reference
             );
   procedure Put
             (  Storage : in out Blackboard;
                Element : Element_Type
             );
--
-- Put -- A new item into the blackboard
--
--    Storage   - The blackboard
--    Element   - The item to put
--    Preserve  - A reference to an non-overriddable item
--  [ Pointer ] - A reference to the item
--    Success   - The outcome of the operation
--
-- This  procedure  puts Element into Storage and returns a reference to
-- it. The operation overrides the most outdated items in the blackboard
-- except  for  ones  indicated  by Preserve or anz later. The parameter
-- Success is True when Element was put into the blackboard. It is False
-- when putting Element there would require removing items references by
-- Preserve.
--
-- Exceptions :
--
--    Storage_Error - Element is too large to fit into
--
   procedure Put
             (  Storage  : in out Blackboard;
                Element  : Element_Type;
                Preserve : Reference;
                Pointer  : out Reference;
                Success  : out Boolean
             );
   procedure Put
             (  Storage  : in out Blackboard;
                Element  : Element_Type;
                Preserve : Reference;
                Success  : out Boolean
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
   function Upper (Storage : Blackboard) return Reference;
--
-- <, > -- Reference validity checks
--
-- When the blackboard is compared to  reference,  it  is  less  than  a
-- reference if the latter refer to a not  yet  written  object.  It  is
-- greter than a reference if the object is already lost (overwritten).
--
-- Returns :
--
--    Comparison result
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
   pragma Inline (">", "<");

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
   function "<"  (Left, Right : Reference) return Boolean renames LT;
   function ">"  (Left, Right : Reference) return Boolean renames GT;
   function "<=" (Left, Right : Reference) return Boolean renames LE;
   function ">=" (Left, Right : Reference) return Boolean renames GE;

   First_Reference : constant Reference := 0;

   type Buffer is array (Storage_Count range <>) of Storage_Element;
   pragma Atomic_Components (Buffer);
   type Blackboard (Size : Storage_Count) is
      new Root_Storage_Pool with
   record
      Upper     : Reference := 0;
      Lower     : Reference := 0;
      Preserved : Reference;
      Pointer   : Reference;       -- The reference to the last element
      Index     : Storage_Count;   -- The index of the last element
      Length    : Storage_Count;   -- The length of the last element
      Address   : System.Address;
      Data      : Buffer (1..Size);

      pragma Atomic (Lower);
      pragma Atomic (Upper);
   end record;

   procedure Allocate
             (  Storage   : in out Blackboard;
                Address   : out System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             );
   procedure Deallocate
             (  Storage   : in out Blackboard;
                Address   : System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             );
   function Storage_Size (Storage : Blackboard) return Storage_Count;

end Generic_Blackboard;
