--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Indefinite_FIFO                     Luebeck            --
--  Interface                                      Summer, 2008       --
--                                                                    --
--                                Last revision :  16:40 15 Oct 2020  --
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
--  FIFO is a first-in first-out queue of elements, which can be of  any
--  indefinite type. When only one task puts elements  into  the  queue,
--  and  only  one  gets  them  from there, then no locking is required.
--  Differently to the queue of definite elements it  is  impossible  in
--  adavance  to  tell  if  the  queue  is  full,  otherwise the package
--  recembles Generic_FIFO.
--
with System.Storage_Elements;  use System.Storage_Elements;
with System.Storage_Pools;     use System.Storage_Pools;

with Ada.Finalization;

generic
   type Element_Type (<>) is private;
package Generic_Indefinite_FIFO is
--
-- FIFO -- The queue of messages
--
--    Size - In storage elements
--
   type FIFO (Size : Storage_Count) is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Delete -- An number of elements from the queue
--
--    Queue - The queue
--    Count - The number of elements to delete
--
-- The  procedure  removes  the specified number of elements from Queue.
-- When Count is greater than the current queue length, all elements are
-- removed.
--
   procedure Delete
             (  Queue : in out FIFO;
                Count : Natural := 1
             );
--
-- Free_Space -- Unused space in the queue
--
--    Queue - The queue
--
-- Note  that the result cannot guarantee consequent Put fail-safe, when
-- the free space is  segmented.  There  can  be  up  to  two  segments.
-- Therefore only when Free_Space is twice as required then Put will not
-- fail.
--
-- Returns :
--
--    The number of unused storage elements
--
   function Free_Space (Queue : FIFO) return Storage_Count;
--
-- Get -- An element from the queue
--
--    Queue - The queue
--
-- Returns :
--
--    The element from the queue
--
-- Exceptions :
--
--    Constraint_Error - The queue is empty
--
   function Get (Queue : FIFO) return Element_Type;
--
-- Is_Empty -- Check if empty
--
--    Queue - The queue
--
-- Returns :
--
--    True if Queue is empty
--
   function Is_Empty (Queue : FIFO) return Boolean;
--
-- Peek -- An element from the queue leaving it there
--
--    Queue - The queue
--
-- Returns :
--
--    The element from the queue
--
-- Exceptions :
--
--    Constraint_Error - The queue is empty
--
   function Peek (Queue : FIFO) return Element_Type;
--
-- Put -- An element into the queue
--
--    Queue   - The queue
--    Element - To put into the queue
--  [ Full ]  - The queue is full (no exception raised)
--
-- Exceptions :
--
--    Constraint_Error - The queue has no space for Element (full)
--
   procedure Put
             (  Queue   : in out FIFO;
                Element : Element_Type
             );
   procedure Put
             (  Queue   : in out FIFO;
                Element : Element_Type;
                Full    : out Boolean
             );
private
   type Buffer is array (Storage_Offset range <>) of Storage_Element;
   type FIFO_Ptr is access all FIFO'Class;
   type FIFO (Size : Storage_Count) is
      new Root_Storage_Pool with
   record
      Free    : Storage_Count := 1;
      First   : Storage_Count := 1;
      Cut_Off : Storage_Count := Size + 1;
         -- The following two are the private interface of Allocate
      Last    : System.Address; -- Of the last allocated element
      Next    : Storage_Count;  -- The element next to last allocated
      Self    : FIFO_Ptr := FIFO'Unchecked_Access;
      Storage : Buffer (1..Size);

      pragma Atomic (Free);
      pragma Atomic (First);
      pragma Atomic (Cut_Off);
   end record;

   procedure Allocate
             (  Queue     : in out FIFO;
                Address   : out System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             );
   function Can_Allocate
             (  Queue     : FIFO;
                Size      : Storage_Count;
                Alignment : Storage_Count
             )  return Boolean;
   procedure Deallocate
             (  Queue     : in out FIFO;
                Address   : System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             );
   function Storage_Size (Queue : FIFO) return Storage_Count;

   pragma Inline (Deallocate);
   pragma Inline (Delete);
   pragma Inline (Free_Space);
   pragma Inline (Get);
   pragma Inline (Is_Empty);
   pragma Inline (Peek);
   pragma Inline (Put);

end Generic_Indefinite_FIFO;
