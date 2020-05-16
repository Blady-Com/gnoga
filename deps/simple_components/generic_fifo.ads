--                                                                    --
--  package Generic_FIFO            Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Autumn, 2007       --
--                                                                    --
--                                Last revision :  08:25 05 May 2020  --
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
--  FIFO is a simple first-in first-out queue. When only one  task  puts
--  elements into the queue, and only one gets them from there, then  no
--  locking is required.
--
with Ada.Finalization;

generic
   type Element_Type is private;
package Generic_FIFO is
--
-- FIFO -- The queue
--
--    Size - The maximal number of elements in the queue + 1
--
   type FIFO (Size : Positive) is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Delete -- A number of elements from the queue
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
-- Returns :
--
--    The number of unused FIFO elements
--
   function Free_Space (Queue : FIFO) return Natural;
--
-- Get -- An element from the queue
--
--    Queue   - The queue
--    Element - From the queue
--    Empty   - Set to true if there is no element to get
--
   procedure Get
             (  Queue   : in out FIFO;
                Element : out Element_Type;
                Empty   : out Boolean
             );
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
-- Is_Full -- Check if full
--
--    Queue - The queue
--
-- Returns :
--
--    True if Queue is full
--
   function Is_Full (Queue : FIFO) return Boolean;
--
-- Is_Preserved -- Check if element is purged
--
--    Queue   - The queue
--    Element - From the queue
--
-- This function is called by Purge in order to determine if the element
-- to remain in the queue. The default implementation returns True.
--
-- Returns :
--
--    True if Element is to stay in the queue
--
   function Is_Preserved (Queue : FIFO; Element : Element_Type)
      return Boolean;
   type Is_Preserved_Ptr is access
      function (Element : Element_Type) return Boolean;
--
-- Peek -- An element from the queue leaving it there
--
--    Queue   - The queue
--    Element - From the queue
--    Empty   - Set to true if there is no element to get
--
   procedure Peek
             (  Queue   : FIFO;
                Element : out Element_Type;
                Empty   : out Boolean
             );
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
-- Purge -- Delete certain elements from the queue
--
--    Queue          - The queue
--    Purged         - The number of elements removed
--  [ Is_Preserved ] - Function to use
--
-- This procedure removes the elements for  which function  Is_Preserved
-- returned False.
--
   procedure Purge (Queue : in out FIFO; Purged : out Natural);
   procedure Purge
             (  Queue        : in out FIFO;
                Is_Preserved : Is_Preserved_Ptr;
                Purged       : out Natural
             );
--
-- Put -- An element into the queue
--
--    Queue   - The queue
--    Element - To put into the queue
--    Full    - Set to rue if element was not put
--
   procedure Put
             (  Queue   : in out FIFO;
                Element : Element_Type;
                Full    : out Boolean
             );
--
-- Put -- An element into the queue
--
--    Queue   - The queue
--    Element - To put into the queue
--
-- Exceptions :
--
--    Constraint_Error - The queue is full
--
   procedure Put
             (  Queue   : in out FIFO;
                Element : Element_Type
             );
private
   pragma Inline (Delete);
   pragma Inline (Get);
   pragma Inline (Is_Empty);
   pragma Inline (Is_Full);
   pragma Inline (Peek);
   pragma Inline (Put);

   type Element_Array is array (Positive range <>) of Element_Type;

   type FIFO_Ptr is access all FIFO'Class;
   type FIFO (Size : Positive) is
      new Ada.Finalization.Limited_Controlled with
   record
      Self   : FIFO_Ptr := FIFO'Unchecked_Access;
      Free   : Positive := 1;
      First  : Positive := 1;
      Buffer : Element_Array (1..Size);

      pragma Atomic (Free);
      pragma Atomic (First);
   end record;

end Generic_FIFO;
