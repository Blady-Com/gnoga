--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_FIFO.Generic_Signaled               Luebeck            --
--  Interface                                      Autumn, 2007       --
--                                                                    --
--                                Last revision :  20:01 04 Apr 2016  --
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

generic
package Generic_FIFO.Generic_Signaled is
--
-- Signaled_FIFO -- The queue
--
--    Size - The maximal number of elements in the queue + 1
--
   type Signaled_FIFO is new FIFO with private;
--
-- Cancel -- Prematurely cancel waiting
--
--    Queue - The queue
--
-- This  operation  terminates  waiting  of  any tasks blocked on Queue.
-- Blocking  calls to Get (when queue is empty) or to Put (when queue is
-- full) are released with  propagation  of  End_Error.  Differently  to
-- Delete, Get, Peek and Put, Cancel can be called from any task.
--
   procedure Cancel (Queue : in out Signaled_FIFO);
--
-- Delete -- Overrides Generic_FIFO...
--
   procedure Delete
             (  Queue : in out Signaled_FIFO;
                Count : Natural := 1
             );
--
-- Get -- Overrides Generic_FIFO...
--
   procedure Get
             (  Queue   : in out Signaled_FIFO;
                Element : out Element_Type;
                Empty   : out Boolean
             );
--
-- Get -- Overrides Generic_FIFO...
--
--    Queue     - The queue
--  [ Timeout ] - Blocking timeout
--
-- Returns :
--
--    The element from the queue
--
-- Exceptions :
--
--    Constraint_Error - Timeout expiration when Queue is still empty
--    End_Error        - Canceled by a call to Cancel
--
   function Get (Queue : Signaled_FIFO) return Element_Type;
   function Get (Queue : Signaled_FIFO; Timeout : Duration)
      return Element_Type;
--
-- Peek -- Overrides Generic_FIFO...
--
--    Queue     - The queue
--  [ Timeout ] - Blocking timeout
--
-- Returns :
--
--    The element from the queue
--
-- Exceptions :
--
--    Constraint_Error - Timeout expiration when Queue is still empty
--    End_Error        - Canceled by a call to Cancel
--
   function Peek (Queue : Signaled_FIFO) return Element_Type;
   function Peek (Queue : Signaled_FIFO; Timeout : Duration)
      return Element_Type;
--
-- Purge -- Overrides Generic_FIFO...
--
   procedure Purge (Queue : in out Signaled_FIFO; Purged : out Natural);
   procedure Purge
             (  Queue        : in out Signaled_FIFO;
                Is_Preserved : Is_Preserved_Ptr;
                Purged       : out Natural
             );
--
-- Put -- Overrides Generic_FIFO...
--
   procedure Put
             (  Queue   : in out Signaled_FIFO;
                Element : Element_Type;
                Full    : out Boolean
             );
--
-- Put -- Overrides Generic_FIFO...
--
--    Queue     - The queue
--    Element   - To put into the queue
--  [ Timeout ] - Blocking timeout
--
-- Exceptions :
--
--    Constraint_Error - Timeout expiration when Queue is still full
--    End_Error        - Canceled by a call to Cancel
--
   procedure Put
             (  Queue   : in out Signaled_FIFO;
                Element : Element_Type
             );
   procedure Put
             (  Queue   : in out Signaled_FIFO;
                Element : Element_Type;
                Timeout : Duration
             );
private
   pragma Inline (Delete);
   pragma Inline (Get);
   pragma Inline (Peek);
   pragma Inline (Put);

   protected type Signal (Queue : access Signaled_FIFO) is
      entry Break;
      procedure Release_Get;
      procedure Release_Put;
      entry Wait_To_Get;
      entry Wait_To_Put;
   private
      Released_Get : Boolean := False;
      Released_Put : Boolean := False;
   end Signal;

   type Signaled_FIFO is new FIFO with record
      Event : Signal (Signaled_FIFO'Unchecked_Access);
      Get_Blocked : Boolean := False;
      Put_Blocked : Boolean := False;

      pragma Atomic (Get_Blocked);
      pragma Atomic (Put_Blocked);
   end record;

end Generic_FIFO.Generic_Signaled;
