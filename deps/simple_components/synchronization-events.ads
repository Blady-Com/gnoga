--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Events                      Luebeck            --
--  Interface                                      Spring, 2008       --
--                                                                    --
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
--
--  This  package  provides plain events. Such an event can be signaled,
--  reset,  and  awaited  for. When signaled or reset it remains in this
--  state until next reset or signal action respectively.
--
package Synchronization.Events is
--
-- Event -- A plain event
--
   protected type Event is
   --
   -- Is_Signaled -- The current event state
   --
   -- Returns :
   --
   --    True if the event is signaled
   --
      function Is_Signaled return Boolean;
   --
   -- Reset -- The event to the non-signaled state
   --
      procedure Reset;
   --
   -- Signal -- The event
   --
      procedure Signal;
   --
   -- Wait -- For the event signaled
   --
      entry Wait;
   --
   -- Wait_For_Reset -- For the event reset
   --
      entry Wait_For_Reset;

   private
      pragma Inline (Is_Signaled);
      pragma Inline (Reset);
      pragma Inline (Signal);

      Set : Boolean := False;
   end Event;

end Synchronization.Events;
