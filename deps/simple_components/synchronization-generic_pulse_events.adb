--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Pulse_Events                Luebeck            --
--  Implementation                                 Spring, 2008       --
--                                                                    --
--                                Last revision :  16:09 11 May 2008  --
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

package body Synchronization.Generic_Pulse_Events  is

   protected body Pulse_Event is

      entry Wait (Value : out Event_Value) when True is
      begin
         requeue Lounge (Current) with abort;
      end Wait;

      entry Pulse (Value : Event_Value)
         when Lounge (not Current)'Count = 0 is
      begin
         Data    := Value;
         Current := not Current;
      end Pulse;

      entry Lounge (for State in Boolean) (Value : out Event_Value)
         when Current /= State is
      begin
         Value := Data;
      end Lounge;

   end Pulse_Event;

end Synchronization.Generic_Pulse_Events ;
