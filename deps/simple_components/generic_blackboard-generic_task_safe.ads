--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Blackboard.                         Luebeck            --
--        Generic_Task_Safe                        Autumn, 2007       --
--  Interface                                                         --
--                                Last revision :  21:32 05 May 2008  --
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
--  This  package  provides  task-safe  blackboards.  All operations are
--  task-safe.  Accessing  the blackboard for read is lock-free. Writing
--  into the blackboard is a protected action.
--
generic
package Generic_Blackboard.Generic_Task_Safe is
--
-- Shared_Blackboard -- A task-safe blackboard
--
   type Shared_Blackboard is new Blackboard with private;
--
-- Put -- Overrides Generic_Blackboard...
--
-- The implementation uses a protected action in order to achieve mutual
-- exclusion when called concurrently.
--
   procedure Put
             (  Storage : in out Shared_Blackboard;
                Element : Element_Type;
                Pointer : out Reference
             );
private
   pragma Inline (Put);

   protected type Mutex is
      procedure Safe_Put
                (  Storage : in out Blackboard;
                   Element : Element_Type;
                   Pointer : out Reference
                );
      pragma Inline (Safe_Put);
   end Mutex;

   type Shared_Blackboard is new Blackboard with record
      Lock : Mutex;
   end record;

end Generic_Blackboard.Generic_Task_Safe;
