--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Blackboard.                         Luebeck            --
--        Generic_Task_Safe                        Autumn, 2007       --
--  Implementation                                                    --
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

package body Generic_Blackboard.Generic_Task_Safe is

   procedure Put
             (  Storage : in out Shared_Blackboard;
                Element : Element_Type;
                Pointer : out Reference
             )  is
   begin
      Storage.Lock.Safe_Put (Blackboard (Storage), Element, Pointer);
   end Put;

   protected body Mutex is
      procedure Safe_Put
                (  Storage : in out Blackboard; 
                   Element : Element_Type;
                   Pointer : out Reference
                )  is
      begin
         Put (Storage, Element, Pointer);
      end Safe_Put;
   end Mutex;

end Generic_Blackboard.Generic_Task_Safe;
