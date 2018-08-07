--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     ...Generic_Call_Service_Arrays              Luebeck            --
--                                                 Spring, 2018       --
--  Implementation                                                    --
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

package body Synchronization.Interprocess.Process_Call_Service.
             Generic_Call_Service_Arrays is

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Object : Call_Service_Array
             )  is
   begin
      for Index in Object'Range loop
         Enumerate (Stream, Object (Index));
      end loop;
   end Enumerate;

end Synchronization.Interprocess.Process_Call_Service.
    Generic_Call_Service_Arrays;
