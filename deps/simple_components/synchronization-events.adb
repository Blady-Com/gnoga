--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Synchronization.Events                     Luebeck            --
--  Interface                                      Spring, 2008       --
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

package body Synchronization.Events is

   protected body Event is

      function Is_Signaled return Boolean is
      begin
         return Set;
      end Is_Signaled;
      
      procedure Reset is
      begin
         Set := False;
      end Reset;

      procedure Signal is
      begin
         Set := True;
      end Signal;

      entry Wait when Set is
      begin
         null;
      end Wait;

      entry Wait_For_Reset when not Set is
      begin
         null;
      end Wait_For_Reset;

   end Event;

end Synchronization.Events;
