--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Synchronization.Mutexes                    Luebeck            --
--  Inmplementation                                Spring, 2008       --
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

package body Synchronization.Mutexes is

   procedure Finalize (Object : in out Holder) is
   begin
      Object.Resource.Release;
   end Finalize;

   procedure Initialize (Object : in out Holder) is
   begin
      Object.Resource.Seize;
   end Initialize;

   protected body Mutex is

      function Get_Owner return Task_ID is
      begin
         return Owner;
      end Get_Owner;

      procedure Grab is
      begin
         if Owner = Current_Task then
            Count := Count + 1;
         elsif Owner = Null_Task_ID then
            Owner := Current_Task;
            Count := 1;
         end if;
      end Grab;

      function Is_Mine return Boolean is
      begin
         return Owner = Current_Task;
      end Is_Mine;

      function Is_Owned return Boolean is
      begin
         return Owner /= Null_Task_ID;
      end Is_Owned;

      entry Lounge when Owner = Null_Task_ID is
      begin
         Owner := Lounge'Caller;
         Count := 1;
      end Lounge;

      procedure Release is
      begin
         if Owner = Current_Task then
            Count := Count - 1;
            if Count = 0 then
               Owner := Null_Task_ID;
            end if;
         else
            raise Ownership_Error;
         end if;
      end Release;

      entry Seize when True is
      begin
         if Seize'Caller = Owner then
            Count := Count + 1;
         elsif Owner = Null_Task_ID then
            Owner := Seize'Caller;
            Count := 1;
         else
            requeue Lounge with abort;
         end if;
      end Seize;

   end Mutex;

end Synchronization.Mutexes;
