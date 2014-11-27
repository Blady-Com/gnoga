--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Synchronization.                           Luebeck            --
--         Generic_Mutexes_Array                   Spring, 2008       --
--  Inmplementation                                                   --
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

package body Synchronization.Generic_Mutexes_Array is

   procedure Finalize (Object : in out Set_Holder) is
   begin
      Object.Resource.Release_All (Object.Seize.all);
   end Finalize;

   procedure Finalize (Object : in out Singleton_Holder) is
   begin
      Object.Resource.Release (Object.Seize);
   end Finalize;

   procedure Initialize (Object : in out Set_Holder) is
   begin
      Object.Resource.Seize_All (Object.Seize.all);
   end Initialize;

   procedure Initialize (Object : in out Singleton_Holder) is
   begin
      Object.Resource.Seize (Object.Seize);
   end Initialize;

   protected body Mutexes_Array is

      procedure Acquire
                (  Mutexes : Mutexes_Set;
                   Caller  : Task_ID;
                   Success : out Boolean
                )  is
      begin
         for Mutex in Mutexes'Range loop
            if Mutexes (Mutex) then
               -- Whished to seize this one
               if Owner (Mutex) = Caller then
                  Count (Mutex) := Count (Mutex) + 1;
               elsif Owner (Mutex) = Null_Task_ID then
                  Owner (Mutex) := Caller;
                  Count (Mutex) := 1;
               else
                  if Mutex /= Mutexes'First then
                     -- We cannot seize this one, roll everything back
                     for Modified in Mutexes'First
                                  .. Mutex_Type'Pred (Mutex)
                     loop
                        if Mutexes (Modified) then
                           Count (Modified) := Count (Modified) - 1;
                           if Count (Modified) = 0 then
                              Owner (Modified) := Null_Task_ID;
                           end if;
                        end if;
                     end loop;
                  end if;
                  Success := False;
                  return;
               end if;
            end if;
         end loop;
         Success := True;
      end Acquire;

      function Check (Mutex : Mutex_Type; Caller : Task_ID)
         return Boolean is
      begin
         if Owner (Mutex) /= Caller and then Mutex /= Owner'Last then
            for Index in Mutex_Type'Succ (Mutex)..Owner'Last loop
               if Owner (Index) = Caller then
                  return False;
               end if;
            end loop;
         end if;
         return True;
      end Check;

      function Check (Mutexes : Mutexes_Set; Caller : Task_ID)
         return Boolean is
      begin
         for Mutex in Mutexes'Range loop
            if (  Mutexes (Mutex)
               and then
                  Owner (Mutex) /= Caller
               and then
                  Mutex /= Owner'Last
               )
            then
               for Index in Mutex_Type'Succ (Mutex)..Owner'Last loop
                  if Owner (Index) = Caller then
                     return False;
                  end if;
               end loop;
               return True;
            end if;
         end loop;
         return True;
      end Check;

      function Empty_Lounges return Boolean is
      begin
         return
         (  Lounge_Single (Current)'Count = 0
         and then
            Lounge_Multiple (Current)'Count = 0
         );
      end Empty_Lounges;

      function Get_Owner (Mutex : Mutex_Type) return Task_ID is
      begin
         return Owner (Mutex);
      end Get_Owner;

      procedure Grab (Mutex : Mutex_Type) is
         Success : Boolean;
      begin
         Grab (Mutex, Success);
      end Grab;

      procedure Grab (Mutex : Mutex_Type; Success : out Boolean) is
      begin
         if not Check (Mutex, Current_Task) then
            raise Ownership_Error;
         end if;
         if Owner (Mutex) = Current_Task then
            Count (Mutex) := Count (Mutex) + 1;
         elsif Owner (Mutex) = Null_Task_ID then
            Owner (Mutex) := Current_Task;
            Count (Mutex) := 1;
         end if;
      end Grab;

      procedure Grab_All (Mutexes : Mutexes_Set; Success : out Boolean) is
      begin
         if not Check (Mutexes, Current_Task) then
            raise Ownership_Error;
         end if;
         Acquire (Mutexes, Current_Task, Success);
      end Grab_All;

      function Is_Mine (Mutex : Mutex_Type) return Boolean is
      begin
         return Owner (Mutex) = Current_Task;
      end Is_Mine;

      function Is_Mine (Mutexes : Mutexes_Set) return Boolean is
      begin
         for Mutex in Owner'Range loop
            if Owner (Mutex) /= Current_Task then
               return False;
            end if;
         end loop;
         return True;
      end Is_Mine;

      function Is_Owned (Mutex : Mutex_Type) return Boolean is
      begin
         return Owner (Mutex) /= Null_Task_ID;
      end Is_Owned;

      entry Lounge_Multiple (for Toggle in Boolean)
            (  Mutexes : Mutexes_Set
            )  when Current = Toggle is
         Success : Boolean;
      begin
         Acquire (Mutexes, Lounge_Multiple'Caller, Success);
         if not Success then
            requeue Lounge_Multiple (not Current) with abort;
         end if;
      end Lounge_Multiple;

      entry Lounge_Single (for Toggle in Boolean) (Mutex : Mutex_Type)
         when Current = Toggle is
      begin
         if Owner (Mutex) = Lounge_Single'Caller then
            Count (Mutex) := Count (Mutex) + 1;
         elsif Owner (Mutex) = Null_Task_ID then
            Owner (Mutex) := Lounge_Single'Caller;
            Count (Mutex) := 1;
         else
            requeue Lounge_Single (not Current) with abort;
         end if;
      end Lounge_Single;

      entry Release (Mutex : Mutex_Type) when Empty_Lounges is
      begin
         if Owner (Mutex) = Release'Caller then
            Count (Mutex) := Count (Mutex) - 1;
            if Count (Mutex) = 0 then
               Owner (Mutex) := Null_Task_ID;
            end if;
            Current := not Current;
         else
            raise Ownership_Error;
         end if;
      end Release;

      entry Release_All (Mutexes : Mutexes_Set) when Empty_Lounges is
      begin
         for Mutex in Mutexes'Range loop
            if Mutexes (Mutex) then
               if Owner (Mutex) = Release_All'Caller then
                  Count (Mutex) := Count (Mutex) - 1;
                  if Count (Mutex) = 0 then
                     Owner (Mutex) := Null_Task_ID;
                  end if;
               else
                  Current := not Current;
                  raise Ownership_Error;
               end if;
            end if;
         end loop;
         Current := not Current;
      end Release_All;

      entry Seize (Mutex : Mutex_Type) when True is
      begin
         if not Check (Mutex, Seize'Caller) then
            raise Ownership_Error;
         end if;
         requeue Lounge_Single (Current) with abort;
      end Seize;

      entry Seize_All (Mutexes : Mutexes_Set) when True is
      begin
         if not Check (Mutexes, Seize_All'Caller) then
            raise Ownership_Error;
         end if;
         requeue Lounge_Multiple (Current) with abort;
      end Seize_All;

   end Mutexes_Array;

   function "or" (Left, Right : Mutex_Type) return Mutexes_Set is
      Result : Mutexes_Set := (others => False);
   begin
      Result (Left)  := True;
      Result (Right) := True;
      return Result;
   end "or";

   function "or" (Left : Mutexes_Set; Right : Mutex_Type)
      return Mutexes_Set is
      Result : Mutexes_Set := Left;
   begin
      Result (Right) := True;
      return Result;
   end "or";

   function "or" (Left : Mutex_Type; Right : Mutexes_Set)
      return Mutexes_Set is
      Result : Mutexes_Set := Right;
   begin
      Result (Left) := True;
      return Result;
   end "or";

   function "not" (Left : Mutex_Type) return Mutexes_Set is
      Result : Mutexes_Set := (others => True);
   begin
      Result (Left)  := False;
      return Result;
   end "not";

end Synchronization.Generic_Mutexes_Array;
