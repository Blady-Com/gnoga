--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.                            Luebeck            --
--        Generic_Events_Array                     Spring, 2008       --
--  Implementation                                                    --
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

package body Synchronization.Generic_Events_Array is

   protected body Events_Array is

      function Empty_Lounges return Boolean is
      begin
         return
         (  Lounge_Wait (Current)'Count = 0
         and then
            Lounge_Set_Multiple (Current)'Count = 0
         and then
            Lounge_Set_Single (Current)'Count = 0
         );
      end Empty_Lounges;

      function Get_State return Events_State is
      begin
         return State;
      end Get_State;

      function Is_Signaled (Event : Event_Type) return Boolean is
      begin
         return State (Event);
      end Is_Signaled;

      entry Lounge_Set_Multiple (for Toggle in Boolean)
            (  Events    : Events_State;
               Condition : Abstract_Condition'Class
            )  when Current = Toggle is
      begin
         if not Satisfied (Condition, State) then
            requeue Lounge_Set_Multiple (not Current) with abort;
         end if;
      end Lounge_Set_Multiple;

      entry Lounge_Set_Single (for Toggle in Boolean)
            (  Event     : Event_Type;
               Condition : Abstract_Condition'Class
            )  when Current = Toggle is
      begin
         if not Satisfied (Condition, State) then
            requeue Lounge_Set_Single (not Current) with abort;
         end if;
      end Lounge_Set_Single;

      entry Lounge_Wait (for Toggle in Boolean)
            (  Condition : Abstract_Condition'Class
            )  when Current = Toggle is
      begin
         if not Satisfied (Condition, State) then
            requeue Lounge_Wait (not Current);
         end if;
      end Lounge_Wait;

      entry Reset
            (  Events    : Events_State;
               Condition : Abstract_Condition'Class := Always_Signaled
            )  when Empty_Lounges is
      begin
         State   := State and not Events;
         Current := not Current;
         if not Satisfied (Condition, State) then
            requeue Lounge_Set_Multiple (not Current) with abort;
         end if;
      end Reset;

      entry Reset
            (  Event     : Event_Type;
               Condition : Abstract_Condition'Class := Always_Signaled
            )  when Empty_Lounges is
      begin
         State (Event) := False;
         Current := not Current;
         if not Satisfied (Condition, State) then
            requeue Lounge_Set_Single (not Current) with abort;
         end if;
      end Reset;

      entry Set
            (  State     : Events_State;
               Condition : Abstract_Condition'Class := Always_Signaled
            )  when Empty_Lounges is
      begin
         Events_Array.State := State;
         Current := not Current;
         if not Satisfied (Condition, State) then
            requeue Lounge_Set_Multiple (not Current) with abort;
         end if;
      end Set;

      entry Signal
            (  Events    : Events_State;
               Condition : Abstract_Condition'Class := Always_Signaled
            )  when Empty_Lounges is
      begin
         State   := State or Events;
         Current := not Current;
         if not Satisfied (Condition, State) then
            requeue Lounge_Set_Multiple (not Current) with abort;
         end if;
      end Signal;

      entry Signal
            (  Event     : Event_Type;
               Condition : Abstract_Condition'Class := Always_Signaled
            )  when Empty_Lounges is
      begin
         State (Event) := True;
         Current := not Current;
         if not Satisfied (Condition, State) then
            requeue Lounge_Set_Single (not Current) with abort;
         end if;
      end Signal;

      entry Wait (Condition : Abstract_Condition'Class) when True is
      begin
         if not Satisfied (Condition, State) then
            requeue Lounge_Wait (not Current) with abort;
         end if;
      end Wait;

   end Events_Array;

   function Reset (Event : Event_Type) return Event_Reset is
   begin
      return (Ada.Finalization.Controlled with Event);
   end Reset;

   function Satisfied (Condition : Event_Signaled; State : Events_State)
      return Boolean is
   begin
      return State (Condition.Event);
   end Satisfied;

   function Satisfied (Condition : Always; State : Events_State)
      return Boolean is
   begin
      return True;
   end Satisfied;

   function Satisfied (Condition : Any; State : Events_State)
      return Boolean is
   begin
      for Event in State'Range loop
         if State (Event) then
            return True;
         end if;
      end loop;
      return False;
   end Satisfied;

   function Satisfied (Condition : Each; State : Events_State)
      return Boolean is
   begin
      for Event in State'Range loop
         if not State (Event) then
            return False;
         end if;
      end loop;
      return True;
   end Satisfied;

   function Satisfied (Condition : None; State : Events_State)
      return Boolean is
   begin
      for Event in State'Range loop
         if State (Event) then
            return False;
         end if;
      end loop;
      return True;
   end Satisfied;

   function Satisfied (Condition : Event_Reset; State : Events_State)
      return Boolean is
   begin
      return not State (Condition.Event);
   end Satisfied;

   function Signaled (Event : Event_Type) return Event_Signaled is
   begin
      return (Ada.Finalization.Controlled with Event);
   end Signaled;

   function "or" (Left, Right : Event_Type) return Events_State is
      Result : Events_State := (others => False);
   begin
      Result (Left)  := True;
      Result (Right) := True;
      return Result;
   end "or";

   function "or" (Left : Events_State; Right : Event_Type)
      return Events_State is
      Result : Events_State := Left;
   begin
      Result (Right) := True;
      return Result;
   end "or";

   function "or" (Left : Event_Type; Right : Events_State)
      return Events_State is
      Result : Events_State := Right;
   begin
      Result (Left) := True;
      return Result;
   end "or";

   function "not" (Left : Event_Type) return Events_State is
      Result : Events_State := (others => True);
   begin
      Result (Left)  := False;
      return Result;
   end "not";

end Synchronization.Generic_Events_Array;
