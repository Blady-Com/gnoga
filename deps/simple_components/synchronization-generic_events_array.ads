--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.                            Luebeck            --
--        Generic_Events_Array                     Spring, 2008       --
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
--
--  This  package  provides  arrays of events. Any combination of events
--  from  the  array  can  be  signaled,  reset  or  awaited  for.   The
--  implementation represented here is free of race conditions. When the
--  state of the events is being changed it is guarantee that all  tasks
--  awaiting  for  the  state  are  released before any consequent state
--  change. Another  class  race  condition  problems  is  eliminated by
--  providing atomic signal-then-wait operations. For instance a set  of
--  tasks may synchronize themselves at dedicated points by signaling an
--  event and then awaiting for all events signaled. If the  task  later
--  resets  its event, that would constitute a race condition. Because a
--  task might reset its event before other tasks queue for  all  events
--  set. The following figure illustrates the case:
--
--        task A                 task B
--        |
--        |
--        |
--        | working
--        |______________________.
--                               | working
--                               |
--        .______________________|
--        | signal event A
--        |
--        | wait for events A, B
--        |   (blocked)
--        |______________________.
--                               | signal event B
--        .______________________|
--        |   (released)
--        | reset event A
--        |______________________.
--                               |
--                               | wait for events A, B
--        _______________________| (deadlocked, because A is
--                                  prematurely reset)
--
--  The race condition is between the task A reseting the  event  A  and
--  the task B awaiting for the event A. The condition  is  broken  when
--  each task atomically signals its event and queues for waiting.
--
with Ada.Finalization;

generic
   type Event_Type is (<>);
package Synchronization.Generic_Events_Array is
--
-- Events_State -- The array of event states
--
   type Events_State is array (Event_Type) of Boolean;
   function "or" (Left, Right : Event_Type) return Events_State;
   function "or" (Left : Events_State; Right : Event_Type)
      return Events_State;
   function "or" (Left : Event_Type; Right : Events_State)
      return Events_State;
   function "not" (Left : Event_Type) return Events_State;
--
-- Abstract_Condition -- An  abstract condition to wait for. The entries
--                       of the events array awaits for instances of the
-- types derived from this base type. The derived  type  shall  override
-- the abstract primitive operation Satisfied.
--
   type Abstract_Condition is abstract
      new Ada.Finalization.Controlled with null record;
--
-- Satisfied -- Condition check
--
--    Condition - The check for
--    State     - The current state of the events
--
-- This  function  is  used to check if the condition is satisfied. Note
-- that the function is called in the course of a protected action. That
-- means, that it shall neither block, nor invoke  any  other  protected
-- actions. Further if it accesses shared non-local data, the user shall
-- warranty that these data are either atomic or else are never accessed
-- outside the protected actions of Events_Array.
--
-- Returns :
--
--    True if the condition is satisfied by State
--
   function Satisfied
            (  Condition : Abstract_Condition;
               State     : Events_State
            )  return Boolean is abstract;
--
-- Some frequently used conditions
--
--    Always_Signaled - No wait, the condition is always satisfied
--    All_Signaled    - Satisfied when all events are signaled
--    Any_Signaled    - Satisfied when at least one event is signaled
--    No_Signaled     - Satisfied when no event is signaled
--
   Always_Signaled : constant Abstract_Condition'Class;
   All_Signaled    : constant Abstract_Condition'Class;
   Any_Signaled    : constant Abstract_Condition'Class;
   No_Signaled     : constant Abstract_Condition'Class;
--
-- Event_Signaled -- Satisfied when a particular event is signaled
--
--    Event - The event to wait for
--
-- This condition can be used as follows:
--
--    Flags.Wait (Signaled (Event));
--
   type Event_Signaled (Event : Event_Type) is
      new Abstract_Condition with null record;
   function Satisfied (Condition : Event_Signaled; State : Events_State)
      return Boolean;
   function Signaled (Event : Event_Type) return Event_Signaled;
--
-- Event_Reset -- Satisfied when a particular event is reset
--
--    Event - The event to wait for
--
-- This condition can be used as follows:
--
--    Flags.Wait (Reset (Event));
--
   type Event_Reset (Event : Event_Type) is
      new Abstract_Condition with null record;
   function Satisfied (Condition : Event_Reset; State : Events_State)
      return Boolean;
   function Reset (Event : Event_Type) return Event_Reset;
--
-- Events_Array -- An array of events. Initially all events in the array
--                 are non-signaled.
--
   protected type Events_Array is
   --
   -- Get_State -- The current state of the events
   --
   -- This function returns the state of the array.  The  result  is  an
   -- array which for each event contains True if the event is signaled.
   --
   -- Returns :
   --
   --    The current state of events
   --
      function Get_State return Events_State;
   --
   -- Is_Signaled -- The current event state
   --
   --    Event - The event to get the state of
   --
   -- Returns :
   --
   --    True if the event is signaled
   --
      function Is_Signaled (Event : Event_Type) return Boolean;
   --
   -- Reset -- Event(s) to the non-signaled state
   --
   --    Event[s]  - An array, or else an event
   --    Condition - A condition to wait for after resetting the events
   --
   -- When  the  parameter  is  an  event,  then the event is set to the
   -- non-signaled  state.  When  the  parameter  is an array, then each
   -- event for which the array contains True is set to the non-signaled
   -- state.  The  parameter  Condition  is  the  condition  to wait for
   -- immediately after resetting  the  events.  The  default  value  is
   -- Always_Signaled,  i.e.  Reset  returns  after  changing the events
   -- without waiting.
   --
      entry Reset
            (  Events    : Events_State;
               Condition : Abstract_Condition'Class := Always_Signaled
            );
      entry Reset
            (  Event     : Event_Type;
               Condition : Abstract_Condition'Class := Always_Signaled
            );
   --
   -- Signal -- Event(s)
   --
   --    Event[s]  - An array, or else an event
   --    Condition - A condition to wait for after signaling the events
   --
   -- When  the  parameter  is  an  event,  then the event is set to the
   -- signaled state. When the parameter is an array,  then  each  event
   -- for  which  the  array contains True is set to the signaled state.
   -- The parameter  Condition  specifies  the  condition  to  wait  for
   -- immediately  after  signaling  the  events.  The  default value is
   -- Always_Signaled, i.e. Signal returns  after  changing  the  events
   -- without waiting.
   --
      entry Signal
            (  Events    : Events_State;
               Condition : Abstract_Condition'Class := Always_Signaled
            );
      entry Signal
            (  Event     : Event_Type;
               Condition : Abstract_Condition'Class := Always_Signaled
            );
   --
   -- Set -- Events to a definite state
   --
   --    State     - To set
   --    Condition - A condition to wait for
   --
   -- This  entry  sets  all events according to the value io State. The
   -- parameter  Condition  specifies  the  condition   to   wait   for,
   -- immediately  after  setting  the  events.  The  default  value  is
   -- Always_Signaled,  i.e.  Set  returns  after  changing  the  events
   -- without waiting.
   --
      entry Set
            (  State     : Events_State;
               Condition : Abstract_Condition'Class := Always_Signaled
            );
   --
   -- Wait -- For a condition
   --
   --    Condition - Wait condition
   --
   -- This  entries  waits for Condition. See also Signal, Reset and Set
   -- entries which also are capable of waiting for a certain condition.
   --
      entry Wait (Condition : Abstract_Condition'Class);

   private
      pragma Inline (Is_Signaled);
      pragma Inline (Get_State);
   --
   -- Empty_Lounges -- Check if all current longues are empty
   --
      function Empty_Lounges return Boolean;
      pragma Inline (Empty_Lounges);
   --
   -- Lounge_* -- Waiting rooms for not yet satisfied conditions
   --
      entry Lounge_Set_Multiple (Boolean)
            (  Events    : Events_State;
               Condition : Abstract_Condition'Class
            );
      entry Lounge_Set_Single (Boolean)
            (  Event     : Event_Type;
               Condition : Abstract_Condition'Class
            );
      entry Lounge_Wait (Boolean)
            (  Condition : Abstract_Condition'Class
            );

      Current : Boolean := False;
      State   : Events_State := (others => False);
   end Events_Array;

private
   type Always is new Abstract_Condition with null record;
   function Satisfied (Condition : Always; State : Events_State)
      return Boolean;

   type Any is new Abstract_Condition with null record;
   function Satisfied (Condition : Any; State : Events_State)
      return Boolean;

   type Each is new Abstract_Condition with null record;
   function Satisfied (Condition : Each; State : Events_State)
      return Boolean;

   type None is new Abstract_Condition with null record;
   function Satisfied (Condition : None; State : Events_State)
      return Boolean;

   Always_Signaled : constant Abstract_Condition'Class :=
      Always'(Ada.Finalization.Controlled with null record);
   All_Signaled : constant Abstract_Condition'Class :=
      Each'(Ada.Finalization.Controlled with null record);
   Any_Signaled : constant Abstract_Condition'Class :=
      Any'(Ada.Finalization.Controlled with null record);
   No_Signaled : constant Abstract_Condition'Class :=
      None'(Ada.Finalization.Controlled with null record);

   pragma Inline (Satisfied);
   pragma Inline (Signaled);
   pragma Inline (Reset);
   pragma Inline ("or", "not");

end Synchronization.Generic_Events_Array;
