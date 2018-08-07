--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Events                                      Spring, 2018       --
--  Implementation                                                    --
--                                Last revision :  00:04 22 Jul 2018  --
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

with Ada.Calendar;       use Ada.Calendar;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with System_Errno;       use System_Errno;

package body Synchronization.Interprocess.Events is

   Event_Init : constant String := "Event is not initialized or " &
                                   "already finalized";

   procedure Finalize (Object : in out Event) is
   begin
      null;
   end Finalize;

   function Get_Size (Object : Event) return Storage_Count is
   begin
      return Round (Event_Data'Max_Size_In_Storage_Elements);
   end Get_Size;

   function Is_Signaled (Object : Event) return Boolean is
   begin
      declare
         Data : Event_Data renames Object.Data.all;
      begin
         return Data.Futex > 0;
      end;
   exception
      when Constraint_Error =>
         if Object.Data = null then
            Raise_Exception (Status_Error'Identity, Event_Init);
         else
            raise;
         end if;
   end Is_Signaled;

   procedure Map
             (  Object   : in out Event;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
      package Mapper is new Generic_Memory_Mapper (Event_Data);
   begin
      Object.Data := Mapper.Map (Location, Owner).all'Unchecked_Access;
   end Map;

   procedure Reset (Object : in out Event) is
   begin
      declare
         Data : Event_Data renames Object.Data.all;
      begin
         Data.Futex := 0;
      end;
   exception
      when Constraint_Error =>
         if Object.Data = null then
            Raise_Exception (Status_Error'Identity, Event_Init);
         else
            raise;
         end if;
   end Reset;

   procedure Signal (Object : in out Event) is
   begin
      declare
         Data : Event_Data renames Object.Data.all;
      begin
         Data.Futex := 1;
         if -1 = futex_wake (Data.Futex'Access, int'Last) then
            Raise_From_Errno (Data_Error'Identity, "Wake up fault: ");
         end if;
      end;
   exception
      when Constraint_Error =>
         if Object.Data = null then
            Raise_Exception (Status_Error'Identity, Event_Init);
         else
            raise;
         end if;
   end Signal;

   procedure Wait
             (  Object  : in out Event;
                Timeout : Duration := Duration'Last
             )  is
      Started : constant Time := Clock;
   begin
      declare
         Data : Event_Data renames Object.Data.all;
      begin
         loop
            if ( -1
               =  futex_wait
                  (  Data.Futex'Access,
                     0,
                     From_Duration (Timeout - (Clock - Started))
               )  )
            then
               case errno is
                  when EAGAIN => -- OK
                     exit;
                  when ETIMEDOUT => -- Timed out
                     Raise_Exception
                     (  Timeout_Error'Identity,
                        "Timeout"
                     );
                  when others =>
                     Raise_From_Errno
                     (  Data_Error'Identity,
                        "Wait fault: "
                      );
               end case;
            else
               exit;
            end if;
         end loop;
      end;
   exception
      when Constraint_Error =>
         if Object.Data = null then
            Raise_Exception (Status_Error'Identity, Event_Init);
         else
            raise;
         end if;
   end Wait;

   procedure Wait
             (  Object   : in out Event;
                Timeout  : Duration;
                Signaled : out Boolean
             )  is
      Started : constant Time := Clock;
   begin
      declare
         Data : Event_Data renames Object.Data.all;
      begin
         loop
            if ( -1
               =  futex_wait
                  (  Data.Futex'Access,
                     0,
                     From_Duration (Timeout - (Clock - Started))
               )  )
            then
               case errno is
                  when EAGAIN => -- OK
                     Signaled := True;
                     return;
                  when ETIMEDOUT => -- Timed out
                     Signaled := False;
                     return;
                  when others =>
                     Raise_From_Errno
                     (  Data_Error'Identity,
                        "Wait fault: "
                      );
               end case;
            else
               exit;
            end if;
         end loop;
      end;
   exception
      when Constraint_Error =>
         if Object.Data = null then
            Raise_Exception (Status_Error'Identity, Event_Init);
         else
            raise;
         end if;
   end Wait;

end Synchronization.Interprocess.Events;
