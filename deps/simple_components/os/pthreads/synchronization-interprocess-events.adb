--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Events                                      Spring, 2018       --
--  Implementation                                                    --
--                                Last revision :  01:09 01 May 2018  --
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
         return Data.Set;
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
      if Owner then
         Initialize (Object.Data.Cond'Access);
         Initialize (Object.Data.Mutex'Access);
      end if;
   end Map;

   procedure Reset (Object : in out Event) is
   begin
      declare
         Data : Event_Data renames Object.Data.all;
      begin
         if Data.Set then
            declare
               Lock : Mutex_Holder (Data.Mutex'Access);
            begin
               Data.Set := False;
            end;
         end if;
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
         if not Data.Set then
            declare
               Result : int;
               Lock   : Mutex_Holder (Data.Mutex'Access);
            begin
               Data.Set := True;
               Result := pthread_cond_broadcast (Data.Cond'Access);
               if Result /= 0 then
                  Raise_From_Errno
                  (  Data_Error'Identity,
                     "Signal event broadcast fault: ",
                     Result
                  );
               end if;
            end;
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

   procedure Unmap
             (  Object : in out Event;
                Shared : in out Abstract_Shared_Environment'Class;
                Owner  : Boolean
             )  is
   begin
      if Owner then
         declare
            Result : int;
            Data   : Event_Data renames Object.Data.all;
         begin
            Result := pthread_mutex_destroy (Data.Mutex'Access);
            Result := pthread_cond_destroy  (Data.Cond'Access);
         end;
      end if;
   exception
      when others =>
         null;
   end Unmap;

   procedure Wait
             (  Object  : in out Event;
                Timeout : Duration := Duration'Last
             )  is
   begin
      declare
         Data : Event_Data renames Object.Data.all;
      begin
         if Data.Set then
            return;
         end if;
         declare
            Result   : int;
            Deadline : constant timespec :=
                                From_Duration (Timeout, True);
         begin
            loop
               declare
                  Lock : Mutex_Holder (Data.Mutex'Access);
               begin
                  exit when Data.Set;
                  Result := pthread_cond_timedwait
                            (  Data.Cond'Access,
                               Data.Mutex'Access,
                               Deadline
                            );
                  if Result /= 0 then
                     if Result = ETIMEDOUT then
                        Raise_Exception
                        (  Timeout_Error'Identity,
                           "Timeout"
                        );
                     else
                        Raise_From_Errno
                        (  Data_Error'Identity,
                           "Wait fault: ",
                           Result
                        );
                     end if;
                  end if;
                  exit when Data.Set;
               end;
            end loop;
         end;
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
   begin
      declare
         Data : Event_Data renames Object.Data.all;
      begin
         if Data.Set then
            Signaled := True;
            return;
         end if;
         declare
            Result   : int;
            Deadline : constant timespec :=
                                From_Duration (Timeout, True);
         begin
            loop
               declare
                  Lock : Mutex_Holder (Data.Mutex'Access);
               begin
                  exit when Data.Set;
                  Result := pthread_cond_timedwait
                            (  Data.Cond'Access,
                               Data.Mutex'Access,
                               Deadline
                            );
                  if Result /= 0 then
                     if Result = ETIMEDOUT then
                        Signaled := False;
                        return;
                     else
                        Raise_From_Errno
                        (  Data_Error'Identity,
                           "Wait fault: ",
                           Result
                        );
                     end if;
                  end if;
                  exit when Data.Set;
               end;
            end loop;
         end;
         Signaled := True;
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
