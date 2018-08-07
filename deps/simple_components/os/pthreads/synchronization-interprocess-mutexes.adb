--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Mutexes                                     Spring, 2018       --
--  Interface                                                         --
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

package body Synchronization.Interprocess.Mutexes is

   Mutex_Init : constant String := "Mutex is not initialized or " &
                                   "already finalized";

   procedure Finalize (Object : in out Holder) is
   begin
      Release (Object.Resource.all);
   end Finalize;

   procedure Finalize (Object : in out Mutex) is
   begin
      null;
   end Finalize;

   function Get_Size (Object : Mutex) return Storage_Count is
   begin
      return Round (Mutex_Data'Max_Size_In_Storage_Elements);
   end Get_Size;

   function Get_Timeout (Object : Mutex) return Duration is
   begin
      return Object.Timeout;
   end Get_Timeout;

   procedure Grab (Object : in out Mutex) is
   begin
      declare
         Self   : aliased Unsigned_64;
         Result : int;
         Data   : Mutex_Data renames Object.Data.all;
      begin
         Result := pthread_threadid_np (0, Self'Access);
         if Data.Owner = Self then -- We already own this mutex
            Data.Count := Data.Count + 1;
         else
            Result := pthread_mutex_trylock (Data.Mutex'Access);
            if Result /= 0 then
               if Result /= EBUSY then -- Cannot get it
                  Raise_From_Errno
                  (  Data_Error'Identity,
                     "Grab fault: ",
                     Result
                  );
               end if;
            else
               Data.Count := 1;
               Data.Owner := Self;
            end if;
         end if;
      end;
   exception
      when Constraint_Error =>
         if Object.Data = null then
            Raise_Exception (Status_Error'Identity, Mutex_Init);
         else
            raise;
         end if;
   end Grab;

   procedure Initialize (Object : in out Holder) is
   begin
      Seize (Object.Resource.all);
   end Initialize;

   function Is_Mine (Object : Mutex) return Boolean is
   begin
      declare
         Result : int;
         Data   : Mutex_Data renames Object.Data.all;
         Self   : aliased Unsigned_64;
      begin
         Result := pthread_threadid_np (0, Self'Access);
         return Data.Owner = Self;
      end;
   exception
      when Constraint_Error =>
         if Object.Data = null then
            Raise_Exception (Status_Error'Identity, Mutex_Init);
         else
            raise;
         end if;
   end Is_Mine;

   function Is_Owned (Object : Mutex) return Boolean is
   begin
      declare
         Data : Mutex_Data renames Object.Data.all;
      begin
         return Data.Owner /= 0;
      end;
   exception
      when Constraint_Error =>
         if Object.Data = null then
            Raise_Exception (Status_Error'Identity, Mutex_Init);
         else
            raise;
         end if;
   end Is_Owned;

   procedure Map
             (  Object   : in out Mutex;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
      package Mapper is new Generic_Memory_Mapper (Mutex_Data);
   begin
      Object.Data := Mapper.Map (Location, Owner).all'Unchecked_Access;
      if Owner then
         Initialize (Object.Data.Mutex'Access);
      end if;
   end Map;

   procedure Release (Object : in out Mutex) is
   begin
      declare
         Result : int;
         Data   : Mutex_Data renames Object.Data.all;
         Self   : aliased Unsigned_64;
      begin
         Result := pthread_threadid_np (0, Self'Access);
         if Data.Owner = Self then -- We own the mutex
            if Data.Count > 1 then
               Data.Count := Data.Count - 1;
            else
               Data.Count := 0;
               Data.Owner := 0;
               Result := pthread_mutex_unlock (Data.Mutex'Access);
               if Result /= 0 then
                  if Result = EPERM then
                     Raise_Exception
                     (  Ownership_Error'Identity,
                        "Mutex is not owned by this task"
                     );
                  else
                     Raise_From_Errno
                     (  Data_Error'Identity,
                        "Mutex release fault: ",
                        Result
                     );
                  end if;
               end if;
            end if;
         else
            Raise_Exception
            (  Ownership_Error'Identity,
               "Mutex is not owned by this task"
            );
         end if;
      end;
   exception
      when Constraint_Error =>
         if Object.Data = null then
            Raise_Exception (Status_Error'Identity, Mutex_Init);
         else
            raise;
         end if;
   end Release;

   procedure Seize (Object : in out Mutex) is
   begin
      Seize (Object, Object.Timeout);
   end Seize;

   procedure Seize (Object : in out Mutex; Timeout : Duration) is
      Result : int;
   begin
      declare
         Data : Mutex_Data renames Object.Data.all;
         Self : aliased Unsigned_64;
      begin
         Result := pthread_threadid_np (0, Self'Access);
         if Data.Owner = Self then -- We already own this mutex
            Data.Count := Data.Count + 1;
         else
            if Timeout = Duration'Last then -- Infinite wait
               Result := pthread_mutex_lock (Data.Mutex'Access);
            else
               Result := pthread_mutex_timedlock
                         (  Data.Mutex'Access,
                            From_Duration (Timeout, True)
                         );
            end if;
            if Result /= 0  then
               if Result = ETIMEDOUT then -- Timed out
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
            Data.Owner := Self;
            Data.Count := 1;
         end if;
      end;
   exception
      when Constraint_Error =>
         if Object.Data = null then
            Raise_Exception (Status_Error'Identity, Mutex_Init);
         else
            raise;
         end if;
   end Seize;

   procedure Set_Timeout
             (  Object  : in out Mutex;
                Timeout : Duration
             )  is
   begin
      Object.Timeout := Timeout;
   end Set_Timeout;

   procedure Unmap
             (  Object : in out Mutex;
                Shared : in out Abstract_Shared_Environment'Class;
                Owner  : Boolean
             )  is
   begin
      if Owner then
         declare
            Result : int;
            Data   : Mutex_Data renames Object.Data.all;
         begin
            Result := pthread_mutex_destroy (Data.Mutex'Access);
         end;
      end if;
   exception
      when others =>
         null;
   end Unmap;

end Synchronization.Interprocess.Mutexes;
