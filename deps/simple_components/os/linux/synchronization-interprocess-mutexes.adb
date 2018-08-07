--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Mutexes                                     Spring, 2018       --
--  Interface                                                         --
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
         Data : Mutex_Data renames Object.Data.all;
         Self : constant int := int (gettid);
      begin
         if Data.Futex = Self then
            Data.Count := Data.Count + 1;
         else
            if Compare_And_Swap
               (  Target  => Data.Futex'Access,
                  Old_Val => 0,
                  New_Val => Self
               )
            then
               Data.Count := 1;
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
         Data : Mutex_Data renames Object.Data.all;
      begin
         return Data.Futex = int (gettid);
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
         return Data.Futex /= 0;
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
   end Map;

   procedure Release (Object : in out Mutex) is
   begin
      declare
         Data : Mutex_Data renames Object.Data.all;
      begin
         if Data.Futex = int (gettid) then
            if Data.Count > 1 then
               Data.Count := Data.Count - 1;
            else
               Data.Count := 0;
               Data.Futex := 0;
               if -1 = futex_wake (Data.Futex'Access, 1) then
                  Raise_From_Errno
                  (  Data_Error'Identity,
                     "Mutex release fault: "
                  );
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
      Started : constant Time := Clock;
   begin
      declare
         Data : Mutex_Data renames Object.Data.all;
         Self : constant int := int (gettid);
      begin
         if Data.Futex = Self then
            Data.Count := Data.Count + 1;
         else
            loop
               if Compare_And_Swap
                  (  Target  => Data.Futex'Access,
                     Old_Val => 0,
                     New_Val => Self
                  )
               then
                  Data.Count := 1;
                  return;
               end if;
               if ( -1
                  =  futex_wait
                     (  Data.Futex'Access,
                        Data.Futex,
                        From_Duration (Timeout - (Clock - Started))
                  )  )
               then
                  case errno is
                     when EAGAIN => -- Changed, try again
                        null;
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
               end if;
            end loop;
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

end Synchronization.Interprocess.Mutexes;
