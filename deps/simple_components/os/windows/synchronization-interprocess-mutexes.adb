--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Mutexes                                     Spring, 2018       --
--  Interface                                                         --
--                                Last revision :  18:00 18 Aug 2022  --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

with System.Address_To_Access_Conversions;

package body Synchronization.Interprocess.Mutexes is

   procedure Check (Message : String; Code : DWORD := GetLastError) is
   begin
      if Code = ERROR_INVALID_HANDLE then
         Raise_Exception
         (  Status_Error'Identity,
            Message & "Mutex is not initialized or already finalized"
         );
      else
         Raise_From_LastError (Data_Error'Identity, Message, Code);
      end if;
   end Check;

   procedure Finalize (Object : in out Holder) is
   begin
      Release (Object.Resource.all);
   end Finalize;

   procedure Finalize (Object : in out Mutex) is
      Result : BOOL;
   begin
      if Object.External /= INVALID_HANDLE_VALUE then
         Result := CloseHandle (Object.External);
         Object.External := INVALID_HANDLE_VALUE;
      end if;
      Synchronization.Interprocess.Finalize
      (  Abstract_Shared_Object (Object)
      );
   end Finalize;

   function Get_Size (Object : Mutex) return Storage_Count is
   begin
      return Round (HANDLE'Max_Size_In_Storage_Elements);
   end Get_Size;

   function Get_Timeout (Object : Mutex) return Duration is
   begin
      return Duration (Long_Float (Object.Timeout) / 1000.0);
   end Get_Timeout;

   procedure Grab (Object : in out Mutex) is
   begin
      if Object.Owner = Current_Task then
         Object.Count := Object.Count + 1;
      else
         case WaitForSingleObject (Object.External, 0) is
            when WAIT_ABANDONED | WAIT_OBJECT_0 =>
               Object.Owner := Current_Task;
               Object.Count := 1;
            when WAIT_TIMEOUT =>
               return;
            when others =>
               Check ("Grab mutex fault: ");
         end case;
      end if;
   end Grab;

   procedure Initialize (Object : in out Holder) is
   begin
      Seize (Object.Resource.all);
   end Initialize;

   function Is_Mine (Object : Mutex) return Boolean is
   begin
      return Object.Owner = Current_Task;
   end Is_Mine;

   function Is_Owned (Object : Mutex) return Boolean is
      Result : BOOL;
   begin
      return Object.Owner /= Null_Task_Id;
   end Is_Owned;

   procedure Map
             (  Object   : in out Mutex;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
      package Y_Of is
         new System.Address_To_Access_Conversions (HANDLE);
      This : HANDLE renames Y_Of.To_Pointer (Location).all;
   begin
      if Owner then
         Object.External := CreateMutex;
         if Object.External = 0 then
            Object.External := INVALID_HANDLE_VALUE;
            Raise_From_LastError
            (  Data_Error'Identity,
               "Mutex creation fault: ",
               GetLastError
            );
         end if;
         This := Object.External;
      else
         if (  0
            =  DuplicateHandle
               (  SourceProcessHandle => Shared.Server,
                  SourceHandle        => This,
                  TargetProcessHandle => GetCurrentProcess,
                  TargetHandle        =>
                     Object.External'Unchecked_Access
            )  )
         then
            Raise_From_LastError
            (  Data_Error'Identity,
               "Cannot duplicate a handle to the mutex: ",
               GetLastError
            );
         end if;
      end if;
   end Map;

   procedure Release (Object : in out Mutex) is
   begin
      if Object.Owner = Current_Task then
         Object.Count := Object.Count - 1;
         if Object.Count = 0 then
            Object.Owner := Null_Task_Id;
            if 0 = ReleaseMutex (Object.External) then
               Object.Owner := Null_Task_Id;
               declare
                  Code : constant DWORD := GetLastError;
               begin
                  Check ("Release mutex fault: ", Code);
               end;
            end if;
         end if;
      elsif Object.Owner = Null_Task_Id then
         Raise_Exception
         (  Ownership_Error'Identity,
            "Mutex is not owned"
         );
      else
         Raise_Exception
         (  Ownership_Error'Identity,
            "Mutex is owned by anothers task"
         );
      end if;
   end Release;

   procedure Seize (Object : in out Mutex) is
   begin
      if Object.Owner = Current_Task then
         Object.Count := Object.Count + 1;
      else
         case WaitForSingleObject (Object.External, Object.Timeout) is
            when WAIT_ABANDONED | WAIT_OBJECT_0 =>
               Object.Owner := Current_Task;
               Object.Count := 1;
            when WAIT_TIMEOUT =>
               Raise_Exception (Timeout_Error'Identity, "Timed out");
            when others =>
               Check ("Seize mutex fault: ");
         end case;
      end if;
   end Seize;

   procedure Seize (Object : in out Mutex; Timeout : Duration) is
   begin
      if Object.Owner = Current_Task then
         Object.Count := Object.Count + 1;
      else
         case WaitForSingleObject
              (  Object.External,
                 Milliseconds (Timeout)
              )  is
            when WAIT_ABANDONED | WAIT_OBJECT_0 =>
               Object.Owner := Current_Task;
               Object.Count := 1;
            when WAIT_TIMEOUT =>
               Raise_Exception (Timeout_Error'Identity, "Timed out");
            when others =>
               Check ("Seize mutex fault: ");
         end case;
      end if;
   end Seize;

   procedure Set_Timeout
             (  Object  : in out Mutex;
                Timeout : Duration
             )  is
   begin
      Object.Timeout := Milliseconds (Timeout);
   end Set_Timeout;

end Synchronization.Interprocess.Mutexes;
