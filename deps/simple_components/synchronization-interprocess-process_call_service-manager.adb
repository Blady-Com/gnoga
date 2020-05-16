--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Process_Call_Service.Manager                Spring, 2018       --
--  Implementation                                                    --
--                                Last revision :  22:08 06 Jan 2020  --
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
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

with Ada.Unchecked_Deallocation;

package body Synchronization.Interprocess.Process_Call_Service.
             Manager is

   Manager_Init : constant String := "Call service manager is not " &
                                     "initialized or already finalized";

   procedure Enumerate
             (  Stream  : access Root_Stream_Type'Class;
                Manager : Call_Service_Manager
             )  is
   begin
      Enumerate (Stream, Abstract_Shared_Object (Manager));
      for Server_ID in Manager.Services'Range loop
         Enumerate (Stream, Manager.Services (Server_ID).all);
      end loop;
   end Enumerate;

   function Get_Server
            (  Manager : Call_Service_Manager
            )  return Call_Service_Ptr is
   begin
      if (  Manager.Server = 0
         or else Manager.Services (Manager.Server).Data = null
         )
      then
         Raise_Exception (Status_Error'Identity, Manager_Init);
      end if;
      return Manager.Services (Manager.Server);
   end Get_Server;

   function Get_Server_ID
            (  Manager : Call_Service_Manager
            )  return Call_Service_ID is
   begin
      if (  Manager.Server = 0
         or else Manager.Services (Manager.Server).Data = null
         )
      then
         Raise_Exception (Status_Error'Identity, Manager_Init);
      end if;
      return Manager.Server;
   end Get_Server_ID;

   function Get_Service
            (  Manager : Call_Service_Manager;
               ID      : Call_Service_ID
            )  return Call_Service_Ptr is
   begin
      if ID not in 1..Manager.Size then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid call service ID"
         );
      elsif Manager.Server = 0 then
         Raise_Exception (Status_Error'Identity, Manager_Init);
      elsif Manager.Services (ID).Data = null then
         Raise_Exception
         (  Status_Error'Identity,
            (  "Call service"
            &  Call_Service_ID'Image (ID)
            &  " is not initialized or "
            &  "already finalized"
         )  );
      else
         return Manager.Services (ID);
      end if;
   end Get_Service;

   function Get_Service
            (  Manager : Call_Service_Manager;
               ID      : Process_ID
            )  return Call_Service_Ptr is
      use Process_Maps;
      Result : Call_Service_Ptr;
   begin
      if Manager.Server = 0 then
         Raise_Exception (Status_Error'Identity, Manager_Init);
      end if;
      declare
         Self : Call_Service_Manager'Class renames
                Call_Service_Manager'Class (Manager.Self.all);
         Lock : Synchronization.Mutexes.Holder (Self.Lock'Access);
      begin
         declare
            Offset : constant Integer := Find (Manager.Processes, ID);
         begin
            if Offset > 0 then
               declare
                  Index : constant Call_Service_ID :=
                                   Get (Manager.Processes, Offset);
               begin
                  if (  Index in Manager.Services'Range
                     and then
                        Manager.Services (Index).Data /= null
                     )  then
                     return Manager.Services (Index);
                  end if;
               end;
            end if;
         end;
         for Index in Manager.Services'Range loop
            if Manager.Services (Index).Data /= null then
               declare
                  That : constant Process_ID :=
                         Get_Process_ID (Manager.Services (Index).all);
               begin
                  if That /= Null_Process then
                     if That = ID then
                        Result := Manager.Services (Index);
                     end if;
                     Replace (Self.Processes, That, Index);
                  end if;
               end;
            end if;
         end loop;
      end;
      if Result = null then
         Raise_Exception
         (  Status_Error'Identity,
            "Process" & Process_ID'Image (ID) & " is not serviced"
         );
      end if;
      return Result;
   end Get_Service;

   procedure Finalize (Manager : in out Call_Service_Manager) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Call_Service'Class,
                Call_Service_Ptr
             );
   begin
      for Server_ID in Manager.Services'Range loop
         Free (Manager.Services (Server_ID));
      end loop;
   end Finalize;

   function Get_Size
            (  Manager : Call_Service_Manager
            )  return Storage_Count is
   begin
      return 0;
   end Get_Size;

   procedure Initialize (Manager : in out Call_Service_Manager) is
   begin
      for Server_ID in Manager.Services'Range loop
         Manager.Services (Server_ID) :=
            new Call_Service
                (  Manager.Request_Queue_Size,
                   Manager.Request_Stream_Size,
                   Manager.Response_Stream_Size
                );
      end loop;
   end Initialize;

   procedure Map
             (  Manager  : in out Call_Service_Manager;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
      package Mapper is new Generic_Memory_Mapper (Service_Data);
      Offset : System.Address := Location;
   begin
      for Server_ID in Manager.Services'Range loop
         declare
            Service : Call_Service'Class renames
                      Manager.Services (Server_ID).all;
            Data    : Service_Data_Ptr;
         begin
            Data   := Mapper.Map (Offset, Owner).all'Unchecked_Access;
            Offset := Offset + Service.Total_Size;
            if (  Compare_And_Swap
                  (  Data.Index'Access,
                     0,
                     Server_ID + Last_ID - 1
               )  )  then -- Using this slot for the service
               Set_Server (Service);
               Manager.Server := Server_ID;
               return;
            end if;
         end;
      end loop;
      Raise_Exception
      (  Mode_Error'Identity,
         "There is no free service to use for the call server"
      );
   end Map;

   procedure Wait_For_Initialization
             (  Manager  : in out Call_Service_Manager;
                Services : Services_List;
                Timeout  : Duration := Duration'Last
             )  is
      Started : constant Time := Clock;
   begin
      for Index in Services'Range loop
         if Services (Index) not in Manager.Services'Range then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Invalid call service ID"
               &  Call_Service_ID'Image (Services (Index))
            )  );
         end if;
      end loop;
      for Index in Services'Range loop
         declare
             Service : Call_Service'Class renames
                       Manager.Services (Services (Index)).all;
         begin
            while (  Service.Data = null
                  or else
                     Service.Data.Index not in 1..Manager.Size
                  )  loop
               if Clock - Started > Timeout then
                  Raise_Exception
                  (  Timeout_Error'Identity,
                     (  "Call service ID"
                     &  Call_Service_ID'Image (Services (Index))
                     &  " is not ready"
                  )  );
               end if;
               delay 0.01;
            end loop;
         end;
      end loop;
   end Wait_For_Initialization;

   procedure Wait_For_Initialization
             (  Manager : in out Call_Service_Manager;
                Timeout : Duration := Duration'Last
             )  is
      Services : Services_List (1..Natural (Manager.Size));
   begin
      for Index in Services'Range loop
         Services (Index) := Call_Service_ID (Index);
      end loop;
      Wait_For_Initialization (Manager, Services, Timeout);
   end Wait_For_Initialization;

end Synchronization.Interprocess.Process_Call_Service.Manager;
