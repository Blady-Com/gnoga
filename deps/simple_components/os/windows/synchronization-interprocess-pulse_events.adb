--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Pulse_Events                                Spring, 2018       --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

with System.Address_To_Access_Conversions;

package body Synchronization.Interprocess.Pulse_Events is

   procedure Check (Message : String) is
      Code : constant DWORD := GetLastError;
   begin
      if Code = ERROR_INVALID_HANDLE then
         Raise_Exception
         (  Status_Error'Identity,
            (  Message
            &  "Pulse event is not initialized or already finalized"
         )  );
      else
         Raise_From_LastError (Data_Error'Identity, Message, Code);
      end if;
   end Check;

   procedure Finalize (Object : in out Pulse_Event) is
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

   function Get_Size (Object : Pulse_Event) return Storage_Count is
   begin
      return Round (HANDLE'Max_Size_In_Storage_Elements);
   end Get_Size;

   procedure Map
             (  Object   : in out Pulse_Event;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
      package Conversions is
         new System.Address_To_Access_Conversions (HANDLE);
      This : HANDLE renames Conversions.To_Pointer (Location).all;
   begin
      if Owner then
         Object.External := CreateEvent (ManualReset => 0);
         if Object.External = 0 then
            Object.External := INVALID_HANDLE_VALUE;
            Raise_From_LastError
            (  Data_Error'Identity,
               "Event creation fault: ",
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
               "Cannot duplicate a handle to the event: ",
               GetLastError
            );
         end if;
      end if;
   end Map;

   procedure Pulse (Object : in out Pulse_Event) is
   begin
      if 0 = PulseEvent (Object.External) then
         Check ("Pulse event fault: ");
      end if;
   end Pulse;

   procedure Wait
             (  Object  : in out Pulse_Event;
                Timeout : Duration := Duration'Last
             )  is
   begin
      case WaitForSingleObject
           (  Object.External,
              Milliseconds (Timeout)
           )  is
         when WAIT_OBJECT_0 =>
            return;
         when WAIT_TIMEOUT =>
            Raise_Exception (Timeout_Error'Identity, "Timed out");
         when others =>
            Check ("Event wait fault: ");
      end case;
   end Wait;

   procedure Wait
             (  Object   : in out Pulse_Event;
                Timeout  : Duration;
                Signaled : out Boolean
             )  is
   begin
      case WaitForSingleObject
           (  Object.External,
              Milliseconds (Timeout)
           )  is
         when WAIT_OBJECT_0 =>
            Signaled := True;
         when WAIT_TIMEOUT =>
            Signaled := False;
         when others =>
            Check ("Event wait fault: ");
      end case;
   end Wait;

end Synchronization.Interprocess.Pulse_Events;
