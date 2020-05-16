--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Generic_Process_Call_Service                Spring, 2018       --
--  Implementation                                                    --
--                                Last revision :  14:52 29 Feb 2020  --
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

with Ada.Calendar;             use Ada.Calendar;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Ada.Tags;                 use Ada.Tags;
with Ada.Task_Identification;  use Ada.Task_Identification;
with Strings_Edit.Floats;      use Strings_Edit.Floats;
with Strings_Edit.Integers;    use Strings_Edit.Integers;

with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

--  with Ada.Text_IO, Synchronization.Mutexes;
--  with GNAT.Exception_Actions, GNAT.Traceback.Symbolic;

package body Synchronization.Interprocess.Process_Call_Service is

   Caller_Init  : constant String := "The caller's service is not " &
                                     "started or alredy stopped";
   Callee_Init  : constant String := "The callee's service is not " &
                                     "started or alredy stopped";
   Diff_Service : constant String := "The caller and callee belong " &
                                     "to different call services";
   Method_Init  : constant String := "Method is not initialized or " &
                                     "already finalized";
   Service_Init : constant String := "Call service is not " &
                                     "initialized or already finalized";
   Same_Process : constant String := "Caller and callee belong to " &
                                     "the same process";
--
-- No -- The unique request sequence number
--
   No : aliased Sequence_No := 0;
--
-- Main -- Environment_Task.  Unfortunately the older versions of Ada do
--         not have  it  defined  in  Ada.Task_Identification.  We  use
--         Current_Task while the package elaboration to get it.
--
   Main : Task_ID;

--     Trace_Lock   : aliased Synchronization.Mutexes.Mutex;
--     Trace_File   : Ada.Text_IO.File_Type;
--     Trace_Open   : Boolean := False;

--     function "&" (Left : String; Right : Duration) return String is
--     begin
--        return Left & Image (Float (Right), AbsSmall => -3) & "ms";
--     end "&";
--
--     function "&" (Left : String; Right : Storage_Offset) return String is
--     begin
--        return Left & Image (Integer (Right));
--     end "&";
--
--     procedure Trace_Exception_Action
--               (  Occurrence : Exception_Occurrence
--               );
--
--     procedure Trace (ID : Call_Service_ID; Text : String) is
--        use Ada.Strings.Fixed;
--        use Ada.Text_IO;
--        use GNAT.Exception_Actions;
--        Lock : Synchronization.Mutexes.Holder (Trace_Lock'Access);
--     begin
--        if not Trace_Open then
--           Register_Global_Action (Trace_Exception_Action'Access);
--           Create
--           (  Trace_File,
--              Out_File,
--              "d:\temp\test_" & Image (Integer (ID)) & ".log"
--           );
--           Trace_Open := True;
--        end if;
--        declare
--           Prefix : constant String := Image (Current_Task);
--        begin
--           Put_Line
--           (  Trace_File,
--              (  Prefix
--              &  "| "
--              &  (  (  (  Character'Pos (Prefix (Prefix'First))
--                       +  Character'Pos (Prefix (Prefix'Last))
--                       )
--                    mod
--                       5
--                    )
--                 *  "   "
--                 )
--              &  Text
--           )  );
--        end;
--        Flush (Trace_File);
--     end Trace;
--
--     procedure Trace_Exception_Action
--               (  Occurrence : Exception_Occurrence
--               )  is
--        use GNAT.Traceback.Symbolic;
--     begin
--        Trace
--        (  0,
--           "++! " & Exception_Information (Occurrence)
--        );
--     end Trace_Exception_Action;

   function "=" (Left, Right : Request_Header) return Boolean is
   begin
      return Left.No = Right.No;
   end "=";

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Object : Call_Service
             )  is
   begin
      Enumerate (Stream, Abstract_Shared_Object (Object));
      Event'Write (Stream, Object.Requests.Not_Full);
      Event'Write (Stream, Object.Requests.Not_Empty);
      Mutex'Write (Stream, Object.Requests.Lock);
      Universal_FIFO'Write (Stream, Object.Queue);
      Universal_Stream'Write (Stream, Object.Requests.Stream);
      Event'Write (Stream, Object.Responses.Not_Full);
      Event'Write (Stream, Object.Responses.Not_Empty);
      Mutex'Write (Stream, Object.Responses.Lock);
      Universal_Stream'Write (Stream, Object.Responses.Stream);
      Enumerate (Stream, Object.Epilogue);
      Call_Service'Class (Object.Self.all).Total_Size :=
         (  Get_Offset (Object.Responses.Stream)
         -  Get_Offset (Object)
         +  Get_Size   (Object)
         );
   end Enumerate;

   procedure Finalize (Lock : in out Lock_Holder) is
   begin
      if Lock.Owner then
--           Trace
--           (  Get_ID (Lock.Stream.Service.all),
--              (  "releasing results mutex of "
--              &  Get_Offset (Lock.Stream.Service.Responses.Stream)
--           )  );
         Lock.Owner := False;
         Release (Lock.Stream.Service.Responses.Lock);
      end if;
   end Finalize;

   procedure Read_Response_Header
             (  Service : in out Call_Service'Class;
                Header  : in out Request_Header;
                Started : Time;
                Timeout : Duration
             )  is
       Empty   : Boolean;
       Current : Request_Header;
   begin
      loop
--           Trace
--           (  Get_ID (Service),
--              (  "wait for results "
--              &  (Timeout - (Clock - Started))
--              &  " "
--              &  Image (Header)
--              &  " from "
--              &  Get_Offset (Service.Responses.Stream)
--           )  );
         select
            Service.Response.Wait_For_Results (Header, Empty);
         or delay Timeout - (Clock - Started);
            Raise_Exception
            (  Timeout_Error'Identity,
               "Timed out awaiting for the response"
            );
         end select;
         exit when not Empty; -- Have header read by another task
         --
         -- Reading the header
         --
--           Trace
--           (  Get_ID (Service),
--              (  "wait for response header "
--              &  (Timeout - (Clock - Started))
--              &  " "
--              &  Image (Header)
--              &  " from "
--              &  Get_Offset (Service.Responses.Stream)
--           )  );
         Wait_For_Not_Empty
         (  Service.Responses.Stream,
            Timeout - (Clock - Started),
            Empty
         );
         if Empty then
            Raise_Exception
            (  Timeout_Error'Identity,
               "No response from the call server"
            );
         end if;
         begin
            Request_Header'Read
            (  Service.Responses.Stream'Access,
               Current
            );
            Empty := False;
         exception
            when Timeout_Error =>
               Raise_Exception
               (  Timeout_Error'Identity,
                  "Response header reading timeout"
               );
         end;
--           Trace
--           (  Get_ID (Service),
--              (  "response "
--              &  Image (Current.From)
--              &  "->"
--              &  Image (Get_ID (Service))
--              &  " header "
--              &  Image (Current)
--              &  " from "
--              &  Get_Offset (Service.Responses.Stream)
--           )  );
         if Current.Method not in Service.Methods'Range then
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid method in the response header"
            );
         end if;
         if Header = Current then -- The header is of the response to us
            Header.Mode := Current.Mode;
            return;
         end if;
          -- The header is for another task
--           Trace (Get_ID (Service), "pass response to another task");
         Service.Response.Accept_Results (Current);
      end loop;
   exception
      when others =>
         Service.Response.Cancel;
         Close (Service.Requests.Stream);
         raise;
   end Read_Response_Header;

   procedure Generic_Complete
             (  Method : in out Abstract_Method'Class;
                Caller : in out Call_Service'Class;
                No     : Sequence_No
             )  is
      Results : Results_Stream (Caller'Access);
   begin
      Results.Header.Mode   := Success_Response;
      Results.Header.Method := Get_ID (Method);
      Results.Header.From   := Get_ID (Get_Service (Method).all);
      Results.Header.No     := No;
--                            Trace
--                            (  Results.Header.From,
--                               (  "execute "
--                               &  Image (Get_ID (Caller))
--                               &  "->"
--                               &  Image (Results.Header.From)
--                               &  " from "
--                               &  Get_Offset (Caller.Responses.Stream)
--                            )  );
      Complete
      (  Method     => Method,
         Results    => Results,
         Caller     => Caller,
         No         => No
      );
   exception
      when Error : others =>
         declare
            Stream : Universal_Stream renames
                     Caller.Responses.Stream;
            Lock   : Holder (Caller.Responses.Lock'Access);
         begin
            Results.Header.Mode := Failure_Response;
            Request_Header'Write (Stream'Access, Results.Header);
            Exception_Id'Write
            (  Stream'Access,
               Exception_Identity (Error)
            );
            String'Output (Stream'Access, Exception_Message (Error));
         exception
            when others =>
               null;
         end;
   end Generic_Complete;

   function Generic_Function_Call
            (  Method  : Abstract_Method'Class;
               Callee  : Call_Service'Class;
               Timeout : Duration := Duration'Last
            )  return Result_Type is
   begin
      if Method.Service = null or else Method.Index = 0 then
         Raise_Exception (Status_Error'Identity, Method_Init);
      elsif (  Method.Service.Data = null
            or else
               Method.Service.Data.Index not in 1..Last_ID
            )  then
         Raise_Exception (Status_Error'Identity, Caller_Init);
      elsif (  Callee.Data = null
            or else
               Callee.Data.Index not in 1..Last_ID
            )  then
         Raise_Exception (Status_Error'Identity, Callee_Init);
      elsif Method.Service.Parent /= Callee.Parent then
         Raise_Exception (Use_Error'Identity, Diff_Service);
      elsif Method.Service.Data.Index = Callee.Data.Index then
         Raise_Exception (Use_Error'Identity, Same_Process);
      end if;
      declare
         Caller  : Call_Service'Class renames Method.Service.all;
         Target  : Call_Service'Class renames
                   Method.Service.Servers (Callee.Data.Index).all;
         Started : constant Time  := Clock;
         Header  : Request_Header :=
                   (  Mode   => Synchronous_Request,
                      Method => Method.Index,
                      From   => Caller.Data.Index,
                      No     => Initiate (Caller'Access)
                   );
      begin
--           Trace
--           (  Get_ID (Caller),
--              (  "do function call "
--              &  Image (Get_ID (Caller))
--              &  "->"
--              &  Image (Get_ID (Target))
--           )  );
         Seize (Target.Requests.Lock, Timeout);
         begin
--              Trace
--              (  Get_ID (Caller),
--                 (  "queue header "
--                 &  Image (Get_ID (Caller))
--                 &  "->"
--                 &  Image (Get_ID (Target))
--                 &  " "
--                 &  Image (Header)
--                 &  " to "
--                 &  Get_Offset (Target.Queue)
--              )  );
            if Is_Closed (Target.Requests.Stream) then
               Raise_Exception (Status_Error'Identity, Callee_Init);
            end if;
            Put (Target.Queue, Header, Timeout - (Clock - Started));
--              Trace
--              (  Get_ID (Caller),
--                 (  "write parameters "
--                 &  Image (Get_ID (Caller))
--                 &  "->"
--                 &  Image (Get_ID (Target))
--                 &  " to "
--                 &  Get_Offset (Target.Requests.Stream)
--              )  );
            Send_Parameters (Target.Requests.Stream);
--              Trace
--              (  Get_ID (Caller),
--                 (  "write parameters done "
--                 &  Image (Get_ID (Caller))
--                 &  "->"
--                 &  Image (Get_ID (Target))
--                 &  " to "
--                 &  Get_Offset (Target.Requests.Stream)
--              )  );
            Release (Target.Requests.Lock);
         exception
            when End_Error =>
               Release (Target.Requests.Lock);
               Raise_Exception (Status_Error'Identity, Caller_Init);
            when others =>
               Release (Target.Requests.Lock);
               raise;
         end;
         begin
            Read_Response_Header (Caller, Header, Started, Timeout);
         exception
            when End_Error =>
               Raise_Exception (Status_Error'Identity, Callee_Init);
         end;
         begin
            case Header.Mode is
               when Failure_Response => -- Error
--                    Trace
--                    (  Get_ID (Caller),
--                       (  "reading exception occurrence "
--                       &  Image (Get_ID (Caller))
--                       &  "->"
--                       &  Image (Get_ID (Target))
--                       &  " from "
--                       &  Get_Offset (Caller.Responses.Stream)
--                    )  );
                  declare -- Receive exception ID and message
                     Responses : Universal_Stream renames
                                 Caller.Responses.Stream;
                     ID        : constant Exception_ID :=
                                 Exception_ID'Input (Responses'Access);
                     Message   : constant String :=
                                 String'Input (Responses'Access);
                  begin
                     Raise_Exception (ID, Message);
                  end;
               when Success_Response => -- Success, receive results
--                    Trace
--                    (  Get_ID (Caller),
--                       (  "reading result "
--                       &  Image (Get_ID (Caller))
--                       &  "->"
--                       &  Image (Get_ID (Target))
--                       &  " from "
--                       &  Get_Offset (Caller.Responses.Stream)
--                    )  );
                  declare
                      Result : constant Result_Type :=
                               Result_Type'Input
                               (  Caller.Responses.Stream'Access
                               );
                  begin
--                       Trace
--                       (  Get_ID (Caller),
--                          (  "result read "
--                          &  Image (Get_ID (Caller))
--                          &  "->"
--                          &  Image (Get_ID (Target))
--                          &  " from "
--                          &  Get_Offset (Caller.Responses.Stream)
--                       )  );
                     Caller.Response.Complete;
                     return Result;
                  end;
               when others => -- Cancel
                  Raise_Exception
                  (  Status_Error'Identity,
                     "Call canceled by the server"
                  );
            end case;
         exception
            when End_Error =>
               Caller.Response.Complete;
               Raise_Exception (Status_Error'Identity, Callee_Init);
            when others =>
               Caller.Response.Complete;
               raise;
         end;
      end;
   end Generic_Function_Call;

   procedure Generic_Post
             (  Method  : Abstract_Method'Class;
                Callee  : Call_Service'Class;
                Timeout : Duration := Duration'Last
             )  is
   begin
      if Method.Service = null or else Method.Index = 0 then
         Raise_Exception (Status_Error'Identity, Method_Init);
      elsif (  Method.Service.Data = null
            or else
               Method.Service.Data.Index not in 1..Last_ID
            )  then
         Raise_Exception (Status_Error'Identity, Caller_Init);
      elsif (  Callee.Data = null
            or else
               Callee.Data.Index not in 1..Last_ID
            )  then
         Raise_Exception (Status_Error'Identity, Callee_Init);
      elsif Method.Service.Parent /= Callee.Parent then
         Raise_Exception (Use_Error'Identity, Diff_Service);
      elsif Method.Service.Data.Index = Callee.Data.Index then
         Raise_Exception (Use_Error'Identity, Same_Process);
      end if;
      declare
         Caller  : Call_Service'Class renames Method.Service.all;
         Target  : Call_Service'Class renames
                   Method.Service.Servers (Callee.Data.Index).all;
         Started : constant Time  := Clock;
         Header  : constant Request_Header :=
                            (  Mode   => Synchronous_Request,
                               Method => Method.Index,
                               From   => Caller.Data.Index,
                               No     => Initiate (Caller'Access)
                            );
      begin
--           Trace
--           (  Get_ID (Caller),
--              (  "do asynchronous call "
--              &  Image (Get_ID (Caller))
--              &  " ->"
--              &  Image (Get_ID (Target))
--           )  );
         Seize (Target.Requests.Lock, Timeout);
         begin
--              Trace
--              (  Get_ID (Caller),
--                 (  "queue header "
--                 &  Image (Get_ID (Caller))
--                 &  "->"
--                 &  Image (Get_ID (Target))
--                 &  " "
--                 &  Image (Header)
--                 &  " to "
--                 &  Get_Offset (Target.Queue)
--              )  );
            if Is_Closed (Target.Requests.Stream) then
               Raise_Exception (Status_Error'Identity, Callee_Init);
            end if;
            Put (Target.Queue, Header, Timeout - (Clock - Started));
--              Trace
--              (  Get_ID (Caller),
--                 (  "write parameters "
--                 &  Image (Get_ID (Caller))
--                 &  "->"
--                 &  Image (Get_ID (Target))
--                 &  " to "
--                 &  Get_Offset (Target.Requests.Stream)
--              )  );
            Send_Parameters (Target.Requests.Stream);
--              Trace
--              (  Get_ID (Caller),
--                 (  "write parameters done "
--                 &  Image (Get_ID (Caller))
--                 &  "->"
--                 &  Image (Get_ID (Target))
--                 &  " to "
--                 &  Get_Offset (Target.Requests.Stream)
--              )  );
            Release (Target.Requests.Lock);
         exception
            when End_Error =>
               Release (Target.Requests.Lock);
               Raise_Exception (Status_Error'Identity, Caller_Init);
            when others =>
               Release (Target.Requests.Lock);
               raise;
         end;
      end;
   end Generic_Post;

   procedure Generic_Procedure_Call
             (  Method  : Abstract_Method'Class;
                Callee  : Call_Service'Class;
                Timeout : Duration := Duration'Last
             )  is
   begin
      if Method.Service = null or else Method.Index = 0 then
         Raise_Exception (Status_Error'Identity, Method_Init);
      elsif (  Method.Service.Data = null
            or else
               Method.Service.Data.Index not in 1..Last_ID
            )  then
         Raise_Exception (Status_Error'Identity, Caller_Init);
      elsif (  Callee.Data = null
            or else
               Callee.Data.Index not in 1..Last_ID
            )  then
         Raise_Exception (Status_Error'Identity, Callee_Init);
      elsif Method.Service.Parent /= Callee.Parent then
         Raise_Exception (Use_Error'Identity, Diff_Service);
      elsif Method.Service.Data.Index = Callee.Data.Index then
         Raise_Exception (Use_Error'Identity, Same_Process);
      end if;
      declare
         Caller  : Call_Service'Class renames Method.Service.all;
         Target  : Call_Service'Class renames
                   Method.Service.Servers (Callee.Data.Index).all;
         Started : constant Time  := Clock;
         Header  : Request_Header :=
                   (  Mode   => Synchronous_Request,
                      Method => Method.Index,
                      From   => Caller.Data.Index,
                      No     => Initiate (Caller'Access)
                   );
      begin
--           Trace
--           (  Get_ID (Caller),
--              (  "do procedure call "
--              &  Image (Get_ID (Caller))
--              &  "->"
--              &  Image (Get_ID (Target))
--           )  );
         Seize (Target.Requests.Lock, Timeout);
         begin
--              Trace
--              (  Get_ID (Caller),
--                 (  "queue header "
--                 &  Image (Get_ID (Caller))
--                 &  "->"
--                 &  Image (Get_ID (Target))
--                 &  " "
--                 &  Image (Header)
--                 &  " to "
--                 &  Get_Offset (Target.Queue)
--              )  );
            if Is_Closed (Target.Requests.Stream) then
               Raise_Exception (Status_Error'Identity, Callee_Init);
            end if;
            Put (Target.Queue, Header, Timeout - (Clock - Started));
--              Trace
--              (  Get_ID (Caller),
--                 (  "write parameters "
--                 &  Image (Get_ID (Caller))
--                 &  "->"
--                 &  Image (Get_ID (Target))
--                 &  " to "
--                 &  Get_Offset (Target.Requests.Stream)
--              )  );
            Send_Parameters (Target.Requests.Stream);
--              Trace
--              (  Get_ID (Caller),
--                 (  "write parameters done "
--                 &  Image (Get_ID (Caller))
--                 &  "->"
--                 &  Image (Get_ID (Target))
--                 &  " to "
--                 &  Get_Offset (Target.Requests.Stream)
--              )  );
            Release (Target.Requests.Lock);
         exception
            when End_Error =>
               Release (Target.Requests.Lock);
               Raise_Exception (Status_Error'Identity, Caller_Init);
            when others =>
               Release (Target.Requests.Lock);
               raise;
         end;
         begin
            Read_Response_Header (Caller, Header, Started, Timeout);
         exception
            when End_Error =>
               Raise_Exception (Status_Error'Identity, Callee_Init);
         end;
         begin
            if Header.Mode = Failure_Response then -- Error
--                 Trace
--                 (  Get_ID (Caller),
--                    (  "reading exception occurrence "
--                    &  Image (Get_ID (Caller))
--                    &  "->"
--                    &  Image (Get_ID (Target))
--                 )  );
               declare -- Receive exception ID and message
                  Responses : Universal_Stream renames
                              Caller.Responses.Stream;
                  ID        : constant Exception_ID :=
                                 Exception_ID'Input (Responses'Access);
                  Message   : constant String :=
                                 String'Input (Responses'Access);
               begin
                  Raise_Exception (ID, Message);
               end;
            else -- Success
--                 Trace
--                 (  Get_ID (Caller),
--                    (  "reading results "
--                    &  Image (Get_ID (Caller))
--                    &  "->"
--                    &  Image (Get_ID (Target))
--                    &  " from "
--                    &  Get_Offset (Caller.Responses.Stream)
--                 )  );
               Receive_Results (Caller.Responses.Stream);
               Caller.Response.Complete;
               return;
            end if;
         exception
            when End_Error =>
               Caller.Response.Complete;
               Raise_Exception (Status_Error'Identity, Callee_Init);
            when others =>
               Caller.Response.Complete;
               raise;
         end;
      end;
   end Generic_Procedure_Call;

   function Get_ID (Service : Call_Service) return Call_Service_ID is
   begin
      if Service.Data = null then
         Raise_Exception (Status_Error'Identity, Service_Init);
      else
         return Service.Data.Index;
      end if;
   end Get_ID;

   function Get_ID (Method : Abstract_Method) return Method_ID is
   begin
      if Method.Index = Null_Method_ID then
         Raise_Exception (Status_Error'Identity, Method_Init);
      else
         return Method.Index;
      end if;
   end Get_ID;

   function Get_Process_ID (Service : Call_Service) return Process_ID is
   begin
      if Service.Data = null then
         Raise_Exception (Status_Error'Identity, Service_Init);
      else
         return Service.Data.Process;
      end if;
   end Get_Process_ID;

   function Get_Service
            (  Method : Abstract_Method;
               ID     : Call_Service_ID
            )  return Call_Service_Ptr is
   begin
      if Method.Service = null then
         Raise_Exception (Status_Error'Identity, Method_Init);
      elsif Method.Service.Data = null then
         Raise_Exception (Status_Error'Identity, Service_Init);
      elsif ID not in Method.Service.Servers'Range then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid call service ID"
         );
      else
         return Method.Service.Servers (ID);
      end if;
   end Get_Service;

   function Get_Service
            (  Method : Abstract_Method
            )  return Call_Service_Ptr is
   begin
      if Method.Service = null then
         Raise_Exception (Status_Error'Identity, Method_Init);
      elsif Method.Service.Data = null then
         Raise_Exception (Status_Error'Identity, Service_Init);
      else
         return Method.Service;
      end if;
   end Get_Service;

   function Get_Service_ID (Method : Abstract_Method)
      return Call_Service_ID is
   begin
      if Method.Service = null then
         Raise_Exception (Status_Error'Identity, Method_Init);
      elsif Method.Service.Data = null then
         Raise_Exception (Status_Error'Identity, Service_Init);
      else
         return Method.Service.Data.Index;
      end if;
   end Get_Service_ID;

   function Get_Size (Object : Call_Service_Stop)
      return Storage_Count is
   begin
      return 0;
   end Get_Size;

   function Get_Size (Object : Call_Service) return Storage_Count is
   begin
      return Round (Service_Data'Max_Size_In_Storage_Elements);
   end Get_Size;

   function Get_Size (Object : Abstract_Method) return Storage_Count is
   begin
      return 0;
   end Get_Size;

   function Image (ID : Call_Service_ID) return String is
      Result : constant String := Call_Service_ID'Image (ID);
   begin
      return Result (Result'First + 1..Result'Last);
   end Image;

   function Image (ID : Method_ID) return String is
      Result : constant String := Method_ID'Image (ID);
   begin
      return Result (Result'First + 1..Result'Last);
   end Image;

   function Image
            (  Request   : Request_Header;
               Show_Mode : Boolean := True
            )  return String is
      Text : constant String := Sequence_No'Image (Request.No);
   begin
      if Show_Mode then
         return
         (  Request_Type'Image (Request.Mode)
         &  "->"
         &  Image (Request.Method)
         &  ":"
         &  Text (Text'First + 1..Text'Last)
         );
      else
         return
         (  Image (Request.Method)
         &  ":"
         &  Text (Text'First + 1..Text'Last)
         );
      end if;
   end Image;

   function Initiate (Caller : access Call_Service)
      return Sequence_No is
      function Increment
               (  Accumulator : access Sequence_No;
                  Value       : Sequence_No
               )  return Sequence_No;
      pragma Import
             (  Intrinsic,
                Increment,
                "__sync_add_and_fetch_8"
             );
   begin
      return Increment (No'Access, 1);
   end Initiate;

   function Is_Server (Service : Call_Service) return Boolean is
   begin
      return Service.Mode = Server_Mode;
   end Is_Server;

   function Is_Synchronous (Method : Abstract_Method) return Boolean is
   begin
      return True;
   end Is_Synchronous;

   procedure Map
             (  Object   : in out Call_Service_Stop;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
   begin
      null;
   end Map;

   procedure Map
             (  Object   : in out Call_Service;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
      package Mapper is new Generic_Memory_Mapper (Service_Data);
   begin
      Object.Data := Mapper.Map (Location, Owner).all'Unchecked_Access;
      Object.Parent := Shared'Unchecked_Access;
      case Object.Mode is
         when Server_Mode =>
            Set_Mode (Object.Queue,            Out_End);
            Set_Mode (Object.Requests.Stream,  In_End);
            Set_Mode (Object.Responses.Stream, In_End);
            declare -- Enumerate methods and servers
               This        : Abstract_Shared_Object_Ptr := Shared.First;
               Method_No   : Method_ID       := 0;
               Server_No   : Call_Service_ID := 0;
               Server_ID   : Call_Service_ID := 0;
               Done        : Boolean := False;    -- No more methods
               Last_Method : Abstract_Method_Ptr; -- Last method
               Last_Server : Call_Service_Ptr;    -- Last server
            begin
               while This /= null loop
                  if This = Object'Unchecked_Access then
                     Server_No   := Server_No + 1;
                     Server_ID   := Server_No;
                     Done        := True;
                     Object.Prev := Last_Server;
                     Last_Server := Object'Unchecked_Access;
                  elsif This.all in Call_Service'Class then
                     declare
                        Service : Call_Service'Class renames
                                  Call_Service'Class (This.all);
                     begin
                        Server_No    := Server_No + 1;
                        Service.Prev := Last_Server;
                        Last_Server  := Service'Unchecked_Access;
                     end;
                  elsif (  not Done
                        and then
                           This.all in Abstract_Method'Class
                        )  then
                     declare
                        Method : Abstract_Method'Class renames
                                 Abstract_Method'Class (This.all);
                     begin
                        if Method.Service = null then -- A free method
                           Method_No      := Method_No + 1;
                           Method.Index   := Method_No;
                           Method.Service := Object'Unchecked_Access;
                           Method.Prev    := Last_Method;
                           Last_Method    := Method'Unchecked_Access;
                        end if;
                     end;
                  end if;
                  This := This.Next;
               end loop;
               Object.Methods := new Method_Array (1..Method_No);
               while Method_No > 0 and then Last_Method /= null loop
                  Object.Methods (Method_No) := Last_Method;
                  Last_Method := Last_Method.Prev;
                  Method_No   := Method_No - 1;
               end loop;
               Object.Servers := new Call_Service_Array (1..Server_No);
               while Server_No > 0 and then Last_Server /= null loop
                  Object.Servers (Server_No) := Last_Server;
                  Last_Server := Last_Server.Prev;
                  Server_No   := Server_No - 1;
               end loop;
               Object.Data.Service := Object'Unchecked_Access;
               if (  Compare_And_Swap
                     (  Object.Data.Index'Access,
                        0,
                        Server_ID
                     )
                  or else
                     (  Compare_And_Swap
                        (  Object.Data.Index'Access,
                           Server_ID,
                           Server_ID
                     )  )
                  or else
                     (  Compare_And_Swap
                        (  Object.Data.Index'Access,
                           Server_ID + Last_ID - 1,
                           Server_ID
                  )  )  )
               then
                  Object.Server_ID    := Server_ID;
                  Object.Data.Process := Get_Process_ID;
                  Object.Data.Service := Object'Unchecked_Access;
               else
                  Raise_Exception
                  (  Mode_Error'Identity,
                     (  "The service (no."
                     &  Image (Server_ID)
                     &  ") is already assigned to another "
                     &  "process (no."
                     &  Image (Object.Data.Index)
                     &  ")"
                  )  );
               end if;
            end;
         when Client_Mode =>
            Set_Mode (Object.Queue,            Multiplexed_In_End);
            Set_Mode (Object.Requests.Stream,  Multiplexed_Out_End);
            Set_Mode (Object.Responses.Stream, Multiplexed_Out_End);
      end case;
   end Map;

   procedure Map
             (  Object   : in out Abstract_Method;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
   begin
      null;
   end Map;

   procedure On_Start (Service : in out Call_Service) is
   begin
      null;
   end On_Start;

   procedure Read
             (  Stream  : access Root_Stream_Type'Class;
                Request : out Request_Header
             )  is
      subtype Four_Elements is Stream_Element_Array (1..4);
      function To_32 is
         new Ada.Unchecked_Conversion (Four_Elements, Unsigned_32);
      subtype Eight_Elements is Stream_Element_Array (1..8);
      function To_64 is
         new Ada.Unchecked_Conversion (Eight_Elements, Unsigned_64);
      Data : Stream_Element_Array (1..17);
      Last : Stream_Element_Offset;
   begin
      Read (Stream.all, Data, Last);
      if Last /= 17 then
         raise End_Error;
      end if;
      Request.Mode   := Request_Type'Val (Data (1));
      Request.Method := Method_ID (To_32 (Data (2..5)));
      Request.From   := Call_Service_ID (To_32 (Data (6..9)));
      Request.No     := Sequence_No (To_64 (Data (10..17)));
   end Read;

   procedure Read
             (  Results : in out Null_Stream;
                Item    : out Stream_Element_Array;
                Last    : out Stream_Element_Offset
             )  is
   begin
      Last := Item'First - 1;
   end Read;

   procedure Read
             (  Results : in out Results_Stream;
                Item    : out Stream_Element_Array;
                Last    : out Stream_Element_Offset
             )  is
   begin
      Last := Item'First - 1;
   end Read;

   procedure Set_Server (Service : in out Call_Service) is
   begin
      if Service.Data /= null then
         Raise_Exception
         (  Status_Error'Identity,
            "Call service is already initialized"
         );
      end if;
      Service.Mode := Server_Mode;
      Set_Mode (Service.Queue,            Out_End);
      Set_Mode (Service.Requests.Stream,  In_End);
      Set_Mode (Service.Responses.Stream, In_End);
   end Set_Server;

   procedure Start
             (  Object : in out Call_Service;
                Shared : in out Abstract_Shared_Environment'Class;
                Owner  : Boolean
             )  is
   begin
      case Object.Mode is
         when Server_Mode =>
            Object.Server :=
               new Call_Server (Object'Unchecked_Access);
         when Client_Mode =>
            null;
      end case;
   end Start;

   procedure Unmap
             (  Object : in out Call_Service_Stop;
                Shared : in out Abstract_Shared_Environment'Class;
                Owner  : Boolean
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Call_Server,
                Call_Server_Ptr
             );
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Method_Array,
                Method_Array_Ptr
             );
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Call_Service_Array,
                Call_Service_Array_Ptr
             );
       This : Call_Service'Class renames Object.Parent.all;
   begin
      if This.Server /= null then
         Close (This.Requests.Stream);
         Close (This.Responses.Stream);
         Signal (This.Requests.Not_Empty);  -- Wake up the readers
         Signal (This.Responses.Not_Empty);
         if Current_Task /= Main then
            This.Server.Quit;
         end if;
         while not This.Server'Terminated loop
            delay 0.001;
         end loop;
         if Compare_And_Swap
            (  This.Data.Index'Access,
               This.Server_ID,
               0
            )
         then
            null;
         end if;
         This.Data.Process := Null_Process;
         Free (This.Server);
         Free (This.Methods);
         Free (This.Servers);
      end if;
   end Unmap;

   procedure Write
             (  Stream  : access Root_Stream_Type'Class;
                Request : Request_Header
             )  is
      subtype Four_Elements is Stream_Element_Array (1..4);
      function To_32 is
         new Ada.Unchecked_Conversion (Unsigned_32, Four_Elements);
      subtype Eight_Elements is Stream_Element_Array (1..8);
      function To_64 is
         new Ada.Unchecked_Conversion (Unsigned_64, Eight_Elements);
   begin
      Write
      (  Stream.all,
         (  Stream_Element (Request_Type'Pos (Request.Mode))
         &  To_32 (Unsigned_32 (Request.Method))
         &  To_32 (Unsigned_32 (Request.From))
         &  To_64 (Unsigned_64 (Request.No))
      )  );
   end Write;

   procedure Write
             (  Results : in out Null_Stream;
                Item    : Stream_Element_Array
             )  is
   begin
      null;
   end Write;

   procedure Write
             (  Results : in out Results_Stream;
                Item    : Stream_Element_Array
             )  is
      Service : Call_Service'Class renames Results.Service.all;
      Stream  : Universal_Stream   renames Service.Responses.Stream;
   begin
      if not Results.Lock.Owner then
--           Trace
--           (  Get_ID (Results.Service.all),
--              "taking results mutex to " & Get_Offset (Stream)
--           );
         Seize (Service.Responses.Lock, Service.Timeout);
         Results.Lock.Owner := True;
--           Trace
--           (  Get_ID (Results.Service.all),
--              (  "writing results header "
--              &  Image (Results.Header)
--              &  " to "
--              &  Get_Offset (Stream)
--           )  );
         Request_Header'Write (Stream'Access, Results.Header);
      end if;
--        Trace
--        (  Get_ID (Results.Service.all),
--           "writing results to " & Get_Offset (Stream)
--        );
      Write (Stream, Item);
--        Trace
--        (  Get_ID (Results.Service.all),
--           "results written to " & Get_Offset (Stream)
--        );
   end Write;

   protected body Response_Event is

      entry Accept_Results (Header : Request_Header) when True is
      begin
         if Exiting then
            return;
         elsif Cancel'Count > 0 then
            Raise_Exception
            (  Data_Error'Identity,
               "Canceled due to communication error"
            );
         end if;
         State   := not State;
         Pending := True;
         Result  := Header;
         requeue Accept_Results_Lounge with abort;
      end Accept_Results;

      entry Cancel when (  (  Accept_Results'Count
                           +  Incoming_Results (True)'Count
                           +  Incoming_Results (False)'Count
                           )
                        =  0
                        )  is
      begin
         Pending := False;
      end Cancel;

      entry Incoming_Results (for Current in Boolean)
            (  Header    : in out Request_Header;
               No_Header : out Boolean
            )  when (  Exiting
                    or else
                       Cancel'Count > 0
                    or else
                       not Pending
                    or else
                       Current = State
                    )  is
      begin
         if Exiting then
            Raise_Exception
            (  End_Error'Identity,
               "Process call service is exiting"
            );
         elsif Cancel'Count > 0 then
            Raise_Exception
            (  Data_Error'Identity,
               "Canceled due to communication error"
            );
         elsif Pending then -- Have a header already read
            if Header = Result then -- The expected header
               No_Header   := False;
               Header.Mode := Result.Mode;
            else -- Wait for another header
               requeue Incoming_Results (not State);
            end if;
         else -- Free to read a header
            No_Header := True;
            Pending   := True;
         end if;
      end Incoming_Results;

      entry Accept_Results_Lounge (Header : Request_Header)
         when not Pending or else Exiting is
      begin
         null;
      end Accept_Results_Lounge;

      entry Wait_For_Results
            (  Header    : in out Request_Header;
               No_Header : out Boolean
            )  when True is
      begin
         if Exiting then
            Raise_Exception
            (  End_Error'Identity,
               "Process call service is exiting"
            );
         elsif Cancel'Count > 0 then
            Raise_Exception
            (  Data_Error'Identity,
               "Canceled due to communication error"
            );
         elsif Pending then -- Have a header already read
            if Header = Result then -- The expected header
               No_Header   := False;
               Header.Mode := Result.Mode;
            else -- Wait for another header
               requeue Incoming_Results (not State);
            end if;
         else -- Free to read a header
            No_Header := True;
            Pending   := True;
         end if;
      end Wait_For_Results;

      procedure Complete is
      begin
         Pending := False;
      end Complete;

      procedure Quit is
      begin
         Exiting := True;
      end Quit;
   end Response_Event;

   task body Call_Server is
      Current : Request_Header;
      Stock   : Null_Stream;
      Empty   : Boolean;
   begin
      On_Start (Service.all);
      while Is_Callable (Main) loop
         select
            accept Quit;
            exit;
         else
            null;
         end select;
--           Trace
--           (  Get_ID (Service.all),
--              (  "wait for an incoming request to "
--              &  Get_Offset (Service.Queue)
--           )  );
         Wait_For_Not_Empty (Service.Queue, Service.Timeout, Empty);
         if not Empty then
            Current := Get (Service.Queue);
--              Trace
--              (  Get_ID (Service.all),
--                 (  "request "
--                 &  Image (Current.From)
--                 &  "->"
--                 &  Image (Get_ID (Service.all))
--                 &  " header "
--                 &  Image (Current)
--                 &  " to "
--                 &  Get_Offset (Service.Queue)
--              )  );
            declare
               Method : Abstract_Method'Class renames
                        Service.Methods (Current.Method).all;
               Caller : Call_Service'Class renames
                        Service.Servers (Current.From).all;
            begin
               if Current.Mode = Asynchronous_Request then
                  begin
                     Execute
                     (  Method     => Method,
                        Parameters => Service.Requests.Stream,
                        Results    => Stock,
                        Caller     => Caller,
                        No         => Current.No
                     );
                  exception
                     when others =>
                        null;
                  end;
               else
                  begin
                     if Is_Synchronous (Method) then
                        declare
                           Results : Results_Stream (Caller'Access);
                        begin
                           Results.Header.Mode   := Success_Response;
                           Results.Header.Method := Current.Method;
                           Results.Header.From   := Get_ID (Service.all);
                           Results.Header.No     := Current.No;
--                            Trace
--                            (  Get_ID (Service.all),
--                               (  "execute "
--                               &  Image (Get_ID (Caller))
--                               &  "->"
--                               &  Image (Get_ID (Service.all))
--                               &  " from "
--                               &  Get_Offset (Caller.Responses.Stream)
--                               &  " to "
--                               &  Get_Offset (Service.Requests.Stream)
--                            )  );
                           Execute
                           (  Method     => Method,
                              Parameters => Service.Requests.Stream,
                              Results    => Results,
                              Caller     => Caller,
                              No         => Current.No
                           );
                        end;
                     else
                        Execute
                        (  Method     => Method,
                           Parameters => Service.Requests.Stream,
                           Results    => Stock,
                           Caller     => Caller,
                           No         => Current.No
                        );
                     end if;
                  exception
                     when Error : others =>
                        declare
                           Stream : Universal_Stream renames
                                    Caller.Responses.Stream;
                           Lock   : Holder
                                    (  Caller.Responses.Lock'Access
                                    );
                        begin
                           Current.Mode := Failure_Response;
                           Current.From := Get_ID (Service.all);
                           Request_Header'Write
                           (  Stream'Access,
                              Current
                           );
                           Exception_Id'Write
                           (  Stream'Access,
                              Exception_Identity (Error)
                           );
                           String'Output
                           (  Stream'Access,
                              Exception_Message (Error)
                           );
                        exception
                           when others =>
                              null;
                        end;
                  end;
               end if;
            end;
         end if;
      end loop;
--        Trace (Get_ID (Service.all), "initiate exiting call service");
      Close (Service.Requests.Stream);
      Close (Service.Responses.Stream);
      while not Is_Empty (Service.Queue) loop
         Current := Get (Service.Queue);
--           Trace
--           (  Get_ID (Service.all),
--              (  "canceling request "
--              &  Image (Current.From)
--              &  "->"
--              &  Image (Get_ID (Service.all))
--              &  " header "
--              &  Image (Current)
--              &  " to "
--              &  Get_Offset (Service.Queue)
--           )  );
         if Current.Mode /= Asynchronous_Request then
            declare
               Caller  : Call_Service'Class renames
                         Service.Servers (Current.From).all;
               Stream  : Universal_Stream renames
                         Caller.Responses.Stream;
               Lock    : Holder (Caller.Responses.Lock'Access);
            begin
               Current.Mode := Cancel_Response;
               Current.From := Get_ID (Service.all);
               Request_Header'Write (Stream'Access, Current);
            exception
               when others =>
                  null;
            end;
         end if;
      end loop;
--        Trace (Get_ID (Service.all), "*** exiting call service");
   end Call_Server;

begin
   Main := Current_Task;
end Synchronization.Interprocess.Process_Call_Service;
