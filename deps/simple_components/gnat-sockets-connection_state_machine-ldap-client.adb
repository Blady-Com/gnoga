--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     LDAP.Client                                 Summer, 2019       --
--  Implementation                                                    --
--                                Last revision :  21:39 03 Aug 2019  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Ada.Calendar;           use Ada.Calendar;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Synchronization;        use Synchronization;

with Object.Handle.Generic_Set;
with ada.Text_IO;
package body GNAT.Sockets.Connection_State_Machine.LDAP.Client is

   No_Object : constant String := "The base object does no exist";

   package Entry_Sets is new Search_Entry_Handles.Generic_Set;
   package Value_Sets is new Value_Handles.Generic_Set;

   procedure Cancel (Client : in out LDAP_Client) is
   begin
      Client.Event.Cancel;
   end Cancel;

   procedure Check_Result
             (  Client : in out LDAP_Client;
                Result : LDAP_Result'Class
             )  is
   begin
      if Result.Result_Code.Value /= Success_Code then
         if Get_Length (Result.Diagnostic_Message) = 0 then
            Raise_Exception
            (  LDAP_Error'Identity,
               Image (Result.Result_Code.Value)
            );
         else
            Raise_Exception
            (  LDAP_Error'Identity,
               (  Image (Result.Result_Code.Value)
               &  ' '
               &  Get_Value (Result.Diagnostic_Message)
            )  );
         end if;
      end if;
      Client.Event.Stop_Wait;
   exception
      when others =>
         Client.Event.Stop_Wait;
         raise;
   end Check_Result;

   procedure Connect
             (  Client         : in out LDAP_Client;
                Host           : String;
                Port           : Port_Type := 389;
                Max_Connect_No : Positive  := Positive'Last;
                Timeout        : Duration  := Duration'Last
             )  is
      Deadline  : Time;
      Connected : Boolean := True;
   begin
      Shutdown (Client);
      Deadline := Clock + Timeout;
      select
         Client.Event.Released;
      or delay until Deadline;
         Raise_Exception
         (  Synchronization.Timeout_Error'Identity,
            "Shutdown timeout expired"
         );
      end select;
      Connect
      (  Client.Listener.all,
         Client'Unchecked_Access,
         Host,
         Port,
         Max_Connect_No
      );
      select
         Client.Event.Wait (Connected);
      or delay until Deadline;
         Raise_Exception
         (  Synchronization.Timeout_Error'Identity,
            "Connection timeout expired"
         );
      end select;
   exception
      when Time_Error =>
         Client.Event.Released;
         Connect
         (  Client.Listener.all,
            Client'Unchecked_Access,
            Host,
            Port,
            Max_Connect_No
         );
         Client.Event.Wait (Connected);
   end Connect;

   procedure Connected (Client : in out LDAP_Client) is
   begin
      Connected (LDAP_Peer (Client));
      Client.Event.Set;
   end Connected;

   function Create
            (  Name       : Distinguished_Name;
               Attributes : Attributes_List
            )  return Search_Entry_Handles.Handle is
      Result : constant Search_Entry_Handles.Handle :=
                        Search_Entry_Handles.Ref
                        (  new Search_Entry (Attributes.Size)
                        );
      This   : Search_Entry'Class renames
               Search_Entry_Handles.Ptr (Result).all;
   begin
      This.Name := new Distinguished_Name'(Name);
      This.Attributes := Attributes;
      return Result;
   end Create;

   procedure End_Of_Query (Client : in out LDAP_Client) is
   begin
      End_Of_Query (LDAP_Client (Client));
      Client.Event.Set;
   end End_Of_Query;

   function Exists
            (  Client  : LDAP_Client;
               Name    : Distinguished_Name;
               Timeout : Duration
            )  return Boolean is
      Result : constant Search_Result :=
                        Send_Search
                        (  Client  => Client,
                           Name    => Name,
                           Filter  => Present ("objectClass"),
                           Scope   => Base_Object_Scope,
                           Timeout => Timeout
                        );
   begin
      return Result.Entries_Count > 0;
   end Exists;

   procedure Finalize (Item : in out Search_Entry) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Distinguished_Name,
                Distinguished_Name_Ptr
             );
   begin
      Free (Item.Name);
      Object.Finalize (Object.Entity (Item));
   end Finalize;

   procedure Finalize (Lock : in out Holder) is
   begin
      Lock.Event.Release;
   end Finalize;

   function Get_Entry_Attributes
            (  Result : Search_Result;
               Index  : Positive
            )  return Attributes_List is
   begin
      if Index > Result.Entries_Count then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong entry attributes index"
         );
      end if;
      return Search_Entry_Handles.Ptr
             (  Result.Entries (Index)
             ) .Attributes;
   end Get_Entry_Attributes;

   function Get_Entries_Number (Result : Search_Result)
      return Natural is
   begin
      return Result.Entries_Count;
   end Get_Entries_Number;

   function Get_Entry_Name
            (  Result : Search_Result;
               Index  : Positive
            )  return Distinguished_Name is
   begin
      if Index > Result.Entries_Count then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong entry object index"
         );
      end if;
      declare
         This : Search_Entry'Class renames
                Search_Entry_Handles.Ptr (Result.Entries (Index)).all;
      begin
         if This.Name = null then
            return Null_Name;
         else
            return This.Name.all;
         end if;
      end;
   end Get_Entry_Name;

   function Get_References (Result : Search_Result)
      return Values_List is
   begin
      return Result.References;
   end Get_References;

   procedure Initialize (Lock : in out Holder) is
   begin
      Lock.Event.Seize;
   end Initialize;

   procedure Process_Packet (Client : in out LDAP_Client) is
   begin
      case Get_Selected (Client.Message.Protocol_Op) is
         when Add_Response_Choice =>
            declare
               This : LDAP_Result renames
                      Client.Message.Protocol_Op.Add_Response;
            begin
               Receive_Add_Response
               (  LDAP_Client'Class (Client),
                  Client.Message.Message_ID.Value,
                  This.Result_Code.Value,
                  Get_Matched (This),
                  Get_Value (This.Diagnostic_Message),
                  Get_Referral (This)
               );
            end;
         when Bind_Response_Choice =>
            declare
               This : LDAP_Bind_Response renames
                      Client.Message.Protocol_Op.Bind_Response;
               function Get_Credentials return String is
               begin
                  if Is_Set (This, 5) then
                     return Get_Value (This.Credentials);
                  else
                     return "";
                  end if;
               end Get_Credentials;
            begin
               Receive_Bind_Response
               (  LDAP_Client'Class (Client),
                  Client.Message.Message_ID.Value,
                  This.Result_Code.Value,
                  Get_Matched (This),
                  Get_Value (This.Diagnostic_Message),
                  Get_Referral (This),
                  Get_Credentials
               );
            end;
         when Compare_Response_Choice =>
            declare
               This : LDAP_Result renames
                      Client.Message.Protocol_Op.Compare_Response;
            begin
               Receive_Compare_Response
               (  LDAP_Client'Class (Client),
                  Client.Message.Message_ID.Value,
                  This.Result_Code.Value,
                  Get_Matched (This),
                  Get_Value (This.Diagnostic_Message),
                  Get_Referral (This)
               );
            end;
         when Delete_Response_Choice =>
            declare
               This : LDAP_Result renames
                      Client.Message.Protocol_Op.Delete_Response;
            begin
               Receive_Delete_Response
               (  LDAP_Client'Class (Client),
                  Client.Message.Message_ID.Value,
                  This.Result_Code.Value,
                  Get_Matched (This),
                  Get_Value (This.Diagnostic_Message),
                  Get_Referral (This)
               );
            end;
         when Extended_Response_Choice =>
            declare
               This : LDAP_Extended_Response renames
                      Client.Message.Protocol_Op.Extended_Response;
            begin
               Receive_Extended_Response
               (  LDAP_Client'Class (Client),
                  Client.Message.Message_ID.Value,
                  Get (This.Name),
                  Get_Value (This.Value)
               );
            end;
         when Intermediate_Response_Choice =>
            declare
               This : LDAP_Intermediate_Response renames
                      Client.Message.Protocol_Op.Intermediate_Response;
            begin
               Receive_Intermediate_Response
               (  LDAP_Client'Class (Client),
                  Client.Message.Message_ID.Value,
                  Get (This.Name),
                  Get_Value (This.Value)
               );
            end;
         when Modify_Response_Choice =>
            declare
               This : LDAP_Result renames
                      Client.Message.Protocol_Op.Modify_Response;
            begin
               Receive_Modify_Response
               (  LDAP_Client'Class (Client),
                  Client.Message.Message_ID.Value,
                  This.Result_Code.Value,
                  Get_Matched (This),
                  Get_Value (This.Diagnostic_Message),
                  Get_Referral (This)
               );
            end;
         when Modify_DN_Response_Choice =>
            declare
               This : LDAP_Result renames
                      Client.Message.Protocol_Op.Modify_DN_Response;
            begin
               Receive_Modify_DN_Response
               (  LDAP_Client'Class (Client),
                  Client.Message.Message_ID.Value,
                  This.Result_Code.Value,
                  Get_Matched (This),
                  Get_Value (This.Diagnostic_Message),
                  Get_Referral (This)
               );
            end;
         when Search_Result_Done_Choice =>
            declare
               This : LDAP_Result renames
                      Client.Message.Protocol_Op.Search_Result_Done;
            begin
               Receive_Search_Done_Response
               (  LDAP_Client'Class (Client),
                  Client.Message.Message_ID.Value,
                  This.Result_Code.Value,
                  Get_Matched (This),
                  Get_Value (This.Diagnostic_Message),
                  Get_Referral (This)
               );
            end;
         when Search_Result_Entry_Choice =>
            declare
               This : LDAP_Search_Result_Entry renames
                      Client.Message.Protocol_Op.Search_Result_Entry;
            begin
               if Get_Length (This.Object_Name) > 0 then
                  Receive_Search_Entry_Response
                  (  LDAP_Client'Class (Client),
                     Client.Message.Message_ID.Value,
                     Value (Get_Value (This.Object_Name)),
                     Get (This.Attributes)
                  );
               else
                  Receive_Search_Entry_Response
                  (  LDAP_Client'Class (Client),
                     Client.Message.Message_ID.Value,
                     Null_Name,
                     Get (This.Attributes)
                  );
               end if;
            end;
         when Search_Result_Reference_Choice =>
            Receive_Search_Reference_Response
            (  LDAP_Client'Class (Client),
               Client.Message.Message_ID.Value,
               Get (Client.Message.Protocol_Op.Search_Result_Reference)
            );
         when others =>
            Trace
            (  LDAP_Client'Class (Client),
               (  "Unsolicited response "
               &  Image (Get_Selected (Client.Message.Protocol_Op))
            )  );
            return;
      end case;
      Client.Event.Start_Notification
      (  Client.Message.Message_ID.Value,
         Get_Selected (Client.Message.Protocol_Op)
      );
   end Process_Packet;

   procedure Receive_Add_Response
             (  Client   : in out LDAP_Client;
                ID       : Integer_32;
                Result   : Result_Code;
                Matched  : Distinguished_Name;
                Message  : String;
                Referral : Values_List
             )  is
   begin
      null;
   end Receive_Add_Response;

   procedure Receive_Bind_Response
             (  Client      : in out LDAP_Client;
                ID          : Integer_32;
                Result      : Result_Code;
                Matched     : Distinguished_Name;
                Message     : String;
                Referral    : Values_List;
                Credentials : String
             )  is
   begin
      null;
   end Receive_Bind_Response;

   function Receive_Challenge
            (  Client      : access LDAP_Client;
               Mechanism   : String;
               Matched     : Distinguished_Name;
               Message     : String;
               Credentials : String
            )  return String is
   begin
      raise Use_Error;
      return "";
   end Receive_Challenge;

   procedure Receive_Compare_Response
             (  Client   : in out LDAP_Client;
                ID       : Integer_32;
                Result   : Result_Code;
                Matched  : Distinguished_Name;
                Message  : String;
                Referral : Values_List
             )  is
   begin
      null;
   end Receive_Compare_Response;

   procedure Receive_Delete_Response
             (  Client   : in out LDAP_Client;
                ID       : Integer_32;
                Result   : Result_Code;
                Matched  : Distinguished_Name;
                Message  : String;
                Referral : Values_List
             )  is
   begin
      null;
   end Receive_Delete_Response;

   procedure Receive_Extended_Response
             (  Client : in out LDAP_Client;
                ID     : Integer_32;
                Name   : Object_Identifier;
                Value  : String
             )  is
   begin
      null;
   end Receive_Extended_Response;

   procedure Receive_Intermediate_Response
             (  Client : in out LDAP_Client;
                ID     : Integer_32;
                Name   : Object_Identifier;
                Value  : String
             )  is
   begin
      null;
   end Receive_Intermediate_Response;

   procedure Receive_Modify_Response
             (  Client   : in out LDAP_Client;
                ID       : Integer_32;
                Result   : Result_Code;
                Matched  : Distinguished_Name;
                Message  : String;
                Referral : Values_List
             )  is
   begin
      null;
   end Receive_Modify_Response;

   procedure Receive_Modify_DN_Response
             (  Client   : in out LDAP_Client;
                ID       : Integer_32;
                Result   : Result_Code;
                Matched  : Distinguished_Name;
                Message  : String;
                Referral : Values_List
             )  is
   begin
      null;
   end Receive_Modify_DN_Response;

   procedure Receive_Search_Done_Response
             (  Client   : in out LDAP_Client;
                ID       : Integer_32;
                Result   : Result_Code;
                Matched  : Distinguished_Name;
                Message  : String;
                Referral : Values_List
             )  is
   begin
      null;
   end Receive_Search_Done_Response;

   procedure Receive_Search_Entry_Response
             (  Client     : in out LDAP_Client;
                ID         : Integer_32;
                Name       : Distinguished_Name;
                Attributes : Attributes_List
             )  is
   begin
      null;
   end Receive_Search_Entry_Response;

   procedure Receive_Search_Reference_Response
             (  Client : in out LDAP_Client;
                ID     : Integer_32;
                URIs   : Values_List
             )  is
   begin
      null;
   end Receive_Search_Reference_Response;

   procedure Released (Client : in out LDAP_Client) is
   begin
      Released (LDAP_Client (Client));
      Client.Event.Set;
   end Released;

   procedure Send_Abandon
             (  Client : in out LDAP_Client;
                ID     : Integer_32
             )  is
      Lock   : Holder (Client.Event'Access);
      New_ID : constant Integer_32 := Client.Sequence;
   begin
      Erase (Client.Buffer);
      Set_Abandon_Request (Client.Output.Message, ID);
      Send_Request (Client, New_ID);
      Client.Sequence := Client.Sequence + 1;
   end Send_Abandon;

   procedure Send_Add
             (  Client     : in out LDAP_Client;
                Name       : Distinguished_Name;
                Attributes : Attributes_List;
                Timeout    : Duration := Duration'First
             )  is
      Lock   : Holder (Client.Event'Access);
      New_ID : constant Integer_32 := Client.Sequence;
   begin
      Erase (Client.Buffer);
      Set_Add_Request
      (  Client.Output.Message,
         Name,
         Attributes
      );
      Send_Request (Client, New_ID);
      Client.Sequence := Client.Sequence + 1;
      if Timeout >= 0.0 then
         select
            Client.Event.Start_Wait (New_ID);
         or delay Timeout;
            raise Timeout_Error;
         end select;
         declare
            Result : LDAP_Result renames
                     Client.Message.Protocol_Op.Add_Response;
         begin
            case Result.Result_Code.Value is
               when Attribute_Or_Value_Exists_Code =>
                  begin
                     if Get_Length (Result.Diagnostic_Message) = 0 then
                        Raise_Exception
                        (  LDAP_Error'Identity,
                           Image (Result.Result_Code.Value)
                        );
                     else
                        Raise_Exception
                        (  LDAP_Error'Identity,
                           (  Image (Result.Result_Code.Value)
                           &  ' '
                           &  Get_Value (Result.Diagnostic_Message)
                        )  );
                     end if;
                  exception
                     when others =>
                        Client.Event.Stop_Wait;
                        raise;
                  end;
               when others =>
                  Check_Result (Client, Result);
            end case;
         end;
      end if;
   end Send_Add;

   procedure Send_Bind
             (  Client   : in out LDAP_Client;
                Name     : Distinguished_Name;
                Password : String;
                Timeout  : Duration := Duration'First
             )  is
      Lock   : Holder (Client.Event'Access);
      New_ID : constant Integer_32 := Client.Sequence;
   begin
      Erase (Client.Buffer);
      Set_Bind_Request
      (  Client.Output.Message,
         Name,
         Password
      );
      Send_Request (Client, New_ID);
      Client.Sequence := Client.Sequence + 1;
      if Timeout >= 0.0 then
         select
            Client.Event.Start_Wait (New_ID);
            Check_Result
            (  Client,
               Client.Message.Protocol_Op.Bind_Response
            );
         or delay Timeout;
            raise Timeout_Error;
         end select;
      end if;
   end Send_Bind;

   procedure Send_Bind
             (  Client      : in out LDAP_Client;
                Name        : Distinguished_Name;
                Mechanism   : String;
                Credentials : String   := "";
                Timeout     : Duration := Duration'First
             )  is
      Lock   : Holder (Client.Event'Access);
      New_ID : constant Integer_32 := Client.Sequence;
   begin
      Erase (Client.Buffer);
      Set_Bind_Request
      (  Client.Output.Message,
         Name,
         Mechanism,
         Credentials
      );
      Send_Request (Client, New_ID);
      Client.Sequence := Client.Sequence + 1;
      if Timeout >= 0.0 then
         declare
            Deadline : constant Time := Clock + Timeout;
         begin
            loop
               select
                  Client.Event.Start_Wait (New_ID);
                  declare
                      Result : LDAP_Bind_Response renames
                               Client.Message.Protocol_Op.Bind_Response;
                  begin
                     case Result.Result_Code.Value is
                        when SASL_Bind_In_Progress_Code =>
                           Client.Event.Stop_Wait;
                        when others =>
                           Check_Result (Client, Result);
                           exit;
                     end case;
                     declare
                        Password : constant String :=
                           Receive_Challenge
                           (  LDAP_Client'Class
                              (  Client
                              ) 'Unchecked_Access,
                              Mechanism,
                              Get_Name  (Result.Matched_DN),
                              Get_Value (Result.Diagnostic_Message),
                              Get_Value (Result.Credentials)
                           );
                     begin
                        Set_Bind_Request
                        (  Client.Output.Message,
                           Name,
                           Mechanism,
                           Credentials
                        );
                        Send_Request (Client, New_ID);
                     end;
                  end;
               or delay Timeout;
                  raise Timeout_Error;
               end select;
           end loop;
        end;
      end if;
   end Send_Bind;

   procedure Receive_Challenge
             (  Client      : in out LDAP_Client;
                Matched     : Distinguished_Name;
                Message     : String;
                Credentials : String
             )  is
   begin
      raise Use_Error;
   end Receive_Challenge;

   procedure Send_Compare
             (  Client      : in out LDAP_Client;
                Name        : Distinguished_Name;
                Description : String;
                Value       : String
             )  is
      Lock   : Holder (Client.Event'Access);
      New_ID : constant Integer_32 := Client.Sequence;
   begin
      Erase (Client.Buffer);
      Set_Compare_Request
      (  Client.Event.Client.Output.Message,
         Name,
         Description,
         Value
      );
      Send_Request (Client, New_ID);
      Client.Sequence := Client.Sequence + 1;
   end Send_Compare;

   function Send_Compare
            (  Client      : LDAP_Client;
               Name        : Distinguished_Name;
               Description : String;
               Value       : String;
               Timeout     : Duration
            )  return Boolean is
      Self   : LDAP_Client'Class renames Client.Event.Client.all;
      Lock   : Holder (Self.Event'Access);
      New_ID : constant Integer_32 := Client.Sequence;
   begin
      Erase (Self.Buffer);
      Set_Compare_Request
      (  Self.Output.Message,
         Name,
         Description,
         Value
      );
      Send_Request (Self, New_ID);
      Self.Sequence := Self.Sequence + 1;
      select
         Self.Event.Start_Wait (New_ID);
         declare
             Result : LDAP_Result renames
                      Client.Message.Protocol_Op.Compare_Response;
         begin
            case Result.Result_Code.Value is
               when Compare_True_Code =>
                  Self.Event.Stop_Wait;
                  return True;
               when Compare_False_Code =>
                  Self.Event.Stop_Wait;
                  return False;
               when others =>
                  Check_Result (Self, Result);
                  return True;
            end case;
         end;
      or delay Timeout;
         raise Timeout_Error;
      end select;
   end Send_Compare;

   procedure Send_Delete
             (  Client  : in out LDAP_Client;
                Name    : Distinguished_Name;
                Timeout : Duration := Duration'First
             )  is
      Lock   : Holder (Client.Event'Access);
      New_ID : constant Integer_32 := Client.Sequence;
   begin
      Erase (Client.Buffer);
      Set_Delete_Request
      (  Client.Output.Message,
         Name
      );
      Send_Request (Client, New_ID);
      Client.Sequence := Client.Sequence + 1;
      if Timeout >= 0.0 then
         select
            Client.Event.Start_Wait (New_ID);
         or delay Timeout;
            raise Timeout_Error;
         end select;
         declare
            Result : LDAP_Result renames
                     Client.Message.Protocol_Op.Delete_Response;
         begin
            case Result.Result_Code.Value is
               when No_Such_Object_Code | No_Such_Attribute_Code =>
                  Client.Event.Stop_Wait;
               when others =>
                  Check_Result (Client, Result);
            end case;
         end;
      end if;
   end Send_Delete;

   procedure Send_Extended
             (  Client  : in out LDAP_Client;
                Name    : Object_Identifier;
                Value   : String;
                Timeout : Duration := Duration'First
             )  is
      Lock   : Holder (Client.Event'Access);
      New_ID : constant Integer_32 := Client.Sequence;
   begin
      Erase (Client.Buffer);
      Set_Extended_Request
      (  Client.Output.Message,
         Name,
         Value
      );
      Send_Request (Client, New_ID);
      Client.Sequence := Client.Sequence + 1;
      if Timeout >= 0.0 then
         select
            Client.Event.Start_Wait (New_ID);
            Check_Result
            (  Client,
               Client.Message.Protocol_Op.Extended_Response
            );
         or delay Timeout;
            raise Timeout_Error;
         end select;
      end if;
   end Send_Extended;

   procedure Send_Modify
             (  Client  : in out LDAP_Client;
                Name    : Distinguished_Name;
                Update  : Updates_List;
                Timeout : Duration := Duration'First
             )  is
      Lock   : Holder (Client.Event'Access);
      New_ID : constant Integer_32 := Client.Sequence;
   begin
      Erase (Client.Buffer);
      Set_Modify_Request (Client.Output.Message, Name, Update);
      Send_Request (Client, New_ID);
      Client.Sequence := Client.Sequence + 1;
      if Timeout >= 0.0 then
         select
            Client.Event.Start_Wait (New_ID);
            Check_Result
            (  Client,
               Client.Message.Protocol_Op.Modify_Response
            );
         or delay Timeout;
            raise Timeout_Error;
         end select;
      end if;
   end Send_Modify;

   procedure Send_Modify_DN
             (  Client       : in out LDAP_Client;
                Name         : Distinguished_Name;
                New_RDN      : Distinguished_Name;
                Delete_RDN   : Boolean;
                New_Superior : Distinguished_Name;
                Timeout      : Duration := Duration'First
             )  is
      Lock   : Holder (Client.Event'Access);
      New_ID : constant Integer_32 := Client.Sequence;
   begin
      Erase (Client.Buffer);
      Set_Modify_DN_Request
      (  Client.Output.Message,
         Name,
         New_RDN,
         Delete_RDN,
         New_Superior
      );
      Send_Request (Client, New_ID);
      Client.Sequence := Client.Sequence + 1;
      if Timeout >= 0.0 then
         select
            Client.Event.Start_Wait (New_ID);
            Check_Result
            (  Client,
               Client.Message.Protocol_Op.Modify_DN_Response
            );
         or delay Timeout;
            raise Timeout_Error;
         end select;
      end if;
   end Send_Modify_DN;

   procedure Send_Modify_DN
             (  Client     : in out LDAP_Client;
                Name       : Distinguished_Name;
                New_RDN    : Distinguished_Name;
                Delete_RDN : Boolean;
                Timeout    : Duration := Duration'First
             )  is
      Lock   : Holder (Client.Event'Access);
      New_ID : constant Integer_32 := Client.Sequence;
   begin
      Erase (Client.Buffer);
      Set_Modify_DN_Request
      (  Client.Output.Message,
         Name,
         New_RDN,
         Delete_RDN
      );
      Send_Request (Client, New_ID);
      Client.Sequence := Client.Sequence + 1;
      if Timeout >= 0.0 then
         select
            Client.Event.Start_Wait (New_ID);
            Check_Result
            (  Client,
               Client.Message.Protocol_Op.Modify_DN_Response
            );
         or delay Timeout;
            raise Timeout_Error;
         end select;
      end if;
   end Send_Modify_DN;

   procedure Send_Search
             (  Client        : in out LDAP_Client;
                Name          : Distinguished_Name;
                Filter        : Search_Filter;
                Must_Exist    : Boolean          := False;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False;
                Timeout       : Duration         := Duration'First
             )  is
      Lock   : Holder (Client.Event'Access);
      New_ID : constant Integer_32 := Client.Sequence;
   begin
      Erase (Client.Buffer);
      Set_Search_Request
      (  Client.Output.Message,
         Name,
         Filter,
         Scope,
         Aliasing_Mode,
         Size_Limit,
         Time_Limit,
         Types_Only
      );
      Send_Request (Client, New_ID);
      Client.Sequence := Client.Sequence + 1;
      if Timeout >= 0.0 then
         declare
             Deadline : constant Time := Clock + Timeout;
         begin
            loop
               select
                  Client.Event.Start_Wait (New_ID);
               or delay until Deadline;
                  raise Timeout_Error;
               end select;
               case Get_Selected (Client.Message.Protocol_Op) is
                  when Search_Result_Entry_Choice |
                       Search_Result_Reference_Choice =>
                     Client.Event.Stop_Wait;
                  when Search_Result_Done_Choice =>
                     declare
                        Result : LDAP_Result renames
                                 Client.Message.Protocol_Op.
                                 Search_Result_Done;
                     begin
                        case Result.Result_Code.Value is
                           when No_Such_Object_Code =>
                              Client.Event.Stop_Wait;
                              exit when not Must_Exist;
                              Raise_Exception
                              (  End_Error'Identity,
                                 No_Object
                              );
                           when others =>
                              Check_Result (Client, Result);
                              exit;
                        end case;
                     end;
                  when others =>
                     Client.Event.Stop_Wait;
               end case;
            end loop;
         end;
      end if;
   end Send_Search;

   function Send_Search
            (  Client        : LDAP_Client;
               Name          : Distinguished_Name;
               Filter        : Search_Filter;
               Must_Exist    : Boolean          := False;
               Scope         : Scope_Type       := Whole_Subtree_Scope;
               Aliasing_Mode : Dereference_Type := Deref_Always;
               Size_Limit    : Integer_32       := 0;
               Time_Limit    : Duration         := 0.0;
               Types_Only    : Boolean          := False;
               Timeout       : Duration
            )  return Search_Result is
      Self     : LDAP_Client'Class renames Client.Event.Client.all;
      Values   : Value_Sets.Set;
      Entries  : Entry_Sets.Set;
      Lock     : Holder (Self.Event'Access);
      New_ID   : constant Integer_32 := Client.Sequence;
      Deadline : constant Time := Clock + Timeout;
   begin
      Erase (Self.Buffer);
      Set_Search_Request
      (  Self.Output.Message,
         Name,
         Filter,
         Scope,
         Aliasing_Mode,
         Size_Limit,
         Time_Limit,
         Types_Only
      );
      Send_Request (Self, New_ID);
      Self.Sequence := Self.Sequence + 1;
      loop
         select
            Self.Event.Start_Wait (New_ID);
         or delay until Deadline;
            raise Timeout_Error;
         end select;
         case Get_Selected (Client.Message.Protocol_Op) is
            when Search_Result_Entry_Choice =>
               declare
                  This : LDAP_Search_Result_Entry renames
                         Self.Message.Protocol_Op.
                         Search_Result_Entry;
               begin
                  Entry_Sets.Add
                  (  Entries,
                     Create
                     (  Get (This.Object_Name),
                        Get (This.Attributes)
                  )  );
                  Self.Event.Stop_Wait;
               exception
                  when others =>
                     Self.Event.Stop_Wait;
                     raise;
               end;
            when Search_Result_Reference_Choice =>
               declare
                  This : LDAP_Implicit_String_Sequence renames
                         Client.Message.Protocol_Op.
                         Search_Result_Reference;
               begin
                  for Index in 1..Get_Length (This) loop
                     Value_Sets.Add
                     (  Values,
                        Create (Get (This, Index))
                     );
                  end loop;
                  Self.Event.Stop_Wait;
               exception
                  when others =>
                     Self.Event.Stop_Wait;
                     raise;
               end;
            when Search_Result_Done_Choice =>
               declare
                  Result : LDAP_Result renames
                           Self.Message.Protocol_Op.Search_Result_Done;
               begin
                  case Result.Result_Code.Value is
                     when No_Such_Object_Code =>
                        Self.Event.Stop_Wait;
                        exit when not Must_Exist;
                        Raise_Exception
                        (  End_Error'Identity,
                           No_Object
                        );
                     when others =>
                        Check_Result (Self, Result);
                        exit;
                  end case;
               end;
            when others =>
               Self.Event.Stop_Wait;
         end case;
      end loop;
      declare
         Result : Search_Result
                  (  Entry_Sets.Get_Size (Entries),
                     Value_Sets.Get_Size (Values)
                  );
      begin
         for Index in Result.Entries'Range loop
            Result.Entries (Index) := Entry_Sets.Ref (Entries, Index);
         end loop;
         for Index in 1..Result.References.Size loop
            Result.References.List (Index) :=
               Value_Sets.Ref (Values, Index);
         end loop;
         return Result;
      end;
   end Send_Search;

   procedure Send_Unbind (Client : in out LDAP_Client)  is
      Lock   : Holder (Client.Event'Access);
      New_ID : constant Integer_32 := Client.Sequence;
   begin
      Erase (Client.Buffer);
      Set_Unbind_Request (Client.Output.Message);
      Client.Sequence := Client.Sequence + 1;
      Send_Request (Client, New_ID);
   end Send_Unbind;

   procedure Trace
             (  Client  : in out LDAP_Client;
                Message : String
             )  is
   begin
      Trace
      (  Client.Listener.Factory.all,
         (  Get_Client_Name (Client.Listener.Factory.all, Client)
         &  ' '
         &  Message
      )  );
   end Trace;

   procedure Wait
             (  Client : in out LDAP_Client;
                Connected : Boolean;
                Timeout   : Duration := Duration'Last
             )  is
      Require : Boolean := Connected;
   begin
      select
         Client.Event.Wait (Require);
      or delay Timeout;
         Raise_Exception
         (  Synchronization.Timeout_Error'Identity,
            "Connection timeout expired"
         );
      end select;
   end Wait;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Event_Type
             )  is
   begin
      null;
   end Write;

   protected body Event_Type is
      entry Cancel when Event_Type.Released'Count = 0 and then
                        Event_Type.Wait'Count = 0 is
      begin
         null;
      end Cancel;

      entry Do_Notification
            (  ID     : Integer_32;
               Choice : Positive
            )  when Wait_Lounge (not Current_Lounge)'Count = 0 is
      begin
         requeue Stop_Notification;
      end Do_Notification;

      entry Start_Notification (ID : Integer_32; Choice : Positive)
         when Do_Notification'Count   = 0 and then
              Stop_Notification'Count = 0 is
      begin
         Current_ID := ID;
         Current_Lounge := not Current_Lounge;
         Current_Response := Choice;
         requeue Do_Notification;
      end Start_Notification;

      entry Stop_Notification (ID : Integer_32; Choice : Positive)
         when Requests_Count = 0 is
      begin
         null;
      end Stop_Notification;

      entry Lounge when Owner = Null_Task_ID is
      begin
         Owner := Lounge'Caller;
         Count := 1;
      end Lounge;

      procedure Release is
      begin
         if Owner = Current_Task then
            Count := Count - 1;
            if Count = 0 then
               Owner := Null_Task_ID;
            end if;
         else
            raise Synchronization.Ownership_Error;
         end if;
      end Release;

      entry Released when Down or else Event_Type.Cancel'Count > 0 is
      begin
         if Get_Session_State (Client.all) /= Session_Down then
            Down := False;
            if Event_Type.Cancel'Count > 0 then
               Raise_Exception
               (  Synchronization.Cancel_Error'Identity,
                  "Canceled"
               );
            else
               requeue Released with abort;
            end if;
         end if;
      end Released;

      entry Seize when True is
      begin
         if Seize'Caller = Owner then
            Count := Count + 1;
         elsif Owner = Null_Task_ID then
            Owner := Seize'Caller;
            Count := 1;
         else
            requeue Lounge with abort;
         end if;
      end Seize;

      entry Start_Wait (ID : Integer_32) when True is
      begin
         requeue Wait_Lounge (Current_Lounge) with abort;
      end Start_Wait;

      procedure Stop_Wait is
      begin
         Requests_Count := Requests_Count - 1;
      end Stop_Wait;

      entry Wait (Connected : in out Boolean)
         when Ready or else Down or else Event_Type.Cancel'Count > 0 is
      begin
         case Get_Session_State (Client.all) is
            when Session_Down =>
               Ready := False;
               Down  := True;
               if Connected then
                  declare
                     Error : Exception_Occurrence;
                  begin
                     Get_Occurrence (Client.all, Error);
                     if Exception_Identity (Error) /= Null_Id then
                        Reraise_Occurrence (Error);
                     else
                        Raise_Exception
                        (  Status_Error'Identity,
                           "Unable to connect"
                        );
                     end if;
                  end;
               else
                  Connected := False;
               end if;
            when Session_Disconnected | Session_Connecting |
                 Session_Handshaking  | Session_Busy       =>
               Ready := False;
               Down  := False;
               if Event_Type.Cancel'Count > 0 then
                  Raise_Exception
                  (  Synchronization.Cancel_Error'Identity,
                     "Canceled"
                  );
               else
                  requeue Wait with abort;
               end if;
            when Session_Active | Session_Connected =>
               Ready     := True;
               Down      := False;
               Connected := True;
         end case;
      end Wait;

      entry Wait_Lounge (for State in Boolean) (ID : Integer_32)
         when (  Down
              or else
                 (  Current_Lounge /= State
                 and then
                    Do_Notification'Count > 0
              )  )  is
      begin
         if Get_Session_State (Client.all) = Session_Down then
            raise Status_Error;
         elsif ID = Current_ID then
            Requests_Count := Requests_Count + 1;
         else
            requeue Wait_Lounge (Current_Lounge) with abort;
         end if;
      end Wait_Lounge;

      procedure Set is
      begin
         case Get_Session_State (Client.all) is
            when Session_Down =>
               Ready := False;
               Down  := True;
            when Session_Disconnected | Session_Connecting |
                 Session_Handshaking  | Session_Busy       =>
               Ready := False;
               Down  := False;
            when Session_Active | Session_Connected =>
               Ready := True;
               Down  := False;
         end case;
      end Set;

   end Event_Type;

end GNAT.Sockets.Connection_State_Machine.LDAP.Client;
