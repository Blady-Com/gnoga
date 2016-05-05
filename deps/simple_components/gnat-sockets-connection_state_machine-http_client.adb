--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     HTTP_Client                                 Spring, 2015       --
--  Implementation                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Ada.Tags;                 use Ada.Tags;
with Strings_Edit;             use Strings_Edit;
with Strings_Edit.Floats;      use Strings_Edit.Floats;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body GNAT.Sockets.Connection_State_Machine.HTTP_Client is
   use GNAT.Sockets.Server.Stream_Element_Offset_Edit;

   package Strings is
      DELETE     : aliased String_Data := ( 7, 7, "DELETE ");
      GET        : aliased String_Data := ( 4, 4, "GET ");
      HEAD       : aliased String_Data := ( 5, 5, "HEAD ");
      OPTIONS    : aliased String_Data := ( 8, 8, "OPTIONS ");
      POST       : aliased String_Data := ( 5, 5, "POST ");
      PUT        : aliased String_Data := ( 4, 4, "PUT ");
      EOL        : aliased String_Data := ( 2, 2, CRLF);
      Colon      : aliased String_Data := ( 2, 2, ": ");
      HTTP       : aliased String_Data := (11, 11, " HTTP/1.1" & CRLF);
      Keep_Alive : aliased String_Data :=
                           (24, 24, "Connection: keep-alive" & CRLF);
      Close      : aliased String_Data :=
                           (19, 19, "Connection: close" & CRLF);
   end Strings;

   procedure Free is
      new Ada.Unchecked_Deallocation (String_Data, String_Data_Ptr);

   procedure Check_Spelling (Name : String) is
   begin
      null;
   end Check_Spelling;

   function Check_Matched
            (  Source  : String;
               Pointer : Integer
            )  return Boolean is
   begin
      case Source (Pointer) is
         when '0'..'9' | 'A'..'Z' | 'a'..'z' =>
            return False;
         when others =>
            return True;
      end case;
   end Check_Matched;

   procedure Cleanup (Session : in out HTTP_Session) is
   begin
      Session.Expecting   := Nothing;
      Session.Data_Length := 0;
      Session.Version     := 1.1;
      Session.Code        := 200;
      Session.Reason      := null;
      Session.Stream      := null;
      Session.Destination := null;
      Session.Send_Index  := 1;
      Session.Send_Length := 0;
      Session.Connection  := Connection_Close;
      Session.Response_Headers := (others => null);
      Session.Specific         := (others => False);
      Deallocate_All (Session.Pool);
   end Cleanup;

   procedure Completed
             (  Session : in out HTTP_Session;
                Method  : HTTP_Server.HTTP_Method;
                Status  : Positive
             )  is
   begin
      null;
   end Completed;

   procedure Completed
             (  Session : in out HTTP_Session;
                Method  : HTTP_Server.HTTP_Method;
                Status  : Positive;
                Message : in out Root_Stream_Type'Class
             )  is
   begin
      null;
   end Completed;

   procedure Completed
             (  Session : in out HTTP_Session;
                Method  : HTTP_Server.HTTP_Method;
                Status  : Positive;
                Message : in out HTTP_Server.Content_Destination'Class
             )  is
   begin
      null;
   end Completed;

   procedure Connect_Parameters_Set
             (  Session        : in out HTTP_Session;
                Host           : String;
                Address        : Sock_Addr_Type;
                Max_Connect_No : Positive
             )  is
   begin
      Set_Request_Header (Session, Host_Header, Host);
      Set_Overlapped_Size (Session, 0); -- Set half-duplex exchange
   end Connect_Parameters_Set;

   procedure Connected (Session : in out HTTP_Session) is
   begin
--        Cleanup (Session);
--        begin
--           Set_Request_Header
--           (  Session,
--              Host_Header,
--              (  Official_Name
--                 (  Get_Host_By_Address
--                    (  Get_Client_Address (Session).Addr
--                 )  )
--              &  ":"
--              &  Image (Integer (Get_Client_Address (Session).Port))
--           )  );
--        exception
--           when Error : Host_Error =>
--              Trace
--              (  Session.Listener.Factory.all,
--                 (  "Cannot get server's host name: "
--                 &  Exception_Message (Error)
--              )  );
--        end;
      Session.Expecting := Nothing;
      Connected (State_Machine (Session));
   end Connected;

   procedure Delete
             (  Session : in out HTTP_Session;
                Name    : String
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, "");
      Session.Method    := HTTP_DELETE;
      Session.Expecting := Response_Line;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.DELETE'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Delete;

   procedure Delete
             (  Session : in out HTTP_Session;
                Name    : String;
                Message : access Root_Stream_Type'Class
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, "");
      Session.Method    := HTTP_DELETE;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.DELETE'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Delete;

   procedure Delete
             (  Session : in out HTTP_Session;
                Name    : String;
                Message : access HTTP_Server.Content_Destination'Class
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, "");
      Session.Method      := HTTP_DELETE;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.DELETE'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Delete;

   procedure End_Of_Query (Session : in out HTTP_Session) is
   begin
      if Session.Stream /= null then
         Completed
         (  HTTP_Session'Class (Session),
            Session.Method,
            Session.Code,
            Session.Stream.all
         );
         Session.Stream := null;
      elsif Session.Destination /= null then
         Commit (Session.Destination.all);
         Completed
         (  HTTP_Session'Class (Session),
            Session.Method,
            Session.Code,
            Session.Destination.all
         );
         Session.Destination := null;
      else
         Completed
         (  HTTP_Session'Class (Session),
            Session.Method,
            Session.Code
         );
      end if;
      Session.Expecting := Nothing;
      if (  not Session.Keep_Alive
         or else
            0 /= (Session.Connection and Connection_Close)
         )
      then
         Shutdown (Session);
      end if;
   end End_Of_Query;

   procedure Finalize (Session : in out HTTP_Session) is
   begin
      for Index in Session.Request_Headers'Range loop
         Free (Session.Request_Headers (Index));
      end loop;
      Free (Session.Page_Name);
      Free (Session.If_Modified);
      Free (Session.If_Unmodified);
      Finalize (State_Machine (Session));
   end Finalize;

   procedure Get
             (  Session : in out HTTP_Session;
                Name    : String;
                Message : access Root_Stream_Type'Class;
                From    : Stream_Element_Count := 1;
                To      : Stream_Element_Count := 0
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session, From, To);
      Set_Request_Header (Session, Content_Type_Header, "");
      Session.Method    := HTTP_GET;
      Session.Stream    := Message.all'Unchecked_Access;
      Session.Expecting := Response_Line;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.GET'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Get;

   procedure Get
             (  Session : in out HTTP_Session;
                Name    : String;
                Message : access HTTP_Server.Content_Destination'Class;
                From    : Stream_Element_Count := 1;
                To      : Stream_Element_Count := 0
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session, From, To);
      Set_Request_Header (Session, Content_Type_Header, "");
      Session.Method      := HTTP_GET;
      Session.Destination := Message.all'Unchecked_Access;
      Session.Expecting   := Response_Line;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.GET'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Get;

   function Get (Data : String_Data_Ptr) return String is
   begin
      if Data = null then
         return "";
      else
         return Data.Text (1..Data.Length);
      end if;
   end Get;

   function Get
            (  Session : HTTP_Session;
               Header  : Request_Header
            )  return String is
   begin
      return Get (Session.Request_Headers (Header));
   end Get;

   function Get_Keep_Alive (Session : HTTP_Session) return Boolean is
   begin
      return Session.Keep_Alive;
   end Get_Keep_Alive;

   function Get_Prefix
            (  Session : access Connection'Class;
               Data    : Stream_Element_Array;
               End_Of_Stream : Boolean
            )  return String is
   begin
      return Image (Stream_Element_Count'(Data'Length), 16) & CRLF;
   end Get_Prefix;

   function Get_Request_Header
            (  Session : HTTP_Session;
               Header  : HTTP_Server.Text_Header
            )  return String is
   begin
      return Get (Session, Header);
   end Get_Request_Header;

   function Get_Request_Method
            (  Session : HTTP_Session
            )  return HTTP_Method is
   begin
      return Session.Method;
   end Get_Request_Method;

   procedure Get_Request_Range
             (  Session : HTTP_Session;
                From    : out Stream_Element_Count;
                To      : out Stream_Element_Count
             )  is
   begin
      From := Session.Range_From;
      To   := Session.Range_To;
   end Get_Request_Range;

   function Get_Response_Code (Session : HTTP_Session)
      return Positive is
   begin
      return Session.Code;
   end Get_Response_Code;

   function Get_Response_Connection_Flags
            (  Session : HTTP_Session
            )  return Connection_Flags is
   begin
      return Session.Connection;
   end Get_Response_Connection_Flags;

   function Get_Response_Header
            (  Session : HTTP_Session;
               Header  : Text_Header
            )  return String is
   begin
      if Session.Response_Headers (Header) = null then
         return "";
      else
         return Session.Response_Headers (Header).all;
      end if;
   end Get_Response_Header;

   function Get_Response_Reason (Session : HTTP_Session)
      return String is
   begin
      if Session.Reason = null then
         return "";
      else
         return Session.Reason.all;
      end if;
   end Get_Response_Reason;

   function Get_Response_Version (Session : HTTP_Session)
      return HTTP_Version is
   begin
      return Session.Version;
   end Get_Response_Version;

   function Get_Session_State (Session : HTTP_Session)
      return Session_State is
      Result : constant Session_State :=
               Get_Session_State (State_Machine (Session));
   begin
      if Result = Session_Connected then
         if Session.Expecting = Nothing then
            return Result;
         else
            return Session_Busy;
         end if;
      else
         return Result;
      end if;
   end Get_Session_State;

   function Get_Suffix
            (  Session : access Connection'Class;
               Data    : Stream_Element_Array;
               End_Of_Stream : Boolean
            )  return String is
   begin
      if End_Of_Stream then
         if Data'Length = 0 then
            return CRLF & CRLF; -- Last empty chunk
         else
            return CRLF & '0' & CRLF & CRLF; -- Last not empty chunk
         end if;
      else
         return CRLF;
      end if;
   end Get_Suffix;

   procedure Head
             (  Session : in out HTTP_Session;
                Name    : String;
                From    : Stream_Element_Count := 1;
                To      : Stream_Element_Count := 0
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session, From, To);
      Set_Request_Header (Session, Content_Type_Header, "");
      Session.Method    := HTTP_HEAD;
      Session.Expecting := Response_Line;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.HEAD'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Head;

   procedure Header_Received
             (  Session : in out HTTP_Session'Class;
                Header  : Response_Header;
                Value   : String
             )  is
      procedure Get_Text is
         pragma Inline (Get_Text);
         type Header_Ptr is access String;
         for Header_Ptr'Storage_Pool use Session.Pool;
         Ptr : Header_Ptr;
      begin
         if Session.Response_Headers (Header) /= null then
            return; -- The value is already set
         end if;
         Ptr := new String'(Value);
         Session.Response_Headers (Header) := Ptr.all'Unchecked_Access;
      end Get_Text;
      function Get_Time return Time is
      begin
         return To_Time (Value);
      exception
         when Data_Error | Time_Error =>
            declare
               Seconds : Integer;
            begin
               Seconds := Strings_Edit.Integers.Value (Value);
               return Clock + Duration (Seconds);
            exception
               when others =>
                  return Clock - 1.0;
            end;
      end Get_Time;
   begin
      case Header is
         when Text_Header =>
            Get_Text;
         when Date_Header =>
            Session.Date := Get_Time;
            Session.Specific (Date_Header) := True;
         when Connection_Header =>
            Session.Connection := To_Flags (Value);
         when Content_Length_Header =>
            Session.Data_Length :=
               Stream_Element_Offset_Edit.Value (Value);
         when Expires_Header =>
            Session.Expires := Get_Time;
            Session.Specific (Expires_Header) := True;
         when Last_Modified_Header =>
            Session.Last_Modified := Get_Time;
            Session.Specific (Last_Modified_Header) := True;
         when Retry_After_Header =>
            Session.Retry_After := Get_Time;
            Session.Specific (Retry_After_Header) := True;
      end case;
   end Header_Received;

   procedure Initialize (Session : in out HTTP_Session) is
   begin
      Session.LF.Value (1) := 10;
      Session.Line.Terminator := Character'Val (13);
      Set_Maximum_Size (Session.Line, Session.Response_Length);
      Initialize (State_Machine (Session));
   end Initialize;

   function Image (Header : Response_Header) return String is
      Result     : String  := Response_Header'Image (Header);
      Capitalize : Boolean := True;
   begin
      for Index in Result'First..Result'Last - 7 loop
         if Result (Index) = '_' then
            Result (Index) := '-';
            Capitalize := True;
         elsif Capitalize then
            Result (Index) := To_Upper (Result (Index));
            Capitalize := False;
         else
            Result (Index) := To_Lower (Result (Index));
         end if;
      end loop;
      return Result (Result'First..Result'Last - 7);
   end Image;

   function Is_Active (Session : HTTP_Session) return Boolean is
   begin
      return Session.Expecting /= Nothing;
   end Is_Active;

   procedure Message_Store_Error
             (  Session : in out HTTP_Session;
                Stream  : in out Root_Stream_Type'Class;
                Error   : Exception_Occurrence
             )  is
   begin
      Raise_Exception (Data_Error'Identity, Exception_Message (Error));
   end Message_Store_Error;

   procedure Message_Store_Error
             (  Session : in out HTTP_Session;
                Content : in out HTTP_Server.Content_Destination'Class;
                Error   : Exception_Occurrence
             )  is
   begin
      Raise_Exception (Data_Error'Identity, Exception_Message (Error));
   end Message_Store_Error;

   procedure Options
             (  Session : in out HTTP_Session;
                Name    : String
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, "");
      Session.Method    := HTTP_OPTIONS;
      Session.Expecting := Response_Line;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.OPTIONS'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Options;

   procedure Options
             (  Session : in out HTTP_Session;
                Name    : String;
                Message : access Root_Stream_Type'Class
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, "");
      Session.Method    := HTTP_OPTIONS;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.OPTIONS'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Options;

   procedure Options
             (  Session : in out HTTP_Session;
                Name    : String;
                Message : access HTTP_Server.Content_Destination'Class
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, "");
      Session.Method      := HTTP_OPTIONS;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.OPTIONS'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Options;
   ------------------------------------------------------ String content
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : String;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_Post;
      Session.Expecting := Response_Line;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.POST'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         ( "Content-Length: "
         &  Strings_Edit.Integers.Image (Content'Length)
         &  CRLF
      )  );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Queue_String  (Session, Content);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Post;

   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : String;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_Post;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.POST'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         ( "Content-Length: "
         &  Strings_Edit.Integers.Image (Content'Length)
         &  CRLF
      )  );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Queue_String  (Session, Content);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Post;

   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : String;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method      := HTTP_Post;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.POST'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         ( "Content-Length: "
         &  Strings_Edit.Integers.Image (Content'Length)
         &  CRLF
      )  );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Queue_String  (Session, Content);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Post;
   ---------------------------------------- Stream element array content
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : Stream_Element_Array;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_Post;
      Session.Expecting := Response_Line;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.POST'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         ( "Content-Length: "
         &  Strings_Edit.Integers.Image (Content'Length)
         &  CRLF
      )  );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Queue_String  (Session, Content);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Post;

   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : Stream_Element_Array;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_Post;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.POST'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         ( "Content-Length: "
         &  Strings_Edit.Integers.Image (Content'Length)
         &  CRLF
      )  );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Queue_String  (Session, Content);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Post;

   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : Stream_Element_Array;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method      := HTTP_Post;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.POST'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         ( "Content-Length: "
         &  Strings_Edit.Integers.Image (Content'Length)
         &  CRLF
      )  );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Queue_String  (Session, Content);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Post;
   --------------------------------------------- Stream content, chunked
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_Post;
      Session.Expecting := Response_Line;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.POST'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Chunked_Stream, 0, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Post;

   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_Post;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.POST'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Chunked_Stream, 0, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Post;

   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method      := HTTP_Post;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.POST'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Chunked_Stream, 0, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Post;
   ---------------------------------------- Stream content, fixed length
   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Length  : Stream_Element_Count;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_Post;
      Session.Expecting := Response_Line;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.POST'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         "Content-Length: " & Image (Length) & CRLF
      );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Stream, Length, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Post;

   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Length  : Stream_Element_Count;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_Post;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.POST'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         "Content-Length: " & Image (Length) & CRLF
      );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Stream, Length, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Post;

   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Length  : Stream_Element_Count;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method      := HTTP_Post;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.POST'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         "Content-Length: " & Image (Length) & CRLF
      );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Stream, Length, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Post;

   procedure Post -- Custom content, chunked
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_Post;
      Session.Expecting := Response_Line;
      Queue_String  (Session, Strings.POST'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Chunked_Content, 0, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_None (Session);
      Queue_None (Session);
      Queue_None (Session);
      Queue_None (Session);
      Sent (Session);
   end Post;

   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_Post;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Queue_String  (Session, Strings.POST'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Chunked_Content, 0, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_None (Session);
      Queue_None (Session);
      Queue_None (Session);
      Queue_None (Session);
      Sent (Session);
   end Post;

   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method      := HTTP_Post;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Queue_String  (Session, Strings.POST'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Chunked_Content, 0, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_None (Session);
      Queue_None (Session);
      Queue_None (Session);
      Queue_None (Session);
      Sent (Session);
   end Post;

   procedure Post  -- Custom content, fixed length
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Length  : Stream_Element_Count;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_Post;
      Session.Expecting := Response_Line;
      Queue_String (Session, Strings.POST'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         "Content-Length: " & Image (Length) & CRLF
      );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Content, Length, Content.all'Unchecked_Access)
      );
      Queue_None (Session);
      Queue_None (Session);
      Sent (Session);
   end Post;

   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Length  : Stream_Element_Count;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_Post;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Queue_String (Session, Strings.POST'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         "Content-Length: " & Image (Length) & CRLF
      );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Content, Length, Content.all'Unchecked_Access)
      );
      Queue_None (Session);
      Queue_None (Session);
      Sent (Session);
   end Post;

   procedure Post
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Length  : Stream_Element_Count;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Post_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method      := HTTP_Post;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Queue_String (Session, Strings.POST'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         "Content-Length: " & Image (Length) & CRLF
      );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Content, Length, Content.all'Unchecked_Access)
      );
      Queue_None (Session);
      Queue_None (Session);
      Sent (Session);
   end Post;
   ---------------------------------------------------------------------
   procedure Process_Chunk_Line (Session : in out HTTP_Session'Class) is
      Response : String renames
                 Session.Line.Value (1..Session.Line.Last);
      Pointer  : Integer := Response'First;
   begin
      Get
      (  Source  => Response,
         Pointer => Pointer,
         Value   => Session.Data_Length,
         Base    => 16,
         First   => 0
      );
      if Session.Data_Length = 0 then
         Session.Expecting := Chunk_Footer;
      else
         Session.Expecting := Chunk_Data;
      end if;
   exception
      when Data_Error | Constraint_Error | End_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            "Malformed data chunk header line"
         );
   end Process_Chunk_Line;

   procedure Process_Header_Line
             (  Session : in out HTTP_Session'Class
             )  is
      Response : String renames
                    Session.Line.Value (1..Session.Line.Last);
      Pointer  : Integer := Response'First;
   begin
      if Pointer > Response'Last then
         if Session.Trace_Header then
            Trace (Session, "Header:");
            for Index in Session.Response_Headers'Range loop
               if Session.Response_Headers (Index) /= null then
                  Trace
                  (  Session,
                     (  "   "
                     &  Text_Header'Image (Index)
                     &  '='
                     &  Quote
                        (  Session.Response_Headers (Index).all,
                           '''
                  )  )  );
               end if;
            end loop;
         end if;
         if Session.Expecting = Chunk_Footer then
            End_Of_Query (Session);
         else
            case Session.Method is
               when HTTP_GET     | HTTP_POST  | HTTP_PUT     |
                    HTTP_DELETE  | HTTP_TRACE | HTTP_OPTIONS |
                    HTTP_CONNECT | HTTP_PATCH =>
                  case Session.Code is
                     when 100..199 | 204 | 304 => -- No message data
                        End_Of_Query (Session);
                     when others => -- The message data follow
                        if (  "chunked"
                           =  To_Lower
                              (  Get_Response_Header
                                 (  Session,
                                    Transfer_Encoding_Header
                           )  )  )
                        then
                           Session.Expecting := Chunk_Header;
                        elsif Session.Data_Length = 0 then
                           End_Of_Query (Session);
                        else
                           Session.Expecting := Response_Data;
                        end if;
                  end case;
               when HTTP_HEAD =>
                  End_Of_Query (Session);
            end case;
         end if;
      else
         while Pointer <= Response'Last loop
            if Response (Pointer) = ':' then
               declare
                  Input : constant Header_Value :=
                          Value
                          (  Response
                             (  Response'First
                             .. Pointer - 1
                          )  );
               begin
                  if Input.None then
                     Trace
                     (  Session,
                        (  "Unsupported header "
                        &  Quote
                           (  Response (Response'First..Pointer - 1),
                              '''
                     )  )  );
                  else
                     Pointer := Pointer + 1;
                     if Pointer <= Response'Last and then
                        Response (Pointer) = ' '
                     then
                        Pointer := Pointer + 1;
                     end if;
                     Header_Received
                     (  Session,
                        Input.Header,
                        Response (Pointer..Response'Last)
                     );
                  end if;
               end;
               return;
            end if;
            Pointer := Pointer + 1;
         end loop;
         Trace (Session, "Malformed header " & Response);
      end if;
   end Process_Header_Line;

   procedure Process_Response_Line
             (  Session : in out HTTP_Session'Class
             )  is
      Response : String renames
                    Session.Line.Value (1..Session.Line.Last);
      Pointer  : Integer := Response'First;
      function Get_Version return HTTP_Version is
         Version : Float;
      begin
         Skip_Blanks (Response, Pointer);
         if not Is_Prefix ("http/", Response, Pointer, Lower) then
            Trace (Session, "No HTTP version specified (1.1 assumed)");
            return 1.1;
         end if;
         Pointer := Pointer + 5;
         begin
            Get (Response, Pointer, Version);
         exception
            when others =>
               raise Constraint_Error;
         end;
         return HTTP_Version (Version);
      exception
         when Constraint_Error =>
            Trace (Session, "Illegal HTTP version (1.1 assumed)");
            return 1.1;
      end Get_Version;
   begin
      Session.Version := Get_Version;
      Skip_Blanks (Response, Pointer);
      begin
         Get
         (  Source  => Response,
            Pointer => Pointer,
            Value   => Session.Code,
            First   => 100,
            Last    => 599
         );
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Illegal status code"
            );
      end;
      Skip_Blanks (Response, Pointer);
      declare
         type Reason_Ptr is access String;
         for Reason_Ptr'Storage_Pool use Session.Pool;
         Ptr : Reason_Ptr;
      begin
         Ptr := new String'(Response (Pointer..Response'Last));
         Session.Reason := Ptr.all'Unchecked_Access;
      end;
      Session.Expecting := Header_Line;
   end Process_Response_Line;

   procedure Process_Trailer_CRLF
             (  Session : in out HTTP_Session'Class
             )  is
   begin
      if Session.Expecting = Chunk_Data_CRLF then
         Session.Expecting := Chunk_Header;
      else
         End_Of_Query (Session);
      end if;
   end Process_Trailer_CRLF;
   ------------------------------------------------------ String content
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : String;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_PUT;
      Session.Expecting := Response_Line;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.PUT'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         ( "Content-Length: "
         &  Strings_Edit.Integers.Image (Content'Length)
         &  CRLF
      )  );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Queue_String  (Session, Content);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Put;

   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : String;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_PUT;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.PUT'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         ( "Content-Length: "
         &  Strings_Edit.Integers.Image (Content'Length)
         &  CRLF
      )  );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Queue_String  (Session, Content);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Put;

   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : String;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method      := HTTP_PUT;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String  (Session, Strings.PUT'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         ( "Content-Length: "
         &  Strings_Edit.Integers.Image (Content'Length)
         &  CRLF
      )  );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Queue_String  (Session, Content);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Put;
   ---------------------------------------- Stream element array content
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : Stream_Element_Array;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_PUT;
      Session.Expecting := Response_Line;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.PUT'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         ( "Content-Length: "
         &  Strings_Edit.Integers.Image (Content'Length)
         &  CRLF
      )  );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Queue_String  (Session, Content);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Put;

   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : Stream_Element_Array;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_PUT;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.PUT'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         ( "Content-Length: "
         &  Strings_Edit.Integers.Image (Content'Length)
         &  CRLF
      )  );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Queue_String  (Session, Content);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Put;

   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : Stream_Element_Array;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method      := HTTP_PUT;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.PUT'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         ( "Content-Length: "
         &  Strings_Edit.Integers.Image (Content'Length)
         &  CRLF
      )  );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Queue_String  (Session, Content);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Put;
   --------------------------------------------- Stream content, chunked
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_PUT;
      Session.Expecting := Response_Line;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.PUT'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Chunked_Stream, 0, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Put;

   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_PUT;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.PUT'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Chunked_Stream, 0, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Put;

   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method      := HTTP_PUT;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.PUT'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Chunked_Stream, 0, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Put;
   ---------------------------------------- Stream content, fixed length
   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Length  : Stream_Element_Count;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_PUT;
      Session.Expecting := Response_Line;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.PUT'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         "Content-Length: " & Image (Length) & CRLF
      );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Stream, Length, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Put;

   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Length  : Stream_Element_Count;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_PUT;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.PUT'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         "Content-Length: " & Image (Length) & CRLF
      );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Stream, Length, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Put;

   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access Root_Stream_Type'Class;
                Length  : Stream_Element_Count;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method      := HTTP_PUT;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Set (Session.Page_Name, Name);
      Queue_String (Session, Strings.PUT'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         "Content-Length: " & Image (Length) & CRLF
      );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Stream, Length, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Sent (Session);
   end Put;

   procedure Put -- Custom content, chunked
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_PUT;
      Session.Expecting := Response_Line;
      Queue_String  (Session, Strings.PUT'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Chunked_Content, 0, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_None (Session);
      Queue_None (Session);
      Queue_None (Session);
      Queue_None (Session);
      Sent (Session);
   end Put;

   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_PUT;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Queue_String  (Session, Strings.PUT'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Chunked_Content, 0, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_None (Session);
      Queue_None (Session);
      Queue_None (Session);
      Queue_None (Session);
      Sent (Session);
   end Put;

   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method      := HTTP_PUT;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Queue_String  (Session, Strings.PUT'Unchecked_Access);
      Queue_String  (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String  (Session, Strings.HTTP'Unchecked_Access);
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Chunked_Content, 0, Content.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
      Queue_None (Session);
      Queue_None (Session);
      Queue_None (Session);
      Queue_None (Session);
      Sent (Session);
   end Put;

   procedure Put  -- Custom content, fixed length
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Length  : Stream_Element_Count;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_PUT;
      Session.Expecting := Response_Line;
      Queue_String (Session, Strings.PUT'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         "Content-Length: " & Image (Length) & CRLF
      );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Content, Length, Content.all'Unchecked_Access)
      );
      Queue_None (Session);
      Queue_None (Session);
      Sent (Session);
   end Put;

   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Length  : Stream_Element_Count;
                Message : access Root_Stream_Type'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method    := HTTP_PUT;
      Session.Expecting := Response_Line;
      Session.Stream    := Message.all'Unchecked_Access;
      Queue_String (Session, Strings.PUT'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         "Content-Length: " & Image (Length) & CRLF
      );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Content, Length, Content.all'Unchecked_Access)
      );
      Queue_None (Session);
      Queue_None (Session);
      Sent (Session);
   end Put;

   procedure Put
             (  Session : in out HTTP_Session;
                Name    : String;
                Content : access HTTP_Server.Content_Source'Class;
                Length  : Stream_Element_Count;
                Message : access HTTP_Server.Content_Destination'Class;
                MIME    : String := Put_MIME
             )  is
   begin
      if not Is_Connected (Session) then
         Raise_Exception (Use_Error'Identity, "Not connected");
      elsif Is_Active (Session) then
         Raise_Exception (Use_Error'Identity, "A request is pending");
      end if;
      Cleanup (Session);
      Set_Request_Range  (Session);
      Set_Request_Header (Session, Content_Type_Header, MIME);
      Session.Method      := HTTP_PUT;
      Session.Expecting   := Response_Line;
      Session.Destination := Message.all'Unchecked_Access;
      Queue_String (Session, Strings.PUT'Unchecked_Access);
      Queue_String (Session, Session.Page_Name.all'Unchecked_Access);
      Queue_String (Session, Strings.HTTP'Unchecked_Access);
      Queue_String
      (  Session,
         "Content-Length: " & Image (Length) & CRLF
      );
      Queue_Headers (Session);
      Queue_String  (Session, Strings.EOL'Unchecked_Access);
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Content, Length, Content.all'Unchecked_Access)
      );
      Queue_None (Session);
      Queue_None (Session);
      Sent (Session);
   end Put;

   procedure Queue_Action
             (  Session : in out HTTP_Session'Class;
                Chain   : Action
             )  is
   begin
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Chain, 0, Chain)
      );
      Session.Send_Length := Session.Send_Length + 1;
   end Queue_Action;

   procedure Queue_None (Session : in out HTTP_Session'Class) is
   begin
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_None, 0)
      );
      Session.Send_Length := Session.Send_Length + 1;
   end Queue_None;

   procedure Queue_String
             (  Session : in out HTTP_Session'Class;
                Text    : Text_Data_Ptr
             )  is
   begin
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_String, 1, Text)
      );
      Session.Send_Length := Session.Send_Length + 1;
   end Queue_String;

   procedure Queue_String
             (  Session : in out HTTP_Session'Class;
                Text    : String
             )  is
      type Text_Ptr is access String;
      for Text_Ptr'Storage_Pool use Session.Pool;
      Ptr : constant Text_Ptr := new String'(Text);
   begin
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Literal_String, 1, Ptr.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
   end Queue_String;

   procedure Queue_String
             (  Session : in out HTTP_Session'Class;
                Text    : Stream_Element_Array
             )  is
      type Text_Ptr is access Stream_Element_Array;
      for Text_Ptr'Storage_Pool use Session.Pool;
      Ptr : constant Text_Ptr := new Stream_Element_Array'(Text);
   begin
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Literal_Stream, 1, Ptr.all'Unchecked_Access)
      );
      Session.Send_Length := Session.Send_Length + 1;
   end Queue_String;

   procedure Queue_Headers (Session : in out HTTP_Session'Class) is
   begin
      if Session.Keep_Alive then
         Queue_String (Session, Strings.Keep_Alive'Unchecked_Access);
      else
         Queue_String (Session, Strings.Close'Unchecked_Access);
      end if;
      if Session.Range_From <= Session.Range_To then
         Queue_String
         (  Session,
            (  "Range: bytes="
            &  Image (Session.Range_From)
            &  "-"
            &  Image (Session.Range_To)
            &  CRLF
         )  );
      end if;
      if Session.Send_Date then
         Queue_String (Session, "Date: " & To_HTTP (Clock) & CRLF);
      end if;
      if (  Session.If_Modified /= null
         and then
            Session.If_Modified.Length > 0
         )
      then
         Queue_String
         (  Session,
            Session.If_Modified.all'Unchecked_Access
         );
      end if;
      if (  Session.If_Unmodified /= null
         and then
            Session.If_Unmodified.Length > 0
         )
      then
         Queue_String
         (  Session,
            Session.If_Unmodified.all'Unchecked_Access
         );
      end if;
      Put
      (  Session.Send_List,
         Session.Send_Length + 1,
         (Sending_Headers, -1, 0)
      );
      Session.Send_Length := Session.Send_Length + 1;
   end Queue_Headers;

   procedure Receive_Header_Tracing
             (  Session : in out HTTP_Session;
                Enable  : Boolean
             )  is
   begin
      Session.Trace_Header := Enable;
   end Receive_Header_Tracing;

   procedure Receive_Message_Tracing
             (  Session : in out HTTP_Session;
                Enable : Boolean
             )  is
   begin
      Session.Trace_Message := Enable;
   end Receive_Message_Tracing;

   procedure Receive_Header_Line
             (  Session : in out HTTP_Session'Class;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Handler : Action
             )  is
   begin
      Get_Header_Line : loop
         Feed
         (  Session.Data.List (Session.Data.Current).all,
            Data,
            Pointer,
            Session,
            Session.State
         );
         if Session.State = 0 then -- Done with this item
            Session.Data.Current := Session.Data.Current + 1;
            while Session.Data.Current > Session.Data.Length loop
               if Session.Data.Caller = null then
                  Session.Data.Current := 1;
                  Handler (Session);
                  exit Get_Header_Line;
               end if;
               Session.Data := Session.Data.Caller;
               Session.Data.Current := Session.Data.Current + 1;
            end loop;
         else
            exit when Pointer > Data'Last; -- All data consumed
            Raise_Exception
            (  Status_Error'Identity,
               (  "Unprocessed data left when after return from "
               &  Expanded_Name
                  (  Session.Data.List (Session.Data.Current).all'Tag
            )  )  );
         end if;
      end loop Get_Header_Line;
   end Receive_Header_Line;

   procedure Received
             (  Session : in out HTTP_Session;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      function Done (Data : Stream_Element_Array) return String is
      begin
         return (1..Data'Length => '.');
      end Done;

      function Left (Data : Stream_Element_Array) return String is
      begin
         return (1..Data'Length => 'x');
      end Left;

      procedure Store (Data : Stream_Element_Array) is
         pragma Inline (Store);
      begin
         if Data'Length > 0 then
            if Session.Stream /= null then
               begin
                  Write (Session.Stream.all, Data);
               exception
                  when Error : others =>
                     Message_Store_Error -- Dispatching call
                     (  HTTP_Session'Class (Session),
                        Session.Stream.all,
                        Error
                     );
                     Session.Stream := null;
               end;
            elsif Session.Destination /= null then
               begin
                  Put (Session.Destination.all, To_String (Data));
               exception
                  when Error : others =>
                     Message_Store_Error -- Dispatching call
                     (  HTTP_Session'Class (Session),
                        Session.Destination.all,
                        Error
                     );
                     Session.Destination := null;
               end;
            end if;
         end if;
      end Store;

      procedure Store (Data : String) is
         pragma Inline (Store);
      begin
         if Data'Length > 0 then
            if Session.Stream /= null then
               begin
                  Write (Session.Stream.all, From_String (Data));
               exception
                  when Error : others =>
                     Message_Store_Error -- Dispatching call
                     (  HTTP_Session'Class (Session),
                        Session.Stream.all,
                        Error
                     );
                     Session.Stream := null;
               end;
            elsif Session.Destination /= null then
               begin
                  Put (Session.Destination.all, Data);
               exception
                  when Error : others =>
                     Message_Store_Error -- Dispatching call
                     (  HTTP_Session'Class (Session),
                        Session.Destination.all,
                        Error
                     );
                     Session.Destination := null;
               end;
           end if;
         end if;
      end Store;

   begin
      Pointer := Data'First;
      while Pointer <= Data'Last loop
         case Session.Expecting is
            when Nothing => -- Unsolicited data
               Trace
               (  Session,
                  (  "Unsolicited data |"
                  &  Image (Data (Pointer..Data'Last))
                  &  "|"
               )  );
               Raise_Exception
               (  Data_Error'Identity,
                  "Unsolicited server response"
               );
            when Response_Data | Chunk_Data => -- Response data receipt
               if Session.Data_Length > 0 then -- Receiving the body
                  if Session.Data_Length > Data'Last - Pointer + 1 then
                     Session.Data_Length :=
                        Session.Data_Length - Data'Last + Pointer - 1;
                     if Session.Trace_Message then
                        Trace
                        (  Session,
                           (  "Append data "
                           &  Done (Data (Data'First..Pointer - 1))
                           &  "|"
                           &  Image (Data (Pointer..Data'Last))
                           &  "| "
                           &  Image (Session.Data_Length)
                           &  " more expected"
                        )  );
                     end if;
                     Store (Data (Pointer..Data'Last));
                     Pointer := Data'Last + 1;
                  else
                     if Session.Trace_Message then
                        Trace
                        (  Session,
                           (  "Append data "
                           &  Done (Data (Data'First..Pointer - 1))
                           &  "|"
                           &  Image
                              (  Data
                                 (  Pointer
                                 .. Pointer + Session.Data_Length - 1
                              )  )
                           &  "|"
                           &  Left
                              (  Data
                                 (  Pointer + Session.Data_Length
                                 .. Data'Last
                              )  )
                           &  " No more expected"
                        )  );
                     end if;
                     Store
                     (  Data
                        (  Pointer
                        .. Pointer + Session.Data_Length - 1
                     )  );
                     Pointer := Pointer + Session.Data_Length;
                     Session.Data_Length := 0;
                  end if;
               end if;
               if Session.Data_Length = 0 then
                  if Session.Expecting = Chunk_Data then
                     Session.Expecting := Chunk_Data_CRLF;
                  else
                     End_Of_Query (HTTP_Session'Class (Session));
                  end if;
               end if;
            when Response_Line =>
               Receive_Header_Line
               (  Session,
                  Data,
                  Pointer,
                  Process_Response_Line'Access
               );
            when Header_Line | Chunk_Footer =>
               Receive_Header_Line
               (  Session,
                  Data,
                  Pointer,
                  Process_Header_Line'Access
               );
            when Chunk_Header =>
               Receive_Header_Line
               (  Session,
                  Data,
                  Pointer,
                  Process_Chunk_Line'Access
               );
            when Chunk_Data_CRLF =>
               Receive_Header_Line
               (  Session,
                  Data,
                  Pointer,
                  Process_Trailer_CRLF'Access
               );
         end case;
      end loop;
   end Received;

   procedure Send
             (  Session : in out HTTP_Session'Class;
                Message : String
             )  is
      Pointer : Integer := Message'First;
   begin
      Send (Session, Message, Pointer);
      if Pointer <= Message'Last then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Output buffer overrun, "
            &  Image (Queued_To_Send (Session))
            &  " elements queued, space for at least more "
            &  Image (Message'Last - Pointer + 1)
            &  " requred (available "
            &  Image (Available_To_Send (Session))
            &  ")"
         )  );
      end if;
   end Send;

   procedure Send
             (  Session : in out HTTP_Session'Class;
                Message : Stream_Element_Array
             )  is
      Pointer : Stream_Element_Offset := Message'First;
   begin
      Send (Session, Message, Pointer);
      if Pointer <= Message'Last then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Output buffer overrun, "
            &  Image (Queued_To_Send (Session))
            &  " elements queued, space for at least more "
            &  Image (Message'Last - Pointer + 1)
            &  " requred (available "
            &  Image (Available_To_Send (Session))
            &  ")"
         )  );
      end if;
   end Send;

   procedure Sent (Session : in out HTTP_Session) is
      Completed : Boolean;

      procedure Send_Headers
                (  Header   : in out Natural;
                   Position : in out Integer
                )  is
      begin
         while Header < Request_Header_Array'Length loop
            declare
               Current : constant Request_Header :=
                         Request_Header'Val (Header);
            begin
               if Session.Request_Headers (Current) /= null then
                  if Position < 0 then
                     declare
                        Prefix  : constant String :=
                                  Image (Current) & ": ";
                        Pointer : Integer := -Position;
                     begin
                        if Pointer <= Prefix'Length then
                           Send (Session, Prefix, Pointer);
                           if Pointer <= Prefix'Last then
                              Position  := -Pointer;
                              Completed := False;
                              return;
                           end if;
                        end if;
                     end;
                     Position := 1;
                  end if;
                  declare
                     This : String_Data renames
                            Session.Request_Headers (Current).all;
                  begin
                     if Position <= This.Length then
                        Send
                        (  Session,
                           This.Text (Position..This.Length),
                           Position
                        );
                        if Position <= This.Length then
                           Completed := False;
                           return;
                        end if;
                     end if;
                     if Position <= This.Length + 2 then
                        declare
                           Pointer : Integer := CRLF'First;
                        begin
                           Send (Session, CRLF, Pointer);
                           if Pointer <= CRLF'Last then
                              Position  := Position + 1;
                              Completed := False;
                              return;
                           end if;
                           Position := -1;
                        end;
                     end if;
                  end;
               end if;
            end;
            Header := Header + 1;
         end loop;
         Completed := True;
      end Send_Headers;

      procedure Send_Content
                (  Source : Content_Source_Ptr;
                   Count  : in out Stream_Element_Offset
                )  is
         type String_Ptr is access String;
         for String_Ptr'Storage_Pool use Session.Pool;
         Chunk : String_Ptr;
      begin
         Chunk := new String'(Get (Source));
         if Chunk'Length = 0 then -- End of data
            Put -- CRLF
            (  Session.Send_List,
               Session.Send_Index + 1,
               (  Sending_String,
                  1,
                  Strings.EOL'Unchecked_Access
            )  );
            Put -- Nothing
            (  Session.Send_List,
               Session.Send_Index + 2,
               (Sending_None, 0)
            );
            if Count > 0 then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Missing "
                  &  Image (Count)
                  &  " contents data to transfer"
               )  );
            end if;
         else -- A chunk of data obtained
            if Chunk'Length < Count then
               Count := Count - Chunk'Length;
               Put -- Data
               (  Session.Send_List,
                  Session.Send_Index + 1,
                  (  Sending_Literal_String,
                     1,
                     Chunk.all'Unchecked_Access
               )  );
               Put -- Free memory and repeat
               (  Session.Send_List,
                  Session.Send_Index + 2,
                  (  Sending_Clean_And_Jump,
                     -2,
                     Chunk.all'Unchecked_Access
               )  );
            else -- Send slice of the chunk and finish
               declare
                  type Slice_Ptr is access Slice;
                  for Slice_Ptr'Storage_Pool use Session.Pool;
                  Data : constant Slice_Ptr :=
                         new Slice'(Count, Chunk.all'Unchecked_Access);
               begin
                  Put -- Data (1..Count)
                  (  Session.Send_List,
                     Session.Send_Index + 1,
                     (  Sending_Slice,
                        1,
                        Data.all'Unchecked_Access
                  )  );
                  Put -- CRLF
                  (  Session.Send_List,
                     Session.Send_Index + 2,
                     (  Sending_String,
                        1,
                        Strings.EOL'Unchecked_Access
                  )  );
               end;
            end if;
         end if;
      exception
         when Content_Not_Ready =>
            Raise_Exception
            (  Data_Error'Identity,
               "Content not ready"
            );
      end Send_Content;

      procedure Send_Content (Source : Content_Source_Ptr) is
         type String_Ptr is access String;
         for String_Ptr'Storage_Pool use Session.Pool;
         Chunk : String_Ptr;
         Head  : String_Ptr;
      begin
         Chunk := new String'(Get (Source));
         Head  := new String'(Integers.Image (Chunk'Length, 16) & CRLF);
         Put
         (  Session.Send_List,
            Session.Send_Index + 1,
            (  Sending_Literal_String,
               1,
               Head.all'Unchecked_Access
         )  );
         if Chunk'Length = 0 then
            Put -- CRLF
            (  Session.Send_List,
               Session.Send_Index + 2,
               (  Sending_String,
                  1,
                  Strings.EOL'Unchecked_Access
            )  );
            Put -- Nothing
            (  Session.Send_List,
               Session.Send_Index + 3,
               (Sending_None, 0)
            );
            Put -- Free memory
            (  Session.Send_List,
               Session.Send_Index + 4,
               (  Sending_Clean_And_Jump,
                  0,
                  Chunk.all'Unchecked_Access
            )  );
         else
            Put -- Data
            (  Session.Send_List,
               Session.Send_Index + 2,
               (  Sending_Literal_String,
                  1,
                  Chunk.all'Unchecked_Access
            )  );
            Put -- CRLF
            (  Session.Send_List,
               Session.Send_Index + 3,
               (  Sending_String,
                  1,
                  Strings.EOL'Unchecked_Access
            )  );
            Put -- Free memory and repeat
            (  Session.Send_List,
               Session.Send_Index + 4,
               (  Sending_Clean_And_Jump,
                  -4,
                  Chunk.all'Unchecked_Access
            )  );
         end if;
      exception
         when Content_Not_Ready =>
            Raise_Exception
            (  Data_Error'Identity,
               "Content not ready"
            );
      end Send_Content;

      Index  : Positive renames Session.Send_Index;
      Length : Natural  renames Session.Send_Length;
   begin
      while Available_To_Send (Session) > 0 and then Index <= Length
      loop
         declare
            Data : Send_Data renames Session.Send_List.Vector (Index);
         begin
            case Data.Kind_Of is
               when Sending_None =>
                  null;
               when Sending_Headers =>
                  Send_Headers (Data.Header, Integer (Data.Count));
                  exit when not Completed;
               when Sending_Literal_Stream =>
                  if Data.Stream /= null then
                     declare
                        This : Stream_Element_Array renames
                               Data.Stream.all;
                     begin
                        if Data.Count <= This'Last then
                           Send
                           (  Session,
                              This (Data.Count..This'Last),
                              Data.Count
                           );
                           exit when Data.Count <= This'Last;
                        end if;
                     end;
                  end if;
               when Sending_Literal_String =>
                  if Data.Text /= null then
                     declare
                        This  : String renames Data.Text.all;
                        Count : Integer := Integer (Data.Count);
                     begin
                        if Count <= This'Last then
                           Send
                           (  Session,
                              This (Count..This'Last),
                              Count
                           );
                           Data.Count := Stream_Element_Offset (Count);
                           exit when Count <= This'Last;
                        end if;
                     end;
                  end if;
               when Sending_Slice =>
                  if Data.Slice /= null and then Data.Slice.Text /= null
                  then
                     declare
                        This   : String renames Data.Slice.Text.all;
                        Length : constant Integer :=
                                 Integer (Data.Slice.Length);
                        Count  : Integer := Integer (Data.Count);
                     begin
                        if Count < Length then
                           Send
                           (  Session,
                              This (Count..Length - 1),
                              Count
                           );
                           Data.Count := Stream_Element_Offset (Count);
                           exit when Count < Length;
                        end if;
                     end;
                  end if;
               when Sending_String =>
                  if Data.Data /= null then
                     declare
                        This  : String_Data renames Data.Data.all;
                        Count : Integer := Integer (Data.Count);
                     begin
                        if Count <= This.Length then
                           Send
                           (  Session,
                              This.Text (Count..This.Length),
                              Count
                           );
                           Data.Count := Stream_Element_Offset (Count);
                           exit when Count <= This.Length;
                        end if;
                     end;
                  end if;
               when Sending_Chain =>
                  Data.Chain (Session);
               when Sending_Content =>
                  Send_Content (Data.Content_Source, Data.Count);
               when Sending_Chunked_Content =>
                  Send_Content (Data.Chunked_Content_Source);
               when Sending_Stream =>
                  Send
                  (  Client        => Session,
                     Stream        => Data.Stream_Source.all,
                     Count         => Data.Count,
                     End_Of_Stream => Completed
                  );
                  exit when not Completed;
               when Sending_Chunked_Stream =>
                  Send
                  (  Client        => Session,
                     Stream        => Data.Chunked_Stream_Source.all,
                     Reserve       => 18,
                     Get_Prefix    => Get_Prefix'Access,
                     Get_Suffix    => Get_Suffix'Access,
                     End_Of_Stream => Completed
                  );
                  exit when not Completed;
               when Sending_Clean_And_Jump =>
                  declare
                     procedure Free is
                        new Ada.Unchecked_Deallocation
                            (  String,
                               HTTP_Server.String_Ptr
                            );
                     Ptr : HTTP_Server.String_Ptr := Data.Stub;
                  begin
                     Free (Ptr);
                     Index := Index + Integer (Data.Count);
                  end;
            end case;
         end;
         Index := Index + 1;
      end loop;
   end Sent;

   procedure Set (Data : in out String_Data_Ptr; Text : String) is
   begin
      if Data = null then
         if Text'Length > 0 then
            Data := new String_Data'(Text'Length, Text'Length, Text);
         end if;
      elsif Data.Size < Text'Length then
         Free (Data);
         Data := new String_Data'(Text'Length, Text'Length, Text);
      else
         Data.Length := Text'Length;
         Data.Text (1..Text'Length) := Text;
      end if;
   end Set;

   procedure Set_Keep_Alive
             (  Session : in out HTTP_Session;
                Enable  : Boolean
             )  is
   begin
      Session.Keep_Alive := Enable;
   end Set_Keep_Alive;

   procedure Set_Request_Date
             (  Session : in out HTTP_Session;
                Enable  : Boolean
             )  is
   begin
      Session.Send_Date := Enable;
   end Set_Request_Date;

   procedure Set_Request_Header
             (  Session : in out HTTP_Session;
                Header  : HTTP_Server.Text_Header;
                Value   : String := ""
             )  is
      Ptr : String_Data_Ptr renames Session.Request_Headers (Header);
   begin
      Set (Ptr, Value);
   end Set_Request_Header;

   procedure Set_Request_If_Modified_Since
             (  Session : in out HTTP_Session;
                Date    : Time
             )  is
   begin
      Set
      (  Session.If_Modified,
         "If-Modified-Since: " & To_HTTP (Date) & CRLF
      );
   end Set_Request_If_Modified_Since;

   procedure Set_Request_If_Modified_Since
             (  Session : in out HTTP_Session
             )  is
   begin
      Set (Session.If_Modified, "");
   end Set_Request_If_Modified_Since;

   procedure Set_Request_If_Unmodified_Since
             (  Session : in out HTTP_Session;
                Date    : Time
             )  is
   begin
      Set
      (  Session.If_Unmodified,
         "If-Unmodified-Since: " & To_HTTP (Date) & CRLF
      );
   end Set_Request_If_Unmodified_Since;

   procedure Set_Request_If_Unmodified_Since
             (  Session : in out HTTP_Session
             )  is
   begin
      Set (Session.If_Unmodified, "");
   end Set_Request_If_Unmodified_Since;

   procedure Set_Request_Range
             (  Session : in out HTTP_Session;
                From    : Stream_Element_Count := 1;
                To      : Stream_Element_Count := 0
             )  is
   begin
      Session.Range_From := From;
      Session.Range_To   := To;
   end Set_Request_Range;

   procedure Skip_Blanks (Source : String; Pointer : in out Integer) is
   begin
      while Pointer <= Source'Last loop
         case Source (Pointer) is
            when ' ' | Character'Val (9) =>
               Pointer := Pointer + 1;
            when others =>
               exit;
         end case;
      end loop;
   end Skip_Blanks;

   procedure Trace
             (  Session : in out HTTP_Session;
                Message : String
             )  is
   begin
      Trace
      (  Session.Listener.Factory.all,
         Image (Get_Client_Address (Session)) & ' ' & Message
      );
   end Trace;

   function Value (Text : String) return Header_Value is
      Index : constant Integer := Locate (Response_Headers, Text);
   begin
      if Index > 0 then
         return
         (  None   => False,
            Header => GetTag (Response_Headers, Index)
         );
      else
         return (None => True);
      end if;
   end Value;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Pool
             )  is
   begin
      null;
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Send_Data_List
             )  is
   begin
      null;
   end Write;

begin
   Add (Response_Headers, "Access-Control-Allow-Origin",
                                   Access_Control_Allow_Origin_Header);
   Add (Response_Headers, "Accept-Patch",     Accept_Patch_Header);
   Add (Response_Headers, "Accept-Ranges",    Accept_Ranges_Header);
   Add (Response_Headers, "Age",              Age_Header);
   Add (Response_Headers, "Allow",            Allow_Header);
   Add (Response_Headers, "Cache-Control",    Cache_Control_Header);
   Add (Response_Headers, "Connection",       Connection_Header);
   Add (Response_Headers, "Content-Disposition",
                                   Content_Disposition_Header);
   Add (Response_Headers, "Content-Encoding", Content_Encoding_Header);
   Add (Response_Headers, "Content-Language", Content_Language_Header);
   Add (Response_Headers, "Content-Length",   Content_Length_Header);
   Add (Response_Headers, "Content-Location", Content_Location_Header);
   Add (Response_Headers, "Content-MD5",      Content_MD5_Header);
   Add (Response_Headers, "Content-Range",    Content_Range_Header);
   Add (Response_Headers, "Content-Type",     Content_Type_Header);
   Add (Response_Headers, "Date",             Date_Header);
   Add (Response_Headers, "ETag",             ETag_Header);
   Add (Response_Headers, "Expires",          Expires_Header);
   Add (Response_Headers, "Last-Modified",    Last_Modified_Header);
   Add (Response_Headers, "Link",             Link_Header);
   Add (Response_Headers, "Location",         Location_Header);
   Add (Response_Headers, "P3P",              P3P_Header);
   Add (Response_Headers, "Pragma",           Pragma_Header);
   Add (Response_Headers, "Proxy-Authenticate",
                                    Proxy_Authenticate_Header);
   Add (Response_Headers, "Public-Key-Pins",  Public_Key_Pins_Header);
   Add (Response_Headers, "Refresh",          Refresh_Header);
   Add (Response_Headers, "Retry-After",      Retry_After_Header);
   Add (Response_Headers, "Permanent",        Permanent_Header);
   Add (Response_Headers, "Server",           Server_Header);
   Add (Response_Headers, "Set-Cookie",       Set_Cookie_Header);
   Add (Response_Headers, "Status",           Status_Header);
   Add (Response_Headers, "Strict-Transport-Security",
                                    Strict_Transport_Security_Header);
   Add (Response_Headers, "Trailer",          Trailer_Header);
   Add (Response_Headers, "Transfer-Encoding",
                                    Transfer_Encoding_Header);
   Add (Response_Headers, "Upgrade",          Upgrade_Header);
   Add (Response_Headers, "Vary",             Vary_Header);
   Add (Response_Headers, "Via",              Via_Header);
   Add (Response_Headers, "Warning",          Warning_Header);
   Add (Response_Headers, "WWW-Authenticate", WWW_Authenticate_Header);
   Add (Response_Headers, "X-Frame-Options",  X_Frame_Options_Header);

end GNAT.Sockets.Connection_State_Machine.HTTP_Client;
