--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     HTTP_Server                                 Winter, 2013       --
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
with Ada.Tags;                 use Ada.Tags;
with GNAT.SHA1;                use GNAT.SHA1;
with Interfaces;               use Interfaces;
with Strings_Edit;             use Strings_Edit;
with Strings_Edit.Base64;      use Strings_Edit.Base64;
with Strings_Edit.Floats;      use Strings_Edit.Floats;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.Sockets.Connection_State_Machine.Big_Endian.Unsigneds;
with Strings_Edit.UTF8;

package body GNAT.Sockets.Connection_State_Machine.HTTP_Server is
   use CGI_Keys;
   use Content_Ranges;
   use GNAT.Sockets.Server.Stream_Element_Offset_Edit;

   Block_Size           : constant := 1024;
   Default_Response     : constant String := "Not implemented";
   Bad_Request_Response : constant String := "Bad request";

   function From_Digest (Data : Message_Digest) return String is
      Result  : String (1..Data'Length / 2);
      Pointer : Integer := Data'First;
   begin
      for Index in Result'Range loop
         Result (Index) :=
            Character'Val
            (  Strings_Edit.Integers.Value
               (  Source => Data (Pointer..Pointer + 1),
                  Base   => 16
            )  );
         Pointer := Pointer + 2;
      end loop;
      return Result;
   end From_Digest;

   function To_String (Data : Stream_Element_Array) return String is
      Result : String (1..Data'Length);
   begin
      for Index in Data'Range loop
         Result (Integer (Index - Data'First + 1)) :=
            Character'Val (Data (Index));
      end loop;
      return Result;
   end To_String;

   function Accumulated_Body_Length (Client : HTTP_Client)
      return Stream_Element_Count is
   begin
      if Client.Body_Content.First = null then
         return 0;
      else
         return Client.Body_Content.Length;
      end if;
   end Accumulated_Body_Length;

   procedure Accumulate_Body
             (  Client  : in out HTTP_Client;
                Content : String
             )  is
   begin
      Accumulate_Body (Client, From_String (Content));
   end Accumulate_Body;

   procedure Accumulate_Body
             (  Client  : in out HTTP_Client'Class;
                Content : Content_Item_Ptr
             )  is
   begin
      if Client.Body_Content.First = null then
         Client.Body_Content.First  := Content;
         Client.Body_Content.Length := Content.Length;
      else
         if Client.Body_Content.Last /= null then
            Client.Body_Content.Last.Next := Content;
         end if;
         Client.Body_Content.Length :=
            Client.Body_Content.Length + Content.Length;
      end if;
      Client.Body_Content.Last := Content;
   end Accumulate_Body;

   procedure Accumulate_Body
             (  Client  : in out HTTP_Client;
                Content : access Stream_Element_Array
             )  is
      type Content_Item_Ptr is access Content_Item;
      for Content_Item_Ptr'Storage_Pool use Client.Pool;
      Ptr : Content_Item_Ptr;
   begin
      Ptr := new Content_Item'
                 (  Kind     => Stream_Elements_Pointer,
                    Length   => Content'Length,
                    Next     => null,
                    First    => Content'First,
                    Data_Ptr => Content.all'Unchecked_Access
                 );
      Accumulate_Body (Client, Ptr.all'Unchecked_Access);
   end Accumulate_Body;

   procedure Accumulate_Body
             (  Client  : in out HTTP_Client;
                Content : access String
             )  is
      type Content_Item_Ptr is access Content_Item;
      for Content_Item_Ptr'Storage_Pool use Client.Pool;
      Ptr : Content_Item_Ptr;
   begin
      Ptr := new Content_Item'
                 (  Kind     => String_Pointer,
                    Length   => Content'Length,
                    Next     => null,
                    First    => Stream_Element_Offset (Content'First),
                    Text_Ptr => Content.all'Unchecked_Access
                 );
      Accumulate_Body (Client, Ptr.all'Unchecked_Access);
   end Accumulate_Body;

   procedure Accumulate_Body
             (  Client  : in out HTTP_Client;
                Content : Stream_Element_Array
             )  is
      type Content_Item_Ptr is access Content_Item;
      for Content_Item_Ptr'Storage_Pool use Client.Pool;
      Ptr : Content_Item_Ptr;
   begin
      if Content'Length > 0 then
         Ptr := new Content_Item'
                    (  Kind   => Literal_Value,
                       Length => Content'Length,
                       Next   => null,
                       First  => 1,
                       Data   => Content
                    );
         Accumulate_Body (Client, Ptr.all'Unchecked_Access);
      end if;
   end Accumulate_Body;

   procedure Body_Error
             (  Client : in out HTTP_Client;
                Stream : in out Root_Stream_Type'Class;
                Error  : Exception_Occurrence
             )  is
   begin
      Raise_Exception (Data_Error'Identity, Exception_Message (Error));
   end Body_Error;

   procedure Body_Error
             (  Client  : in out HTTP_Client;
                Content : in out Content_Destination'Class;
                Error   : Exception_Occurrence
             )  is
   begin
      Raise_Exception (Data_Error'Identity, Exception_Message (Error));
   end Body_Error;

   procedure Body_Received
             (  Client : in out HTTP_Client;
                Stream : in out Root_Stream_Type'Class
             )  is
   begin
      null;
   end Body_Received;

   procedure Body_Received
             (  Client  : in out HTTP_Client;
                Content : in out Content_Destination'Class
             )  is
   begin
      null;
   end Body_Received;

   procedure Body_Sent
             (  Client : in out HTTP_Client;
                Stream : in out Root_Stream_Type'Class;
                Get    : Boolean
             )  is
   begin
      null;
   end Body_Sent;

   procedure Body_Error
             (  Client  : in out HTTP_Client;
                Content : in out CGI_Keys.Table'Class;
                Error   : Exception_Occurrence
             )  is
   begin
      Raise_Exception (Data_Error'Identity, Exception_Message (Error));
   end Body_Error;

   procedure Body_Received
             (  Client  : in out HTTP_Client;
                Content : in out CGI_Keys.Table'Class
             )  is
   begin
      null;
   end Body_Received;

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

   function Check_WebSocket
            (  Client : access HTTP_Client
            )  return Boolean is
      This : HTTP_Client renames Client.all;
   begin
      if 0 = (This.Connection and Connection_Upgrade) then
         Reply_Text
         (  Client.all,
            400,
            Bad_Request_Response,
            "Connection field is not 'upgrade'"
         );
         return False;
      end if;
      begin
         case Integer'
              (  Value (Get_Header (This, Sec_WebSocket_Version_Header))
              )  is
            when 13 =>
               null;
            when others =>
               Send_Status_Line (This, 426, "Upgrade required");
               Send_Date (This);
               Send_Content_Type (This, "text/plain");
               Send (This, "Sec-WebSocket-Version: 13" & CRLF);
               Send_Connection (This, False);
               Send_Body (This, "Unsupported WebSocket version");
               return False;
         end case;
      exception
         when Error : others =>
            Reply_Text
            (  This,
               400,
               Bad_Request_Response,
               "WebSocket version error: " & Exception_Message (Error)
            );
            return False;
      end;
      case Get_Header (This, Sec_WebSocket_Key_Header)'Length is
         when 16 =>
            null;
         when 0 =>
            Reply_Text
            (  This,
               400,
               Bad_Request_Response,
               "Missing WebSocket key"
            );
            return False;
         when others =>
            Reply_Text
            (  This,
               400,
               Bad_Request_Response,
               "Broken WebSocket key, must be 16-bytes long"
            );
            return False;
      end case;
--        declare -- Origin checks
--           Host    : String := Get_Header (This, Host_Header);
--           Checked : Boolean := False;
--        begin
--           if Host /= "localhost" then
--              declare
--                 Server : Host_Entry_Type := Get_Host_By_Name (Host_Name);
--                 Origin : Host_Entry_Type := Get_Host_By_Name (Host);
--              begin
--        Verify : for I in 1..Addresses_Length (Origin) loop
--                    declare
--                       Address : Inet_Addr_Type := Addresses (Origin, I);
--                    begin
--                       for J in 1..Addresses_Length (Server) loop
--                          if Addresses (Server, J) = Address then
--                             Checked := True;
--                             exit Verify;
--                          end if;
--                       end loop;
--                    end;
--                 end loop Verify;
--                 if not Checked then
--                    Reply_Text
--                    (  This,
--                       400,
--                       Bad_Request_Response,
--                       "Host does not contain server's name"
--                    );
--                    return False;
--                 end if;
--              end;
--           end if;
--        exception
--           when others =>
--              if not Checked then
--                 Reply_Text
--                 (  This,
--                    400,
--                    Bad_Request_Response,
--                    "Host does not contain server's name"
--                 );
--                 return False;
--              end if;
--        end;
      return True;
   end Check_WebSocket;

   procedure Cleanup_Body_Part (Client : in out HTTP_Client'Class) is
   begin
      if Client.Part_Mark /= null then
         declare
            type Part_Ptr is access String;
            for Part_Ptr'Storage_Pool use Client.Pool;
            function To_Part_Ptr is
               new Ada.Unchecked_Conversion (Text_Ptr, Part_Ptr);
            procedure Free is
               new Ada.Unchecked_Deallocation (String, Part_Ptr);
            Ptr : Part_Ptr := To_Part_Ptr (Client.Part_Mark);
         begin
            Free (Ptr);
            Client.Multipart := (others => null);
            Client.Part_Mark := null;
         end;
      end if;
   end Cleanup_Body_Part;

   procedure Commit (Destination : in out Content_Destination) is
   begin
      null;
   end Commit;

   procedure Commit (Destination : in out CGI_Content) is
      Client : HTTP_Client'Class renames Destination.Client.all;
      type Value_Ptr is access String;
      for Value_Ptr'Storage_Pool use Client.Pool;
      Ptr : Value_Ptr;
   begin
      case Destination.State is
         when CGI_Value =>
            if Destination.Offset /= 0 then
               Ptr :=
                  new String'
                      (  From_Escaped
                         (  Client.Line.Value
                            (  Client.Line.Value'First
                            .. Client.Line.Last
                            ),
                            True
                      )  );
               Replace
               (  Destination.Keys.all,
                  Destination.Offset,
                  Ptr.all'Unchecked_Access
               );
               if Destination.Client.Trace_Body then
                  Trace
                  (  Client,
                     (  "CGI "
                     &  Quote
                        (  GetName
                           (  Destination.Keys.all,
                              Destination.Offset
                           ),
                           '''
                        )
                     &  '='
                     &  Quote
                        (  GetTag
                           (  Destination.Keys.all,
                              Destination.Offset
                           ) .all,
                           '''
                  )  )  );
               end if;
            end if;
         when CGI_Key =>
            null;
      end case;
   end Commit;

   function Compare (Left, Right : String) return Boolean is
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      for Index in Left'Range loop
         if (  To_Lower (Left (Index))
            /= Right (Index + (Right'First - Left'First))
            )
         then
            return False;
         end if;
      end loop;
      return True;
   end Compare;

   procedure Continue (Client : in out HTTP_Client; Chain : Action) is
   begin
      Client.Chain := Chain;
   end Continue;

   procedure Content_Chunk (Client : in out HTTP_Client'Class) is
   begin
      if Client.Source /= null then
         declare
            Chunk : constant String := Get (Client.Source);
         begin
            if Client.Chunked then -- Chunked transfer
               Send (Client, Integers.Image (Chunk'Length, 16) & CRLF);
               if Chunk'Length = 0 then
                  Client.Source := null;
                  Send (Client, CRLF);
               else
                  Send (Client, Chunk);
                  Send (Client, CRLF);
                  Continue (Client, Content_Chunk'Access);
               end if;
            else -- Fixed length transfer
               if Chunk'Length = 0 then -- End of data
                  Client.Source := null;
                  if Client.Data_Length > 0 then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Missing "
                        &  Image (Client.Data_Length)
                        &  " contents data to transfer"
                     )  );
                  end if;
                  Send (Client, CRLF);
               else -- A chunk of data obtained
                  if Chunk'Length < Client.Data_Length then
                     Send (Client, Chunk);
                     Client.Data_Length :=
                        Client.Data_Length - Chunk'Length;
                     Continue (Client, Content_Chunk'Access);
                  else
                     Client.Source := null;
                     Send
                     (  Client,
                        Chunk
                        (  Chunk'First
                        .. (  Chunk'First
                           +  Natural (Client.Data_Length)
                           -  1
                     )  )  );
                     Client.Data_Length := 0;
                     Send (Client, CRLF);
                  end if;
               end if;
            end if;
         end;
      else
         if Client.Chunked then -- Chunked transfer completed
            Send (Client, "0" & CRLF & CRLF);
         else -- Fixed length transfer completed
            Send (Client, CRLF);
         end if;
      end if;
   exception
      when Content_Not_Ready =>
         Continue (Client, Content_Chunk'Access);
      when Error : others =>
          Trace_Error
          (  Client.Listener.Factory.all,
             "Sending content chunk",
             Error
          );
          Client.Source := null;
          Client.Data_Length := 0;
          if Client.Chunked then -- Chunked transfer completed
             Send (Client, "0" & CRLF & CRLF);
          else -- Fixed length transfer completed
             Send (Client, CRLF);
          end if;
   end Content_Chunk;

   procedure Do_Body (Client : in out HTTP_Client) is
   begin
      null;
   end Do_Body;

   procedure Do_Connect (Client : in out HTTP_Client) is
   begin
      Reply_Text (Client, 501, Default_Response, Default_Response);
   end Do_Connect;

   procedure Do_Delete (Client : in out HTTP_Client) is
   begin
      Reply_Text (Client, 501, Default_Response, Default_Response);
   end Do_Delete;

   procedure Do_Head (Client : in out HTTP_Client) is
   begin
      Send_Status_Line (Client, 200, "OK");
      Send_Date   (Client, Clock);
      Send_Server (Client);
      Send_Length (Client, Natural'(Default_Response'Length));
      Send_Connection (Client, False);
      Send_Content_Type (Client);
   end Do_Head;

   procedure Do_Get (Client : in out HTTP_Client) is
   begin
      Send_Status_Line (Client, 200, "OK");
      Send_Date   (Client, Clock);
      Send_Server (Client);
      Send_Length (Client, Natural'(Default_Response'Length));
      Send_Connection (Client, False);
      Send_Content_Type (Client);
      Send (Client, CRLF);
      Send (Client, Default_Response);
   end Do_Get;

   procedure Do_Options (Client : in out HTTP_Client) is
      Response : constant String := "";
   begin
      Send_Status_Line (Client, 200, "OK");
      Send_Date   (Client, Clock);
      Send_Server (Client);
      Send_Length (Client, Natural'(Response'Length));
      Send_Connection (Client, False);
      Send_Allow (Client, Client.Allowed);
      Send (Client, CRLF);
      Send (Client, Response);
   end Do_Options;

   procedure Do_Patch (Client : in out HTTP_Client) is
   begin
      Reply_Text (Client, 501, Default_Response, Default_Response);
   end Do_Patch;

   procedure Do_Post (Client : in out HTTP_Client) is
   begin
      Reply_Text (Client, 501, Default_Response, Default_Response);
   end Do_Post;

   procedure Do_Put (Client : in out HTTP_Client) is
   begin
      Reply_Text (Client, 501, Default_Response, Default_Response);
   end Do_Put;

   procedure Do_Trace (Client : in out HTTP_Client) is
   begin
      Reply_Text (Client, 501, Default_Response, Default_Response);
   end Do_Trace;

   procedure Do_WebSocket (Client : in out HTTP_Client) is
   begin
      if not Check_WebSocket (Client'Access) then
         return;
      end if;
      declare
         Result : constant WebSocket_Accept :=
                  WebSocket_Open (HTTP_Client'Class (Client)'Access);
         Socket : WebSocket_Data renames Client.WebSocket;
      begin
         if Result.Accepted then
            declare
               type Message_Ptr is access WebSocket_Message;
               for Message_Ptr'Storage_Pool use Client.Pool;
               Ptr : constant Message_Ptr :=
                     new WebSocket_Message (Result.Size);
            begin
               Socket.Data := Ptr.all'Unchecked_Access;
            end;
            Socket.Max_Length := Result.Size;
            Socket.Chunked    := Result.Chunked;
            Client.Expecting  := WebSocket_Header;
            if Result.Duplex then -- Allow full-duplex operation
               Set_Overlapped_Size (Client, Client.Output_Size);
            end if;
            Socket.Duplex := Result.Duplex;
            Socket.State := Open_Socket;
            if Client.Trace_Body then
               Trace (Client, "WebSocket connection accepted");
            end if;
            --
            -- Responding with connection acknowledge
            --
            Send_Status_Line (Client, 101, "Switching Protocols");
            Send (Client, "Upgrade: websocket" & CRLF);
            Send (Client, "Connection: Upgrade" & CRLF);
            Send
            (  Client,
               (  "Sec-WebSocket-Accept: "
               &  To_Base64
                  (  From_Digest
                     (  Digest
                        (  To_Base64
                           (  Get_Header
                              (  Client,
                                 Sec_WebSocket_Key_Header
                           )  )
                        &  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
                  )  )  )
               &  CRLF
            )  );
            if Result.Protocols = "" then
               if Client.Headers (Sec_WebSocket_Protocol_Header) /= null
               then
                  Send -- Copy protocols
                  (  Client,
                     (  "Sec-WebSocket-Protocol: "
                     &  Get_Header
                        (  Client,
                           Sec_WebSocket_Protocol_Header
                        )
                     &  CRLF
                  )  );
               end if;
            else
               Send
               (  Client,
                  "Sec-WebSocket-Protocol: " & Result.Protocols & CRLF
               );
            end if;
            Send (Client, CRLF);
            if Result.Duplex then -- Allow full-duplex operation
               if Queued_To_Send (Client) > 0 then
                  Client.Mutex.Set (Server_Sending);
               else
                  Client.Mutex.Set (Idle);
               end if;
            end if;
            begin
               WebSocket_Initialize (HTTP_Client'Class (Client));
            exception
               when Error : others =>
                  Trace
                  (  Client,
                     (  "WebSocket initialization callback fault: "
                     &  Exception_Information (Error)
                  )  );
                  raise;
            end;
         else
            Reply_Text
            (  Client,
               Result.Code,
               Result.Reason,
               Result.Reason
            );
         end if;
      end;
   exception
      when Error : others =>
         WebSocket_Cleanup (Client);
         raise;
   end Do_WebSocket;

   procedure Finalize (Client : in out HTTP_Client) is
   begin
      WebSocket_Cleanup (Client);
      Finalize (State_Machine (Client));
   end Finalize;

   function From_Escaped
            (  Name           : String;
               Translate_Plus : Boolean := False
            )  return String is
      Length  : Natural := 0;
      Pointer : Integer := Name'First;
   begin
      while Pointer <= Name'Last loop
         if Name (Pointer) = '%' then
            Pointer := Pointer + 3;
         else
            Pointer := Pointer + 1;
         end if;
         Length := Length + 1;
      end loop;
      declare
         Pointer : Integer := Name'First;
         Result  : String (1..Length);
      begin
         for Index in Result'Range loop
            if Name (Pointer) = '%' then
               Result (Index) :=
                  Character'Val
                  (  Integer'
                     (  Value
                        (  Name (Pointer + 1..Pointer + 2),
                           16
                  )  )  );
               Pointer := Pointer + 3;
            elsif Name (Pointer) = '+' and then Translate_Plus then
               Result (Index) := ' ';
               Pointer := Pointer + 1;
            else
               Result (Index) := Name (Pointer);
               Pointer := Pointer + 1;
            end if;
         end loop;
         return Result;
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Illegal escape sequence"
            );
      end;
   end From_Escaped;

   function Get_Allowed (Client : HTTP_Client) return HTTP_Allowed is
   begin
      return Client.Allowed;
   end Get_Allowed;

   function Get_CGI_Key (Client : HTTP_Client; Index : Positive)
      return String is
   begin
      if (  Client.CGI.Keys /= null
         and then
            Index <= GetSize (Client.CGI.Keys.all)
         )
      then
         return GetName (Client.CGI.Keys.all, Index);
      end if;
      Raise_Exception (Constraint_Error'Identity, "Invalid key index");
   end Get_CGI_Key;

   function Get_CGI_Size (Client : HTTP_Client) return Natural is
   begin
      if Client.CGI.Keys = null then
         return 0;
      else
         return GetSize (Client.CGI.Keys.all);
      end if;
   end Get_CGI_Size;

   function Get_CGI_Value (Client : HTTP_Client; Index : Positive)
      return String is
   begin
      if (  Client.CGI.Keys /= null
         and then
            Index <= GetSize (Client.CGI.Keys.all)
         )
      then
         declare
            Value : constant String_Ptr :=
                    GetTag (Client.CGI.Keys.all, Index);
         begin
            if Value = null then
               return "";
            else
               return Value.all;
            end if;
         end;
      end if;
      Raise_Exception (Constraint_Error'Identity, "Invalid key index");
   end Get_CGI_Value;

   function Get_CGI_Value (Client : HTTP_Client; Key : String)
      return String is
   begin
      if Client.CGI.Keys /= null then
         declare
            Offset : constant Natural :=
                     Locate (Client.CGI.Keys.all, Key);
            Value  : String_Ptr;
         begin
            if Offset > 0 then
               Value := GetTag (Client.CGI.Keys.all, Offset);
               if Value /= null then
                  return Value.all;
               end if;
            end if;
         end;
      end if;
      return "";
   end Get_CGI_Value;

   function Get_Closing (Client : HTTP_Client) return Boolean is
   begin
      return 0 /= (Client.Connection and Connection_Close);
   end Get_Closing;

   function Get_Date (Client : HTTP_Client) return Time is
   begin
      if Client.Specific (Date_Header) then
         return Client.Date;
      else
         raise Time_Error;
      end if;
   end Get_Date;

   function Get_Header
            (  Client : HTTP_Client;
               Header : Text_Header
            )  return String is
   begin
      if Client.Headers (Header) = null then
         return "";
      else
         return Client.Headers (Header).all;
      end if;
   end Get_Header;

   function Get_If_Modified_Since (Client : HTTP_Client) return Time is
   begin
      if Client.Specific (If_Modified_Since_Header) then
         return Client.If_Modified_Since;
      else
         raise Time_Error;
      end if;
   end Get_If_Modified_Since;

   function Get_If_Unmodified_Since (Client : HTTP_Client) return Time is
   begin
      if Client.Specific (If_Unmodified_Since_Header) then
         return Client.If_Unmodified_Since;
      else
         raise Time_Error;
      end if;
   end Get_If_Unmodified_Since;

   function Get_Last_Modified (Client : HTTP_Client) return Time is
   begin
      if Client.Specific (Last_Modified_Header) then
         return Client.Last_Modified;
      else
         raise Time_Error;
      end if;
   end Get_Last_Modified;

   function Get_Method (Client : HTTP_Client) return HTTP_Method is
   begin
      return Client.Method;
   end Get_Method;

   function Get_Multipart_Header
            (  Client : HTTP_Client;
               Header : Multipart_Header
            )  return String is
   begin
      if Client.Multipart (Header) = null then
         return "";
      else
         return Client.Multipart (Header).all;
      end if;
   end Get_Multipart_Header;

   function Get_Name (Client : HTTP_Client) return String is
   begin
      return "Simple_Components_HTTP_Server/1.0";
   end Get_Name;

   function Get_Prefix
            (  Client : access Connection'Class;
               Data   : Stream_Element_Array;
               End_Of_Stream : Boolean
            )  return String is
   begin
      return Image (Stream_Element_Count'(Data'Length), 16) & CRLF;
   end Get_Prefix;

   function Get_Ranges (Client : HTTP_Client) return Ranges_Set is
   begin
      if Client.Suffix = Stream_Element_Offset'First then -- No suffix
         return (Explicit_Range, Client.Ranges);
      else
         return (Suffix_Range, Client.Ranges, Client.Suffix);
      end if;
   end Get_Ranges;

   function Get_Status_Line (Client : HTTP_Client) return Status_Line is
   begin
      if Client.Status = null then
         return (None, 0, 0, 0);
      else
         return Client.Status.all;
      end if;
   end Get_Status_Line;

   function Get_Suffix
            (  Client : access Connection'Class;
               Data   : Stream_Element_Array;
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

   function Get_Version (Client : HTTP_Client) return HTTP_Version is
   begin
      return Client.Version;
   end Get_Version;

   procedure Header_Received
             (  Client : in out HTTP_Client;
                Header : Request_Header;
                Value  : String
             )  is
      procedure Get_Text is
         pragma Inline (Get_Text);
         type Header_Ptr is access String;
         for Header_Ptr'Storage_Pool use Client.Pool;
         Ptr : Header_Ptr;
      begin
         if Client.Headers (Header) /= null then
            return; -- The value is already set
         end if;
         if Header = Sec_WebSocket_Key_Header then
            declare
               Key : constant String := From_Base64 (Value);
            begin
               Ptr := new String'(Key);
               Client.Headers (Header) := Ptr.all'Unchecked_Access;
            end;
         else
            Ptr := new String'(Value);
            Client.Headers (Header) := Ptr.all'Unchecked_Access;
            if Header = Content_Type_Header then
               if Is_Prefix ("multipart", Value, Lower) then
                   declare
                      type Boundary_Ptr is access String;
                      for Boundary_Ptr'Storage_Pool use Client.Pool;
                      Pointer  : aliased Integer := Value'First + 9;
                      Boundary : Boundary_Ptr;
                   begin
                      while Pointer <= Value'Last loop
                         if Is_Prefix
                            (  "boundary=",
                               Value,
                               Pointer,
                               Lower
                            )
                         then
                            Boundary :=
                               new String'
                                   (  CRLF
                                   &  "--"
                                   &  Value (Pointer + 9.. Value'Last)
                                   );
                            Client.Boundary :=
                               Boundary.all'Unchecked_Access;
                            Client.Position := Client.Boundary'First;
                            exit;
                         end if;
                         Pointer := Pointer + 1;
                      end loop;
                   end;
                   if Client.Boundary = null then
                      Raise_Exception
                      (  Data_Error'Identity,
                         "Missing multipart boundary specification"
                      );
                   end if;
               end if;
            end if;
         end if;
      end Get_Text;
   begin
      case Header is
         when Text_Header =>
            Get_Text;
         when Date_Header =>
            Client.Date := To_Time (Value);
            Client.Specific (Date_Header) := True;
         when Content_Length_Header =>
            Client.Data_Length :=
               Stream_Element_Offset_Edit.Value (Value);
         when Last_Modified_Header =>
            Client.If_Modified_Since := To_Time (Value);
            Client.Specific (Last_Modified_Header) := True;
         when If_Modified_Since_Header =>
            Client.If_Modified_Since := To_Time (Value);
            Client.Specific (If_Modified_Since_Header) := True;
         when If_Unmodified_Since_Header =>
            Client.If_Unmodified_Since := To_Time (Value);
            Client.Specific (If_Unmodified_Since_Header) := True;
         when Connection_Header =>
            Client.Connection := To_Flags (Value);
         when Range_Header =>
            declare
               Pointer : Integer := Value'First;
               From    : Stream_Element_Offset;
               To      : Stream_Element_Offset;
            begin
               if not Is_Prefix ("bytes=", Value, Pointer, Lower) then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Illegal or missing range units specification"
                  );
               end if;
               Pointer := Pointer + 6;
               loop
                  begin
                     Get (Value, Pointer, From);
                  exception
                     when others =>
                        Raise_Exception
                        (  Data_Error'Identity,
                           "Missing or wrong lower range bound " &
                           "specification"
                        );
                  end;
                  if From < 0 then
                     exit when Pointer > Value'Last;
                     Raise_Exception
                     (  Data_Error'Identity,
                        "Suffix range is not the last on of " &
                        "the range specification"
                     );
                  end if;
                  if not Is_Prefix ("-", Value, Pointer) then
                     Raise_Exception
                     (  Data_Error'Identity,
                        "Missing minus in the range specification"
                     );
                  end if;
                  Pointer := Pointer + 1;
                  if Pointer <= Value'Last then
                     begin
                        Get (Value, Pointer, To);
                     exception
                        when others =>
                           Raise_Exception
                           (  Data_Error'Identity,
                              "Missing or wrong upper range bound " &
                              "specification"
                           );
                     end;
                     begin
                        Add (Client.Ranges, From, To);
                     exception
                        when others =>
                           Raise_Exception
                           (  Data_Error'Identity,
                              "Illegal range specification"
                           );
                     end;
                  else
                     Client.Suffix := From;
                     exit;
                  end if;
                  exit when Pointer > Value'Last;
                  if Value (Pointer) /= ',' then
                     Raise_Exception
                     (  Data_Error'Identity,
                        "Missing comma in the ranges specification"
                     );
                  end if;
                  Pointer := Pointer + 1;
               end loop;
            end;
      end case;
   end Header_Received;

   function Image (Header : Request_Header) return String is
      Result     : String  := Request_Header'Image (Header);
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

   procedure Initialize (Client : in out HTTP_Client) is
   begin
      Client.LF.Value (1) := 10;
      Client.Line.Terminator := Character'Val (13);
      Set_Maximum_Size (Client.Line, Client.Request_Length);
      Initialize (State_Machine (Client));
   end Initialize;

   function Is_Empty (Stream : Content_Stream) return Boolean is
   begin
      return Stream.First = null;
   end Is_Empty;

   procedure Message_Chunk (Client : in out HTTP_Client'Class) is
      Done : Boolean;
   begin
      Send
      (  Client        => Client,
         Stream        => Client.Body_Content,
         End_Of_Stream => Done
      );
      if Done then
         if Client.Stub /= null then
            declare
               type Stub_Ptr is access Content_Item;
               for Stub_Ptr'Storage_Pool use Client.Pool;
               function To_Ptr is
                  new Ada.Unchecked_Conversion
                      (  Content_Item_Ptr,
                         Stub_Ptr
                      );
               procedure Free is
                  new Ada.Unchecked_Deallocation
                      (  Content_Item,
                         Stub_Ptr
                      );
               Ptr : Stub_Ptr := To_Ptr (Client.Stub);
            begin
               Free (Ptr);
            end;
            Client.Stub := null;
         end if;
         if Client.WebSocket.State = Closing_Socket then
            WebSocket_Cleanup (Client);
         end if;
      else
         Continue (Client, Message_Chunk'Access);
      end if;
   end Message_Chunk;

   procedure Multipart_Header_Received
             (  Client : in out HTTP_Client;
                Header : Request_Header;
                Value  : String
             )  is
      type Header_Ptr is access String;
      for Header_Ptr'Storage_Pool use Client.Pool;
      Ptr : Header_Ptr;
   begin
      case Header is
         when Multipart_Header =>
            if Client.Multipart (Header) = null then
               Ptr := new String'(Value);
               Client.Multipart (Header) := Ptr.all'Unchecked_Access;
               if Client.Part_Mark = null then
                  Client.Part_Mark := Client.Multipart (Header);
               end if;
            end if;
         when others =>
            Trace
            (  Client,
               "Unsupported multipart header " & Quote (Value, ''')
            );
      end case;
   end Multipart_Header_Received;

   procedure Queue_Content
             (  Client : in out HTTP_Client;
                Data   : Stream_Element_Array
             )  is
      type Content_Item_Ptr is access Content_Item;
      for Content_Item_Ptr'Storage_Pool use Client.Pool;
      Ptr : Content_Item_Ptr;
   begin
      Ptr := new Content_Item'
                 (  Kind   => Literal_Value,
                    Length => Data'Length,
                    Next   => null,
                    First  => 1,
                    Data   => Data
                 );
      if Client.Stub = null then
         Client.Stub := Ptr.all'Unchecked_Access;
      end if;
      Accumulate_Body (Client, Ptr.all'Unchecked_Access);
      Continue (Client, Message_Chunk'Access);
   end Queue_Content;

   procedure Queue_Content
             (  Client : in out HTTP_Client;
                Data   : String
             )  is
      type Content_Item_Ptr is access Content_Item;
      for Content_Item_Ptr'Storage_Pool use Client.Pool;
      Ptr : Content_Item_Ptr;
   begin
      Ptr := new Content_Item
                 (  Kind   => Literal_Value,
                    Length => Data'Length
                 );
      declare
         Message : Stream_Element_Array renames Ptr.Data;
         Pointer : Stream_Element_Offset := Message'First;
      begin
         for Index in Data'Range loop
            Message (Pointer) := Character'Pos (Data (Index));
            Pointer := Pointer + 1;
         end loop;
      end;
      if Client.Stub = null then
         Client.Stub := Ptr.all'Unchecked_Access;
      end if;
      Accumulate_Body (Client, Ptr.all'Unchecked_Access);
      Continue (Client, Message_Chunk'Access);
   end Queue_Content;

   procedure Process_Body_Tail (Client : in out HTTP_Client'Class) is
      Request : String renames Client.Line.Value (1..Client.Line.Last);
      Pointer : constant Integer := Request'First;
   begin
      if Request = "--" then
         Client.Expecting := Multipart_Epilogue;
      elsif Pointer > Request'Last then
         Client.Expecting := Multipart_Header_Line;
      else
         Trace
         (  Client,
            "Malformed ending of a multipart body: " & Request
         );
      end if;
   end Process_Body_Tail;

   procedure Process_Chunk_Line (Client : in out HTTP_Client'Class) is
      Request : String renames Client.Line.Value (1..Client.Line.Last);
      Pointer : Integer := Request'First;
   begin
      Get
      (  Source  => Request,
         Pointer => Pointer,
         Value   => Client.Data_Length,
         Base    => 16,
         First   => 0
      );
      Client.Expecting := Client.Chunk_Type; -- Restore expected type
      if Client.Data_Length = 0 then
         Client.Chunked := False; -- End of chunked receive
         if Client.Stream /= null then
            Body_Received (Client, Client.Stream.all);
            Client.Stream := null;
         elsif Client.Destination /= null then
            Commit (Client.Destination.all);
            if Client.Destination = Client.CGI'Unchecked_Access then
               Body_Received (Client, Client.CGI.Keys.all);
            else
               Body_Received (Client, Client.Destination.all);
            end if;
            Client.Destination := null;
         end if;
      end if;
   exception
      when Data_Error | Constraint_Error | End_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            "Malformed data chunk header line"
         );
   end Process_Chunk_Line;

   procedure Process_Epilogue (Client : in out HTTP_Client'Class) is
   begin
      null;
   end Process_Epilogue;

   procedure Process_Header_Line (Client : in out HTTP_Client'Class) is
      Request : String renames Client.Line.Value (1..Client.Line.Last);
      Pointer : Integer := Request'First;
   begin
      if Pointer > Request'Last then
         if Client.Trace_Header then
            Trace (Client, "Header:");
            for Index in Client.Headers'Range loop
               if Client.Headers (Index) /= null then
                  Trace
                  (  Client,
                     (  "   "
                     &  Text_Header'Image (Index)
                     &  '='
                     &  Quote (Client.Headers (Index).all, ''')
                  )  );
               end if;
            end loop;
         end if;
         if Client.Boundary /= null then -- Multipart body
            Client.Expecting := Multipart_Preamble;
            Client.Multipart := (others => null);
            Client.Part_Mark := null;
         elsif Client.Chunked then -- Chunked transfer-encoded body
            Do_Body (Client);
            Client.Chunk_Type := Body_Data;
            Client.Expecting  := Chunk_Line;
         elsif Client.Data_Length > 0 then -- Fixed-length body
            Do_Body (Client);
            Client.Expecting := Body_Data;
         else
            Process_Request (Client);
         end if;
      else
         while Pointer <= Request'Last loop
            if Request (Pointer) = ':' then
               declare
                  Index : constant Integer :=
                          Locate
                          (  Request_Headers,
                             Request (Request'First..Pointer - 1)
                          );
               begin
                  if Index > 0 then
                     Pointer := Pointer + 1;
                     if Pointer <= Request'Last and then
                        Request (Pointer) = ' '
                     then
                        Pointer := Pointer + 1;
                     end if;
                     Header_Received
                     (  Client,
                        GetTag (Request_Headers, Index),
                        Request (Pointer..Request'Last)
                     );
                  else
                     Trace
                     (  Client,
                        (  "Unsupported header "
                        &  Quote
                           (  Request (Request'First..Pointer - 1),
                              '''
                     )  )  );
                  end if;
               end;
               return;
            end if;
            Pointer := Pointer + 1;
         end loop;
         Trace (Client, "Malformed header " & Request);
      end if;
   end Process_Header_Line;

   procedure Process_Part_Header (Client : in out HTTP_Client'Class) is
      Request : String renames Client.Line.Value (1..Client.Line.Last);
      Pointer : Integer := Request'First;
   begin
      if Pointer > Request'Last then
         if Client.Trace_Header then
            Trace (Client, "Multipart header:");
            for Index in Client.Multipart'Range loop
               if Client.Multipart (Index) /= null then
                  Trace
                  (  Client,
                     (  "   "
                     &  Multipart_Header'Image (Index)
                     &  '='
                     &  Quote (Client.Multipart (Index).all, ''')
                  )  );
               end if;
            end loop;
         end if;
         Do_Body (Client);
         Client.Expecting := Multipart_Body_Data;
      else
         while Pointer <= Request'Last loop
            if Request (Pointer) = ':' then
               declare
                  Index : constant Integer :=
                          Locate
                          (  Request_Headers,
                             Request (Request'First..Pointer - 1)
                          );
               begin
                  if Index > 0 then
                     Pointer := Pointer + 1;
                     if Pointer <= Request'Last and then
                        Request (Pointer) = ' '
                     then
                        Pointer := Pointer + 1;
                     end if;
                     Multipart_Header_Received
                     (  Client,
                        GetTag (Request_Headers, Index),
                        Request (Pointer..Request'Last)
                     );
                  else
                     Trace
                     (  Client,
                        (  "Unsupported header "
                        &  Quote
                           (  Request (Request'First..Pointer - 1),
                              '''
                     )  )  );
                  end if;
               end;
               return;
            end if;
            Pointer := Pointer + 1;
         end loop;
         Trace (Client, "Malformed multipart header " & Request);
      end if;
   end Process_Part_Header;

   procedure Process_Preamble (Client : in out HTTP_Client'Class) is
      Request  : String renames Client.Line.Value (1..Client.Line.Last);
      Boundary : String renames Client.Boundary.all;
   begin
      if Is_Prefix -- Matching boundary without forward CRLF
         (  Boundary (Boundary'First + 2..Boundary'Last),
            Request
         )
      then -- Boundary is here
         if Is_Prefix
            (  "--",
               Request,
               Request'First + Boundary'Length
            )
         then -- Last boundary
            Client.Expecting := Multipart_Epilogue;
         else
            Client.Expecting := Multipart_Header_Line;
         end if;
      end if;
   end Process_Preamble;

   procedure Process_Request (Client : in out HTTP_Client'Class) is
   begin
      Client.Expecting := Request_Line;
      case Client.Method is
         when HTTP_GET =>
            if Compare
               (  Get_Header (Client, Upgrade_Header),
                  "websocket"
               )
            then
               Do_WebSocket (Client);
            else -- This is a normal GET request
               Do_Get (Client);
            end if;
         when HTTP_HEAD =>
            Do_Head (Client);
         when HTTP_POST =>
            Do_Post (Client);
         when HTTP_PUT =>
            Do_Put (Client);
         when HTTP_DELETE =>
            Do_Delete (Client);
         when HTTP_TRACE =>
            Do_Trace (Client);
         when HTTP_OPTIONS =>
            Do_Options (Client);
         when HTTP_CONNECT =>
            Do_Connect (Client);
         when HTTP_PATCH =>
            Do_Patch (Client);
      end case;
   end Process_Request;

   procedure Process_Request_Line (Client : in out HTTP_Client'Class) is
      Request : String renames Client.Line.Value (1..Client.Line.Last);
      Pointer : Integer := Request'First;
      function Get_Version return HTTP_Version is
         Version : Float;
      begin
         if not Is_Prefix (" http/", Request, Pointer, Lower) then
            Trace (Client, "No HTTP version specified (1.1 assumed)");
            return 1.1;
         end if;
         Pointer := Pointer + 6;
         begin
            Get (Request, Pointer, Version);
         exception
            when others =>
               raise Constraint_Error;
         end;
         if Pointer <= Request'Last then
            Trace
            (  Client,
               "Unrecognized text follows HTTP version (1.1 assumed)"
            );
            return 1.1;
         else
            return HTTP_Version (Version);
         end if;
      exception
         when Constraint_Error =>
            Trace (Client, "Illegal HTTP version (1.1 assumed)");
            return 1.1;
      end Get_Version;
   begin
      Skip (Request, Pointer);
      begin
         Client.Method :=
            Find (Commands, Request (Request'First..Pointer - 1));
      exception
         when End_Error =>
            Raise_Exception
            (  Data_Error'Identity,
               (  "Method "
               &  Request (Request'First..Pointer - 1)
               &  " is not supported"
            )  );
      end;
      Pointer := Pointer + 1;
      if Pointer > Request'Last then
         Raise_Exception (Data_Error'Identity, "Missing request-URI");
      end if;
      Client.Expecting          := Header_Line;
      Client.Chunked            := False;
      Client.Connection         := 0;
      Client.Status             := null;
      Client.Stream             := null;
      Client.Source             := null;
      Client.Destination        := null;
      Client.Boundary           := null;
      Client.Data_Length        := 0;
      Client.Body_Content.First := null;
      Client.Headers            := (others => null);
      Client.Multipart          := (others => null);
      Client.Specific           := (others => False);
      Client.Suffix             := Stream_Element_Count'First;
      Client.CGI.Keys           := null;
      Erase (Client.Ranges);
      if Request (Pointer) = '*' then
         Pointer := Pointer + 1;
         Status_Line_Received (Client, Client.Method, Get_Version);
      elsif Request (Pointer) = '/' then
         declare
            Path : Integer;
         begin
            Pointer := Pointer + 1;
            Path    := Pointer;
            Skip (Request, Pointer);
            Status_Line_Received
            (  Client,
               Client.Method,
               From_Escaped (Request (Path..Pointer - 1)),
               Get_Version
            );
         end;
      else
         declare
            Host_First  : Integer := Request'First;
            Host_Next   : Integer := Request'First;
            Path_First  : Integer := Request'First;
            Path_Next   : Integer := Request'First;
            Query_First : Integer := Request'First;
            Query_Next  : Integer := Request'First;
            Port        : Integer := 80;
            Scheme      : Scheme_Type;
         begin
            begin
               Get (Request, Pointer, Schemes, Scheme);
            exception
               when others =>
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "URI does not start with a supported scheme "
                     &  "(e.g. http)"
                  )  );
            end;
            if not Is_Prefix ("://", Request, Pointer) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Scheme of the URI is not followed by ://"
               );
            end if;
            Pointer := Pointer + 3;
            Host_First := Pointer;
            Host_Next  := Pointer;
            while Pointer <= Request'Last loop
               case Request (Pointer) is
                  when ' ' | '/' =>
                     exit;
                  when ':' =>
                     Pointer := Pointer + 1;
                     begin
                        Get
                        (  Source  => Request,
                           Pointer => Pointer,
                           Value   => Port,
                           First   => 1,
                           Last    => 2**16-1
                        );
                     exception
                        when Constraint_Error =>
                           Raise_Exception
                           (  Data_Error'Identity,
                              "Port number is not in range"
                           );
                        when others =>
                           Raise_Exception
                           (  Data_Error'Identity,
                              "No port number after colon ':' in URI"
                           );
                     end;
                     exit;
                  when others =>
                     Pointer   := Pointer + 1;
                     Host_Next := Pointer;
               end case;
            end loop;
            if Host_First >= Host_Next then
               Raise_Exception
               (  Data_Error'Identity,
                  "Empty host address in URI"
               );
            end if;
            if Request (Pointer) = '/' then
               Pointer := Pointer + 1;
               Path_First := Pointer;
               Path_Next  := Pointer;
               while Pointer <= Request'Last loop
                  case Request (Pointer) is
                     when ' ' =>
                        exit;
                     when '?' =>
                        Pointer     := Pointer + 1;
                        Query_First := Pointer;
                        Skip (Request, Pointer);
                        Query_Next  := Pointer;
                        exit;
                     when others =>
                        Pointer   := Pointer + 1;
                        Path_Next := Pointer;
                  end case;
               end loop;
            end if;
            Status_Line_Received
            (  Client  => Client,
               Scheme  => Scheme,
               Method  => Client.Method,
               Port    => Port_Type (Port),
               Version => Get_Version,
               Host =>
                  From_Escaped (Request (Host_First..Host_Next - 1)),
               Path =>
                  From_Escaped (Request (Path_First..Path_Next - 1)),
               Query =>
                  From_Escaped (Request (Query_First..Query_Next - 1))
            );
         end;
      end if;
   end Process_Request_Line;

   procedure Put
             (  Destination : in out CGI_Content;
                Data        : String
             )  is
      type Value_Ptr is access String;
      for Value_Ptr'Storage_Pool use Destination.Client.Pool;
      Value : String  renames Destination.Client.Line.Value.all;
      Last  : Natural renames Destination.Client.Line.Last;
   begin
      for Index in Data'Range loop
         case Destination.State is
            when CGI_Value =>
               if Data (Index) = '&' then
                  if Destination.Offset /= 0 then
                     declare
                        Ptr : Value_Ptr;
                     begin
                        Ptr :=
                           new String'
                               (  From_Escaped
                                  (  Value (Value'First..Last),
                                     True
                               )  );
                        Last := 0;
                        Replace
                        (  Destination.Keys.all,
                           Destination.Offset,
                           Ptr.all'Unchecked_Access
                        );
                        if Destination.Client.Trace_Body then
                           Trace
                           (  Destination.Client.all,
                              (  "CGI "
                              &  Quote
                                 (  GetName
                                    (  Destination.Keys.all,
                                       Destination.Offset
                                    ),
                                    '''
                                 )
                              &  '='
                              &  Quote
                                 (  GetTag
                                    (  Destination.Keys.all,
                                       Destination.Offset
                                    ) .all,
                                    '''
                           )  )  );
                        end if;
                     end;
                  end if;
                  Destination.State := CGI_Key;
               else
                  if Last < Value'Last then
                     Last := Last + 1;
                     Value (Last) := Data (Index);
                  else
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "CGI value is longer than "
                        &  Image (Destination.Client.Request_Length)
                        &  " bytes"
                     )  );
                  end if;
               end if;
            when CGI_Key =>
               if Data (Index) = '=' then
                  begin
                     declare
                        Key : constant String :=
                                 From_Escaped
                                 (  Value (Value'First..Last),
                                    True
                                 );
                     begin
                        Destination.Offset :=
                           Locate (Destination.Keys.all, Key);
                        if Destination.Offset > 0 then
                           if (  GetTag
                                 (  Destination.Keys.all,
                                    Destination.Offset
                                 )
                              /= null
                              )
                           then
                              Destination.Offset := 0;
                           end if;
                        elsif (  Destination.Client.Validate_CGI
                              and then
                                 Validate_Key
                                 (  Destination.Client.all,
                                    Key
                              )  )  then
                           Add
                           (  Destination.Keys.all,
                              Key,
                              null,
                              Destination.Offset
                           );
                        end if;
                     end;
                  exception
                     when others =>
                        Destination.Offset := 0;
                  end;
                  Last := 0;
                  Destination.State := CGI_Value;
               else
                  if Last < Value'Last then
                     Last := Last + 1;
                     Value (Last) := Data (Index);
                  end if;
               end if;
         end case;
      end loop;
   end Put;

   procedure Read
             (  Stream : in out Content_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
      Count : Stream_Element_Count := Item'Length;

      procedure Add
                (  Data  : Stream_Element_Array;
                   First : in out Stream_Element_Offset;
                   Next  : Content_Item_Ptr
                )  is
         pragma Inline (Add);
         Size : constant Stream_Element_Count := Data'Last + 1 - First;
      begin
         if Size > Count then
            Last := Last + Count;
            Item (Last - Count + 1..Last) :=
               Data (First..First + Count - 1);
            First := First + Count;
            Stream.Length := Stream.Length - Count;
            Count := 0;
         else
            Last  := Last  + Size;
            Count := Count - Size;
            Item (Last - Size + 1..Last) := Data (First..Data'Last);
            Stream.Length := Stream.Length - Size;
            Stream.First  := Next;
         end if;
      end Add;

      procedure Add
                (  Data  : String;
                   First : in out Stream_Element_Offset;
                   Next  : Content_Item_Ptr
                )  is
         pragma Inline (Add);
         Size : constant Stream_Element_Count :=
                Stream_Element_Offset (Data'Last) + 1 - First;
      begin
         if Size > Count then
            Last := Last + Count;
            Item (Last - Count + 1..Last) :=
               From_String
               (  Data
                  (  Integer (First)
                  .. Integer (First + Count - 1)
               )  );
            First := First + Count;
            Stream.Length := Stream.Length - Count;
            Count := 0;
         else
            Last  := Last  + Size;
            Count := Count - Size;
            Item (Last - Size + 1..Last) :=
               From_String
               (  Data
                  (  Integer (First)
                  .. Integer (Data'Last)
               )  );
            Stream.Length := Stream.Length - Size;
            Stream.First  := Next;
         end if;
      end Add;
   begin
      Last := Item'First - 1;
      while Count > 0 and then Stream.First /= null loop
         declare
            This : Content_Item renames Stream.First.all;
         begin
            case This.Kind is
               when Literal_Value =>
                  Add (This.Data, This.First, This.Next);
               when Stream_Elements_Pointer =>
                  Add (This.Data_Ptr.all, This.First, This.Next);
               when String_Pointer =>
                  Add (This.Text_Ptr.all, This.First, This.Next);
            end case;
         end;
      end loop;
   end Read;

   procedure Receive_Body
             (  Client : in out HTTP_Client;
                Stream : access Root_Stream_Type'Class
             )  is
   begin
      Client.Stream := Stream.all'Unchecked_Access;
   end Receive_Body;

   procedure Receive_Body
             (  Client  : in out HTTP_Client;
                Content : access Content_Destination'Class
             )  is
   begin
      Client.Destination := Content.all'Unchecked_Access;
   end Receive_Body;

   procedure Receive_Body
             (  Client  : in out HTTP_Client;
                Content : access CGI_Keys.Table'Class
             )  is
   begin
      for Offset in 1..GetSize (Content.all) loop
         Replace (Content.all, Offset, null);
      end loop;
      Client.Destination  := Client.CGI'Unchecked_Access;
      Client.CGI.Keys     := Content.all'Unchecked_Access;
      Client.CGI.State    := CGI_Key;
      Client.CGI.Offset   := 0;
      Client.Line.Last    := 0;
      Client.Validate_CGI := False;
   end Receive_Body;

   procedure Receive_Body
             (  Client    : in out HTTP_Client;
                Content   : String;
                Delimiter : Character := ','
             )  is
      Start : Integer := Content'First;
   begin
      Erase (Client.CGI.Map);
      for Index in Content'Range loop
         if Content (Index) = Delimiter then
            Replace (Client.CGI.Map, Content (Start..Index - 1), null);
            Start := Index + 1;
         elsif Index = Content'Last then
            Replace (Client.CGI.Map, Content (Start..Index), null);
         end if;
      end loop;
      Client.Destination  := Client.CGI'Unchecked_Access;
      Client.CGI.Keys     := Client.CGI.Map'Unchecked_Access;
      Client.CGI.State    := CGI_Key;
      Client.CGI.Offset   := 0;
      Client.Line.Last    := 0;
      Client.Validate_CGI := False;
   end Receive_Body;

   procedure Receive_Body (Client : in out HTTP_Client) is
   begin
      Erase (Client.CGI.Map);
      Client.Destination  := Client.CGI'Unchecked_Access;
      Client.CGI.Keys     := Client.CGI.Map'Unchecked_Access;
      Client.CGI.State    := CGI_Key;
      Client.CGI.Offset   := 0;
      Client.Line.Last    := 0;
      Client.Validate_CGI := True;
   end Receive_Body;

   procedure Received
             (  Client  : in out HTTP_Client;
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
            if Client.Stream /= null then
               begin
                  Write (Client.Stream.all, Data);
               exception
                  when Error : others =>
                     Body_Error -- Dispatching call
                     (  HTTP_Client'Class (Client),
                        Client.Stream.all,
                        Error
                     );
                     Client.Stream := null;
                     Cleanup_Body_Part (Client);
               end;
            elsif Client.Destination /= null then
               begin
                  Put (Client.Destination.all, To_String (Data));
               exception
                  when Error : others =>
                     if (  Client.Destination
                        =  Client.CGI'Unchecked_Access
                        )
                     then
                        Body_Error -- Dispatching call
                        (  HTTP_Client'Class (Client),
                           Client.CGI.Keys.all,
                           Error
                        );
                     else
                        Body_Error -- Dispatching call
                        (  HTTP_Client'Class (Client),
                           Client.Destination.all,
                           Error
                        );
                     end if;
                     Client.Destination := null;
                     Cleanup_Body_Part (Client);
               end;
            end if;
         end if;
      end Store;

      procedure Store (Data : String) is
         pragma Inline (Store);
      begin
         if Data'Length > 0 then
            if Client.Stream /= null then
               begin
                  Write (Client.Stream.all, From_String (Data));
               exception
                  when Error : others =>
                     Body_Error -- Dispatching call
                     (  HTTP_Client'Class (Client),
                        Client.Stream.all,
                        Error
                     );
                     Client.Stream := null;
                     Cleanup_Body_Part (Client);
               end;
            elsif Client.Destination /= null then
               begin
                  Put (Client.Destination.all, Data);
               exception
                  when Error : others =>
                     if (  Client.Destination
                        =  Client.CGI'Unchecked_Access
                        )
                     then
                        Body_Error -- Dispatching call
                        (  HTTP_Client'Class (Client),
                           Client.CGI.Keys.all,
                           Error
                        );
                     else
                        Body_Error -- Dispatching call
                        (  HTTP_Client'Class (Client),
                           Client.Destination.all,
                           Error
                        );
                     end if;
                     Client.Destination := null;
                     Cleanup_Body_Part (Client);
               end;
           end if;
         end if;
      end Store;

      procedure Multipart_Body (Last : Stream_Element_Offset) is
         pragma Inline (Multipart_Body);
      --
      --  |<- Boundary ->|    |<- Data ----------------------->|
      --  |***+++++      |    |xxxxxx|+++++////////////////////|
      --      |    |                  |    |                  |
      --      |    Position     Pointer    From            Last
      --      Client.Position
      --
      --  + - Matched so far as boundary prefix
      --  * - Matched as boundary prefix earlier (not in Data buffer)
      --  x - Stored as the body
      --  / - Data to process
      --
         Boundary : String renames Client.Boundary.all;
         Position : Integer := Client.Position;
         Start    : constant Stream_Element_Offset := Pointer;
         From     : Stream_Element_Offset := Pointer;
      begin
         loop
            if Position > Boundary'Last then -- All boundary matched
               declare
                  Tail : Stream_Element_Array renames
                         Data (Pointer..From - 1 - Boundary'Length);
               begin
                  if Tail'Length > 0 then
                     if Client.Trace_Body then
                        Trace
                        (  Client,
                           (  "Multipart body |"
                           &  Image (Tail)
                           &  "| "
                           &  Image (Pointer)
                           &  ".."
                           &  Image (Last)
                           &  " "
                           &  Image (Client.Data_Length - From + Start)
                           &  " more expected"
                        )  );
                     end if;
                     Store (Tail);
                  end if;
               end;
               Pointer := From;
               Client.Data_Length :=
                   Client.Data_Length - Pointer + Start;
               Client.Position  := Boundary'First;
               Client.Expecting := Multipart_Body_Tail;
               if Client.Stream /= null then
                  Body_Received -- Dispatching call
                  (  HTTP_Client'Class (Client),
                     Client.Stream.all
                  );
                  Client.Stream := null;
               elsif Client.Destination /= null then
                  Commit (Client.Destination.all);
                  if (  Client.Destination
                     =  Client.CGI'Unchecked_Access
                     )
                  then
                     Body_Received -- Dispatching call
                     (  HTTP_Client'Class (Client),
                        Client.CGI.Keys.all
                     );
                  else
                     Body_Received -- Dispatching call
                     (  HTTP_Client'Class (Client),
                        Client.Destination.all
                     );
                  end if;
                  Client.Destination := null;
               end if;
               Cleanup_Body_Part (Client);
               return;
            elsif From > Last then -- Everything matched the boundary,
               declare             -- so far
                  Tail : Stream_Element_Array renames
                         Data
                         (  Pointer
                         .. (  From
                            -  1
                            -  Stream_Element_Offset
                               (  Position
                               -  Boundary'First
                         )  )  );
               begin
                  if Tail'Length > 0 then
                     if Client.Trace_Body then
                        Trace
                        (  Client,
                           (  "Multipart body |"
                           &  Image (Tail)
                           &  "| "
                           &  Image (Pointer)
                           &  ".."
                           &  Image (Last)
                           &  " "
                           &  Image (Client.Data_Length - From + Start)
                           &  " more expected"
                        )  );
                     end if;
                     Store (Tail);
                  end if;
               end;
               Pointer := From;
               Client.Position := Position;
               Client.Data_Length :=
                  Client.Data_Length - Pointer + Start;
               return;
            elsif Character'Pos (Boundary (Position)) = Data (From) then
               Position := Position + 1;
               From     := From + 1;
            elsif Client.Position > Boundary'First then -- Some out of
               declare                                  -- buffer data
                  From : Integer := Boundary'First + 1;
                  To   : constant Integer := Client.Position;
               begin
                  while From < To loop
                     exit when
                        Is_Prefix (Boundary (From..To), Boundary);
                     From := From + 1;
                  end loop;
                  if Client.Trace_Body then
                     Trace
                     (  Client,
                        (  "Multipart body "
                        &  "|"
                        &  Image
                           (  From_String
                              (  Boundary (Boundary'First..From - 1)
                           )  )
                        &  "| "
                        &  Image (Pointer)
                        &  ".."
                        &  Image (Last)
                     )  );
                  end if;
                  Store (Boundary (Boundary'First..From - 1));
                  Position := Boundary'First + To - From;
                  Client.Position := Position;
               end;
            elsif Position > Boundary'First then -- Boundary prefix was
               From :=                           -- matched, retreating
                  (  From
                  +  1
                  -  Stream_Element_Offset (Position - Boundary'First)
                  );
               Position := Boundary'First;
            else
               From := From + 1;
            end if;
         end loop;
      end Multipart_Body;

      procedure WebSocket_Message_End is
         Socket : WebSocket_Data renames Client.WebSocket;
         Frame  : WebSocket_Message renames Socket.Frame.all;

         function Status return WebSocket_Status is
         begin
            if Frame.Pointer > 2 then
               return (  WebSocket_Status (Frame.Data (1)) * 256
                      +  WebSocket_Status (Frame.Data (2))
                      );
            else
               return 1000;
            end if;
         end Status;

         function Message return String is
         begin
            if Frame.Pointer > 2 then
               return To_String (Frame.Data (3..Frame.Pointer - 1));
            else
               return "";
            end if;
         end Message;
      begin
         case Frame.Type_Of is
            when WebSocket_Binary_Type => -- Binary data frame ended
               if Socket.State = Open_Socket then
                  if Socket.Duplex then
                     Socket.Context := Current_Task;
                  end if;
                  if Client.Trace_Body then
                     Trace
                     (  Client,
                        (  "WebSocket received binary message ["
                        &  Image (Frame.Data (1..Frame.Pointer - 1))
                        &  "]"
                     )  );
                  end if;
                  WebSocket_Received
                  (  HTTP_Client'Class (Client),
                     Frame.Data (1..Frame.Pointer - 1)
                  );
               end if;
            when WebSocket_Text_Type => -- Text data frame ended
               if Socket.State = Open_Socket then
                  if Socket.Duplex then
                     Socket.Context := Current_Task;
                  end if;
                  if Client.Trace_Body then
                     Trace
                     (  Client,
                        (  "WebSocket received text message ["
                        &  To_String (Frame.Data (1..Frame.Pointer - 1))
                        &  "]"
                     )  );
                  end if;
                  WebSocket_Received
                  (  HTTP_Client'Class (Client),
                     To_String (Frame.Data (1..Frame.Pointer - 1))
                  );
               end if;
            when WebSocket_Close_Type =>
               if Socket.State = Open_Socket then
                  if Socket.Duplex then
                     Socket.Context := Current_Task;
                  end if;
                  begin
                     WebSocket_Closed
                     (  HTTP_Client'Class (Client),
                        Status,
                        Message
                     );
                  exception
                     when Error : others =>
                        Trace
                        (  Client,
                           (  "WebSocket closing callback fault: "
                           &  Exception_Information (Error)
                        )  );
                  end;
                  if Socket.State = Open_Socket then
                     WebSocket_Close (Client, Status, Message);
                  end if;
               end if;
               if Socket.State = Closing_Socket then
                  if Queued_To_Send (Client) = 0 then
                     WebSocket_Cleanup (Client);
                  end if;
               end if;
            when WebSocket_Ping_Type =>
               if Socket.State = Open_Socket then
                  WebSocket_Send
                  (  Client, -- Pong frame
                     (  (  2#1000_0000# or WebSocket_Pong_Type,
                           Stream_Element (Frame.Length)
                        )
                     &  Frame.Data (1..Frame.Length)
                  )  );
               end if;
            when others => -- Ignore unsupported frames
               Trace
               (  Client,
                  (  "Message of an unsupported type "
                  &  Image (Integer (Frame.Type_Of))
                  &  " ignored"
               )  );
         end case;
      end WebSocket_Message_End;
   begin
      Pointer := Data'First;
      while Pointer <= Data'Last loop
         case Client.Expecting is
            when Body_Data => -- Single body receipt
               if Client.Data_Length > 0 then -- Receiving the body
                  if Client.Data_Length > Data'Last - Pointer + 1 then
                     Client.Data_Length :=
                        Client.Data_Length - Data'Last + Pointer - 1;
                     if Client.Trace_Body then
                        Trace
                        (  Client,
                           (  "Append body "
                           &  Done (Data (Data'First..Pointer - 1))
                           &  "|"
                           &  Image (Data (Pointer..Data'Last))
                           &  "| "
                           &  Image (Client.Data_Length)
                           &  " more expected"
                        )  );
                     end if;
                     Store (Data (Pointer..Data'Last));
                     Pointer := Data'Last + 1;
                  else
                     if Client.Trace_Body then
                        Trace
                        (  Client,
                           (  "Append body "
                           &  Done (Data (Data'First..Pointer - 1))
                           &  "|"
                           &  Image
                              (  Data
                                 (  Pointer
                                 .. Pointer + Client.Data_Length - 1
                              )  )
                           &  "|"
                           &  Left
                              (  Data
                                 (  Pointer + Client.Data_Length
                                 .. Data'Last
                              )  )
                           &  " No more expected"
                        )  );
                     end if;
                     Store
                     (  Data
                        (  Pointer
                        .. Pointer + Client.Data_Length - 1
                     )  );
                     Pointer := Pointer + Client.Data_Length;
                     Client.Data_Length := 0;
                  end if;
               end if;
               if Client.Data_Length = 0 then -- End of chunk
                  if Client.Chunked then -- Chunked receipt
                     Client.Chunk_Type := Body_Data;
                     Client.Expecting  := Chunk_Line;
                  else -- Explicit length body
                     if Client.Stream /= null then
                        Body_Received -- Dispatching call
                        (  HTTP_Client'Class (Client),
                           Client.Stream.all
                        );
                        Client.Stream := null;
                     elsif Client.Destination /= null then
                        Commit (Client.Destination.all);
                        if (  Client.Destination
                           =  Client.CGI'Unchecked_Access
                           )
                        then
                           Body_Received -- Dispatching call
                           (  HTTP_Client'Class (Client),
                              Client.CGI.Keys.all
                           );
                        else
                           Body_Received -- Dispatching call
                           (  HTTP_Client'Class (Client),
                              Client.Destination.all
                           );
                        end if;
                        Client.Destination := null;
                     end if;
                     Cleanup_Body_Part (Client);
                     Process_Request (Client);
                  end if;
               end if;
            when Multipart_Body_Data => -- Receiving multipart body
               if Client.Data_Length > 0 then -- Receiving a body part
                  if Client.Data_Length > Data'Last - Pointer + 1 then
                     Multipart_Body (Data'Last);
                  else
                     Multipart_Body (Pointer + Client.Data_Length - 1);
                  end if;
               end if;
               if Client.Data_Length = 0 then -- End of chunk
                  if Client.Chunked then -- Chunked receipt
                     Client.Chunk_Type := Multipart_Body_Data;
                     Client.Expecting  := Chunk_Line;
                  else -- Explicit length
                     if Client.Expecting = Multipart_Epilogue then
                        Process_Request (Client);
                     else
                        Raise_Exception
                        (  Data_Error'Identity,
                           "Malformed multipart body"
                        );
                     end if;
                  end if;
               end if;
            when Multipart_Body_Tail =>
               Receive_Multipart_Line
               (  Client,
                  Data,
                  Pointer,
                  Process_Body_Tail'Access
               );
            when Multipart_Preamble => -- Multipart lines and headers
               Receive_Multipart_Line
               (  Client,
                  Data,
                  Pointer,
                  Process_Preamble'Access
               );
            when Multipart_Header_Line =>
               Receive_Multipart_Line
               (  Client,
                  Data,
                  Pointer,
                  Process_Part_Header'Access
               );
            when Multipart_Epilogue =>
               Receive_Multipart_Line
               (  Client,
                  Data,
                  Pointer,
                  Process_Epilogue'Access
               );
            when Request_Line =>
               Receive_Header_Line
               (  Client,
                  Data,
                  Pointer,
                  Process_Request_Line'Access
               );
            when Header_Line =>
               Receive_Header_Line
               (  Client,
                  Data,
                  Pointer,
                  Process_Header_Line'Access
               );
            when Chunk_Line =>
               Receive_Header_Line
               (  Client,
                  Data,
                  Pointer,
                  Process_Chunk_Line'Access
               );
            when WebSocket_Header =>
               declare
                  Octet  : constant Stream_Element := Data (Pointer);
                  Socket : WebSocket_Data renames Client.WebSocket;
               begin
                  Socket.Final := 0 /= (Octet and 2#1000_0000#);
                  if 0 /= (Octet and 2#0111_0000#) then
                     Raise_Exception
                     (  Data_Error'Identity,
                        "RSV bits must be cleared"
                     );
                  end if;
                  Socket.Frame_Type := Octet and 2#1111#;
                  Client.Expecting := WebSocket_Length;
                  Pointer := Pointer + 1;
               end;
            when WebSocket_Length =>
               declare
                  Octet  : constant Stream_Element := Data (Pointer);
                  Socket : WebSocket_Data renames Client.WebSocket;
               begin
                  if 0 = (Octet and 2#1000_0000#) then
                     Raise_Exception
                     (  Data_Error'Identity,
                        "Mask bit must be set"
                     );
                  end if;
                  Socket.Frame_Length :=
                     Stream_Element_Count (Octet and 2#0111_1111#);
                  if Socket.Frame_Type >= 8 then -- A control frame
                     if Socket.Frame_Length > 125 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           "Control frame length is greater than 125"
                        );
                     end if;
                     Socket.Frame := Socket.Control'Unchecked_Access;
                     declare
                        Frame : WebSocket_Message renames
                                Socket.Frame.all;
                     begin
                        Frame.Type_Of     := Socket.Frame_Type;
                        Frame.Length      := Socket.Frame_Length;
                        Frame.Pointer     := 1;
                        Socket.Mask_Index := 0;
                        Client.Expecting  := WebSocket_Mask;
                     end;
                  else -- A data frame
                     Socket.Frame := Socket.Data;
                     declare
                        Frame : WebSocket_Message renames
                                Socket.Frame.all;
                     begin
                        if Socket.Pending then -- A message is pending
                           if (  Socket.Frame_Type
                              /= WebSocket_Continunation_Type
                              )
                           then
                              Raise_Exception
                              (  Data_Error'Identity,
                                 "Missing message continuation frame"
                              );
                           end if;
                        else -- A new frame
                           Frame.Type_Of := Socket.Frame_Type;
                           Frame.Length  := 0;
                           Frame.Pointer := 1;
                           if (  Socket.Frame_Type
                              =  WebSocket_Continunation_Type
                              )
                           then
                              Raise_Exception
                              (  Data_Error'Identity,
                                 "Unsolicited continuation frame"
                              );
                           end if;
                        end if;
                        Socket.Pending := not Socket.Final;
                        if Socket.Frame_Length < 126 then
                           Frame.Length :=
                              Frame.Length + Socket.Frame_Length;
                           Socket.Mask_Index := 0;
                           Client.Expecting := WebSocket_Mask;
                        else
                           if Socket.Frame_Length = 126 then
                              Socket.Length_Count := 2; -- Two more
                           else
                              Socket.Length_Count := 8; -- Eight more
                           end if;
                           Socket.Frame_Length := 0;
                           Client.Expecting := WebSocket_Length_Ex;
                        end if;
                     end;
                  end if;
                  Pointer := Pointer + 1;
               end;
            when WebSocket_Length_Ex =>
               declare
                  Socket : WebSocket_Data renames Client.WebSocket;
                  Frame  : WebSocket_Message renames Socket.Frame.all;
               begin
                  Socket.Frame_Length :=
                     (  Socket.Frame_Length * 256
                     +  Stream_Element_Count (Data (Pointer))
                     );
                  if (  not Socket.Chunked
                     and then
                        Socket.Frame_Length > Socket.Max_Length
                     )
                  then
                     Raise_Exception
                     (  Status_Error'Identity,
                        "Data message length exceeds the limit set"
                     );
                  end if;
                  if Socket.Length_Count = 1 then
                     Frame.Length := Frame.Length + Socket.Frame_Length;
                     if (  not Socket.Chunked
                        and then
                           Frame.Length > Socket.Max_Length
                        )
                     then
                        Raise_Exception
                        (  Status_Error'Identity,
                           "Data message length exceeds the limit set"
                        );
                     end if;
                     Socket.Mask_Index := 0;
                     Client.Expecting  := WebSocket_Mask;
                  else
                     Socket.Length_Count := Socket.Length_Count - 1;
                  end if;
                  Pointer := Pointer + 1;
               end;
            when WebSocket_Mask =>
               declare
                  Socket : WebSocket_Data renames Client.WebSocket;
               begin
                  Socket.Mask (Socket.Mask_Index) := Data (Pointer);
                  Socket.Mask_Index := Socket.Mask_Index + 1;
                  Pointer := Pointer + 1;
                  if Socket.Mask_Index = 0 then
                     if Socket.Frame.Length > 0 then
                        Client.Expecting := WebSocket_Payload_Data;
                     else -- No payload data
                        Client.Expecting := WebSocket_Header;
                        if Socket.Final then
                           WebSocket_Message_End;
                        end if;
                     end if;
                  end if;
               end;
            when WebSocket_Payload_Data =>
               declare
                  Socket : WebSocket_Data renames Client.WebSocket;
                  Frame  : WebSocket_Message renames Socket.Frame.all;
               begin
                  while Pointer <= Data'Last loop
                     exit when Frame.Pointer > Frame.Length;
                     if Socket.Chunked then
                        if Frame.Pointer > Frame.Data'Last then
                           if Socket.Duplex then
                              Socket.Context := Current_Task;
                           end if;
                           if Socket.Frame_Type = WebSocket_Binary_Type then
                               if Client.Trace_Body then
                                  Trace
                                  (  Client,
                                     (  "WebSocket received binary "
                                     &  "message part ["
                                     &  Image (Frame.Data)
                                     &  "]"
                                  )  );
                               end if;
                               WebSocket_Received_Part
                               (  HTTP_Client'Class (Client),
                                  Frame.Data (1..Frame.Pointer - 1)
                               );
                           else
                               if Client.Trace_Body then
                                  Trace
                                  (  Client,
                                     (  "WebSocket received text "
                                     &  "message part ["
                                     &  To_String (Frame.Data)
                                     &  "]"
                                  )  );
                               end if;
                               WebSocket_Received_Part
                               (  HTTP_Client'Class (Client),
                                  To_String (Frame.Data)
                               );
                           end if;
                           Frame.Pointer := Frame.Data'First;
                           Frame.Length :=
                              Frame.Length - Frame.Data'Length;
                        end if;
                     end if;
                     Frame.Data (Frame.Pointer) :=
                        (  Data (Pointer)
                        xor
                           Socket.Mask (Socket.Mask_Index)
                        );
                     Socket.Mask_Index := Socket.Mask_Index + 1;
                     Frame.Pointer := Frame.Pointer + 1;
                     Pointer := Pointer + 1;
                  end loop;
                  if Frame.Pointer > Frame.Length then
                     Client.Expecting := WebSocket_Header;
                     if Socket.Final then
                        WebSocket_Message_End;
                     end if;
                  end if;
               end;
         end case;
      end loop;
   exception
      when Error : others =>
         if Client.WebSocket.State /= Closed_Socket then
            if Client.Trace_Body then
               Trace
               (  Client,
                  "WebSocket error " & Exception_Message (Error)
               );
            end if;
            begin
               WebSocket_Error (HTTP_Client'Class (Client), Error);
            exception
               when Error : others =>
                  Trace
                  (  Client,
                     (  "WebSocket error callback fault: "
                     &  Exception_Information (Error)
                  )  );
            end;
            WebSocket_Cleanup (Client);
         end if;
         raise;
   end Received;

   procedure Receive_Body_Tracing
             (  Client : in out HTTP_Client;
                Enable : Boolean
             )  is
   begin
      Client.Trace_Body := Enable;
   end Receive_Body_Tracing;

   procedure Receive_Header_Tracing
             (  Client : in out HTTP_Client;
                Enable : Boolean
             )  is
   begin
      Client.Trace_Header := Enable;
   end Receive_Header_Tracing;

   procedure Receive_Header_Line
             (  Client  : in out HTTP_Client'Class;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Handler : Completion
             )  is
   begin
      Get_Header_Line : loop
         Feed
         (  Client.Data.List (Client.Data.Current).all,
            Data,
            Pointer,
            Client,
            Client.State
         );
         if Client.State = 0 then -- Done with this item
            Client.Data.Current := Client.Data.Current + 1;
            while Client.Data.Current > Client.Data.Length loop
               if Client.Data.Caller = null then
                  Client.Data.Current := 1;
                  Handler (Client);
                  exit Get_Header_Line;
               end if;
               Client.Data := Client.Data.Caller;
               Client.Data.Current := Client.Data.Current + 1;
            end loop;
         else
            exit when Pointer > Data'Last; -- All data consumed
            Raise_Exception
            (  Status_Error'Identity,
               (  "Unprocessed data left when after return from "
               &  Expanded_Name
                  (  Client.Data.List (Client.Data.Current).all'Tag
            )  )  );
         end if;
      end loop Get_Header_Line;
   end Receive_Header_Line;

   procedure Receive_Multipart_Line
             (  Client  : in out HTTP_Client'Class;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Handler : Completion
             )  is
   begin
      Get_Header_Line : while Client.Data_Length > 0 loop
         if Client.Data_Length > Data'Last - Pointer + 1 then
            declare
               Start : constant Stream_Element_Offset := Pointer;
               Slice : Stream_Element_Array renames
                       Data (Pointer..Data'Last);
            begin
               exit when Slice'Length = 0;
               Feed
               (  Client.Data.List (Client.Data.Current).all,
                  Slice,
                  Pointer,
                  Client,
                  Client.State
               );
               Client.Data_Length :=
                  Client.Data_Length - Pointer + Start;
            end;
         else
            declare
               Start : constant Stream_Element_Offset := Pointer;
               Slice : Stream_Element_Array renames
                       Data (Pointer..Pointer + Client.Data_Length - 1);
            begin
               exit when Slice'Length = 0;
               Feed
               (  Client.Data.List (Client.Data.Current).all,
                  Slice,
                  Pointer,
                  Client,
                  Client.State
               );
               Client.Data_Length :=
                  Client.Data_Length - Pointer + Start;
            end;
         end if;
         if Client.State = 0 then -- Done with this item
            Client.Data.Current := Client.Data.Current + 1;
            while Client.Data.Current > Client.Data.Length loop
               if Client.Data.Caller = null then
                  Client.Data.Current := 1;
                  Handler (Client);
                  exit Get_Header_Line;
               end if;
               Client.Data := Client.Data.Caller;
               Client.Data.Current := Client.Data.Current + 1;
            end loop;
         else
            exit when Pointer > Data'Last;
            Raise_Exception
            (  Status_Error'Identity,
               (  "Unprocessed data left when after return from "
               &  Expanded_Name
                  (  Client.Data.List (Client.Data.Current).all'Tag
            )  )  );
         end if;
      end loop Get_Header_Line;
      if Client.Data_Length = 0 then -- End of chunk
         if Client.Chunked then -- Chunked receipt
            Client.Chunk_Type := Client.Expecting;
            Client.Expecting  := Chunk_Line;
         else -- Explicit length
            if Client.Expecting = Multipart_Epilogue then
               Client.Expecting  := Header_Line;
               Process_Request (Client);
            else
               Raise_Exception
               (  Data_Error'Identity,
                  "Malformed multipart body"
               );
            end if;
         end if;
      end if;
   end Receive_Multipart_Line;

   procedure Reply_HTML
             (  Client  : in out HTTP_Client;
                Code    : Positive;
                Reason  : String;
                Message : String;
                Get     : Boolean := True
             )  is
   begin
      Send_Status_Line (Client, Code, Reason);
      Send_Date (Client);
      Send_Content_Type (Client, "text/html");
      Send_Connection (Client, False);
      if Get then
         Send_Body (Client, Message);
      end if;
   end Reply_HTML;

   procedure Reply_Text
             (  Client  : in out HTTP_Client;
                Code    : Positive;
                Reason  : String;
                Message : String;
                Get     : Boolean := True
             )  is
   begin
      Send_Status_Line (Client, Code, Reason);
      Send_Date (Client);
      Send_Content_Type (Client, "text/plain");
      Send_Connection (Client, False);
      if Get then
         Send_Body (Client, Message);
      end if;
   end Reply_Text;

   procedure Send
             (  Client  : in out HTTP_Client;
                Message : String
             )  is
      Pointer : Integer := Message'First;
   begin
      Send (Client, Message, Pointer);
      if Pointer <= Message'Last then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Output buffer overrun, "
            &  Image (Queued_To_Send (Client))
            &  " elements queued, space for at least more "
            &  Image (Message'Last - Pointer + 1)
            &  " requred (available "
            &  Image (Available_To_Send (Client))
            &  ")"
         )  );
      end if;
   end Send;

   procedure Send
             (  Client  : in out HTTP_Client;
                Message : Stream_Element_Array
             )  is
      Pointer : Stream_Element_Offset := Message'First;
   begin
      Send (Client, Message, Pointer);
      if Pointer <= Message'Last then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Output buffer overrun, "
            &  Image (Queued_To_Send (Client))
            &  " elements queued, space for at least more "
            &  Image (Message'Last - Pointer + 1)
            &  " requred (available "
            &  Image (Available_To_Send (Client))
            &  ")"
         )  );
      end if;
   end Send;

   procedure Send_Accept_Ranges
             (  Client        : in out HTTP_Client;
                Accept_Ranges : Boolean
             )  is
   begin
      if Accept_Ranges then
         Send (Client, "Accept-Ranges: bytes" & CRLF);
      else
         Send (Client, "Accept-Ranges: none" & CRLF);
      end if;
   end Send_Accept_Ranges;

   procedure Send_Age (Client : in out HTTP_Client; Age : Duration) is
      Value : Unsigned_32;
   begin
      if Age = Duration'Last then
         Send (Client, "Age: 2147483648" & CRLF);
      elsif Age <= 0.0 then
         Send (Client, "Age: 0" & CRLF);
      else
         Value := Unsigned_32 (Age);
         Send (Client, "Age:" & Unsigned_32'Image (Value) & CRLF);
      end if;
   exception
      when Constraint_Error =>
         Send (Client, "Age: 2147483648" & CRLF);
   end Send_Age;

   procedure Send_Allow
             (  Client  : in out HTTP_Client;
                Allowed : HTTP_Allowed
             )  is
      First   : Boolean := True;
      Text    : String (1..120);
      Pointer : Integer := Text'First;
   begin
      Put (Text, Pointer, "Allow:");
      for Index in 1..GetSize (Commands) loop
         if Allowed (GetTag (Commands, Index)) then
            if First then
               First := False;
            else
               Put (Text, Pointer, ", ");
            end if;
            Put (Text, Pointer, GetName (Commands, Index));
         end if;
      end loop;
      Put (Text, Pointer, CRLF);
      Send (Client, Text (Text'First..Pointer - 1));
   end Send_Allow;

   procedure Send_Body
             (  Client : in out HTTP_Client;
                Stream : access Root_Stream_Type'Class;
                Get    : Boolean := True
             )  is
   begin
      Client.Stream := Stream.all'Unchecked_Access;
      Send (Client, "Transfer-Encoding: chunked" & CRLF);
      if Get then
         Send (Client, CRLF);
         Client.Chunked := True;
         Continue (Client, Stream_Chunk'Access);
      else
         Body_Sent (Client, Client.Stream.all, Get);
      end if;
   end Send_Body;

   procedure Send_Body
             (  Client : in out HTTP_Client;
                Stream : access Root_Stream_Type'Class;
                Length : Stream_Element_Count;
                Get    : Boolean := True
             )  is
   begin
      Client.Stream := Stream.all'Unchecked_Access;
      Send (Client, "Content-Length: " & Image (Length) & CRLF);
      Client.Data_Length := Length;
      if Get then
         Send (Client, CRLF);
         Client.Chunked := False;
         Continue (Client, Stream_Chunk'Access);
      else
         Body_Sent (Client, Client.Stream.all, Get);
      end if;
   end Send_Body;

   procedure Send_Body
             (  Client  : in out HTTP_Client;
                Content : access Content_Source'Class;
                Get     : Boolean := True
             )  is
   begin
      Client.Source := Content.all'Unchecked_Access;
      Send (Client, "Transfer-Encoding: chunked" & CRLF);
      if Get then
         Send (Client, CRLF);
         Client.Chunked := True;
         Continue (Client, Content_Chunk'Access);
      end if;
   end Send_Body;

   procedure Send_Body
             (  Client  : in out HTTP_Client;
                Content : access Content_Source'Class;
                Length  : Stream_Element_Count;
                Get     : Boolean := True
             )  is
   begin
      Client.Source := Content.all'Unchecked_Access;
      Send (Client, "Content-Length: " & Image (Length) & CRLF);
      Client.Data_Length := Length;
      if Get then
         Send (Client, CRLF);
         Client.Chunked := False;
         Continue (Client, Content_Chunk'Access);
      end if;
   end Send_Body;

   procedure Send_Body
             (  Client : in out HTTP_Client;
                Get    : Boolean := True
             )  is
   begin
      Send_Body
      (  Client => Client,
         Stream => Client.Body_Content'Unchecked_Access,
         Length => Accumulated_Body_Length (Client),
         Get    => Get
      );
   end Send_Body;

   procedure Send_Body
             (  Client  : in out HTTP_Client;
                Content : String;
                Get     : Boolean := True
             )  is
   begin
      Send_Length (Client, Natural'(Content'Length));
      Send (Client, CRLF);
      if Get then
         Send (Client, Content);
         Send (Client, CRLF);
      end if;
   end Send_Body;

   procedure Send_Connection
             (  Client     : in out HTTP_Client;
                Persistent : Boolean := True
             )  is
   begin
      if (  0 = (Client.Connection and Connection_Close)
         and
            Persistent
         )  then
         Send (Client, "Connection: keep-alive" & CRLF);
      else
         Client.Connection := Client.Connection or Connection_Close;
         Send (Client, "Connection: close" & CRLF);
      end if;
   end Send_Connection;

   procedure Send_Content_Range
             (  Client        : in out HTTP_Client;
                Content_Range : String := "none"
             )  is
   begin
      Send (Client, "Content-Range: " & Content_Range & CRLF);
   end Send_Content_Range;

   procedure Send_Content_Range
             (  Client : in out HTTP_Client;
                From   : Stream_Element_Count;
                To     : Stream_Element_Count;
                Length : Stream_Element_Count
             )  is
   begin
      Send
      (  Client,
         (  "Content-Range: "
         &  Image (From)
         &  '-'
         &  Image (To)
         &  '/'
         &  Image (Length)
         &  CRLF
      )  );
   end Send_Content_Range;

   procedure Send_Content_Type
             (  Client  : in out HTTP_Client;
                Media   : String := "text/plain";
                Charset : String := "UTF-8"
             )  is
   begin
      Send
      (  Client,
         "Content-Type: " & Media & "; charset=" & Charset & CRLF
      );
   end Send_Content_Type;

   procedure Send_Date
             (  Client : in out HTTP_Client;
                Date   : Time := Clock
             )  is
   begin
      Send (Client, "Date: " & To_HTTP (Date) & CRLF);
   end Send_Date;

   procedure Send_If_Modified_Since
             (  Client : in out HTTP_Client;
                Date   : Time
             )  is
   begin
      Send (Client, "If-Modified-Since: " & To_HTTP (Date) & CRLF);
   end Send_If_Modified_Since;

   procedure Send_If_Unmodified_Since
             (  Client : in out HTTP_Client;
                Date   : Time
             )  is
   begin
      Send (Client, "If-Unmodified-Since: " & To_HTTP (Date) & CRLF);
   end Send_If_Unmodified_Since;

   procedure Send_Last_Modified
             (  Client : in out HTTP_Client;
                Date   : Time
             )  is
   begin
      Send (Client, "Last-Modified: " & To_HTTP (Date) & CRLF);
   end Send_Last_Modified;

   procedure Send_Length
             (  Client : in out HTTP_Client;
                Length : Natural
             )  is
   begin
      Send (Client, "Content-Length: " & Image (Length) & CRLF);
   end Send_Length;

   procedure Send_Length
             (  Client : in out HTTP_Client;
                Length : Stream_Element_Count
             )  is
   begin
      Send (Client, "Content-Length: " & Image (Length) & CRLF);
   end Send_Length;

   procedure Send_Server (Client : in out HTTP_Client) is
   begin
      Send
      (  Client,
         "Server: " & Get_Name (HTTP_Client'Class (Client)) & CRLF
      );
   end Send_Server;

   procedure Send_Status_Line
             (  Client  : in out HTTP_Client;
                Code    : Positive;
                Text    : String;
                Version : String := "HTTP/1.1"
             )  is
   begin
      Send (Client, Version & " " & Image (Code) & ' ');
      Send (Client, Text);
      Send (Client, CRLF);
   end Send_Status_Line;

   procedure Sent (Client : in out HTTP_Client) is
      Chain : Action;
   begin
      if Queued_To_Send (Client) = 0 then
         Chain := Client.Chain;
         Client.Chain := null;
         if Chain /= null then
            Chain (Client);
         elsif 0 /= (Client.Connection and Connection_Close) then
            raise Connection_Error; -- I am done
         else
            case Client.WebSocket.State is
               when Open_Socket | Closed_Socket =>
                  null;
               when Closing_Socket =>
                  WebSocket_Cleanup (Client);
            end case;
         end if;
      end if;
   exception
      when Error : others =>
         if Client.WebSocket.State /= Closed_Socket then
            if Client.Trace_Body then
               Trace
               (  Client,
                  "WebSocket error " & Exception_Message (Error)
               );
            end if;
            begin
               WebSocket_Error (HTTP_Client'Class (Client), Error);
            exception
               when Error : others =>
                  Trace
                  (  Client,
                     (  "WebSocket error callback fault: "
                     &  Exception_Information (Error)
                  )  );
            end;
            WebSocket_Cleanup (Client);
         end if;
         raise;
   end Sent;

   procedure Send_Content
             (  Client : in out HTTP_Client;
                Data   : Stream_Element_Array
             )  is
      Length : constant Stream_Element_Count :=
               Available_To_Send (Client);
   begin
      if Data'Length = 0 then
         return;
      end if;
      if Client.Stub = null and then Length >= Data'Length then
         Send (Client, Data); -- Send all at once
      else -- Send as much as we can
         Send (Client, Data (Data'First..Data'First + Length - 1));
         Queue_Content (Client, Data (Data'First + Length..Data'Last));
      end if;
   end Send_Content;

   procedure Send_Content
             (  Client : in out HTTP_Client;
                Data   : String
             )  is
      Length : constant Stream_Element_Count :=
               Available_To_Send (Client);
   begin
      if Data'Length = 0 then
         return;
      end if;
      if Client.Stub = null and then Length >= Data'Length then
         Send (Client, Data);
      else -- Send as much as we can
         Send
         (  Client,
            Data (Data'First..Data'First + Integer (Length) - 1)
         );
         Queue_Content
         (  Client,
            Data (Data'First + Integer (Length)..Data'Last)
         );
      end if;
   end Send_Content;

   procedure Set_Allowed
             (  Client  : in out HTTP_Client;
                Allowed : HTTP_Allowed
             )  is
   begin
      Client.Allowed := Allowed;
   end Set_Allowed;

   procedure Set_Failed
             (  Client : in out HTTP_Client;
                Error  : Exception_Occurrence
             )  is
   begin
      Client.Mutex.Failed (Error);
   end Set_Failed;

   procedure Skip (Source : String; Pointer : in out Integer) is
   begin
      while Pointer <= Source'Last loop
         case Source (Pointer) is
            when ' ' | Character'Val (9) =>
               exit;
            when others =>
               Pointer := Pointer + 1;
         end case;
      end loop;
   end Skip;

   procedure Status_Line_Received
             (  Client  : in out HTTP_Client;
                Method  : HTTP_Method;
                Version : HTTP_Version
             )  is
   begin
      Client.Method  := Method;
      Client.Version := Version;
   end Status_Line_Received;

   procedure Status_Line_Received
             (  Client  : in out HTTP_Client;
                Method  : HTTP_Method;
                Path    : String;
                Version : HTTP_Version
             )  is
      type Status_Ptr is access Status_Line;
      for Status_Ptr'Storage_Pool use Client.Pool;
      Ptr : constant Status_Ptr :=
               new Status_Line'
                   (  Kind         => File,
                      Path_Length  => Path'Length,
                      Host_Length  => 0,
                      Query_Length => 0,
                      File         => Path
                   );
   begin
      Client.Method  := Method;
      Client.Version := Version;
      Client.Status  := Ptr.all'Unchecked_Access;
   end Status_Line_Received;

   procedure Status_Line_Received
             (  Client  : in out HTTP_Client;
                Scheme  : Scheme_Type;
                Method  : HTTP_Method;
                Host    : String;
                Port    : Port_Type;
                Path    : String;
                Query   : String;
                Version : HTTP_Version
             )  is
      type Status_Ptr is access Status_Line;
      for Status_Ptr'Storage_Pool use Client.Pool;
      Ptr : constant Status_Ptr :=
               new Status_Line'
                   (  Kind         => URI,
                      Scheme       => Scheme,
                      Path_Length  => Path'Length,
                      Host_Length  => Host'Length,
                      Query_Length => Query'Length,
                      Host         => Host,
                      Port         => Port,
                      Path         => Path,
                      Query        => Query
                   );
   begin
      Client.Method  := Method;
      Client.Version := Version;
      Client.Status  := Ptr.all'Unchecked_Access;
   end Status_Line_Received;

   procedure Stream_Chunk (Client : in out HTTP_Client'Class) is
      Done : Boolean;
   begin
      if Client.Stream /= null then
         if Client.Chunked then -- Chunked transfer
            Send
            (  Client        => Client,
               Stream        => Client.Stream.all,
               Reserve       => 18,
               Get_Prefix    => Get_Prefix'Access,
               Get_Suffix    => Get_Suffix'Access,
               End_Of_Stream => Done
            );
            if Done then
               Body_Sent (Client, Client.Stream.all, True);
               Client.Stream := null;
            else
               Continue (Client, Stream_Chunk'Access);
            end if;
         else
            Send
            (  Client        => Client,
               Stream        => Client.Stream.all,
               Count         => Client.Data_Length,
               End_Of_Stream => Done
            );
            if Done then
               if Client.Data_Length > 0 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Missing "
                     &  Image (Client.Data_Length)
                     &  " streamed data to transfer"
                  )  );
               end if;
               Body_Sent (Client, Client.Stream.all, True);
               Client.Stream := null;
            else
               if Client.Data_Length = 0 then
                  Body_Sent (Client, Client.Stream.all, True);
                  Client.Stream := null;
               else
                  Continue (Client, Stream_Chunk'Access);
               end if;
            end if;
         end if;
      end if;
   end Stream_Chunk;

   function To_Escaped (Name : String) return String is
      Length : Natural := 0;
   begin
      for Index in Name'Range loop
         case Name (Index) is
            when 'A'..'Z' | 'a'..'z' | '0'..'9' |
                 '-' | '_' | '.' | '!' | '~' | '*' | ''' | '(' | ')' =>
               Length := Length + 1;
            when others =>
               Length := Length + 3;
         end case;
      end loop;
      declare
         Pointer : Integer := 1;
         Result  : String (1..Length);
      begin
         for Index in Name'Range loop
            case Name (Index) is
               when 'A'..'Z' | 'a'..'z' | '0'..'9' |
                    '-' | '_' | '.' | '!' | '~' | '*' |
                    ''' | '(' | ')' =>
                  Result (Pointer) := Name (Index);
                  Pointer := Pointer + 1;
               when others =>
                  Result (Pointer) := '%';
                  Pointer := Pointer + 1;
                  Strings_Edit.Integers.Put
                  (  Destination => Result,
                     Pointer     => Pointer,
                     Value       => Character'Pos (Name (Index)),
                     Base        => 16,
                     Field       => 2,
                     Justify     => Right,
                     Fill        => '0'
                  );
            end case;
         end loop;
         return Result;
      end;
   end To_Escaped;

   function To_Flags (Value : String) return Connection_Flags is
      Result : Connection_Flags := 0;
      Start  : Integer := Value'First;

      procedure Add (Field : String) is
         Index : constant Integer := Locate (Connections, Trim (Field));
      begin
         if Index > 0 then
            Result := Result or GetTag (Connections, Index);
         end if;
      end Add;
   begin
      for Pointer in Value'Range loop
         if Value (Pointer) = ',' then
            if Start <= Pointer - 1 then
               Add (Value (Start..Pointer - 1));
            end if;
            Start := Pointer + 1;
         end if;
      end loop;
      if Start <= Value'Last then
         Add (Value (Start..Value'Last));
      end if;
      return Result;
   end To_Flags;

   function To_HTML (Text : String) return String is
      use Strings_Edit.UTF8;
      Length  : Natural := 0;
      Pointer : Integer := Text'First;
      Code    : UTF8_Code_Point;
   begin
      while Pointer <= Text'Last loop
         Get (Text, Pointer, Code);
         case Code is
            when Character'Pos ('<') | Character'Pos ('>') =>
               Length := Length + 4;
            when Character'Pos ('&') =>
               Length := Length + 5;
            when Character'Pos ('"') =>
               Length := Length + 6;
            when 32..33 | 35..37 | 39..59 | 61 | 63..126 =>
               Length := Length + 1;
            when others =>
               if Code <= 16#F# then
                  Length := Length + 4;
               elsif Code <= 16#FF# then
                  Length := Length + 5;
               elsif Code <= 16#FFF# then
                  Length := Length + 6;
               elsif Code <= 16#FFFF# then
                  Length := Length + 7;
               elsif Code <= 16#FFFFF# then
                  Length := Length + 8;
               else
                  Length := Length + 9;
               end if;
         end case;
      end loop;
      Pointer := Text'First;
      declare
         Result : String (1..Length);
         Index  : Integer := 1;
      begin
         while Pointer <= Text'Last loop
            Get (Text, Pointer, Code);
            case Code is
               when Character'Pos ('<') =>
                  Put (Result, Index, "&lt;");
               when Character'Pos ('>') =>
                  Put (Result, Index, "&gt;");
               when Character'Pos ('&') =>
                  Put (Result, Index, "&amp;");
               when Character'Pos ('"') =>
                  Put (Result, Index, "&quot;");
               when 32..33 | 35..37 | 39..59 | 61 | 63..126 =>
                  Result (Index) := Character'Val (Code);
                  Index := Index + 1;
               when others =>
                  Put (Result, Index, "&#");
                  Put (Result, Index, Integer (Code), Base => 16);
                  Put (Result, Index, ";");
            end case;
         end loop;
         return Result (1..Index - 1);
      end;
   end To_HTML;

   function To_HTTP (Date : Time) return String is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;

      function Day_Of_Week return String is
      begin
         case Day_Of_Week (Date) is
            when Monday    => return "Mon";
            when Tuesday   => return "Tue";
            when Wednesday => return "Wed";
            when Thursday  => return "Thu";
            when Friday    => return "Fri";
            when Saturday  => return "Sat";
            when Sunday    => return "Sun";
         end case;
      end Day_Of_Week;

      function Month_Name return String is
      begin
         case Month is
            when  1 => return "Jan";
            when  2 => return "Feb";
            when  3 => return "Mar";
            when  4 => return "Apr";
            when  5 => return "May";
            when  6 => return "Jun";
            when  7 => return "Jul";
            when  8 => return "Aug";
            when  9 => return "Sep";
            when 10 => return "Oct";
            when 11 => return "Nov";
            when 12 => return "Dec";
         end case;
      end Month_Name;

      function Zone return String is
      begin
         declare
            Offset  : Time_Offset := UTC_Time_Offset (Date);
            Pointer : Integer := 2;
            Text    : String (1..5);
         begin
            if Offset >= 0 then
               Text (1) := '+';
            else
               Offset   := -Offset;
               Text (1) := '-';
            end if;
            Put
            (  Destination => Text,
               Pointer     => Pointer,
               Value       => Integer (Offset / 60),
               Field       => 2,
               Justify     => Right,
               Fill        => '0'
            );
            Put
            (  Destination => Text,
               Pointer     => Pointer,
               Value       => Integer (Offset rem 60),
               Field       => 2,
               Justify     => Right,
               Fill        => '0'
            );
            return Text;
         end;
      exception
         when Unknown_Zone_Error =>
            return "GMT";
      end Zone;

   begin
      Split (Date, Year, Month, Day, Seconds);
      return
      (  Day_Of_Week
      &  ", "
      &  Image (Integer (Day))
      &  ' '
      &  Month_Name
      &  ' '
      &  Image (Integer (Year))
      &  ' '
      &  Image (Duration (Seconds))
      &  ' '
      &  Zone
      );
   end To_HTTP;

   function To_Time (Date : String) return Time is
      Year     : Year_Number;
      Month    : Month_Number;
      Day      : Day_Number;
      Week_Day : Day_Name;
      Hour     : Hour_Number;
      Minute   : Minute_Number;
      Second   : Second_Number;
      Zone     : Time_Zones.Time_Offset;
      Pointer  : Integer := Date'First;

      procedure Get_Year (Expand : Boolean) is
         Value : Integer;
      begin
         Get (Date, Pointer, Value);
         if Expand then
            if Year < 50 then
               Year := Year_Number (Value + 2000);
            elsif Year < 100 then
               Year := Year_Number (Value + 1900);
            end if;
         else
            Year := Year_Number (Value);
         end if;
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Wrong or missing year number"
            );
      end Get_Year;

      procedure Get_Day is
      begin
         Get (Date, Pointer, Integer (Day));
      exception
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Wrong or missing day number"
            );
      end Get_Day;

      procedure Get_Month is
      begin
         Get (Date, Pointer, Months, Month);
      exception
         when End_Error =>
            Raise_Exception
            (  Data_Error'Identity,
               "Wrong or missing month name"
            );
      end Get_Month;

      procedure Get_Time is
      begin
         begin
            Get (Date, Pointer, Integer (Hour));
         exception
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Wrong or missing hour number"
               );
         end;
         if Date (Pointer) = ':' then
            Pointer := Pointer + 1;
         else
            Raise_Exception (Data_Error'Identity, "Colon is expected");
         end if;
         begin
            Get (Date, Pointer, Integer (Minute));
         exception
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Wrong or missing minute number"
               );
         end;
         if Date (Pointer) = ':' then
            Pointer := Pointer + 1;
         else
            Raise_Exception (Data_Error'Identity, "Colon is expected");
         end if;
         begin
            Get (Date, Pointer, Integer (Second));
         exception
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Wrong or missing second number"
               );
         end;
      end Get_Time;

      procedure Get_Zone is
         procedure Get_Offset is
         begin
            Zone :=
               Time_Offset
               (  Integer'(Value (Date (Pointer - 3..Pointer - 3)))
               +  Integer'(Value (Date (Pointer - 2..Pointer - 1)))
               );
            if Date (Pointer - 4) = '-' then
               Zone := -Zone;
            end if;
         exception
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Wrong time zone offset"
               );
         end Get_Offset;
      begin
         Get (Date, Pointer, Zones, Zone);
      exception
         when End_Error =>
            if Pointer + 4 < Date'Last then
               Raise_Exception
               (  Data_Error'Identity,
                  "Wrong time zone offset"
               );
            end if;
            if Date (Pointer) = '+' or else Date (Pointer) = '-' then
               Pointer := Pointer + 5;
               Get_Offset;
            else
               Raise_Exception
               (  Data_Error'Identity,
                  "Missing sign of time zone offset"
               );
            end if;
      end Get_Zone;

   begin
      begin
         Get (Date, Pointer, Week_Days, Week_Day);
      exception
         when End_Error =>
            Raise_Exception
            (  Data_Error'Identity,
               "Wrong or missing week day: " & Date (Pointer..Date'Last)
            );
      end;
      if Date (Pointer) = ',' then
         Pointer := Pointer + 1;
      end if;
      Get (Date, Pointer);
      begin
         Get (Date, Pointer, Months, Month); -- Dec 31 23:59:59 1999
         Get (Date, Pointer);
         Get_Day;
      exception
         when End_Error => -- No month
            Get_Day;
            if Date (Pointer) = '-' then -- 31-Dec-99 23:59:59 GMT
               Pointer := Pointer + 1;
               Get_Month;
               if Date (Pointer) = '-' then
                  Pointer := Pointer + 1;
               else
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Hyphen is expected after the month name"
                  );
               end if;
               Get_Year (True);
            else                         -- 31 Dec 1999 23:59:59 GMT
               Get (Date, Pointer);
               Get_Month;
               Get (Date, Pointer);
               Get_Year (False);
            end if;
            Get (Date, Pointer);
            Get_Time;
            Get (Date, Pointer);
            Get_Zone;
      end;
      return
         Time_Of
         (  Year      => Year,
            Month     => Month,
            Day       => Day,
            Hour      => Hour,
            Minute    => Minute,
            Second    => Second,
            Time_Zone => Zone
         );
   exception
      when Time_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            "Illegal date specification"
         );
   end To_Time;

   procedure Trace
             (  Client  : in out HTTP_Client;
                Message : String
             )  is
   begin
      Trace
      (  Client.Listener.Factory.all,
         Image (Get_Client_Address (Client)) & ' ' & Message
      );
   end Trace;

   function Validate_Key
            (  Client : HTTP_Client;
               Key    : String
            )  return Boolean is
   begin
      return True;
   end Validate_Key;

   function Value (Text : String) return Header_Value is
      Index : constant Integer := Locate (Request_Headers, Text);
   begin
      if Index > 0 then
         return
         (  None   => False,
            Header => GetTag (Request_Headers, Index)
         );
      else
         return (None => True);
      end if;
   end Value;

   procedure WebSocket_Blocking_Send
             (  Client : in out HTTP_Client'Class;
                Data   : Stream_Element_Array;
                First  : Boolean;
                Last   : Boolean
             )  is
      Pointer : Stream_Element_Offset := Data'First;
      Next    : Stream_Element_Offset;
   begin
      if First then
         Client.Mutex.Seize;
      end if;
      begin
         if Data'Length > 0 then
            while Pointer <= Data'Last loop
               Push (Client, Data (Pointer..Data'Last), Next);
               if Next < Pointer then
                  raise Connection_Error;
               end if;
               if Client.Trace_Body then
                  Trace
                  (  Client,
                     (  "WebSocket message part sent ["
                     &  Image (Data (Pointer..Next))
                     &  "] "
                     &  Image (Pointer)
                     &  ".."
                     &  Image (Next)
                     &  "/"
                     &  Image (Data'Last)
                  )  );
               end if;
               Pointer := Next + 1;
            end loop;
            if Last then
               Client.Mutex.Set (Idle);
            end if;
         end if;
      exception
         when Error : Connection_Error =>
            Set_Failed (Client, Error);
            Raise_Exception
            (  End_Error'Identity,
               "Connection closed by peer"
            );
         when Error : others =>
            Set_Failed (Client, Error);
            raise;
      end;
   end WebSocket_Blocking_Send;

   procedure WebSocket_Finalize (Client : in out HTTP_Client) is
   begin
      null;
   end WebSocket_Finalize;

   procedure WebSocket_Cleanup (Client : in out HTTP_Client'Class) is
      Socket : WebSocket_Data renames Client.WebSocket;
   begin
      if Socket.State /= Closed_Socket then
         if Client.Trace_Body then
            Trace (Client, "WebSocket closing connection ...");
         end if;
         if Socket.Duplex then
            Client.Mutex.Set (Disabled);
         end if;
         begin
            if Client.Trace_Body then
               Trace (Client, "WebSocket finalizing ...");
            end if;
            WebSocket_Finalize (Client);
         exception
            when Error : others =>
               Trace
               (  Client,
                  (  "WebSocket finalization callback fault: "
                  &  Exception_Information (Error)
               )  );
         end;
         Set_Overlapped_Size (Client, 0); -- Disabe full-duplex mode
         Client.Stub := null;
         if Socket.Data /= null then
            declare
               type Message_Ptr is access WebSocket_Message;
               for Message_Ptr'Storage_Pool use Client.Pool;
               function To_Message_Ptr is
                  new Ada.Unchecked_Conversion
                      (  WebSocket_Message_Ptr,
                         Message_Ptr
                      );
               procedure Free is
                  new Ada.Unchecked_Deallocation
                      (  WebSocket_Message,
                         Message_Ptr
                      );
               Ptr : Message_Ptr := To_Message_Ptr (Socket.Data);
            begin
               Free (Ptr);
               Socket.Data := null;
            end;
         end if;
         Socket.State     := Closed_Socket;
         Socket.Pending   := False;
         Socket.Duplex    := False;
         Client.Expecting := Request_Line;
      end if;
   end WebSocket_Cleanup;

   procedure WebSocket_Close
             (  Client  : in out HTTP_Client;
                Status  : WebSocket_Status := WebSocket_Normal_Closure;
                Message : String := ""
             )  is
   begin
      if Message'Length > 123 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "WebSocket closing message is longer than 123 bytes"
         );
      elsif Client.WebSocket.State = Open_Socket then
         declare
            Header : constant Stream_Element_Array :=
                              (  2#1000_0000# or WebSocket_Close_Type,
                                 Stream_Element (Message'Length + 2),
                                 Stream_Element (Status / 256),
                                 Stream_Element (Status mod 256)
                              );
         begin
            if Client.WebSocket.Duplex then
               WebSocket_Send (Client, Header & From_String (Message));
               Client.Mutex.Set (Closing);
               Client.WebSocket.State := Closing_Socket;
            else
               Send_Content (Client, Header);
               Send_Content (Client, Message);
               if Queued_To_Send (Client) = 0 then
                  WebSocket_Cleanup (Client);
               else
                  Client.WebSocket.State := Closing_Socket;
               end if;
            end if;
         end;
      else
         Raise_Exception (End_Error'Identity, "No WebSocket open");
      end if;
   end WebSocket_Close;

   procedure WebSocket_Closed
             (  Client  : in out HTTP_Client;
                Status  : WebSocket_Status;
                Message : String
             )  is
   begin
      null;
   end WebSocket_Closed;

   procedure WebSocket_Error
             (  Client : in out HTTP_Client;
                Error  : Exception_Occurrence
             )  is
   begin
      null;
   end WebSocket_Error;

   procedure WebSocket_Initialize (Client : in out HTTP_Client) is
   begin
      null;
   end WebSocket_Initialize;

   function WebSocket_Header
            (  Length : Stream_Element_Count;
               Frame  : WebSocket_Frame_Type
            )  return Stream_Element_Array is
   begin
      if Length < 126 then
         return
         (  1 => 2#1000_0000# or Frame,
            2 => Stream_Element (Length)
         );
      elsif Length < 2**16 then
         return
         (  1 => 2#1000_0000# or Frame,
            2 => 126,
            3 => Stream_Element (Length / 256),
            4 => Stream_Element (Length mod 256)
         );
      else
         declare
            use Big_Endian.Unsigneds;
            Header  : Stream_Element_Array (1..10);
            Pointer : Stream_Element_Offset := 3;
         begin
            Header (1) := 2#1000_0000# or Frame;
            Header (2) := 127;
            Put (Header, Pointer, Unsigned_64 (Length));
            return Header;
         end;
      end if;
   end WebSocket_Header;

   function WebSocket_Open
            (  Client : access HTTP_Client
            )  return WebSocket_Accept is
   begin
      return
      (  Accepted => False,
         Code     => 501,
         Length   => Default_Response'Length,
         Reason   => Default_Response
      );
   end WebSocket_Open;

   procedure WebSocket_Received
             (  Client  : in out HTTP_Client;
                Message : Stream_Element_Array
             )  is
   begin
      null;
   end WebSocket_Received;

   procedure WebSocket_Received
             (  Client  : in out HTTP_Client;
                Message : String
             )  is
   begin
      null;
   end WebSocket_Received;

   procedure WebSocket_Received_Part
             (  Client  : in out HTTP_Client;
                Message : Stream_Element_Array
             )  is
   begin
      null;
   end WebSocket_Received_Part;

   procedure WebSocket_Received_Part
             (  Client  : in out HTTP_Client;
                Message : String
             )  is
   begin
      null;
   end WebSocket_Received_Part;

   procedure WebSocket_Send
             (  Client  : in out HTTP_Client;
                Message : Stream_Element_Array
             )  is
      Header : constant Stream_Element_Array :=
               WebSocket_Header (Message'Length, WebSocket_Binary_Type);
   begin
      if Client.WebSocket.Duplex then
         if Client.WebSocket.Context = Current_Task then
            if Client.WebSocket.State = Open_Socket then
               declare
                  Seized : Boolean;
               begin
                  Client.Mutex.Grab (Seized);
                  if Seized then
                     begin
                        Send_Content (Client, Header);
                        Send_Content (Client, Message);
                     exception
                        when others =>
                           Client.Mutex.Release;
                           raise;
                     end;
                  else
                     Queue_Content (Client, Header);
                     Queue_Content (Client, Message);
                  end if;
               end;
            else
               Raise_Exception
               (  End_Error'Identity,
                  "No WebSocket open"
               );
            end if;
         else
            WebSocket_Blocking_Send (Client, Header,  True, False);
            WebSocket_Blocking_Send (Client, Message, False, True);
         end if;
      else
         if Client.WebSocket.State = Open_Socket then
            Send_Content (Client, Header);
            Send_Content (Client, Message);
         else
            Raise_Exception (End_Error'Identity, "No WebSocket open");
         end if;
      end if;
   end WebSocket_Send;

   procedure WebSocket_Send
             (  Client  : in out HTTP_Client;
                Message : String
             )  is
      Header : constant Stream_Element_Array :=
               WebSocket_Header (Message'Length, WebSocket_Text_Type);
   begin
      if Client.WebSocket.Duplex then
         if Client.WebSocket.Context = Current_Task then
            if Client.WebSocket.State = Open_Socket then
               declare
                  Seized : Boolean;
               begin
                  Client.Mutex.Grab (Seized);
                  if Seized then
                     begin
                        Send_Content (Client, Header);
                        Send_Content (Client, Message);
                     exception
                        when others =>
                           Client.Mutex.Release;
                           raise;
                     end;
                  else
                     Queue_Content (Client, Header);
                     Queue_Content (Client, Message);
                  end if;
               end;
            else
               Raise_Exception
               (  End_Error'Identity,
                  "No WebSocket open"
               );
            end if;
         else
            WebSocket_Blocking_Send (Client, Header,  True, False);
            declare
               Pointer : Integer := Message'First;
            begin
               while Message'Last + 1 - Pointer > Block_Size loop
                  WebSocket_Blocking_Send
                  (  Client,
                     From_String
                     (  Message (Pointer..Pointer + Block_Size - 1)
                     ),
                     False,
                     False
                  );
                  Pointer := Pointer + Block_Size;
               end loop;
               WebSocket_Blocking_Send
               (  Client,
                  From_String (Message (Pointer..Message'Last)),
                  False,
                  True
               );
            end;
         end if;
      else
         if Client.WebSocket.State = Open_Socket then
            Send_Content (Client, Header);
            Send_Content (Client, Message);
         else
            Raise_Exception (End_Error'Identity, "No WebSocket open");
         end if;
      end if;
   end WebSocket_Send;

   procedure Write
             (  Client  : in out HTTP_Client;
                Factory : in out Connections_Factory'Class;
                Blocked : out Boolean
             )  is
   begin
      if Client.WebSocket.Duplex then
         declare
            Seized : Boolean;
         begin
            Client.Mutex.Grab (Seized);
            if Seized then
               begin
                  Write (Connection (Client), Factory, Blocked);
                  if Blocked or else Queued_To_Send (Client) = 0 then
                     Client.Mutex.Release;
                  end if;
               exception
                  when others =>
                     Client.Mutex.Release;
                     raise;
               end;
            else
               Blocked := False; -- Try later
            end if;
         end;
      else
         Write (Connection (Client), Factory, Blocked);
      end if;
   end Write;

   procedure Write
             (  Stream : in out Content_Stream;
                Item   : Stream_Element_Array
             )  is
   begin
      null;
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Content_Stream
             )  is
   begin
      null;
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Data_Pool
             )  is
   begin
      null;
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Content_Destination
             )  is
   begin
      null;
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : WebSocket_Data
             )  is
   begin
      null;
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Send_Mutex
             )  is
   begin
      null;
   end Write;

   protected body Send_Mutex is

      procedure Failed (Error : Exception_Occurrence) is
      begin
         Set_Failed (State_Machine (Client.all), Error);
         if State in Idle..Task_Sending then
            State := Server_Sending;
         end if;
      end Failed;

      function Get_Status return Duplex_Status is
      begin
         return State;
      end Get_Status;

      procedure Grab (Seized : out Boolean) is
      begin
         case State is
            when Disabled | Closing | Server_Sending =>
               Seized := True;
            when Idle =>
               State  := Server_Sending;
               Seized := True;
            when Task_Sending =>
               Seized := False;
         end case;
      end Grab;

      procedure Release is
      begin
         case State is
            when Disabled | Closing | Idle =>
               null;
            when Task_Sending | Server_Sending =>
               State := Idle;
         end case;
      end Release;

      entry Seize when State in Disabled..Idle is
      begin
         case State is
            when Idle =>
               State := Task_Sending;
            when Closing =>
               Raise_Exception
               (  End_Error'Identity,
                  "WebSocket is being closed"
               );
            when others =>
               Raise_Exception
               (  End_Error'Identity,
                  "No WebSocket open"
               );
         end case;
      end Seize;

      procedure Set (New_State : Duplex_Status) is
      begin
         State := New_State;
      end Set;

   end Send_Mutex;

begin
   Add (Commands, "CONNECT", HTTP_CONNECT);
   Add (Commands, "DELETE",  HTTP_DELETE);
   Add (Commands, "GET",     HTTP_GET);
   Add (Commands, "HEAD",    HTTP_HEAD);
   Add (Commands, "OPTIONS", HTTP_OPTIONS);
   Add (Commands, "PATCH",   HTTP_PATCH);
   Add (Commands, "POST",    HTTP_POST);
   Add (Commands, "PUT",     HTTP_PUT);
   Add (Commands, "TRACE",   HTTP_TRACE);

   Add (Request_Headers, "Accept",            Accept_Header);
   Add (Request_Headers, "Accept-Charset",    Accept_Charset_Header);
   Add (Request_Headers, "Accept-Encoding",   Accept_Encoding_Header);
   Add (Request_Headers, "Accept-Language",   Accept_Language_Header);
   Add (Request_Headers, "Accept-Datetime",   Accept_Datetime_Header);
   Add (Request_Headers, "Allow",             Allow_Header);
   Add (Request_Headers, "Authorization",     Authorization_Header);
   Add (Request_Headers, "Cache-Control",     Cache_Control_Header);
   Add (Request_Headers, "Cookie",            Cookie_Header);
   Add (Request_Headers, "Connection",        Connection_Header);
   Add (Request_Headers, "Content-Disposition",
                                            Content_Disposition_Header);
   Add (Request_Headers, "Content-Encoding",  Content_Encoding_Header);
   Add (Request_Headers, "Content-Language",  Content_Language_Header);
   Add (Request_Headers, "Content-Length",    Content_Length_Header);
   Add (Request_Headers, "Content-Location",  Content_Location_Header);
   Add (Request_Headers, "Content-MD5",       Content_MD5_Header);
   Add (Request_Headers, "Content-Type",      Content_Type_Header);
   Add (Request_Headers, "Date",              Date_Header);
   Add (Request_Headers, "Expect",            Expect_Header);
   Add (Request_Headers, "Expires",           Expires_Header);
   Add (Request_Headers, "From",              From_Header);
   Add (Request_Headers, "Host",              Host_Header);
   Add (Request_Headers, "If-Match",          If_Match_Header);
   Add (Request_Headers, "If-Modified-Since", If_Modified_Since_Header);
   Add (Request_Headers, "If-None-Match",     If_None_Match_Header);
   Add (Request_Headers, "If-Range",          If_Range_Header);
   Add (Request_Headers, "If-Unmodified-Since",
                                       If_Unmodified_Since_Header);
   Add (Request_Headers, "Last-Modified",     Last_Modified_Header);
   Add (Request_Headers, "Max-Forwards",      Max_Forwards_Header);
   Add (Request_Headers, "Origin",            Origin_Header);
   Add (Request_Headers, "Pragma",            Pragma_Header);
   Add (Request_Headers, "Proxy-Authorization",
                                       Proxy_Authorization_Header);
   Add (Request_Headers, "Range",             Range_Header);
   Add (Request_Headers, "Referer",           Referer_Header);
   Add (Request_Headers, "Sec-WebSocket-Extensions",
                                       Sec_WebSocket_Extensions_Header);
   Add (Request_Headers, "Sec-WebSocket-Key", Sec_WebSocket_Key_Header);
   Add (Request_Headers, "Sec-WebSocket-Protocol",
                                       Sec_WebSocket_Protocol_Header);
   Add (Request_Headers, "Sec-WebSocket-Version",
                                       Sec_WebSocket_Version_Header);
   Add (Request_Headers, "TE",                TE_Header);
   Add (Request_Headers, "Trailer",           Trailer_Header);
   Add (Request_Headers, "Transfer-Encoding", Transfer_Encoding_Header);
   Add (Request_Headers, "Upgrade",           Upgrade_Header);
   Add (Request_Headers, "User-Agent",        User_Agent_Header);
   Add (Request_Headers, "Via",               Via_Header);
   Add (Request_Headers, "Warning",           Warning_Header);

   Add (Week_Days, "Mon",       Monday);
   Add (Week_Days, "Monday",    Monday);
   Add (Week_Days, "Tue",       Tuesday);
   Add (Week_Days, "Tuesday",   Tuesday);
   Add (Week_Days, "Wed",       Wednesday);
   Add (Week_Days, "Wednesday", Wednesday);
   Add (Week_Days, "Thu",       Thursday);
   Add (Week_Days, "Thursday",  Thursday);
   Add (Week_Days, "Fri",       Friday);
   Add (Week_Days, "Friday",    Friday);
   Add (Week_Days, "Sat",       Saturday);
   Add (Week_Days, "Saturday",  Saturday);
   Add (Week_Days, "Sun",       Sunday);
   Add (Week_Days, "Sunday",    Sunday);

   Add (Months, "Jan", 1);
   Add (Months, "Feb", 2);
   Add (Months, "Mar", 3);
   Add (Months, "Apr", 4);
   Add (Months, "May", 5);
   Add (Months, "Jun", 6);
   Add (Months, "Jul", 7);
   Add (Months, "Aug", 8);
   Add (Months, "Sep", 9);
   Add (Months, "Oct", 10);
   Add (Months, "Nov", 11);
   Add (Months, "Dec", 12);

   Add (Zones, "UT",   0);
   Add (Zones, "GMT",  0);
   Add (Zones, "EDT", -4*60);
   Add (Zones, "EST", -5*60);
   Add (Zones, "CDT", -5*60);
   Add (Zones, "CST", -6*60);
   Add (Zones, "MDT", -6*60);
   Add (Zones, "MST", -7*60);
   Add (Zones, "PDT", -7*60);
   Add (Zones, "PST", -8*60);
   Add (Zones, "Z",    0);
   Add (Zones, "A",   -1*60);
   Add (Zones, "M",  -12*60);
   Add (Zones, "N",    1*60);
   Add (Zones, "Y",   12*60);

   Add (Connections, "close",      Connection_Close);
   Add (Connections, "keep-alive", Connection_Persistent);
   Add (Connections, "upgrade",    Connection_Upgrade);

   Add (Schemes, "http",  HTTP_Scheme);
   Add (Schemes, "https", HTTPS_Scheme);
   Add (Schemes, "ws",    WS_Scheme);
   Add (Schemes, "wss",   WSS_Scheme);

end GNAT.Sockets.Connection_State_Machine.HTTP_Server;
