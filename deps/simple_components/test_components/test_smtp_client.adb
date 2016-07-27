--                                                                    --
--  procedure Test_SMTP_Client      Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Summer, 2016       --
--                                                                    --
--                                Last revision :  12:48 19 Jun 2016  --
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

with Ada.Characters.Latin_1;       use Ada.Characters.Latin_1;
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;                  use Ada.Text_IO;
with GNAT.Sockets.SMTP;            use GNAT.Sockets.SMTP;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;
with Strings_Edit.Streams;         use Strings_Edit.Streams;

with Ada.Streams.Stream_IO;
with GNAT.Exception_Traces;
with GNAT.Sockets.Server.Secure.X509;
with GNAT.Sockets.SMTP.Client.Synchronous;

with Strings_Edit.Lexicographical_Order;
use  Strings_Edit.Lexicographical_Order;

procedure Test_SMTP_Client is
   use GNAT.Sockets.SMTP.Client;
   use GNAT.Sockets.SMTP.Client.Synchronous;

   Server_Address : constant String := "127.0.0.1";
   CRLF           : constant String  := CR & LF;

begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   declare
      Factory : aliased Connections_Factory;
      Server  : aliased Connections_Server (Factory'Access, 0);
   begin
      Put_Line ("SMTP synchronous client test started");
      Send
      (  Server   => Server,
         Host     => Server_Address,
         Message  => Create
                     (  From     => "<test@localhost>",
                        Subject  => "Synchronous client test",
                        To       => "<smpt4dev@localhost>",
                        Contents => "Hello all"
                     ),
         User     => "User",
         Password => "Password",
         Timeout  => 10.0
      );
      Put_Line ("SMTP synchronous client test completed");
   end;
   declare
      Factory : aliased Connections_Factory;
      Server  : aliased Connections_Server (Factory'Access, 0);
      Client  : Connection_Ptr :=
                   new SMTP_Client
                       (  Listener     => Server'Unchecked_Access,
                          Reply_Length => 1024,
                          Input_Size   => 80,
                          Output_Size  => 1024
                       );
   begin
      Put_Line ("SMTP asynchronous client test started");
      Set_Credentials (SMTP_Client (Client.all), "user", "password");
      Send
      (  SMTP_Client (Client.all),
         Create
         (  From     => "<test@localhost>",
            Subject  => "Sample",
            To       => "<smpt4dev@localhost>",
            Contents => "Hello all"
      )  );
      Connect (Server, Client, "127.0.0.1", SMTP_Port);
      delay 10.0; -- Don't know how long it would take
      Put_Line ("SMTP asynchronous client test started");
   end;
   declare
      Factory   : aliased Connections_Factory;
      Server    : aliased Connections_Server (Factory'Access, 0);
      Reference : Handle;
   begin
      Trace_On
      (  Factory  => Factory,
         Received => GNAT.Sockets.Server.Trace_Decoded,
         Sent     => GNAT.Sockets.Server.Trace_Decoded
      );
      Set
      (  Reference,
         new SMTP_Client
             (  Listener     => Server'Unchecked_Access,
                Reply_Length => 1024,
                Input_Size   => 80,
                Output_Size  => 1024 -- The maximum line length
                                     -- according to the standard
      )      );
      declare
         Client : SMTP_Client renames SMTP_Client (Ptr (Reference).all);

         procedure Test_1 is
            File : Ada.Streams.Stream_IO.File_Type;
         begin
            Set_Credentials (Client, "test", "pwd", SMTP_CRAM_MD5);
            Put_Line ("SMTP client test 1 started");
            Ada.Streams.Stream_IO.Open
            (  File,
               Ada.Streams.Stream_IO.In_File,
               "test_smtp_client.adb"
            );
            Send
            (  Client,
               Create
               (  From     => "<test@localhost>",
                  Subject  => "Test 1.1",
                  To       => "<smpt4dev@localhost>",
                  Contents => "Hello"
            )  );
            Send
            (  Client,
               Create
               (  From     => "<test@localhost>",
                  Subject  => "Test 1.2",
                  To       => "<smpt4dev@localhost>",
                  Contents => ".Muiltiline" & CRLF & ".test" & CRLF &
                              "with dots"
            )  );
            declare
               Message : Mail :=
                         Create
                         (  From     => "<test@localhost>",
                            Subject  => "Test 1.3",
                            To       => "<smpt4dev@localhost>"
                         );
            begin
               Attach_Stream
               (  Message,
                  Ada.Streams.Stream_IO.Stream (File).all
               );
               Send (Client, Message);
            end;
            Connect
            (  Server,
               Client'Unchecked_Access,
               Server_Address,
               SMTP_Port
            );
            while not Is_Down (Client) loop -- Busy waiting
               delay 0.1;
            end loop;
            Ada.Streams.Stream_IO.Close (File);
            Put_Line ("Test 1. SMTP client finished");
         end Test_1;

         procedure Test_2 is
            Stream  : aliased String_Stream (1024 * 4);
            Message : Mail := Create
                              (  From     => "<test@localhost>",
                                 Subject  => "Test 2",
                                 To       => "<smpt4dev@localhost>",
                                 Contents => "See attachments."
                              );
         begin
            Put_Line ("SMTP client test 2 started");
            Set
            (  Stream,
               (  "Discussion: All of the primitive function "
               &  "attributes except Rounding and Machine "
               &  "correspond to subprograms in the "
               &  "Generic_Primitive_Functions generic package "
               &  "proposed as a separate ISO standard (ISO/IEC "
               &  "DIS 11729) for Ada 83. The Scaling, "
               &  "Unbiased_Rounding, and Truncation attributes "
               &  "correspond to the Scale, Round, and Truncate "
               &  "functions, respectively, in "
               &  "Generic_Primitive_Functions. The Rounding "
               &  "attribute rounds away from zero; this "
               &  "functionality was not provided in "
               &  "Generic_Primitive_Functions. The name Round "
               &  "was not available for either of the primitive "
               &  "function attributes that perform rounding, "
               &  "since an attribute of that name is used for "
               &  "a different purpose for decimal fixed point "
               &  "types. Likewise, the name Scale was not "
               &  "available, since an attribute of that name is "
               &  "also used for a different purpose for decimal "
               &  "fixed point types. The functionality of the "
               &  "Machine attribute was also not provided in "
               &  "Generic_Primitive_Functions. The functionality "
               &  "of the Decompose procedure of "
               &  "Generic_Primitive_Functions is only provided in "
               &  "the form of the separate attributes Exponent "
               &  "and Fraction. The functionality of the "
               &  "Successor and Predecessor functions of "
               &  "Generic_Primitive_Functions is provided by "
               &  "the extension of the existing Succ and Pred "
               &  "attributes."
            )  );
            Rewind (Stream);
            Attach_File
            (  Message     => Message,
               Disposition => "test_smtp_client.adb",
               Contents    => "test_smtp_client.adb"
            );
            Attach_Stream
            (  Message     => Message,
               Disposition => "text.txt",
               Contents    => Stream
            );
            Send (Client, Message);
            Connect
            (  Server,
               Client'Unchecked_Access,
               Server_Address,
               SMTP_Port
            );
            while not Is_Down (Client) loop -- Busy waiting
               delay 0.1;
            end loop;
            Put_Line ("Test 2. SMTP client finished");
         end Test_2;

         procedure Test_3 is
            Stream  : aliased String_Stream (100);
            Message : Mail := Create
                              (  From     => "<test@localhost>",
                                 Subject  => "Test 3",
                                 To       => "<smpt4dev@localhost>",
                                 Contents => "See attachments."
                              );
         begin
            Put_Line ("SMTP client test 3 started");
            Set (Stream, "Test stream");
            Attach_Stream
            (  Message     => Message,
               Disposition => "text.txt",
               Contents    => Stream
            );
            Set_Credentials
            (  Client,
               "mailbox@dmitry-kazakov.de",
               "015ztgnQ?4kPyuv",
               SMTP_CRAM_MD5
            );
            Send (Client, Message);
            Connect
            (  Server,
               Client'Unchecked_Access,
               Server_Address,
               SMTP_Port
            );
            while not Is_Down (Client) loop -- Busy waiting
               delay 0.1;
            end loop;
            Put_Line ("Test 3. SMTP client finished");
         end Test_3;
      begin
         Set_Enhanced (Client, True);
         Test_1;
         Test_2;
         Test_3;
      end;
   end;
   declare
      use GNAT.Sockets.Server.Secure.X509;
      Factory   : aliased X509_Authentication_Factory
                          (  Decoded_Size => 1024
                          );
      Server    : aliased Connections_Server (Factory'Access, 0);
      Reference : Handle;
   begin
      Add_System_Trust (Factory);
      Add_Key_From_PEM_File
      (  Factory          => Factory,
         Certificate_File => "test.cert.pem",
         Key_File         => "test.key.pem"
      );
      Add_Key_From_PEM_File
      (  Factory          => Factory,
         Certificate_File => "test.cert.pem",
         Key_File         => "test.key.pem"
      );
      Generate_Diffie_Hellman_Parameters (Factory);
      Trace_On
      (  Factory  => Factory,
         Received => GNAT.Sockets.Server.Trace_Any,
         Sent     => GNAT.Sockets.Server.Trace_Any
      );
      Set_TLS_Tracing
      (  Factory => Factory,
         Session => True,
         Decoded => True
      );
      Set
      (  Reference,
         new SMTP_Client
             (  Listener     => Server'Unchecked_Access,
                Reply_Length => 1024,
                Input_Size   => 80,
                Output_Size  => 1024 -- The maximum line length
                                     -- according to the standard
      )      );
      declare
         Client : SMTP_Client renames SMTP_Client (Ptr (Reference).all);

         procedure Test_4 is
            Stream  : aliased String_Stream (100);
            Message : Mail :=
               Create
               (  From     => "<some mail>",
                  Subject  => "Test 4",
                  To       => "<some mail>",
                  Contents => "See attachments."
               );
         begin
            Put_Line ("SMTP client test 4 started");
            Set (Stream, "Test stream");
            Attach_Stream
            (  Message     => Message,
               Disposition => "text.txt",
               Contents    => Stream
            );
            Set_Credentials
            (  Client,
               "username",
               "password",
               SMTP_CRAM_MD5
            );
            Send (Client, Message);
            Connect
            (  Server,
               Client'Unchecked_Access,
               Server_Address,
               SMTP_Port
            );
            while not Is_Down (Client) loop -- Busy waiting
               delay 0.1;
            end loop;
            Put_Line ("Test 4. SMTP client finished");
         end Test_4;
      begin
         Set_Enhanced (Client, True);
         Test_4;
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_SMTP_Client;
