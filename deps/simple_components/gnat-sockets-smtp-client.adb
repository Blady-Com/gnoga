--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.SMTP.Client                    Luebeck            --
--  Implementation                                 Summer, 2016       --
--                                                                    --
--                                Last revision :  14:07 11 Nov 2019  --
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
--  this library; if not, write _  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Characters.Latin_1;      use Ada.Characters.Latin_1;
with Ada.Exceptions;              use Ada.Exceptions;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Ada.Strings.Maps.Constants;  use Ada.Strings.Maps.Constants;
with Strings_Edit;                use Strings_Edit;
with Strings_Edit.Base64;         use Strings_Edit.Base64;
with Strings_Edit.Integers;       use Strings_Edit.Integers;
with Strings_Edit.Quoted;         use Strings_Edit.Quoted;
with Strings_Edit.UTF8.Mapping;   use Strings_Edit.UTF8.Mapping;

with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Deallocation;
with GNAT.MD5;

package body GNAT.Sockets.SMTP.Client is
   use Mail_Handles;
   use Object;

   Opad           : constant Stream_Element := 16#5C#;
   Ipad           : constant Stream_Element := 16#36#;
   Encoded_Length : constant := 72;
   Chunk_Length   : constant := (Encoded_Length * 3 + 3) / 4;
   CRLF           : constant String := CR & LF;
   Colon          : constant String := ":" ;
   Dot            : constant String := "." ;
   LF_Dot         : constant String := LF & ".";
   Data_End       : constant String := CRLF & "." & CRLF;
   Multipart_End  : constant String := "--" & CRLF & Data_End;
   Boundary       : constant String :=   -- 1234567890123456
                             CRLF & CRLF & "--[message part]" & CRLF;

   package Random_UID is
      new Ada.Numerics.Discrete_Random (Interfaces.Unsigned_64);

   Dice : Random_UID.Generator;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Attachment_Object'Class,
             Attachment_Object_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Root_Stream_Type'Class,
             Stream_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

--     function "abs" (Name : String) return String is
--        Length : Natural := 0;
--     begin
--        for Index in Name'Range loop
--           case Name (Index) is
--              when 'A'..'Z' | 'a'..'z' | '0'..'9' |
--                   '-' | '_' | '.' | '!' | '~' | '*' | ''' | '(' | ')' =>
--                 Length := Length + 1;
--              when others =>
--                 Length := Length + 3;
--           end case;
--        end loop;
--        declare
--           Pointer : Integer := 1;
--           Result  : String (1..Length);
--        begin
--           for Index in Name'Range loop
--              case Name (Index) is
--                 when 'A'..'Z' | 'a'..'z' | '0'..'9' |
--                      '-' | '_' | '.' | '!' | '~' | '*' |
--                      ''' | '(' | ')' =>
--                    Result (Pointer) := Name (Index);
--                    Pointer := Pointer + 1;
--                 when others =>
--                    Result (Pointer) := '%';
--                    Pointer := Pointer + 1;
--                    Strings_Edit.Integers.Put
--                    (  Destination => Result,
--                       Pointer     => Pointer,
--                       Value       => Character'Pos (Name (Index)),
--                       Base        => 16,
--                       Field       => 2,
--                       Justify     => Right,
--                       Fill        => '0'
--                    );
--              end case;
--           end loop;
--           return Result;
--        end;
--     end "abs";

   procedure Add_Address
             (  List    : in out Mail_Address_List;
                Address : String
             )  is
      use Persistent.Catalogue;
      Text : constant String := Trim (Address);
   begin
      if Text'Length > 0 then
         Add (List.Set, Address);
      end if;
   end Add_Address;

   procedure Append
             (  Object     : in out Mail_Object;
                Attachment : Attachment_Object_Ptr
             )  is
   begin
      if Object.First = null then
         Object.First := Attachment;
      else
         Object.Last.Next := Attachment;
      end if;
      Object.Last := Attachment;
   end Append;

   procedure Attach_File
             (  Message      : in out Mail;
                Contents     : String;
                Content_Type : String := "text/plain";
                Disposition  : String := "";
                Description  : String := ""
             )  is
      Attachment : Attachment_Object_Ptr;
   begin
      if not Is_Valid (Message.Reference) then
         Raise_Exception
         (  Use_Error'Identity,
            "Attaching to an uninitialized mail message"
         );
      end if;
      declare
         Object : Mail_Object'Class renames Ptr (Message.Reference).all;
         Header : constant String :=
                           Get_Content_Type (Content_Type) &
                           Get_Content_Disposition (Disposition) &
                           Get_Content_Description (Description) &
                           "Content-Transfer-Encoding: base64" &
                           CRLF & CRLF;
      begin
         Set_MIME (Object);
         Attachment :=
            new Attachment_File
                (  Name_Length   => Contents'Length,
                   Header_Length => Header'Length
                );
         Attachment.Header := Header;
         declare
            This : Attachment_File renames
                   Attachment_File (Attachment.all);
         begin
            This.Name := Contents;
            Stream_IO.Open (This.File, Stream_IO.In_File, Contents);
            This.Open := True;
         exception
            when others =>
               Free (Attachment);
               raise;
         end;
         Append (Object, Attachment);
      end;
   end Attach_File;

   procedure Attach_Stream
             (  Message      : in out Mail;
                Contents     : access Root_Stream_Type'Class;
                Content_Type : String := "text/plain";
                Disposition  : String := "";
                Description  : String := ""
             )  is
   begin
      if not Is_Valid (Message.Reference) then
         Raise_Exception
         (  Use_Error'Identity,
            "Attaching to an uninitialized mail message"
         );
      end if;
      declare
         Object : Mail_Object'Class renames Ptr (Message.Reference).all;
      begin
         Set_MIME (Object);
         Append
         (  Object,
            Create_Stream_Attachment
            (  Contents     => Contents.all'Unchecked_Access,
               Owned        => True,
               Content_Type => Content_Type,
               Disposition  => Disposition,
               Description  => Description
         )  );
      end;
   end Attach_Stream;

   procedure Attach_Stream
             (  Message      : in out Mail;
                Contents     : in out Root_Stream_Type'Class;
                Content_Type : String := "text/plain";
                Disposition  : String := "";
                Description  : String := ""
             )  is
   begin
      if not Is_Valid (Message.Reference) then
         Raise_Exception
         (  Use_Error'Identity,
            "Attaching to an uninitialized mail message"
         );
      end if;
      declare
         Object : Mail_Object'Class renames Ptr (Message.Reference).all;
      begin
         Set_MIME (Object);
         Append
         (  Object,
            Create_Stream_Attachment
            (  Contents     => Contents'Unchecked_Access,
               Owned        => False,
               Content_Type => Content_Type,
               Disposition  => Disposition,
               Description  => Description
         )  );
      end;
   end Attach_Stream;

   procedure Attach_String
             (  Message      : in out Mail;
                Contents     : String;
                Content_Type : String := "text/plain";
                Disposition  : String := "";
                Description  : String := ""
             )  is
   begin
      if not Is_Valid (Message.Reference) then
         Raise_Exception
         (  Use_Error'Identity,
            "Attaching to an uninitialized mail message"
         );
      end if;
      declare
         Object : Mail_Object'Class renames Ptr (Message.Reference).all;
      begin
         Set_MIME (Object);
         Append
         (  Object,
            Create_String_Attachment
            (  Contents     => Contents,
               Content_Type => Content_Type,
               Disposition  => Disposition,
               Description  => Description
         )  );
      end;
   end Attach_String;

   procedure Check (Message : Mail) is
   begin
      if not Is_Valid (Message.Reference) then
         Raise_Exception (Use_Error'Identity, "Empty mail message");
      end if;
      declare
         Object : Mail_Object'Class renames
                  Ptr (Message.Reference).all;
      begin
         if Object.Next /= null then
            Raise_Exception
            (  Use_Error'Identity,
               "The mail message is already queued"
            );
         elsif (  (  Object.Contents = null
                  or else
                     Object.Contents'Length = 0
                  )
               and then
                  Object.First = null
               )  then
            Raise_Exception
            (  Use_Error'Identity,
               "Missing message body"
            );
         elsif (  Object.Texts (Mail_From) = null
               or else
                  Object.Texts (Mail_From)'Length = 0
               )  then
            Raise_Exception
            (  Use_Error'Identity,
               "Missing sender's address"
            );
         elsif (  Object.Texts (Mail_Subject) = null
               or else
                  Object.Texts (Mail_Subject)'Length = 0
               )  then
            Raise_Exception (Use_Error'Identity, "Missing subject");
         elsif Is_Empty (Object.Addresses (Mail_To)) then
            Raise_Exception
            (  Use_Error'Identity,
               "Missing recipient's address"
            );
         end if;
      end;
   end Check;

   procedure Close (Attachment : in out Attachment_Object) is
   begin
      null;
   end Close;

   procedure Close (Attachment : in out Attachment_File) is
   begin
      if Attachment.Open then
         Attachment.Open := False;
         begin
            Stream_IO.Close (Attachment.File);
         exception
            when others =>
               null;
         end;
      end if;
   end Close;

   procedure Completed (Client : in out SMTP_Client) is
   begin
      null;
   end Completed;

   procedure Connected (Client : in out SMTP_Client) is
   begin
      Client.Command        := SMTP_Greeting;
      Client.Receive_State  := Receive_Code;
      Client.Send_State     := Send_Command;
      Client.Count          := 0;
      Client.Authentication := 0;
      Client.Mail_Size      := Natural'Last;
      Client.Offered_TLS    := False;
      Client.Extensions     := (others => False);
      Connected (Connection (Client));
      Connected (Connection (Client));
   end Connected;

   function CRAM_MD5 (User, Password, Challenge : String)
      return String is
      use GNAT.MD5;

      function "xor" (Text : String; Pad : Stream_Element)
         return String is
         Result : String (Text'Range);
      begin
         for Index in Result'Range loop
            Result (Index) :=
               Character'Val (Character'Pos (Text (Index)) xor Pad);
         end loop;
         return Result;
      end "xor";

      Secret : String := (1..64 => NUL);
   begin
      if Password'Length <= Secret'Length then
         Secret (1..Password'Length) := Password;
      else
         Secret (1..16) := To_String (Digest (Password));
      end if;
      return To_Base64
             (  User
             &  ' '
             &  Digest
                (  (Secret xor Opad)
                &  To_String (Digest ((Secret xor Ipad) & Challenge))
             )  );
   end CRAM_MD5;

   function Cnonce return String is
      use Interfaces;
      use Random_UID;
      use GNAT.MD5;
      Value  : constant Unsigned_64 := Random (Dice);
      Result : constant String :=
                  (  1 => Character'Val (          Value mod 2**8),
                     2 => Character'Val ((Value / 2**8 ) mod 2**8),
                     3 => Character'Val ((Value / 2**16) mod 2**8),
                     4 => Character'Val ((Value / 2**24) mod 2**8),
                     5 => Character'Val ((Value / 2**32) mod 2**8),
                     6 => Character'Val ((Value / 2**40) mod 2**8),
                     7 => Character'Val ((Value / 2**48) mod 2**8),
                     8 => Character'Val ( Value / 2**56)
                  );
      Encode : constant String := Digest (Result);
   begin
      return Encode (Encode'First..Encode'First + 13);
   end Cnonce;

   function DIGEST_MD5
            (  Host      : String;
               User      : String;
               Password  : String;
               Challenge : String;
               Request   : String;
               Cnonce    : String
            )  return String is
      use GNAT.MD5;
      Method  : constant String := "AUTHENTICATE";
      QOP     : constant String := "auth";
      Nc      : constant String := "00000001";

      Pointer : Integer := Challenge'First;
      Topic   : SMTP_Challenge;
      From    : array (SMTP_Challenge) of Integer :=
                   (others => Challenge'First);
      To      : array (SMTP_Challenge) of Integer :=
                   (others => Challenge'First - 1);

      function Nonce return String is
      begin
         return Challenge (From (SMTP_NONCE)..To (SMTP_NONCE));
      end Nonce;

      function Realm return String is
      begin
         if From (SMTP_REALM) <= To (SMTP_REALM) then
            return Challenge (From (SMTP_REALM)..To (SMTP_REALM));
         else
            return Host;
         end if;
      end Realm;

      function URI return String is
      begin
         return Request & '/' & Realm;
      end URI;

      function A1 return String is
      begin
         if (  To_Lowercase
               (  Challenge
                  (  From (SMTP_ALGORITHM)
                  .. To (SMTP_ALGORITHM)
               )  )
            =  "md5-sess"
            )
         then
            return
            (  To_String (Digest (User & ':' & Realm & ':' & Password))
            &  ':'
            &  Nonce
            &  ':'
            &  Cnonce
            );
         else
            return User & ':' & Realm & ':' & Password;
         end if;
      end A1;

      function A2 return String is
         QOP : constant String :=
                  To_Lowercase
                  (  Challenge (From (SMTP_QOP)..To (SMTP_QOP))
                  );
      begin
         if QOP = "auth-int" or else QOP = "auth-conf" then
            return
            (  Method
            &  ':'
            &  URI
            &  ":00000000000000000000000000000000"
           );
         else
            return Method & ':' & URI;
         end if;
      end A2;

   begin
      while Pointer <= Challenge'Last loop
         Get (Challenge, Pointer);
         exit when Pointer > Challenge'Last;
         declare
            Got_It : Boolean;
         begin
            Get (Challenge, Pointer, Challenges, Topic, Got_It);
            if not Got_It then
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid digest-md5 challenge, unsupported " &
                  "topic at " &
                  Challenge (Pointer..Challenge'Last)
               );
            end if;
         end;
         Get (Challenge, Pointer);
         if Pointer > Challenge'Last or else Challenge (Pointer) /= '='
         then
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid digest-md5 challenge, '=' is expected after " &
               "a keyword at " &
               Challenge (Pointer..Challenge'Last)
            );
         end if;
         Pointer := Pointer + 1;
         Get (Challenge, Pointer);
         if Pointer > Challenge'Last then
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid digest-md5 challenge, topic value is " &
               "expected after equality sign at " &
               Challenge (Pointer..Challenge'Last)
            );
         end if;
         if Challenge (Pointer) = '"' then
            Pointer := Pointer + 1;
            From (Topic) := Pointer;
            while Pointer <= Challenge'Last loop
               if Challenge (Pointer) = '"' then
                  To (Topic) := Pointer - 1;
                  exit;
               end if;
               Pointer := Pointer + 1;
            end loop;
            if Pointer > Challenge'Last then
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid digest-md5 challenge, missing closing " &
                  "quotation marks"
               );
            end if;
            Pointer := Pointer + 1;
         else
            From (Topic) := Pointer;
            loop
               if Pointer > Challenge'Last then
                  To (Topic) := Pointer - 1;
                  exit;
               end if;
               case Challenge (Pointer) is
                  when ' ' | ',' | Character'Val (9) =>
                     To (Topic) := Pointer - 1;
                     exit;
                  when others =>
                     Pointer := Pointer + 1;
               end case;
            end loop;
         end if;
         Get (Challenge, Pointer);
         exit when Pointer > Challenge'Last;
         if Challenge (Pointer) /= ',' then
            Raise_Exception
            (  Data_Error'Identity,
               "Invalid digest-md5 challenge, ',' is expected at " &
               Challenge (Pointer..Challenge'Last)
            );
         end if;
         Pointer := Pointer + 1;
      end loop;
      declare
         Response : constant String :=
                       Digest
                       (  Digest (A1)
                       &  ':'
                       &  Nonce
                       &  ':'
                       &  Nc
                       &  ':'
                       &  Cnonce
                       &  ':'
                       &  QOP
                       &  ':'
                       &  Digest (A2)
                       );
      begin
         declare
            Decoded : constant String :=
               (  "charset=utf-8,"
               &  "username="""
               &  User
               &  """,realm="""
               &  Realm
               &  """,nonce="""
               &  Nonce
               &  """,nc="
               &  Nc
               &  ",cnonce="""
               &  Cnonce
               &  """,digest-uri="""
               &  URI
               &  """,response="
               &  Response
               &  ",qop="
               &  QOP
               );
         begin
            return To_Base64 (Decoded);
         end;
      end;
   end DIGEST_MD5;

   function Create
            (  From     : String;
               Subject  : String;
               To       : Mail_Address_List'Class;
               Contents : String;
               Cc       : Mail_Address_List'Class := Empty;
               Bcc      : Mail_Address_List'Class := Empty;
               Date     : Time   := Clock;
               MIME     : String := "text/plain"
            )  return Mail is
      Result : Mail;
   begin
      Set (Result.Reference, new Mail_Object);
      declare
         Object : Mail_Object'Class renames Ptr (Result.Reference).all;
      begin
         Object.Texts (Mail_From)        := new String'(Trim (From));
         Object.Addresses (Mail_To).Set  := To.Set;
         Object.Addresses (Mail_Cc).Set  := Cc.Set;
         Object.Addresses (Mail_Bcc).Set := Bcc.Set;
         Object.Texts (Mail_Subject)     := new String'(Subject);
         Object.Dates (Mail_Date)        := Date;
         if MIME'Length > 0 then
            Set_MIME (Object);
            Prepend (Object, Create_String_Attachment (Contents, MIME));
         else
            Object.Contents := new String'(Contents);
         end if;
      end;
      return Result;
   end Create;

   function Create
            (  From     : String;
               Subject  : String;
               To       : String;
               Contents : String;
               Cc       : Mail_Address_List'Class := Empty;
               Bcc      : Mail_Address_List'Class := Empty;
               Date     : Time   := Clock;
               MIME     : String := "text/plain"
            )  return Mail is
   begin
      return Create
             (  From     => From,
                Subject  => Subject,
                To       => Empty / To,
                Contents => Contents,
                Cc       => Cc,
                Bcc      => Bcc,
                Date     => Date,
                MIME     => MIME
             );
   end Create;

   function Create
            (  From     : String;
               Subject  : String;
               To       : Mail_Address_List'Class;
               Cc       : Mail_Address_List'Class := Empty;
               Bcc      : Mail_Address_List'Class := Empty;
               Date     : Time := Clock
            )  return Mail is
      Result : Mail;
   begin
      Set (Result.Reference, new Mail_Object);
      declare
         Object : Mail_Object'Class renames Ptr (Result.Reference).all;
      begin
         Object.Texts (Mail_From)        := new String'(Trim (From));
         Object.Addresses (Mail_To).Set  := To.Set;
         Object.Addresses (Mail_Cc).Set  := Cc.Set;
         Object.Addresses (Mail_Bcc).Set := Bcc.Set;
         Object.Texts (Mail_Subject)     := new String'(Subject);
         Object.Dates (Mail_Date)        := Date;
      end;
      return Result;
   end Create;

   function Create
            (  From     : String;
               Subject  : String;
               To       : String;
               Cc       : Mail_Address_List'Class := Empty;
               Bcc      : Mail_Address_List'Class := Empty;
               Date     : Time := Clock
            )  return Mail is
   begin
      return Create
             (  From     => From,
                Subject  => Subject,
                To       => Empty / To,
                Cc       => Cc,
                Bcc      => Bcc,
                Date     => Date
             );
   end Create;

   function Create_Stream_Attachment
            (  Contents     : Stream_Ptr;
               Owned        : Boolean;
               Content_Type : String := "text/plain";
               Disposition  : String := "";
               Description  : String := ""
            )  return Attachment_Object_Ptr is
      Result : Attachment_Object_Ptr;
      Header : constant String :=
                        Get_Content_Type (Content_Type) &
                        Get_Content_Disposition (Disposition) &
                        Get_Content_Description (Description) &
                        "Content-Transfer-Encoding: base64" &
                        CRLF & CRLF;
   begin
      Result := new Attachment_Stream (Header'Length);
      Result.Header := Header;
      declare
         Object : Attachment_Stream renames
                  Attachment_Stream (Result.all);
      begin
         Object.Owned  := Owned;
         Object.Stream := Contents;
      end;
      return Result;
   end Create_Stream_Attachment;

   function Create_String_Attachment
            (  Contents     : String;
               Content_Type : String := "text/plain";
               Disposition  : String := "";
               Description  : String := ""
            )  return Attachment_Object_Ptr is
      Result : Attachment_Object_Ptr;
      Header : constant String :=
                        Get_Content_Type (Content_Type) &
                        Get_Content_Disposition (Disposition) &
                        Get_Content_Description (Description) &
                        "Content-Transfer-Encoding: base64" &
                        CRLF & CRLF;
   begin
      Result := new Attachment_String
                    (  Length        => Contents'Length,
                       Header_Length => Header'Length
                    );
      Result.Header := Header;
      Attachment_String (Result.all).Buffer := Contents;
      return Result;
   end Create_String_Attachment;

   procedure Delete (Message : in out Mail_Object_Ptr) is
   begin
      if Message /= null then
         declare
            This  : Mail_Object_Ptr  := Message;
            Empty : constant Boolean := This.Next = This;
         begin
            if Empty then -- Empty queue
               Message := null;
            else
               Message := This.Next;
            end if;
            This.Previous.Next := This.Next;
            This.Next.Previous := This.Previous;
            This.Previous      := null;
            This.Next          := null;
            Release (Entity_Ptr (This));
         end;
      end if;
   end Delete;

   procedure Disconnected (Client : in out SMTP_Client) is
      This  : Mail_Object_Ptr := Client.Queue;
      Count : Natural := 0;
   begin
      if This /= null then
         loop
            Count := Count + 1;
            This  := This.Next;
            exit when This = Client.Queue;
         end loop;
         declare
            Unsent : Mail_Array (1..Count);
         begin
            for Index in Unsent'Range loop
               Unsent (Index) := (Reference => Ref (Client.Queue));
               Delete (Client.Queue);
            end loop;
            Send_Abandoned (SMTP_Client'Class (Client), Unsent);
         end;
      end if;
      Disconnected (Connection (Client));
   end Disconnected;

   procedure Do_Quit (Client : in out SMTP_Client'Class) is
   begin
      Client.Command       := SMTP_QUIT;
      Client.Send_State    := Send_Command;
      Client.Receive_State := Receive_Code;
      Client.Count         := 0;
      Send_Text (Client, "QUIT" & CRLF);
   end Do_Quit;

   procedure Elevated (Client : in out SMTP_Client) is
   begin
      Client.Command := SMTP_EHLO;
      Send_Text
      (  Client,
         (  "EHLO "
         &  Official_Name (Get_Host_By_Name (Host_Name))
         &  CRLF
      )  );
   end Elevated;

   function Empty return Mail_Address_List is
      Result : Mail_Address_List;
   begin
      return Result;
   end Empty;

   procedure Erase (Object : in out Mail_Object) is
   begin
      Object.Status := Mail_Pending;
      for Index in Object.Texts'Range loop
         Free (Object.Texts (Index));
      end loop;
      for Index in Object.Addresses'Range loop
         Object.Addresses (Index) := Empty;
      end loop;
      Free (Object.Contents);
      Object.Last := null;
      while Object.First /= null loop
         declare
            This : Attachment_Object_Ptr := Object.First;
         begin
            Object.First := This.Next;
            Free (This);
         end;
      end loop;
   end Erase;

   procedure Erase (Message : in out Mail) is
   begin
      if Is_Valid (Message.Reference) then
         Erase (Ptr (Message.Reference).all);
      end if;
   end Erase;

   procedure Finalize (Object : in out Mail_Object) is
   begin
      if Object.Next /= null then
         Raise_Exception
         (  Program_Error'Identity,
            "Premature destruction of a queued mail object"
         );
      end if;
      Erase (Object);
   end Finalize;

   procedure Finalize (Attachment : in out Attachment_Stream) is
   begin
      if Attachment.Owned then
         Free (Attachment.Stream);
      end if;
      Finalize (Attachment_Object (Attachment));
   end Finalize;

   procedure Finalize (Attachment : in out Attachment_File) is
   begin
      Close (Attachment);
      Finalize (Attachment_Object (Attachment));
   end Finalize;

   procedure Finalize (Client : in out SMTP_Client) is
   begin
      while Client.Queue /= null loop
         Delete (Client.Queue);
      end loop;
      Free (Client.Encoded);
      Free (Client.User);
      Free (Client.Password);
      Finalize (Connection (Client));
   end Finalize;

   function Find
            (  List    : Mail_Address_List;
               Address : String
            )  return Natural is
      use Persistent.Catalogue;
   begin
      return Find (List.Set, Trim (Address));
   end Find;

   function From_String (List : String) return Mail_Address_List is
      Result : Mail_Address_List;
      Start  : Integer := List'First;
   begin
      for Index in List'Range loop
         if List (Index) = ',' then
            Add_Address (Result, List (Start..Index - 1));
            Start := Index + 1;
         end if;
      end loop;
      Add_Address (Result, List (Start..List'Last));
      return Result;
   end From_String;

   function Get
            (  Message : Mail_Object'Class;
               Header  : Text_Header
            )  return String is
   begin
      if Message.Texts (Header) = null then
         return "";
      else
         return Message.Texts (Header).all;
      end if;
   end Get;

   function Get
            (  Message : Mail_Object'Class;
               Header  : List_Header
            )  return Mail_Address_List is
   begin
      return (Set => Message.Addresses (Header).Set);
   end Get;

   function Get
            (  Message : Mail_Object'Class;
               Header  : Date_Header
            )  return Time is
   begin
      return Message.Dates (Header);
   end Get;

   function Get
            (  Message : Mail;
               Header  : Text_Header
            )  return String is
   begin
      if not Is_Valid (Message.Reference) then
         return "";
      else
         return Get (Ptr (Message.Reference).all, Header);
      end if;
   end Get;

   function Get
            (  Message : Mail;
               Header  : List_Header
            )  return Mail_Address_List is
   begin
      if not Is_Valid (Message.Reference) then
         return Empty;
      else
         return Get (Ptr (Message.Reference).all, Header);
      end if;
   end Get;

   function Get
            (  Message : Mail;
               Header  : Date_Header
            )  return Time is
   begin
      if Is_Valid (Message.Reference) then
         return Get (Ptr (Message.Reference).all, Header);
      else
         Raise_Exception
         (  Constraint_Error'Identity,
            "Uninitialized object"
         );
      end if;
   end Get;

   function Get_Address
            (  List  : Mail_Address_List;
               Index : Positive
            )  return String is
      use Persistent.Catalogue;
   begin
      return Get (List.Set, Index);
   end Get_Address;

   function Get_Accepted
            (  Client : SMTP_Client
            )  return SMTP_AUTH_Mechanism is
   begin
      return Client.Supported;
   end Get_Accepted;

   function Get_Authentication
            (  Client : SMTP_Client
            )  return SMTP_AUTH_Mechanism is
   begin
      return Client.Authentication;
   end Get_Authentication;

   function Get_Content_Type (Value : String) return String is
   begin
      if Value'Length = 0 then
         return "";
      else
         return "Content-Type: " & Value & CRLF;
      end if;
   end Get_Content_Type;

   function Get_Content_Disposition (Value : String) return String is
   begin
      if Value'Length = 0 then
         return "";
      else
         return "Content-Disposition: attachment; " & Value & CRLF;
      end if;
   end Get_Content_Disposition;

   function Get_Content_Description (Value : String) return String is
   begin
      if Value'Length = 0 then
         return "";
      else
         return "Content-Description: " & Value & CRLF;
      end if;
   end Get_Content_Description;

   function Get_Enhanced (Client : SMTP_Client) return Boolean is
   begin
      return Client.Force_ESMTP;
   end Get_Enhanced;

   function Get_Extension
            (  Client    : SMTP_Client;
               Extension : SMTP_Extension
            )  return Boolean is
   begin
      return Client.Extensions (Extension);
   end Get_Extension;

   function Get_Length (List : Mail_Address_List) return Natural is
      use Persistent.Catalogue;
   begin
      return Get_Size (List.Set);
   end Get_Length;

   function Get_Mail_Size (Client : SMTP_Client) return Natural is
   begin
      return Client.Mail_Size;
   end Get_Mail_Size;

   function Get_Password (Client : SMTP_Client) return String is
   begin
      return Value (Client.Password);
   end Get_Password;

   function Get_Status (Message : Mail) return Mail_Status is
   begin
      if Is_Valid (Message.Reference) then
         return Ptr (Message.Reference).Status;
      else
         return Mail_Pending;
      end if;
   end Get_Status;

   function Get_TLS (Client : SMTP_Client) return Boolean is
   begin
      return Client.Accept_TLS;
   end Get_TLS;

   function Get_TLS_Always (Client : SMTP_Client) return Boolean is
   begin
      return Client.Force_TLS;
   end Get_TLS_Always;

   function Get_User (Client : SMTP_Client) return String is
   begin
      return Value (Client.User);
   end Get_User;

   function Image (Header : Mail_Header) return String is
   begin
      case Header is
         when Mail_Message_ID   => return "Message-ID";
         when Mail_In_Reply_To  => return "In-Reply-To";
         when Mail_Subject      => return "Subject";
         when Mail_MIME_Version => return "MIME-Version";
         when Mail_Content_Type => return "Content-Type";
         when Mail_Precedence   => return "Predecence";
         when Mail_References   => return "References";
         when Mail_Reply_To     => return "Reply-To";
         when Mail_Sender       => return "Sender";
         when Mail_Archived_At  => return "Archived-At";
         when Mail_To           => return "To";
         when Mail_From         => return "From";
         when Mail_Bcc          => return "Bcc";
         when Mail_Cc           => return "Cc";
         when Mail_Date         => return "Date";
         when Mail_Body         => return "Body";
      end case;
   end Image;

   function Image (List  : Mail_Address_List) return String is
      use Persistent.Catalogue;
      Length : Natural := 0;
   begin
      for Index in 1..Get_Size (List.Set) loop
         Length := Length + Get (List.Set, Index)'Length;
      end loop;
      if Length = 0 then
         return "";
      end if;
      declare
         Result  : String (1..Length + Get_Size (List.Set) - 1);
         Pointer : Integer := Result'First;
      begin
         for Index in 1..Get_Size (List.Set) loop
            if Index > 1 then
               Put (Result, Pointer, ",");
            end if;
            Put (Result, Pointer, Get (List.Set, Index));
         end loop;
         return Result;
      end;
   end Image;

   function Is_Empty (List : Mail_Address_List) return Boolean is
      use Persistent.Catalogue;
   begin
      return Get_Size (List.Set) = 0;
   end Is_Empty;

   function Is_In
            (  List    : Mail_Address_List;
               Address : String
            )  return Boolean is
      use Persistent.Catalogue;
   begin
      return Is_In (List.Set, Trim (Address));
   end Is_In;

   function Is_Opportunistic (Client : SMTP_Client) return Boolean is
   begin
      return True;
   end Is_Opportunistic;

   procedure On_Auth (Client : in out SMTP_Client'Class) is
   begin
      if Client.Authentication >= SMTP_DIGEST_MD5 then
         case Client.Index is
            when 1 =>
               Client.Index := 2;
               Send_Text
               (  Client,
                  (  DIGEST_MD5
                     (  Image (Client.Get_Client_Address.Addr),
                        Value (Client.User),
                        Value (Client.Password),
                        From_Base64
                        (  Client.Reply (1..Client.Pointer - 1)
                        ),
                        "smtp",
                        Cnonce
                     )
                  & CRLF
               )  );
            when others =>
               Client.Command := SMTP_MAIL;
               On_Mail (Client);
         end case;
      elsif Client.Authentication >= SMTP_CRAM_MD5 then
         case Client.Index is
            when 1 =>
               Client.Index := 2;
               Send_Text
               (  Client,
                  (  CRAM_MD5
                     (  Value (Client.User),
                        Value (Client.Password),
                        From_Base64
                        (  Client.Reply (1..Client.Pointer - 1)
                     )  )
                  & CRLF
               )  );
            when others =>
               Client.Command := SMTP_MAIL;
               On_Mail (Client);
         end case;
      elsif Client.Authentication >= SMTP_LOGIN then
         case Client.Index is
            when 1 =>
               Client.Index := 2;
               Send_Text
               (  Client,
                  To_Base64 (Value (Client.User)) & CRLF
               );
            when 2 =>
               Client.Index := 3;
               Send_Text
               (  Client,
                  To_Base64 (Value (Client.Password)) & CRLF
               );
            when others =>
               Client.Command := SMTP_MAIL;
               On_Mail (Client);
         end case;
      else -- Success
         Client.Command := SMTP_MAIL;
         On_Mail (Client);
      end if;
   end On_Auth;

   procedure On_Content_End
             (  Client  : in out SMTP_Client'Class;
                Message : in out Mail_Object'Class
             )  is
   begin
      if Message.First = null then
         Client.Send_State := Send_Data_End;
         Client.Index      := Data_End'First;
      else
         Client.Send_State := Send_Boundary_Middle;
         Client.Attachment := Message.First;
         Client.Index      := Boundary'First;
      end if;
   end On_Content_End;

   procedure On_EHLO_Response
             (  Client : in out SMTP_Client'Class;
                Reply  : String
             )  is
      Got_It  : Boolean;
      Flag    : SMTP_Extension;
      Pointer : Integer := Reply'First;
   begin
      Get (Reply, Pointer, Extensions, Flag, Got_It);
      if not Got_It then
         return; -- Ignore unrecognized
      end if;
      Client.Extensions (Flag) := True;
      case Flag is
         when SMTP_AUTH =>
            declare
               Mode : SMTP_AUTH_Mechanism;
            begin
               loop
                  Get (Reply, Pointer);
                  exit when Pointer > Reply'Last;
                  begin
                     Get (Reply, Pointer, Authentications, Mode);
                     Client.Authentication :=
                        Client.Authentication or Mode;
                  exception
                     when End_Error => -- Skip an unsupported method
                        loop
                           case Reply (Pointer) is
                              when ' ' | Character'Val (9) =>
                                 Pointer := Pointer + 1;
                                 exit;
                              when others =>
                                 Pointer := Pointer + 1;
                           end case;
                           exit when Pointer > Reply'Last;
                        end loop;
                  end;
               end loop;
            exception
               when others =>
                  null;
            end;
         when SMTP_SIZE =>
            begin
               Get (Reply, Pointer);
               Get (Reply, Pointer, Client.Mail_Size, First => 1);
            exception
               when others =>
                  Client.Mail_Size := Natural'Last;
            end;
         when SMTP_STARTTLS =>
            Client.Offered_TLS := True;
         when others =>
            null;
      end case;
   exception
      when others => -- Ignore unrecognized
         null;
   end On_EHLO_Response;

   procedure On_Greeting
             (  Client : in out SMTP_Client'Class;
                Code   : Error_Code;
                Reply  : String
             )  is
   begin
      if Client.Force_ESMTP then
         Client.ESMTP := True;
      else
         declare
            Start : Boolean := True;
         begin
            Client.ESMTP := False;
            for Index in Reply'Range loop
               if (  Start
                  and then
                     Is_Prefix ("esmtp", Reply, Index, Lower_Case_Map)
                  and then
                     (  Index + 5 > Reply'Last
                     or else
                        Reply (Index + 5) = ' '
                  )  )
               then
                  Client.ESMTP := True;
                  exit;
               end if;
               Start := Reply (Index) = ' ';
            end loop;
         end;
      end if;
      Client.Receive_State := Receive_Code;
      Client.Count := 0;
      if Client.ESMTP then
         Client.Command := SMTP_EHLO;
         Send_Text
         (  Client,
            (  "EHLO "
            &  Official_Name (Get_Host_By_Name (Host_Name))
            &  CRLF
         )  );
      else
         Client.Command := SMTP_HELO;
         Send_Text
         (  Client,
            (  "HELO "
            &  Official_Name (Get_Host_By_Name (Host_Name))
            &  CRLF
         )  );
      end if;
   end On_Greeting;

   procedure On_Mail (Client : in out SMTP_Client'Class) is
   begin
      if Client.Queue = null then
         Completed (Client);
         if Client.Queue = null then
            Do_Quit (Client);
            return;
         end if;
      elsif Client.Command = SMTP_EHLO then
         if (  (  Client.Force_TLS
               or else
                  (Client.Offered_TLS and then Client.Accept_TLS)
               )
            and then
               Is_TLS_Capable (Client.Listener.Factory.all)
            and then
               not Is_Elevated (Client)
            )
         then -- Go TLS
            Client.Command       := SMTP_STARTTLS;
            Client.Receive_State := Receive_Code;
            Client.Count         := 0;
            Send_Text (Client, "STARTTLS" & CRLF);
            return;
         else -- Use authentication
            Client.Authentication :=
               Client.Authentication and Client.Supported;
            if Client.Authentication >= SMTP_DIGEST_MD5 then
               Client.Authentication := SMTP_DIGEST_MD5;
               Client.Command := SMTP_AUTH;
               Send_Text (Client, "AUTH DIGEST-MD5" & CRLF);
               Client.Send_State    := Send_Command;
               Client.Receive_State := Receive_Code;
               Client.Count         := 0;
               Client.Index         := 1;
               return;
            elsif Client.Authentication >= SMTP_CRAM_MD5 then
               Client.Authentication := SMTP_CRAM_MD5;
               Client.Command := SMTP_AUTH;
               Send_Text (Client, "AUTH CRAM-MD5" & CRLF);
               Client.Send_State    := Send_Command;
               Client.Receive_State := Receive_Code;
               Client.Count         := 0;
               Client.Index         := 1;
               return;
            elsif Client.Authentication >= SMTP_LOGIN then
               Client.Authentication := SMTP_LOGIN;
               Client.Command := SMTP_AUTH;
               Send_Text (Client, "AUTH LOGIN" & CRLF);
               Client.Send_State    := Send_Command;
               Client.Receive_State := Receive_Code;
               Client.Count         := 0;
               Client.Index         := 1;
               return;
            elsif Client.Authentication = SMTP_PLAIN then
               Client.Command := SMTP_AUTH;
               Send_Text
               (  Client,
                  (  "AUTH PLAIN "
                  &  To_Base64
                     (  NUL
                     &  Value (Client.User) & NUL
                     &  Value (Client.Password)
                     )
                  &  CRLF
               )  );
               Client.Send_State    := Send_Command;
               Client.Receive_State := Receive_Code;
               Client.Count         := 0;
               return;
            end if;
         end if;
      end if;
      declare
         Message : Mail_Object'Class renames Client.Queue.all;
      begin
         Client.Command       := SMTP_MAIL;
         Client.Send_State    := Send_Command;
         Client.Receive_State := Receive_Code;
         Client.Count         := 0;
         Send_Text
         (  Client,
            "MAIL FROM:" & Get (Message, Mail_From) & CRLF
         );
      end;
   end On_Mail;

   procedure On_Recipient (Client : in out SMTP_Client'Class) is
   begin
      if Client.Queue = null then
         Completed (Client);
         if Client.Queue = null then
            Do_Quit (Client);
            return;
         end if;
      end if;
      declare
         Message : Mail_Object'Class renames Client.Queue.all;
         List    : Mail_Address_List;
      begin
         loop
            List := Get (Message, Client.Header);
            Client.Index := Client.Index + 1;
            if Client.Index > Get_Length (List) then
               case Client.Header is
                  when Mail_To =>
                     Client.Header := Mail_Bcc;
                     Client.Index  := 0;
                  when Mail_Bcc =>
                     Client.Header := Mail_Cc;
                     Client.Index  := 0;
                  when Mail_Cc =>
                     Client.Command       := SMTP_DATA;
                     Client.Receive_State := Receive_Code;
                     Client.Count         := 0;
                     Client.Header        := Mail_Header'First;
                     Send_Text (Client, "DATA" & CRLF);
                     return;
                  when others =>
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Internal error, "
                        &  "wrong address type"
                     )  );
               end case;
            else
               Client.Receive_State := Receive_Code;
               Client.Count         := 0;
               Send_Text
               (  Client,
                  "RCPT TO:" & Get_Address (List, Client.Index) & CRLF
               );
               return;
            end if;
         end loop;
      end;
   end On_Recipient;

   procedure On_Reply_Error (Client : in out SMTP_Client'Class) is
   begin
      case Client.Command is
         when SMTP_HELO | SMTP_EHLO | SMTP_RSET | SMTP_Sent |
              SMTP_HELP | SMTP_NOOP | SMTP_QUIT | SMTP_AUTH |
              SMTP_STARTTLS | SMTP_Greeting =>
            Send_Error
            (  Client,
               Client.Code,
               Client.Command,
               Client.Reply (1..Client.Pointer - 1)
            );
            raise Connection_Error;
         when SMTP_MAIL | SMTP_RCPT | SMTP_DATA =>
            if Client.Queue = null then
               Send_Error
               (  Client,
                  Client.Code,
                  Client.Command,
                  Client.Reply (1..Client.Pointer - 1)
               );
               raise Connection_Error;
            else
               Client.Queue.Status := Mail_Rejected;
               Send_Error
               (  Client,
                  Client.Code,
                  Client.Command,
                  Client.Reply (1..Client.Pointer - 1),
                  (Reference => Ref (Client.Queue))
               );
               Delete (Client.Queue);
               On_Mail (Client);
            end if;
      end case;
   end On_Reply_Error;

   procedure Prepend
             (  Object     : in out Mail_Object;
                Attachment : Attachment_Object_Ptr
             )  is
   begin
      Attachment.Next := Object.First;
      if Object.First = null then
         Object.Last := Attachment;
      end if;
      Object.First := Attachment;
   end Prepend;

   procedure Read
             (  Attachment : in out Attachment_String;
                Buffer     : in out Stream_Element_Array;
                Last       : out Stream_Element_Offset
             )  is
      Size : Integer := Attachment.Length - Attachment.Index + 1;
   begin
      Last := Buffer'First - 1;
      if Size > 0 then
         if Size > Buffer'Length then
            Size := Buffer'Length;
         end if;
         for Index in Attachment.Index..Attachment.Index + Size - 1 loop
            Last := Last + 1;
            Buffer (Last) := Character'Pos (Attachment.Buffer (Index));
         end loop;
         Attachment.Index := Attachment.Index + Size;
      end if;
   end Read;

   procedure Read
             (  Attachment : in out Attachment_Stream;
                Buffer     : in out Stream_Element_Array;
                Last       : out Stream_Element_Offset
             )  is
   begin
      Read (Attachment.Stream.all, Buffer, Last);
   exception
      when End_Error =>
         Last := Buffer'First - 1;
   end Read;

   procedure Read
             (  Attachment : in out Attachment_File;
                Buffer     : in out Stream_Element_Array;
                Last       : out Stream_Element_Offset
             )  is
   begin
      Stream_IO.Read (Attachment.File, Buffer, Last);
      if Last < Buffer'First then
         Stream_IO.Close (Attachment.File);
         Attachment.Open := False;
      end if;
   exception
      when End_Error =>
         Stream_IO.Close (Attachment.File);
         Attachment.Open := False;
         Last := Buffer'First - 1;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error reading attached file "
            &  Quote (Attachment.Name)
            &  ": "
            &  Exception_Message (Error)
         )  );
   end Read;

   procedure Received
             (  Client  : in out SMTP_Client;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Pointer := Data'First;
      if Client.Send_State /= Send_Command then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Unsolicited response while sending state is "
            &  SMTP_Send_State'Image (Client.Send_State)
         )  );
      end if;
      while Pointer <= Data'Last loop
         case Client.Receive_State is
            when Receive_Code =>
               loop
                  case Data (Pointer) is
                     when Character'Pos ('0')..Character'Pos ('9') =>
                        if Client.Count = 0 then
                           Client.Pointer := Integer (Data (Pointer))
                                           - Character'Pos ('0');
                        elsif Client.Count > 2 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "Invalid reply code, longer than 3 digits"
                           );
                        else
                           Client.Pointer := Client.Pointer * 10
                                           + Integer (Data (Pointer))
                                           - Character'Pos ('0');
                        end if;
                        Client.Count := Client.Count + 1;
                        Pointer      := Pointer + 1;
                     when Character'Pos (' ') =>
                        if Client.Count = 0 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "Missing reply code"
                           );
                        elsif Client.Pointer not in 200..554 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              (  "Invalid reply code "
                              &  Image (Client.Pointer)
                              &  " not in 200..554"
                           )  );
                        end if;
                        Client.Code :=
                           (False, Reply_Code (Client.Pointer));
                        Pointer              := Pointer + 1;
                        Client.Receive_State := Receive_Class;
                        Client.End_Of_Reply  := True;
                        Client.Pointer       := 1;
                        exit;
                     when Character'Pos ('-') =>
                        if Client.Count = 0 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "Missing continuation reply code "
                           );
                        elsif Client.Pointer not in 200..554 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              (  "Invalid continuation reply code "
                              &  Image (Client.Pointer)
                              &  " not in 200..554"
                           )  );
                        end if;
                        Client.Code :=
                           (False, Reply_Code (Client.Pointer));
                        Pointer              := Pointer + 1;
                        Client.Receive_State := Receive_Line;
                        Client.End_Of_Reply  := False;
                        Client.Pointer       := 1;
                        exit;
                     when others =>
                        Raise_Exception
                        (  Data_Error'Identity,
                           "Invalid reply code, not numerical"
                        );
                  end case;
                  exit when Pointer > Data'Last;
               end loop;
            when Receive_Class =>
               if (  Pointer = Data'Last
                  or else
                     Data (Pointer + 1) /= Character'Pos ('.')
                  )
               then -- Assume plain code
                  Client.Receive_State := Receive_Line;
               else
                  case Data (Pointer) is
                     when Character'Pos ('0') |
                          Character'Pos ('1') |
                          Character'Pos ('3') |
                          Character'Pos ('6')..Character'Pos ('9') =>
                        Raise_Exception
                        (  Data_Error'Identity,
                           (  "Invalid extended status code class, "
                           &  "not 2,4,5"
                        )  );
                     when Character'Pos ('2') =>
                        Client.Receive_State := Receive_Subject;
                        Client.Count         := 0;
                        Pointer              := Pointer + 1;
                        Client.Code :=
                           (True, Client.Code.Reply, (Success, 0, 0));
                     when Character'Pos ('4') =>
                        Client.Receive_State := Receive_Subject;
                        Client.Count         := 0;
                        Pointer              := Pointer + 1;
                        Client.Code :=
                           (  True,
                              Client.Code.Reply,
                              (Persistent_Transient_Failure, 0, 0)
                           );
                     when Character'Pos ('5') =>
                        Client.Receive_State := Receive_Subject;
                        Client.Count         := 0;
                        Pointer              := Pointer + 1;
                        Client.Code :=
                           (  True,
                              Client.Code.Reply,
                              (Permanent_Failure, 0, 0)
                           );
                     when others =>
                        Client.Receive_State := Receive_Line;
                  end case;
               end if;
            when Receive_Subject =>
               case Data (Pointer) is
                  when Character'Pos ('.') =>
                     if Client.Count = 0 then
                        Client.Count := 1;
                        Client.Code.Error.Subject := 0;
                     elsif Client.Count = 1 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           (  "Invalid extended status code, "
                           &  "missing numeric code subject"
                        )  );
                     else
                        Client.Receive_State     := Receive_Detail;
                        Client.Count             := 0;
                        Client.Code.Error.Detail := 0;
                     end if;
                  when Character'Pos ('0')..Character'Pos ('9') =>
                     if Client.Count = 0 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           (  "Invalid extended status code, "
                           &  "missing (.) after the code class"
                        )  );
                     elsif Client.Count > 3 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           (  "Invalid extended status code, "
                           &  "subject is longer than 3 digits"
                        )  );
                     end if;
                     Client.Count := Client.Count + 1;
                     Client.Code.Error.Subject :=
                        (  Client.Code.Error.Subject * 10
                        +  Code_Subject
                           (  Data (Pointer)
                           -  Character'Pos ('0')
                        )  );
                  when others =>
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Invalid extended status code subject, "
                        &  "not numeric ended by a dot (.)"
                     )  );
               end case;
               Pointer := Pointer + 1;
            when Receive_Detail =>
               case Data (Pointer) is
                  when Character'Pos ('0')..Character'Pos ('9') =>
                     if Client.Count > 3 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           (  "Invalid extended status code, "
                           &  "detail is longer than 3 digits"
                        )  );
                     end if;
                     Client.Count := Client.Count + 1;
                     Client.Code.Error.Detail :=
                        (  Client.Code.Error.Detail * 10
                        +  Code_Detail
                           (  Data (Pointer)
                           -  Character'Pos ('0')
                        )  );
                     Pointer := Pointer + 1;
                  when Character'Pos (' ') =>
                     if Client.Count = 0 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           (  "Invalid extended status code, "
                           &  "missing numeric code detail"
                        )  );
                     end if;
                     Pointer := Pointer + 1;
                     Client.Receive_State := Receive_Line;
                  when others =>
                     if Client.Count = 0 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           (  "Invalid extended status code, "
                           &  "missing numeric code detail"
                        )  );
                     end if;
                     Client.Receive_State := Receive_Line;
               end case;
            when Receive_Line =>
               loop
                  if Data (Pointer) = Stream_Element'Val (13) then
                     Client.Receive_State := Receive_LF;
                     Pointer := Pointer + 1;
                     exit;
                  elsif Client.Pointer > Client.Reply'Last then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "The reply line is longer than "
                        &  Image (Client.Reply_Length)
                        &  " characters"
                     )  );
                  end if;
                  Client.Reply (Client.Pointer) :=
                     Character'Val (Data (Pointer));
                  Client.Pointer := Client.Pointer + 1;
                  Pointer := Pointer + 1;
                  exit when Pointer > Data'Last;
               end loop;
            when Receive_LF =>
               if Data (Pointer) /= Stream_Element'Val (10) then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Response terminating <LF> is expected "
                     &  "after <CR>"
                  )  );
               end if;
               Pointer := Pointer + 1;
               Client.Receive_State := Receive_Code;
               Client.Count := 0;
               if Client.Code.Reply >= 400 then -- Fatal error
                  On_Reply_Error (Client);
               elsif not Client.End_Of_Reply then -- Multiline response
                  case Client.Command is
                     when SMTP_DATA | SMTP_HELP | SMTP_MAIL |
                          SMTP_NOOP | SMTP_RCPT | SMTP_RSET |
                          SMTP_Sent | SMTP_QUIT | SMTP_AUTH |
                          SMTP_STARTTLS | SMTP_Greeting =>
                        null;
                     when SMTP_HELO | SMTP_EHLO =>
                        On_EHLO_Response
                        (  Client,
                           Client.Reply (1..Client.Pointer - 1)
                        );
                  end case;
               else
                  case Client.Command is
                     when SMTP_Greeting =>
                        On_Greeting
                        (  Client,
                           Client.Code,
                           Client.Reply (1..Client.Pointer - 1)
                        );
                     when SMTP_Sent =>
                        Client.Queue.Status := Mail_Sent;
                        Send_Success
                        (  SMTP_Client'Class (Client),
                           (Reference => Ref (Client.Queue))
                        );
                        Delete (Client.Queue);
                        On_Mail (Client);
                     when SMTP_HELO =>
                        On_Mail (Client);
                     when SMTP_EHLO =>
                        On_Mail (Client);
                     when SMTP_AUTH =>
                        On_Auth (Client);
                     when SMTP_MAIL =>
                        Client.Command := SMTP_RCPT;
                        Client.Header  := Mail_To;
                        Client.Index   := 0;
                        On_Recipient (Client);
                     when SMTP_RCPT =>
                        On_Recipient (Client);
                     when SMTP_STARTTLS =>
                        Create_Transport (Client);
                        Client.Command := SMTP_EHLO;
                        Client.Count := 0;
                        return;
                     when SMTP_DATA =>
                        Client.Send_State := Send_Header;
                        Client.Header     := Mail_Header'First;
                        Client.Index      := 1;
                        Sent (Client);
                     when SMTP_RSET =>
                        null;
                     when SMTP_HELP =>
                        null;
                     when SMTP_NOOP =>
                        null;
                     when SMTP_QUIT =>
                        Raise_Exception
                        (  Connection_Error'Identity,
                           "End of SMPT connection"
                        );
                  end case;
               end if;
         end case;
      end loop;
   exception
      when Connection_Error =>
         Shutdown (Client);
         raise;
      when Data_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Received;

   procedure Remove
             (  List    : in out Mail_Address_List;
                Address : String
             )  is
      use Persistent.Catalogue;
   begin
      Remove (List.Set, Trim (Address));
   end Remove;

   procedure Remove
             (  List  : in out Mail_Address_List;
                Index : Positive
             )  is
      use Persistent.Catalogue;
   begin
      Remove (List.Set, Index);
   end Remove;

   procedure Send
             (  Client  : in out SMTP_Client;
                Message : Mail
             )  is
   begin
      Check (Message);
      declare
         This : constant Mail_Object_Ptr := Ptr (Message.Reference);
      begin
         This.Status := Mail_Pending;
         if Client.Queue = null then -- Empty messages queue
            Client.Queue  := This;
            This.Next     := This;
            This.Previous := This;
         else -- Add to the queue tail
            This.Next                  := Client.Queue;
            This.Previous              := Client.Queue.Previous;
            Client.Queue.Previous.Next := This;
            Client.Queue.Previous      := This;
         end if;
         Increment_Count (This.all);
      end;
   end Send;

   procedure Send
             (  Client   : in out SMTP_Client;
                Messages : Mail_Array
             )  is
   begin
      for Index in Messages'Range loop
         Check (Messages (Index));
      end loop;
      for Index in Messages'Range loop
         declare
            This : constant Mail_Object_Ptr :=
                            Ptr (Messages (Index).Reference);
         begin
            This.Status := Mail_Pending;
            if Client.Queue = null then -- Empty messages queue
               Client.Queue  := This;
               This.Next     := This;
               This.Previous := This;
            else -- Add to the queue tail
               This.Next                  := Client.Queue;
               This.Previous              := Client.Queue.Previous;
               Client.Queue.Previous.Next := This;
               Client.Queue.Previous      := This;
            end if;
            Increment_Count (This.all);
         end;
      end loop;
   end Send;

   procedure Send_Abandoned
             (  Client   : in out SMTP_Client;
                Messages : Mail_Array
             )  is
   begin
      for Index in Messages'Range loop
         Trace
         (  Client.Listener.Factory.all,
            (  "Abandoning sending the mail "
            &  Quote (Get (Messages (Index), Mail_Subject))
            &  " to: "
            &  Image (Get (Messages (Index), Mail_To))
         )  );
      end loop;
   end Send_Abandoned;

   procedure Send_Data
             (  Client : in out SMTP_Client'Class;
                Data   : Stream_Element_Array
             )  is
      Pointer : Stream_Element_Offset := Data'First;
   begin
      Send (Client, Data, Pointer);
      if Pointer < Data'Last then
         Raise_Exception
         (  Data_Error'Identity,
            (  "The output buffer is too small. At least "
            &  Image (Integer (Data'Last - Pointer))
            &  " more elements required"
         )  );
      end if;
   end Send_Data;

   procedure Send_Error
             (  Client  : in out SMTP_Client;
                Code    : Error_Code;
                Context : SMTP_Command;
                Reply   : String
             )  is
   begin
      Trace
      (  Client.Listener.Factory.all,
         "Error: " & Image (Code) & ". " & Reply
      );
   end Send_Error;

   procedure Send_Error
             (  Client  : in out SMTP_Client;
                Code    : Error_Code;
                Context : SMTP_Command;
                Reply   : String;
                Message : Mail
             )  is
   begin
      Trace
      (  Client.Listener.Factory.all,
         (  "Error: "
         &  Image (Code)
         &  ". "
         &  Reply
         &  ". While sending mail "
         &  Quote (Get (Message, Mail_Subject))
         &  " to: "
         &  Image (Get (Message, Mail_To))
      )  );
   end Send_Error;

   procedure Send_Success
             (  Client  : in out SMTP_Client;
                Message : Mail
             )  is
   begin
      Trace
      (  Client.Listener.Factory.all,
         (  "Mail "
         &  Quote (Get (Message, Mail_Subject))
         &  " successfully sent to: "
         &  Image (Get (Message, Mail_To))
      )  );
   end Send_Success;

   procedure Send_Text
             (  Client : in out SMTP_Client'Class;
                Text   : String
             )  is
      Pointer : Integer := Text'First;
   begin
      Send (Client, Text, Pointer);
      if Pointer < Text'Last then
         Raise_Exception
         (  Data_Error'Identity,
            (  "The output buffer is too small. At least "
            &  Image (Text'Last - Pointer)
            &  " more elements required"
         )  );
      end if;
   end Send_Text;

   procedure Sent (Client : in out SMTP_Client) is
   begin
      loop
         case Client.Send_State is
            when Send_Boundary_First =>
               Send (Client, Boundary (3..Boundary'Last), Client.Index);
               if Client.Index <= Boundary'Last then
                  return;
               end if;
               declare
                  Message : Mail_Object'Class renames Client.Queue.all;
               begin
                  if Message.Contents /= null then
                     Client.Send_State := Send_String_Body;
                  elsif Message.First /= null then
                     if Client.Encoded = null then
                        Client.Encoded :=
                           new String (1..Encoded_Length + 3);
                     end if;
                     Client.Send_State := Send_Content_Header;
                  else
                     Client.Send_State := Send_Boundary_Last;
                  end if;
                  Client.Index := 1;
               end;
            when Send_Boundary_Middle =>
               Send (Client, Boundary (1..2 + 2 + 16 + 2), Client.Index);
               if Client.Index <= 2 + 2 + 16 + 2 then
                  return;
               end if;
               Client.Send_State := Send_Content_Header;
               Client.Index := Client.Attachment.Header'First;
            when Send_Boundary_Last =>
               Send
               (  Client,
                  Boundary (2 + 2..2 + 2 + 16),
                  Client.Index
               );
               if Client.Index <= 2 + 2 + 16 then
                  return;
               end if;
               Client.Send_State := Send_Multipart_End;
               Client.Index      := Multipart_End'First;
            when Send_Content_Header =>
               Send
               (  Client,
                  Client.Attachment.Header,
                  Client.Index
               );
               if Client.Index <= Client.Attachment.Header'Last
               then
                  return;
               end if;
               Client.Send_State   := Send_Content;
               Client.Index        := 1;
               Client.Encoded_Last := 0;
               if Client.Encoded = null then
                  Client.Encoded := new String (1..Encoded_Length + 3);
               end if;
            when Send_Colon =>
               Send (Client, Colon, Client.Index);
               if Client.Index <= Colon'Last then
                  return;
               end if;
               Client.Send_State := Send_Value;
               Client.Index      := 1;
            when Send_Command =>
               return;
            when Send_Content =>
               loop
                  if Client.Index > Client.Encoded_Last then
                     declare -- Get a next chunk of encoded data
                        Buffer : Stream_Element_Array (1..Chunk_Length);
                        Last   : Stream_Element_Offset;
                     begin
                        Read (Client.Attachment.all, Buffer, Last);
                        if Last < Buffer'First then -- No more data
                           Close (Client.Attachment.all);
                           Client.Attachment := Client.Attachment.Next;
                           if Client.Attachment = null then
                              Client.Send_State := Send_Boundary_Last;
                           else
                              Client.Send_State := Send_Boundary_Middle;
                           end if;
                           Client.Index := 1;
                           exit;
                        end if;
                        Client.Encoded_Last := Client.Encoded'First;
                        Put
                        (  Client.Encoded.all,
                           Client.Encoded_Last,
                           To_Base64 (To_String (Buffer (1..Last)))
                        );
                        Put
                        (  Client.Encoded.all,
                           Client.Encoded_Last,
                           CRLF
                        );
                        Client.Encoded_Last := Client.Encoded_Last - 1;
                        Client.Index        := Client.Encoded'First;
                     end;
                  end if;
                  Send
                  (  Client,
                     Client.Encoded (Client.Index..Client.Encoded_Last),
                     Client.Index
                  );
                  if Client.Index <= Client.Encoded_Last then
                     return;
                  end if;
               end loop;
            when Send_Header =>
               declare
                  Message : Mail_Object'Class renames Client.Queue.all;
               begin
                  case Client.Header is
                     when Text_Header =>
                        if Message.Texts (Client.Header) = null then
                           Client.Header :=
                              Mail_Header'Val
                              (  Mail_Header'Pos (Client.Header)
                              +  1
                              );
                        else
                           declare
                              Text : constant String :=
                                     Image (Client.Header);
                           begin
                              Send (Client, Text, Client.Index);
                              if Client.Index <= Text'Last then
                                 return;
                              end if;
                              Client.Send_State := Send_Colon;
                              Client.Index      := 1;
                           end;
                        end if;
                     when List_Header =>
                        if Is_Empty
                           (  Message.Addresses (Client.Header)
                           )  then
                           Client.Header :=
                              Mail_Header'Val
                              (  Mail_Header'Pos (Client.Header)
                              +  1
                              );
                        else
                           declare
                              Text : constant String :=
                                     Image (Client.Header);
                           begin
                              Send (Client, Text, Client.Index);
                              if Client.Index <= Text'Last then
                                 return;
                              end if;
                              Client.Send_State := Send_Colon;
                              Client.Index      := 1;
                           end;
                        end if;
                     when Mail_Date =>
                        declare
                           Text : constant String :=
                                  Image (Client.Header);
                        begin
                           Send (Client, Text, Client.Index);
                           if Client.Index <= Text'Last then
                              return;
                           end if;
                           Client.Send_State := Send_Colon;
                           Client.Index      := 1;
                        end;
                     when Mail_Body =>
                        declare
                           Message : Mail_Object'Class renames
                                     Client.Queue.all;
                        begin
                           if Message.First = null then -- Single part
                              Client.Send_State := Send_CRLF;
                              Client.Index      := 1;
                           else -- Multiple parts
                              Client.Attachment := Message.First;
                              Client.Send_State := Send_Boundary_First;
                              Client.Index      := 1;
                           end if;
                        end;
                  end case;
               end;
            when Send_Value =>
               declare
                  Message : Mail_Object'Class renames Client.Queue.all;
               begin
                  case Client.Header is
                     when Text_Header =>
                        declare
                           Text : constant String :=
                                  Get (Message, Client.Header);
                        begin
                           Send (Client, Text, Client.Index);
                           if Client.Index <= Text'Last then
                              return;
                           end if;
                           Client.Send_State := Send_CRLF;
                           Client.Index      := 1;
                        end;
                     when List_Header =>
                        declare
                           Text : constant String :=
                                  Image
                                  (  Message.Addresses (Client.Header)
                                  );
                        begin
                           Send (Client, Text, Client.Index);
                           if Client.Index <= Text'Last then
                              return;
                           end if;
                           Client.Send_State := Send_CRLF;
                           Client.Index      := 1;
                        end;
                     when Mail_Date =>
                        declare
                           Text : constant String :=
                                  To_String
                                  (  Message.Dates (Client.Header)
                                  );
                        begin
                           Send (Client, Text, Client.Index);
                           if Client.Index <= Text'Last then
                               return;
                            end if;
                           Client.Send_State := Send_CRLF;
                           Client.Index      := 1;
                        end;
                      when Mail_Body =>
                         Send (Client, CRLF, Client.Index);
                         if Client.Index <= CRLF'Last then
                            return;
                         end if;
                  end case;
               end;
            when Send_CRLF =>
               Send (Client, CRLF, Client.Index);
               if Client.Index <= CRLF'Last then
                  return;
               elsif Client.Header = Mail_Body then
                  declare
                     Message : Mail_Object'Class renames
                               Client.Queue.all;
                  begin
                     if Message.Contents /= null then
                        Client.Send_State := Send_String_Body;
                     elsif Message.First /= null then
                        Client.Send_State := Send_Boundary_First;
                     else
                        Client.Send_State := Send_Data_End;
                     end if;
                     Client.Index := 1;
                  end;
               else
                  Client.Header :=
                      Mail_Header'Val
                      (  Mail_Header'Pos (Client.Header)
                      +  1
                      );
                  Client.Send_State := Send_Header;
                  Client.Index      := 1;
               end if;
            when Send_Data_End =>
               Send (Client, Data_End, Client.Index);
               if Client.Index <= Data_End'Last then
                  return;
               end if;
               Client.Receive_State := Receive_Code;
               Client.Send_State    := Send_Command;
               Client.Command       := SMTP_Sent;
               Client.Index         := 1;
               Client.Count         := 0;
            when Send_Multipart_End =>
               Send (Client, Multipart_End, Client.Index);
               if Client.Index <= Multipart_End'Last then
                  return;
               end if;
               Client.Receive_State := Receive_Code;
               Client.Send_State    := Send_Command;
               Client.Command       := SMTP_Sent;
               Client.Index         := 1;
               Client.Count         := 0;
            when Send_String_Dot =>
               declare
                  Pointer : Integer := Dot'First;
               begin
                  Send (Client, Dot, Pointer);
                  if Pointer <= Dot'Last then
                     return;
                  end if;
                  Client.Send_State := Send_String_Body;
               end;
            when Send_String_Body =>
               declare
                  Message : Mail_Object'Class renames Client.Queue.all;
                  Index   : Integer;
               begin
                  loop
                     if Client.Index > Message.Contents'Last then
                        On_Content_End (Client, Message);
                        exit;
                     end if;
                     Index := Client.Index;
                     while Index <= Message.Contents'Last loop
                        case Message.Contents (Index) is
                           when '.' =>
                              if Index = Message.Contents'First then
                                 Index := Index + 1;
                                 Client.Send_State := Send_String_Dot;
                                 exit;
                              end if;
                              Index := Index + 1;
                           when CR =>
                              Index := Index + 1;
                              if Is_Prefix
                                 (  LF_Dot,
                                    Message.Contents.all,
                                    Index
                                 )
                              then
                                 Index := Index + 2;
                                 Client.Send_State := Send_String_Dot;
                                 exit;
                              end if;
                           when others =>
                              Index := Index + 1;
                        end case;
                     end loop;
                     Send
                     (  Client,
                        Message.Contents (Client.Index..Index - 1),
                        Client.Index
                     );
                     if Client.Index < Index then
                        Client.Send_State := Send_String_Body;
                        return;
                     elsif Client.Send_State = Send_String_Dot then
                        exit;
                     end if;
                  end loop;
               end;
         end case;
      end loop;
   end Sent;

   procedure Set (Data : in out String_Ptr; Value : String) is
   begin
      if Data /= null then
         if Data.all = Value then
            return;
         end if;
         Free (Data);
      end if;
      Data := new String'(Value);
   end Set;

   procedure Set
             (  Object : in out Mail_Object;
                Header : Text_Header;
                Text   : String
             )  is
   begin
      Set (Object.Texts (Header), Text);
   end Set;

   procedure Set
             (  Message : in out Mail;
                Header  : Text_Header;
                Text    : String
             )  is
   begin
      if not Is_Valid (Message.Reference) then
         Set (Message.Reference, new Mail_Object);
      end if;
      Set (Ptr (Message.Reference).all, Header, Text);
   end Set;

   procedure Set
             (  Message : in out Mail;
                Header  : List_Header;
                List    : Mail_Address_List'Class
             )  is
   begin
      if not Is_Valid (Message.Reference) then
         Set (Message.Reference, new Mail_Object);
      end if;
      Ptr (Message.Reference).Addresses (Header).Set := List.Set;
   end Set;

   procedure Set
             (  Message : in out Mail;
                Header  : Date_Header;
                Date    : Time
             )  is
   begin
      if not Is_Valid (Message.Reference) then
         Set (Message.Reference, new Mail_Object);
      end if;
      Ptr (Message.Reference).Dates (Header) := Date;
   end Set;

   procedure Set_Enhanced
             (  Client   : in out SMTP_Client;
                Enhanced : Boolean
             )  is
   begin
      Client.Force_ESMTP := Enhanced;
   end Set_Enhanced;

   procedure Set_MIME (Object : in out Mail_Object'Class) is
   begin
      Set (Object, Mail_MIME_Version, "1.0");
      Set
      (  Object,
         Mail_Content_Type,
         "multipart/mixed; boundary=""" & Boundary (7..16 + 4) & """"
      );
      if Object.Contents /= null then
         Prepend
         (  Object,
            Create_String_Attachment (Object.Contents.all)
         );
         Free (Object.Contents);
      end if;
   end Set_MIME;

   procedure Set_Credentials
             (  Client   : in out SMTP_Client;
                User     : String;
                Password : String;
                Accepted : SMTP_AUTH_Mechanism :=
                           SMTP_AUTH_Mechanism'Last
             )  is
   begin
      Set (Client.User, User);
      Set (Client.Password, Password);
      Client.Supported := Accepted;
   end Set_Credentials;

   procedure Set_TLS
             (  Client : in out SMTP_Client;
                Enable : Boolean;
                Always : Boolean := False
             )  is
   begin
      Client.Accept_TLS := Enable;
      Client.Force_TLS  := Always;
   end Set_TLS;

   function Value (Text : String_Ptr) return String is
   begin
      if Text = null then
         return "";
      else
         return Text.all;
      end if;
   end Value;

   function "/" (Left, Right : String) return Mail_Address_List is
      Result : Mail_Address_List;
   begin
      Add_Address (Result, Left);
      Add_Address (Result, Right);
      return Result;
   end "/";

   function "/"
             (  List    : Mail_Address_List;
                Address : String
             )  return Mail_Address_List is
      Result : Mail_Address_List := List;
   begin
      Add_Address (Result, Address);
      return Result;
   end "/";

   function "and" (Left, Right : Mail_Address_List)
      return Mail_Address_List is
      use Persistent.Catalogue;
   begin
      return (Set => Left.Set and Right.Set);
   end "and";

   function "or" (Left, Right : Mail_Address_List)
      return Mail_Address_List is
      use Persistent.Catalogue;
   begin
      return (Set => Left.Set or Right.Set);
   end "or";

   function "xor" (Left, Right : Mail_Address_List)
      return Mail_Address_List is
      use Persistent.Catalogue;
   begin
      return (Set => Left.Set xor Right.Set);
   end "xor";

begin
   Add (Extensions, "8BITMIME",            SMTP_8BITMIME);
   Add (Extensions, "ATRN",                SMTP_ATRN);
   Add (Extensions, "AUTH",                SMTP_AUTH);
   Add (Extensions, "AUTH=",               SMTP_AUTH);
   Add (Extensions, "BINARYMIME",          SMTP_BINARYMIME);
   Add (Extensions, "CHECKPOINT",          SMTP_CHECKPOINT);
   Add (Extensions, "CHUNKING",            SMTP_CHUNKING);
   Add (Extensions, "DSN",                 SMTP_DSN);
   Add (Extensions, "ENHANCEDSTATUSCODES", SMTP_ENHANCEDSTATUSCODES);
   Add (Extensions, "ETRN",                SMTP_ETRN);
   Add (Extensions, "EXPN",                SMTP_EXPN);
   Add (Extensions, "HELP",                SMTP_HELP);
   Add (Extensions, "ONEX",                SMTP_ONEX);
   Add (Extensions, "PIPELINING",          SMTP_PIPELINING);
   Add (Extensions, "RSET",                SMTP_RSET);
   Add (Extensions, "SAML",                SMTP_SAML);
   Add (Extensions, "SEND",                SMTP_SEND);
   Add (Extensions, "SIZE",                SMTP_SIZE);
   Add (Extensions, "SMTPUTF8",            SMTP_SMTPUTF8);
   Add (Extensions, "SOML",                SMTP_SOML);
   Add (Extensions, "STARTTLS",            SMTP_STARTTLS);
   Add (Extensions, "TIME",                SMTP_TIME);
   Add (Extensions, "TLS",                 SMTP_TLS);
   Add (Extensions, "TURN",                SMTP_TURN);
   Add (Extensions, "UTF8SMTP",            SMTP_UTF8SMTP);
   Add (Extensions, "VERB",                SMTP_VERB);
   Add (Extensions, "VRFY",                SMTP_VRFY);
   Add (Extensions, "X-EXPS",              SMTP_X_EXPS);
   Add (Extensions, "X-EXPS=LOGIN",        SMTP_X_EXPS_LOGIN);
   Add (Extensions, "X-LINK2STATE",        SMTP_X_LINK2STATE);
   Add (Extensions, "X-RCPTLIMIT",         SMTP_X_RCPTLIMIT);
   Add (Extensions, "X-TURNME",            SMTP_X_TURNME);
   Add (Extensions, "XADR",                SMTP_XADR);
   Add (Extensions, "XAUD",                SMTP_XAUD);
   Add (Extensions, "XDSN",                SMTP_XDSN);
   Add (Extensions, "XEXCH50",             SMTP_XEXCH50);
   Add (Extensions, "XGEN",                SMTP_XGEN);
   Add (Extensions, "XONE",                SMTP_XONE);
   Add (Extensions, "XQUE",                SMTP_XQUE);
   Add (Extensions, "XREMOTEQUEUE",        SMTP_XREMOTEQUEUE);
   Add (Extensions, "XSTA",                SMTP_XSTA);
   Add (Extensions, "XTRN",                SMTP_XTRN);
   Add (Extensions, "XUSR",                SMTP_XUSR);
   Add (Extensions, "XVRB",                SMTP_XVRB);

   Add (Authentications, "ANONYMOUS",  SMTP_ANONYMOUS);
   Add (Authentications, "CRAM-MD5",   SMTP_CRAM_MD5);
   Add (Authentications, "DIGEST-MD5", SMTP_DIGEST_MD5);
   Add (Authentications, "LOGIN",      SMTP_LOGIN);
   Add (Authentications, "PLAIN",      SMTP_PLAIN);

   Add (Challenges, "algorithm",  SMTP_ALGORITHM);
   Add (Challenges, "charset",    SMTP_CHARSET);
   Add (Challenges, "cnonce",     SMTP_CNONCE);
   Add (Challenges, "digest-uri", SMTP_DIGEST_URI);
   Add (Challenges, "nonce",      SMTP_NONCE);
   Add (Challenges, "qop",        SMTP_QOP);
   Add (Challenges, "realm",      SMTP_REALM);

end GNAT.Sockets.SMTP.Client;
