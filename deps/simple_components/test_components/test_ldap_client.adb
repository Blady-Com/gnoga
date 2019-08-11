--                                                                    --
--  procedure Test_LDAP_Client      Copyright (c)  Dmitry A. Kazakov  --
--  Test data client                               Luebeck            --
--                                                 Summer, 2019       --
--                                                                    --
--                                Last revision :  18:41 01 Aug 2019  --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Streams;                  use Ada.Streams;
with Ada.Text_IO;                  use Ada.Text_IO;
with GNAT.Sockets;                 use GNAT.Sockets;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;
with Interfaces;                   use Interfaces;
with Strings_Edit;                 use Strings_Edit;
with Strings_Edit.Floats;          use Strings_Edit.Floats;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Strings_Edit.Quoted;          use Strings_Edit.Quoted;

with Ada.Numerics.Discrete_Random;
with GNAT.Exception_Traces;
with GNAT.Sockets.Connection_State_Machine.ASN1.Distinguished_Names;
with GNAT.Sockets.Connection_State_Machine.ASN1.Strings;

with GNAT.Sockets.Connection_State_Machine.LDAP.Client;
use  GNAT.Sockets.Connection_State_Machine.LDAP;
use  GNAT.Sockets.Connection_State_Machine.LDAP.Client;

with Strings_Edit.Distinguished_Names;
use  Strings_Edit.Distinguished_Names;

with Strings_Edit.Object_Identifiers;
use  Strings_Edit.Object_Identifiers;

procedure Test_LDAP_Client is
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
--if false then
   declare
      use GNAT.Sockets.Connection_State_Machine;
      use ASN1.Distinguished_Names;
      use ASN1.Strings;

      function Hex (Data : Stream_Element_Array) return String is
         Result  : String (1..Data'Length * 3);
         Pointer : Integer := Result'First;
      begin
         for Index in Data'Range loop
            if Index > Data'First then
               Result (Pointer) := ' ';
               Pointer := Pointer + 1;
            end if;
            Put
            (  Destination => Result,
               Pointer     => Pointer,
               Value       => Integer (Data (Index)),
               Base        => 16,
               Fill        => '0',
               Field       => 2,
               Justify     => Strings_Edit.Right
            );
         end loop;
         return Result (1..Pointer - 1);
      end Hex;

      function "+" (Data : String) return Stream_Element_Array is
         Result  : Stream_Element_Array (1..Data'Length);
         Element : Stream_Element_Offset := 1;
         Pointer : Integer := Data'First;
      begin
         Get (Data, Pointer);
         while Pointer <= Data'Last loop
            Get (Data, Pointer, Integer (Result (Element)), Base => 16);
            Get (Data, Pointer);
            Element := Element + 1;
         end loop;
         return Result (1..Element - 1);
      end "+";

      function "+"
               (  Left  : Stream_Element_Array;
                  Right : String
               )  return Stream_Element_Array is
      begin
         return Left & (+Right);
      end "+";

      procedure Compare
                (  Result   : Stream_Element_Array;
                   Expected : Stream_Element_Array;
                   Name     : String
                )  is
      begin
         if Result /= Expected then
            Put_Line (Hex (Result));
            Put_Line (Hex (Expected) & " (expected)");
            Raise_Exception (Data_Error'Identity, Name & " error");
         end if;
      end Compare;

      procedure Test_Feed
                (  Data  : Stream_Element_Array;
                   Title : String
                )  is
         Factory : aliased Connections_Factory;
         Server  : aliased Connections_Server (Factory'Access, 0);
         Client  : LDAP_Client
                   (  Listener           => Server'Unchecked_Access,
                      Message_Length     => 512,
                      Incoming_Data_Size => 1024 * 10,
                      Outgoing_Data_Size => 1024 * 10,
                      Input_Size         => 512,
                      Output_Size        => 512
                   );
         Pointer : Stream_Element_Offset;
      begin
         Connected (Client);
         Pointer := Data'First;
         Received
         (  Client  => Client,
            Data    => Data,
            Pointer => Pointer
         );
         if Pointer /= Data'Last + 1 then
            Raise_Exception
            (  Status_Error'Identity,
               (  Title
               &  " Feed overrun Pointer"
               &  Stream_Element_Offset'Image (Pointer)
               &  " /= "
               &  Stream_Element_Offset'Image (Data'Last + 1)
               &  " (expected)"
            )  );
         end if;
      exception
         when Status_Error =>
            raise;
         when Error : others =>
            Raise_Exception
            (  Data_Error'Identity,
               (  Title
               &  " exception in feed test ["
               &  Image  (Data)
               &  "] "
               &  Exception_Information (Error)
            )  );
      end Test_Feed;

      package Dump is new Generic_Dump;
      use Dump;

      Buffer  : aliased External_String_Buffer (1024 * 10);
      Request : Stream_Element_Array (1..1024);
      Pointer : Stream_Element_Offset;

      Filter : LDAP_Filter;
      A      : LDAP_Attribute;
      AC     : LDAP_Authentication_Choice;
      AL     : LDAP_Attribute_List;
      AR     : LDAP_Add_Request;
      AVA    : LDAP_Attribute_Value_Assertion;
      BR     : LDAP_Bind_Request;
      C      : LDAP_SASL_Credentials;
      CTRL   : LDAP_Control;
      CR     : LDAP_Compare_Request;
      ER     : LDAP_Extended_Request;
      ERS    : LDAP_Extended_Response;
      IR     : LDAP_Intermediate_Response;
      R      : LDAP_Result;
      O      : LDAP_Operation;
      OL     : LDAP_Operations_List;
      M      : LDAP_Message;
      MDR    : LDAP_Modify_DN_Request;
      MR     : LDAP_Modify_Request;
      MRA    : LDAP_Matching_Rule_Assertion;
      S      : LDAP_Implicit_String;
      SF     : LDAP_Substring_Filter;
      SR     : LDAP_Search_Request;
      SRE    : LDAP_Search_Result_Entry;
      SS     : LDAP_String_Sequence;
      SSS    : LDAP_Substring;
   begin
      Put_Line ("LDAP_String ----------------------------------------");
      External_Initialize (S, Buffer'Unchecked_Access);
      Put (S);
      Put_Line ("LDAP_String_Sequence -------------------------------");
      External_Initialize (SS, Buffer'Unchecked_Access);
      Put (SS);
      Put_Line ("LDAP_Attribute -------------------------------------");
      External_Initialize (A, Buffer'Unchecked_Access);
      Put (A);
      Put_Line ("LDAP_Add_Request -----------------------------------");
      External_Initialize (AR, Buffer'Unchecked_Access);
      Put (AR);
      Put_Line ("LDAP_Attribute_List --------------------------------");
      External_Initialize (AL, Buffer'Unchecked_Access);
      Put (AL);
      Put_Line ("LDAP_Attribute_Value_Assertion ---------------------");
      External_Initialize (AVA, Buffer'Unchecked_Access);
      Put (AVA);
      Put_Line ("LDAP_Result ----------------------------------------");
      External_Initialize (R, Buffer'Unchecked_Access);
      Put (R);
      Put_Line ("LDAP_SASL_Credentials ------------------------------");
      External_Initialize (C, Buffer'Unchecked_Access);
      Put (C);
      Put_Line ("LDAP_Authentication_Choice -------------------------");
      External_Initialize (AC, Buffer'Unchecked_Access);
      Put (AC);
      Put_Line ("LDAP_Bind_Request ----------------------------------");
      External_Initialize (BR, Buffer'Unchecked_Access);
      Put (BR);
      Put_Line ("LDAP_Compare_Request -------------------------------");
      External_Initialize (CR, Buffer'Unchecked_Access);
      Put (CR);
      Put_Line ("LDAP_Extended_Request ------------------------------");
      External_Initialize (ER, Buffer'Unchecked_Access);
      Put (ER);
      Put_Line ("LDAP_Extended_Response -----------------------------");
      External_Initialize (ERS, Buffer'Unchecked_Access);
      Put (ERS);
      Put_Line ("LDAP_Operations_List -------------------------------");
      External_Initialize (OL, Buffer'Unchecked_Access);
      Put (OL);
      Put_Line ("LDAP_Modify_Request --------------------------------");
      External_Initialize (MR, Buffer'Unchecked_Access);
      Put (MR);
      Put_Line ("LDAP_Modify_DN_Request -----------------------------");
      External_Initialize (MDR, Buffer'Unchecked_Access);
      Put (MDR);
      Put_Line ("LDAP_Substring -------------------------------------");
      External_Initialize (SSS,    Buffer'Unchecked_Access);
      Put (SSS);
      Put_Line ("LDAP_Substring_Filter ------------------------------");
      External_Initialize (SF,     Buffer'Unchecked_Access);
      Put (SF);
      Put_Line ("LDAP_Matching_Rule_Assertion -----------------------");
      External_Initialize (MRA, Buffer'Unchecked_Access);
      Put (MRA);
      Put_Line ("LDAP_Filter ----------------------------------------");
      External_Initialize (Filter, Buffer'Unchecked_Access);
      Put (Filter);
      Put_Line ("LDAP_Intermediate_Response -------------------------");
      External_Initialize (IR, Buffer'Unchecked_Access);
      Put (IR);
      Put_Line ("LDAP_Search_Request --------------------------------");
      External_Initialize (SR, Buffer'Unchecked_Access);
      Put (SR);
      Put_Line ("LDAP_Search_Result_Entry ---------------------------");
      External_Initialize (SRE, Buffer'Unchecked_Access);
      Put (SRE);
      Put_Line ("LDAP_Operation -------------------------------------");
      External_Initialize (O, Buffer'Unchecked_Access);
      Put (O);
      Put_Line ("LDAP_Control ---------------------------------------");
      External_Initialize (CTRL, Buffer'Unchecked_Access);
      Put (CTRL);

      Put_Line ("LADP_Message ---------------------------------------");
      External_Initialize (M, Buffer'Unchecked_Access);
      Put (M);
      ------------------------------------------------------------------
      -- Abandon request
      --
      Erase (Buffer);
      M.Message_ID.Value := 6;
      Set_Abandon_Request (M, 5);
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 06" -- Begin the LDAPMessage sequence
         +"   02 01 06" -- The message ID (integer value 6)
         +"   50 01 05", -- The abandon request protocol op (application primitive integer 5)
         "Abandon request"
      );
      ------------------------------------------------------------------
      -- Add request
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Add_Request
      (  M,
         Value ("dc=example,dc=com"),
         "objectClass",
         "top"
      );
      Set_Attribute_Value (M, "domain");
      Set_Add_Attribute (M, "dc", "example");
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 49" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   68 44" -- Begin the add request protocol op
         +"      04 11 64 63 3d 65 78 61 6d 70" -- The DN of the entry to add
         +"            6c 65 2c 64 63 3d 63 6f" -- (octet string "dc=example,dc=com")
         +"            6d"
         +"      30 2f" -- Begin the sequence of attributes
         +"         30 1c" -- Begin the first attribute sequence
         +"            04 0b 6f 62 6a 65 63 74 43 6c" -- The attribute description
         +"    61 73 73"                -- (octet string "objectClass")
         +"            31 0d" -- Begin the set of values
         +" 04 03 74 6f 70" -- The first value (octet string "top")
         +" 04 06 64 6f 6d 61 69 6e" -- The second value (octet string "domain")
         +"         30 0f" -- Begin the second attribute sequence
         +"            04 02 64 63" -- The attribute description (octet string "dc")
         +"            31 09" -- Begin the set of values
         +" 04 07 65 78 61 6d 70 6c 65", -- The value (octet string "example")
         "Add request"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Add_Request
      (  M,
         Value ("dc=example,dc=com"),
         "objectClass" - "top"/"domain" or "dc" - "example"
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 49" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   68 44" -- Begin the add request protocol op
         +"      04 11 64 63 3d 65 78 61 6d 70" -- The DN of the entry to add
         +"            6c 65 2c 64 63 3d 63 6f" -- (octet string "dc=example,dc=com")
         +"            6d"
         +"      30 2f" -- Begin the sequence of attributes
         +"         30 1c" -- Begin the first attribute sequence
         +"            04 0b 6f 62 6a 65 63 74 43 6c" -- The attribute description
         +"    61 73 73"                -- (octet string "objectClass")
         +"            31 0d" -- Begin the set of values
         +" 04 03 74 6f 70" -- The first value (octet string "top")
         +" 04 06 64 6f 6d 61 69 6e" -- The second value (octet string "domain")
         +"         30 0f" -- Begin the second attribute sequence
         +"            04 02 64 63" -- The attribute description (octet string "dc")
         +"            31 09" -- Begin the set of values
         +" 04 07 65 78 61 6d 70 6c 65", -- The value (octet string "example")
         "Add request (using list)"
      );
      ------------------------------------------------------------------
      -- Add response
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Add_Response
      (  Message => M,
         Code    => Success_Code
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 0c" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   69 07" -- Begin the add response protocol op
         +"      0a 01 00" -- success result code (enumerated value 0)
         +"      04 00" -- No matched DN (0-byte octet string)
         +"      04 00", -- No diagnostic message (0-byte octet string)
         "Add response"
      );
      Erase (Buffer);
      M.Message_ID.Value := 3;
      Set_Add_Response
      (  Message     => M,
         Code        => No_Such_Object_Code,
         Matched     => Value ("ou=People,dc=example,dc=com"),
         Diagnostics => "Entry uid=missing1, ou=missing2, "     &
                        "ou=People, dc=example, dc=com cannot " &
                        "be created because its parent does not exist."
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 81 9b" -- Begin the LDAPMessage sequence
         +"   02 01 03" -- The message ID (integer value 3)
         +"   69 81 95" -- Begin the add response protocol op
         +"      0a 01 20" -- noSuchObject result code (enumerated value 32)
         +"      04 1b 6f 75 3d 50 65 6f 70 6c" -- Matched DN
         +"            65 2c 64 63 3d 65 78 61"
         +"            6d 70 6c 65 2c 64 63 3d"
         +"            63 6f 6d"
         +"      04 73 45 6e 74 72 79 20 75 69" -- Diagnostic message
         +"            64 3d 6d 69 73 73 69 6e" -- (115-byte octet string)
         +"            67 31 2c 20 6f 75 3d 6d"
         +"            69 73 73 69 6e 67 32 2c"
         +"            20 6f 75 3d 50 65 6f 70"
         +"            6c 65 2c 20 64 63 3d 65"
         +"            78 61 6d 70 6c 65 2c 20"
         +"            64 63 3d 63 6f 6d 20 63"
         +"            61 6e 6e 6f 74 20 62 65"
         +"            20 63 72 65 61 74 65 64"
         +"            20 62 65 63 61 75 73 65"
         +"            20 69 74 73 20 70 61 72"
         +"            65 6e 74 20 64 6f 65 73"
         +"            20 6e 6f 74 20 65 78 69"
         +"            73 74 2e",
         "Add response (with diagnostics)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 3;
      Set_Add_Response
      (  Message     => M,
         Code        => Referral_Code,
         Diagnostics => "This server is read-only. " &
                        " Try a different one."
      );
      Set_Response_Referral
      (  M,
         (  "ldap://alternate1.example.com:389/uid=jdoe,"
         &  "ou=Remote,dc=example,dc=com"
      )  );
      Set_Response_Referral
      (  M,
         (  "ldap://alternate2.example.com:389/uid=jdoe,"
         &  "ou=Remote,dc=example,dc=com"
      )  );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 81 cf" -- Begin the LDAPMessage sequence
         +"   02 01 03" -- The message ID (integer value 3)
         +"   69 81 c9" -- Begin the add response protocol op
         +"      0a 01 0a" -- REFERRAL result code (enumerated value 10)
         +"      04 00" -- No matched DN (0-byte octet string)
         +"      04 2f 54 68 69 73 20 73 65 72" -- Diagnostic message
         +"            76 65 72 20 69 73 20 72" -- (47-byte octet string)
         +"            65 61 64 2d 6f 6e 6c 79"
         +"            2e 20 20 54 72 79 20 61"
         +"            20 64 69 66 66 65 72 65"
         +"            6e 74 20 6f 6e 65 2e"
         +"      a3 81 90" -- Begin the referrals sequence
         +"         04 46 6c 64 61 70 3a 2f 2f 61" -- First referral URL
         +" 6c 74 65 72 6e 61 74 65" -- (70-byte octet string)
         +" 31 2e 65 78 61 6d 70 6c"
         +" 65 2e 63 6f 6d 3a 33 38"
         +" 39 2f 75 69 64 3d 6a 64"
         +" 6f 65 2c 6f 75 3d 52 65"
         +" 6d 6f 74 65 2c 64 63 3d"
         +" 65 78 61 6d 70 6c 65 2c"
         +" 64 63 3d 63 6f 6d"
         +"         04 46 6c 64 61 70 3a 2f 2f 61" -- Second referral URL
         +" 6c 74 65 72 6e 61 74 65" -- (70-byte octet string)
         +" 32 2e 65 78 61 6d 70 6c"
         +" 65 2e 63 6f 6d 3a 33 38"
         +" 39 2f 75 69 64 3d 6a 64"
         +" 6f 65 2c 6f 75 3d 52 65"
         +" 6d 6f 74 65 2c 64 63 3d"
         +" 65 78 61 6d 70 6c 65 2c"
         +" 64 63 3d 63 6f 6d",
         "Add response (with referral)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 3;
      Set_Add_Response
      (  Message     => M,
         Code        => Referral_Code,
         Diagnostics => "This server is read-only. " &
                        " Try a different one.",
         Referral    =>
            "ldap://alternate1.example.com:389/uid=jdoe,ou=Remote,dc=example,dc=com"/
            "ldap://alternate2.example.com:389/uid=jdoe,ou=Remote,dc=example,dc=com"
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 81 cf" -- Begin the LDAPMessage sequence
         +"   02 01 03" -- The message ID (integer value 3)
         +"   69 81 c9" -- Begin the add response protocol op
         +"      0a 01 0a" -- REFERRAL result code (enumerated value 10)
         +"      04 00" -- No matched DN (0-byte octet string)
         +"      04 2f 54 68 69 73 20 73 65 72" -- Diagnostic message
         +"            76 65 72 20 69 73 20 72" -- (47-byte octet string)
         +"            65 61 64 2d 6f 6e 6c 79"
         +"            2e 20 20 54 72 79 20 61"
         +"            20 64 69 66 66 65 72 65"
         +"            6e 74 20 6f 6e 65 2e"
         +"      a3 81 90" -- Begin the referrals sequence
         +"         04 46 6c 64 61 70 3a 2f 2f 61" -- First referral URL
         +" 6c 74 65 72 6e 61 74 65" -- (70-byte octet string)
         +" 31 2e 65 78 61 6d 70 6c"
         +" 65 2e 63 6f 6d 3a 33 38"
         +" 39 2f 75 69 64 3d 6a 64"
         +" 6f 65 2c 6f 75 3d 52 65"
         +" 6d 6f 74 65 2c 64 63 3d"
         +" 65 78 61 6d 70 6c 65 2c"
         +" 64 63 3d 63 6f 6d"
         +"         04 46 6c 64 61 70 3a 2f 2f 61" -- Second referral URL
         +" 6c 74 65 72 6e 61 74 65" -- (70-byte octet string)
         +" 32 2e 65 78 61 6d 70 6c"
         +" 65 2e 63 6f 6d 3a 33 38"
         +" 39 2f 75 69 64 3d 6a 64"
         +" 6f 65 2c 6f 75 3d 52 65"
         +" 6d 6f 74 65 2c 64 63 3d"
         +" 65 78 61 6d 70 6c 65 2c"
         +" 64 63 3d 63 6f 6d",
         "Add response (with referral list)"
      );
      ------------------------------------------------------------------
      -- Bind request
      --
      Erase (Buffer);
      M.Message_ID.Value := 1;
      Set_Bind_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "secret123"
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 39" -- Begin the LDAPMessage sequence
         +"   02 01 01" -- The message ID (integer value 1)
         +"   60 34" -- Begin the bind request protocol op
         +"   02 01 03" -- The LDAP protocol version (integer value 3)
         +"   04 24 75 69 64 3d 6a 64 6f 65" -- The bind DN (36-byte octet string
         +"         2c 6f 75 3d 50 65 6f 70" -- "uid=jdoe,ou=People,dc=example,dc=com")
         +"         6c 65 2c 64 63 3d 65 78"
         +"         61 6d 70 6c 65 2c 64 63"
         +"         3d 63 6f 6d"
         +"   80 09 73 65 63 72 65 74 31 32" -- The password (9-byte octet string "secret123"
         +"         33",
         "Bind request"
      );

      Erase (Buffer);
      M.Message_ID.Value := 1;
      Set_Bind_Request (M, Null_Name, "CRAM-MD5", "");
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 16" -- Begin the LDAPMessage sequence
         +"   02 01 01" -- The message ID (integer value 1)
         +"   60 11" -- Begin the bind request protocol op
         +"      02 01 03" -- The LDAP protocol version (integer value 3)
         +"      04 00" -- Empty bind DN (0-byte octet string)
         +"      a3 0a" -- Begin the SASL authentication sequence
         +"         04 08 43 52 41 4d 2d 4d 44 35", -- The SASL mechanism name
                                                    -- (the octet string "CRAM-MD5")
         "Bind request SASL"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Bind_Request
      (  M,
         Null_Name,
         "CRAM-MD5",
         "u:jdoe d52116c87c31d9cc747600f9486d2a1d"
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 3f" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   60 3a" -- Begin the bind request protocol op
         +"      02 01 03" -- The LDAP protocol version (integer value 3)
         +"      04 00" -- Empty bind DN (0-byte octet string)
         +"      a3 33" -- Begin the SASL authentication sequence
         +"         04 08 43 52 41 4d 2d 4d 44 35" -- The SASL mechanism name
                                                   -- (the octet string "CRAM-MD5")
         +"         04 27 75 3a 6a 64 6f 65 20 64" -- The SASL credentials (the octet string
         +" 35 32 31 31 36 63 38 37" -- "u:jdoe d52116c87c31d9cc747600f9486d2a1d")
         +" 63 33 31 64 39 63 63 37"
         +" 34 37 36 30 30 66 39 34"
         +" 38 36 64 32 61 31 64",
         "Bind request SASL (with credentials)"
      );
      ------------------------------------------------------------------
      -- Bind_Response
      --
      Erase (Buffer);
      M.Message_ID.Value := 1;
      Set_Bind_Response (M, Success_Code);
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 0c" -- Begin the LDAPMessage sequence
         +"   02 01 01" -- The message ID (integer value 1)
         +"   61 07" -- Begin the bind response protocol op
         +"      0a 01 00" -- success result code (enumerated value 0)
         +"      04 00"  -- No matched DN (0-byte octet string)
         +"      04 00", -- No diagnostic message (0-byte octet string)
         "Bind response"
      );

      Erase (Buffer);
      M.Message_ID.Value := 1;
      Set_Bind_Response
      (  M,
         SASL_Bind_In_Progress_Code,
         Credentials => "<10a13c7bf708ca0f399ca99e927da88b>"
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 30" -- Begin the LDAPMessage sequence
         +"   02 01 01" -- The message ID (integer value 1)
         +"   61 2b" -- Begin the bind response protocol op
         +"   0a 01 0e" -- saslBindInProgress result code (enumerated value 14)
         +"   04 00" -- No matched DN (0-byte octet string)
         +"   04 00" -- No diagnostic message (0-byte octet string)
         +"   87 22 3c 31 30 61 31 33 63 37" -- The server SASL credentials (the octet string
         +"         62 66 37 30 38 63 61 30" -- "<10a13c7bf708ca0f399ca99e927da88b>")
         +"         66 33 39 39 63 61 39 39"
         +"         65 39 32 37 64 61 38 38"
         +"         62 3e",
         "Bind response SASL (with credentials)"
      );
      ------------------------------------------------------------------
      -- Compare request
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Compare_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "employeeType",
         "salaried"
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 45" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   6e 40" -- Begin the compare request protocol op
         +"   04 24 75 69 64 3d 6a 64 6f 65" -- The target entry DN (octet string
         +"         2c 6f 75 3d 50 65 6f 70" -- "uid=jdoe,ou=People,dc=example,dc=com")
         +"         6c 65 2c 64 63 3d 65 78"
         +"         61 6d 70 6c 65 2c 64 63"
         +"         3d 63 6f 6d"
         +"   30 18" -- Begin the attribute value assertion sequence
         +"      04 0c 65 6d 70 6c 6f 79 65 65" -- The attribute description (octet string
         +"            54 79 70 65"             -- "employeeType")
         +"      04 08 73 61 6c 61 72 69 65 64", -- The assertion value (octet string "salaried")
         "Compare request"
      );
      ------------------------------------------------------------------
      -- Compare response
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Compare_Response (M, Compare_True_Code);
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 0c" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   6f 07" -- Begin the compare response protocol op
         +"      0a 01 06" -- compareTrue result code (enumerated value 6)
         +"      04 00" -- No matched DN (0-byte octet string)
         +"      04 00", -- No diagnostic message (0-byte octet string)"
         "Compare response"
      );
      ------------------------------------------------------------------
      -- Delete request
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Delete_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com")
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 29" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   4a 24 75 69 64 3d 6a 64 6f 65" -- The delete request protocol op (octet string
         +"         2c 6f 75 3d 50 65 6f 70" -- "uid=jdoe,ou=People,dc=example,dc=com"
         +"         6c 65 2c 64 63 3d 65 78" -- with type application class, primitive,
         +"         61 6d 70 6c 65 2c 64 63" -- tag number ten)
         +"         3d 63 6f 6d",
         "Delete request"
      );
      ------------------------------------------------------------------
      -- Delete response
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Delete_Response (M, Success_Code);
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 0c" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   6b 07" -- Begin the delete response protocol op
         +"      0a 01 00" -- success result code (enumerated value 0)
         +"      04 00" -- No matched DN (0-byte octet string)
         +"      04 00", -- No diagnostic message (0-byte octet string)
         "Delete response"
      );
      ------------------------------------------------------------------
      -- Extended request
      --
      Erase (Buffer);
      M.Message_ID.Value := 1;
      Set_Extended_Request
      (  M,
         (1, 3, 6, 1, 4, 1, 1466, 20037)
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 1d" -- Begin the LDAPMessage sequence
         +"   02 01 01" -- The message ID (integer value 1)
         +"   77 18" -- Begin the extended request protocol op
         +"      80 16 31 2e 33 2e 36 2e 31 2e" -- The extended request OID
         +"            34 2e 31 2e 31 34 36 36" -- (octet string "1.3.6.1.4.1.1466.20037"
         +"            2e 32 30 30 33 37",       -- with type context-specific primitive zero)
         "Extended request"
      );
      ------------------------------------------------------------------
      -- Extended response
      --
      Erase (Buffer);
      M.Message_ID.Value := 1;
      Set_Extended_Response
      (  Message => M,
         Code    => Success_Code,
         Name    => (1, 3, 6, 1, 4, 1, 1466, 20037)
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 24" -- Begin the LDAPMessage sequence
         +"   02 01 01" -- The message ID (integer value 1)
         +"   78 1f" -- Begin the extended response protocol op
         +"      0a 01 00" -- success result code (enumerated value 0)
         +"      04 00" -- No matched DN (0-byte octet string)
         +"      04 00" -- No diagnostic message (0-byte octet string)
         +"      8a 16 31 2e 33 2e 36 2e 31 2e" -- The extended response OID
         +"            34 2e 31 2e 31 34 36 36" -- (octet string "1.3.6.1.4.1.1466.20037"
         +"            2e 32 30 30 33 37",       -- with type context-specific primitive zero)
         "Extended response"
      );
      ------------------------------------------------------------------
      -- Modify request
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Modify_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         Delete_Operation,
         "givenName",
         "John"
      );
      Set_Modify_Request_Add_Operation
      (  M,
         Add_Operation,
         "givenName",
         "Jonathan"
      );
      Set_Modify_Request_Add_Operation
      (  M,
         Replace_Operation,
         "cn",
         "Jonathan Doe"
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 81 80" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   66 7b" -- Begin the modify request protocol op
         +"      04 24 75 69 64 3d 6a 64 6f 65" -- The DN of the entry to modify (octet string
         +"            2c 6f 75 3d 50 65 6f 70" -- "uid=jdoe,ou=People,dc=example,dc=com")
         +"            6c 65 2c 64 63 3d 65 78"
         +"            61 6d 70 6c 65 2c 64 63"
         +"            3d 63 6f 6d"
         +"      30 53" -- Begin the sequence of modifications
         +"         30 18" -- Begin the sequence for the first modification
         +"            0a 01 01" -- The delete modification type (enumerated value 1)
         +"            30 13" -- Begin the attribute sequence
         +" 04 09 67 69 76 65 6e 4e 61 6d" -- The attribute description
         +"       65"                      -- (octet string "givenName")
         +" 31 06" -- Begin the attribute value set
         +"    04 04 4a 6f 68 6e" -- The attribute value (octet string "John")
         +"         30 1c" -- Begin the sequence for the second modification
         +"            0a 01 00" -- The add modification type (enumerated value 0)
         +"            30 17" -- Begin the attribute sequence
         +" 04 09 67 69 76 65 6e 4e 61 6d" -- The attribute description
         +"       65"                      -- (octet string "givenName")
         +" 31 0a"  -- Begin the attribute value set
         +"    04 08 4a 6f 6e 61 74 68 61 6e" -- The attribute value
                                             -- (octet string "Jonathan")
         +"         30 19" -- Begin the sequence for the third modification
         +"            0a 01 02" -- The replace modification type (enumerated value 2)
         +"            30 14" -- Begin the attribute sequence
         +" 04 02 63 6e" -- The attribute description (octet string "cn")
         +" 31 0e" -- Begin the attribute value set
         +"    04 0c 4a 6f 6e 61 74 68 61 6e" -- The attribute value
         +"          20 44 6f 65",             -- (octet string "Jonathan Doe")
         "Modify request"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Modify_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         (  Delete  ("givenName" - "John")
         or Add     ("givenName" - "Jonathan")
         or Replace ("cn" - "Jonathan Doe")
      )  );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 81 80" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   66 7b" -- Begin the modify request protocol op
         +"      04 24 75 69 64 3d 6a 64 6f 65" -- The DN of the entry to modify (octet string
         +"            2c 6f 75 3d 50 65 6f 70" -- "uid=jdoe,ou=People,dc=example,dc=com")
         +"            6c 65 2c 64 63 3d 65 78"
         +"            61 6d 70 6c 65 2c 64 63"
         +"            3d 63 6f 6d"
         +"      30 53" -- Begin the sequence of modifications
         +"         30 18" -- Begin the sequence for the first modification
         +"            0a 01 01" -- The delete modification type (enumerated value 1)
         +"            30 13" -- Begin the attribute sequence
         +" 04 09 67 69 76 65 6e 4e 61 6d" -- The attribute description
         +"       65"                      -- (octet string "givenName")
         +" 31 06" -- Begin the attribute value set
         +"    04 04 4a 6f 68 6e" -- The attribute value (octet string "John")
         +"         30 1c" -- Begin the sequence for the second modification
         +"            0a 01 00" -- The add modification type (enumerated value 0)
         +"            30 17" -- Begin the attribute sequence
         +" 04 09 67 69 76 65 6e 4e 61 6d" -- The attribute description
         +"       65"                      -- (octet string "givenName")
         +" 31 0a"  -- Begin the attribute value set
         +"    04 08 4a 6f 6e 61 74 68 61 6e" -- The attribute value
                                             -- (octet string "Jonathan")
         +"         30 19" -- Begin the sequence for the third modification
         +"            0a 01 02" -- The replace modification type (enumerated value 2)
         +"            30 14" -- Begin the attribute sequence
         +" 04 02 63 6e" -- The attribute description (octet string "cn")
         +" 31 0e" -- Begin the attribute value set
         +"    04 0c 4a 6f 6e 61 74 68 61 6e" -- The attribute value
         +"          20 44 6f 65",             -- (octet string "Jonathan Doe")
         "Modify request (using updates list)"
      );

      ------------------------------------------------------------------
      -- Modify response
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Modify_Response (M, Success_Code);
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 0c" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   67 07" -- Begin the modify response protocol op
         +"      0a 01 00" -- success result code (enumerated value 0)
         +"      04 00" -- No matched DN (0-byte octet string)
         +"      04 00", -- No diagnostic message (0-byte octet string)
         "Modify response"
      );
      ------------------------------------------------------------------
      -- Modify DN request
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Modify_DN_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         Value ("uid=john.doe"),
         True
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 3c" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   6c 37" -- Begin the modify DN request protocol op
         +"      04 24 75 69 64 3d 6a 64 6f 65" -- The DN of the entry to rename (octet string
         +"            2c 6f 75 3d 50 65 6f 70" -- "uid=jdoe,ou=People,dc=example,dc=com")
         +"            6c 65 2c 64 63 3d 65 78"
         +"            61 6d 70 6c 65 2c 64 63"
         +"            3d 63 6f 6d"
         +"      04 0c 75 69 64 3d 6a 6f 68 6e" -- The new RDN (octet string "uid=john.doe")
         +"            2e 64 6f 65"
         +"      01 01 ff", -- Delete the old RDN value (boolean true)
         "Modify DN request"
      );
      Erase (Buffer);
      M.Message_ID.Value := 3;
      Set_Modify_DN_Request
      (  M,
         Value ("uid=john.doe,ou=People,dc=example,dc=com"),
         Value ("uid=john.doe"),
         False,
         Value ("ou=Users,dc=example,dc=com")
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 5c" -- Begin the LDAPMessage sequence
         +"   02 01 03" -- The message ID (integer value 3)
         +"   6c 57" -- Begin the modify DN request protocol op
         +"      04 28 75 69 64 3d 6a 6f 68 6e" -- The DN of the entry to move (octet string
         +"            2e 64 6f 65 2c 6f 75 3d" -- "uid=john.doe,ou=People,dc=example,dc=com")
         +"            50 65 6f 70 6c 65 2c 64"
         +"            63 3d 65 78 61 6d 70 6c"
         +"            65 2c 64 63 3d 63 6f 6d"
         +"      04 0c 75 69 64 3d 6a 6f 68 6e" -- The new RDN (octet string "uid=john.doe")
         +"            2e 64 6f 65"
         +"      01 01 00" -- Don't delete the old RDN value (boolean false)
         +"      80 1a 6f 75 3d 55 73 65 72 73" -- The new superior DN (octet string
         +"            2c 64 63 3d 65 78 61 6d" -- "ou=Users,dc=example,dc=com" with type
         +"            70 6c 65 2c 64 63 3d 63" -- context-specific primitive zero)
         +"            6f 6d",
         "Modify DN request 2"
      );
      ------------------------------------------------------------------
      -- Modify DN response
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Modify_DN_Response (M, Success_Code);
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 0c" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   6d 07" -- Begin the modify DN response protocol op
         +"      0a 01 00" -- success result code (enumerated value 0)
         +"      04 00" -- No matched DN (0-byte octet string)
         +"      04 00", -- No diagnostic message (0-byte octet string)
         "Modify DN response"
      );
      ------------------------------------------------------------------
      -- Search request
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "uid"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"87 03 75 69 64", -- The octet string "uid" with type context-specific primitive seven
         "Search request filter present"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         Present ("uid")
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"87 03 75 69 64", -- The octet string "uid" with type context-specific primitive seven
         "Search request filter present (using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "uid",
         Equal,
         "jdoe"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a3 0b" -- Begin the AttributeValueAssertion sequence with type
                 -- context-specific constructed three
         +"   04 03 75 69 64" -- The attribute description (octet string "uid")
         +"   04 04 6a 64 6f 65", -- The assertion value (octet string "jdoe")
         "Search request filter equal"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "uid" = "jdoe"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a3 0b" -- Begin the AttributeValueAssertion sequence with type
                 -- context-specific constructed three
         +"   04 03 75 69 64" -- The attribute description (octet string "uid")
         +"   04 04 6a 64 6f 65", -- The assertion value (octet string "jdoe")
         "Search request filter equal (using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "createTimestamp",
         Greater_Or_Equal,
         "20170102030405.678Z"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a5 26" -- Begin the AttributeValueAssertion sequence with type
                 -- context-specific constructed five
         +"   04 0f 63 72 65 61 74 65 54 69" -- The attribute description
         +"         6d 65 73 74 61 6d 70"    -- (octet string "createTimestamp")
         +"   04 13 32 30 31 37 30 31 30 32" -- The assertion value
         +"         30 33 30 34 30 35 2e 36" -- (octet string "20170102030405.678Z")
         +"         37 38 5a",
         "Search request filter greater of equal"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "createTimestamp" >= "20170102030405.678Z"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a5 26" -- Begin the AttributeValueAssertion sequence with type
                 -- context-specific constructed five
         +"   04 0f 63 72 65 61 74 65 54 69" -- The attribute description
         +"         6d 65 73 74 61 6d 70"    -- (octet string "createTimestamp")
         +"   04 13 32 30 31 37 30 31 30 32" -- The assertion value
         +"         30 33 30 34 30 35 2e 36" -- (octet string "20170102030405.678Z")
         +"         37 38 5a",
         "Search request filter greater of equal " &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "accountBalance",
         Less_Or_Equal,
         "1234"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a6 16" -- Begin the AttributeValueAssertion sequence with type
                 -- context-specific constructed six
         +"   04 0e 61 63 63 6f 75 6e 74 42" -- The attribute description
         +"         61 6c 61 6e 63 65"       -- (octet string "accountBalance")
         +"   04 04 31 32 33 34",            -- The assertion value (octet string "1234")
         "Search request filter less of equal"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "accountBalance" <= "1234"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a6 16" -- Begin the AttributeValueAssertion sequence with type
                 -- context-specific constructed six
         +"   04 0e 61 63 63 6f 75 6e 74 42" -- The attribute description
         +"         61 6c 61 6e 63 65"       -- (octet string "accountBalance")
         +"   04 04 31 32 33 34",            -- The assertion value (octet string "1234")
         "Search request filter less of equal" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "givenName",
         Approximately_Equal,
         "John"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a8 11" -- Begin the AttributeValueAssertion sequence with type
                 -- context-specific constructed eight
         +"   04 09 67 69 76 65 6e 4e 61 6d" -- The attribute description
         +"         65"                      -- (octet string "givenName")
         +"   04 04 4a 6f 68 6e",            -- The assertion value (octet string "John")
         "Search request filter approximately equal"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         Like ("givenName", "John")
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a8 11" -- Begin the AttributeValueAssertion sequence with type
                 -- context-specific constructed eight
         +"   04 09 67 69 76 65 6e 4e 61 6d" -- The attribute description
         +"         65"                      -- (octet string "givenName")
         +"   04 04 4a 6f 68 6e",            -- The assertion value (octet string "John")
         "Search request filter approximately equal" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "cn",
         Initial_Component,
         "abc"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a4 0b" -- Begin the SubstringFilter sequence with type
                 -- context-specific constructed four
         +"   04 02 63 6e" -- The attribute description (octet string "cn")
         +"   30 05" -- Begin the substrings sequence
         +"      80 03 61 62 63", -- The initial element (octet string "abc") with type
                                  -- context-specific primitive zero
         "Search request filter substring initial"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "cn" = Prefix ("abc")
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a4 0b" -- Begin the SubstringFilter sequence with type
                 -- context-specific constructed four
         +"   04 02 63 6e" -- The attribute description (octet string "cn")
         +"   30 05" -- Begin the substrings sequence
         +"      80 03 61 62 63", -- The initial element (octet string "abc") with type
                                  -- context-specific primitive zero
         "Search request filter substring initial" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "cn",
         Any_Component,
         "lmn"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a4 0b" -- Begin the SubstringFilter sequence with type
                 -- context-specific constructed four
         +"   04 02 63 6e" -- The attribute description (octet string "cn")
         +"   30 05" -- Begin the substrings sequence
         +"      81 03 6c 6d 6e", -- The any element (octet string "lmn") with type
                                  -- context-specific primitive one "abc") with type
                                  -- context-specific primitive zero
         "Search request filter substring any"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "cn" = Substring ("lmn")
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a4 0b" -- Begin the SubstringFilter sequence with type
                 -- context-specific constructed four
         +"   04 02 63 6e" -- The attribute description (octet string "cn")
         +"   30 05" -- Begin the substrings sequence
         +"      81 03 6c 6d 6e", -- The any element (octet string "lmn") with type
                                  -- context-specific primitive one "abc") with type
                                  -- context-specific primitive zero
         "Search request filter substring any" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "cn",
         Final_Component,
         "xyz"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a4 0b" -- Begin the SubstringFilter sequence with type
                  -- context-specific constructed four
         +"   04 02 63 6e" -- The attribute description (octet string "cn")
         +"   30 05" -- Begin the substrings sequence
         +"      82 03 78 79 7a", -- The final element (octet string "xyz") with type
                                  -- context-specific primitive two
         "Search request filter substring final"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "cn" = Suffix ("xyz")
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a4 0b" -- Begin the SubstringFilter sequence with type
                  -- context-specific constructed four
         +"   04 02 63 6e" -- The attribute description (octet string "cn")
         +"   30 05" -- Begin the substrings sequence
         +"      82 03 78 79 7a", -- The final element (octet string "xyz") with type
                                  -- context-specific primitive two
         "Search request filter substring final" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "cn",
         Initial_Component,
         "abc"
      );
      Set_Search_Request_Substring (M, Any_Component,   "def");
      Set_Search_Request_Substring (M, Any_Component,   "lmn");
      Set_Search_Request_Substring (M, Any_Component,   "uvw");
      Set_Search_Request_Substring (M, Final_Component, "xyz");
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a4 1f" -- Begin the SubstringFilter sequence with type
                  -- context-specific constructed four
         +"   04 02 63 6e" -- The attribute description (octet string "cn")
         +"   30 19" -- Begin the substrings sequence
         +"      80 03 61 62 63" -- The initial element (octet string "abc") with type
                                 -- context-specific primitive zero
         +"      81 03 64 65 66" -- The first any element (octet string "def") with type
                                 -- context-specific primitive one
         +"      81 03 6c 6d 6e" -- The second any element (octet string "lmn") with type
                                 -- context-specific primitive one
         +"      81 03 75 76 77" -- The third any element (octet string "uvw") with type
                                 -- context-specific primitive one
         +"      82 03 78 79 7a", -- The final element (octet string "xyz") with type
                                  -- context-specific primitive two
         "Search request filter substring multiple"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "cn" = Prefix    ("abc")
              / Substring ("def")
              / Substring ("lmn")
              / Substring ("uvw")
              / Suffix    ("xyz")
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a4 1f" -- Begin the SubstringFilter sequence with type
                  -- context-specific constructed four
         +"   04 02 63 6e" -- The attribute description (octet string "cn")
         +"   30 19" -- Begin the substrings sequence
         +"      80 03 61 62 63" -- The initial element (octet string "abc") with type
                                 -- context-specific primitive zero
         +"      81 03 64 65 66" -- The first any element (octet string "def") with type
                                 -- context-specific primitive one
         +"      81 03 6c 6d 6e" -- The second any element (octet string "lmn") with type
                                 -- context-specific primitive one
         +"      81 03 75 76 77" -- The third any element (octet string "uvw") with type
                                 -- context-specific primitive one
         +"      82 03 78 79 7a", -- The final element (octet string "xyz") with type
                                  -- context-specific primitive two
         "Search request filter substring multiple" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "jdoe",
         Rule        => "",
         Description => "uid"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a9 0b" -- Begin the MatchingRuleAssertion sequence with type
                  -- context-specific constructed nine
         +"   82 03 75 69 64" -- The attribute description (octet string "uid" with type
                              -- context-specific primitive two)
         +"   83 04 6a 64 6f 65", -- The assertion value (octet string "jdoe" with type
                                  -- context-specific primitive three
         "Search request filter matching rule"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         Extended
         (  Value       => "jdoe",
            Rule        => "",
            Description => "uid"
      )  );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a9 0b" -- Begin the MatchingRuleAssertion sequence with type
                  -- context-specific constructed nine
         +"   82 03 75 69 64" -- The attribute description (octet string "uid" with type
                              -- context-specific primitive two)
         +"   83 04 6a 64 6f 65", -- The assertion value (octet string "jdoe" with type
                                  -- context-specific primitive three
         "Search request filter matching rule" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "foo",
         "caseIgnoreMatch"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a9 16" -- Begin the MatchingRuleAssertion sequence with type
                  -- context-specific constructed nine
         +"   81 0f 63 61 73 65 49 67 6e 6f" -- The matching rule ID (octet string
         +"         72 65 4d 61 74 63 68"    -- "caseIgnoreMatch" with type
                                            -- context-specific primitive one)
         +"   83 03 66 6f 6f", -- The assertion value (octet string "foo" with type
                               -- context-specific primitive three
         "Search request filter matching rule (ID)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         Extended
         (  Value => "foo",
            Rule  => "caseIgnoreMatch"
      )  );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a9 16" -- Begin the MatchingRuleAssertion sequence with type
                  -- context-specific constructed nine
         +"   81 0f 63 61 73 65 49 67 6e 6f" -- The matching rule ID (octet string
         +"         72 65 4d 61 74 63 68"    -- "caseIgnoreMatch" with type
                                            -- context-specific primitive one)
         +"   83 03 66 6f 6f", -- The assertion value (octet string "foo" with type
                               -- context-specific primitive three
         "Search request filter matching rule (ID)" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "jdoe",
         "caseIgnoreMatch",
         "uid",
         True
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a9 1f" -- Begin the MatchingRuleAssertion sequence with type
                 -- context-specific constructed nine
         +"   81 0f 63 61 73 65 49 67 6e 6f" -- The matching rule ID (octet string
         +"         72 65 4d 61 74 63 68"    -- "caseIgnoreMatch" with type
         +"   82 03 75 69 64" -- The attribute description (octet string "uid" with type
                              -- context-specific primitive two)
         +"   83 04 6a 64 6f 65" -- The assertion value (octet string "jdoe" with type
                                 -- context-specific primitive three
         +"   84 01 ff", -- The dnAttributes flag (boolean true)
         "Search request filter matching rule (full)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         Extended
         (  Value       => "jdoe",
            Rule        => "caseIgnoreMatch",
            Description => "uid",
            Attributes  => True
      )  );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a9 1f" -- Begin the MatchingRuleAssertion sequence with type
                 -- context-specific constructed nine
         +"   81 0f 63 61 73 65 49 67 6e 6f" -- The matching rule ID (octet string
         +"         72 65 4d 61 74 63 68"    -- "caseIgnoreMatch" with type
         +"   82 03 75 69 64" -- The attribute description (octet string "uid" with type
                              -- context-specific primitive two)
         +"   83 04 6a 64 6f 65" -- The assertion value (octet string "jdoe" with type
                                 -- context-specific primitive three
         +"   84 01 ff", -- The dnAttributes flag (boolean true)
         "Search request filter matching rule (full)" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set
      (  Set_Search_Request_Conjunction
         (  M,
            Value ("uid=jdoe,ou=People,dc=example,dc=com")
         ) .all,
         "givenName",
         Equal,
         "John"
      );
      Set
      (  Append_Term (Get_Search_Request_Filter (M)).all,
         "sn",
         Equal,
         "Doe"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a0 1e" -- Begin the and set with type context-specific constructed zero
         +"   a3 11" -- Begin the AttributeValueAssertion sequence with type
                     -- context-specific constructed three
         +"      04 09 67 69 76 65 6e 4e 61 6d" -- The attribute description
         +"            65"                      -- (octet string "givenName")
         +"      04 04 4a 6f 68 6e" -- The assertion value (octet string "John")
         +"   a3 09" -- Begin the AttributeValueAssertion sequence with type
                     -- context-specific constructed three
         +"      04 02 73 6e" -- The attribute description (octet string "sn")
         +"      04 03 44 6f 65", -- The assertion value (octet string "Doe")
         "Search request conjunction filter"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "givenName" = "John" and "sn" = "Doe"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a0 1e" -- Begin the and set with type context-specific constructed zero
         +"   a3 11" -- Begin the AttributeValueAssertion sequence with type
                     -- context-specific constructed three
         +"      04 09 67 69 76 65 6e 4e 61 6d" -- The attribute description
         +"            65"                      -- (octet string "givenName")
         +"      04 04 4a 6f 68 6e" -- The assertion value (octet string "John")
         +"   a3 09" -- Begin the AttributeValueAssertion sequence with type
                     -- context-specific constructed three
         +"      04 02 73 6e" -- The attribute description (octet string "sn")
         +"      04 03 44 6f 65", -- The assertion value (octet string "Doe")
         "Search request conjunction filter" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request_Conjunction
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com")
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a0 00", -- An empty and set with type context-specific constructed zero
         "Search request null conjunction filter"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         Success
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a0 00", -- An empty and set with type context-specific constructed zero
         "Search request null conjunction filter" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set
      (  Set_Search_Request_Disjunction
         (  M,
            Value ("uid=jdoe,ou=People,dc=example,dc=com")
         ) .all,
         "givenName",
         Equal,
         "John"
      );
      Set
      (  Append_Term (Get_Search_Request_Filter (M)).all,
         "givenName",
         Equal,
         "Jonathan"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a1 2a" -- Begin the or set with type context-specific constructed one
         +"   a3 11" -- Begin the AttributeValueAssertion sequence with type
                     -- context-specific constructed three
         +"      04 09 67 69 76 65 6e 4e 61 6d" -- The attribute description
         +"            65"                      -- (octet string "givenName")
         +"      04 04 4a 6f 68 6e" -- The assertion value (octet string "John")
         +"   a3 15" -- Begin the AttributeValueAssertion sequence with type
                     -- context-specific constructed three
         +"      04 09 67 69 76 65 6e 4e 61 6d"  -- The attribute description
         +"            65"                       -- (octet string "givenName")
         +"      04 08 4a 6f 6e 61 74 68 61 6e", -- The assertion value (octet string "Jonathan")
         "Search request disjunction filter"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         "givenName" = "John" or "givenName" = "Jonathan"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a1 2a" -- Begin the or set with type context-specific constructed one
         +"   a3 11" -- Begin the AttributeValueAssertion sequence with type
                     -- context-specific constructed three
         +"      04 09 67 69 76 65 6e 4e 61 6d" -- The attribute description
         +"            65"                      -- (octet string "givenName")
         +"      04 04 4a 6f 68 6e" -- The assertion value (octet string "John")
         +"   a3 15" -- Begin the AttributeValueAssertion sequence with type
                     -- context-specific constructed three
         +"      04 09 67 69 76 65 6e 4e 61 6d"  -- The attribute description
         +"            65"                       -- (octet string "givenName")
         +"      04 08 4a 6f 6e 61 74 68 61 6e", -- The assertion value (octet string "Jonathan")
         "Search request disjunction filter" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request_Disjunction
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com")
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a1 00", -- An empty and set with type context-specific constructed zero
         "Search request null disjunction filter"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         Failure
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a1 00", -- An empty and set with type context-specific constructed zero
         "Search request null disjunction filter" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set
      (  Set_Search_Request_Negation
         (  M,
            Value ("uid=jdoe,ou=People,dc=example,dc=com")
         ) .all,
         "givenName",
         Equal,
         "John"
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a2 13" -- Begin the not filter with type context-specific constructed two
         +"   a3 11" -- Begin the AttributeValueAssertion sequence with type
                     -- context-specific constructed three
         +"      04 09 67 69 76 65 6e 4e 61 6d" -- The attribute description
         +"            65"                      -- (octet string "givenName")
         +"      04 04 4a 6f 68 6e", -- The assertion value (octet string "John")
         "Search request negation filter"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request
      (  M,
         Value ("uid=jdoe,ou=People,dc=example,dc=com"),
         not ("givenName" = "John")
      );
      Pointer := 1;
      Encode (M.Protocol_Op.Search_Request.Filter, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"a2 13" -- Begin the not filter with type context-specific constructed two
         +"   a3 11" -- Begin the AttributeValueAssertion sequence with type
                     -- context-specific constructed three
         +"      04 09 67 69 76 65 6e 4e 61 6d" -- The attribute description
         +"            65"                      -- (octet string "givenName")
         +"      04 04 4a 6f 68 6e", -- The assertion value (octet string "John")
         "Search request negation filter" &
         "(using filter expression)"
      );

      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Request_Conjunction
      (  Message       => M,
         Name          => Value ("dc=example,dc=com"),
         Scope         => Whole_Subtree_Scope,
         Aliasing_Mode => Never_Deref_Aliases,
         Size_Limit    => 1_000,
         Time_Limit    => 30.0,
         Types_Only    => False
      );
      Set
      (  Append_Term (Get_Search_Request_Filter (M)).all,
         "objectClass",
         Equal,
         "person"
      );
      Set
      (  Append_Term (Get_Search_Request_Filter (M)).all,
         "uid",
         Equal,
         "jdoe"
      );
      Set_Search_Request_Attribute (M, "*");
      Set_Search_Request_Attribute (M, "+");
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 56" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   63 51" -- Begin the search request protocol op
         +"      04 11 64 63 3d 65 78 61 6d 70" -- The search base DN
         +"            6c 65 2c 64 63 3d 63 6f" -- (octet string "dc=example,dc=com")
         +"            6d"
         +"      0a 01 02" -- The wholeSubtree scope (enumerated value 2)
         +"      0a 01 00" -- The neverDerefAliases policy (enumerated value 0)
         +"      02 02 03 e8" -- The size limit (integer value 1000)
         +"      02 01 1e" -- The time limit (integer value 30)
         +"      01 01 00" -- The typesOnly flag (boolean false)
         +"      a0 24" -- Begin an and filter
         +"         a3 15" -- Begin an equality filter
         +"            04 0b 6f 62 6a 65 63 74 43 6c" -- The attribute description
         +"    61 73 73"                -- (octet string "objectClass")
         +"            04 06 70 65 72 73 6f 6e" -- The assertion value (octet string "person")
         +"         a3 0b" -- Begin an equality filter
         +"            04 03 75 69 64" -- The attribute description (octet string "uid")
         +"            04 04 6a 64 6f 65" -- The assertion value (octet string "jdoe")
         +"      30 06" -- Begin the set of requested attributes
         +"         04 01 2a"  -- Request all user attributes (octet string "*")
         +"         04 01 2b", -- Request all operational attributes (octet string "+")
         "Search request attributes"
      );
      ------------------------------------------------------------------
      -- Search result entry
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Result_Entry
      (  M,
         Value ("dc=example,dc=com"),
         "objectClass",
         "top"
      );
      Set_Attribute_Value (M, "domain");
      Set_Add_Attribute (M, "dc", "example");
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 49" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   64 44" -- Begin the search result entry protocol op
         +"      04 11 64 63 3d 65 78 61 6d 70" -- The entry DN
         +"            6c 65 2c 64 63 3d 63 6f" -- (octet string "dc=example,dc=com")
         +"            6d"
         +"      30 2f" -- Begin the sequence of attributes
         +"         30 1c" -- Begin the first attribute sequence
         +"            04 0b 6f 62 6a 65 63 74 43 6c" -- The attribute description
         +"   61 73 73"                -- (octet string "objectClass")
         +"            31 0d" -- Begin the set of objectClass values
         +"04 03 74 6f 70" -- The first value (octet string "top")
         +"04 06 64 6f 6d 61 69 6e" -- The second value (octet string "domain")
         +"         30 0f" -- Begin the second attribute sequence
         +"            04 02 64 63" -- The attribute description (octet string "dc")
         +"            31 09" -- Begin the set of dc values
         +"04 07 65 78 61 6d 70 6c 65", -- The value (octet string "example")
         "Search result entry"
      );
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Result_Entry
      (  M,
         Value ("dc=example,dc=com"),
         "objectClass"
      );
      Set_Add_Attribute (M, "dc");
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 33" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   64 2e" -- Begin the search result entry protocol op
         +"      04 11 64 63 3d 65 78 61 6d 70" -- The entry DN
         +"            6c 65 2c 64 63 3d 63 6f" -- (octet string "dc=example,dc=com")
         +"            6d"
         +"      30 19" -- Begin the sequence of attributes
         +"         30 0f" -- Begin the first attribute sequence
         +"            04 0b 6f 62 6a 65 63 74 43 6c" -- The attribute description
         +"    61 73 73"                -- (octet string "objectClass")
         +"            31 00" -- The empty value set
         +"         30 06" -- Begin the second attribute sequence
         +"            04 02 64 63" -- The attribute description (octet string "dc")
         +"            31 00", -- The empty value set
         "Search result entry (types only)"
      );
      ------------------------------------------------------------------
      -- Search result reference
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Result_Reference
      (  M,
         "ldap://ds1.example.com:389/dc=example,dc=com??sub?"
      );
      Set_Search_Result_Reference_URI
      (  M,
         "ldap://ds2.example.com:389/dc=example,dc=com??sub?"
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 6d" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   73 68" -- Begin the search result reference protocol op
         +"      04 32 6c 64 61 70 3a 2f 2f 64" -- The first referral URI (octet string "ldap://
         +"            73 31 2e 65 78 61 6d 70" -- ds1.example.com:389/dc=example,dc=com??sub?")
         +"            6c 65 2e 63 6f 6d 3a 33"
         +"            38 39 2f 64 63 3d 65 78"
         +"            61 6d 70 6c 65 2c 64 63"
         +"            3d 63 6f 6d 3f 3f 73 75"
         +"            62 3f"
         +"      04 32 6c 64 61 70 3a 2f 2f 64" -- The second referral URI (octet string "ldap://
         +"            73 32 2e 65 78 61 6d 70" -- ds2.example.com:389/dc=example,dc=com??sub?")
         +"            6c 65 2e 63 6f 6d 3a 33"
         +"            38 39 2f 64 63 3d 65 78"
         +"            61 6d 70 6c 65 2c 64 63"
         +"            3d 63 6f 6d 3f 3f 73 75"
         +"            62 3f",
         "Search result reference"
      );
      ------------------------------------------------------------------
      -- Search result done
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Search_Result_Done (M, Success_Code);
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 0c" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   65 07" -- Begin the search result done protocol op
         +"      0a 01 00" -- success result code (enumerated value 0)
         +"      04 00"  -- No matched DN (0-byte octet string)
         +"      04 00", -- No diagnostic message (0-byte octet string)
         "Search result done"
      );
      ------------------------------------------------------------------
      -- Unbind request
      --
      Erase (Buffer);
      M.Message_ID.Value := 3;
      Set_Unbind_Request (M);
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 05" -- Begin the LDAPMessage sequence
         +"   02 01 03" -- The message ID (integer value 3)
         +"   42 00", -- The unbind request protocol op
         "Unbind request"
      );
      ------------------------------------------------------------------
      -- Intermediate response
      --
      Erase (Buffer);
      M.Message_ID.Value := 2;
      Set_Intermediate_Response
      (  Message => M,
         Name    => (1, 3, 6, 1, 4, 1, 4203, 1, 9, 1, 4),
         Value   => Character'Val (16#80#) &
                    Character'Val (16#09#) &
                    "NomNomNom"
      );
      Pointer := 1;
      Encode (M, Request, Pointer);
      Compare
      (  Request (1..Pointer - 1),
         +"30 2c" -- Begin the LDAPMessage sequence
         +"   02 01 02" -- The message ID (integer value 2)
         +"   79 27" -- Begin the intermediate response protocol op
         +"      80 18 31 2e 33 2e 36 2e 31 2e" -- The responseName (octet string
         +"            34 2e 31 2e 34 32 30 33" -- "1.3.6.1.4.1.4203.1.9.1.4")
         +"            2e 31 2e 39 2e 31 2e 34"
         +"      81 0b" -- Begin the responseValue element
         +"         80 09 4e 6f 6d 4e 6f 6d 4e 6f" -- The syncCookie value
         +" 6d",                      -- (octet string "NomNomNom")
         "Intermediate request"
      );

      Test_Feed
      (  +"30 0C"
         +"   02 01 01"
         +"   61 07"
         +"   0A 01 00"
         +"   04 00"
         +"   04 00",
         "Bind response"
      );
   exception
      when Error : others =>
         Put_Line ("Error: " & Exception_Information (Error));
         return;
   end;
--end if;
--if false then
   --
   -- Open LDAP server
   --
   declare
      Address   : constant String := "ldap.forumsys.com";
      Port      : constant := 389;
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
         new LDAP_Client
             (  Listener           => Server'Unchecked_Access,
                Message_Length     => 512,
                Incoming_Data_Size => 1024 * 10,
                Outgoing_Data_Size => 1024 * 10,
                Input_Size         => 512,
                Output_Size        => 512
      )      );
      declare
         Client : LDAP_Client renames
                  LDAP_Client (Ptr (Reference).all);

         procedure Test_Bind_1 is
            File : File_Type;
         begin
            Connect (Client, Address, Port, 3, 5.0);
            Put_Line ("LDAP client connected to " & Address);
            Send_Bind
            (  Client,
               Value ("cn=read-only-admin,dc=example,dc=com"),
              "password",
               5.0
            );
            Put_Line ("LDAP bind successful");
            declare
               Result : constant Search_Result :=
                  Send_Search
                  (  Client,
                     Value ("dc=example,dc=com"),
                     Present ("objectClass"),
                     Timeout => 5.0
                  );
            begin
               Put_Line
               (  Image (Get_Entries_Number (Result))
               &  " entries found"
               );
               for Index in 1..Get_Entries_Number (Result) loop
                  Put_Line
                  (  Image (Index)
                  &  " "
                  &  Image (Get_Entry_Name (Result, Index))
                  );
                  declare
                     Attributes : constant Attributes_List :=
                                  Get_Entry_Attributes (Result, Index);
                  begin
                     for Item in 1..Get_Length (Attributes) loop
                        Put_Line
                        (  "   "
                        &  Image (Item)
                        &  " "
                        &  Get_Description (Attributes, Item)
                        );
                        declare
                           Values : constant Values_List :=
                                    Get_Values (Attributes, Item);
                        begin
                           for No in 1..Get_Length (Values) loop
                              Put_Line
                              (  "      "
                              &  Image (No)
                              &  " = "
                              &  Get (Values, No)
                              );
                           end loop;
                        end;
                     end loop;
                  end;
               end loop;
            end;
            Put_Line
            (  "Compare = "
            &  Boolean'Image
               (  Send_Compare
                  (  Client,
                     Value ("uid=euclid,dc=example,dc=com"),
                     "uid",
                     "euclid",
                     Timeout => 5.0
            )  )  );
            delay 4.0;
         end Test_Bind_1;
      begin
         Put_Line ("LDAP client started");
         Test_Bind_1;
      end;
   end;
--end if;
   --
   -- Local LDAP server Apache DS default configuratin
   --
   declare
      Address   : constant String := "127.0.01";
      Port      : constant := 10389;
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
         new LDAP_Client
             (  Listener           => Server'Unchecked_Access,
                Message_Length     => 512,
                Incoming_Data_Size => 1024 * 10,
                Outgoing_Data_Size => 1024 * 10,
                Input_Size         => 512,
                Output_Size        => 512
      )      );
      declare
         Client : LDAP_Client renames
                  LDAP_Client (Ptr (Reference).all);

         procedure Test_1 is
            File : File_Type;
         begin
            Connect (Client, Address, Port, 3, 5.0);
            Put_Line ("LDAP client connected to " & Address);
            Send_Bind
            (  Client,
               Value ("uid=admin,ou=system"),
              "secret",
               5.0
            );
            Put_Line ("LDAP bind to Apache DS successful");
            declare
               Result : constant Search_Result :=
                  Send_Search
                  (  Client,
                     Value ("dc=example,dc=com"),
                     Present ("objectClass"),
                     Timeout => 5.0
                  );
            begin
               Put_Line
               (  Image (Get_Entries_Number (Result))
               &  " entries found"
               );
               for Index in 1..Get_Entries_Number (Result) loop
                  Put_Line
                  (  Image (Index)
                  &  " "
                  &  Image (Get_Entry_Name (Result, Index))
                  );
                  declare
                     Attributes : constant Attributes_List :=
                                  Get_Entry_Attributes (Result, Index);
                  begin
                     for Item in 1..Get_Length (Attributes) loop
                        Put_Line
                        (  "   "
                        &  Image (Item)
                        &  " "
                        &  Get_Description (Attributes, Item)
                        );
                        declare
                           Values : constant Values_List :=
                                    Get_Values (Attributes, Item);
                        begin
                           for No in 1..Get_Length (Values) loop
                              Put_Line
                              (  "      "
                              &  Image (No)
                              &  " = "
                              &  Get (Values, No)
                              );
                           end loop;
                        end;
                     end loop;
                  end;
               end loop;
            end;
            if not Exists
                   (  Client,
                      Value ("ou=USER,dc=example,dc=com"),
                      Timeout => 5.0
                   )
            then -- Create
               Send_Add
               (  Client,
                  Value ("ou=USER,dc=example,dc=com"),
                  (  "objectClass" - "organizationalUnit"
                  or "objectClass" - "top"
                  ),
                  Timeout => 5.0
               );
            end if;
            if Exists
               (  Client,
                  Value
                  (  "c=US+description=United\ States\ of\ America,"
                  &  "ou=USER,dc=example,dc=com"
                  ),
                  Timeout => 5.0
                )
            then -- Delete
               Send_Delete
               (  Client,
                  Value
                  (  "c=US+description=United\ States\ of\ America,"
                  &  "ou=USER,dc=example,dc=com"
               )  );
            end if;
            Send_Add
            (  Client,
               Value
               (  "c=US+description=United\ States\ of\ America,"
               &  "ou=USER,dc=example,dc=com"
               ),
               (  "objectClass" - "country"
               or "objectClass" - "top"
               or "description" - "United States of America"
               ),
               Timeout => 5.0
            );
            Send_Modify
            (  Client,
               Value
               (  "c=US+description=United\ States\ of\ America,"
               &  "ou=USER,dc=example,dc=com"
               ),
               Add ("description" - "Northern hemisphere"),
               Timeout => 5.0
            );
            delay 4.0;
         end Test_1;
      begin
         Put_Line ("LDAP client started");
         Test_1;
      end;

   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_LDAP_Client;
