--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Server.OpenSSL                 Luebeck            --
--  Implementation                                 Winter, 2019       --
--                                                                    --
--                                Last revision :  08:30 04 Aug 2022  --
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

with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with Synchronization.Mutexes;  use Synchronization.Mutexes;

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Server.OpenSSL is

   procedure Free is
      new Ada.Unchecked_Deallocation (Encoder'Class, Encoder_Ptr);

   package Conversions is
      new System.Address_To_Access_Conversions (OpenSSL_Session);

   function "+" (Ptr : chars_ptr) return String is
   begin
      if Ptr = Null_Ptr then
         return "";
      else
         return Value (Ptr);
      end if;
   end "+";

   Lock         : aliased Mutex;
   Init         : Boolean    := False;
   Read_Method  : BIO_METHOD := No_METHOD;
   Write_Method : BIO_METHOD := No_METHOD;

   function BIO_Control
            (  b   : BIO;
               cmd : BIO_CTRL;
               num : long;
               ptr : System.Address
            )  return long is
   begin
      case cmd is
         when BIO_CTRL_FLUSH | BIO_CTRL_PUSH | BIO_CTRL_POP =>
            return 1;
         when others =>
            return 0;
      end case;
   end BIO_Control;

   function BIO_Gets
            (  b    : BIO;
               data : Stream_Element_Pointers.Pointer;
               dlen : int
            )  return int is
      use Stream_Element_Pointers;
      Written : int     := 0;
      Count   : aliased size_t;
      Ptr     : Pointer := data;
   begin
      while Written < dlen loop
         if BIO_read (b, Ptr.all'Address, 1, Count'Access) <= 0 then
            return -1;
         end if;
         Written := Written + 1;
         exit when Ptr.all = 0;
         Increment (Ptr);
      end loop;
      return Written;
   end BIO_Gets;

   function BIO_Puts
            (  b    : BIO;
               data : Stream_Element_Pointers.Pointer
            )  return int is
      use Stream_Element_Pointers;
      Length : constant size_t := size_t (Virtual_Length (data));
      Count  : aliased size_t;
   begin
      if 0 < BIO_Write
             (  b,
                data.all'Address,
                size_t (Length),
                Count'Access
             )  then
         return int (Count);
      else
         return -1;
      end if;
   end BIO_Puts;

   function BIO_Read
            (  b         : BIO;
               data      : System.Address;
               dlen      : size_t;
               readbytes : access size_t
            )  return int is
      use Conversions;
      subtype Buffer_Type is
         Stream_Element_Array (1..Stream_Element_Count (dlen));
      Session : Object_Pointer := To_Pointer (BIO_get_data (b));
      Pointer : Stream_Element_Offset := 1;
      Buffer  : Buffer_Type;
      for Buffer'Address use data;
   begin
      BIO_clear_flags (b, BIO_FLAGS_RWS + BIO_FLAGS_SHOULD_RETRY);
      if Session = null then
         readbytes.all := 0;
      else
         Pull (Session.Client.Read, Buffer, Pointer);
         readbytes.all := size_t (Pointer - 1);
      end if;
      if dlen > readbytes.all then
         BIO_set_flags (b, BIO_FLAGS_READ + BIO_FLAGS_SHOULD_RETRY);
      end if;
      return 1;
   end BIO_Read;

   function BIO_Read
            (  b    : BIO;
               data : System.Address;
               dlen : int
            )  return int is
      Count : aliased size_t;
   begin
      if BIO_Read (b, data, size_t (dlen), Count'Access) > 0 then
         return int (Count);
      else
         return -1;
      end if;
   end BIO_Read;

   function BIO_Write
            (  b       : BIO;
               data    : System.Address;
               dlen    : size_t;
               written : access size_t
            )  return int is
      subtype Buffer_Type is
         Stream_Element_Array (1..Stream_Element_Count (dlen));
      Session : OpenSSL_Session renames
                Conversions.To_Pointer (BIO_get_data (b)).all;
      Client  : Connection'Class renames Session.Client.all;
      Last    : Stream_Element_Offset;
      Buffer  : Buffer_Type;
      for Buffer'Address use data;
   begin
      BIO_clear_flags (b, BIO_FLAGS_RWS + BIO_FLAGS_SHOULD_RETRY);
      Send_Socket (Get_Socket (Client), Buffer, Last);
      if Last > 0 then
         if Is_Trace_Received_On
            (  Client.Socket_Listener.Factory.all,
               Trace_Encoded
            )  then
            Trace_Sent
            (  Factory => Client.Socket_Listener.Factory.all,
               Client  => Client,
               Data    => Buffer,
               From    => Buffer'First,
               To      => Last,
               Encoded => True
            );
         end if;
         Client.Data_Sent := True;
      end if;
      written.all := size_t (Last);
      if dlen > written.all then
         BIO_set_flags (b, BIO_FLAGS_WRITE + BIO_FLAGS_SHOULD_RETRY);
      end if;
      return 1;
   exception
      when Error : Socket_Error =>
         Receive_Error (Client, Error);
         written.all := size_t (Last);
         return 0;
      when Error : others =>
         Trace_Error
         (  Client.Socket_Listener.Factory.all,
            "TLS transport writing",
            Error
         );
         written.all := size_t (Last);
         return 0;
   end BIO_Write;

   function BIO_Write
            (  b    : BIO;
               data : System.Address;
               dlen : int
            )  return int is
      Count : aliased size_t;
   begin
      if BIO_Write (b, data, size_t (dlen), Count'Access) > 0 then
         return int (Count);
      else
         return -1;
      end if;
   end BIO_Write;

   procedure Check_Private_Key (Session : SSL) is
   begin
      if SSL_check_private_key (Session) <= 0 then
         Check_Error (Use_Error'Identity);
      end if;
   end Check_Private_Key;

   procedure Check_Private_Key
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type
             )  is
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if SSL_CTX_check_private_key (Factory.Client_Context) <= 0 then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if SSL_CTX_check_private_key (Factory.Server_Context) <= 0 then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Check_Private_Key;

   procedure Clear_Options (Session : SSL; Options : SSL_OP) is
      Result : SSL_OP;
   begin
      Result := SSL_clear_options (Session, Options);
   end Clear_Options;

   procedure Clear_Options
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                Options : SSL_OP
             )  is
      Result : SSL_OP;
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         Result :=
            SSL_CTX_clear_options (Factory.Client_Context, Options);
      end if;
      if Context in Any_Context..Server_Context then
         Result :=
            SSL_CTX_clear_options (Factory.Server_Context, Options);
      end if;
   end Clear_Options;

   procedure Create_Context
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type
             )  is
      Result : int;
   begin
      declare
         Exclusive : Holder (Lock'Access);
      begin
         if not Init then
            if OPENSSL_init_ssl (OPENSSL_INIT_SSL_DEFAULT) <= 0 then
               Check_Error (Status_Error'Identity);
            end if;
            Read_Method :=
               BIO_meth_new (BIO_get_new_index, "GNAT sockets read");
            Result := BIO_meth_set_gets (Read_Method, BIO_Gets'Access);
            Result :=
               BIO_meth_set_read_ex (Read_Method, BIO_Read'Access);
            Result :=
               BIO_meth_set_ctrl (Read_Method, BIO_Control'Access);

            Write_Method :=
               BIO_meth_new (BIO_get_new_index, "GNAT sockets write");
            Result := BIO_meth_set_puts (Write_Method, BIO_Puts'Access);
            Result :=
               BIO_meth_set_write_ex (Write_Method, BIO_Write'Access);
            Result :=
               BIO_meth_set_ctrl (Write_Method, BIO_Control'Access);
            Init := True;
         end if;
      end;
      if Context in Client_Context..Any_Context then
         if Factory.Client_Context = No_SSL_CTX then
            Factory.Client_Context := SSL_CTX_new (TLS_client_method);
            if Factory.Client_Context = No_SSL_CTX then
               Check_Error (Data_Error'Identity);
            end if;
            Result := SSL_CTX_up_ref (Factory.Client_Context);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         Factory.Server_Context := SSL_CTX_new (TLS_server_method);
         if Factory.Server_Context = No_SSL_CTX then
            Check_Error (Data_Error'Identity);
         end if;
         Result := SSL_CTX_up_ref (Factory.Server_Context);
      end if;
   end Create_Context;

   function Create_Transport
            (  Factory  : access Abstract_OpenSSL_Factory;
               Listener : access Connections_Server'Class;
               Client   : access Connection'Class
            )  return Encoder_Ptr is
      Result : Encoder_Ptr;
   begin
      if Client.Client then
         Create_Context (Factory.all, Client_Context);
         Result := new OpenSSL_Session
                       (  Client.all'Unchecked_Access,
                          Factory.Decoded_Size
                       );
      else
         Create_Context (Factory.all, Server_Context);
         Result := new OpenSSL_Session
                       (  Client.all'Unchecked_Access,
                          Factory.Decoded_Size
                       );
      end if;
      declare
         Session : OpenSSL_Session renames OpenSSL_Session (Result.all);
         Self    : Abstract_OpenSSL_Factory'Class renames
                   Abstract_OpenSSL_Factory'Class (Factory.all);
      begin
         if Client.Client then
            Session.SSL_Session := SSL_new (Factory.Client_Context);
            SSL_set_connect_state (Session.SSL_Session);
            if Session.SSL_Session = No_SSL then
               Check_Error (Data_Error'Identity);
            end if;
            Factory.Client_Context :=  No_SSL_CTX;
         else
            Session.SSL_Session := SSL_new (Factory.Server_Context);
            SSL_set_accept_state (Session.SSL_Session);
            if Session.SSL_Session = No_SSL then
               Check_Error (Data_Error'Identity);
            end if;
            Factory.Server_Context :=  No_SSL_CTX;
         end if;
         Session.Input := BIO_new (Read_Method);
         BIO_set_data (Session.Input,  Session'Address);
         SSL_set0_rbio (Session.SSL_Session, Session.Input);

         Session.Output := BIO_new (Write_Method);
         BIO_set_data (Session.Output, Session'Address);
         SSL_set0_wbio (Session.SSL_Session, Session.Output);

         if Factory.Trace_Session then
            Trace (Self, "OpenSSL setting up session");
         end if;
         Prepare (Self, Client.all, Session.SSL_Session);
         if Factory.Trace_Session then
            Trace (Self, "OpenSSL handshake engaged");
         end if;
      end;
      return Result;
   exception
      when Error : others =>
         Free (Result);
         Trace_Error
         (  Factory.all,
            "OpenSSL transport creation fault",
            Error
         );
         return null;
   end Create_Transport;

   procedure Decrypt
             (  Transport : in out OpenSSL_Session;
                Client    : in out Connection'Class;
                Got_It    : out Boolean
             )  is
      Factory : Connections_Factory'Class renames
                Client.Socket_Listener.Factory.all;
      Result  : int;
      Count   : aliased size_t := 0;
      Buffer  : Input_Buffer renames Transport.Buffer;
      Last    : Stream_Element_Offset;
   begin
--        if not Has_Data (Client) then
--  ada.Text_IO.Put_Line("   no data read");
--           Got_It := False;
--           return;
--        end if;
      if Buffer.Free_To_Read < Buffer.First_Read then
         --
         -- [XXXXXXXXXXXXXX              XXXXX]
         --   Free_To_Read |  First_Read |
         --
         Last := Buffer.First_Read - 2;
         if Last <= Buffer.First_Read then -- Read buffer is full
            Got_It := True;
            return;
         end if;
      else
         --
         -- [           XXXXXXXXX             ]
         --  First_Read |        | Free_To_Read
         --
         if (  Buffer.Free_To_Read - Buffer.First_Read
            >= Buffer.Read'Length
            )
         then -- Read buffer is full
            Got_It := True;
            return;
         elsif Buffer.Free_To_Read > Buffer.Read'Last then -- Wrap
            Buffer.Free_To_Read := Buffer.Read'First;
            Last := Buffer.First_Read - 2;
         else
            Last := Buffer.Read'Last;
         end if;
      end if;
      Result :=
         SSL_read_ex
         (  Transport.SSL_Session,
            Buffer.Read (Buffer.Free_To_Read)'Access,
            int (Last - Buffer.Free_To_Read + 1),
            Count'Access
         );
      if Result <= 0 then
         case SSL_get_error (Transport.SSL_Session, Result) is
            when SSL_ERROR_NONE                 |
                 SSL_ERROR_WANT_ACCEPT          |
                 SSL_ERROR_WANT_CLIENT_HELLO_CB |
                 SSL_ERROR_WANT_CONNECT         |
                 SSL_ERROR_WANT_READ            |
                 SSL_ERROR_WANT_WRITE           |
                 SSL_ERROR_WANT_X509_LOOKUP     =>
               Last := (  Buffer.Free_To_Read
                       +  Stream_Element_Offset (Count)
                       -  1
                       );
            when SSL_ERROR_ZERO_RETURN =>
               Last := Buffer.Free_To_Read - 1;
            when others =>
               Last := Buffer.Free_To_Read - 1;
               Check_Error (Data_Error'Identity);
         end case;
      else
         Last :=
            Buffer.Free_To_Read + Stream_Element_Offset (Count) - 1;
      end if;
      if Last + 1 /= Buffer.Free_To_Read then -- Some data read
         Got_It := True;
         if Is_Trace_Received_On (Factory, Trace_Decoded) then
            Trace_Received
            (  Factory => Factory,
               Client  => Client,
               Data    => Buffer.Read,
               From    => Buffer.Free_To_Read,
               To      => Last,
               Encoded => False
            );
         end if;
         Buffer.Expected :=
            Stream_Element_Offset'Max
            (  Buffer.Expected - (Last - Buffer.Free_To_Read + 1),
               0
            );
         Buffer.Free_To_Read := Last + 1;
      else
         Got_It := False;
      end if;
   exception
      when End_Error =>
         raise Connection_Error;
   end Decrypt;

   procedure Encode
             (  Transport : in out OpenSSL_Session;
                Client    : in out Connection'Class;
                Data      : Stream_Element_Array;
                Last      : out Stream_Element_Offset
             )  is
      Result : int;
      Count  : aliased size_t := 0;
   begin
      Result :=
         SSL_write_ex
         (  Transport.SSL_Session,
            Data (Data'First)'Address,
            Data'Length,
            Count'Access
         );
      if Result <= 0 then
         case SSL_get_error (Transport.SSL_Session, Result) is
            when SSL_ERROR_NONE                 |
                 SSL_ERROR_WANT_ACCEPT          |
                 SSL_ERROR_WANT_CLIENT_HELLO_CB |
                 SSL_ERROR_WANT_CONNECT         |
                 SSL_ERROR_WANT_READ            |
                 SSL_ERROR_WANT_WRITE           |
                 SSL_ERROR_WANT_X509_LOOKUP     =>
               Last := Data'First + Stream_Element_Offset (Count) - 1;
            when SSL_ERROR_ZERO_RETURN =>
               Last := Data'First - 1;
            when others =>
               Check_Error (Data_Error'Identity);
               Last := Data'First - 1;
         end case;
      else
         Last := Data'First + Stream_Element_Offset (Count) - 1;
      end if;
   end Encode;

   procedure Finalize (Factory : in out Abstract_OpenSSL_Factory) is
   begin
      Finalize (Connections_Factory (Factory));
      if Factory.Client_Context /= No_SSL_CTX then
         SSL_CTX_free (Factory.Client_Context);
         Factory.Client_Context := No_SSL_CTX;
      end if;
      if Factory.Server_Context /= No_SSL_CTX then
         SSL_CTX_free (Factory.Server_Context);
         Factory.Server_Context := No_SSL_CTX;
      end if;
   end Finalize;

   procedure Finalize (Session : in out OpenSSL_Session) is
   begin
      if Session.SSL_Session /= No_SSL then
         SSL_free (Session.SSL_Session);
         Session.SSL_Session := No_SSL;
      end if;
      Finalize (Encoder (Session));
   end Finalize;

   function Get_Cipher_List (Session : SSL; Priority : int)
      return String is
      Result : constant chars_ptr :=
                        SSL_get_cipher_list (Session, Priority);
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Cipher_List;

   function Get_Name_Of_Cipher
            (  Session : SSL;
               Index   : Positive
            )  return String is
      Stack  : OPENSSL_STACK;
      Cipher : SSL_CIPHER;
      No     : int := int (Index);
   begin
      Stack := SSL_get_ciphers (Session);
      if Stack /= No_STACK then
         if No <= OPENSSL_sk_num (Stack) then
            Cipher := OPENSSL_sk_value (Stack, No - 1);
            if Cipher = No_CIPHER then
               return "";
            else
               return +SSL_CIPHER_get_name (Cipher);
            end if;
         end if;
      end if;
      Raise_Exception
      (  Constraint_Error'Identity,
         "Wrong cipher number"
      );
   end Get_Name_Of_Cipher;

   function Get_Name_Of_Cipher
            (  Factory : Abstract_OpenSSL_Factory;
               Context : Context_Type;
               Index   : Positive
            )  return String is
      Result : Natural := 0;
      Stack  : OPENSSL_STACK;
      Cipher : SSL_CIPHER;
      No     : int := int (Index);
   begin
      if Context in Client_Context..Any_Context then
         if Factory.Client_Context /= No_SSL_CTX then
            Stack := SSL_CTX_get_ciphers (Factory.Client_Context);
            if Stack /= No_STACK then
               if No <= OPENSSL_sk_num (Stack) then
                  Cipher := OPENSSL_sk_value (Stack, No - 1);
                  if Cipher = No_CIPHER then
                     return "";
                  else
                     return +SSL_CIPHER_get_name (Cipher);
                  end if;
               else
                  No := No - OPENSSL_sk_num (Stack);
               end if;
            end if;
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if Factory.Server_Context /= No_SSL_CTX then
            Stack := SSL_CTX_get_ciphers (Factory.Server_Context);
            if Stack /= No_STACK then
               if No <= OPENSSL_sk_num (Stack) then
                  Cipher := OPENSSL_sk_value (Stack, No - 1);
                  if Cipher = No_CIPHER then
                     return "";
                  else
                     return +SSL_CIPHER_get_name (Cipher);
                  end if;
               else
                  No := No - OPENSSL_sk_num (Stack);
               end if;
            end if;
         end if;
      end if;
      Raise_Exception
      (  Constraint_Error'Identity,
         "Wrong cipher number"
      );
   end Get_Name_Of_Cipher;

   function Get_Name_Of_Client_Cipher
            (  Session : SSL;
               Index   : Positive
            )  return String is
      Stack  : OPENSSL_STACK;
      Cipher : SSL_CIPHER;
      No     : int := int (Index);
   begin
      Stack := SSL_get_client_ciphers (Session);
      if Stack /= No_STACK then
         if No <= OPENSSL_sk_num (Stack) then
            Cipher := OPENSSL_sk_value (Stack, No - 1);
            if Cipher = No_CIPHER then
               return "";
            else
               return +SSL_CIPHER_get_name (Cipher);
            end if;
         end if;
      end if;
      Raise_Exception
      (  Constraint_Error'Identity,
         "Wrong cipher number"
      );
   end Get_Name_Of_Client_Cipher;

   function Get_Name_Of_Supported_Cipher
            (  Session : SSL;
               Index   : Positive
            )  return String is
      Stack  : OPENSSL_STACK;
      Cipher : SSL_CIPHER;
      No     : int := int (Index);
   begin
      Stack := SSL_get1_supported_ciphers (Session);
      if Stack /= No_STACK then
         if No <= OPENSSL_sk_num (Stack) then
            Cipher := OPENSSL_sk_value (Stack, No - 1);
            if Cipher = No_CIPHER then
               return "";
            else
               return +SSL_CIPHER_get_name (Cipher);
            end if;
         end if;
      end if;
      Raise_Exception
      (  Constraint_Error'Identity,
         "Wrong cipher number"
      );
   end Get_Name_Of_Supported_Cipher;

   function Get_Number_Of_Ciphers (Session : SSL) return Natural is
      Stack : OPENSSL_STACK;
   begin
      Stack := SSL_get_ciphers (Session);
      if Stack = No_STACK then
         return 0;
      else
         return Natural (OPENSSL_sk_num (Stack));
      end if;
   end Get_Number_Of_Ciphers;

   function Get_Number_Of_Ciphers
            (  Factory : Abstract_OpenSSL_Factory;
               Context : Context_Type
            )  return Natural is
      Result : int := 0;
      Stack  : OPENSSL_STACK;
   begin
      if Context in Client_Context..Any_Context then
         if Factory.Client_Context /= No_SSL_CTX then
            Stack := SSL_CTX_get_ciphers (Factory.Client_Context);
            if Stack /= No_STACK then
               Result := Result + OPENSSL_sk_num (Stack);
            end if;
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if Factory.Server_Context /= No_SSL_CTX then
            Stack := SSL_CTX_get_ciphers (Factory.Server_Context);
            if Stack /= No_Stack then
               Result := Result + OPENSSL_sk_num (Stack);
            end if;
         end if;
      end if;
      return Natural (Result);
   end Get_Number_Of_Ciphers;

   function Get_Number_Of_Client_Ciphers (Session : SSL)
      return Natural is
      Stack : OPENSSL_STACK;
   begin
      Stack := SSL_get_client_ciphers (Session);
      if Stack = No_STACK then
         return 0;
      else
         return Natural (OPENSSL_sk_num (Stack));
      end if;
   end Get_Number_Of_Client_Ciphers;

   function Get_Number_Of_Supported_Ciphers (Session : SSL)
      return Natural is
      Stack : OPENSSL_STACK;
   begin
      Stack := SSL_get1_supported_ciphers (Session);
      if Stack = No_STACK then
         return 0;
      else
         return Natural (OPENSSL_sk_num (Stack));
      end if;
   end Get_Number_Of_Supported_Ciphers;

   function Get_Options (Session : SSL) return SSL_OP is
   begin
     return SSL_get_options (Session);
   end Get_Options;

   function Get_Options
            (  Factory : Abstract_OpenSSL_Factory;
               Context : Context_Type
            )  return SSL_OP is
      Result : SSL_OP := 0;
   begin
      if Context in Client_Context..Any_Context then
         if Factory.Client_Context /= No_SSL_CTX then
            Result := Result
                   or SSL_CTX_get_options (Factory.Client_Context);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if Factory.Server_Context /= No_SSL_CTX then
            Result := Result
                   or SSL_CTX_get_options (Factory.Server_Context);
         end if;
      end if;
      return Result;
   end Get_Options;

   procedure Get_Proto_Version
             (  Session : SSL;
                Minimal : out SSL_Version_No;
                Maximal : out SSL_Version_No
             )  is
   begin
      Minimal :=
         SSL_Version_No
         (  SSL_ctrl (Session, SSL_CTRL_GET_MIN_PROTO_VERSION, 0)
         );
      Maximal :=
         SSL_Version_No
         (  SSL_ctrl (Session, SSL_CTRL_GET_MAX_PROTO_VERSION, 0)
         );
   end Get_Proto_Version;

   procedure Get_Proto_Version
             (  Factory : Abstract_OpenSSL_Factory;
                Context : Context_Type;
                Minimal : out SSL_Version_No;
                Maximal : out SSL_Version_No
             )  is
   begin
      Minimal := 0;
      Maximal := SSL_Version_No'Last;
      if (  Context in Client_Context..Any_Context
        and then
            Factory.Client_Context /= No_SSL_CTX
         )  then
         Minimal :=
            SSL_Version_No'Max
            (  Minimal,
               SSL_Version_No
               (  SSL_CTX_ctrl
                  (  Factory.Client_Context,
                     SSL_CTRL_GET_MIN_PROTO_VERSION,
                     0
            )  )  );
         Maximal :=
            SSL_Version_No'Min
            (  Maximal,
               SSL_Version_No
               (  SSL_CTX_ctrl
                  (  Factory.Client_Context,
                     SSL_CTRL_GET_MAX_PROTO_VERSION,
                     0
            )  )  );
      end if;
      if (  Context in Any_Context..Server_Context
        and then
            Factory.Server_Context /= No_SSL_CTX
         )  then
         Minimal :=
            SSL_Version_No'Max
            (  Minimal,
               SSL_Version_No
               (  SSL_CTX_ctrl
                  (  Factory.Server_Context,
                     SSL_CTRL_GET_MIN_PROTO_VERSION,
                     0
            )  )  );
         Maximal :=
            SSL_Version_No'Min
            (  Maximal,
               SSL_Version_No
               (  SSL_CTX_ctrl
                  (  Factory.Server_Context,
                     SSL_CTRL_GET_MAX_PROTO_VERSION,
                     0
            )  )  );
      end if;
   end Get_Proto_Version;

   function Get_Shared_Ciphers (Session : SSL) return String is
      List   : chars_ptr;
      Buffer : char_array (1..1024);
   begin
      List := SSL_get_shared_ciphers (Session, Buffer, Buffer'Length);
      if List = Null_Ptr then
         return "";
      else
         return Value (List);
      end if;
   end Get_Shared_Ciphers;

   function Get_Session
            (  Client : Connection'Class
            )  return SSL is
   begin
      return OpenSSL_Session (Client.Transport.all).SSL_Session;
   end Get_Session;

   procedure Handshake_Completed
             (  Factory : in out Abstract_OpenSSL_Factory;
                Client  : in out Connection'Class;
                Session : SSL
             )  is
   begin
      null;
   end Handshake_Completed;

   function Is_TLS_Capable
            (  Factory : Abstract_OpenSSL_Factory
            )  return Boolean is
   begin
      return True;
   end Is_TLS_Capable;

   function Is_Trace_Decoded (Factory : Abstract_OpenSSL_Factory)
      return Boolean is
   begin
      return Factory.Trace_Decoded;
   end Is_Trace_Decoded;

   function Is_Trace_Session (Factory : Abstract_OpenSSL_Factory)
      return Boolean is
   begin
      return Factory.Trace_Session;
   end Is_Trace_Session;

   procedure Load_Verify_Locations
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                CA_File : String := "";
                CA_Path : String := ""
             )  is
      File   : aliased char_array := To_C (CA_File);
      Path   : aliased char_array := To_C (CA_Path);
      Result : int;
   begin
      if CA_File = "" and then CA_Path = "" then
         return;
      end if;
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if CA_Path = "" then
            Result := SSL_CTX_load_verify_locations
                      (  Factory.Client_Context,
                         To_Chars_Ptr (File'Unchecked_Access),
                         Null_Ptr
                      );
         elsif CA_File = "" then
            Result := SSL_CTX_load_verify_locations
                      (  Factory.Client_Context,
                         Null_Ptr,
                         To_Chars_Ptr (Path'Unchecked_Access)
                      );
         else
            Result := SSL_CTX_load_verify_locations
                      (  Factory.Client_Context,
                         To_Chars_Ptr (File'Unchecked_Access),
                         To_Chars_Ptr (Path'Unchecked_Access)
                      );
         end if;
      end if;
      if Result <= 0 then
         Check_Error (Use_Error'Identity);
      end if;
      if Context in Any_Context..Server_Context then
         if CA_Path = "" then
            Result := SSL_CTX_load_verify_locations
                      (  Factory.Server_Context,
                         To_Chars_Ptr (File'Unchecked_Access),
                         Null_Ptr
                      );
         elsif CA_File = "" then
            Result := SSL_CTX_load_verify_locations
                      (  Factory.Server_Context,
                         Null_Ptr,
                         To_Chars_Ptr (Path'Unchecked_Access)
                      );
         else
            Result := SSL_CTX_load_verify_locations
                      (  Factory.Server_Context,
                         To_Chars_Ptr (File'Unchecked_Access),
                         To_Chars_Ptr (Path'Unchecked_Access)
                      );
         end if;
      end if;
      if Result <= 0 then
         Check_Error (Use_Error'Identity);
      end if;
   end Load_Verify_Locations;

   procedure Process
             (  Transport : in out OpenSSL_Session;
                Listener  : in out Connections_Server'Class;
                Client    : in out Connection'Class;
                Data_Left : out Boolean
             )  is
     Result : int;
   begin
      case Transport.State is
         when TLS_Handshake =>
            if Client.Client then -- A client
               Result := SSL_connect (Transport.SSL_Session);
            else
               Result := SSL_accept (Transport.SSL_Session);
            end if;
            if Result <= 0 then
               case SSL_get_error (Transport.SSL_Session, Result) is
                  when SSL_ERROR_NONE =>
                     null;
                  when SSL_ERROR_ZERO_RETURN =>
                     raise Connection_Error;
                  when SSL_ERROR_WANT_ACCEPT          |
                       SSL_ERROR_WANT_CLIENT_HELLO_CB |
                       SSL_ERROR_WANT_CONNECT         |
                       SSL_ERROR_WANT_READ            |
                       SSL_ERROR_WANT_WRITE           |
                       SSL_ERROR_WANT_X509_LOOKUP     =>
                     Data_Left := Has_Data (Client);
                     return; -- Pump I/O
                  when others =>
                     Check_Error (Data_Error'Identity);
                     Data_Left := Has_Data (Client);
                     return;
               end case;
            end if;
            declare
               Factory : Abstract_OpenSSL_Factory'Class renames
                         Abstract_OpenSSL_Factory'Class
                         (  Listener.Factory.all
                         );
            begin
               begin
                  Handshake_Completed
                  (  Factory,
                     Client,
                     Transport.SSL_Session
                  );
               exception
                  when Connection_Error =>
                     if Factory.Trace_Session then
                        Trace
                        (  Factory,
                           "OpenSSL successful handshake rejected"
                        );
                     end if;
                     raise;
                  when Error : others =>
                     if Factory.Trace_Session then
                        Trace
                        (  Factory,
                           "OpenSSL successful handshake rejected"
                        );
                     end if;
                     Trace_Error
                     (  Factory,
                        "OpenSSL handshake completion",
                        Error
                     );
                     raise Connection_Error;
               end;
               Transport.State := TLS_Exchange;
               if Factory.Trace_Session then
                  if 0 = SSL_session_reused (Transport.SSL_Session) then
                     Trace
                     (  Factory,
                        "OpenSSL handshake successful, new session"
                     );
                  else
                     Trace
                     (  Factory,
                        "OpenSSL handshake successful, resumed session"
                     );
                  end if;
               end if;
               declare
                  Saved : constant Session_State := Client.Session;
               begin
                  Client.Session := Session_Connected;
                  if Is_Opportunistic (Client) then
                     Elevated (Client);
                  else
                     Connected (Client);
                  end if;
                  Connected (Listener, Client);
                  Client.Session := Session_Active;
               exception
                  when others =>
                     if Client.Session = Session_Connected then
                        Client.Session := Saved;
                     end if;
                    raise;
               end;
            end;
            Data_Left := Has_Data (Client);
         when TLS_Exchange =>
            null;
      end case;
      loop
         Decrypt (Transport, Client, Data_Left);
         exit when not Data_Left;
         Process (Transport.Buffer, Client, Data_Left);
         exit when Data_Left; -- Won't handle it right now
      end loop;
   end Process;

   procedure Prepare
             (  Factory : in out Abstract_OpenSSL_Factory;
                Client  : in out Connection'Class;
                Session : SSL
             )  is
   begin
      null;
   end Prepare;

   procedure Read
             (  Client  : in out Connection'Class;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      Start : Stream_Element_Offset := Pointer;
   begin
      Pull (Client.Read, Data, Pointer);
   exception
      when Error : others =>
         Trace_Error
         (  Client.Socket_Listener.Factory.all,
            "OpenSSL transport reading",
            Error
         );
         raise;
   end Read;

   procedure Set_Cipher_List (Session : SSL; List : String) is
   begin
      if SSL_set_cipher_list (Session, To_C (List)) <= 0 then
         Check_Error (Use_Error'Identity);
      end if;
   end Set_Cipher_List;

   procedure Set_Cipher_List
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                List    : String
             )  is
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if SSL_CTX_set_cipher_list
            (  Factory.Client_Context,
               To_C (List)
            )  <= 0
         then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if SSL_CTX_set_cipher_list
            (  Factory.Server_Context,
               To_C (List)
            )  <= 0
         then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Set_Cipher_List;

   procedure Set_Cipher_Suites (Session : SSL; List : String) is
   begin
      if SSL_set_ciphersuites (Session, To_C (List)) <= 0 then
         Check_Error (Use_Error'Identity);
      end if;
   end Set_Cipher_Suites;

   procedure Set_Cipher_Suites
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                List    : String
             )  is
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if SSL_CTX_set_ciphersuites
            (  Factory.Client_Context,
               To_C (List)
            )  <= 0
         then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if SSL_CTX_set_ciphersuites
            (  Factory.Server_Context,
               To_C (List)
            )  <= 0
         then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Set_Cipher_Suites;

   procedure Set_Default_Verify_Dir
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type
             )  is
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_set_default_verify_dir (Factory.Client_Context)
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_set_default_verify_dir (Factory.Server_Context)
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Set_Default_Verify_Dir;

   procedure Set_Default_Verify_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type
             )  is
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_set_default_verify_file (Factory.Client_Context)
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_set_default_verify_file (Factory.Server_Context)
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Set_Default_Verify_File;

   procedure Set_Default_Verify_Paths
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type
             )  is
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_set_default_verify_paths (Factory.Client_Context)
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_set_default_verify_paths (Factory.Server_Context)
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Set_Default_Verify_Paths;

   procedure Set_Options (Session : SSL; Options : SSL_OP) is
      Result : SSL_OP;
   begin
      Result := SSL_set_options (Session, Options);
   end Set_Options;

   procedure Set_Options
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                Options : SSL_OP
             )  is
      Result : SSL_OP;
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         Result :=
            SSL_CTX_set_options (Factory.Client_Context, Options);
      end if;
      if Context in Any_Context..Server_Context then
         Result :=
            SSL_CTX_set_options (Factory.Server_Context, Options);
      end if;
   end Set_Options;

   procedure Set_Proto_Version
             (  Session : SSL;
                Minimal : SSL_Version_No;
                Maximal : SSL_Version_No
             )  is
   begin
      if (  SSL_ctrl
            (  Session,
               SSL_CTRL_SET_MIN_PROTO_VERSION,
               long (Minimal)
            )
         <= 0
         )  then
         Check_Error (Use_Error'Identity);
      end if;
      if (  SSL_ctrl
            (  Session,
               SSL_CTRL_SET_MAX_PROTO_VERSION,
               long (Maximal)
            )
         <= 0
         )  then
         Check_Error (Use_Error'Identity);
      end if;
   end Set_Proto_Version;

   procedure Set_Proto_Version
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                Minimal : SSL_Version_No;
                Maximal : SSL_Version_No
             )  is
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_ctrl
               (  Factory.Client_Context,
                  SSL_CTRL_SET_MIN_PROTO_VERSION,
                  long (Minimal)
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
         if (  SSL_CTX_ctrl
               (  Factory.Client_Context,
                  SSL_CTRL_SET_MAX_PROTO_VERSION,
                  long (Maximal)
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_ctrl
               (  Factory.Server_Context,
                  SSL_CTRL_SET_MIN_PROTO_VERSION,
                  long (Minimal)
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
         if (  SSL_CTX_ctrl
               (  Factory.Server_Context,
                  SSL_CTRL_SET_MAX_PROTO_VERSION,
                  long (Maximal)
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Set_Proto_Version;

   procedure Set_TLS_Tracing
             (  Factory : in out Abstract_OpenSSL_Factory;
                Session : Boolean;
                Decoded : Boolean
             )  is
   begin
      Factory.Trace_Session := Session;
      Factory.Trace_Decoded := Decoded;
   end Set_TLS_Tracing;

   procedure Use_Certificate_Chain_File
             (  Session : SSL;
                File    : String
             )  is
   begin
      if SSL_use_certificate_chain_file (Session, To_C (File)) <= 0
      then
         Check_Error (Use_Error'Identity);
      end if;
   end Use_Certificate_Chain_File;

   procedure Use_Certificate_Chain_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             )  is
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_use_certificate_chain_file
               (  Factory.Client_Context,
                  To_C (File)
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_use_certificate_chain_file
               (  Factory.Server_Context,
                  To_C (File)
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Use_Certificate_Chain_File;

   procedure Use_Certificate_ASN1
             (  Session     : SSL;
                Certificate : Stream_Element_Array
             )  is
   begin
      if (  SSL_use_certificate_ASN1
            (  Session,
               Certificate'Length,
               Certificate'Address
            )
         <= 0
         )  then
         Check_Error (Use_Error'Identity);
      end if;
   end Use_Certificate_ASN1;

   procedure Use_Certificate_ASN1
             (  Factory     : in out Abstract_OpenSSL_Factory;
                Context     : Context_Type;
                Certificate : Stream_Element_Array
             )  is
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_use_certificate_ASN1
               (  Factory.Client_Context,
                  Certificate'Length,
                  Certificate'Address
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_use_certificate_ASN1
               (  Factory.Server_Context,
                  Certificate'Length,
                  Certificate'Address
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Use_Certificate_ASN1;

   procedure Use_Certificate_ASN1_File
             (  Session : SSL;
                File    : String
             )  is
   begin
      if (  SSL_use_certificate_file
            (  Session,
               To_C (File),
               SSL_FILETYPE_ASN1
            )
         <= 0
         )  then
         Check_Error (Use_Error'Identity);
      end if;
   end Use_Certificate_ASN1_File;

   procedure Use_Certificate_ASN1_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             )  is
      Name : aliased char_array := To_C (File);
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_use_certificate_file
               (  Factory.Client_Context,
                  Name,
                  SSL_FILETYPE_ASN1
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_use_certificate_file
               (  Factory.Server_Context,
                  Name,
                  SSL_FILETYPE_ASN1
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Use_Certificate_ASN1_File;

   procedure Use_Certificate_PEM_File
             (  Session : SSL;
                File    : String
             )  is
   begin
      if (  SSL_use_certificate_file
            (  Session,
               To_C (File),
               SSL_FILETYPE_PEM
            )
         <= 0
         )  then
         Check_Error (Use_Error'Identity);
      end if;
   end Use_Certificate_PEM_File;

   procedure Use_Certificate_PEM_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             )  is
      Name : aliased char_array := To_C (File);
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_use_certificate_file
               (  Factory.Client_Context,
                  Name,
                  SSL_FILETYPE_PEM
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_use_certificate_file
               (  Factory.Server_Context,
                  Name,
                  SSL_FILETYPE_PEM
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Use_Certificate_PEM_File;

   procedure Use_Key_ASN1
             (  Session : SSL;
                Key     : Stream_Element_Array
             )  is
   begin
      if (  SSL_use_PrivateKey_ASN1
            (  Session,
               Key'Length,
               Key'Address
            )
         <= 0
         )  then
         Check_Error (Use_Error'Identity);
      end if;
   end Use_Key_ASN1;

   procedure Use_Key_ASN1
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                Key     : Stream_Element_Array
             )  is
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_use_PrivateKey_ASN1
               (  Factory.Client_Context,
                  Key'Length,
                  Key'Address
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_use_PrivateKey_ASN1
               (  Factory.Server_Context,
                  Key'Length,
                  Key (Key'First)'Address
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Use_Key_ASN1;

   procedure Use_Key_ASN1_File
             (  Session : SSL;
                File    : String
             )  is
   begin
      if (  SSL_use_PrivateKey_file
            (  Session,
               To_C (File),
               SSL_FILETYPE_ASN1
            )
         <= 0
         )  then
         Check_Error (Use_Error'Identity);
      end if;
   end Use_Key_ASN1_File;

   procedure Use_Key_ASN1_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             )  is
      Name : aliased char_array := To_C (File);
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_use_PrivateKey_file
               (  Factory.Client_Context,
                  Name,
                  SSL_FILETYPE_ASN1
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_use_PrivateKey_file
               (  Factory.Server_Context,
                  Name,
                  SSL_FILETYPE_ASN1
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Use_Key_ASN1_File;

   procedure Use_Key_PEM_File
             (  Session : SSL;
                File    : String
             )  is
   begin
      if (  SSL_use_PrivateKey_file
            (  Session,
               To_C (File),
               SSL_FILETYPE_PEM
            )
         <= 0
         )  then
         Check_Error (Use_Error'Identity);
      end if;
   end Use_Key_PEM_File;

   procedure Use_Key_PEM_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             )  is
      Name : aliased char_array := To_C (File);
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_use_PrivateKey_file
               (  Factory.Client_Context,
                  Name,
                  SSL_FILETYPE_PEM
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_use_PrivateKey_file
               (  Factory.Server_Context,
                  Name,
                  SSL_FILETYPE_PEM
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Use_Key_PEM_File;

   procedure Use_RSA_Key_ASN1
             (  Session : SSL;
                Key     : Stream_Element_Array
             )  is
   begin
      if (  SSL_use_RSAPrivateKey_ASN1
            (  Session,
               Key'Length,
               Key'Address
            )
         <= 0
         )  then
         Check_Error (Use_Error'Identity);
      end if;
   end Use_RSA_Key_ASN1;

   procedure Use_RSA_Key_ASN1
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                Key     : Stream_Element_Array
             )  is
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_use_RSAPrivateKey_ASN1
               (  Factory.Client_Context,
                  Key'Length,
                  Key'Address
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_use_RSAPrivateKey_ASN1
               (  Factory.Server_Context,
                  Key'Length,
                  Key (Key'First)'Address
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Use_RSA_Key_ASN1;

   procedure Use_RSA_Key_ASN1_File
             (  Session : SSL;
                File    : String
             )  is
   begin
      if (  SSL_use_RSAPrivateKey_file
            (  Session,
               To_C (File),
               SSL_FILETYPE_ASN1
            )
         <= 0
         )  then
         Check_Error (Use_Error'Identity);
      end if;
   end Use_RSA_Key_ASN1_File;

   procedure Use_RSA_Key_ASN1_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             )  is
      Name : aliased char_array := To_C (File);
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_use_RSAPrivateKey_file
               (  Factory.Client_Context,
                  Name,
                  SSL_FILETYPE_ASN1
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_use_RSAPrivateKey_file
               (  Factory.Server_Context,
                  Name,
                  SSL_FILETYPE_ASN1
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Use_RSA_Key_ASN1_File;

   procedure Use_RSA_Key_PEM_File
             (  Session : SSL;
                File    : String
             )  is
   begin
      if (  SSL_use_RSAPrivateKey_file
            (  Session,
               To_C (File),
               SSL_FILETYPE_PEM
            )
         <= 0
         )  then
         Check_Error (Use_Error'Identity);
      end if;
   end Use_RSA_Key_PEM_File;

   procedure Use_RSA_Key_PEM_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             )  is
      Name : aliased char_array := To_C (File);
   begin
      Create_Context (Factory, Context);
      if Context in Client_Context..Any_Context then
         if (  SSL_CTX_use_RSAPrivateKey_file
               (  Factory.Client_Context,
                  Name,
                  SSL_FILETYPE_PEM
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
      if Context in Any_Context..Server_Context then
         if (  SSL_CTX_use_RSAPrivateKey_file
               (  Factory.Server_Context,
                  Name,
                  SSL_FILETYPE_PEM
               )
            <= 0
            )  then
            Check_Error (Use_Error'Identity);
         end if;
      end if;
   end Use_RSA_Key_PEM_File;

   procedure Write
             (  Client  : in out Connection'Class;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      Last : Stream_Element_Offset;
   begin
      Send_Socket
      (  Get_Socket (Client),
         Data (Pointer..Data'Last),
         Last
      );
      if (  Last >= Pointer
         and then
            Is_Trace_Received_On
            (  Client.Socket_Listener.Factory.all,
               Trace_Encoded
         )   )
      then
         Trace_Sent
         (  Factory => Client.Socket_Listener.Factory.all,
            Client  => Client,
            Data    => Data,
            From    => Pointer,
            To      => Last,
            Encoded => True
         );
      end if;
      if Last >= Pointer then
         Client.Data_Sent := True;
         Pointer := Last + 1;
      end if;
   exception
      when Error : Socket_Error =>
         Send_Error (Client, Error);
         raise;
      when Error : others =>
         Trace_Error
         (  Client.Socket_Listener.Factory.all,
            "OpenSSL transport writing",
            Error
         );
         raise;
   end Write;

end GNAT.Sockets.Server.OpenSSL;
