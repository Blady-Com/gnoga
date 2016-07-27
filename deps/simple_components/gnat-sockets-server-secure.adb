--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Server.Secure                  Luebeck            --
--  Implementation                                 Winter, 2015       --
--                                                                    --
--                                Last revision :  12:47 19 Jun 2016  --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

with System.Address_To_Access_Conversions;

package body GNAT.Sockets.Server.Secure is

   package Socket_Pull is
      new Transport_Set_Pull_Function
          (  Transport_Type => Connection'Class,
             Get_Session    => Get_Session,
             Read           => Read
          );
   package Socket_Push is
      new Transport_Set_Push_Function
          (  Transport_Type => Connection'Class,
             Get_Session    => Get_Session,
             Write           => Write
          );
   package Conversions is
      new System.Address_To_Access_Conversions (TLS_Session'Class);

   function Create_Transport
            (  Factory  : access Abstract_GNUTLS_Factory;
               Listener : access Connections_Server'Class;
               Client   : access Connection'Class
            )  return Encoder_Ptr is
      Result : Encoder_Ptr;
   begin
      if Client.Client then
         Result := new TLS_Session
                       (  Client.all'Unchecked_Access,
                          Factory.Decoded_Size,
                          Init_Client or Init_Nonblock
                       );
      else
         Result := new TLS_Session
                       (  Client.all'Unchecked_Access,
                          Factory.Decoded_Size,
                          Init_Server or Init_Nonblock
                       );
      end if;
      declare
         TLS : TLS_Session renames TLS_Session (Result.all);
      begin
         Session_Set_Ptr (TLS.Session, Client'Address);
         Socket_Pull.Set (TLS.Session, Client);
         Socket_Push.Set (TLS.Session, Client);
         if Factory.Trace_Session then
            Trace (Factory.all, "TLS setting up session");
         end if;
         Prepare
         (  Abstract_GNUTLS_Factory'Class (Factory.all),
            Client.all,
            TLS.Session
         );
         if Factory.Trace_Session then
            Trace (Factory.all, "TLS handshake engaged");
         end if;
      end;
      return Result;
   end Create_Transport;

   procedure Decrypt
             (  Transport : in out TLS_Session;
                Client    : in out Connection'Class;
                Got_It    : out Boolean
             )  is
      Factory : Connections_Factory'Class renames
                Client.Listener.Factory.all;
      Buffer  : Input_Buffer renames Transport.Buffer;
      Last    : Stream_Element_Offset;
   begin
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
      Record_Recv
      (  Transport.Session,
         Buffer.Read (Buffer.Free_To_Read..Last),
         Last
      );
      if Last + 1 /= Buffer.Free_To_Read then -- Some data read
         Got_It := True;
         if 0 /= (Factory.Trace_Flags and Trace_Decoded_Received) then
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

   function Get_Session
            (  Client : Connection'Class
            )  return Session_Type_Ptr is
   begin
      return TLS_Session
             (  Client.Transport.all
             ) .Session'Unchecked_Access;
   end Get_Session;

   procedure Handshake_Completed
             (  Factory : in out Abstract_GNUTLS_Factory;
                Client  : in out Connection'Class;
                Session : in out Session_Type
             )  is
   begin
      Client.Session := Session_Connected;
      if Is_Opportunistic (Client) then
         Elevated (Client);
      else
         Connected (Client);
      end if;
   end Handshake_Completed;

   function Is_TLS_Capable
            (  Factory : Abstract_GNUTLS_Factory
            )  return Boolean is
   begin
      return True;
   end Is_TLS_Capable;

   function Is_Trace_Decoded (Factory : Abstract_GNUTLS_Factory)
      return Boolean is
   begin
      return Factory.Trace_Decoded;
   end Is_Trace_Decoded;

   function Is_Trace_Session (Factory : Abstract_GNUTLS_Factory)
      return Boolean is
   begin
      return Factory.Trace_Session;
   end Is_Trace_Session;

   procedure Process
             (  Transport : in out TLS_Session;
                Listener  : in out Connections_Server'Class;
                Client    : in out Connection'Class;
                Data_Left : out Boolean
             )  is
   begin
      case Transport.State is
         when TLS_Handshake =>
            if Handshake (Transport.Session) then
               Data_Left := Has_Data (Client);
            else
               declare
                  Factory : Abstract_GNUTLS_Factory'Class renames
                            Abstract_GNUTLS_Factory'Class
                            (  Listener.Factory.all
                            );
               begin
                  begin
                     Handshake_Completed
                     (  Factory,
                        Client,
                        Transport.Session
                     );
                  exception
                     when Connection_Error =>
                        if Factory.Trace_Session then
                           Trace
                           (  Factory,
                              "TLS successful handshake rejected"
                           );
                        end if;
                        raise;
                     when Error : others =>
                        if Factory.Trace_Session then
                           Trace
                           (  Factory,
                              "TLS successful handshake rejected"
                           );
                        end if;
                        Trace_Error
                        (  Factory,
                           "Handshake completion",
                           Error
                        );
                        raise  Connection_Error;
                  end;
                  Transport.State := TLS_Exchange;
                  if Factory.Trace_Session then
                     if Session_Is_Resumed (Transport.Session) then
                        Trace
                        (  Factory,
                           (  "TLS handshake successful, "
                           &  "resumed session: "
                           &  Session_Get_Desc (Transport.Session)
                        )  );
                     else
                        Trace
                        (  Factory,
                           (  "TLS handshake successful, "
                           &  "new session: "
                           &  Session_Get_Desc (Transport.Session)
                        )  );
                     end if;
                  end if;
               end;
               Data_Left := Has_Data (Client);
            end if;
         when TLS_Exchange =>
            loop
               Decrypt (Transport, Client, Data_Left);
               exit when not Data_Left;
               Process (Transport.Buffer, Client, Data_Left);
               exit when Data_Left; -- Won't handle it right now
            end loop;
      end case;
   end Process;

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
         (  Client.Listener.Factory.all,
            "TLS transport reading",
            Error
         );
         raise;
   end Read;

   procedure Encode
             (  Transport : in out TLS_Session;
                Client    : in out Connection'Class;
                Data      : Stream_Element_Array;
                Last      : out Stream_Element_Offset
             )  is
   begin
      Record_Send (Transport.Session, Data, Last);
   end Encode;

   procedure Set_TLS_Tracing
             (  Factory : in out Abstract_GNUTLS_Factory;
                Session : Boolean;
                Decoded : Boolean
             )  is
   begin
      Factory.Trace_Session := Session;
      Factory.Trace_Decoded := Decoded;
   end Set_TLS_Tracing;

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
            (  Client.Listener.Factory.all,
               Trace_Encoded
         )   )
      then
         Trace_Sent
         (  Factory => Client.Listener.Factory.all,
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
         Receive_Error (Client, Error);
         raise;
      when Error : others =>
         Trace_Error
         (  Client.Listener.Factory.all,
            "TLS transport writing",
            Error
         );
         raise;
   end Write;

end GNAT.Sockets.Server.Secure;
