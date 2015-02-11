--                                                                    --
--  package GNAT.Sockets.Server     Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  21:07 11 Feb 2015  --
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

with Ada.Calendar;           use Ada.Calendar;
with Ada.Exceptions;         use Ada.Exceptions;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body GNAT.Sockets.Server is

   Receive_Masks : constant array (IO_Tracing_Mode) of Factory_Flags :=
                   (  Trace_None    =>  0,
                      Trace_Encoded => Trace_Encoded_Received,
                      Trace_Decoded => Trace_Decoded_Received,
                      Trace_Any     => Trace_Encoded_Received
                                    or Trace_Decoded_Received
                   );
   Sent_Masks   : constant array (IO_Tracing_Mode) of Factory_Flags :=
                   (  Trace_None    =>  0,
                      Trace_Encoded => Trace_Encoded_Sent,
                      Trace_Decoded => Trace_Decoded_Sent,
                      Trace_Any     => Trace_Encoded_Sent
                                    or Trace_Decoded_Sent
                   );

   procedure Append
             (  List  : in out Connection_Ptr;
                Item  : Connection_Ptr;
                Count : in out Integer
             )  is
   begin
      if Item.Successor = null then
         if List = null then
            List := Item;
            Item.Successor   := Item;
            Item.Predecessor := Item;
         else
            Item.Successor   := List;
            Item.Predecessor := List.Predecessor;
            List.Predecessor := Item;
            Item.Predecessor.Successor := Item;
         end if;
         Count := Count + 1;
      end if;
   end Append;

   function Available_To_Process (Client : Connection)
      return Stream_Element_Count is
   begin
      return Used (Client.Read);
   end Available_To_Process;

   function Available_To_Send (Client : Connection)
      return Stream_Element_Count is
   begin
      return Free (Client.Written);
   end Available_To_Send;

   procedure Close (Socket : in out Socket_Type) is
   begin
      if Socket /= No_Socket then
         begin
            Shutdown_Socket (Socket);
         exception
            when others =>
               null;
         end;
         begin
            Close_Socket (Socket);
         exception
            when others =>
               null;
         end;
         Socket := No_Socket;
      end if;
   end Close;

   procedure Connected (Client : in out Connection) is
   begin
      null;
   end Connected;

   function Create_Transport
            (  Factory  : access Connections_Factory;
               Listener : access Connections_Server'Class;
               Client   : access Connection'Class
            )  return Encoder_Ptr is
   begin
      return null;
   end Create_Transport;

   procedure Data_Sent
             (  Listener : in out Connections_Server;
                Client   : Connection_Ptr
             )  is
   begin
      Client.Data_Sent := False;
      Sent (Client.all);
   end Data_Sent;

   procedure Disconnected
             (  Listener : in out Connections_Server;
                Client   : in out Connection'Class
             )  is
      Count : Natural := 1;
   begin
      Remove (Listener.Postponed, Client, Count);
   end Disconnected;

   procedure Fill_From_Stream
             (  Buffer  : in out Output_Buffer;
                Stream  : in out Root_Stream_Type'Class;
                Count   : Stream_Element_Count;
                Reserve : Stream_Element_Count;
                Last    : out Stream_Element_Offset;
                Next    : out Stream_Element_Offset;
                Done    : out Boolean
             )  is
   begin
      if Reserve >= Buffer.Written'Length then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Output buffer is too small for prefix and suffix ("
            &  Image (Reserve)
            &  ")"
         )  );
      end if;
      if Buffer.First_Written <= Buffer.Free_To_Write then
         --
         -- [     XXXXXXXXXXXXXXX        ]
         --       |              |
         --       First_Written  Free_To_Write
         --
         declare
            Tail : constant Stream_Element_Offset :=
               Reserve + Buffer.Written'First - Buffer.First_Written;
         begin
            if Tail > 0 then
               if Buffer.Free_To_Write + Tail = Buffer.Written'Last then
                  Done := False;
               else
                  Next := Buffer.Written'Last - Tail;
                  if Count < Next - Buffer.Free_To_Write then
                     Next := Buffer.Free_To_Write + Count - 1;
                  end if;
                  Read
                  (  Stream,
                     Buffer.Written (Buffer.Free_To_Write..Next),
                     Last
                  );
                  Done := Last < Next;
                  Next := Last + 1;
               end if;
            else
               Next := Buffer.Written'Last;
               if Count < Next - Buffer.Free_To_Write then
                  Next := Buffer.Free_To_Write + Count - 1;
               end if;
               Read
               (  Stream,
                  Buffer.Written (Buffer.Free_To_Write..Next),
                  Last
               );
               Done := Last < Next;
               if Last < Buffer.Written'Last then
                  Next := Last + 1;
               else
                  Next := Buffer.Written'First;
               end if;
            end if;
         end;
      else
         --
         -- [XXXXX               XXXXXXX]
         --       |              |
         --       Free_To_Write  First_Written
         --
         if Buffer.Free_To_Write + Reserve >= Buffer.First_Written then
            Done := False;
         else
            Next := Buffer.First_Written - Reserve;
            if Count < Next - Buffer.Free_To_Write then
               Next := Buffer.Free_To_Write + Count - 1;
            end if;
            Read
            (  Stream,
               Buffer.Written (Buffer.Free_To_Write..Next),
               Last
            );
            Done := Last < Next;
            Next := Last + 1;
         end if;
      end if;
   end Fill_From_Stream;

   procedure Finalize (Listener : in out Connections_Server) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Worker, Worker_Ptr);
   begin
      if Listener.Doer /= null then
         Listener.Finalizing := True;
         Abort_Selector (Listener.Selector);
         while not Listener.Doer'Terminated loop
            delay 0.001;
         end loop;
         Free (Listener.Doer);
         Close_Selector (Listener.Selector);
      end if;
   end Finalize;

   procedure Finalize (Client : in out Connection) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Encoder'Class, Encoder_Ptr);
   begin
      Close (Client.Socket);
      Free (Client.Transport);
      Object.Finalize (Object.Entity (Client));
   end Finalize;

   function Free (Buffer : Output_Buffer) return Stream_Element_Count is
   begin
      return Buffer.Written'Length - Used (Buffer) - 1;
   end Free;

   function From_String (Data : String) return Stream_Element_Array is
      Result  : Stream_Element_Array (1..Data'Length);
      Pointer : Stream_Element_Offset := Result'First;
   begin
      for Index in Data'Range loop
         Result (Pointer) := Character'Pos (Data (Index));
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_String;

   function Get_Client_Address (Client : Connection)
      return Sock_Addr_Type is
   begin
      return Client.Client_Address;
   end Get_Client_Address;

   function Get_Clients_Count (Listener : Connections_Server)
      return Natural is
   begin
      return Listener.Clients;
   end Get_Clients_Count;

   function Get_IO_Timeout (Factory : Connections_Factory)
      return Duration is
   begin
      return 0.02;
   end Get_IO_Timeout;

   procedure Get_Occurrence
             (  Client : Connection;
                Source : out Exception_Occurrence
             )  is
   begin
      Save_Occurrence (Source, Client.Last_Error);
   end Get_Occurrence;

   function Get_Overlapped_Size (Client : Connection)
      return Stream_Element_Count is
   begin
      return Client.Overlapped_Read;
   end Get_Overlapped_Size;

   function Get_Polling_Timeout (Factory : Connections_Factory)
      return Duration is
   begin
      return 0.5;
   end Get_Polling_Timeout;

   function Get_Socket (Client : Connection) return Socket_Type is
   begin
      return Client.Socket;
   end Get_Socket;

   function Has_Data (Buffer : Input_Buffer) return Boolean is
   begin
      return
      (  Buffer.Free_To_Read /= Buffer.First_Read
      and then
         (  Buffer.Expected = 0
         or else
            Used (Buffer) >= Buffer.Size - 1
      )  );
   end Has_Data;

   function Has_Data (Client : Connection) return Boolean is
   begin
      return Has_Data (Client.Read);
   end Has_Data;

   function Image (Data : Stream_Element_Array) return String is
      Length : Natural := 0;
   begin
      for Index in Data'Range loop
         case Data (Index) is
            when 32..36 | 38..126 =>
               Length := Length + 1;
            when others =>
               Length := Length + 3;
         end case;
      end loop;
      declare
         Result  : String (1..Length);
         Pointer : Integer := 1;
      begin
         for Index in Data'Range loop
            case Data (Index) is
               when 32..36 | 38..126 =>
                  Put
                  (  Destination => Result,
                     Pointer     => Pointer,
                     Value       => Character'Val (Data (Index))
                  );
               when others =>
                  Put
                  (  Destination => Result,
                     Pointer     => Pointer,
                     Value       => '%'
                  );
                  Put
                  (  Destination => Result,
                     Pointer     => Pointer,
                     Value       => Integer (Data (Index)),
                     Base        => 16,
                     Field       => 2,
                     Fill        => '0',
                     Justify     => Right
                  );
            end case;
         end loop;
         return Result;
      end;
   end Image;

   procedure Initialize (Listener : in out Connections_Server) is
   begin
      Listener.IO_Timeout := Get_IO_Timeout (Listener.Factory.all);
      Listener.Polling_Timeout :=
         Get_Polling_Timeout (Listener.Factory.all);
      Listener.Doer := new Worker (Listener'Unchecked_Access);
   end Initialize;

   function Is_Trace_Received_On
            (  Factory : Connections_Factory;
               Encoded : IO_Tracing_Mode
            )  return Boolean is
   begin
      return 0 /= (Factory.Trace_Flags and Receive_Masks (Encoded));
   end Is_Trace_Received_On;

   function Is_Trace_Sent_On
            (  Factory : Connections_Factory;
               Encoded : IO_Tracing_Mode
            )  return Boolean is
   begin
      return 0 /= (Factory.Trace_Flags and Sent_Masks (Encoded));
   end Is_Trace_Sent_On;

   procedure Keep_On_Sending (Client : in out Connection) is
   begin
      Client.Dont_Block := True;
   end Keep_On_Sending;

   procedure Process
             (  Buffer    : in out Input_Buffer;
                Receiver  : in out Connection'Class;
                Data_Left : out Boolean
             )  is
      Last    : Stream_Element_Offset;
      Pointer : Stream_Element_Offset;
   begin
      while Has_Data (Buffer) loop
         if Buffer.Free_To_Read < Buffer.First_Read then
            --
            -- [XXXXXXXXXXXXXX              XXXXX]
            --   Free_To_Read |  First_Read |
            --
            if Buffer.First_Read > Buffer.Read'Last then
               --
               -- [XXXXXXXXXXXXXX                   ]
               --   Free_To_Read |        First_Read |
               --
               Buffer.First_Read := Buffer.Read'First; -- Wrap
               Last := Buffer.Free_To_Read - 1;
            else
               Last := Buffer.Read'Last;
            end if;
         else
            --
            -- [           XXXXXXXXX             ]
            --  First_Read |        | Free_To_Read
            --
            Last := Buffer.Free_To_Read - 1;
         end if;
         Pointer := Last + 1;
         Received
         (  Receiver,
            Buffer.Read (Buffer.First_Read..Last),
            Pointer
         );
         if Pointer < Buffer.First_Read or else Pointer > Last + 1 then
            Raise_Exception
            (  Layout_Error'Identity,
               (  "Subscript error, pointer "
               &  Image (Pointer)
               &  " out of range "
               &  Image (Buffer.First_Read)
               &  ".."
               &  Image (Last)
               &  "+"
            )  );
         elsif Pointer > Buffer.Read'Last then
            if Buffer.Free_To_Read <= Buffer.Read'Last then
               Buffer.First_Read := Buffer.Read'First; -- Wrap
            else
               Buffer.First_Read := Pointer; -- Not yet
            end if;
         else
            Buffer.First_Read := Pointer;
         end if;
         if Pointer <= Last then -- Some input left unprocessed
            Data_Left := True;
            return;
         end if;
      end loop;
      Data_Left := False;
   end Process;

   procedure Process
             (  Listener  : in out Connections_Server;
                Client    : Connection_Ptr;
                Data_Left : out Boolean
             )  is
   begin
      if Client.Transport = null then
         Process (Client.Read, Client.all, Data_Left);
      else
         Process
         (  Client.Transport.all,
            Listener,
            Client.all,
            Data_Left
         );
      end if;
   end Process;

   procedure Process_Packet (Client : in out Connection) is
   begin
      null;
   end Process_Packet;

   procedure Pull
             (  Buffer  : in out Input_Buffer;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      Last   : Stream_Element_Offset;
      Offset : Stream_Element_Offset;
   begin
      while Pointer <= Data'Last and then Has_Data (Buffer) loop
         if Buffer.Free_To_Read < Buffer.First_Read then
            --
            -- [XXXXXXXXXXXXXX              XXXXX]
            --   Free_To_Read |  First_Read |
            --
            if Buffer.First_Read > Buffer.Read'Last then
               --
               -- [XXXXXXXXXXXXXX                   ]
               --   Free_To_Read |        First_Read |
               --
               Buffer.First_Read := Buffer.Read'First; -- Wrap
               Last := Buffer.Free_To_Read - 1;
            else
               Last := Buffer.Read'Last;
            end if;
         else
            --
            -- [           XXXXXXXXX             ]
            --  First_Read |        | Free_To_Read
            --
            Last := Buffer.Free_To_Read - 1;
         end if;
         Offset := Last - Buffer.First_Read;
         if Offset > Data'Last - Pointer then
            Offset := Data'Last - Pointer;
            Last   := Buffer.First_Read + Offset;
         end if;
         Data (Pointer..Pointer + Offset) :=
            Buffer.Read (Buffer.First_Read..Last);
         Pointer := Pointer + Offset + 1;
         if Last >= Buffer.Read'Last then
            if Buffer.Free_To_Read <= Buffer.Read'Last then
               Buffer.First_Read := Buffer.Read'First; -- Wrap
            else
               Buffer.First_Read := Last + 1; -- Not yet
            end if;
         else
            Buffer.First_Read := Last + 1;
         end if;
      end loop;
   end Pull;

   procedure Push
             (  Client : in out Connection;
                Data   : Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      if Client.Transport = null then
         Send_Socket (Client.Socket, Data, Last);
      else
         Encode (Client.Transport.all, Client, Data, Last);
      end if;
      if Last + 1 /= Data'First then
         Client.Data_Sent := True;
         if (  0
            /= (  Client.Listener.Factory.Trace_Flags
               and
                  Trace_Decoded_Sent
            )  )
         then
            Trace_Sent
            (  Factory => Client.Listener.Factory.all,
               Client  => Client,
               Data    => Data,
               From    => Data'First,
               To      => Last,
               Encoded => False
            );
         end if;
      end if;
   end Push;

   procedure Queue
             (  Client  : in out Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Store
      (  Client.Written,
         Data,
         Pointer,
         Client.Listener.Unblock_Send
      );
   end Queue;

   function Queued_To_Send (Client : Connection)
      return Stream_Element_Count is
   begin
      return Used (Client.Written);
   end Queued_To_Send;

   procedure Read
             (  Client  : in out Connection;
                Factory : in out Connections_Factory'Class
             )  is
      Buffer : Input_Buffer renames Client.Read;
      Last   : Stream_Element_Offset;
   begin
      if Client.Overlapped_Read < Queued_To_Send (Client) then
         return; -- Not ready to read yet
      elsif Buffer.Free_To_Read < Buffer.First_Read then
         --
         -- [XXXXXXXXXXXXXX              XXXXX]
         --   Free_To_Read |  First_Read |
         --
         Last := Buffer.First_Read - 2;
         if Last <= Buffer.First_Read then -- Read buffer is full
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
            return;
         elsif Buffer.Free_To_Read > Buffer.Read'Last then -- Wrap
            Buffer.Free_To_Read := Buffer.Read'First;
            Last := Buffer.First_Read - 2;
         else
            Last := Buffer.Read'Last;
         end if;
      end if;
      Receive_Socket
      (  Client.Socket,
         Buffer.Read (Buffer.Free_To_Read..Last),
         Last
      );
      if Client.Transport = null then
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
      else
         if 0 /= (Factory.Trace_Flags and Trace_Encoded_Received) then
            Trace_Received
            (  Factory => Factory,
               Client  => Client,
               Data    => Buffer.Read,
               From    => Buffer.Free_To_Read,
               To      => Last,
               Encoded => True
            );
         end if;
      end if;
      if Last = Buffer.Free_To_Read - 1 then -- Nothing read
         raise Connection_Error;
      end if;
      Buffer.Expected :=
         Stream_Element_Offset'Max
         (  Buffer.Expected - (Last - Buffer.Free_To_Read + 1),
            0
         );
      Buffer.Free_To_Read := Last + 1;
   exception
      when Error : Socket_Error | Layout_Error =>
         Receive_Error (Client, Error);
         raise Connection_Error;
   end Read;

   procedure Receive_Socket
             (  Client : in out Connection;
                Data   : in out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      Receive_Socket (Client.Socket, Data, Last);
   end Receive_Socket;

   procedure Remove
             (  List  : in out Connection_Ptr;
                Item  : in out Connection'Class;
                Count : in out Integer
             )  is
   begin
      if Item.Successor /= null then
         Count := Count - 1;
         if List = Item.Successor.Predecessor then -- First in the list
            if List = List.Successor then -- The single item of the list
               List := null;
               Item.Successor := null;
               return;
            else
               List := Item.Successor;
            end if;
         end if;
         Item.Predecessor.Successor := Item.Successor;
         Item.Successor.Predecessor := Item.Predecessor;
         Item.Successor := null;
      end if;
   end Remove;

   procedure Receive_Error
             (  Client     : in out Connection;
                Occurrence : Exception_Occurrence
             )  is
   begin
      null;
   end Receive_Error;

   procedure Save_Occurrence
             (  Client : in out Connection;
                Source : Exception_Occurrence
             )  is
   begin
      Save_Occurrence (Client.Last_Error, Source);
   end Save_Occurrence;

   procedure Send
             (  Client  : in out Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      Buffer : Output_Buffer renames Client.Written;
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - 1 > Data'Last
         )  )
      then
         Raise_Exception (Layout_Error'Identity, "Subscript error");
      end if;
      Store (Buffer, Data, Pointer, Client.Listener.Unblock_Send);
      if (  Buffer.Send_Blocked
         and then
            Buffer.Free_To_Write /= Buffer.First_Written
         )
      then -- Request socket unblocking
         Buffer.Send_Blocked := False;
         Client.Listener.Unblock_Send := True;
      end if;
   end Send;

   procedure Send
             (  Client  : in out Connection;
                Data    : String;
                Pointer : in out Integer
             )  is
      Buffer : Output_Buffer renames Client.Written;
   begin
      Pointer := Data'Last + 1;
      for Index in Data'Range loop
         if Used (Buffer) + 1 >= Buffer.Written'Length then
            Pointer := Index;
            exit;
         end if;
         Buffer.Written (Buffer.Free_To_Write) :=
            Stream_Element (Character'Pos (Data (Index)));
         if Buffer.Free_To_Write = Buffer.Written'Last then
            Buffer.Free_To_Write := Buffer.Written'First;
         else
            Buffer.Free_To_Write := Buffer.Free_To_Write + 1;
         end if;
      end loop;
      if (  Buffer.Send_Blocked
         and then
            Buffer.Free_To_Write /= Buffer.First_Written
         )
      then -- Request socket unblocking
         Buffer.Send_Blocked := False;
         Client.Listener.Unblock_Send := True;
      end if;
   end Send;

   procedure Send
             (  Client : in out Connection;
                Stream : in out Root_Stream_Type'Class;
                End_Of_Stream : out Boolean
             )  is
      Buffer : Output_Buffer renames Client.Written;
      Last   : Stream_Element_Offset;
      Next   : Stream_Element_Offset;
   begin
      Fill_From_Stream
      (  Buffer  => Buffer,
         Stream  => Stream,
         Count   => Stream_Element_Count'Last,
         Reserve => 1,
         Last    => Last,
         Next    => Next,
         Done    => End_Of_Stream
      );
      Buffer.Free_To_Write := Next;
      if (  Buffer.Send_Blocked
         and then
            Buffer.Free_To_Write /= Buffer.First_Written
         )
      then -- Request socket unblocking
         Buffer.Send_Blocked := False;
         Client.Listener.Unblock_Send := True;
      end if;
   end Send;

   procedure Send
             (  Client : in out Connection;
                Stream : in out Root_Stream_Type'Class;
                Count  : in out Stream_Element_Count;
                End_Of_Stream : out Boolean
             )  is
      Buffer : Output_Buffer renames Client.Written;
      Last   : Stream_Element_Offset;
      Next   : Stream_Element_Offset;
   begin
      Fill_From_Stream
      (  Buffer  => Buffer,
         Stream  => Stream,
         Count   => Count,
         Reserve => 1,
         Last    => Last,
         Next    => Next,
         Done    => End_Of_Stream
      );
      Count := Count - (Last + 1 - Buffer.Free_To_Write);
      Buffer.Free_To_Write := Next;
      if (  Buffer.Send_Blocked
         and then
            Buffer.Free_To_Write /= Buffer.First_Written
         )
      then -- Request socket unblocking
         Buffer.Send_Blocked := False;
         Client.Listener.Unblock_Send := True;
      end if;
   end Send;

   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Reserve       : Stream_Element_Count;
                Get_Prefix    : Create_Stream_Element_Array;
                Get_Suffix    : Create_Stream_Element_Array;
                End_Of_Stream : out Boolean
             )  is
      Count : Stream_Element_Count := Stream_Element_Count'Last;
   begin
      Send
      (  Client        => Client,
         Stream        => Stream,
         Count         => Count,
         Reserve       => Reserve,
         Get_Prefix    => Get_Prefix,
         Get_Suffix    => Get_Suffix,
         End_Of_Stream => End_Of_Stream
      );
   end Send;

   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Count         : in out Stream_Element_Count;
                Reserve       : Stream_Element_Count;
                Get_Prefix    : Create_Stream_Element_Array;
                Get_Suffix    : Create_Stream_Element_Array;
                End_Of_Stream : out Boolean
             )  is
      Buffer : Output_Buffer renames Client.Written;
      Last   : Stream_Element_Offset;
      Next   : Stream_Element_Offset;
   begin
      if Buffer.Free_To_Write = Buffer.First_Written then
         if Buffer.Written'Length <= Reserve then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Output buffer size"
               &  Stream_Element_Count'Image (Buffer.Written'Length)
               &  " is less than required"
               &  Stream_Element_Count'Image (Reserve + 1)
               &  " elements"
            )  );
         end if;
         Fill_From_Stream
         (  Buffer  => Buffer,
            Stream  => Stream,
            Count   => Count,
            Reserve => Reserve + 1,
            Last    => Last,
            Next    => Next,
            Done    => End_Of_Stream
         );
         Count := Count - (Last + 1 - Buffer.Free_To_Write);
         declare
            Header : Stream_Element_Array :=
                     Get_Prefix.all
                     (  Client'Unchecked_Access,
                        Buffer.Written (Buffer.Free_To_Write..Last),
                        End_Of_Stream
                     );
            Tail   : Stream_Element_Array :=
                     Get_Suffix.all
                     (  Client'Unchecked_Access,
                        Buffer.Written (Buffer.Free_To_Write..Last),
                        End_Of_Stream
                     );
         begin
            if Header'Length + Tail'Length > Reserve then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Prefix returns more than"
                  &  Stream_Element_Count'Image (Reserve)
                  &  " elements"
               )  );
            elsif Header'Length > 0 then
               Last := Buffer.First_Written;
               for Index in reverse Header'Range loop
                  if Last = Buffer.Written'First then
                     Last := Buffer.Written'Last;
                  else
                     Last := Last - 1;
                  end if;
                  Buffer.Written (Last) := Header (Index);
               end loop;
            end if;
            Buffer.First_Written := Last;
            Buffer.Free_To_Write := Next;
            if Tail'Length > 0 then
               Last := Tail'First;
               Store (Buffer, Tail, Last, Client.Listener.Unblock_Send);
            end if;
         end;
      else
         End_Of_Stream := False;
      end if;
      if (  Buffer.Send_Blocked
         and then
            Buffer.Free_To_Write /= Buffer.First_Written
         )
      then -- Request socket unblocking
         Buffer.Send_Blocked := False;
         Client.Listener.Unblock_Send := True;
      end if;
   end Send;

   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Count         : in out Stream_Element_Count;
                Reserve       : Natural;
                Get_Prefix    : Create_String;
                Get_Suffix    : Create_String;
                End_Of_Stream : out Boolean
             )  is
      Buffer : Output_Buffer renames Client.Written;
      Last   : Stream_Element_Offset;
      Next   : Stream_Element_Offset;
   begin
      if Buffer.Free_To_Write = Buffer.First_Written then
         if Buffer.Written'Length <= Reserve then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Output buffer size"
               &  Stream_Element_Count'Image (Buffer.Written'Length)
               &  " is less than required"
               &  Integer'Image (Reserve + 1)
               &  " elements"
            )  );
         end if;
         Fill_From_Stream
         (  Buffer  => Buffer,
            Stream  => Stream,
            Count   => Count,
            Reserve => Stream_Element_Count (Reserve) + 1,
            Last    => Last,
            Next    => Next,
            Done    => End_Of_Stream
         );
         Count := Count - (Last + 1 - Buffer.Free_To_Write);
         declare
            Header : String :=
                     Get_Prefix.all
                     (  Client'Unchecked_Access,
                        Buffer.Written (Buffer.Free_To_Write..Last),
                        End_Of_Stream
                     );
            Tail   : String :=
                     Get_Suffix.all
                     (  Client'Unchecked_Access,
                        Buffer.Written (Buffer.Free_To_Write..Last),
                        End_Of_Stream
                     );
         begin
            if Header'Length + Tail'Length > Reserve then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Prefix returns more than"
                  &  Integer'Image (Reserve)
                  &  " elements"
               )  );
            elsif Header'Length > 0 then
               Last := Buffer.First_Written;
               for Index in reverse Header'Range loop
                  if Last = Buffer.Written'First then
                     Last := Buffer.Written'Last;
                  else
                     Last := Last - 1;
                  end if;
                  Buffer.Written (Last) :=
                     Stream_Element (Character'Pos (Header (Index)));
               end loop;
            end if;
            Buffer.First_Written := Last;
            Buffer.Free_To_Write := Next;
            if Tail'Length > 0 then
               declare
                  Pointer : Integer := Tail'First;
               begin
                  Send (Client, Tail, Pointer);
               end;
            end if;
         end;
      else
         End_Of_Stream := False;
      end if;
      if (  Buffer.Send_Blocked
         and then
            Buffer.Free_To_Write /= Buffer.First_Written
         )
      then -- Request socket unblocking
         Buffer.Send_Blocked := False;
         Client.Listener.Unblock_Send := True;
      end if;
   end Send;

   procedure Send
             (  Client        : in out Connection;
                Stream        : in out Root_Stream_Type'Class;
                Reserve       : Natural;
                Get_Prefix    : Create_String;
                Get_Suffix    : Create_String;
                End_Of_Stream : out Boolean
             )  is
      Count : Stream_Element_Count := Stream_Element_Count'Last;
   begin
      Send
      (  Client        => Client,
         Stream        => Stream,
         Count         => Count,
         Reserve       => Reserve,
         Get_Prefix    => Get_Prefix,
         Get_Suffix    => Get_Suffix,
         End_Of_Stream => End_Of_Stream
      );
   end Send;

   procedure Send_Error
             (  Client     : in out Connection;
                Occurrence : Exception_Occurrence
             )  is
   begin
      null;
   end Send_Error;

   procedure Send_Socket
             (  Client : in out Connection;
                Data   : Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      Send_Socket (Client.Socket, Data, Last);
   end Send_Socket;

   procedure Sent (Client : in out Connection) is
   begin
      null;
   end Sent;

   procedure Set_Expected_Count
             (  Client : in out Connection;
                Count  : Stream_Element_Count
             )  is
   begin
      Client.Read.Expected := Count;
   end Set_Expected_Count;

   procedure Service_Postponed (Listener : in out Connections_Server) is
      Leftover  : Connection_Ptr;
      Client    : Connection_Ptr;
      Data_Left : Boolean;
      Count     : Integer := 0;
   begin
      loop
         Client := Listener.Postponed;
         exit when Client = null;
         Remove (Listener.Postponed, Client.all, Count);
         begin
            Process (Listener, Client, Data_Left);
            if Data_Left then
               Append (Leftover, Client, Count);
            end if;
         exception
            when Connection_Error =>
               Stop (Listener, Client.Socket);
            when Error : others =>
               Trace_Error
               (  Listener.Factory.all,
                  "Postponed service",
                  Error
               );
               Stop (Listener, Client.Socket);
         end;
      end loop;
      Listener.Postponed := Leftover;
   end Service_Postponed;

   procedure Set_Failed
             (  Client : in out Connection;
                Error  : Exception_Occurrence
             )  is
   begin
      Save_Occurrence (Client.Last_Error, Error);
      Client.Failed := True;
   end Set_Failed;

   procedure Set_Overlapped_Size
             (  Client : in out Connection;
                Size   : Stream_Element_Count
             )  is
   begin
      Client.Overlapped_Read := Size;
   end Set_Overlapped_Size;

   procedure Stop
             (  Listener : in out Connections_Server'Class;
                Socket   : Socket_Type
             )  is
      Client : Socket_Type := Socket;
      This   : Connection_Ptr := Get (Listener.Connections, Client);
   begin
      if This /= null then
         Disconnected (Listener, This.all);
         Listener.Clients := Listener.Clients - 1;
      end if;
      Clear (Listener.Read_Sockets,    Client);
      Clear (Listener.Write_Sockets,   Client);
      Clear (Listener.Blocked_Sockets, Client);
      Put (Listener.Connections, Client, null);
   end Stop;

   procedure Store
             (  Buffer  : in out Output_Buffer;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Unblock : out Boolean
             )  is
      Free  : Stream_Element_Offset;
      Count : Stream_Element_Offset := Data'Last - Pointer + 1;
   begin
      if Buffer.First_Written = Buffer.Free_To_Write then
         --
         -- Moving  First_Written  as far back  as possible  to diminish
         -- buffer  fragmenting.  We cannot  move  it further  than  the
         -- number of elements we put there,  because of race condition,
         -- when Free_To_Write is not yet set.  But  when  Free_To_Write
         -- points into the elements written everything is OK
         --
         -- [   ............        ]
         --     |<--Count-->|
         --     |           Free_To_Write = First_Written
         --     new First_Written
         --
         Count :=
            Stream_Element_Offset'Min
            (  Buffer.Written'Length - 1,
               Count
            );
         Free := Stream_Element_Offset'Max
                 (  Buffer.Written'First,
                    Buffer.Free_To_Write - Count
                 );
         Buffer.Written (Free..Free + Count - 1) :=
            Data (Pointer..Pointer + Count - 1);
         Pointer := Pointer + Count;
         Buffer.First_Written := Free;
         Buffer.Free_To_Write := Free + Count;
         if Buffer.Send_Blocked then
            Buffer.Send_Blocked := False;
            Unblock := True;
         end if;
         return;
      elsif Buffer.First_Written < Buffer.Free_To_Write then
         --
         -- [     XXXXXXXXXXXXXXX        ]
         --       |              |
         --       First_Written  Free_To_Write
         --
         Free :=
            (  Buffer.Written'Length
            -  Buffer.Free_To_Write
            +  Buffer.First_Written
            -  1  -- Last element is never written
            );
         if Free <= 0 then
            return;
         end if;
         declare
            Tail : Stream_Element_Offset :=
                   Stream_Element_Offset'Min
                   (  Buffer.Written'Last - Buffer.Free_To_Write + 1,
                      Free
                   );
         begin
            if Count <= Tail then -- Can queue all Count elements
               Buffer.Written
               (  Buffer.Free_To_Write
               .. Buffer.Free_To_Write + Count - 1
               )  := Data (Pointer..Data'Last);
               Pointer := Data'Last + 1;
               Free := Buffer.Free_To_Write + Count;
               if Free > Buffer.Written'Last then
                  Buffer.Free_To_Write := Buffer.Written'First;
               else
                  Buffer.Free_To_Write := Free;
               end if;
               return;
            end if; -- Can queue only Tail elements
            Buffer.Written
            (  Buffer.Free_To_Write
            .. Buffer.Free_To_Write + Tail - 1
            )  := Data (Pointer..Pointer + Tail - 1);
            Pointer := Pointer + Tail;
            Count   := Count   - Tail;
            Free    := Free    - Tail;
            if Buffer.Free_To_Write + Tail > Buffer.Written'Last then
               Buffer.Free_To_Write := Buffer.Written'First;
            else
               Buffer.Free_To_Write := Buffer.Free_To_Write + Tail;
            end if;
         end;
      else
         --
         -- [XXXXX               XXXXXXXX]
         --       |              |
         --       Free_To_Write  First_Written
         --
         Free :=
            (  Buffer.First_Written
            +  Buffer.Free_To_Write
            -  1  -- Last element is never written
            );
      end if;
      if Free <= 0 then
         return;
      end if;
      Count := Stream_Element_Offset'Min (Count, Free);
      Buffer.Written
      (  Buffer.Free_To_Write
      .. Buffer.Free_To_Write + Count - 1
      ) := Data (Pointer..Pointer + Count - 1);
      Pointer := Pointer + Count;
      Buffer.Free_To_Write := Buffer.Free_To_Write + Count;
      if Buffer.Send_Blocked then
         Buffer.Send_Blocked := False;
         Unblock := True;
      end if;
   end Store;

   function To_String (Data : Stream_Element_Array) return String is
      Result : String (1..Data'Length);
      Index  : Integer := Result'First;
   begin
      for Item in Data'Range loop
         Result (Index) := Character'Val (Data (Item));
         Index := Index + 1;
      end loop;
      return Result;
   end To_String;

   procedure Trace
             (  Factory : in out Connections_Factory;
                Message : String
             )  is
      use Ada.Text_IO;
   begin
      if 0 /= (Factory.Trace_Flags and Standard_Output) then
         Put_Line (Message);
      end if;
      if Is_Open (Factory.Trace_File) then
         Put_Line (Factory.Trace_File, Message);
      end if;
   end Trace;

   procedure Trace_Error
             (  Factory    : in out Connections_Factory;
                Context    : String;
                Occurrence : Exception_Occurrence
             )  is
   begin
      Trace
      (  Factory,
         Context & ": " & Exception_Information (Occurrence)
      );
   end Trace_Error;

   procedure Trace_Off (Factory : in out Connections_Factory) is
      use Ada.Text_IO;
   begin
      if Is_Open (Factory.Trace_File) then
         Close (Factory.Trace_File);
      end if;
      Factory.Trace_Flags :=
         Factory.Trace_Flags and not Standard_Output;
   end Trace_Off;

   procedure Trace_On
             (  Factory  : in out Connections_Factory;
                Received : IO_Tracing_Mode := Trace_None;
                Sent     : IO_Tracing_Mode := Trace_None
             )  is
      Flags : Factory_Flags := Standard_Output;
   begin
      case Received is
         when Trace_Any =>
            Flags := Flags
                  or Trace_Decoded_Received
                  or Trace_Encoded_Received;
         when Trace_Decoded =>
            Flags := Flags or Trace_Decoded_Received;
         when Trace_Encoded =>
            Flags := Flags or Trace_Encoded_Received;
         when Trace_None =>
            null;
      end case;
      case Sent is
         when Trace_Any =>
            Flags := Flags or Trace_Decoded_Sent or Trace_Encoded_Sent;
         when Trace_Decoded =>
            Flags := Flags or Trace_Decoded_Sent;
         when Trace_Encoded =>
            Flags := Flags or Trace_Encoded_Sent;
         when Trace_None =>
            null;
      end case;
      Factory.Trace_Flags := Flags;
   end Trace_On;

   procedure Trace_On
             (  Factory  : in out Connections_Factory;
                Name     : String;
                Received : IO_Tracing_Mode := Trace_None;
                Sent     : IO_Tracing_Mode := Trace_None
             )  is
      use Ada.Text_IO;
      Flags : Factory_Flags := 0;
   begin
      if Is_Open (Factory.Trace_File) then
         Close (Factory.Trace_File);
      end if;
      Create (File => Factory.Trace_File, Name => Name);
      case Received is
         when Trace_Any =>
            Flags := Flags
                  or Trace_Decoded_Received
                  or Trace_Encoded_Received;
         when Trace_Decoded =>
            Flags := Flags or Trace_Decoded_Received;
         when Trace_Encoded =>
            Flags := Flags or Trace_Encoded_Received;
         when Trace_None =>
            null;
      end case;
      case Sent is
         when Trace_Any =>
            Flags := Flags or Trace_Decoded_Sent or Trace_Encoded_Sent;
         when Trace_Decoded =>
            Flags := Flags or Trace_Decoded_Sent;
         when Trace_Encoded =>
            Flags := Flags or Trace_Encoded_Sent;
         when Trace_None =>
            null;
      end case;
      Factory.Trace_Flags := Flags;
   end Trace_On;

   procedure Trace_Received
             (  Factory : in out Connections_Factory;
                Client  : Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset;
                Encoded : Boolean := False
             )  is
   begin
      if Encoded then
         Trace
         (  Factory,
            (  Image (Get_Client_Address (Client))
            &  " encoded> |"
            &  Image (Data (From..To))
            &  "| "
            &  Image (From)
            &  ".."
            &  Image (To)
         )  );
      else
         Trace
         (  Factory,
            (  Image (Get_Client_Address (Client))
            &  " > |"
            &  Image (Data (From..To))
            &  "| "
            &  Image (From)
            &  ".."
            &  Image (To)
         )  );
      end if;
   end Trace_Received;

   procedure Trace_Sending
             (  Factory : in out Connections_Factory;
                Client  : Connection'Class;
                Enabled : Boolean;
                Reason  : String
             )  is
   begin
      if Enabled then
         Trace
         (  Factory,
            (  Image (Get_Client_Address (Client))
            &  " < +++ Resume polling"
            &  Reason
         )  );
      else
         Trace
         (  Factory,
            (  Image (Get_Client_Address (Client))
            &  " < --- Stop polling"
            &  Reason
         )  );
      end if;
   end Trace_Sending;

   procedure Trace_Sent
             (  Factory : in out Connections_Factory;
                Client  : Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset;
                Encoded : Boolean := False
             )  is
   begin
      if Encoded then
         Trace
         (  Factory,
            (  Image (Get_Client_Address (Client))
            &  " <encoded |"
            &  Image (Data (From..To))
            &  "| "
            &  Image (From)
            &  ".."
            &  Image (To)
         )  );
      else
         Trace
         (  Factory,
            (  Image (Get_Client_Address (Client))
            &  " < |"
            &  Image (Data (From..To))
            &  "| "
            &  Image (From)
            &  ".."
            &  Image (To)
         )  );
      end if;
   end Trace_Sent;

   procedure Unblock_Send (Client : in out Connection) is
      Buffer : Output_Buffer renames Client.Written;
   begin
      if Buffer.Send_Blocked then -- Request socket unblocking
         Buffer.Send_Blocked := False;
         Client.Listener.Unblock_Send := True;
         Abort_Selector (Client.Listener.Selector);
      end if;
   end Unblock_Send;

   function Used (Buffer : Input_Buffer) return Stream_Element_Count is
      Diff : Stream_Element_Offset :=
             Buffer.Free_To_Read - Buffer.First_Read;
   begin
      if Diff < 0 then
         return Buffer.Read'Length - Diff;
      else
         return Diff;
      end if;
   end Used;

   function Used (Buffer : Output_Buffer) return Stream_Element_Count is
   begin
      if Buffer.Free_To_Write >= Buffer.First_Written then
         return Buffer.Free_To_Write - Buffer.First_Written;
      else
         return Buffer.Written'Length - Buffer.First_Written +
                Buffer.Free_To_Write;
      end if;
   end Used;

   procedure Write
             (  Client  : in out Connection;
                Factory : in out Connections_Factory'Class;
                Blocked : out Boolean
             )  is
      Buffer : Output_Buffer renames Client.Written;
      Next   : Stream_Element_Count;
   begin
      Blocked := Buffer.First_Written = Buffer.Free_To_Write;
      if Blocked then
         if Client.Dont_Block then
            Blocked           := False;
            Client.Data_Sent  := True;
            Client.Dont_Block := False;
         end if;
      else
         loop
            if Buffer.First_Written > Buffer.Free_To_Write then
               --
               -- [XXXXX               XXXXXXX]
               --       |              |
               --       Free_To_Write  First_Written
               --
               if Client.Transport = null then
                  Send_Socket
                  (  Client.Socket,
                     Buffer.Written
                     (  Buffer.First_Written
                     .. Buffer.Written'Last
                     ),
                     Next
                  );
               else
                  Encode
                  (  Client.Transport.all,
                     Client,
                     Buffer.Written
                     (  Buffer.First_Written
                     .. Buffer.Written'Last
                     ),
                     Next
                  );
               end if;
               Next := Next + 1;
               if Next = Buffer.First_Written then
                  exit; -- Cannot send anything right now
               elsif Next <= Buffer.Written'Last then
                  if 0 /= (Factory.Trace_Flags and Trace_Decoded_Sent)
                  then
                     Trace_Sent
                     (  Factory => Factory,
                        Client  => Client,
                        Data    => Buffer.Written,
                        From    => Buffer.First_Written,
                        To      => Next - 1,
                        Encoded => False
                     );
                  end if;
                  Buffer.First_Written := Next;
                  Client.Data_Sent := True;
                  exit;
               end if;
               if 0 /= (Factory.Trace_Flags and Trace_Decoded_Sent) then
                  Trace_Sent
                  (  Factory => Factory,
                     Client  => Client,
                     Data    => Buffer.Written,
                     From    => Buffer.First_Written,
                     To      => Next - 1,
                     Encoded => False
                  );
               end if;
               Buffer.First_Written := 0;
               Client.Data_Sent := True;
            else
               --
               -- [     XXXXXXXXXXXXXXX        ]
               --       |              |
               --       First_Written  Free_To_Write
               --
               if Client.Transport = null then
                  Send_Socket
                  (  Client.Socket,
                     Buffer.Written
                     (  Buffer.First_Written
                     .. Buffer.Free_To_Write - 1
                     ),
                     Next
                  );
               else
                  Encode
                  (  Client.Transport.all,
                     Client,
                     Buffer.Written
                     (  Buffer.First_Written
                     .. Buffer.Free_To_Write - 1
                     ),
                     Next
                  );
               end if;
               Next := Next + 1;
               if Next = Buffer.First_Written then
                  exit;
               elsif Next <= Buffer.Free_To_Write then
                  if 0 /= (Factory.Trace_Flags and Trace_Decoded_Sent)
                  then
                     Trace_Sent
                     (  Factory => Factory,
                        Client  => Client,
                        Data    => Buffer.Written,
                        From    => Buffer.First_Written,
                        To      => Next - 1,
                        Encoded => False
                     );
                  end if;
                  Buffer.First_Written := Next;
                  Client.Data_Sent := True;
                  exit;
               end if;
               if 0 /= (Factory.Trace_Flags and Trace_Decoded_Sent) then
                  Trace_Sent
                  (  Factory => Factory,
                     Client  => Client,
                     Data    => Buffer.Written,
                     From    => Buffer.First_Written,
                     To      => Next - 1,
                     Encoded => False
                  );
               end if;
               Buffer.First_Written := Next;
               Client.Data_Sent := True;
            end if;
            exit when Buffer.First_Written = Buffer.Free_To_Write;
         end loop;
      end if;
   end Write;

   function Get_Server_Address
            (  Listener : Connections_Server
            )  return Sock_Addr_Type is
      Address : Sock_Addr_Type;
   begin
      Address.Addr := Any_Inet_Addr;
      Address.Port := Listener.Port;
      return Address;
   end Get_Server_Address;

   task body Worker is
      Address       : Sock_Addr_Type :=
                      Get_Server_Address (Listener.all);
      Server_Socket : Socket_Type;
      Client_Socket : Socket_Type;
      Read_Sockets  : Socket_Set_Type;
      Write_Sockets : Socket_Set_Type;
      That_Time     : Time := Clock;
      This_Time     : Time;
      Status        : Selector_Status;

      procedure Unblock (Requested_Only : Boolean) is
         Socket : Socket_Type;
      begin
         loop
            Get (Listener.Blocked_Sockets, Socket);
            exit when Socket = No_Socket;
            declare
               Client : Connection_Ptr :=
                        Get (Listener.Connections, Socket);
            begin
               if Client = null then
                  Clear (Listener.Read_Sockets,    Client_Socket);
                  Clear (Listener.Write_Sockets,   Client_Socket);
                  Clear (Listener.Blocked_Sockets, Client_Socket);
               elsif Client.Failed then
                  if (  Exception_Identity (Client.Last_Error)
                     /= Connection_Error'Identity
                     )
                  then
                     Trace_Error
                     (  Listener.Factory.all,
                        "Unblocking socket",
                        Client.Last_Error
                     );
                  end if;
                  Stop (Listener.all, Client.Socket);
               elsif (  Requested_Only
                     and then
                        Client.Written.Send_Blocked
                     )  then -- Keep it blocked
                  Set (Read_Sockets, Client.Socket);
               else -- Unblock
                  Set (Listener.Write_Sockets, Client.Socket);
                  Set (Write_Sockets, Client.Socket);
                  Status := Completed;  -- Make sure it written later on
                  Client.Written.Send_Blocked := False;
                  Client.Data_Sent := True;
                  if (  0
                     /= (  Listener.Factory.Trace_Flags
                        and
                           (Trace_Encoded_Sent or Trace_Decoded_Sent)
                     )  )
                  then
                     if Requested_Only then
                        Trace_Sending
                        (  Listener.Factory.all,
                           Client.all,
                           True,
                           ", some data to send"
                        );
                     else
                        Trace_Sending
                        (  Listener.Factory.all,
                           Client.all,
                           True,
                           ", blocking timeout expired"
                        );
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end Unblock;
   begin
      Create_Socket (Server_Socket);
      Set_Socket_Option
      (  Server_Socket,
         Socket_Level,
         (Reuse_Address, True)
      );
      Bind_Socket (Server_Socket, Address);
      Listen_Socket (Server_Socket);
      Set (Listener.Read_Sockets, Server_Socket);
      Create_Selector (Listener.Selector);
      loop
         Copy (Listener.Read_Sockets,  Read_Sockets);
         Copy (Listener.Write_Sockets, Write_Sockets);
         Check_Selector
         (  Selector     => Listener.Selector,
            R_Socket_Set => Read_Sockets,
            W_Socket_Set => Write_Sockets,
            Status       => Status,
            Timeout      => Listener.IO_Timeout
         );
         exit when Status = Aborted and then Listener.Finalizing;
         if Status = Completed then
            loop -- Reading from sockets
               Get (Read_Sockets, Client_Socket);
               exit when Client_Socket = No_Socket;
               if Client_Socket = Server_Socket then
                  Accept_Socket
                  (  Server_Socket,
                     Client_Socket,
                     Address
                  );
                  declare
                     Client : Connection_Ptr;
                  begin
                     Client :=
                        Create (Listener.Factory, Listener, Address);
                     if Client = null then
                        Close (Client_Socket);
                     else
                        declare
                           This : Connection'Class renames
                                  Client.all;
                        begin
                           This.Client_Address := Address;
                           This.Socket := Client_Socket;
                           This.Listener :=
                              Listener.all'Unchecked_Access;
                           Set (Listener.Read_Sockets,  Client_Socket);
                           Set (Listener.Write_Sockets, Client_Socket);
                           Put
                           (  Listener.Connections,
                              Client_Socket,
                              Client
                           );
                           Listener.Clients :=  Listener.Clients + 1;
                           This.Transport :=
                              Create_Transport
                              (  Listener.Factory,
                                 Listener,
                                 Client
                              );
                           Connected (This);
                        end;
                     end if;
                  exception
                     when Connection_Error =>
                        if Client /= null then
                           Stop (Listener.all, Client.Socket);
                           Client := null;
                        end if;
                     when Error : others =>
                        Trace_Error
                        (  Listener.Factory.all,
                           "Accept socket",
                           Error
                        );
                        if Client /= null then
                           Stop (Listener.all, Client.Socket);
                           Client := null;
                        end if;
                  end;
               else
                  declare
                     Client : Connection_Ptr :=
                              Get (Listener.Connections, Client_Socket);
                  begin
                     if Client = null then
                        Clear (Listener.Read_Sockets,    Client_Socket);
                        Clear (Listener.Write_Sockets,   Client_Socket);
                        Clear (Listener.Blocked_Sockets, Client_Socket);
                     elsif Client.Failed then
                        if (  Exception_Identity (Client.Last_Error)
                           /= Connection_Error'Identity
                           )
                        then
                           Trace_Error
                           (  Listener.Factory.all,
                              "Preparing to receive",
                              Client.Last_Error
                           );
                        end if;
                        Stop (Listener.all, Client.Socket);
                     else
                        begin
                           Read (Client.all, Listener.Factory.all);
                        exception
                           when Connection_Error =>
                              Stop (Listener.all, Client.Socket);
                              Client := null;
                           when Error : Socket_Error =>
                              Send_Error (Client.all, Error);
                              Stop (Listener.all, Client.Socket);
                              Client := null;
                           when Error : others =>
                              Trace_Error
                              (  Listener.Factory.all,
                                 "Receive socket",
                                 Error
                              );
                              Stop (Listener.all, Client.Socket);
                              Client := null;
                        end;
                        declare
                           Data_Left : Boolean;
                           Count     : Integer := 0;
                        begin
                           if Client /= null then
                              Process (Listener.all, Client, Data_Left);
                              if Data_Left then
                                 Append
                                 (  Listener.Postponed,
                                    Client,
                                    Count
                                 );
                              end if;
                           end if;
                        exception
                           when Connection_Error =>
                              Stop (Listener.all, Client.Socket);
                           when Error : others =>
                              Trace_Error
                              (  Listener.Factory.all,
                                 "Processing received",
                                 Error
                              );
                              Stop (Listener.all, Client.Socket);
                        end;
                     end if;
                  end;
               end if;
            end loop;
         end if;
         This_Time := Clock;
         if This_Time - That_Time > Listener.Polling_Timeout then
            -- Unblock everything now
            That_Time := This_Time;
            Unblock (False);
         else
            -- Checking for explicit unblocking requests
            while Listener.Unblock_Send loop
               Listener.Unblock_Send := False;
               Unblock (True);
               Copy (Read_Sockets, Listener.Blocked_Sockets);
            end loop;
         end if;
         if Status = Completed then
            loop -- Writing sockets
               Get (Write_Sockets, Client_Socket);
               exit when Client_Socket = No_Socket;
               declare
                  Client : Connection_Ptr :=
                           Get (Listener.Connections, Client_Socket);
               begin
                  if Client = null then
                     Clear (Listener.Read_Sockets,    Client_Socket);
                     Clear (Listener.Write_Sockets,   Client_Socket);
                     Clear (Listener.Blocked_Sockets, Client_Socket);
                  elsif Client.Failed then
                     if (  Exception_Identity (Client.Last_Error)
                        /= Connection_Error'Identity
                        )
                     then
                        Trace_Error
                        (  Listener.Factory.all,
                           "Preparing to send",
                           Client.Last_Error
                        );
                     end if;
                     Stop (Listener.all, Client.Socket);
                  else
                     declare
                        Block : Boolean;
                     begin
                        Write
                        (  Client.all,
                           Listener.Factory.all,
                           Block
                        );
                        if (  Block
                           and then
                              not Client.Written.Send_Blocked
                           )
                        then
                           Client.Written.Send_Blocked := True;
                           Set
                           (  Client.Listener.Blocked_Sockets,
                              Client.Socket
                           );
                           Clear
                           (  Client.Listener.Write_Sockets,
                              Client.Socket
                           );
                           if (  0
                              /= (  Listener.Factory.Trace_Flags
                                 and
                                    (  Trace_Encoded_Sent
                                    or Trace_Decoded_Sent
                              )  )  )
                           then
                              Trace_Sending
                              (  Listener.Factory.all,
                                 Client.all,
                                 False,
                                 ", nothing to send"
                              );
                           end if;
                        end if;
                     exception
                        when Connection_Error =>
                           Stop (Listener.all, Client.Socket);
                           Client := null;
                        when Error : Socket_Error =>
                           Send_Error (Client.all, Error);
                           Stop (Listener.all, Client.Socket);
                           Client := null;
                        when Error : others =>
                           Trace_Error
                           (  Listener.Factory.all,
                              "Send socket",
                              Error
                           );
                           Stop (Listener.all, Client.Socket);
                           Client := null;
                     end;
                     begin
                        if Client /= null and then Client.Data_Sent then
                           Data_Sent (Listener.all, Client);
                        end if;
                     exception
                        when Connection_Error =>
                           Stop (Listener.all, Client.Socket);
                        when Error : others =>
                           Trace_Error
                           (  Listener.Factory.all,
                              "Processing sent notification",
                              Error
                           );
                           Stop (Listener.all, Client.Socket);
                     end;
                  end if;
               end;
            end loop;
         end if;
         Service_Postponed (Listener.all);
      end loop;
   exception
      when Error : others =>
         Trace_Error (Listener.Factory.all, "Worker task", Error);
   end Worker;

end GNAT.Sockets.Server;
