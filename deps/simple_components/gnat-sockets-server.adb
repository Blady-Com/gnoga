--                                                                    --
--  package GNAT.Sockets.Server     Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  23:36 14 Dec 2014  --
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
      Diff : Stream_Element_Offset :=
             Client.Free_To_Read - Client.First_Read;
   begin
      if Diff < 0 then
         return Client.Read'Length - Diff;
      else
         return Diff;
      end if;
   end Available_To_Process;

   function Available_To_Send (Client : Connection)
      return Stream_Element_Count is
   begin
      return Client.Written'Length - Queued_To_Send (Client) - 1;
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
             (  Client  : in out Connection;
                Stream  : in out Root_Stream_Type'Class;
                Count   : Stream_Element_Count;
                Reserve : Stream_Element_Count;
                Last    : out Stream_Element_Offset;
                Next    : out Stream_Element_Offset;
                Done    : out Boolean
             )  is
   begin
      if Reserve >= Client.Written'Length then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Output buffer is too small for prefix and suffix ("
            &  Image (Reserve)
            &  ")"
         )  );
      end if;
      if Client.First_Written <= Client.Free_To_Write then
         --
         -- [     XXXXXXXXXXXXXXX        ]
         --       |              |
         --       First_Written  Free_To_Write
         --
         declare
            Tail : constant Stream_Element_Offset :=
               Reserve + Client.Written'First - Client.First_Written;
         begin
            if Tail > 0 then
               if Client.Free_To_Write + Tail = Client.Written'Last then
                  Done := False;
               else
                  Next := Client.Written'Last - Tail;
                  if Count < Next - Client.Free_To_Write then
                     Next := Client.Free_To_Write + Count - 1;
                  end if;
                  Read
                  (  Stream,
                     Client.Written (Client.Free_To_Write..Next),
                     Last
                  );
                  Done := Last < Next;
                  Next := Last + 1;
               end if;
            else
               Next := Client.Written'Last;
               if Count < Next - Client.Free_To_Write then
                  Next := Client.Free_To_Write + Count - 1;
               end if;
               Read
               (  Stream,
                  Client.Written (Client.Free_To_Write..Next),
                  Last
               );
               Done := Last < Next;
               if Last < Client.Written'Last then
                  Next := Last + 1;
               else
                  Next := Client.Written'First;
               end if;
            end if;
         end;
      else
         --
         -- [XXXXX               XXXXXXX]
         --       |              |
         --       Free_To_Write  First_Written
         --
         if Client.Free_To_Write + Reserve >= Client.First_Written then
            Done := False;
         else
            Next := Client.First_Written - Reserve;
            if Count < Next - Client.Free_To_Write then
               Next := Client.Free_To_Write + Count - 1;
            end if;
            Read
            (  Stream,
               Client.Written (Client.Free_To_Write..Next),
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
         Abort_Selector (Listener.Selector);
         while not Listener.Doer'Terminated loop
            delay 0.001;
         end loop;
         Free (Listener.Doer);
         Close_Selector (Listener.Selector);
      end if;
   end Finalize;

   procedure Finalize (Client : in out Connection) is
   begin
      Close (Client.Socket);
      Object.Finalize (Object.Entity (Client));
   end Finalize;

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

   function Has_Data (Client : Connection) return Boolean is
   begin
      return
      (  Client.Free_To_Read /= Client.First_Read
      and then
         (  Client.Expected = 0
         or else
            Available_To_Process (Client) >= Client.Input_Size - 1
      )  );
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

   function Is_Trace_Received_On (Factory : Connections_Factory)
      return Boolean is
   begin
      return Factory.Trace_Received;
   end Is_Trace_Received_On;

   function Is_Trace_Sent_On (Factory : Connections_Factory)
      return Boolean is
   begin
      return Factory.Trace_Sent;
   end Is_Trace_Sent_On;

   procedure Keep_On_Sending (Client : in out Connection) is
   begin
      Client.Dont_Block := True;
   end Keep_On_Sending;

   procedure Process
             (  Listener  : in out Connections_Server;
                Client    : Connection_Ptr;
                Data_Left : out Boolean
             )  is
      Last    : Stream_Element_Offset;
      Pointer : Stream_Element_Offset;
   begin
      while Has_Data (Client.all) loop
         if Client.Free_To_Read < Client.First_Read then
            --
            -- [XXXXXXXXXXXXXX              XXXXX]
            --   Free_To_Read |  First_Read |
            --
            if Client.First_Read > Client.Read'Last then
               --
               -- [XXXXXXXXXXXXXX                   ]
               --   Free_To_Read |        First_Read |
               --
               Client.First_Read := Client.Read'First; -- Wrap
               Last := Client.Free_To_Read - 1;
            else
               Last := Client.Read'Last;
            end if;
         else
            --
            -- [           XXXXXXXXX             ]
            --  First_Read |        | Free_To_Read
            --
            Last := Client.Free_To_Read - 1;
         end if;
         Pointer := Last + 1;
         Received
         (  Client.all,
            Client.Read (Client.First_Read..Last),
            Pointer
         );
         if Pointer < Client.First_Read or else Pointer > Last + 1 then
            Raise_Exception
            (  Layout_Error'Identity,
               (  "Subscript error, pointer "
               &  Image (Pointer)
               &  " out of range "
               &  Image (Client.First_Read)
               &  ".."
               &  Image (Last)
               &  "+"
            )  );
         elsif Pointer > Client.Read'Last then
            if Client.Free_To_Read <= Client.Read'Last then
               Client.First_Read := Client.Read'First; -- Wrap
            else
               Client.First_Read := Pointer; -- Not yet
            end if;
         else
            Client.First_Read := Pointer;
         end if;
         if Pointer <= Last then -- Some input left unprocessed
            Data_Left := True;
            return;
         end if;
      end loop;
      Data_Left := False;
   end Process;

   procedure Process_Packet (Client : in out Connection) is
   begin
      null;
   end Process_Packet;

   procedure Queue
             (  Client  : in out Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      Free  : Stream_Element_Offset;
      Count : Stream_Element_Offset := Data'Last - Pointer + 1;
   begin
      if Client.First_Written = Client.Free_To_Write then
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
            (  Client.Written'Length - 1,
               Count
            );
         Free := Stream_Element_Offset'Max
                 (  Client.Written'First,
                    Client.Free_To_Write - Count
                 );
         Client.Written (Free..Free + Count - 1) :=
            Data (Pointer..Pointer + Count - 1);
         Pointer := Pointer + Count;
         Client.First_Written := Free;
         Client.Free_To_Write := Free + Count;
         if Client.Send_Blocked then
            Client.Send_Blocked := False;
            Client.Listener.Unblock_Send := True;
         end if;
         return;
      elsif Client.First_Written < Client.Free_To_Write then
         --
         -- [     XXXXXXXXXXXXXXX        ]
         --       |              |
         --       First_Written  Free_To_Write
         --
         Free :=
            (  Client.Written'Length
            -  Client.Free_To_Write
            +  Client.First_Written
            -  1  -- Last element is never written
            );
         if Free <= 0 then
            return;
         end if;
         declare
            Tail : Stream_Element_Offset :=
                   Stream_Element_Offset'Min
                   (  Client.Written'Last - Client.Free_To_Write + 1,
                      Free
                   );
         begin
            if Count <= Tail then -- Can queue all Count elements
               Client.Written
               (  Client.Free_To_Write
               .. Client.Free_To_Write + Count - 1
               )  := Data (Pointer..Data'Last);
               Pointer := Data'Last + 1;
               Free := Client.Free_To_Write + Count;
               if Free > Client.Written'Last then
                  Client.Free_To_Write := Client.Written'First;
               else
                  Client.Free_To_Write := Free;
               end if;
               return;
            end if; -- Can queue only Tail elements
            Client.Written
            (  Client.Free_To_Write
            .. Client.Free_To_Write + Tail - 1
            )  := Data (Pointer..Pointer + Tail - 1);
            Pointer := Pointer + Tail;
            Count   := Count   - Tail;
            Free    := Free    - Tail;
            if Client.Free_To_Write + Tail > Client.Written'Last then
               Client.Free_To_Write := Client.Written'First;
            else
               Client.Free_To_Write := Client.Free_To_Write + Tail;
            end if;
         end;
      else
         --
         -- [XXXXX               XXXXXXXX]
         --       |              |
         --       Free_To_Write  First_Written
         --
         Free :=
            (  Client.First_Written
            +  Client.Free_To_Write
            -  1  -- Last element is never written
            );
      end if;
      if Free <= 0 then
         return;
      end if;
      Count := Stream_Element_Offset'Min (Count, Free);
      Client.Written
      (  Client.Free_To_Write
      .. Client.Free_To_Write + Count - 1
      ) := Data (Pointer..Pointer + Count - 1);
      Pointer := Pointer + Count;
      Client.Free_To_Write := Client.Free_To_Write + Count;
      if Client.Send_Blocked then
         Client.Send_Blocked := False;
         Client.Listener.Unblock_Send := True;
      end if;
   end Queue;

   function Queued_To_Send (Client : Connection)
      return Stream_Element_Count is
   begin
      if Client.Free_To_Write >= Client.First_Written then
         return Client.Free_To_Write - Client.First_Written;
      else
         return Client.Written'Length - Client.First_Written +
                Client.Free_To_Write;
      end if;
   end Queued_To_Send;

   procedure Read
             (  Client  : in out Connection;
                Factory : in out Connections_Factory'Class
             )  is
      Last : Stream_Element_Offset;
   begin
      if Client.Overlapped_Read < Queued_To_Send (Client) then
         return; -- Not ready to read yet
      elsif Client.Free_To_Read < Client.First_Read then
         --
         -- [XXXXXXXXXXXXXX              XXXXX]
         --   Free_To_Read |  First_Read |
         --
         Last := Client.First_Read - 2;
         if Last <= Client.First_Read then -- Read buffer is full
            return;
         end if;
      else
         --
         -- [           XXXXXXXXX             ]
         --  First_Read |        | Free_To_Read
         --
         if (  Client.Free_To_Read - Client.First_Read
            >= Client.Read'Length
            )
         then -- Read buffer is full
            return;
         elsif Client.Free_To_Read > Client.Read'Last then -- Wrap
            Client.Free_To_Read := Client.Read'First;
            Last := Client.First_Read - 2;
         else
            Last := Client.Read'Last;
         end if;
      end if;
      Receive_Socket
      (  Client.Socket,
         Client.Read (Client.Free_To_Read..Last),
         Last
      );
      if Factory.Trace_Received then
         Trace_Received
         (  Factory => Factory,
            Client  => Client,
            Data    => Client.Read,
            From    => Client.Free_To_Read,
            To      => Last
         );
      end if;
      if Last = Client.Free_To_Read - 1 then -- Nothing read
         raise Connection_Error;
      end if;
      Client.Expected :=
         Stream_Element_Offset'Max
         (  Client.Expected - (Last - Client.Free_To_Read + 1),
            0
         );
      Client.Free_To_Read := Last + 1;
   exception
      when Error : Socket_Error | Layout_Error =>
         Receive_Error (Client, Error);
         raise Connection_Error;
   end Read;

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
      Queue (Client, Data, Pointer);
      if (  Client.Send_Blocked
         and then
            Client.Free_To_Write /= Client.First_Written
         )
      then -- Request socket unblocking
         Client.Send_Blocked := False;
         Client.Listener.Unblock_Send := True;
      end if;
   end Send;

   procedure Send
             (  Client  : in out Connection;
                Data    : String;
                Pointer : in out Integer
             )  is
   begin
      Pointer := Data'Last + 1;
      for Index in Data'Range loop
         if Queued_To_Send (Client) + 1 >= Client.Written'Length then
            Pointer := Index;
            exit;
         end if;
         Client.Written (Client.Free_To_Write) :=
            Stream_Element (Character'Pos (Data (Index)));
         if Client.Free_To_Write = Client.Written'Last then
            Client.Free_To_Write := Client.Written'First;
         else
            Client.Free_To_Write := Client.Free_To_Write + 1;
         end if;
      end loop;
      if (  Client.Send_Blocked
         and then
            Client.Free_To_Write /= Client.First_Written
         )
      then -- Request socket unblocking
         Client.Send_Blocked := False;
         Client.Listener.Unblock_Send := True;
      end if;
   end Send;

   procedure Send
             (  Client : in out Connection;
                Stream : in out Root_Stream_Type'Class;
                End_Of_Stream : out Boolean
             )  is
      Last : Stream_Element_Offset;
      Next : Stream_Element_Offset;
   begin
      Fill_From_Stream
      (  Client  => Client,
         Stream  => Stream,
         Count   => Stream_Element_Count'Last,
         Reserve => 1,
         Last    => Last,
         Next    => Next,
         Done    => End_Of_Stream
      );
      Client.Free_To_Write := Next;
      if (  Client.Send_Blocked
         and then
            Client.Free_To_Write /= Client.First_Written
         )
      then -- Request socket unblocking
         Client.Send_Blocked := False;
         Client.Listener.Unblock_Send := True;
      end if;
   end Send;

   procedure Send
             (  Client : in out Connection;
                Stream : in out Root_Stream_Type'Class;
                Count  : in out Stream_Element_Count;
                End_Of_Stream : out Boolean
             )  is
      Last : Stream_Element_Offset;
      Next : Stream_Element_Offset;
   begin
      Fill_From_Stream
      (  Client  => Client,
         Stream  => Stream,
         Count   => Count,
         Reserve => 1,
         Last    => Last,
         Next    => Next,
         Done    => End_Of_Stream
      );
      Count := Count - (Last + 1 - Client.Free_To_Write);
      Client.Free_To_Write := Next;
      if (  Client.Send_Blocked
         and then
            Client.Free_To_Write /= Client.First_Written
         )
      then -- Request socket unblocking
         Client.Send_Blocked := False;
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
      Last : Stream_Element_Offset;
      Next : Stream_Element_Offset;
   begin
      if Client.Free_To_Write = Client.First_Written then
         if Client.Written'Length <= Reserve then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Output buffer size"
               &  Stream_Element_Count'Image (Client.Written'Length)
               &  " is less than required"
               &  Stream_Element_Count'Image (Reserve + 1)
               &  " elements"
            )  );
         end if;
         Fill_From_Stream
         (  Client  => Client,
            Stream  => Stream,
            Count   => Count,
            Reserve => Reserve + 1,
            Last    => Last,
            Next    => Next,
            Done    => End_Of_Stream
         );
         Count := Count - (Last + 1 - Client.Free_To_Write);
         declare
            Header : Stream_Element_Array :=
                     Get_Prefix.all
                     (  Client'Unchecked_Access,
                        Client.Written (Client.Free_To_Write..Last),
                        End_Of_Stream
                     );
            Tail   : Stream_Element_Array :=
                     Get_Suffix.all
                     (  Client'Unchecked_Access,
                        Client.Written (Client.Free_To_Write..Last),
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
               Last := Client.First_Written;
               for Index in reverse Header'Range loop
                  if Last = Client.Written'First then
                     Last := Client.Written'Last;
                  else
                     Last := Last - 1;
                  end if;
                  Client.Written (Last) := Header (Index);
               end loop;
            end if;
            Client.First_Written := Last;
            Client.Free_To_Write := Next;
            if Tail'Length > 0 then
               Last := Tail'First;
               Queue (Client, Tail, Last);
            end if;
         end;
      else
         End_Of_Stream := False;
      end if;
      if (  Client.Send_Blocked
         and then
            Client.Free_To_Write /= Client.First_Written
         )
      then -- Request socket unblocking
         Client.Send_Blocked := False;
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
      Last : Stream_Element_Offset;
      Next : Stream_Element_Offset;
   begin
      if Client.Free_To_Write = Client.First_Written then
         if Client.Written'Length <= Reserve then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Output buffer size"
               &  Stream_Element_Count'Image (Client.Written'Length)
               &  " is less than required"
               &  Integer'Image (Reserve + 1)
               &  " elements"
            )  );
         end if;
         Fill_From_Stream
         (  Client  => Client,
            Stream  => Stream,
            Count   => Count,
            Reserve => Stream_Element_Count (Reserve) + 1,
            Last    => Last,
            Next    => Next,
            Done    => End_Of_Stream
         );
         Count := Count - (Last + 1 - Client.Free_To_Write);
         declare
            Header : String :=
                     Get_Prefix.all
                     (  Client'Unchecked_Access,
                        Client.Written (Client.Free_To_Write..Last),
                        End_Of_Stream
                     );
            Tail   : String :=
                     Get_Suffix.all
                     (  Client'Unchecked_Access,
                        Client.Written (Client.Free_To_Write..Last),
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
               Last := Client.First_Written;
               for Index in reverse Header'Range loop
                  if Last = Client.Written'First then
                     Last := Client.Written'Last;
                  else
                     Last := Last - 1;
                  end if;
                  Client.Written (Last) :=
                     Stream_Element (Character'Pos (Header (Index)));
               end loop;
            end if;
            Client.First_Written := Last;
            Client.Free_To_Write := Next;
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
      if (  Client.Send_Blocked
         and then
            Client.Free_To_Write /= Client.First_Written
         )
      then -- Request socket unblocking
         Client.Send_Blocked := False;
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

   procedure Sent (Client : in out Connection) is
   begin
      null;
   end Sent;

   procedure Set_Expected_Count
             (  Client : in out Connection;
                Count  : Stream_Element_Count
             )  is
   begin
      Client.Expected := Count;
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
                  "Processing error",
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
      if Factory.Standard_Output then
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
      Factory.Standard_Output := False;
   end Trace_Off;

   procedure Trace_On
             (  Factory  : in out Connections_Factory;
                Received : Boolean := False;
                Sent     : Boolean := False
             )  is
   begin
      Factory.Standard_Output := True;
      Factory.Trace_Received  := Received;
      Factory.Trace_Sent      := Sent;
   end Trace_On;

   procedure Trace_On
             (  Factory  : in out Connections_Factory;
                Name     : String;
                Received : Boolean := False;
                Sent     : Boolean := False
             )  is
      use Ada.Text_IO;
   begin
      if Is_Open (Factory.Trace_File) then
         Close (Factory.Trace_File);
      end if;
      Create (File => Factory.Trace_File, Name => Name);
      Factory.Trace_Received := Received;
      Factory.Trace_Sent := Sent;
   end Trace_On;

   procedure Trace_Received
             (  Factory : in out Connections_Factory;
                Client  : Connection'Class;
                Data    : Stream_Element_Array;
                From    : Stream_Element_Offset;
                To      : Stream_Element_Offset
             )  is
   begin
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
                To      : Stream_Element_Offset
             )  is
   begin
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
   end Trace_Sent;

   procedure Write
             (  Client  : in out Connection;
                Factory : in out Connections_Factory'Class;
                Blocked : out Boolean
             )  is
      Next : Stream_Element_Count;
   begin
      Blocked := Client.First_Written = Client.Free_To_Write;
      if Blocked then
         if Client.Dont_Block then
            Blocked           := False;
            Client.Data_Sent  := True;
            Client.Dont_Block := False;
         end if;
      else
         loop
            if Client.First_Written > Client.Free_To_Write then
               --
               -- [XXXXX               XXXXXXX]
               --       |              |
               --       Free_To_Write  First_Written
               --
               Send_Socket
               (  Client.Socket,
                  Client.Written
                  (  Client.First_Written
                  .. Client.Written'Last
                  ),
                  Next
               );
               if Factory.Trace_Sent then
                  Trace_Sent
                  (  Factory => Factory,
                     Client  => Client,
                     Data    => Client.Written,
                     From    => Client.First_Written,
                     To      => Next
                  );
               end if;
               Next := Next + 1;
               if Next = Client.First_Written then
                  raise Connection_Error;
               elsif Next <= Client.Written'Last then
                  Client.First_Written := Next;
                  Client.Data_Sent := True;
                  exit;
               end if;
               Client.First_Written := 0;
               Client.Data_Sent := True;
            else
               --
               -- [     XXXXXXXXXXXXXXX        ]
               --       |              |
               --       First_Written  Free_To_Write
               --
               Send_Socket
               (  Client.Socket,
                  Client.Written
                  (  Client.First_Written
                  .. Client.Free_To_Write - 1
                  ),
                  Next
               );
               if Factory.Trace_Sent then
                  Trace_Sent
                  (  Factory => Factory,
                     Client  => Client,
                     Data    => Client.Written,
                     From    => Client.First_Written,
                     To      => Next
                  );
               end if;
               Next := Next + 1;
               if Next = Client.First_Written then
                  raise Connection_Error;
               elsif Next <= Client.Free_To_Write then
                  Client.First_Written := Next;
                  Client.Data_Sent := True;
                  exit;
               end if;
               Client.First_Written := Next;
               Client.Data_Sent := True;
            end if;
            exit when Client.First_Written = Client.Free_To_Write;
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
                        "Processing error",
                        Client.Last_Error
                     );
                  end if;
                  Stop (Listener.all, Client.Socket);
               elsif Requested_Only and then Client.Send_Blocked then
                     -- Keep it blocked
                  Set (Read_Sockets, Client.Socket);
               else -- Unblock
                  Set (Listener.Write_Sockets, Client.Socket);
                  Set (Write_Sockets, Client.Socket);
                  Status := Completed;  -- Make sure it written later on
                  Client.Send_Blocked := False;
                  Client.Data_Sent    := True;
                  if Listener.Factory.Trace_Sent then
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
         exit when Status = Aborted;
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
                           Connected (This);
                        end;
                        Listener.Clients :=  Listener.Clients + 1;
                     end if;
                  exception
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
                              "Processing error",
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
                                 "Processing error",
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
                           "Processing error",
                           Client.Last_Error
                        );
                     end if;
                     Stop (Listener.all, Client.Socket);
                  else
                     declare
                        Block : Boolean;
                     begin
                        Write (Client.all, Listener.Factory.all, Block);
                        if Block and then not Client.Send_Blocked then
                           Client.Send_Blocked := True;
                           Set
                           (  Client.Listener.Blocked_Sockets,
                              Client.Socket
                           );
                           Clear
                           (  Client.Listener.Write_Sockets,
                              Client.Socket
                           );
                           if Listener.Factory.Trace_Sent then
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
                              "Processing error",
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
