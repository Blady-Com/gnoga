--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.MQTT.Server                    Luebeck            --
--  Implementation                                 Spring, 2016       --
--                                                                    --
--                                Last revision :  13:32 28 Jan 2022  --
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
with Strings_Edit.Floats;    use Strings_Edit.Floats;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body GNAT.Sockets.MQTT.Server is
   use GNAT.Sockets.Server.Handles;
   use Messages_Sets;

   function Equal (Left, Right : Message_Object_Ptr) return Boolean is
   begin
      return
      (  Left = Right
      or else
         (  Left /= null
         and then
            Right /= null
         and then
            Left.Topic = Right.Topic
      )  );
   end Equal;

   procedure Erase (Set : in out Messages_Set) is
   begin
      for Index in 1..Get_Size (Set.Set) loop
         Release (Get (Set.Set, Index));
      end loop;
      Erase (Set.Set);
   end Erase;

   function Image_Duplicate (Duplicate : Boolean) return String is
   begin
      if Duplicate then
         return " duplicate";
      else
         return "";
      end if;
   end Image_Duplicate;

   function Image_Retain (Retain : Boolean) return String is
   begin
      if Retain then
         return " retain";
      else
         return "";
      end if;
   end Image_Retain;

   function Image_Topic
            (  Topic   : String;
               Message : Stream_Element_Array
            )  return String is
   begin
      return Topic & "=" & Image (Message);
   end Image_Topic;

   procedure Disconnected (Client : in out MQTT_Connection) is
   begin
      if Is_Valid (Client.Session) then
         declare
            Session : MQTT_Session'Class renames
                      Ptr (Client.Session).all;
         begin
            Session.Client := null;
            Session.Last   := Clock;
            Invalidate (Session.Reference);
            if (  not Session.Disconnected
               and then
                  Is_Valid (Session.Will_Message)
               )
            then -- Publish last will
               declare
                  Will : Message_Object'Class renames
                         Ptr (Session.Will_Message).all;
               begin
                  if Is_Tracing_On (Client.Server.all, Trace_Pubishing)
                  then
                     Trace
                     (  MQTT_Connection'Class (Client),
                        Session.Name,
                        "Publishing the last will of",
                        Received
                     );
                  end if;
                  if Session.Will_Retain then
                     Publish
                     (  Client.Server.all,
                        Will.Topic,
                        Will.Content (1..Will.Count),
                        Session.Will_QoS,
                        Retained
                     );
                  else
                     Publish
                     (  Client.Server.all,
                        Will.Topic,
                        Will.Content (1..Will.Count),
                        Session.Will_QoS,
                        Transient
                     );
                  end if;
               end;
            end if;
            if Session.Length = 0 then -- Anonymous session
               declare
                  Lock : Holder (Client.Server.Lock'Unchecked_Access);
               begin
                  if Is_Tracing_On (Client.Server.all, Trace_Sessions)
                  then
                     Trace
                     (  MQTT_Connection'Class (Client),
                        Session.Name,
                        "Deleting anonymous session",
                        Action
                     );
                  end if;
                  Remove (Client.Server.Anonymous, Client.Session);
               end;
            elsif not Session.Persistent then
               declare
                  Lock : Holder (Client.Server.Lock'Unchecked_Access);
               begin
                  if Is_Tracing_On (Client.Server.all, Trace_Sessions)
                  then
                     Trace
                     (  MQTT_Connection'Class (Client),
                        Session.Name,
                        "Deleting session",
                        Action
                     );
                  end if;
                  Delete (Client.Server.Named, Session.Name);
               end;
            end if;
         end;
         Invalidate (Client.Session);
      end if;
   end Disconnected;

   function Do_Insert
            (  New_Element, Old_Element : Message_Object_Ptr
            )  return Boolean is
   begin
      if Old_Element = null then
         Increment_Count (New_Element.all);
         return True;
      else
         return False;
      end if;
   end Do_Insert;

   function Do_Replace
            (  New_Element, Old_Element : Message_Object_Ptr
            )  return Boolean is
   begin
      Increment_Count (New_Element.all);
      Release (Old_Element);
      return True;
   end Do_Replace;

   function Do_Update
            (  New_Element, Old_Element : Message_Object_Ptr
            )  return Boolean is
   begin
      if Old_Element = null then
         Increment_Count (New_Element.all);
         return True;
      elsif (  New_Element.Count = Old_Element.Count
           and then
               (  New_Element.Content (1..New_Element.Count)
               =  Old_Element.Content (1..Old_Element.Count)
            )  )  then
         return False;
      else
         Increment_Count (New_Element.all);
         Release (Old_Element);
         return True;
      end if;
   end Do_Update;

   procedure Drop
             (  Server : in out MQTT_Server;
                Name   : String
             )  is
      Lock  : Holder (Server.Self.Lock'Unchecked_Access);
      Index : constant Integer := Locate (Server.Named, Name);
   begin
      if Index > 0 then
         if Ptr (GetTag (Server.Named, Index)).Client = null then
            Delete (Server.Named, Index);
         else
            Ptr (GetTag (Server.Named, Index)).Persistent := False;
         end if;
      end if;
   end Drop;

   procedure Drop
             (  Server : in out MQTT_Server;
                Index  : Positive
             )  is
      Lock : Holder (Server.Self.Lock'Unchecked_Access);
      Size : constant Natural := GetSize (Server.Named);
   begin
      if Index <= Size then
         if Ptr (GetTag (Server.Named, Index)).Client = null then
            Delete (Server.Named, Index);
         else
            Ptr (GetTag (Server.Named, Index)).Persistent := False;
         end if;
      elsif Index - Size <= Get_Size (Server.Anonymous) then
         return; -- An anonymous session will be dropped anyway
      else
         Raise_Exception
         (  End_Error'Identity,
            "No session " & Image (Index)
         );
      end if;
   end Drop;

   procedure Erase (Session : in out MQTT_Session) is
      Item  : Messages_Queue_Item;
      Empty : Boolean;
   begin
      Erase (Session.Subscribtions.Singular);
      Erase (Session.Subscribtions.Patterns);
      loop
         Get (Session.Output, Item, Empty);
         exit when Empty;
         Release (Item.Message);
      end loop;
      for Index in 1..Get_Size (Session.Input) loop
         Release (Get (Session.Input, Index).Message);
      end loop;
      Erase (Session.Input);
   end Erase;

   procedure Finalize (Session : in out MQTT_Session) is
   begin
      Erase (Session);
      Object.Finalize (Object.Entity (Session));
   end Finalize;

   procedure Finalize (Server : in out MQTT_Server) is
   begin
      Erase (Server.Retained);
   end Finalize;

   function Get_Name (Client : MQTT_Connection) return String is
   begin
      return Ptr (Client.Session).Name;
   end Get_Name;

   function Get_Message
            (  Server : MQTT_Server;
               Index  : Positive
            )  return MQTT_Message is
      Result : MQTT_Message;
      Lock   : Holder (Server.Self.Lock'Unchecked_Access);
   begin
      if Index > Get_Size (Server.Retained.Set) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No message with the number " & Image (Index)
         );
      end if;
      Set (Result.Reference, Get (Server.Retained.Set, Index));
      return Result;
   end Get_Message;

   function Get_Message
            (  Server : MQTT_Server;
               Topic  : String
            )  return Integer is
   begin
      if Check_Topic (Topic) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The message topic contains wildcards"
         );
      end if;
      declare
         Message : aliased Message_Object (Topic'Length, 0);
      begin
         Message.Topic := Topic;
         declare
            Lock : Holder (Server.Self.Lock'Unchecked_Access);
         begin
            return
               Integer'Max
               (  Find (Server.Retained.Set, Message'Unchecked_Access),
                  0
               );
         end;
      end;
   end Get_Message;

   function Get_Message
            (  Server : MQTT_Server;
               Topic  : String
            )  return MQTT_Message is
   begin
      if Check_Topic (Topic) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The message topic contains wildcards"
         );
      end if;
      declare
         Result  : MQTT_Message;
         Message : aliased Message_Object (Topic'Length, 0);
      begin
         Message.Topic := Topic;
         declare
            Index : Integer;
            Lock  : Holder (Server.Self.Lock'Unchecked_Access);
         begin
            Index :=
               Find (Server.Retained.Set, Message'Unchecked_Access);
            if Index <= 0 then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "There is no retained message with this topic"
               );
            end if;
            Set (Result.Reference, Get (Server.Retained.Set, Index));
         end;
         return Result;
      end;
   end Get_Message;

   function Get_Messages_Number
            (  Server : MQTT_Server
            )  return Natural is
      Lock : Holder (Server.Self.Lock'Unchecked_Access);
   begin
      return Get_Size (Server.Retained.Set);
   end Get_Messages_Number;

   function Get_Queue_Size
            (  Server : MQTT_Server
            )  return Positive is
   begin
      return Server.Queue_Size;
   end Get_Queue_Size;

   function Get_Session
            (  Server : MQTT_Server;
               Name   : String
            )  return Integer is
      Lock : Holder (Server.Self.Lock'Unchecked_Access);
   begin
      return Integer'Max (Locate (Server.Named, Name), 0);
   end Get_Session;

   function Get_Session_Name
            (  Server : MQTT_Server;
               Index  : Positive
            )  return String is
      Lock : Holder (Server.Self.Lock'Unchecked_Access);
      Size : constant Natural := GetSize (Server.Named);
   begin
      if Index <= Size then
         return GetName (Server.Named, Index);
      elsif Index - Size <= Get_Size (Server.Anonymous) then
         return ""; -- An anonymous session has no name
      else
         Raise_Exception
         (  End_Error'Identity,
            "No session " & Image (Index)
         );
      end if;
   end Get_Session_Name;

   function Get_Session_Time
            (  Server : MQTT_Server;
               Name   : String
            )  return Time is
      Lock  : Holder (Server.Self.Lock'Unchecked_Access);
      Index : constant Integer := Locate (Server.Named, Name);
   begin
      if Index > 0 then
         return Ptr (GetTag (Server.Named, Index)).Last;
      else
         Raise_Exception
         (  End_Error'Identity,
            "No session " & Name
         );
      end if;
   end Get_Session_Time;

   function Get_Session_Time
            (  Server : MQTT_Server;
               Index  : Positive
            )  return Time is
      Lock : Holder (Server.Self.Lock'Unchecked_Access);
      Size : constant Natural := GetSize (Server.Named);
   begin
      if Index <= Size then
         return Ptr (GetTag (Server.Named, Index)).Last;
      elsif Index - Size <= Get_Size (Server.Anonymous) then
         return Get (Server.Anonymous, Index - Size + 1).Last;
      else
         Raise_Exception
         (  End_Error'Identity,
            "No session " & Image (Index)
         );
      end if;
   end Get_Session_Time;

   function Get_Sessions_Number
            (  Server : MQTT_Server
            )  return Natural is
      Lock : Holder (Server.Self.Lock'Unchecked_Access);
   begin
      return GetSize (Server.Named) + Get_Size (Server.Anonymous);
   end Get_Sessions_Number;

   function Get_Tracing_Flags
            (  Server : MQTT_Server
            )  return MQTT_Trace_Flags is
   begin
      return Server.Flags;
   end Get_Tracing_Flags;

   function Is_Session_Active
            (  Server : MQTT_Server;
               Name   : String
            )  return Boolean is
      Lock  : Holder (Server.Self.Lock'Unchecked_Access);
      Index : constant Integer := Locate (Server.Named, Name);
   begin
      if Index > 0 then
         return Ptr (GetTag (Server.Named, Index)).Client /= null;
      else
         Raise_Exception
         (  End_Error'Identity,
            "No session " & Name
         );
      end if;
   end Is_Session_Active;

   function Is_Session_Active
            (  Server : MQTT_Server;
               Index  : Positive
            )  return Boolean is
      Lock : Holder (Server.Self.Lock'Unchecked_Access);
      Size : constant Natural := GetSize (Server.Named);
   begin
      if Index <= Size then
         return Ptr (GetTag (Server.Named, Index)).Client /= null;
      elsif Index - Size <= Get_Size (Server.Anonymous) then
         return True; -- Anonymous sessions are always active
      else
         Raise_Exception
         (  End_Error'Identity,
            "No session " & Image (Index)
         );
      end if;
   end Is_Session_Active;

   function Is_Tracing_On
            (  Server : MQTT_Server;
               Flags  : MQTT_Trace_Flags
            )  return Boolean is
   begin
      return 0 /= (Server.Flags and Flags);
   end Is_Tracing_On;

   function Less (Left, Right : Message_Object_Ptr) return Boolean is
   begin
      return
      (  Right /= null
      and then
         (  Left = null
         or else
            Left.Topic < Right.Topic
      )  );
   end Less;

   procedure On_Acknowledge
             (  Client  : in out MQTT_Connection;
                Request : Acknowledge_Type;
                Packet  : Packet_Identifier
             )  is
      Lock    : Holder (Client.Server.Lock'Unchecked_Access);
      Session : MQTT_Session'Class renames Ptr (Client.Session).all;
      Element : Message_Response_Item;
      Index   : Integer;
   begin
      if Session.Timeout > 0.0 then
         Session.Last := Clock;
      end if;
      case Request is
         when Publish_Level_1 =>
            Index := Find (Session.Input, Packet);
            if Index > 0 then
               Element := Get (Session.Input, Index);
               Remove (Session.Input, Index);
               Release (Element.Message);
               if (  0 /= (Incoming and Element.Flags)
                  or else
                     0 = (At_Least_Once and Element.Flags)
                  )
               then
                  if Is_Tracing_On
                     (  Client.Server.all,
                        Trace_Acknowledgement
                     )
                  then
                     Trace
                     (  MQTT_Connection'Class (Client),
                        Session.Name,
                        (  "Unsolicited PUBACK for packet "
                        &  Image (Integer (Packet))
                        ),
                        Received
                     );
                  end if;
               else
                  if Is_Tracing_On
                     (  Client.Server.all,
                        Trace_Acknowledgement
                     )
                  then
                     Trace
                     (  MQTT_Connection'Class (Client),
                        Session.Name,
                        "PUBACK for packet " & Image (Integer (Packet)),
                        Received
                     );
                  end if;
               end if;
            else
               if Is_Tracing_On
                  (  Client.Server.all,
                     Trace_Acknowledgement
                  )
               then
                  Trace
                  (  MQTT_Connection'Class (Client),
                     Session.Name,
                     (  "PUBACK for unknown packet "
                     &  Image (Integer (Packet))
                     ),
                     Received
                  );
               end if;
            end if;
         when Publish_Level_2_Received =>
            Index := Find (Session.Input, Packet);
            if Index > 0 then
               Element := Get (Session.Input, Index);
               Remove (Session.Input, Index);
               Release (Element.Message);
               if (  0 /= (Incoming and Element.Flags)
                  or else
                     0 = (Exactly_Once and Element.Flags)
                  )
               then
                  if Is_Tracing_On
                     (  Client.Server.all,
                        Trace_Acknowledgement
                     )
                  then
                     Trace
                     (  MQTT_Connection'Class (Client),
                        Session.Name,
                        (  "Unsolicited PUBREC for packet "
                        &  Image (Integer (Packet))
                        ),
                        Received
                     );
                  end if;
               else
                  if Is_Tracing_On
                     (  Client.Server.all,
                        Trace_Acknowledgement
                     )
                  then
                     Trace
                     (  MQTT_Connection'Class (Client),
                        Session.Name,
                        "PUBREC for packet " & Image (Integer (Packet)),
                        Received
                     );
                  end if;
                  Send_Acknowledge
                  (  Client,
                     Publish_Level_2_Release,
                     Packet
                  );
               end if;
            else
               if Is_Tracing_On
                  (  Client.Server.all,
                     Trace_Acknowledgement
                  )
               then
                  Trace
                  (  MQTT_Connection'Class (Client),
                     Session.Name,
                     (  "PUBREC for unknown packet "
                     &  Image (Integer (Packet))
                     ),
                     Received
                  );
               end if;
            end if;
         when Publish_Level_2_Release =>
            Index := Find (Session.Input, Packet);
            if Index > 0 then
               Element := Get (Session.Input, Index);
               Remove (Session.Input, Index);
               Release (Element.Message);
               if (  0 = (Incoming and Element.Flags)
                  or else
                     0 = (Exactly_Once and Element.Flags)
                  )
               then
                  if Is_Tracing_On
                     (  Client.Server.all,
                        Trace_Acknowledgement
                     )
                  then
                     Trace
                     (  MQTT_Connection'Class (Client),
                        Session.Name,
                        (  "Unsolicited PUBREL for packet "
                        &  Image (Integer (Packet))
                        ),
                        Received
                     );
                  end if;
               else
                  if Is_Tracing_On
                     (  Client.Server.all,
                        Trace_Acknowledgement
                     )
                  then
                     Trace
                     (  MQTT_Connection'Class (Client),
                        Session.Name,
                        (  "PUBREL for packet "
                        &  Image (Integer (Packet))
                        ),
                        Received
                     );
                  end if;
                  Send_Acknowledge
                  (  Client,
                     Publish_Level_2_Complete,
                     Packet
                  );
               end if;
            else
               if Is_Tracing_On
                  (  Client.Server.all,
                     Trace_Acknowledgement
                  )
               then
                  Trace
                  (  MQTT_Connection'Class (Client),
                     Session.Name,
                     (  "PUBREL for unknown packet "
                     &  Image (Integer (Packet))
                     ),
                     Received
                  );
               end if;
            end if;
         when Unsubscribed | Publish_Level_2_Complete =>
            null;
      end case;
   end On_Acknowledge;

   procedure On_Connect
             (  Client       : in out MQTT_Connection;
                Name         : String;
                Clean        : Boolean;
                Will_Topic   : String;
                Will_Message : Stream_Element_Array;
                Will_QoS     : QoS_Level;
                Will_Retain  : Boolean;
                User_Name    : String;
                Password     : String;
                Keep_Alive   : Duration
             )  is
      function Get_Name return String is
      begin
         if Name'Length = 0 then
            return "<anonymous>";
         else
            return Name;
         end if;
      end Get_Name;

      function Image return String is

         function Get_Keep_Alive return String is
         begin
            if Keep_Alive > 0.0 then
               return "";
            else
               return
               (  ", Keep-alive "
               &  Image (Value => Float (Keep_Alive), AbsSmall => 0)
               &  "s"
               );
            end if;
         end Get_Keep_Alive;

         function Get_Password return String is
         begin
            if Password'Length = 0 then
               return "";
            else
               return ", Password " & Password;
            end if;
         end Get_Password;

         function Get_User return String is
         begin
            if User_Name'Length = 0 then
               return "";
            else
               return ", User " & User_Name;
            end if;
         end Get_User;

         function Get_Will return String is
         begin
            if Will_Topic'Length = 0 then
               return "";
            else
               return
               (  ", Last will "
               &  Image_Topic (Will_Topic, Will_Message)
               &  '['
               &  Image (Will_QoS)
               &  ']'
               &  Image_Retain (Will_Retain)
               );
            end if;
         end Get_Will;
      begin
         return
         (  "Connect"
         &  Get_User
         &  Get_Password
         &  Get_Will
         &  Get_Keep_Alive
         );
      end Image;

      Server   : MQTT_Server'Class renames Client.Server.all;
      Reject   : Boolean := False;
      Response : Connect_Response;
   begin
      declare
         Lock  : Holder (Server.Lock'Unchecked_Access);
         Index : Integer;
      begin
         if Name'Length = 0 then -- Use random name
            Set
            (  Client.Session,
               new MQTT_Session (0, Server.Queue_Size)
            );
            Add (Server.Anonymous, Client.Session);
         else
            Index := Locate (Server.Named, Name);
            if Index <= 0 then -- Unknown client is here
               Set
               (  Client.Session,
                  new MQTT_Session (Name'Length, Server.Queue_Size)
               );
               Ptr (Client.Session).Name := Name;
               Add (Server.Named, Name, Client.Session);
            else -- Known client
               Client.Session := GetTag (Server.Named, Index);
               if Ptr (Client.Session).Client /= null then -- Another
                  Invalidate (Client.Session);             -- client
                  Reject   := True;
                  Response := Server_Unavailable;
               end if;
            end if;
         end if;
      end;
      if Reject then
         if Is_Tracing_On (Server, Trace_Sessions) then
            Trace
            (  MQTT_Connection'Class (Client),
               Get_Name,
               Image & " [Rejected: " & Image (Response) & "]",
               Received
            );
         end if;
         Send_Connect_Rejected (Client, Response);
         Shutdown (Client);
         return;
      end if;
      declare
         Session : MQTT_Session'Class renames Ptr (Client.Session).all;
         Last    : Integer := 0;
      begin
         Session.Disconnected := False;
         Session.Client := Client'Unchecked_Access;
         if Clean then
            Erase (Session);
         else
            Session.Persistent := True;
            for Index in 1..Get_Size (Session.Input) loop
               declare -- Awaited messages
                  Full    : Boolean;
                  Element : Message_Response_Item :=
                            Get (Session.Input, Index);
               begin
                  if (  0 = (Incoming and Element.Flags)
                     and then
                        (  0
                        /= (  Element.Flags
                           and
                             (Exactly_Once or At_Least_Once)
                     )  )  )
                  then -- Resend the message
                     Element.Flags := Element.Flags or Duplicate;
                     Put
                     (  Session.Output,
                        (  Flags   => Element.Flags,
                           Packet  => Get_Key (Session.Input, Index),
                           Message => Element.Message
                        ),
                        Full
                     );
                     exit when Full;
                     Last := Index;
--                       if Full then
--                          Response := Server_Unavailable;
--                          if Is_Tracing_On (Server, Trace_Sessions) then
--                             Trace
--                             (  MQTT_Connection'Class (Client),
--                                Get_Name,
--                                (  Image
--                                &  " [Rejected: "
--                                &  Image (Response)
--                                &  "] Cannot queue pending messages, "
--                                &  Strings_Edit.Integers.Image
--                                   (  Session.Queue_Size
--                                   )
--                                &  " elements queue is too short"
--                                ),
--                                Received
--                             );
--                          end if;
--                          Send_Connect_Rejected (Client, Response);
--                          Shutdown (Client);
--                          return;
--                       end if;
                  end if;
               end;
            end loop;
            if Last = Get_Size (Session.Input) then
               for Index in 1..Last loop
                  Release (Get (Session.Input, Index).Message);
               end loop;
               Erase (Session.Input);
            else
               for Index in 1..Last loop
                  Release (Get (Session.Input, Positive'(1)).Message);
                  Remove (Session.Input, Positive'(1));
               end loop;
               Last := 1;
               while Last <= Get_Size (Session.Input) loop
                  declare -- Processing remaining messages
                     Element : Message_Response_Item :=
                               Get (Session.Input, Last);
                  begin
                     if (  0 = (Incoming and Element.Flags)
                        and then
                           (  0
                           /= (  Element.Flags
                              and
                                 (Exactly_Once or At_Least_Once)
                        )  )  )  then
                        if 0 = (Element.Flags or Duplicate) then
                           Element.Flags := Element.Flags or Duplicate;
                           Replace (Session.Input, Last, Element);
                        end if;
                        Last := Last + 1;
                     else
                        Remove (Session.Input, Last);
                     end if;
                  end;
               end loop;
            end if;
         end if;
         Session.Timeout := Keep_Alive * 1.5;
         Session.Last    := Clock;
         Session.Used    := Session.Used + 1;
         if Is_Tracing_On (Server, Trace_Sessions) then
            if Session.Persistent then
               Trace
               (  MQTT_Connection'Class (Client),
                  Get_Name,
                  Image & " [Accepted persistent session]",
                  Received
               );
            else
               Trace
               (  MQTT_Connection'Class (Client),
                  Get_Name,
                  Image & " [Accepted new session]",
                  Received
               );
            end if;
         end if;
         Send_Connect_Accepted (Client);
      end;
   end On_Connect;

   procedure On_Disconnect (Client : in out MQTT_Connection) is
   begin
      if Is_Valid (Client.Session) then
         if Is_Tracing_On (Client.Server.all, Trace_Sessions) then
            Trace
            (  MQTT_Connection'Class (Client),
               Ptr (Client.Session).Name,
               "Disconnect",
               Received
            );
         end if;
         Ptr (Client.Session).Disconnected := True;
      end if;
   end On_Disconnect;

   procedure On_Ping (Client : in out MQTT_Connection) is
   begin
      if Is_Valid (Client.Session) then
         declare
            Lock    : Holder (Client.Server.Lock'Unchecked_Access);
            Session : MQTT_Session'Class renames
                      Ptr (Client.Session).all;
         begin
            if Is_Tracing_On (Client.Server.all, Trace_Ping) then
               Trace
               (  MQTT_Connection'Class (Client),
                  Session.Name,
                  "Ping",
                  Received
               );
            end if;
            if Session.Timeout > 0.0 then
               Session.Last := Clock;
            end if;
            if Is_Tracing_On (Client.Server.all, Trace_Ping) then
               Trace
               (  MQTT_Connection'Class (Client),
                  Session.Name,
                  "Ping response",
                  Sent
               );
            end if;
            Send_Ping_Response (Client);
         end;
      end if;
   end On_Ping;

   procedure On_Ping_Response (Client : in out MQTT_Connection) is
   begin
      if Is_Valid (Client.Session) then
         declare
            Lock    : Holder (Client.Server.Lock'Unchecked_Access);
            Session : MQTT_Session'Class renames
                      Ptr (Client.Session).all;
         begin
            if Is_Tracing_On (Client.Server.all, Trace_Ping) then
               Trace
               (  MQTT_Connection'Class (Client),
                  Session.Name,
                  "Ping response",
                  Received
               );
            end if;
            if Session.Timeout > 0.0 then
               Session.Last := Clock;
            end if;
         end;
      end if;
   end On_Ping_Response;

   procedure On_Publish
             (  Client    : in out MQTT_Connection;
                Topic     : String;
                Message   : Stream_Element_Array;
                Packet    : Packet_Identification;
                Duplicate : Boolean;
                Retain    : Boolean
             )  is
   begin
      begin
         if Check_Topic (Topic) then
            Raise_Exception
            (  Data_Error'Identity,
               "Published topic contains wildcards"
            );
         end if;
      exception
         when Error : Constraint_Error =>
            Raise_Exception
            (  Data_Error'Identity,
               (  "Published topic is invalid: "
               &  Exception_Message (Error)
            )  );
      end;
      if Is_Valid (Client.Session) then
         declare
            Lock    : Holder (Client.Server.Lock'Unchecked_Access);
            Policy  : Message_Type;
            Session : MQTT_Session'Class renames
                      Ptr (Client.Session).all;
         begin
            if Is_Tracing_On (Client.Server.all, Trace_Pubishing) then
               if Packet.QoS = At_Most_Once then
                  Trace
                  (  MQTT_Connection'Class (Client),
                     Session.Name,
                     (  "Publish "
                     &  Image_Topic (Topic, Message)
                     &  " ["
                     &  Image (Packet.QoS)
                     &  "] "
                     &  Image_Retain (Retain)
                     &  Image_Duplicate (Duplicate)
                     ),
                     Received
                  );
               else
                  Trace
                  (  MQTT_Connection'Class (Client),
                     Session.Name,
                     (  "Publish "
                     &  Image_Topic (Topic, Message)
                     &  " ["
                     &  Image (Packet.QoS)
                     &  "] "
                     &  Image_Retain (Retain)
                     &  Image_Duplicate (Duplicate)
                     &  " packet "
                     &  Image (Integer (Packet.ID))
                     ),
                     Received
                  );
               end if;
            end if;
            if Session.Timeout > 0.0 then
               Session.Last := Clock;
            end if;
            case Packet.QoS is
               when MQTT.At_Most_Once =>
                  null;
               when MQTT.At_Least_Once =>
                  Send_Acknowledge (Client, Publish_Level_1, Packet.ID);
               when MQTT.Exactly_Once =>
                  Add
                  (  Session.Input,
                     Packet.ID,
                     (  Flags   => Exactly_Once or Incoming,
                        Message => null
                  )  );
                  Send_Acknowledge
                  (  Client,
                     Publish_Level_2_Received,
                     Packet.ID
                  );
            end case;
            if Retain then
               Policy := Retained;
            else
               Policy := Transient;
            end if;
            Received
            (  Server  => Client.Server.all,
               Client  => Client,
               Topic   => Topic,
               Message => Message,
               QoS     => Packet.QoS,
               Policy  => Policy
            );
            Publish_Unchecked
            (  Server  => Client.Server.all,
               Topic   => Topic,
               Message => Message,
               QoS     => Packet.QoS,
               Policy  => Policy
            );
         end;
      end if;
   end On_Publish;

   procedure On_Subscribe
             (  Client        : in out MQTT_Connection;
                Packet        : Packet_Identifier;
                Topics_Number : Positive
             )  is
      Codes : Return_Code_List (1..Topics_Number);
      Lock  : Holder (Client.Server.Lock'Unchecked_Access);
   begin
      if Is_Valid (Client.Session) then
         declare
            Session : MQTT_Session'Class renames
                      Ptr (Client.Session).all;
         begin
            if Is_Tracing_On (Client.Server.all, Trace_Subscriptions)
            then
               Trace
               (  MQTT_Connection'Class (Client),
                  Session.Name,
                  (  "Subscribing to "
                  &  Image (Integer (Topics_Number))
                  &  " topic(s) packet "
                  &  Image (Integer (Packet))
                  ),
                  Received
               );
            end if;
            if Session.Timeout > 0.0 then
               Session.Last := Clock;
            end if;
            for Index in 1..Topics_Number loop
               declare
                  Wildcards : Boolean;
                  Topic : constant String := Get_Topic (Client, Index);
               begin
                  Codes (Index) := (True, Get_QoS (Client, Index));
                  begin
                     Wildcards := Check_Topic (Topic);
                  exception
                     when Error : Constraint_Error =>
                        Raise_Exception
                        (  Data_Error'Identity,
                           (  "Invalid topic to subscribe: "
                           &  Exception_Message (Error)
                        )  );
                  end;
                  if Codes (Index).Success then
                     if Wildcards then
                        Replace
                        (  Session.Subscribtions.Patterns,
                           Topic,
                           Codes (Index).QoS
                        );
                     else
                        Replace
                        (  Session.Subscribtions.Singular,
                           Topic,
                           Codes (Index).QoS
                        );
                     end if;
                     if Is_Tracing_On
                        (  Client.Server.all,
                           Trace_Subscriptions
                        )
                     then
                        Trace
                        (  MQTT_Connection'Class (Client),
                           Session.Name,
                           (  "Subscribed to "
                           &  Topic
                           &  " ["
                           &  Image (Codes (Index).QoS)
                           &  ']'
                           ),
                           Action
                        );
                     end if;
                  end if;
               end;
            end loop;
            if Is_Tracing_On
               (  Client.Server.all,
                  Trace_Acknowledgement
               )
            then
               Trace
               (  MQTT_Connection'Class (Client),
                  Session.Name,
                  (  "SUBACK for packet "
                  &  Image (Integer (Packet))
                  ),
                  Sent
               );
            end if;
            Send_Subscribe_Acknowledgement (Client, Packet, Codes);
            for Index in 1..Get_Size (Client.Server.Retained.Set) loop
               declare
                  This : Messages_Handles.Handle :=
                         Ref (Get (Client.Server.Retained.Set, Index));
                  Object : Message_Object'Class renames Ptr (This).all;
               begin
                  Push
                  (  Session   => Session,
                     Topic     => Object.Topic,
                     Message   => Object.Content (1..Object.Count),
                     Retain    => True,
                     Duplicate => False,
                     Handle    => This
                  );
               end;
            end loop;
         end;
      end if;
   end On_Subscribe;

   procedure On_Unsubscribe
             (  Client        : in out MQTT_Connection;
                Packet        : Packet_Identifier;
                Topics_Number : Positive
             )  is
      Lock : Holder (Client.Server.Lock'Unchecked_Access);
   begin
      if Is_Valid (Client.Session) then
         declare
            Session : MQTT_Session'Class renames
                      Ptr (Client.Session).all;
         begin
            if Is_Tracing_On (Client.Server.all, Trace_Subscriptions)
            then
               Trace
               (  MQTT_Connection'Class (Client),
                  Session.Name,
                  (  "Unsubscribing from "
                  &  Image (Integer (Topics_Number))
                  &  " topics packet "
                  &  Image (Integer (Packet))
                  ),
                  Received
               );
            end if;
            if Session.Timeout > 0.0 then
               Session.Last := Clock;
            end if;
            for Index in 1..Topics_Number loop
               declare
                  Topic : constant String := Get_Topic (Client, Index);
               begin
                  begin
                     if Check_Topic (Topic) then
                        Delete (Session.Subscribtions.Patterns, Topic);
                     else
                        Delete (Session.Subscribtions.Singular, Topic);
                     end if;
                  exception
                     when Error : Constraint_Error =>
                        Raise_Exception
                        (  Data_Error'Identity,
                           (  "Invalid topic to unsubscribe from: "
                           &  Exception_Message (Error)
                        )  );
                  end;
                  if Is_Tracing_On
                     (  Client.Server.all,
                        Trace_Subscriptions
                     )
                  then
                     Trace
                     (  MQTT_Connection'Class (Client),
                        Session.Name,
                        " Unsubscribed from " & Topic,
                        Action
                     );
                  end if;
               end;
            end loop;
         end;
         Send_Acknowledge (Client, Unsubscribed, Packet);
      end if;
   end On_Unsubscribe;

   procedure Publish
             (  Server  : in out MQTT_Server;
                Topic   : String;
                Message : Stream_Element_Array;
                QoS     : QoS_Level    := At_Most_Once;
                Policy  : Message_Type := Transient
             )  is
   begin
      begin
         if Check_Topic (Topic) then
            Raise_Exception
            (  Name_Error'Identity,
               "Published topic contains wildcards"
            );
         end if;
      exception
         when Error : Constraint_Error =>
            Raise_Exception
            (  Name_Error'Identity,
               Exception_Message (Error)
            );
      end;
      declare
         Lock : Holder (Server.Lock'Unchecked_Access);
      begin
         Publish_Unchecked (Server, Topic, Message, QoS, Policy);
      end;
   end Publish;

   procedure Publish
             (  Server  : in out MQTT_Server;
                Topic   : String;
                Message : String;
                QoS     : QoS_Level    := At_Most_Once;
                Policy  : Message_Type := Transient
             )  is
   begin
      Publish (Server, Topic, From_String (Message), QoS, Policy);
   end Publish;

   procedure Publish
             (  Server  : in out MQTT_Server;
                Message : MQTT_Message;
                QoS     : QoS_Level    := At_Most_Once;
                Policy  : Message_Type := Transient
             )  is
      Handle : Messages_Handles.Handle := Message.Reference;
      Lock   : Holder (Server.Lock'Unchecked_Access);
   begin
      Publish_Unchecked (Server, Handle, QoS, Policy);
   end Publish;

   procedure Publish
             (  Server   : in out MQTT_Server;
                Messages : MQTT_Messages_Array;
                QoS      : QoS_Level    := At_Most_Once;
                Policy   : Message_Type := Transient
             )  is
      Lock : Holder (Server.Lock'Unchecked_Access);
   begin
      Publish_Unchecked (Server, Messages, QoS, Policy);
   end Publish;

   procedure Publish_Unchecked
             (  Server : in out MQTT_Server;
                Handle : in out Messages_Handles.Handle;
                QoS    : QoS_Level;
                Policy : Message_Type
             )  is
      This : constant Message_Object_Ptr := Ptr (Handle);
   begin
      if This = null then
         return;
      end if;
      declare
         Object  : Message_Object'Class renames This.all;
         Changed : Boolean;
      begin
         case Policy is
            when Transient =>
               Changed := True;
            when Retained =>
               Replace
               (  Server.Retained.Set,
                  This,
                  Do_Replace'Access,
                  Changed
               );
            when Updated =>
               Replace
               (  Server.Retained.Set,
                  This,
                  Do_Update'Access,
                  Changed
               );
            when Initial =>
               Replace
               (  Server.Retained.Set,
                  This,
                  Do_Insert'Access,
                  Changed
               );
            when Ignored =>
               return;
         end case;
         if not Changed then
            return;
         end if;
         for Index in 1..GetSize (Server.Named) loop
            Push
            (  Topic     => Object.Topic,
               Message   => Object.Content (1..Object.Count),
               Retain    => False, -- Not propagating Retain
               Duplicate => False,
               Handle    => Handle,
               Session   => Ptr (GetTag (Server.Named, Index)).all
            );
         end loop;
         for Index in 1..Get_Size (Server.Anonymous) loop
            Push
            (  Topic     => Object.Topic,
               Message   => Object.Content (1..Object.Count),
               Retain    => False, -- Not propagating Retain
               Duplicate => False,
               Handle    => Handle,
               Session   => Get (Server.Anonymous, Index).all
            );
         end loop;
      end;
   end Publish_Unchecked;

   procedure Publish_Unchecked
             (  Server  : in out MQTT_Server;
                Topic   : String;
                Message : Stream_Element_Array;
                QoS     : QoS_Level;
                Policy  : Message_Type
             )  is
      This   : Message_Object_Ptr;
      Handle : Messages_Handles.Handle;
   begin
      if Policy = Ignored then
         return;
      end if;
      This := new Message_Object
                  (  Length => Topic'Length,
                     Size   => Message'Length
                  );
      Set (Handle, This);
      This.Topic   := Topic;
      This.Content := Message;
      This.Count   := Message'Length;
      Publish_Unchecked (Server, Handle, QoS, Policy);
   end Publish_Unchecked;

   procedure Publish_Unchecked
             (  Server   : in out MQTT_Server;
                Messages : MQTT_Messages_Array;
                QoS      : QoS_Level;
                Policy   : Message_Type
             )  is
      Handle : Messages_Handles.Handle;
   begin
      if Policy = Ignored then
         return;
      end if;
      for Index in Messages'Range loop
         Handle := Messages (Index).Reference;
         Publish_Unchecked (Server, Handle, QoS, Policy);
      end loop;
   end Publish_Unchecked;

   procedure Pump_Unchecked (Session : in out MQTT_Session) is
      Element : Messages_Queue_Item;
      Empty   : Boolean;
   begin
      if Session.Client = null or else Session.Disconnected then
         return;
      end if;
      if Session.Timeout > 0.0 then
         if Clock > Session.Last + Session.Timeout then
            Shutdown (Session.Client.all);
            return;
         end if;
      end if;
      while Available_To_Send (Session.Client.all) > 0 loop
         Get (Session.Output, Element, Empty);
         exit when Empty; -- Have nothing to send
         declare
            This : Message_Object'Class renames Element.Message.all;
         begin
            case Element.Flags and QoS_Mask  is
               when At_Least_Once => -- At least once
                  if Is_Tracing_On
                     (  Session.Client.Server.all,
                        Trace_Pubishing
                     )
                  then
                     Trace
                     (  Session.Client.all,
                        Session.Name,
                        (  "Pushing "
                        &  Image_Topic
                           (  This.Topic,
                              This.Content (1..This.Count)
                            )
                        &  " packet "
                        &  Image (Integer (Element.Packet))
                        ),
                        Action
                     );
                  end if;
                  Send_Publish
                  (  Peer      => Session.Client.all,
                     Topic     => This.Topic,
                     Message   => This.Content (1..This.Count),
                     Retain    => 0 /= (Element.Flags and Retain),
                     Duplicate => 0 /= (Element.Flags and Duplicate),
                     Packet    => (MQTT.At_Least_Once, Element.Packet)
                  );
                  Add
                  (  Session.Input,
                     Element.Packet,
                     (Element.Flags, Element.Message)
                  );
               when Exactly_Once => -- Exactly once
                  if Is_Tracing_On
                     (  Session.Client.Server.all,
                        Trace_Pubishing
                     )
                  then
                     Trace
                     (  Session.Client.all,
                        Session.Name,
                        (  "Pushing "
                        &  Image_Topic
                           (  This.Topic,
                              This.Content (1..This.Count)
                           )
                        &  " packet "
                        &  Image (Integer (Element.Packet))
                        ),
                        Action
                     );
                  end if;
                  Send_Publish
                  (  Peer      => Session.Client.all,
                     Topic     => This.Topic,
                     Message   => This.Content (1..This.Count),
                     Retain    => 0 /= (Element.Flags and Retain),
                     Duplicate => 0 /= (Element.Flags and Duplicate),
                     Packet    => (MQTT.Exactly_Once, Element.Packet)
                  );
                  Add
                  (  Session.Input,
                     Element.Packet,
                     (Element.Flags, Element.Message)
                  );
               when others => -- At most once
                  if Is_Tracing_On
                     (  Session.Client.Server.all,
                        Trace_Pubishing
                     )
                  then
                     Trace
                     (  Session.Client.all,
                        Session.Name,
                        (  "Pushing "
                        &  Image_Topic
                           (  This.Topic,
                              This.Content (1..This.Count)
                        )  ),
                        Action
                     );
                  end if;
                  Send_Publish
                  (  Peer      => Session.Client.all,
                     Topic     => This.Topic,
                     Message   => This.Content (1..This.Count),
                     Retain    => 0 /= (Element.Flags and Retain),
                     Duplicate => False,
                     Packet    => (QoS => MQTT.At_Most_Once)
                  );
                  Release (Element.Message);
                  Element.Message := null;
            end case;
         exception
            when others =>
               if Element.Message /= null then
                  Release (Element.Message);
               end if;
               raise;
         end;
      end loop;
   end Pump_Unchecked;

   procedure Push
             (  Session   : in out MQTT_Session;
                Topic     : String;
                Message   : Stream_Element_Array;
                Retain    : Boolean;
                Duplicate : Boolean;
                Handle    : in out Messages_Handles.Handle
             )  is
      QoS   : QoS_Level     := At_Most_Once;
      Flags : Message_Flags := 0;
      Found : Boolean       := False;
      Index : Integer;
   begin
      if Session.Client = null or else Session.Disconnected then
         return;
      end if;
      Index := Locate (Session.Subscribtions.Singular, Topic);
      if Index > 0 then
         QoS := QoS + GetTag (Session.Subscribtions.Singular, Index);
         Found := True;
      end if;
      for Index in 1..GetSize (Session.Subscribtions.Patterns) loop
         if Match_Topic
            (  Topic,
               GetName (Session.Subscribtions.Patterns, Index)
            )
         then
            QoS := QoS + GetTag (Session.Subscribtions.Patterns, Index);
            Found := True;
         end if;
      end loop;
      if Found then
         Session.Packet := Session.Packet + 1;
         if not Is_Valid (Handle) then
            declare
               This : constant Message_Object_Ptr :=
                               new Message_Object
                                   (  Length => Topic'Length,
                                      Size   => Message'Length
                                   );
            begin
               Set (Handle, This);
               This.Topic   := Topic;
               This.Content := Message;
               This.Count   := Message'Length;
            end;
         end if;
         case QoS is
            when MQTT.At_Most_Once =>
               null;
            when MQTT.At_Least_Once =>
               Flags := Flags or At_Least_Once;
            when MQTT.Exactly_Once =>
               Flags := Flags or Exactly_Once;
         end case;
         if Retain then
            Flags := Flags or MQTT.Server.Retain;
         end if;
         if Duplicate then
            Flags := Flags or MQTT.Server.Duplicate;
         end if;
         Put
         (  Session.Output,
            (  Flags     => Flags,
               Packet    => Session.Packet,
               Message   => Ptr (Handle)
            ),
            Found
         );
         if Found then -- Full
            Trace
            (  Session.Client.all,
               Session.Name,
               (  "Messages queue is full (all "
               &  Image (Session.Queue_Size)
               &  " elements are used"
               ),
               Action
            );
            Shutdown (Session.Client.all);
         else -- Queued
            Increment_Count (Ptr (Handle).all);
            if Queued_To_Send (Session.Client.all) = 0 then
               --
               -- There is nothing to send.  Re-activate sending in case
               -- sending was blocked
               --
               Sent (Session.Client.all);
            end if;
         end if;
      end if;
   end Push;

   procedure Received
             (  Server  : in out MQTT_Server;
                Client  : in out MQTT_Connection'Class;
                Topic   : String;
                Message : Stream_Element_Array;
                QoS     : QoS_Level;
                Policy  : in out Message_Type
             )  is
   begin
      null;
   end Received;

   procedure Remove
             (  Set   : in out Messages_Set;
                Index : Positive
             )  is
   begin
      Release (Get (Set.Set, Index));
      Remove (Set.Set, Index);
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Constraint_Error'Identity,
            "No message with the number " & Image (Index)
         );
   end Remove;

   procedure Remove
             (  Server : in out MQTT_Server;
                Topic  : String
             )  is
      Pattern : constant Boolean := Check_Topic (Topic);
   begin
      if Pattern then
         if Topic = "#" then
            declare
               Lock : Holder (Server.Lock'Unchecked_Access);
            begin
               Erase (Server.Retained);
            end;
         else
            declare
               Lock : Holder (Server.Lock'Unchecked_Access);
            begin
               for Index in reverse 1..Get_Size (Server.Retained.Set)
               loop
                  if Match_Topic
                     (  Get (Server.Retained.Set, Index).Topic,
                        Topic
                     )
                  then
                     Remove (Server.Retained, Index);
                  end if;
               end loop;
            end;
         end if;
      else
         declare
            Message : aliased Message_Object (Topic'Length, 0);
         begin
            Message.Topic := Topic;
            declare
               Index : Integer;
               Lock  : Holder (Server.Lock'Unchecked_Access);
            begin
               Index :=
                  Find (Server.Retained.Set, Message'Unchecked_Access);
               if Index > 0 then
                  Remove (Server.Retained, Index);
               end if;
            end;
         end;
      end if;
   end Remove;

   procedure Remove
             (  Server : in out MQTT_Server;
                Index  : Positive
             )  is
      Lock : Holder (Server.Lock'Unchecked_Access);
   begin
      Remove (Server.Retained, Index);
   end Remove;

   procedure Send
             (  Client : in out MQTT_Connection;
                Data   : Stream_Element_Array
             )  is
      Lock : Holder (Client.Server.Lock'Unchecked_Access);
   begin
      Send (MQTT_Peer (Client), Data);
   end Send;

   procedure Send_Acknowledge
             (  Client  : in out MQTT_Connection;
                Request : Acknowledge_Type;
                Packet  : Packet_Identifier
             )  is
   begin
      if Is_Tracing_On (Client.Server.all, Trace_Acknowledgement) then
         Trace
         (  MQTT_Connection'Class (Client),
            Ptr (Client.Session).Name,
            (  Image (Request)
            &  " for packet "
            &  Image (Integer (Packet))
            ),
            Sent
         );
      end if;
      Send_Acknowledge (MQTT_Peer (Client), Request, Packet);
   end Send_Acknowledge;

   procedure Sent (Client : in out MQTT_Connection) is
      Server    : MQTT_Server'Class renames Client.Server.all;
      Lock      : Holder (Server.Lock'Unchecked_Access);
      Named     : constant Natural := GetSize (Server.Named);
      Anonymous : constant Natural := Get_Size (Server.Anonymous);
      Total     : constant Natural := Named + Anonymous;
      Last      : Positive renames Server.Last;
   begin
      Sent (MQTT_Peer (Client));
      for Count in 1..Total loop
         Last := Last + 1;
         if Last > Total then
            Last := 1;
         end if;
         if Last > Named then
            Pump_Unchecked
            (  Get (Server.Anonymous, Last - Named).all
            );
         else
            Pump_Unchecked (Ptr (GetTag (Server.Named, Last)).all);
         end if;
      end loop;
   end Sent;

   procedure Set_Queue_Size
             (  Server : in out MQTT_Server;
                Size   : Positive
             )  is
   begin
      Server.Queue_Size := Size;
   end Set_Queue_Size;

   procedure Set_Tracing_Flags
             (  Server : in out MQTT_Server;
                Flags  : MQTT_Trace_Flags
             )  is
   begin
      Server.Flags := Flags;
   end Set_Tracing_Flags;

   function To_Object_Ptr (Ptr : Message_Object_Ptr)
      return Message_Object_Ptr is
   begin
      return Ptr;
   end To_Object_Ptr;

end GNAT.Sockets.MQTT.Server;
