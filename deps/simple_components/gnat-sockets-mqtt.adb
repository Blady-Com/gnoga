--                                                                    --
--  package GNAT.Sockets.MQTT       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2016       --
--                                                                    --
--                                Last revision :  15:46 28 May 2016  --
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

with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.UTF8;      use Strings_Edit.UTF8;

with GNAT.Sockets.Connection_State_Machine.Big_Endian.Unsigneds;
use  GNAT.Sockets.Connection_State_Machine.Big_Endian.Unsigneds;

package body GNAT.Sockets.MQTT is
   use Stream_Element_Offset_Edit;

   Protocol_Name       : constant String := "MQTT";
   Protocol_Version    : constant := 4;

   CONNECT_Request     : constant := 2#0001_0000#;
   CONNACK_Request     : constant := 2#0010_0000#;
   PUBLISH_Request_1   : constant := 2#0011_0000#;
   PUBLISH_Request_2   : constant := 2#0011_1111#;
   PUBACK_Request      : constant := 2#0100_0000#;
   PUBREC_Request      : constant := 2#0101_0000#;
   PUBREL_Request      : constant := 2#0110_0010#;
   PUBCOMP_Request     : constant := 2#0111_0000#;
   SUBSCRIBE_Request   : constant := 2#1000_0010#;
   SUBACK_Request      : constant := 2#1001_0000#;
   UNSUBSCRIBE_Request : constant := 2#1010_0010#;
   UNSUBACK_Request    : constant := 2#1011_0000#;
   PINGREQ_Request     : constant := 2#1100_0000#;
   PINGRESP_Request    : constant := 2#1101_0000#;
   DISCONNECT_Request  : constant := 2#1110_0000#;

   Protocol            : constant := 1;
   Client_Identifier   : constant := 2;
   Will_Topic          : constant := 3;
   Will_Message        : constant := 4;
   User_Name           : constant := 5;
   Password            : constant := 6;

   Topic_Name          : constant := 1;

   function Check_Topic (Topic : String) return Boolean is
      Pointer  : Integer := Topic'First;
      This     : Code_Point;
      Level    : Boolean := True;
      Wildcard : Boolean := False;
   begin
      if Topic'Length = 0 then
         Raise_Exception (Constraint_Error'Identity, "Topic is empty");
      end if;
      loop
         Get (Topic, Pointer, This);
         case This is
            when Character'Pos ('+') =>
               if (  not Level
                  or else
                     (  Pointer <= Topic'Last
                     and then
                        Topic (Pointer) /= '/'
                  )  )
               then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "The wildcard + is not single character in a " &
                     "topic level"
                  );
               end if;
               Wildcard := True;
               Pointer  := Pointer + 1;
            when Character'Pos ('#') =>
               if not Level then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "The wildcard # is not single character in the " &
                     "last topic level"
                  );
               elsif Pointer <= Topic'Last then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "The wildcard # is not at the topic end"
                  );
               end if;
               return True;
            when Character'Pos ('/') =>
               Level := True;
            when others =>
               Level := False;
         end case;
         exit when Pointer > Topic'Last;
      end loop;
      return Wildcard;
   exception
      when Data_Error =>
         Raise_Exception
         (  Constraint_Error'Identity,
            "Topic UTF-8 encoding error"
         );
   end Check_Topic;

   function Compose
            (  Topic   : String;
               Message : Stream_Element_Array
            )  return MQTT_Message is
      Result : MQTT_Message;
      Ptr    : Message_Object_Ptr;
   begin
      if Check_Topic (Topic) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Topic contains wildcards"
         );
      end if;
      Ptr := new Message_Object (Topic'Length, Message'Length);
      Set (Result.Reference, Ptr);
      Ptr.Topic   := Topic;
      Ptr.Content := Message;
      Ptr.Count   := Message'Length;
      return Result;
   end Compose;

   function Compose
            (  Topic   : String;
               Message : String
            )  return MQTT_Message is
      Result : MQTT_Message;
      Ptr    : Message_Object_Ptr;
   begin
      if Check_Topic (Topic) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Topic contains wildcards"
         );
      end if;
      Ptr := new Message_Object (Topic'Length, Message'Length);
      Set (Result.Reference, Ptr);
      Ptr.Topic := Topic;
      Ptr.Count := Message'Length;
      for Index in Ptr.Content'Range loop
         Ptr.Content (Index) :=
            Character'Pos
            (  Message
               (  Integer (Index - 1)
               +  Message'First
            )  );
      end loop;
      return Result;
   end Compose;

   procedure Free is
      new Ada.Unchecked_Deallocation (Output_Buffer, Output_Buffer_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Stream_Element_Array,
             Stream_Element_Array_Ptr
          );

   procedure Finalize (Pier : in out MQTT_Pier) is
   begin
      Finalize (Connection (Pier));
      Free (Pier.Data);
      Free (Pier.Secondary);
      Free (Pier.Secondary);
   end Finalize;

   function Get_Length
            (  Value : Stream_Element_Count
            )  return Stream_Element_Count is
      Count  : Stream_Element_Count := Value;
      Length : Stream_Element_Count := 0;
   begin
      loop
         Length := Length + 1;
         Count  := Count / 128;
         exit when Count = 0;
      end loop;
      if Length > 4 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "MQTT payload data is larger than 2097152 octets"
         );
      end if;
      return Length;
   end Get_Length;

   function Get_Length (List : Topics_List) return Natural is
   begin
      return List'Length;
   end Get_Length;

   function Get_Max_Message_Size (Pier : MQTT_Pier)
      return Stream_Element_Count is
   begin
      return Pier.Data'Length;
   end Get_Max_Message_Size;

   function Get_Max_Secondary_Buffer_Size
            (  Pier : MQTT_Pier
            )  return Stream_Element_Count is
   begin
      return Pier.Max_Size;
   end Get_Max_Secondary_Buffer_Size;

   function Get_Message
            (  Pier  : MQTT_Pier;
               Index : Positive
            )  return Stream_Element_Array is
      This : MQTT_String_Ptr;
   begin
      if Pier.List_Length <= Index then
         This := Get (Pier.List, Index);
         if This /= null then
            return This.Data (1..This.Length);
         end if;
      end if;
      return (1..0 => 0);
   end Get_Message;

   function Get_Message (Message : MQTT_Message)
      return Stream_Element_Array is
      This : Message_Object'Class renames Ptr (Message.Reference).all;
   begin
      return This.Content (1..This.Count);
   end Get_Message;

   function Get_Message (Message : MQTT_Message) return String is
      This : Message_Object'Class renames Ptr (Message.Reference).all;
   begin
      return To_String (This.Content (1..This.Count));
   end Get_Message;

   function Get_QoS
            (  Pier  : MQTT_Pier;
               Index : Positive
            )  return QoS_Level is
      This : MQTT_String_Ptr;
   begin
      case Pier.Header is
         when SUBSCRIBE_Request =>
            if Index > Pier.List_Length then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "There is no token " & Image (Index)
               );
            end if;
            This := Get (Pier.List, Index);
            if This = null then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "There is no QoS for token " & Image (Index)
               );
            else
               return This.QoS;
            end if;
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               "There is no tokens or QoS list (not SUBSCRIBE)"
            );
      end case;
   end Get_QoS;

   function Get_String
            (  Pier  : MQTT_Pier;
               Index : Positive
            )  return String is
      This : MQTT_String_Ptr;
   begin
      if Pier.List_Length >= Index then
         This := Get (Pier.List, Index);
         if This /= null then
            declare
               Data : Stream_Element_Array renames This.Data;
               Text : String (1..Natural (This.Length));
            begin
               for Index in Text'Range loop
                  Text (Index) :=
                     Character'Val
                     (  Data
                        (  Stream_Element_Offset (Index)
                        -  1
                        +  Data'First
                     )  );
               end loop;
               return Text;
            end;
         end if;
      end if;
      return "";
   end Get_String;

   function Get_Topic
            (  Pier  : MQTT_Pier;
               Index : Positive
            )  return String is
   begin
      case Pier.Header is
         when SUBSCRIBE_Request | UNSUBSCRIBE_Request =>
            if Index > Pier.List_Length then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "There is no token " & Image (Index)
               );
            else
               return Get_String (Pier, Index);
            end if;
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               "There is no tokens list (not SUBSCRIBE or UNSUBSCRIBE)"
            );
      end case;
   end Get_Topic;

   function Get_Topic (Message : MQTT_Message) return String is
   begin
      return Ptr (Message.Reference).Topic;
   end Get_Topic;

   function Get_Topic (Topic : Topic_Item) return String is
   begin
      return Ptr (Topic).Topic;
   end Get_Topic;

   function Get_Topic
            (  List  : Topics_List;
               Index : Positive
            )  return String is
   begin
      if Index not in List'Range then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Token list subscript error"
         );
      end if;
      return Ptr (List (Index)).Topic;
   end Get_Topic;

   function Get_Secondary_Buffer_Size
            (  Pier : MQTT_Pier
            )  return Stream_Element_Count is
   begin
      if Pier.Secondary = null then
         return 0;
      else
         return Pier.Secondary.Size;
      end if;
   end Get_Secondary_Buffer_Size;

   function Get_Size
            (  Length : Stream_Element_Count
            )  return Stream_Element_Count is
   begin
      return Get_Length (Length) + Length + 1;
   end Get_Size;

   function Header_Image (Header : Stream_Element) return String is
      use Strings_Edit;
      Text    : String (1..32);
      Pointer : Integer := 1;
   begin
      case Header / 16 is
         when  1 => Put (Text, Pointer, "CONNECT");
         when  2 => Put (Text, Pointer, "CONNACK");
         when  3 => Put (Text, Pointer, "PUBLISH");
         when  4 => Put (Text, Pointer, "PUBACK");
         when  5 => Put (Text, Pointer, "PUBREC");
         when  6 => Put (Text, Pointer, "PUBREL");
         when  7 => Put (Text, Pointer, "PUBCOMP");
         when  8 => Put (Text, Pointer, "SUBSCRIBE");
         when  9 => Put (Text, Pointer, "SUBACK");
         when 10 => Put (Text, Pointer, "UNSUBSCRIBE");
         when 11 => Put (Text, Pointer, "UNSUBACK");
         when 12 => Put (Text, Pointer, "PINGREQ");
         when 13 => Put (Text, Pointer, "PINGRESP");
         when 14 => Put (Text, Pointer, "DISCONNECT");
         when others => Put (Text, Pointer, "Reserved");
      end case;
      Put (Text, Pointer, ":");
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => Integer (Header mod 16),
         Field       => 4,
         Base        => 2,
         Fill        => '0',
         Justify     => Right
      );
      return Text (1..Pointer - 1);
   end Header_Image;

   function Image (Value : Acknowledge_Type) return String is
   begin
      case Value is
         when Publish_Level_1          => return "PUBACK";
         when Publish_Level_2_Received => return "PUBREC";
         when Publish_Level_2_Release  => return "PUBREL";
         when Publish_Level_2_Complete => return "PUBCOMP";
         when Unsubscribed             => return "UNSUBACK";
      end case;
   end Image;

   function Image (QoS : QoS_Level) return String is
   begin
      case QoS is
         when At_Most_Once  => return "at most once";
         when At_Least_Once => return "at least once";
         when Exactly_Once  => return "exactly once";
      end case;
   end Image;

   function Image (Code : Connect_Response) return String is
   begin
      case Code is
         when Unacceptable_Protocol_Version =>
            return "unacceptable protocol version";
         when Identifier_Rejected =>
            return "identifier rejected";
         when Server_Unavailable =>
            return "server unavailable";
         when Bad_User_Name_Or_Password =>
            return "bad use name or password";
         when Not_Authorized =>
            return "not authorized";
         when others =>
            return "code " & Image (Integer (Code));
      end case;
   end Image;

   function Match_Topic
            (  Topic   : String;
               Pattern : String
            )  return Boolean is
      Level   : Boolean := True;
      Pointer : Integer := Topic'First;
      Index   : Integer := Pattern'First;
      This    : Code_Point;
      That    : Code_Point;
   begin
      if Topic'Length = 0 then
         Raise_Exception (Constraint_Error'Identity, "Topic is empty");
      elsif Pattern'Length = 0 then
         return False;
      elsif Topic (Topic'First) = '$' then
         case Pattern (Pattern'First) is
            when '+' | '#' => return False;
            when others    => null;
         end case;
      end if;
      while Pointer <= Topic'Last loop
         Get (Topic, Pointer, This);
         if Index > Pattern'Last then
            return False;
         end if;
         Get (Pattern, Index, That);
         case This is
            when Character'Pos ('/') =>
               if That /= Character'Pos ('/') then
                  return False;
               end if;
            when Character'Pos ('+') | Character'Pos ('#') =>
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Topic contains wildcards"
               );
            when others =>
               case That is
                  when Character'Pos ('+') =>
                     while Pointer <= Topic'Last loop
                        Get (Topic, Pointer, This);
                        case This is
                           when Character'Pos ('/') =>
                              Pointer := Pointer - 1;
                              exit;
                           when Character'Pos ('+') |
                                Character'Pos ('#') =>
                              Raise_Exception
                              (  Constraint_Error'Identity,
                                 "Topic contains wildcards"
                              );
                           when others =>
                              null;
                        end case;
                     end loop;
                  when Character'Pos ('#') =>
                     if Index > Pattern'Last then
                        return True;
                     end if;
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        (  "The wildcard # is not single character "
                        &  "in the last topic pattern level"
                     )  );
                  when others =>
                     if This /= That then
                        return False;
                     end if;
               end case;
         end case;
      end loop;
      return Index > Pattern'Last;
   exception
      when Data_Error =>
         Raise_Exception
         (  Constraint_Error'Identity,
            "Topic or pattern UTF-8 encoding error"
         );
   end Match_Topic;

   procedure On_Acknowledge
             (  Pier    : in out MQTT_Pier;
                Request : Acknowledge_Type;
                Packet  : Packet_Identifier
             )  is
   begin
      case Request is
         when Publish_Level_1 =>
            null;
         when Publish_Level_2_Received =>
            Send_Acknowledge (Pier, Publish_Level_2_Release, Packet);
         when Publish_Level_2_Release =>
            Send_Acknowledge (Pier, Publish_Level_2_Complete, Packet);
         when Publish_Level_2_Complete =>
            null;
         when Unsubscribed =>
            null;
      end case;
   end On_Acknowledge;

   procedure On_Connect
             (  Pier         : in out MQTT_Pier;
                Client       : String;
                Clean        : Boolean;
                Will_Topic   : String;
                Will_Message : Stream_Element_Array;
                Will_QoS     : QoS_Level;
                Will_Retain  : Boolean;
                User_Name    : String;
                Password     : String;
                Keep_Alive   : Duration
             )  is
   begin
      Send_Connect_Rejected (Pier, Server_Unavailable);
      Shutdown (Pier);
   end On_Connect;

   procedure On_Connect_Accepted
             (  Pier            : in out MQTT_Pier;
                Session_Present : Boolean
             )  is
   begin
      null;
   end On_Connect_Accepted;

   procedure On_Connect_Rejected
             (  Pier     : in out MQTT_Pier;
                Response : Connect_Response
             )  is
   begin
      null;
   end On_Connect_Rejected;

   procedure On_Disconnect (Pier : in out MQTT_Pier) is
   begin
      null;
   end On_Disconnect;

   procedure On_Ping (Pier : in out MQTT_Pier) is
   begin
      Send_Ping_Response (Pier);
   end On_Ping;

   procedure On_Ping_Response (Pier : in out MQTT_Pier) is
   begin
      null;
   end On_Ping_Response;

   procedure On_Publish
             (  Pier      : in out MQTT_Pier;
                Topic     : String;
                Message   : Stream_Element_Array;
                Packet    : Packet_Identification;
                Duplicate : Boolean;
                Retain    : Boolean
             )  is
   begin
      case Packet.QoS is
         when At_Most_Once =>
            null;
         when At_Least_Once =>
            Send_Acknowledge (Pier, Publish_Level_1, Packet.ID);
         when Exactly_Once =>
            Send_Acknowledge
            (  Pier,
               Publish_Level_2_Received,
               Packet.ID
            );
      end case;
   end On_Publish;

   procedure On_Subscribe
             (  Pier          : in out MQTT_Pier;
                Packet        : Packet_Identifier;
                Topics_Number : Positive
             )  is
   begin
      On_Subscribe_Acknowledgement
      (  Pier,
         Packet,
         (1..Topics_Number => (Success => False))
      );
   end On_Subscribe;

   procedure On_Subscribe_Acknowledgement
             (  Pier   : in out MQTT_Pier;
                Packet : Packet_Identifier;
                Codes  : Return_Code_List
             )  is
   begin
      null;
   end On_Subscribe_Acknowledgement;

   procedure On_Unsubscribe
             (  Pier          : in out MQTT_Pier;
                Packet        : Packet_Identifier;
                Topics_Number : Positive
             )  is
   begin
      Send_Acknowledge (Pier, Unsubscribed, Packet);
   end On_Unsubscribe;

   procedure Put_Length
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Stream_Element_Count
             )  is
      Length : Stream_Element_Count := Value;
   begin
      loop
         Data (Pointer) := Stream_Element'Val (Length mod 128);
         Pointer := Pointer + 1;
         Length  := Length / 128;
         exit when Length = 0;
         Data (Pointer - 1) := Data (Pointer - 1) + 128;
      end loop;
   end Put_Length;

   procedure Put_String
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             )  is
   begin
      if Value'Length > 2**16 - 1 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "String is longer than 65535 characters"
         );
      elsif (  Pointer < Data'First
            or else
               Data'Last - Pointer - 1 < Value'Length
            )
      then
         if Pointer >= Data'First and then Pointer - 1 <= Data'Last then
            Raise_Exception
            (  End_Error'Identity,
               "No room for output"
            );
         else
            Raise_Exception
            (  Layout_Error'Identity,
               "Invalid pointer"
            );
         end if;
      end if;
      Put (Data, Pointer, Unsigned_16 (Value'Length));
      for Index in Value'Range loop
         Data (Pointer) := Character'Pos (Value (Index));
         Pointer := Pointer + 1;
      end loop;
   end Put_String;

   procedure Put_String
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Stream_Element_Array
             )  is
   begin
      if Value'Length > 2**16 - 1 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "String is longer than 65535 characters"
         );
      elsif (  Pointer < Data'First
            or else
               Data'Last - Pointer < 1 + Value'Length
            )
      then
         if Pointer >= Data'First and then Pointer - 1 <= Data'Last then
            Raise_Exception
            (  End_Error'Identity,
               "No room for output"
            );
         else
            Raise_Exception
            (  Layout_Error'Identity,
               "Invalid pointer"
            );
         end if;
      end if;
      Put (Data, Pointer, Unsigned_16 (Value'Length));
      Data (Pointer..Pointer + Value'Length - 1) := Value;
      Pointer := Pointer + Value'Length;
   end Put_String;

   procedure Received
             (  Pier    : in out MQTT_Pier;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      procedure Do_On_Publish is
         Packet : Packet_Identification;
      begin
         case Pier.QoS is
            when At_Most_Once =>
               Packet := (QoS => At_Most_Once);
            when At_Least_Once =>
               Packet := (At_Least_Once, Pier.Packet_ID);
            when Exactly_Once =>
               Packet := (Exactly_Once, Pier.Packet_ID);
         end case;
         On_Publish
         (  Pier    => MQTT_Pier'Class (Pier),
            Topic   => Get_String (Pier, Topic_Name),
            Message => Pier.Data (1..Pier.Length),
            Packet    => Packet,
            Retain    => 0 /= (Pier.Header and 2#0000_0001#),
            Duplicate => 0 /= (Pier.Header and 2#0000_1000#)
         );
      end Do_On_Publish;

      procedure Data_Received is
         pragma Inline (Data_Received);
      begin
         case Pier.Header is
            when PUBLISH_Request_1..PUBLISH_Request_2 =>
               Do_On_Publish;
               Pier.State := MQTT_Header;
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  "MQTT internal error, unexpected " &
                  "Data_Received for header " &
                  Header_Image (Pier.Header)
               );
         end case;
      end Data_Received;

      procedure Packet_ID_Received is
         pragma Inline (Packet_ID_Received);
      begin
         case Pier.Header is
            when PUBLISH_Request_1..PUBLISH_Request_2 =>
               Pier.Length := Pier.Length - 2;
               if Pier.Length = 0 then
                  Do_On_Publish;
                  Pier.State := MQTT_Header;
               else
                  Pier.Count := 0;
                  Pier.State := MQTT_Data;
               end if;
            when PUBACK_Request =>
               On_Acknowledge
               (  MQTT_Pier'Class (Pier),
                  Publish_Level_1,
                  Pier.Packet_ID
               );
               Pier.State := MQTT_Header;
            when PUBREC_Request =>
               On_Acknowledge
               (  MQTT_Pier'Class (Pier),
                  Publish_Level_2_Received,
                  Pier.Packet_ID
               );
               Pier.State := MQTT_Header;
            when PUBREL_Request =>
               On_Acknowledge
               (  MQTT_Pier'Class (Pier),
                  Publish_Level_2_Release,
                  Pier.Packet_ID
               );
               Pier.State := MQTT_Header;
            when PUBCOMP_Request =>
               On_Acknowledge
               (  MQTT_Pier'Class (Pier),
                  Publish_Level_2_Complete,
                  Pier.Packet_ID
               );
               Pier.State := MQTT_Header;
            when SUBSCRIBE_Request =>
               Pier.Length := Pier.Length - 2;
               if Pier.Length < 3 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT SUBSCRIBE request does not " &
                     "contain topics list"
                  );
               end if;
               Pier.State := MQTT_String_Length_MSB;
            when SUBACK_Request =>
               Pier.Length := Pier.Length - 2;
               if Pier.Length < 1 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT SUBACK request does not " &
                     "contain return codes"
                  );
               end if;
               Pier.State := MQTT_Return_Code;
            when UNSUBSCRIBE_Request =>
               Pier.Length := Pier.Length - 2;
               if Pier.Length < 2 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT UNSUBSCRIBE request is shorter then 2 octets"
                  );
               end if;
               Pier.State := MQTT_String_Length_MSB;
            when UNSUBACK_Request =>
               On_Acknowledge
               (  MQTT_Pier'Class (Pier),
                  Unsubscribed,
                  Pier.Packet_ID
               );
               Pier.State := MQTT_Header;
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  "MQTT internal error, unexpected " &
                  "Packet_ID_Received for header " &
                  Header_Image (Pier.Header)
               );
         end case;
      end Packet_ID_Received;

      procedure String_Received is
      begin
         case Pier.Header is
            when CONNECT_Request =>
               case Pier.List_Length is
                  when Protocol =>
                     Pier.State := MQTT_Connect_Version;
                  when Client_Identifier =>
                     if 0 = (Pier.Flags and 2#0000_0100#) then
                        Reset_String (Pier, Will_Topic);
                        Reset_String (Pier, Will_Message);
                        Pier.List_Length := Pier.List_Length + 2;
                        String_Received;
                     else
                        if Pier.Length < 4 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT connect does not have will " &
                              "topic and message values"
                           );
                        end if;
                        Pier.State := MQTT_String_Length_MSB;
                     end if;
                  when Will_Topic =>
                     if Pier.Length < 2 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           "MQTT connect does not have will " &
                           "message value"
                        );
                     end if;
                     Pier.State := MQTT_String_Length_MSB;
                  when Will_Message =>
                     if 0 = (Pier.Flags and 2#1000_0000#) then
                        Reset_String (Pier, User_Name);
                        Pier.List_Length := Pier.List_Length + 1;
                        String_Received;
                     else
                        if Pier.Length < 2 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT connect does not have " &
                              "user name value"
                           );
                        end if;
                        Pier.State := MQTT_String_Length_MSB;
                     end if;
                  when User_Name =>
                     if 0 = (Pier.Flags and 2#0100_0000#) then
                        Reset_String (Pier, Password);
                        Pier.List_Length := Pier.List_Length + 1;
                        String_Received;
                     else
                        if Pier.Length < 2 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT connect does not have " &
                              "password value"
                           );
                        end if;
                        Pier.State := MQTT_String_Length_MSB;
                     end if;
                  when Password =>
                     if Pier.Length /= 0 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           "MQTT connect contains unrecognized data"
                        );
                     end if;
                     case (Pier.Flags and 2#0001_1000#) / 8 is
                        when 0 => Pier.QoS := At_Most_Once;
                        when 1 => Pier.QoS := At_Least_Once;
                        when 2 => Pier.QoS := Exactly_Once;
                        when others =>
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT invalid connect will QoS"
                           );
                     end case;
                     On_Connect
                     (  Pier =>
                           MQTT_Pier'Class (Pier),
                        Client =>
                           Get_String (Pier, Client_Identifier),
                        Will_Topic =>
                           Get_String (Pier, Will_Topic),
                        Will_Message =>
                           Get_Message (Pier, Will_Message),
                        Will_QoS =>
                           Pier.QoS,
                        User_Name =>
                           Get_String (Pier, User_Name),
                        Password =>
                           Get_String (Pier, Password),
                        Will_Retain =>
                           0 /= (Pier.Flags and 2#0010_0000#),
                        Clean =>
                           0 /= (Pier.Flags and 2#0000_0010#),
                        Keep_Alive =>
                           Pier.Keep_Alive
                     );
                     Pier.State := MQTT_Header;
                  when others =>
                     Raise_Exception
                     (  Data_Error'Identity,
                        "MQTT internal error, invalid connect " &
                        "string index " &
                        Image (Pier.List_Length)
                     );
               end case;
            when PUBLISH_Request_1..PUBLISH_Request_2 =>
               case Pier.List_Length is
                  when Topic_Name =>
                     case Pier.QoS is
                        when At_Most_Once =>
                           if Pier.Length = 0 then
                              Do_On_Publish;
                              Pier.State := MQTT_Header;
                           else
                              Pier.Count := 0;
                              Pier.State := MQTT_Data;
                           end if;
                        when At_Least_Once | Exactly_Once =>
                           if Pier.Length < 2 then
                              Raise_Exception
                              (  Data_Error'Identity,
                                 "MQTT publish request does not " &
                                 "contain packet identifier"
                              );
                           end if;
                           Pier.State := MQTT_Packet_ID_MSB;
                     end case;
                  when others =>
                     Raise_Exception
                     (  Data_Error'Identity,
                        "MQTT internal error, invalid publish " &
                        "string index"
                     );
               end case;
            when SUBSCRIBE_Request =>
               if Pier.Length < 1 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT SUBSCRIBE request does not " &
                     "contain QoS"
                  );
               end if;
               Pier.State := MQTT_QoS;
            when UNSUBSCRIBE_Request =>
               if Pier.Length = 0 then
                  On_Unsubscribe
                  (  Pier          => MQTT_Pier'Class (Pier),
                     Packet        => Pier.Packet_ID,
                     Topics_Number => Pier.List_Length
                  );
                  Pier.State := MQTT_Header;
               elsif Pier.Length < 2 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT UNSUBSCRIBE request topic " &
                     Image (Pier.List_Length + 1) &
                     " is shorter than 2 octets"
                  );
               elsif Pier.List_Length >= Pier.Max_Subscribe_Topics then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT UNSUBSCRIBE request topics number exceeds " &
                     Image (Pier.Max_Subscribe_Topics)
                  );
               else
                  Pier.State := MQTT_String_Length_MSB;
               end if;
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  "MQTT internal error, unexpected String_Received " &
                  "for header " &
                  Header_Image (Pier.Header)
               );
         end case;
      end String_Received;

   begin
      Pointer := Data'First;
      while Pointer <= Data'Last loop
         case Pier.State is
            when MQTT_Header => -- Get header
--                 if (  Pier.Secondary /= null
--                    and then
--                       Pier.Secondary.Last > 0
--                    )  then -- Have unsent  data in the secondary buffer,
--                    return; -- stop processing input until buffer emptied
--                 end if;
               Pier.Header      := Data (Pointer);
               Pier.List_Length := 0;
               Pier.Length      := 0;
               Pier.Count       := 1;
               Pointer          := Pointer + 1;
               Pier.State       := MQTT_Length;
            when MQTT_Length => -- Get length
               Pier.Length :=
                  (  Pier.Length
                  +  Stream_Element_Count
                     (  Data (Pointer) and 2#0111_1111#
                     )
                  *  Pier.Count
                  );
               if 0 = (Data (Pointer) and 2#1000_0000#) then
                  Pointer := Pointer + 1;
                  case Pier.Header is
                     when CONNECT_Request =>
                        if Pier.Length < 12 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT CONNECT request length is less " &
                              "than 12 octets"
                           );
                        end if;
                        Pier.Count  := 1;
                        Pier.State  := MQTT_String_Length_MSB;
                     when CONNACK_Request =>
                        if Pier.Length /= 2 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT CONNACK request length is not 2"
                           );
                        end if;
                        Pier.State := MQTT_Connect_Acknowledge_Flags;
                     when PUBLISH_Request_1..PUBLISH_Request_2 =>
                        case (Pier.Header and 2#0000_0110#) / 2 is
                           when 0 => Pier.QoS := At_Most_Once;
                           when 1 => Pier.QoS := At_Least_Once;
                           when 2 => Pier.QoS := Exactly_Once;
                           when others =>
                              Raise_Exception
                              (  Data_Error'Identity,
                                 "MQTT invalid PUBLISH QoS"
                              );
                        end case;
                        if Pier.Length < 2 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT PUBLISH request length " &
                              "is less than 2 octets"
                           );
                        end if;
                        Pier.State := MQTT_String_Length_MSB;
                     when PUBACK_Request =>
                        if Pier.Length /= 2 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT PUBACK request length is " &
                              "not 2 octets"
                           );
                        end if;
                        Pier.State := MQTT_Packet_ID_MSB;
                     when PUBREC_Request =>
                        if Pier.Length /= 2 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT PUBREC request length is " &
                              "not 2 octets"
                           );
                        end if;
                        Pier.State := MQTT_Packet_ID_MSB;
                     when PUBREL_Request =>
                        if Pier.Length /= 2 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT PUBREL request length is " &
                              "not 2 octets"
                           );
                        end if;
                        Pier.State := MQTT_Packet_ID_MSB;
                     when PUBCOMP_Request => -- PubComp
                        if Pier.Length /= 2 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT PUBCOMP request length is " &
                              "not 2 octets"
                           );
                        end if;
                        Pier.State := MQTT_Packet_ID_MSB;
                     when SUBSCRIBE_Request =>
                        if Pier.Length < 5 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT SUBSCRIBE request length is less " &
                              "than 5 octets"
                           );
                        end if;
                        Pier.State := MQTT_Packet_ID_MSB;
                     when SUBACK_Request =>
                        if Pier.Length < 3 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT SUBACK requeste length is " &
                              "less than 3 octets"
                           );
                        end if;
                        Pier.State := MQTT_Packet_ID_MSB;
                     when UNSUBSCRIBE_Request =>
                        if Pier.Length <= 4 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT UNSUBSCRIBE request length is " &
                              "less than 4 octets"
                           );
                        end if;
                        Pier.State := MQTT_Packet_ID_MSB;
                     when UNSUBACK_Request =>
                        if Pier.Length < 2 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT UNSUBACK request length " &
                              "is less than 2 octets"
                           );
                        end if;
                        Pier.State := MQTT_Packet_ID_MSB;
                     when PINGREQ_Request =>
                        if Pier.Length /= 0 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT PINGREQ request length is not null"
                           );
                        end if;
                        On_Ping (MQTT_Pier'Class (Pier));
                        Pier.State := MQTT_Header;
                     when PINGRESP_Request =>
                        if Pier.Length /= 0 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT PINGRESP request length is not null"
                           );
                        end if;
                        On_Ping_Response (MQTT_Pier'Class (Pier));
                        Pier.State := MQTT_Header;
                     when DISCONNECT_Request =>
                        if Pier.Length /= 0 then
                           Raise_Exception
                           (  Data_Error'Identity,
                              "MQTT DISCONNECT request length is " &
                              "not null"
                           );
                        end if;
                        On_Disconnect (MQTT_Pier'Class (Pier));
                        Pier.State := MQTT_Header;
                     when others =>
                        Raise_Exception
                        (  Data_Error'Identity,
                           "Invalid MQTT header: " &
                           Header_Image (Pier.Header)
                        );
                  end case;
               elsif Pier.Count < 128 * 128 * 128 then
                  Pointer    := Pointer + 1;
                  Pier.Count := Pier.Count * 128;
               else
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT length field is larger than 4 octets"
                  );
               end if;
            when MQTT_Packet_ID_MSB =>
               Pier.Packet_ID :=
                  Packet_Identifier (Data (Pointer)) * 256;
               Pointer    := Pointer + 1;
               Pier.State := MQTT_Packet_ID_LSB;
            when MQTT_Packet_ID_LSB =>
               Pier.Packet_ID :=
                  Pier.Packet_ID + Packet_Identifier (Data (Pointer));
               Pointer := Pointer + 1;
               Packet_ID_Received;
            when MQTT_Connect_Version =>
               Pier.Version := Data (Pointer);
               Pier.Length  := Pier.Length - 1;
               if Pier.Length < 1 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT CONNECT request does not contain flags"
                  );
               end if;
               Pointer := Pointer + 1;
               Pier.State := MQTT_Connect_Flags;
            when MQTT_Connect_Acknowledge_Flags =>
               Pier.Flags := Data (Pointer);
               if 0 /= (Pier.Flags and 2#1111_1110#) then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT invalid CONNACK request flags"
                  );
               end if;
               Pointer    := Pointer + 1;
               Pier.State := MQTT_Connect_Return;
            when MQTT_Connect_Return =>
               Pointer := Pointer + 1;
               if Data (Pointer - 1) = 0 then
                  On_Connect_Accepted
                  (  Pier =>
                        MQTT_Pier'Class (Pier),
                     Session_Present =>
                        0 /= (Pier.Flags and 2#0000_0001#)
                  );
               else
                  On_Connect_Rejected
                  (  Pier =>
                        MQTT_Pier'Class (Pier),
                     Response =>
                        Connect_Response (Data (Pointer - 1))
                  );
               end if;
               Pier.State := MQTT_Header;
            when MQTT_Connect_Flags =>
               Pier.Flags := Data (Pointer);
               if 0 /= (Pier.Flags and 2#0000_0001#) then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT invalid CONNECT request flags"
                  );
               end if;
               Pier.Length := Pier.Length - 1;
               Pointer     := Pointer + 1;
               Pier.State  := MQTT_Connect_Duration_MSB;
               if Pier.Length < 2 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT CONNECT request does not keep alive duration"
                  );
               end if;
            when MQTT_Connect_Duration_MSB =>
               Pier.Keep_Alive :=
                  Duration (Integer (Data (Pointer)) * 256);
               Pointer    := Pointer + 1;
               Pier.State := MQTT_Connect_Duration_LSB;
            when MQTT_Connect_Duration_LSB =>
               Pier.Keep_Alive :=
                  Pier.Keep_Alive + Duration (Integer (Data (Pointer)));
               Pointer     := Pointer + 1;
               Pier.Length := Pier.Length - 2;
               Pier.State  := MQTT_String_Length_MSB; -- Client ID next
               if Pier.Length < 2 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT CONNECT request does not client ID"
                  );
               end if;
            when MQTT_String_Length_MSB =>
               Pier.Count :=
                  Stream_Element_Count (Data (Pointer)) * 256;
               Pointer    := Pointer + 1;
               Pier.State := MQTT_String_Length_LSB;
            when MQTT_String_Length_LSB =>
               Pier.Count :=
                  Pier.Count + Stream_Element_Count (Data (Pointer));
               Pointer     := Pointer + 1;
               Pier.Length := Pier.Length - 2;
               Pier.List_Length := Pier.List_Length + 1;
               if Pier.Length < Pier.Count then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "MQTT string "
                     &  String_Name (Pier.Header, Pier.List_Length)
                     &  " has length "
                     &  Image (Pier.Count)
                     &  " exceeding remaining payload data "
                     &  Image (Pier.Length)
                  )  );
               end if;
               declare
                  This : MQTT_String_Ptr :=
                         Get (Pier.List, Pier.List_Length);
               begin
                  if This = null or else This.Size < Pier.Count then
                     Put
                     (  Pier.List,
                        Pier.List_Length,
                        new MQTT_String (Pier.Count)
                     );
                     This := Get (Pier.List, Pier.List_Length);
                  end if;
                  This.Length := Pier.Count;
                  Pier.Length := Pier.Length - Pier.Count;
                  if This.Length = 0 then
                     String_Received;
                  else
                     Pier.Count  := 0;
                     Pier.State := MQTT_String_Body;
                  end if;
               end;
            when MQTT_String_Body =>
               declare
                  This   : MQTT_String renames
                           Get (Pier.List, Pier.List_Length).all;
                  Length : Stream_Element_Offset;
               begin
                  Length :=
                     Stream_Element_Offset'Min
                     (  Data'Last + 1 - Pointer,
                        Stream_Element_Offset (This.Length) - Pier.Count
                     );
                  This.Data (Pier.Count + 1..Pier.Count + Length) :=
                     Data (Pointer..Pointer + Length - 1);
                  Pointer    := Pointer + Length;
                  Pier.Count := Pier.Count + Length;
                  if Pier.Count >= Stream_Element_Offset (This.Length)
                  then -- The string has been received
                     String_Received;
                  end if;
               end;
            when MQTT_Data =>
               declare
                  Length : Stream_Element_Offset;
               begin
                  Length :=
                     Stream_Element_Offset'Min
                     (  Data'Last + 1 - Pointer,
                        Pier.Length - Pier.Count
                     );
                  Pier.Data (Pier.Count + 1..Pier.Count + Length) :=
                     Data (Pointer..Pointer + Length - 1);
                  Pier.Count := Pier.Count + Length;
                  Pointer    := Pointer + Length;
               end;
               if Pier.Count >= Pier.Length then
                  Data_Received;
                  Pier.State := MQTT_Header;
               end if;
            when MQTT_QoS =>
               case Data (Pointer) is
                  when 0 => Pier.QoS := At_Most_Once;
                  when 1 => Pier.QoS := At_Least_Once;
                  when 2 => Pier.QoS := Exactly_Once;
                  when others =>
                     Raise_Exception
                     (  Data_Error'Identity,
                        "MQTT invalid subscribe QoS for " &
                        Get_String (Pier, Pier.List_Length)
                     );
               end case;
               Pointer := Pointer + 1;
               Get (Pier.List, Pier.List_Length).QoS := Pier.QoS;
               Pier.Length := Pier.Length - 1;
               if Pier.Length = 0 then
                  On_Subscribe
                  (  Pier          => MQTT_Pier'Class (Pier),
                     Packet        => Pier.Packet_ID,
                     Topics_Number => Pier.List_Length
                  );
                  Pier.State := MQTT_Header;
               elsif Pier.Length < 3 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT subscribe token " &
                     Image (Pier.List_Length) &
                     " is shorter than 3 octets"
                  );
               else
                  if Pier.List_Length = Pier.Max_Subscribe_Topics then
                     Raise_Exception
                     (  Data_Error'Identity,
                        "MQTT number of subscribe topics exceeds " &
                        Image (Pier.Max_Subscribe_Topics)
                     );
                  end if;
                  Pier.State := MQTT_String_Length_MSB;
               end if;
            when MQTT_Return_Code =>
               if Pier.List_Length = Pier.Max_Subscribe_Topics then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "MQTT number of subscribe topics exceeds " &
                     Image (Pier.Max_Subscribe_Topics) &
                     " (in subscribe acknowledgement)"
                  );
               end if;
               Pier.List_Length := Pier.List_Length + 1;
               Pier.Length      := Pier.Length - 1;
               declare
                  This : MQTT_String_Ptr :=
                         Get (Pier.List, Pier.List_Length);
               begin
                  if This = null then
                     Put
                     (  Pier.List,
                        Pier.List_Length,
                        new MQTT_String (0)
                     );
                     This := Get (Pier.List, Pier.List_Length);
                  end if;
                  case Data (Pointer) is
                     when 16#00# =>
                        This.QoS := At_Most_Once;
                        This.Failure := False;
                     when 16#01# =>
                        This.QoS := At_Least_Once;
                        This.Failure := False;
                     when 16#02# =>
                        This.QoS := Exactly_Once;
                        This.Failure := False;
                     when 16#80# =>
                        This.Failure := True;
                     when others =>
                        Raise_Exception
                        (  Data_Error'Identity,
                           "MQTT invalid SUBACK request " &
                           "return code " &
                           Image (Pier.List_Length)
                        );
                  end case;
                  Pointer := Pointer + 1;
               end;
               if Pier.Length = 0 then
                  declare
                     Codes : Return_Code_List (1..Pier.List_Length);
                  begin
                     for Index in 1..Pier.List_Length loop
                        declare
                           This : MQTT_String renames
                                  Get (Pier.List, Index).all;
                        begin
                           if This.Failure then
                              Codes (Index) := (Success => False);
                           else
                              Codes (Index) := (True, This.QoS);
                           end if;
                        end;
                     end loop;
                     On_Subscribe_Acknowledgement
                     (  Pier   => MQTT_Pier'Class (Pier),
                        Packet => Pier.Packet_ID,
                        Codes  => Codes
                     );
                  end;
                  Pier.State := MQTT_Header;
               end if;
         end case;
      end loop;
   end Received;

   procedure Release (Ptr : Message_Object_Ptr) is
      use Object;
      Message : Entity_Ptr := Entity_Ptr (Ptr);
   begin
      if Message /= null then
         Release (Message);
      end if;
   end Release;

   function Ref (Thing : Topic_Item_Data_Ptr) return Topic_Item is
   begin
      return Topic_Item'
             (  Topic_Item_Handles.Ref (Thing)
             with null record
             );
   end Ref;

   procedure Reset_String
             (  Pier  : in out MQTT_Pier;
                Index : Positive
             )  is
      This : MQTT_String_Ptr;
   begin
      if Pier.List_Length <= Index then
         This := Get (Pier.List, Index);
         if This /= null then
            This.Length := 0;
         end if;
      end if;
   end Reset_String;

   procedure Send
             (  Pier : in out MQTT_Pier;
                Data : Stream_Element_Array
             )  is
      Pointer : Stream_Element_Offset := Data'First;
   begin
      if Pier.Secondary = null then
         Send (Pier, Data, Pointer);
         if Pointer > Data'Last then
            return; -- All data sent
         end if;
      end if;
      declare
         Count : constant Stream_Element_Count :=
                          Data'Last - Pointer + 1;
      begin
         if Pier.Secondary = null then -- Allocate secondary buffer
            if Pier.Max_Size /= 0 and then Pier.Max_Size < Count then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Output buffer overflow, "
                  &  Image (Pier.Max_Size)
                  &  " items of secondary buffer exhausted"
               )  );
            end if;
            Pier.Secondary := new Output_Buffer (Count);
         elsif Pier.Secondary.Size - Pier.Secondary.Last < Count then
            declare -- No place at the buffer end
               Old  : Output_Buffer renames Pier.Secondary.all;
               Used : constant Stream_Element_Count :=
                               Old.Last - Old.First + 1;
            begin
               if Old.Size - Used >= Count then -- Compact the buffer
                  Old.Data (1..Used) := Old.Data (Old.First..Old.Last);
                  Old.Last  := Used;
                  Old.First := 1;
               else -- Allocate larger buffer
                  if (  Pier.Max_Size /= 0
                     and then
                        Pier.Max_Size < Count + Used
                     )
                  then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Output buffer overflow, "
                        &  Image (Pier.Max_Size)
                        &  " items of secondary buffer exhausted"
                     )  );
                  end if;
                  declare
                     Size : constant Stream_Element_Count :=
                                     Stream_Element_Count'Max
                                     (  Count + Used,
                                        Stream_Element_Count'Min
                                        (  Pier.Max_Size,
                                           Old.Size * 2
                                     )  );
                     Ptr  : constant Output_Buffer_Ptr :=
                                     new Output_Buffer (Size);
                  begin
                     Ptr.Data (1..Used) :=
                        Old.Data (Old.First..Old.Last);
                     Ptr.First := 1;
                     Ptr.Last  := Used;
                     Free (Pier.Secondary);
                     Pier.Secondary := Ptr;
                  end;
               end if;
            end;
         end if;
         declare -- Append remaining data to the buffer end
            Buffer : Output_Buffer renames Pier.Secondary.all;
         begin
            Buffer.Data (Buffer.Last + 1..Buffer.Last + Count) :=
               Data (Pointer..Data'Last);
            Buffer.Last := Buffer.Last + Count;
         end;
      end;
   end Send;

   procedure Send_Acknowledge
             (  Pier    : in out MQTT_Pier;
                Request : Acknowledge_Type;
                Packet  : Packet_Identifier
             )  is
      Frame : Stream_Element_Array (1..4);
   begin
      case Request is
         when Publish_Level_1 =>
            Frame (1) := PUBACK_Request;
         when Publish_Level_2_Received =>
            Frame (1) := PUBREC_Request;
         when Publish_Level_2_Release =>
            Frame (1) := PUBREL_Request;
         when Publish_Level_2_Complete =>
            Frame (1) := PUBCOMP_Request;
         when Unsubscribed =>
            Frame (1) := UNSUBACK_Request;
      end case;
      Frame (2) := 2;
      Frame (3) := Stream_Element (Packet / 256);
      Frame (4) := Stream_Element (Packet mod 256);
      Send (MQTT_Pier'Class (Pier), Frame);
   end Send_Acknowledge;

   procedure Send_Connect
             (  Pier         : in out MQTT_Pier;
                Client       : String;
                Clean        : Boolean              := True;
                Will_Topic   : String               := "";
                Will_Message : Stream_Element_Array := (1..0 => 0);
                Will_QoS     : QoS_Level            := At_Most_Once;
                Will_Retain  : Boolean              := False;
                User_Name    : String               := "";
                Password     : String               := "";
                Keep_Alive   : Duration             :=  0.0
             )  is
      Length : Stream_Element_Count;
   begin
      Length := 4 + 4 + Protocol_Name'Length + Client'Length;
      if Will_Topic'Length > 0 then
         Length := Length + 4 + Will_Topic'Length + Will_Message'Length;
      end if;
      if User_Name'Length > 0 then
         Length := Length + 2 + User_Name'Length;
      end if;
      if Password'Length > 0 then
         Length := Length + 2 + Password'Length;
      end if;
      declare
         Data    : Stream_Element_Array (1..Get_Size (Length));
         Pointer : Stream_Element_Offset := 2;
      begin
         Data (1) := CONNECT_Request;
         Put_Length (Data, Pointer, Length);
         Put_String (Data, Pointer, Protocol_Name);
         Data (Pointer) := Protocol_Version;
         Pointer := Pointer + 1;
         declare
            Flags : Stream_Element renames Data (Pointer);
         begin
            Pointer := Pointer + 1;
            Flags := 0;
            begin
               Put (Data, Pointer, Unsigned_16 (Keep_Alive));
            exception
               when Constraint_Error =>
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Duration is too big"
                  );
            end;
            Put_String (Data, Pointer, Client);
            if Clean then
               Flags := Flags + 2#0000_0010#;
            end if;
            if Will_Topic'Length > 0 then
               Flags := Flags + 2#0000_0100#;
               if Will_Retain then
                  Flags := Flags + 2#0010_0000#;
               end if;
               case Will_QoS is
                  when At_Most_Once =>
                     null;
                  when At_Least_Once =>
                     Flags := Flags + 2#0000_1000#;
                  when Exactly_Once =>
                     Flags := Flags + 2#0001_0000#;
               end case;
               Put_String (Data, Pointer, Will_Topic);
               Put_String (Data, Pointer, Will_Message);
            end if;
            if User_Name'Length > 0 then
               Flags := Flags + 2#1000_0000#;
               Put_String (Data, Pointer, User_Name);
            end if;
            if Password'Length > 0 then
               Flags := Flags + 2#0100_0000#;
               Put_String (Data, Pointer, Password);
            end if;
         end;
         Send (MQTT_Pier'Class (Pier), Data);
      end;
   end Send_Connect;

   procedure Send_Connect_Accepted
             (  Pier            : in out MQTT_Pier;
                Session_Present : Boolean := False
             )  is
   begin
      if Session_Present then
         Send (MQTT_Pier'Class (Pier), (CONNACK_Request, 2, 1, 0));
      else
         Send (MQTT_Pier'Class (Pier), (CONNACK_Request, 2, 0, 0));
      end if;
   end Send_Connect_Accepted;

   procedure Send_Connect_Rejected
             (  Pier     : in out MQTT_Pier;
                Response : Connect_Response
             )  is
   begin
      Send
      (  MQTT_Pier'Class (Pier),
         (CONNACK_Request, 2, 0, Stream_Element (Response))
      );
   end Send_Connect_Rejected;

   procedure Send_Disconnect (Pier : in out MQTT_Pier) is
   begin
      Send (MQTT_Pier'Class (Pier), (DISCONNECT_Request, 0));
   end Send_Disconnect;

   procedure Send_Ping (Pier : in out MQTT_Pier) is
   begin
      Send (MQTT_Pier'Class (Pier), (PINGREQ_Request, 0));
   end Send_Ping;

   procedure Send_Ping_Response (Pier : in out MQTT_Pier) is
   begin
      Send (MQTT_Pier'Class (Pier), (PINGRESP_Request, 0));
   end Send_Ping_Response;

   procedure Send_Publish
             (  Pier      : in out MQTT_Pier;
                Topic     : String;
                Message   : Stream_Element_Array;
                Packet    : Packet_Identification;
                Duplicate : Boolean := False;
                Retain    : Boolean := False
             )  is
      Length : Stream_Element_Count :=
               2 + Topic'Length + Message'Length;
   begin
      if Packet.QoS /= At_Most_Once then
         Length := Length + 2;
      end if;
      declare
         Data    : Stream_Element_Array (1..Get_Size (Length));
         Pointer : Stream_Element_Offset := 2;
      begin
         Data (1) := PUBLISH_Request_1;
         if Retain then
            Data (1) := Data (1) + 2#0001#;
         end if;
         case Packet.QoS is
            when At_Most_Once =>
               null;
            when At_Least_Once =>
               Data (1) := Data (1) + 2#0010#;
               if Duplicate then
                  Data (1) := Data (1) + 2#1000#;
               end if;
            when Exactly_Once  =>
               Data (1) := Data (1) + 2#0100#;
               if Duplicate then
                  Data (1) := Data (1) + 2#1000#;
               end if;
         end case;
         Put_Length (Data, Pointer, Length);
         Put_String (Data, Pointer, Topic);
         if Packet.QoS /= At_Most_Once then
            Put (Data, Pointer, Unsigned_16 (Packet.ID));
         end if;
         Data (Pointer..Pointer + Message'Length - 1) := Message;
         Send (MQTT_Pier'Class (Pier), Data);
      end;
   end Send_Publish;

   procedure Send_Publish
             (  Pier      : in out MQTT_Pier;
                Topic     : String;
                Message   : String;
                Packet    : Packet_Identification;
                Duplicate : Boolean := False;
                Retain    : Boolean := False
             )  is
   begin
      Send_Publish
      (  Pier,
         Topic,
         From_String (Message),
         Packet,
         Duplicate,
         Retain
      );
   end Send_Publish;

   procedure Send_Publish
             (  Pier      : in out MQTT_Pier;
                Message   : MQTT_Message'Class;
                Packet    : Packet_Identification;
                Duplicate : Boolean := False;
                Retain    : Boolean := False
             )  is
      This : Message_Object'Class renames Ptr (Message.Reference).all;
   begin
      Send_Publish
      (  Pier,
         This.Topic,
         This.Content (1..This.Count),
         Packet,
         Duplicate,
         Retain
      );
   end Send_Publish;

   procedure Send_Subscribe
             (  Pier   : in out MQTT_Pier;
                Packet : Packet_Identifier;
                Topic  : String;
                QoS    : QoS_Level
             )  is
   begin
      Send_Subscribe (Pier, Packet, +Topic, (1 => QoS));
   end Send_Subscribe;

   procedure Send_Subscribe
             (  Pier   : in out MQTT_Pier;
                Packet : Packet_Identifier;
                Topics : Topics_List;
                QoS    : QoS_Level_Array
             )  is
      Length : Stream_Element_Count := 2;
   begin
      if Topics'Length /= QoS'Length then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Lists of topics and QoS have different length"
         );
      end if;
      for Index in Topics'Range loop
         Length :=
            (  Length
            +  3
            +  Stream_Element_Count (Ptr (Topics (Index)).Length)
            );
      end loop;
      declare
         Data    : Stream_Element_Array (1..Get_Size (Length));
         Pointer : Stream_Element_Offset := 2;
      begin
         Data (1) := SUBSCRIBE_Request;
         Put_Length (Data, Pointer, Length);
         Put (Data, Pointer, Unsigned_16 (Packet));
         for Index in Topics'Range loop
            Put_String (Data, Pointer, Ptr (Topics (Index)).Topic);
            case QoS (Index + QoS'First - Topics'First) is
               when At_Most_Once  => Data (Pointer) := 0;
               when At_Least_Once => Data (Pointer) := 1;
               when Exactly_Once  => Data (Pointer) := 2;
            end case;
            Pointer := Pointer + 1;
         end loop;
         Send (MQTT_Pier'Class (Pier), Data);
      end;
   end Send_Subscribe;

   procedure Send_Subscribe_Acknowledgement
             (  Pier   : in out MQTT_Pier;
                Packet : Packet_Identifier;
                Codes  : Return_Code_List
             )  is
      Length : constant Stream_Element_Count := 2 + Codes'Length;
   begin
      declare
         Data    : Stream_Element_Array (1..Get_Size (Length));
         Pointer : Stream_Element_Offset := 2;
      begin
         Data (1) := SUBACK_Request;
         Put_Length (Data, Pointer, Length);
         Put (Data, Pointer, Unsigned_16 (Packet));
         for Index in Codes'Range loop
            if Codes (Index).Success then
               case Codes (Index).QoS is
                  when At_Most_Once  => Data (Pointer) := 0;
                  when At_Least_Once => Data (Pointer) := 1;
                  when Exactly_Once  => Data (Pointer) := 2;
               end case;
            else
               Data (Pointer) := 16#80#;
            end if;
            Pointer := Pointer + 1;
         end loop;
         Send (MQTT_Pier'Class (Pier), Data);
      end;
   end Send_Subscribe_Acknowledgement;

   procedure Send_Unsubscribe
             (  Pier   : in out MQTT_Pier;
                Packet : Packet_Identifier;
                Topics : Topics_List
             )  is
      Length : Stream_Element_Count := 2;
   begin
      if Get_Length (Topics) = 0 then
         return;
      end if;
      for Index in Topics'Range loop
         Length :=
            (  Length
            +  2
            +  Stream_Element_Count (Ptr (Topics (Index)).Length)
            );
      end loop;
      declare
         Data    : Stream_Element_Array (1..Get_Size (Length));
         Pointer : Stream_Element_Offset := 2;
      begin
         Data (1) := UNSUBSCRIBE_Request;
         Put_Length (Data, Pointer, Length);
         Put (Data, Pointer, Unsigned_16 (Packet));
         for Index in Topics'Range loop
            Put_String (Data, Pointer, Ptr (Topics (Index)).Topic);
         end loop;
         Send (MQTT_Pier'Class (Pier), Data);
      end;
   end Send_Unsubscribe;

   procedure Sent (Pier : in out MQTT_Pier) is
   begin
      if Pier.Secondary /= null then
         declare
            Buffer : Output_Buffer renames Pier.Secondary.all;
         begin
            if Buffer.Last > 0 then
               Send
               (  Pier,
                  Buffer.Data (Buffer.First..Buffer.Last),
                  Buffer.First
               );
               if Buffer.First > Buffer.Last then
                  Buffer.First := 1;
                  Buffer.Last  := 0;
               end if;
            end if;
         end;
      end if;
   end Sent;

   procedure Set_Max_Message_Size
             (  Pier : in out MQTT_Pier;
                Size : Stream_Element_Count
             )  is
      Ptr : constant Stream_Element_Array_Ptr :=
                     new Stream_Element_Array (1..Size);
   begin
      Free (Pier.Data);
      Pier.Data := Ptr;
   end Set_Max_Message_Size;

   procedure Set_Max_Secondary_Buffer_Size
             (  Pier : in out MQTT_Pier;
                Size : Stream_Element_Count := 0
             )  is
   begin
      Pier.Max_Size := Size;
   end Set_Max_Secondary_Buffer_Size;

   procedure Set_Message
             (  Message : in out MQTT_Message;
                Content : Stream_Element_Array
             )  is
      This : Message_Object'Class renames Ptr (Message.Reference).all;
   begin
      if This.Use_Count > 0 or else This.Size >= Content'Size then
         Message := Compose (This.Topic, Content);
      else
         This.Content (1..Content'Length) := Content;
         This.Count := Content'Length;
      end if;
   end Set_Message;

   procedure Set_Message
             (  Message : in out MQTT_Message;
                Content : String
             )  is
   begin
      Set_Message (Message, From_String (Content));
   end Set_Message;

   procedure Set_Size
             (  Handle : in out Messages_Handles.Handle;
                Size   : Stream_Element_Count
             )  is
      That : Message_Object'Class renames Ptr (Handle).all;
   begin
      if That.Size >= Size then
         return;
      end if;
      declare
         This : constant Message_Object_Ptr :=
                         new Message_Object (That.Length, Size);
      begin
         This.Topic := That.Topic;
         This.Count := That.Count;
         This.Content (1..That.Count) := That.Content (1..That.Count);
         Set (Handle, This);
      end;
   end Set_Size;

   procedure Set_Size
             (  Message : in out MQTT_Message;
                Size    : Stream_Element_Count
             )  is
   begin
      Set_Size (Message.Reference, Size);
   end Set_Size;

   procedure Set_Topic
             (  Message : in out MQTT_Message;
                Topic   : String
             )  is
   begin
      if Check_Topic (Topic) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Topic contains wildcards"
         );
      end if;
      declare
         This : Message_Object_Ptr := Ptr (Message.Reference);
      begin
         if This = null then
            This := new Message_Object (Topic'Length, 0);
            This.Topic := Topic;
            Set (Message.Reference, This);
         elsif (  This.Use_Count = 1
               and then
                  This.Length = Topic'Length
               )  then
            This.Topic := Topic;
         else
            declare
               That : constant Message_Object_Ptr :=
                      new Message_Object (Topic'Length, This.Size);
            begin
               That.Topic   := Topic;
               That.Content := This.Content;
               That.Count   := This.Count;
               Set (Message.Reference, That);
            end;
         end if;
      end;
   end Set_Topic;

   function String_Name
            (  Header : Stream_Element;
               Index  : Positive
            )  return String is
   begin
      case Header is
         when CONNECT_Request =>
            case Index is
               when Client_Identifier => return "client identifier";
               when Will_Topic        => return "will topic";
               when Will_Message      => return "will message";
               when User_Name         => return "user name";
               when Password          => return "password";
               when others            => null;
            end case;
         when PUBLISH_Request_1..PUBLISH_Request_2 =>
            case Index is
               when Topic_Name => return "topic name";
               when others     => null;
            end case;
         when SUBSCRIBE_Request =>
            return "Topic " & Image (Index);
         when UNSUBSCRIBE_Request =>
            return "Topic filter " & Image (Index);
         when others =>
            null;
      end case;
      return "#" & Image (Index);
   end String_Name;

   procedure Trace
             (  Pier    : in out MQTT_Pier;
                Message : String
             )  is
   begin
      Trace
      (  Pier.Listener.Factory.all,
         Image (Get_Client_Address (Pier)) & ' ' & Message
      );
   end Trace;

   function "+" (Left, Right : QoS_Level) return QoS_Level is
   begin
      return
         QoS_Level'Val
         (  Integer'Max
            (  QoS_Level'Pos (Left),
               QoS_Level'Pos (Right)
         )  );
   end "+";

   function "+" (Left : String) return Topics_List is
      Object : constant Topic_Item_Data_Ptr :=
                        new Topic_Item_Data (Left'Length);
      Result : Topics_List (1..1);
   begin
      Set (Result (1), Object);
      Object.Topic := Left;
      return Result;
   end "+";

   function "/" (Left : Topics_List; Right : String)
      return Topics_List is
      Object : constant Topic_Item_Data_Ptr :=
                        new Topic_Item_Data (Left'Length);
      Result : Topics_List (1..Left'Length + 1);
   begin
      Set (Result (Left'Length + 1), Object);
      Object.Topic := Right;
      Result (1..Left'Length) := Left;
      return Result;
   end "/";

   function "/" (Left : String; Right : String) return Topics_List is
      Object : Topic_Item_Data_Ptr;
      Result : Topics_List (1..2);
   begin
      Object := new Topic_Item_Data (Left'Length);
      Object.Topic := Left;
      Set (Result (1), Object);
      Object := new Topic_Item_Data (Right'Length);
      Object.Topic := Right;
      Set (Result (2), Object);
      return Result;
   end "/";

end GNAT.Sockets.MQTT;
