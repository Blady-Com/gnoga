--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.MQTT.Streams                   Luebeck            --
--  Implementation                                 Spring, 2016       --
--                                                                    --
--                                Last revision :  23:08 18 Nov 2019  --
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

with Ada.Exceptions;  use Ada.Exceptions;

package body GNAT.Sockets.MQTT.Streams is

   procedure Erase (Stream : in out MQTT_Stream) is
   begin
      if not Is_Valid (Stream.Message.Reference) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid message attached to the output stream"
         );
      end if;
      Stream.Position := 1;
      Ptr (Stream.Message.Reference).Count := 0;
   end Erase;

   procedure Read
             (  Stream : in out MQTT_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
      This : constant Message_Object_Ptr :=
                      Ptr (Stream.Message.Reference);
   begin
      if This = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "In uninitialized message being read"
         );
      end if;
      declare
         Object : Message_Object'Class renames This.all;
      begin
         if Stream.Position > Object.Count then
            Last := Item'First - 1;
         else
            declare
               Length : constant Stream_Element_Offset :=
                        Stream_Element_Offset'Min
                        (  Object.Count - Stream.Position + 1,
                           Item'Length
                        );
            begin
               Item (Item'First..Item'First + Length - 1) :=
                  Object.Content
                  (  Stream.Position
                  .. Stream.Position + Length - 1
                  );
               Last := Stream.Position + Length;
               Stream.Position := Stream.Position + Length;
            end;
         end if;
      end;
   end Read;

   procedure Rewind (Stream : in out MQTT_Stream) is
   begin
      if not Is_Valid (Stream.Message.Reference) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid message attached to the input stream"
         );
      end if;
      Stream.Position := 1;
   end Rewind;

   procedure Write
             (  Stream : in out MQTT_Stream;
                Item   : Stream_Element_Array
             )  is
      This : Message_Object_Ptr := Ptr (Stream.Message.Reference);
   begin
      if This = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "An uninitialized message being written"
         );
      end if;
      if This.Size - This.Count < Item'Length then
         Set_Size
         (  Stream.Message.Reference,
            Stream_Element_Count'Max
            (  This.Count + Item'Length,
               This.Size  + This.Size / 2
         )  );
         This := Ptr (Stream.Message.Reference);
      end if;
      This.Content (This.Count + 1..This.Count + Item'Length) := Item;
      This.Count := This.Count + Item'Length;
   end Write;

end GNAT.Sockets.MQTT.Streams;
