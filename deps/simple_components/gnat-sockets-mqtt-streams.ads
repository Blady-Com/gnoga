--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.MQTT.Streams                   Luebeck            --
--  Interface                                      Spring, 2016       --
--                                                                    --
--                                Last revision :  20:01 04 Apr 2016  --
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

package GNAT.Sockets.MQTT.Streams is
--
-- MQTT_Stream -- Stream interface to MQTT_Message
--
--    Message - The message to access
--
   type MQTT_Stream (Message : access MQTT_Message'Class) is
      new Root_Stream_Type with private;
--
-- Erase -- Start writing the message
--
--    Stream - The stream object
--
-- The message contents is erased.  The first  written  element  is  the
-- first element of the message contents.
--
-- Exceptions :
--
--    Constraint_Error - Message is invalid
--
   procedure Erase (Stream : in out MQTT_Stream);
--
-- Read -- Overriding Ada.Streams...
--
   procedure Read
             (  Stream : in out MQTT_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
--
-- Rewind -- Start reading the message
--
--    Stream - The stream object
--
-- The first read element is the first element of the message contents.
--
-- Exceptions :
--
--    Constraint_Error - Message is invalid
--
   procedure Rewind (Stream : in out MQTT_Stream);
--
-- Write -- Overriding Ada.Streams...
--
   procedure Write
             (  Stream : in out MQTT_Stream;
                Item   : Stream_Element_Array
             );
private
   type MQTT_Stream (Message : access MQTT_Message'Class) is
      new Root_Stream_Type with
   record
      Position : Stream_Element_Offset := 0;
   end record;

end GNAT.Sockets.MQTT.Streams;
