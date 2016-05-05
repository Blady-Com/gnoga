--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ELV_MAX_Cube_Client.Stream_IO               Summer, 2015       --
--  Interface                                                         --
--                                Last revision :  22:45 07 Apr 2016  --
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

package GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.
        Stream_IO is
   use Interfaces;

   subtype Device_Duration is Duration range 0.0..300.0;
   type Set_Point_Packed is new Unsigned_16;

   function Pack (Point : Set_Point) return Set_Point_Packed;

   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Centigrade;
   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Device_Type;
   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Device_Duration;
   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Day_Schedule;
   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Device_Parameters;
   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Ratio;
   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return RF_Address;
   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Room_ID;
   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Week_Schedule;
   function Read
            (  Stream : access Root_Stream_Type'Class
            )  return Week_Time;

   function Unpack (Point : Set_Point_Packed) return Set_Point;

   procedure Write
             (  Stream      : access Root_Stream_Type'Class;
                Temperature : Centigrade
             );
   procedure Write
             (  Stream  : access Root_Stream_Type'Class;
                Kind_Of : Device_Type
             );
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Value  : Device_Duration
             );
   procedure Write
             (  Stream   : access Root_Stream_Type'Class;
                Schedule : Day_Schedule
             );
   procedure Write
             (  Stream     : access Root_Stream_Type'Class;
                Parameters : Device_Parameters
             );
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Value  : Ratio
             );
   procedure Write
             (  Stream  : access Root_Stream_Type'Class;
                Address : RF_Address
             );
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Room   : Room_ID
             );
   procedure Write
             (  Stream   : access Root_Stream_Type'Class;
                Schedule : Week_Schedule
             );
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Time   : Week_Time
             );

end GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client.Stream_IO;
