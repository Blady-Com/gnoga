------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . G U I . L O C A T I O N                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Gnoga.Types;
with Gnoga.Gui.Base;

package Gnoga.Gui.Location is

   -------------------------------------------------------------------------
   --  Location_Type
   -------------------------------------------------------------------------
   --  Location_Type is the class encapsulating the DOM Location node

   type Location_Type is new Gnoga.Gui.Base.Base_Type with private;
   type Location_Access is access all Location_Type;
   type Pointer_To_Location_Class is access all Location_Type'Class;

   -------------------------------------------------------------------------
   --  Location_Type - Properties
   -------------------------------------------------------------------------

   procedure URL (Location : in out Location_Type; Value : String);
   function URL (Location : Location_Type) return String;
   --  Setting URL will navgigate the browser from the current location and
   --  close the current Gnoga Connection.

   procedure Hash (Location : in out Location_Type; Value : String);
   function Hash (Location : Location_Type) return String;

   procedure Host (Location : in out Location_Type; Value : String);
   function Host (Location : Location_Type) return String;

   procedure Host_Name (Location : in out Location_Type; Value : String);
   function Host_Name (Location : Location_Type) return String;

   function Origin (Location : Location_Type) return String;

   procedure Path_Name (Location : in out Location_Type; Value : String);
   function Path_Name (Location : Location_Type) return String;

   procedure Port (Location : in out Location_Type; Value : String);
   function Port (Location : Location_Type) return String;

   procedure Protocol (Location : in out Location_Type; Value : String);
   function Protocol (Location : Location_Type) return String;

   procedure Search (Location : in out Location_Type; Value : String);
   function Search (Location : Location_Type) return String;
private
   type Location_Type is new Gnoga.Gui.Base.Base_Type with null record;
end Gnoga.Gui.Location;
