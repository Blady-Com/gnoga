------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--   G N O G A . G U I . E L E M E N T . C A N V A S . C O N T E X T _ 2 D  --
--                              . S P R I T E                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2016 Pascal Pignard                    --
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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

--  Sprite package provides API to set some motion properties to an image on a canvas.
--  All positions are in the selected canvas unit.
--  The position of a sprite is the upper left hand corner of the drawn image.
--  Sprites are updated on canvas every specified trigger value in milli-seconds.
--  All motions are in position unit per trigger value.

private with Ada.Containers.Ordered_Maps;

package Gnoga.Gui.Element.Canvas.Context_2D.Sprite is

   -------------------------------------------------------------------------
   --  Sprite_Type
   -------------------------------------------------------------------------

   type Sprite_Type is private;
   type Sprite_Access is access all Sprite_Type;

   -------------------------------------------------------------------------
   --  Sprite_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Sprite                        :    out Sprite_Type;
      Canvas                        : in     Canvas_Access;
      Image_Data                    : in     Image_Data_Type'Class;
      Row, Column                   : in     Integer;
      Row_Velocity, Column_Velocity : in     Integer := 0);
   --  Defines sprite, specifying the image to draw, the position and the motion (optional)

   -------------------------------------------------------------------------
   --  Sprite_Type - Properties
   -------------------------------------------------------------------------

   procedure Locate
     (Sprite      : in Sprite_Type;
      Row, Column : in Integer);
   --  Specifies the position of the upper left hand corner of a sprite

   procedure Position
     (Sprite      : in     Sprite_Type;
      Row, Column :    out Integer);
   function Row
     (Sprite : in Sprite_Type)
      return Integer;
   function Column
     (Sprite : in Sprite_Type)
      return Integer;
   --  Determines the position of a sprite

   procedure Pattern
     (Sprite     : in Sprite_Type;
      Image_Data : in Image_Data_Type'Class);
   --  Specifies the image that defines a sprite

   function Pattern
     (Sprite : in Sprite_Type)
      return Image_Data_Type'Class;
   --  Returns the image that defines a sprite

   procedure Motion
     (Sprite                        : in Sprite_Type;
      Row_Velocity, Column_Velocity : in Integer);
   --  Specifies the motion of a sprite

   function Row_Velocity
     (Sprite : in Sprite_Type)
      return Integer;
   function Column_Velocity
     (Sprite : in Sprite_Type)
      return Integer;
   --   Returns motion properties

   -------------------------------------------------------------------------
   --  Sprite_Type - Methods
   -------------------------------------------------------------------------

   function Coincidence
     (Sprite1, Sprite2 : in Sprite_Type;
      Tolerance        : in Natural)
      return Boolean;
   function Coincidence
     (Sprite      : in Sprite_Type;
      Row, Column : in Integer;
      Tolerance   : in Natural)
      return Boolean;
   --  Determines if two sprites or a sprite and a point on the screen at or near the same location on the screen
   --  Tolerance is in the same unit as position
   --  Tolerance of 0 indicates the exact coincidence

   function Distance
     (Sprite1, Sprite2 : in Sprite_Type)
      return Natural;
   function Distance
     (Sprite      : in Sprite_Type;
      Row, Column : in Integer)
      return Natural;
   --  Determines the distance between two sprites or a sprite and a location
   --  The result is the nearest integer from square root between the upper left hand corner of two sprites
   --  or between the upper left hand corner of a sprite and a location

   procedure Delete (Sprite : in out Sprite_Type);
   procedure Delete_All;
   --  Deletes sprites

   -------------------------------------------------------------------------
   --  Sprite_Type - Engine properties
   -------------------------------------------------------------------------

   procedure Trigger (Value : Positive);
   function Trigger return Positive;
   --  Sprites are updated on canvas every specified trigger value in milli-seconds.

private
   type Sprite_Data is limited record
      Canvas                        : Canvas_Access;
      Row, Column                   : Integer;
      Row_Velocity, Column_Velocity : Integer;
      Saved_Image, Drawn_Image      : Image_Data_Type;
   end record;
   type Sprite_Data_Access is access Sprite_Data;
   type Sprite_Type is new Gnoga.Types.Unique_ID;
   package Sprite_Lists is new Ada.Containers.Ordered_Maps (Sprite_Type, Sprite_Data_Access);
end Gnoga.Gui.Element.Canvas.Context_2D.Sprite;
