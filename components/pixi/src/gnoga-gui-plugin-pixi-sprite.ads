------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--          G N O G A . G U I . P L U G I N . P I X I . S P R I T E         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2017 Pascal Pignard                    --
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

--  Sprite package provides API to set some motion properties to an image on a canvas.
--  All positions are in the selected canvas unit.
--  The position of a sprite is set by the anchor position.
--  Sprites are updated on canvas at 60 FPS in most browsers.

with Gnoga.Types;

package Gnoga.Gui.Plugin.Pixi.Sprite is

   -------------------------------------------------------------------------
   --  Sprite_Types
   -------------------------------------------------------------------------

   type Sprite_Type is new Container_Type with private;
   type Sprite_Access is access all Sprite_Type;
   type Pointer_To_Sprite_Class is access all Sprite_Type'Class;

   type Effect_Type is (Null_Effect, Bounce_Effect, Loop_Effect);

   -------------------------------------------------------------------------
   --  Sprite_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Sprite                                : in out Sprite_Type;
      Parent                                : in out Container_Type'Class;
      Texture                               : in     Texture_Type;
      Row, Column                           : in     Integer;
      Row_Velocity, Column_Velocity         : in     Velocity_Type     := 0.0;
      Row_Acceleration, Column_Acceleration : in     Acceleration_Type := 0.0);
   procedure Create
     (Sprite                                : in out Sprite_Type;
      Parent                                : in out Container_Type'Class;
      Image_Path                            : in     String;
      Row, Column                           : in     Integer;
      Row_Velocity, Column_Velocity         : in     Velocity_Type     := 0.0;
      Row_Acceleration, Column_Acceleration : in     Acceleration_Type := 0.0);
   --  Defines sprite, specifying the image to draw, the position, the motion (optionnal)
   --  and the acceleration (optional)
   --  Path is defined from application root path

   -------------------------------------------------------------------------
   --  Sprite_Type - Properties
   -------------------------------------------------------------------------

   procedure Locate (Sprite : in out Sprite_Type; Row, Column : in Integer);
   --  Specifies the position of the upper left hand corner of a sprite

   procedure Position (Sprite : in Sprite_Type; Row, Column : out Integer);
   function Row (Sprite : in Sprite_Type) return Integer;
   function Column (Sprite : in Sprite_Type) return Integer;
   --  Determines the position of a sprite

   procedure Pattern (Sprite : in Sprite_Type; Image_Data : in String);
   --  Specifies the image that defines a sprite

   function Pattern (Sprite : in Sprite_Type) return String;
   --  Returns the image that defines a sprite

   procedure Motion
     (Sprite                        : in out Sprite_Type;
      Row_Velocity, Column_Velocity : in     Velocity_Type);
   --  Specifies the motion of a sprite in cartesian coordinate

   function Row_Velocity (Sprite : in Sprite_Type) return Velocity_Type;
   function Column_Velocity (Sprite : in Sprite_Type) return Velocity_Type;
   --  Returns cartesian motion properties

   procedure Motion
     (Sprite              : in out Sprite_Type;
      Radial_Velocity     : in     Velocity_Type;
      Azimuth_Of_Velocity : in     Integer);
   procedure Radial_Velocity
     (Sprite : in out Sprite_Type;
      Value  : in     Velocity_Type);
   procedure Azimuth_Of_Velocity
     (Sprite : in out Sprite_Type;
      Value  : in     Integer);
   --  Specifies the motion of a sprite in polar coordinate

   procedure Orient_Velocity
     (Sprite            : in out Sprite_Type;
      To_Row, To_Column : in     Integer);
   --  Points the velocity vector to the specified point

   function Radial_Velocity (Sprite : in Sprite_Type) return Velocity_Type;
   function Azimuth_Of_Velocity (Sprite : in Sprite_Type) return Integer;
   --  Returns polar motion properties

   procedure Acceleration
     (Sprite                                : in out Sprite_Type;
      Row_Acceleration, Column_Acceleration : in     Acceleration_Type);
   --  Specifies the acceleration of a sprite in cartesian coordinate

   function Row_Acceleration
     (Sprite : in Sprite_Type) return Acceleration_Type;
   function Column_Acceleration
     (Sprite : in Sprite_Type) return Acceleration_Type;
   --  Returns cartesian acceleration properties

   procedure Acceleration
     (Sprite                  : in out Sprite_Type;
      Radial_Acceleration     : in     Acceleration_Type;
      Azimuth_Of_Acceleration : in     Integer);
   procedure Radial_Acceleration
     (Sprite : in out Sprite_Type;
      Value  : in     Acceleration_Type);
   procedure Azimuth_Of_Acceleration
     (Sprite : in out Sprite_Type;
      Value  : in     Integer);
   --  Specifies the acceleration of a sprite in polar coordinate

   procedure Orient_Acceleration
     (Sprite            : in out Sprite_Type;
      To_Row, To_Column : in     Integer);
   --  Points the acceleration vector to the specified point

   function Radial_Acceleration
     (Sprite : in Sprite_Type) return Acceleration_Type;
   function Azimuth_Of_Acceleration (Sprite : in Sprite_Type) return Integer;
   --  Returns polar acceleration properties

   procedure Alpha
     (Sprite : in out Sprite_Type;
      Value  : in     Gnoga.Types.Alpha_Type);
   function Alpha (Sprite : in Sprite_Type) return Gnoga.Types.Alpha_Type;
   --  The opacity of the object.

   procedure Anchor
     (Sprite      : in out Sprite_Type;
      Row, Column : in     Gnoga.Types.Frational_Range_Type);
   function Row_Anchor
     (Sprite : in Sprite_Type) return Gnoga.Types.Frational_Range_Type;
   function Column_Anchor
     (Sprite : in Sprite_Type) return Gnoga.Types.Frational_Range_Type;
   --  The anchor sets the origin point of the texture.
   --  The default is 0,0 this means the texture's origin is the top left
   --  Setting the anchor to 0.5,0.5 means the texture's origin is centered
   --  Setting the anchor to 1,1 would mean the texture's origin point will be the bottom right corner

   procedure Blend_Mode
     (Sprite : in out Sprite_Type;
      Value  : in     Blend_Modes_Type);
   function Blend_Mode (Sprite : in Sprite_Type) return Blend_Modes_Type;
   --  The blend mode to be applied to the sprite. Apply a value of PIXI.BLEND_MODES.NORMAL to reset the blend mode.

   --  mask PIXI.Graphics PIXI.Sprite
   --  Sets a mask for the displayObject. A mask is an object that limits the visibility of an
   --  object to the shape of the mask applied to it. In PIXI a regular mask must be a
   --  PIXI.Graphics or a PIXI.Sprite object. This allows for much faster masking in canvas as it
   --  utilises shape clipping. To remove a mask, set this property to null.

   procedure Pivot (Sprite : in out Sprite_Type; Row, Column : in Integer);
   function Row_Pivot (Sprite : in Sprite_Type) return Integer;
   function Column_Pivot (Sprite : in Sprite_Type) return Integer;
   --  The pivot point of the displayObject that it rotates around

   procedure Rotation (Sprite : in out Sprite_Type; Value : in Integer);
   function Rotation (Sprite : in Sprite_Type) return Integer;
   --  The rotation of the object in degrees.

   procedure Rotation_Velocity
     (Sprite : in out Sprite_Type;
      Value  :        Velocity_Type);
   function Rotation_Velocity (Sprite : in Sprite_Type) return Velocity_Type;
   --  The rotation velocity

   procedure Rotation_Acceleration
     (Sprite : in out Sprite_Type;
      Value  :        Acceleration_Type);
   function Rotation_Acceleration
     (Sprite : in Sprite_Type) return Acceleration_Type;
   --  The rotation acceleration

   overriding procedure Width
     (Sprite : in out Sprite_Type;
      Value  : in     Integer);
   overriding function Width (Sprite : in Sprite_Type) return Integer;
   --  The width of the sprite, setting this will actually modify the scale to achieve the value set.

   overriding procedure Height
     (Sprite : in out Sprite_Type;
      Value  : in     Integer);
   overriding function Height (Sprite : in Sprite_Type) return Integer;
   --  The height of the sprite, setting this will actually modify the scale to achieve the value set.

   procedure Scale (Sprite : in out Sprite_Type; Row, Column : in Positive);
   function Row_Scale (Sprite : in Sprite_Type) return Positive;
   function Column_Scale (Sprite : in Sprite_Type) return Positive;
   --  The scale factor of the object.

   procedure Skew (Sprite : in out Sprite_Type; Row, Column : in Positive);
   function Row_Skew (Sprite : in Sprite_Type) return Positive;
   function Column_Skew (Sprite : in Sprite_Type) return Positive;
   --  The skew factor for the object in radians.

   procedure Get_Texture
     (Sprite : in     Sprite_Type;
      Value  : in out Texture_Type'Class);
   procedure Put_Texture
     (Sprite : in out Sprite_Type;
      Value  : in     Texture_Type'Class);
   --  The texture that the sprite is using

   procedure Tint (Sprite : in out Sprite_Type; Value : in Natural);
   function Tint (Sprite : in Sprite_Type) return Natural;
   --  - tint number
   --  The tint applied to the sprite. This is a hex value. A value of
   --  0xFFFFFF will remove any tint effect.

   procedure Visible (Sprite : in out Sprite_Type; Value : in Boolean);
   function Visible (Sprite : in Sprite_Type) return Boolean;
   --  The visibility of the object. If false the object will not be drawn, and
   --  the updateTransform function will not be called.
   --  Only affects recursive calls from parent. You can ask for bounds or call updateTransform manually

   -------------------------------------------------------------------------
   --  Sprite_Type - Methods
   -------------------------------------------------------------------------

   function Coincidence
     (Sprite1, Sprite2 : in Sprite_Type;
      Tolerance        : in Natural) return Boolean;
   function Coincidence
     (Sprite      : in Sprite_Type;
      Row, Column : in Integer;
      Tolerance   : in Natural) return Boolean;
   --  Determines if two sprites or a sprite and a point on the screen at or near the same location on the screen
   --  Tolerance is in the same unit as position
   --  Tolerance of 0 indicates the exact coincdence

   function Overlap_Point
     (Sprite      : in Sprite_Type;
      Row, Column : in Integer) return Boolean;
   --  Retuns if specified point is within the sprite surface

   function Distance (Sprite1, Sprite2 : in Sprite_Type) return Natural;
   function Distance
     (Sprite      : in Sprite_Type;
      Row, Column : in Integer) return Natural;
   --  Determines the distance between two sprites or a sprite and a location
   --  The result is the nearest integer from square root between the upper left hand corner of two sprites
   --  or between the upper left hand corner of a sprite and a location

   procedure Move_To
     (Sprite              : in out Sprite_Type;
      Row, Column         : in     Integer;
      Radial_Velocity     : in     Velocity_Type;
      Radial_Acceleration : in     Acceleration_Type;
      Spent_Time          :    out Duration);
   --  Sets sprite motion in order to reach specified point on a straight line then stop.
   --  Based on specified target point, velocity and acceleration, actual velocity and acceleration are set
   --  during a calculated spent time. Whatever position, velocity or acceleration are changed during this time,
   --  velocity and acceleration are set to zero at the end of time. Thus the sprite may not have reached
   --  the target point. If the time cannot be calculated then Constraint_Error is raised.

   procedure Move_Rel
     (Sprite              : in out Sprite_Type;
      Rel_Row, Rel_Column : in     Integer;
      Velocity            : in     Velocity_Type;
      Acceleration        : in     Acceleration_Type;
      Spent_Time          :    out Duration);
   --  Sets sprite motion in order to reach specified relative point then stop.
   --  See Move_to for details.

   procedure Move_Rel
     (Sprite       : in out Sprite_Type;
      Distance     : in     Integer;
      Velocity     : in     Velocity_Type;
      Acceleration : in     Acceleration_Type;
      Spent_Time   :    out Duration);
   --  Sets sprite motion in order to reach specified distance in the current direction then stop.
   --  See Move_to for details.

   procedure Rotate_To
     (Sprite       : in out Sprite_Type;
      Angle        : in     Integer;
      Velocity     : in     Velocity_Type;
      Acceleration : in     Acceleration_Type;
      Spent_Time   :    out Duration);
   --  Sets sprite rotation in order to reach specified angle then stop.
   --  See Move_to for details.

   procedure Rotate_Rel
     (Sprite       : in out Sprite_Type;
      Rel_Angle    : in     Integer;
      Velocity     : in     Velocity_Type;
      Acceleration : in     Acceleration_Type;
      Spent_Time   :    out Duration);
   --  Sets sprite rotation in order to reach specified relative angle then stop.
   --  See Move_to for details.

   procedure Frame_Limit
     (Sprite                                   : in out Sprite_Type;
      Row_Min, Row_Max, Column_Min, Column_Max :        Integer;
      Effect                                   :        Effect_Type);
   --  Sets sprite frame limit with special effect

   procedure Delete
     (Sprite : in out Sprite_Type;
      Parent : in out Container_Type'Class);
   procedure Delete_All (Parent : in out Container_Type'Class);
   --  Deletes sprites

private
   type Sprite_Type is new Container_Type with null record;
end Gnoga.Gui.Plugin.Pixi.Sprite;
