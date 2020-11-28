------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--            G N O G A . G U I . P L U G I N . P I X I . T E X T           --
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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

--  Text package provides API to set some motion properties to an image on a canvas.
--  All positions are in the selected canvas unit.
--  The position of a Text is set by the anchor position.
--  Texts are updated on canvas at 60 FPS in most browsers.

with Gnoga.Types;

package Gnoga.Gui.Plugin.Pixi.Text is

   -------------------------------------------------------------------------
   --  Text_Types
   -------------------------------------------------------------------------

   type Text_Type is new Container_Type with private;
   type Text_Access is access all Text_Type;
   type Pointer_To_Text_Class is access all Text_Type'Class;

   -------------------------------------------------------------------------
   --  Text_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Text        : in out Text_Type;
      Parent      : in out Container_Type'Class;
      Message     : in     String;
      Row, Column : in     Integer);
   --  Defines Text, specifying the message to draw and the position

   -------------------------------------------------------------------------
   --  Text_Type - Properties
   -------------------------------------------------------------------------

   procedure Locate
     (Text        : in out Text_Type;
      Row, Column : in     Integer);
   --  Specifies the position of the upper left hand corner of a Text

   procedure Position
     (Text        : in     Text_Type;
      Row, Column :    out Integer);
   function Row
     (Text : in Text_Type)
      return Integer;
   function Column
     (Text : in Text_Type)
      return Integer;
   --  Determines the position of a Text

   procedure Motion
     (Text                          : in out Text_Type;
      Row_Velocity, Column_Velocity : in     Velocity_Type);
   --  Specifies the motion of a Text

   function Row_Velocity
     (Text : in Text_Type)
      return Velocity_Type;
   function Column_Velocity
     (Text : in Text_Type)
      return Velocity_Type;
   --   Returns motion properties

   procedure Acceleration
     (Text                                  : in out Text_Type;
      Row_Acceleration, Column_Acceleration : in     Acceleration_Type);
   --  Specifies the acceleration of a text

   function Row_Acceleration
     (Text : in Text_Type)
      return Acceleration_Type;
   function Column_Acceleration
     (Text : in Text_Type)
      return Acceleration_Type;
   --  Returns acceleration properties

   procedure Alpha
     (Text  : in out Text_Type;
      Value : in     Gnoga.Types.Alpha_Type);
   function Alpha
     (Text : in Text_Type)
      return Gnoga.Types.Alpha_Type;
   --  The opacity of the object.

   procedure Anchor
     (Text        : in out Text_Type;
      Row, Column : in     Gnoga.Types.Frational_Range_Type);
   function Row_Anchor
     (Text : in Text_Type)
      return Gnoga.Types.Frational_Range_Type;
   function Column_Anchor
     (Text : in Text_Type)
      return Gnoga.Types.Frational_Range_Type;
   --  The anchor sets the origin point of the texture.
   --  The default is 0,0 this means the texture's origin is the top left
   --  Setting the anchor to 0.5,0.5 means the texture's origin is centered
   --  Setting the anchor to 1,1 would mean the texture's origin point will be the bottom right corner

   procedure Blend_Mode
     (Text  : in out Text_Type;
      Value : in     Blend_Modes_Type);
   function Blend_Mode
     (Text : in Text_Type)
      return Blend_Modes_Type;
   --  The blend mode to be applied to the Text. Apply a value of PIXI.BLEND_MODES.NORMAL to reset the blend mode.

   procedure Message
     (Text  : in out Text_Type;
      Value : in     String);
   function Message
     (Text : in Text_Type)
      return String;
   --  The message of the Text.

   --  mask PIXI.Graphics PIXI.Text
   --  Sets a mask for the displayObject. A mask is an object that limits the visibility of an
   --  object to the shape of the mask applied to it. In PIXI a regular mask must be a
   --  PIXI.Graphics or a PIXI.Text object. This allows for much faster masking in canvas as it
   --  utilises shape clipping. To remove a mask, set this property to null.

   procedure Pivot
     (Text        : in out Text_Type;
      Row, Column : in     Integer);
   function Row_Pivot
     (Text : in Text_Type)
      return Integer;
   function Column_Pivot
     (Text : in Text_Type)
      return Integer;
   --  The pivot point of the displayObject that it rotates around

   procedure Rotation
     (Text  : in out Text_Type;
      Value : in     Integer);
   function Rotation
     (Text : in Text_Type)
      return Integer;
   --  The rotation of the object in degrees.

   procedure Rotation_Velocity
     (Text  : in out Text_Type;
      Value :        Velocity_Type);
   function Rotation_Velocity
     (Text : in Text_Type)
      return Velocity_Type;
   --  The rotation velocity

   procedure Rotation_Acceleration
     (Text  : in out Text_Type;
      Value :        Acceleration_Type);
   function Rotation_Acceleration
     (Text : in Text_Type)
      return Acceleration_Type;
   --  The rotation acceleration

   overriding procedure Width
     (Text  : in out Text_Type;
      Value : in     Integer);
   overriding function Width
     (Text : in Text_Type)
      return Integer;
   --  The width of the Text, setting this will actually modify the scale to achieve the value set.

   overriding procedure Height
     (Text  : in out Text_Type;
      Value : in     Integer);
   overriding function Height
     (Text : in Text_Type)
      return Integer;
   --  The height of the Text, setting this will actually modify the scale to achieve the value set.

   procedure Scale
     (Text        : in out Text_Type;
      Row, Column : in     Positive);
   function Row_Scale
     (Text : in Text_Type)
      return Positive;
   function Column_Scale
     (Text : in Text_Type)
      return Positive;
   --  The scale factor of the object.

   procedure Skew
     (Text        : in out Text_Type;
      Row, Column : in     Positive);
   function Row_Skew
     (Text : in Text_Type)
      return Positive;
   function Column_Skew
     (Text : in Text_Type)
      return Positive;
   --  The skew factor for the object in radians.

   procedure Set_Style
     (Text  : in out Text_Type;
      Value : in     Style_Type'Class);
   procedure Get_Style
     (Text  : in     Text_Type;
      Value : in out Style_Type'Class);
   --  style object PIXI.TextStyle
   --  Set the style of the text.

   procedure Tint
     (Text  : in out Text_Type;
      Value : in     Natural);
   function Tint
     (Text : in Text_Type)
      return Natural;
   --  tint number
   --  The tint applied to the Text. This is a hex value. A value of
   --  0xFFFFFF will remove any tint effect.

   procedure Visible
     (Text  : in out Text_Type;
      Value : in     Boolean);
   function Visible
     (Text : in Text_Type)
      return Boolean;
   --  The visibility of the object. If false the object will not be drawn, and
   --  the updateTransform function will not be called.
   --  Only affects recursive calls from parent. You can ask for bounds or call updateTransform manually

   -------------------------------------------------------------------------
   --  Text_Type - Methods
   -------------------------------------------------------------------------

   function Coincidence
     (Text1, Text2 : in Text_Type;
      Tolerance    : in Natural)
      return Boolean;
   function Coincidence
     (Text        : in Text_Type;
      Row, Column : in Integer;
      Tolerance   : in Natural)
      return Boolean;
   --  Determines if two Texts or a Text and a point on the screen at or near the same location on the screen
   --  Tolerance is in the same unit as position
   --  Tolerance of 0 indicates the exact coincidence

   function Distance
     (Text1, Text2 : in Text_Type)
      return Natural;
   function Distance
     (Text        : in Text_Type;
      Row, Column : in Integer)
      return Natural;
   --  Determines the distance between two Texts or a Text and a location
   --  The result is the nearest integer from square root between the upper left hand corner of two Texts
   --  or between the upper left hand corner of a Text and a location

   procedure Delete
     (Text   : in out Text_Type;
      Parent : in out Container_Type'Class);
   procedure Delete_All (Parent : in out Container_Type'Class);
   --  Deletes Texts

private
   type Text_Type is new Container_Type with null record;
end Gnoga.Gui.Plugin.Pixi.Text;
