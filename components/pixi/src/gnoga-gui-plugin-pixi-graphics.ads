------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--       G N O G A . G U I . P L U G I N . P I X I . G R A P H I C S        --
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

with Gnoga.Types;
with Gnoga.Types.Colors;

package Gnoga.Gui.Plugin.Pixi.Graphics is

   -------------------------------------------------------------------------
   --  Graphics_Types
   -------------------------------------------------------------------------

   type Graphics_Type is new Container_Type with private;
   type Graphics_Access is access all Graphics_Type;
   type Pointer_To_Graphics_Class is access all Graphics_Type'Class;

   -------------------------------------------------------------------------
   --  Graphics_Type - Creation Methods
   -------------------------------------------------------------------------

   overriding procedure Create
     (Graphics : in out Graphics_Type;
      Parent   : in out Container_Type'Class);

   -------------------------------------------------------------------------
   --  Graphics_Type - Properties
   -------------------------------------------------------------------------

   procedure Alpha
     (Graphics : in out Graphics_Type;
      Value    : in     Gnoga.Types.Alpha_Type);
   function Alpha (Graphics : in Graphics_Type) return Gnoga.Types.Alpha_Type;
   --  The opacity of the object.

   procedure Blend_Mode
     (Graphics : in out Graphics_Type;
      Value    : in     Blend_Modes_Type);
   function Blend_Mode (Graphics : in Graphics_Type) return Blend_Modes_Type;
   --  The blend mode to be applied to the graphic shape.
   --  Apply a value of PIXI.BLEND_MODES.NORMAL to reset the blend mode.

   procedure Bounds_Padding
     (Graphics : in out Graphics_Type;
      Value    : in     Integer);
   pragma Obsolescent ("Bounds_Padding no more available.");
   function Bounds_Padding (Graphics : in Graphics_Type) return Integer;
   pragma Obsolescent ("Bounds_Padding no more available.");
   --  The bounds' padding used for bounds calculation.

   procedure Fill_Alpha
     (Graphics : in out Graphics_Type;
      Value    : in     Gnoga.Types.Alpha_Type);
   pragma Obsolescent ("Fill_Alpha no more available.");
   function Fill_Alpha
     (Graphics : in Graphics_Type) return Gnoga.Types.Alpha_Type;
   --  The alpha value used when filling the Graphics object.

   overriding procedure Width
     (Graphics : in out Graphics_Type;
      Value    : in     Integer);
   overriding function Width (Graphics : in Graphics_Type) return Integer;
   --  The width of the Text, setting this will actually modify the scale to achieve the value set.

   overriding procedure Height
     (Graphics : in out Graphics_Type;
      Value    : in     Integer);
   overriding function Height (Graphics : in Graphics_Type) return Integer;
   --  The height of the Text, setting this will actually modify the scale to achieve the value set.

   procedure Line_Color
     (Graphics : in out Graphics_Type;
      Value    : in     Gnoga.Types.Colors.Color_Enumeration);
   procedure Line_Color
     (Graphics : in out Graphics_Type;
      Value    : in     Gnoga.Types.RGBA_Type);
   function Line_Color
     (Graphics : in out Graphics_Type) return Gnoga.Types.RGBA_Type;
   --  The color of any lines drawn.

   procedure Line_Width (Graphics : in out Graphics_Type; Value : in Integer);
   function Line_Width (Graphics : in Graphics_Type) return Integer;
   --  The width (thickness) of any lines drawn.

   procedure Position (Graphics : in Graphics_Type; Row, Column : out Integer);
   function Row (Graphics : in Graphics_Type) return Integer;
   function Column (Graphics : in Graphics_Type) return Integer;
   --  The coordinate of the object relative to the local coordinates of the parent.

   procedure Rotation (Graphics : in out Graphics_Type; Value : in Integer);
   function Rotation (Graphics : in Graphics_Type) return Integer;
   --  The rotation of the object in degrees.

   procedure Scale
     (Graphics    : in out Graphics_Type;
      Row, Column : in     Positive);
   function Row_Scale (Graphics : in Graphics_Type) return Positive;
   function Column_Scale (Graphics : in Graphics_Type) return Positive;
   --  The scale factor of the object.

   procedure Skew (Graphics : in out Graphics_Type; Row, Column : in Positive);
   function Row_Skew (Graphics : in Graphics_Type) return Positive;
   function Column_Skew (Graphics : in Graphics_Type) return Positive;
   --  The skew factor for the object in radians.

   procedure Tint (Graphics : in out Graphics_Type; Value : in Natural);
   function Tint (Graphics : in Graphics_Type) return Natural;
   --  The tint applied to the graphic shape.
   --  This is a hex value. Apply a value of 0xFFFFFF to reset the tint.

   procedure Visible (Graphics : in out Graphics_Type; Value : in Boolean);
   function Visible (Graphics : in Graphics_Type) return Boolean;
   --  The visibility of the object. If false the object will not be drawn, and
   --  the updateTransform function will not be called.
   --  Only affects recursive calls from parent. You can ask for bounds or call updateTransform manually

   -------------------------------------------------------------------------
   --  Graphics_Type - Methods
   -------------------------------------------------------------------------

   procedure Add_Hole (Graphics : in out Graphics_Type);
   pragma Obsolescent ("Add_Hole no more available.");
   --  Adds a hole in the current path.

   procedure Arc_Radians
     (Graphics                     : in out Graphics_Type;
      X, Y                         : in     Integer;
      Radius                       : in     Integer;
      Starting_Angle, Ending_Angle : in     Float;
      Counter_Clockwise            : in     Boolean := False);

   procedure Arc_Degrees
     (Graphics                     : in out Graphics_Type;
      X, Y                         : in     Integer;
      Radius                       : in     Integer;
      Starting_Angle, Ending_Angle : in     Float;
      Counter_Clockwise            : in     Boolean := False);

   procedure Arc
     (Graphics                     : in out Graphics_Type;
      X, Y                         : in     Integer;
      Radius                       : in     Integer;
      Starting_Angle, Ending_Angle : in     Float;
      Counter_Clockwise            : in     Boolean := False) renames
     Arc_Radians;
   --  The arc method creates an arc/curve (used to create circles, or parts of circles).
   --  Angles in radians.

   procedure Arc_To
     (Graphics           : in out Graphics_Type;
      X_1, Y_1, X_2, Y_2 : in     Integer;
      Radius             : in     Integer);
   --  The arcTo() method creates an arc/curve between two tangents on the canvas.

   procedure Begin_Fill
     (Graphics : in out Graphics_Type;
      Color    : in     Gnoga.Types.Colors.Color_Enumeration :=
        Gnoga.Types.Colors.Black;
      Alpha    : in     Gnoga.Types.Alpha_Type := 1.0);
   procedure Begin_Fill
     (Graphics : in out Graphics_Type;
      Color    : in     Gnoga.Types.RGBA_Type;
      Alpha    : in     Gnoga.Types.Alpha_Type := 1.0);
   --  Specifies a simple one-color fill that subsequent calls to other Graphics methods
   --  (such as lineTo() or drawCircle()) use when drawing.

   procedure End_Fill (Graphics : in out Graphics_Type);
   --  Fills the current drawing path

   procedure Bezier_Curve_To
     (Graphics                       : in out Graphics_Type;
      CP_X_1, CP_Y_1, CP_X_2, CP_Y_2 : in     Integer;
      X, Y                           : in     Integer);
   --  Calculate the points for a Bézier curve and then draws it.

   procedure Clear (Graphics : in out Graphics_Type);
   --  Clears the graphics that were drawn to this Graphics object, and resets fill and line style settings.

   procedure Close_Path (Graphics : in out Graphics_Type);
   --  Closes the current path.

   function Contains_Point
     (Graphics    : in Graphics_Type;
      Row, Column : in Integer) return Boolean;
   --  Tests if a point is inside this graphics object

   procedure Draw_Circle
     (Graphics : in out Graphics_Type;
      X, Y     : in     Integer;
      Radius   : in     Integer);
   --  Draws a circle.

   procedure Draw_Ellipse
     (Graphics      : in out Graphics_Type;
      X, Y          : in     Integer;
      Width, Height : in     Integer);
   --  Draws a circle.

   procedure Draw_Polygon
     (Graphics : in out Graphics_Type;
      Path     : in     Gnoga.Types.Point_Array_Type);
   --  Draws a polygon using the given path.

   procedure Draw_Rect
     (Graphics            : in out Graphics_Type;
      X, Y, Width, Height : in     Integer);
   --  Draws a rectangle.

   procedure Draw_Rounded_Rect
     (Graphics            : in out Graphics_Type;
      X, Y, Width, Height : in     Integer);
   --  Draws a rounded rectangle.

   procedure Generate_Canvas_Texture
     (Graphics : in out Graphics_Type;
      Texture  :    out Texture_Type);
   pragma Obsolescent ("Generate_Canvas_Texture no more available.");
   --  Generates a canvas texture.

   procedure Line_Style
     (Graphics   : in out Graphics_Type;
      Line_Width : in     Natural;
      Color      : in     Gnoga.Types.Colors.Color_Enumeration;
      Alpha      : in     Gnoga.Types.Alpha_Type := 1.0);
   procedure Line_Style
     (Graphics   : in out Graphics_Type;
      Line_Width : in     Natural;
      Color      : in     Gnoga.Types.RGBA_Type;
      Alpha      : in     Gnoga.Types.Alpha_Type := 1.0);
   --  Specifies the line style used for subsequent calls to Graphics methods
   --  such as the lineTo() method or the drawCircle() method.

   procedure Line_To (Graphics : in out Graphics_Type; X, Y : in Integer);
   --  Draws a line using the current line style from the current drawing position to (x, y);
   --  The current drawing position is then set to (x, y).

   procedure Move_To (Graphics : in out Graphics_Type; X, Y : in Integer);
   --  Moves the current drawing position to x, y.

   procedure Quadratic_Curve_To
     (Graphics         : in out Graphics_Type;
      CP_X, CP_Y, X, Y : in     Integer);
   --  Calculate the points for a quadratic Bézier curve and then draws it.

private
   type Graphics_Type is new Container_Type with null record;
end Gnoga.Gui.Plugin.Pixi.Graphics;
