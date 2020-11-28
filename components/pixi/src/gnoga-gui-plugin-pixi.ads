------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                  G N O G A . G U I . P L U G I N . P I X I               --
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

with Gnoga.Types.Colors;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Types;

package Gnoga.Gui.Plugin.Pixi is

   --  PIXI Ada API is inspired by https://github.com/pixijs/pixi.js.
   --  Pixi.js is released under the (http://opensource.org/licenses/MIT) MIT License.
   --  Some comments come from Pixi.js documentation:
   --  Pixi.js is a rendering library that will allow you to create rich, interactive graphics,
   --  cross platform applications, and games without having to dive into the WebGL API or
   --  deal with browser and device compatibility.

   procedure Load_PIXI (Window : in out Gnoga.Gui.Window.Window_Type'Class);
   --  Load PIXI code into Window

   -------------------------------------------------------------------------
   --  Application_Type, Renderer_Type and Container_Type
   -------------------------------------------------------------------------

   type Application_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Application_Access is access all Application_Type;
   type Pointer_To_Application_Class is access all Application_Type'Class;

   type Renderer_Type is new Gnoga.Gui.Base.Base_Type with private;
   type Renderer_Access is access all Renderer_Type;
   type Pointer_To_Renderer_Class is access all Renderer_Type'Class;

   type Container_Type is new Gnoga.Gui.Base.Base_Type with private;
   type Container_Access is access all Container_Type;
   type Pointer_To_Container_Class is access all Container_Type'Class;

   -------------------------------------------------------------------------
   --  Application_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Application : in out Application_Type;
      Window      : in out Gnoga.Gui.Window.Window_Type'Class;
      Width       : in     Integer;
      Height      : in     Integer);
   --  Create PIXI application and place it in window document

   procedure Create
     (Application : in out Application_Type;
      Canvas      : in out Gnoga.Gui.Element.Canvas.Canvas_Type'Class;
      Width       : in     Integer;
      Height      : in     Integer);
   --  Create PIXI Application associated to the canvas

   -------------------------------------------------------------------------
   --  Renderer_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Renderer    : in out Renderer_Type;
      Application : in out Application_Type'Class);
   --  Create renderer associated to the application

   -------------------------------------------------------------------------
   --  Renderer_Type - Methods
   -------------------------------------------------------------------------

   procedure Render
     (Renderer  : in out Renderer_Type;
      Container : in     Container_Type'Class);
   --  Render the graphic objects included in the container

   procedure Auto_Rendering
     (Renderer  : in out Renderer_Type;
      Container : in     Container_Type'Class;
      Enable    : in     Boolean);
   function Auto_Rendering
     (Renderer : in out Renderer_Type)
      return Boolean;
   --  Enables periodic rendering based on animation frame

   -------------------------------------------------------------------------
   --  Container_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Container   : in out Container_Type;
      Application : in out Application_Type'Class);

   procedure Create
     (Container : in out Container_Type;
      Renderer  : in out Renderer_Type'Class);
   pragma Obsolescent ("Use Create with Application instead");

   procedure Create
     (Container : in out Container_Type;
      Parent    : in out Container_Type'Class);

   -------------------------------------------------------------------------
   --  Container_Type - Methods
   -------------------------------------------------------------------------

   procedure Add_Child
     (Container : in out Container_Type;
      Child     : in     Container_Type'Class);
   --  Add a child graphic object in the container

   procedure Remove_Child
     (Container : in out Container_Type;
      Child     : in     Container_Type'Class);
   --  Remove a child graphic object in the container

   procedure Remove_Children (Container : in out Container_Type);
   --  Removes all children from this container

   procedure Set_Parent
     (Container : in out Container_Type;
      Parent    : in     Container_Type'Class);
   --  Set the parent Container of this DisplayObject

   function Get_Bounds
     (Container : in Container_Type)
      return Gnoga.Types.Rectangle_Type;
   procedure Get_Bounds
     (Container : in     Container_Type;
      Rect      :    out Gnoga.Types.Rectangle_Type);
   --  Retrieves the bounds of the displayObject as a rectangle object.

   function Get_Local_Bounds
     (Container : in Container_Type)
      return Gnoga.Types.Rectangle_Type;
   procedure Get_Local_Bounds
     (Container : in     Container_Type;
      Rect      :    out Gnoga.Types.Rectangle_Type);
   --  Retrieves the local bounds of the displayObject as a rectangle object.

   function To_Global
     (Container : in Container_Type;
      Position  :    Gnoga.Types.Point_Type)
      return Gnoga.Types.Point_Type;
   procedure To_Global
     (Container : in     Container_Type;
      Position  :        Gnoga.Types.Point_Type;
      Point     :    out Gnoga.Types.Point_Type);
   --  Calculates the global position of the display object

   function To_Local
     (Container : in Container_Type;
      Position  :    Gnoga.Types.Point_Type)
      return Gnoga.Types.Point_Type;
   procedure To_Local
     (Container : in     Container_Type;
      Position  :        Gnoga.Types.Point_Type;
      Point     :    out Gnoga.Types.Point_Type);
   --  Calculates the local position of the display object relative to another point

   procedure Set_Transform
     (Container      : in out Container_Type;
      x, y           : in     Integer;
      scaleX, scaleY : in     Integer;
      rotation       : in     Integer;
      skewX, skewY   : in     Integer;
      pivotX, pivotY : in     Integer);
   --  Convenience function to set the position, scale, skew and pivot at once.

   procedure Update_Transform (Container : in out Container_Type);
   --  Updates the transform on all children of this container for rendering

   -------------------------------------------------------------------------
   --  Texture_Type
   -------------------------------------------------------------------------

   type Texture_Type is new Gnoga.Gui.Base.Base_Type with private;
   type Texture_Access is access all Texture_Type;
   type Pointer_To_Texture_Class is access all Texture_Type'Class;

   -------------------------------------------------------------------------
   --  Texture_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Texture   : in out Texture_Type;
      Renderer  : in out Renderer_Type'Class;
      Image_URL : in     String);
   --  Helper function that creates a Texture object from the given image url.

   procedure Create
     (Texture  : in out Texture_Type;
      Renderer : in out Renderer_Type'Class;
      Canvas   : in     Gnoga.Gui.Element.Canvas.Canvas_Type'Class);
   --  Helper function that creates a new Texture based on the given canvas element.

   -------------------------------------------------------------------------
   --  Texture_Type - Properties
   -------------------------------------------------------------------------

   procedure Frame
     (Texture : in Texture_Type;
      Value   : in Gnoga.Types.Rectangle_Type);
   function Frame
     (Texture : in Texture_Type)
      return Gnoga.Types.Rectangle_Type;
   --  The frame specifies the region of the base texture that this texture uses.

   overriding procedure Width
     (Texture : in out Texture_Type;
      Value   : in     Integer);
   overriding function Width
     (Texture : in Texture_Type)
      return Integer;
   --  The height of the Texture in pixels.

   overriding procedure Height
     (Texture : in out Texture_Type;
      Value   : in     Integer);
   overriding function Height
     (Texture : in Texture_Type)
      return Integer;
   --  The height of the Texture in pixels.

   procedure Orig
     (Texture : in Texture_Type;
      Value   : in Gnoga.Types.Rectangle_Type);
   function Orig
     (Texture : in Texture_Type)
      return Gnoga.Types.Rectangle_Type;
   --  This is the area of original texture, before it was put in atlas.

   procedure Trim
     (Texture : in Texture_Type;
      Value   : in Gnoga.Types.Rectangle_Type);
   function Trim
     (Texture : in Texture_Type)
      return Gnoga.Types.Rectangle_Type;
   --  This is the trimmed area of original texture, before it was put in atlas.

   -------------------------------------------------------------------------
   --  Style_Types
   -------------------------------------------------------------------------

   type Style_Type is new Gnoga.Gui.Base.Base_Type with private;
   type Style_Access is access all Style_Type;
   type Pointer_To_Style_Class is access all Style_Type'Class;

   -------------------------------------------------------------------------
   --  Sprite_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Style  : in out Style_Type;
      Parent : in out Container_Type'Class);

   -------------------------------------------------------------------------
   --  Style_Type - properties
   -------------------------------------------------------------------------

   procedure Align
     (Style : in out Style_Type;
      Value : in     Gnoga.Gui.Element.Alignment_Type);
   function Align
     (Style : in out Style_Type)
      return Gnoga.Gui.Element.Alignment_Type;
   --  align string 'left' default
   --  Alignment for multiline text ('left', 'center' or 'right'),
   --  does not affect single line text

   procedure Break_Words
     (Style : in out Style_Type;
      Value : in     Boolean);
   function Break_Words
     (Style : in out Style_Type)
      return Boolean;
   --  breakWords boolean false default
   --  Indicates if lines can be wrapped within words, it
   --  needs wordWrap to be set to true

   procedure Drop_Shadow
     (Style : in out Style_Type;
      Value : in     Boolean);
   function Drop_Shadow
     (Style : in out Style_Type)
      return Boolean;
   --  dropShadow boolean false default
   --  Set a drop shadow for the text

   procedure Drop_Shadow_Alpha
     (Style : in out Style_Type;
      Value : in     Gnoga.Types.Alpha_Type);
   function Drop_Shadow_Alpha
     (Style : in out Style_Type)
      return Gnoga.Types.Alpha_Type;
   --  dropShadowAlpha number 1 default
   --  Set alpha for the drop shadow

   procedure Drop_Shadow_Angle
     (Style : in out Style_Type;
      Value : in     Integer);
   function Drop_Shadow_Angle
     (Style : in out Style_Type)
      return Integer;
   --  dropShadowAngle number 30° default
   --  Set a angle of the drop shadow

   procedure Drop_Shadow_Blur
     (Style : in out Style_Type;
      Value : in     Natural);
   function Drop_Shadow_Blur
     (Style : in out Style_Type)
      return Natural;
   --  dropShadowBlur number 0 default
   --  Set a shadow blur radius

   procedure Drop_Shadow_Color
     (Style : in out Style_Type;
      Value : in     Gnoga.Types.Colors.Color_Enumeration);
   function Drop_Shadow_Color
     (Style : in out Style_Type)
      return Gnoga.Types.Colors.Color_Enumeration;
   --  dropShadowColor string '#000000' default
   --  A fill style to be used on the dropshadow e.g 'red', '#00FF00'

   procedure Drop_Shadow_Distance
     (Style : in out Style_Type;
      Value : in     Integer);
   function Drop_Shadow_Distance
     (Style : in out Style_Type)
      return Integer;
   --  dropShadowDistance number 5 default
   --  Set a distance of the drop shadow

   procedure Fill
     (Style : in out Style_Type;
      Value : in     Gnoga.Types.Colors.Color_Enumeration);
   function Fill
     (Style : in out Style_Type)
      return Gnoga.Types.Colors.Color_Enumeration;
   --  fill string | Array.<string> | number | Array.<number> | CanvasGradient | CanvasPattern 'black' default
   --  A canvas fillstyle that will be used on the text e.g 'red', '#00FF00'. Can be an array to create a gradient
   --  eg ['#000000','#FFFFFF']

   --  fillGradientType number PIXI.TEXT_GRADIENT.LINEAR_VERTICAL default
   --  If fill is an array of colors
   --  to create a gradient, this can change the type/direction of the gradient. See PIXI.TEXT_GRADIENT

   --  fillGradientStops Array.<number>  default
   --  If fill is an array of colors to create a gradient, this array can set
   --  the stop points (numbers between 0 and 1) for the color, overriding the default behaviour of evenly spacing them.

   procedure Font_Family
     (Style : in out Style_Type;
      Value : in     String);
   function Font_Family
     (Style : in out Style_Type)
      return String;
   --  fontFamily string | Array.<string> 'Arial' default
   --  The font family

   procedure Font_Size
     (Style : in out Style_Type;
      Value : in     String);
   function Font_Size
     (Style : in out Style_Type)
      return String;
   --  fontSize number | string 26 default
   --  The font size (as a number it converts to px, but as a string,
   --  equivalents are '26px','20pt','160%' or '1.6em')

   procedure Font_Style
     (Style : in out Style_Type;
      Value : in     Gnoga.Gui.Element.Font_Style_Type);
   function Font_Style
     (Style : in out Style_Type)
      return Gnoga.Gui.Element.Font_Style_Type;
   --  fontStyle string 'normal' default
   --  The font style ('normal', 'italic' or 'oblique')

   procedure Font_Variant
     (Style : in out Style_Type;
      Value : in     Gnoga.Gui.Element.Font_Variant_Type);
   function Font_Variant
     (Style : in out Style_Type)
      return Gnoga.Gui.Element.Font_Variant_Type;
   --  fontVariant string 'normal' default
   --  The font variant ('normal' or 'small-caps')

   procedure Font_Weight
     (Style : in out Style_Type;
      Value : in     Gnoga.Gui.Element.Font_Weight_Type);
   function Font_Weight
     (Style : in out Style_Type)
      return Gnoga.Gui.Element.Font_Weight_Type;
   --  fontWeight string 'normal' default
   --  The font weight ('normal', 'bold', 'bolder', 'lighter' and '100',
   --  '200', '300', '400', '500', '600', '700', 800' or '900')

   procedure Letter_Spacing
     (Style : in out Style_Type;
      Value : in     Natural);
   function Letter_Spacing
     (Style : in out Style_Type)
      return Natural;
   --  letterSpacing number 0 default
   --  The amount of spacing between letters, default is 0

   procedure Line_Height
     (Style : in out Style_Type;
      Value : in     Natural);
   function Line_Height
     (Style : in out Style_Type)
      return Natural;
   --  lineHeight number  default
   --  The line height, a number that represents the vertical space that a letter uses

   type Line_Join_Type is (Bevel, Round, Miter);

   procedure Line_Join
     (Style : in out Style_Type;
      Value : in     Line_Join_Type);
   function Line_Join
     (Style : in out Style_Type)
      return Line_Join_Type;
   --  lineJoin string 'miter' default
   --  The lineJoin property sets the type of corner created, it can resolve
   --  spiked text issues. Default is 'miter' (creates a sharp corner).

   procedure Miter_Limit
     (Style : in out Style_Type;
      Value : in     Natural);
   function Miter_Limit
     (Style : in out Style_Type)
      return Natural;
   --  miterLimit number 10 default
   --  The miter limit to use when using the 'miter' lineJoin mode. This can reduce
   --  or increase the spikiness of rendered text.

   procedure Padding
     (Style : in out Style_Type;
      Value : in     Natural);
   function Padding
     (Style : in out Style_Type)
      return Natural;
   --  padding number 0 default
   --  Occasionally some fonts are cropped. Adding some padding will prevent this from
   --  happening by adding padding to all sides of the text.

   procedure Stroke
     (Style : in out Style_Type;
      Value : in     Gnoga.Types.Colors.Color_Enumeration);
   function Stroke
     (Style : in out Style_Type)
      return Gnoga.Types.Colors.Color_Enumeration;
   --  stroke string | number 'black' default
   --  A canvas fillstyle that will be used on the text stroke
   --  e.g 'blue', '#FCFF00'

   procedure Stroke_Thickness
     (Style : in out Style_Type;
      Value : in     Natural);
   function Stroke_Thickness
     (Style : in out Style_Type)
      return Natural;
   --  strokeThickness number 0 default
   --  A number that represents the thickness of the stroke.
   --  Default is 0 (no stroke)

   procedure Trim
     (Style : in out Style_Type;
      Value : in     Boolean);
   function Trim
     (Style : in out Style_Type)
      return Boolean;
   --  trim boolean false default
   --  Trim transparent borders

   type Baseline_Type is (Alphabetic, Top, Hanging, Middle, Ideographic, Bottom);

   procedure Text_Baseline
     (Style : in out Style_Type;
      Value : in     Baseline_Type);
   function Text_Baseline
     (Style : in out Style_Type)
      return Baseline_Type;
   --  textBaseline string 'alphabetic' default
   --  The baseline of the text that is rendered.

   procedure Word_Wrap
     (Style : in out Style_Type;
      Value : in     Boolean);
   function Word_Wrap
     (Style : in out Style_Type)
      return Boolean;
   --  wordWrap boolean false default
   --  Indicates if word wrap should be used

   procedure Word_Wrap_Width
     (Style : in out Style_Type;
      Value : in     Natural);
   function Word_Wrap_Width
     (Style : in out Style_Type)
      return Natural;
   --  wordWrapWidth number 100 default
   --  The width at which text will wrap, it needs wordWrap to be set to true

   -------------------------------------------------------------------------
   --  Declaration types
   -------------------------------------------------------------------------

   type Blend_Modes_Type is
     (NORMAL, ADD, MULTIPLY, SCREEN, OVERLAY, DARKEN, LIGHTEN, COLOR_DODGE, COLOR_BURN, HARD_LIGHT, SOFT_LIGHT,
      DIFFERENCE, EXCLUSION, HUE, SATURATION, COLOR, LUMINOSITY, NORMAL_NPM, ADD_NPM, SCREEN_NPM, NONE, SRC_IN, SRC_OUT,
      SRC_ATOP, DST_OVER, DST_IN, DST_OUT, DST_ATOP, SUBTRACT, XOR_BM);
   SRC_OVER : constant Blend_Modes_Type := NORMAL;
   ERASE    : constant Blend_Modes_Type := DST_OUT;
   --  Various blend modes supported by PIXI.
   --  IMPORTANT - The WebGL renderer only supports the NORMAL, ADD, MULTIPLY and SCREEN blend modes.
   --  Anything else will silently act like NORMAL.

   type Scale_Modes_Type is (LINEAR, NEAREST);
   --  The scale modes that are supported by pixi.
   --  - LINEAR: Smooth scaling
   --  - NEAREST: Pixelating scaling

   subtype Velocity_Type is Float range -1_000.0 .. +1_000.0;
   --  A positive value moves the object down or to the right, clockwise for rotation.
   --  A negative value moves the object up or to the left, anti-clockwise for rotation.
   --  A value of 1 moves the object at the speed of 1 pixel per second or 1 degree per second for rotation.
   --  A value of 0 stops the object.

   subtype Acceleration_Type is Float range -1_000.0 .. +1_000.0;
   --  A positive value increases the object velocity.
   --  A negative value decreases the object velocity.
   --  A value of 1 increases the object velocity at the speed of 1 pixel per second
   --  or 1 degree per second for rotation.
   --  A value of 0 stops the acceleration's object.

private
   Frame_Rate : constant := 60.0;
   --  Standard value for most browsers (FPS)

   type Application_Type is new Gnoga.Gui.Element.Element_Type with null record;

   type Renderer_Type is new Gnoga.Gui.Base.Base_Type with null record;
   overriding procedure On_Child_Removed
     (Renderer : in out Renderer_Type;
      Child    : in out Gnoga.Gui.Base.Base_Type'Class);
   type Container_Type is new Gnoga.Gui.Base.Base_Type with record
      Child_Array : Gnoga.Gui.Base.Base_Type_Array;
   end record;
   overriding procedure On_Child_Added
     (Container : in out Container_Type;
      Child     : in out Gnoga.Gui.Base.Base_Type'Class);
   overriding procedure On_Child_Removed
     (Container : in out Container_Type;
      Child     : in out Gnoga.Gui.Base.Base_Type'Class);
   overriding procedure Finalize (Container : in out Container_Type);
   type Texture_Type is new Gnoga.Gui.Base.Base_Type with null record;
   type Style_Type is new Gnoga.Gui.Base.Base_Type with null record;
end Gnoga.Gui.Plugin.Pixi;
