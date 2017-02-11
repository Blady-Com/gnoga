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
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Gnoga.Types;
with Gnoga.Types.Colors;
with Gnoga.Gui.Element;

package Gnoga.Gui.Plugin.Pixi.Graphics is

   -------------------------------------------------------------------------
   --  Graphics_2D_Types
   -------------------------------------------------------------------------

   type Graphics_2D_Type is new Container_Type with private;
   type Graphics_2D_Access is access all Graphics_2D_Type;
   type Pointer_To_Graphics_2D_Class is access all Graphics_2D_Type'Class;

   type Gradient_Type is new Container_Type with private;
   type Gradient_Access is access all Gradient_Type;
   type Pointer_To_Gradient_Class is access all Gradient_Type'Class;

   type Pattern_Type is new Container_Type with private;
   type Pattern_Access is access all Pattern_Type;
   type Pointer_To_Pattern_Class is access all Pattern_Type'Class;

   type Image_Data_Type is new Container_Type with private;
   type Image_Data_Access is access all Image_Data_Type;
   type Pointer_To_Image_Data_Class is access all Image_Data_Type'Class;

   -------------------------------------------------------------------------
   --  Graphics_2D_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Graphics : in out Graphics_2D_Type;
      Parent   : in out Container_Type'Class);

   -------------------------------------------------------------------------
   --  Graphics_2D_Type - Properties
   -------------------------------------------------------------------------

   --  Colors, Styles, Shadows

   procedure Fill_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Types.RGBA_Type);
   procedure Fill_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     String);
   procedure Fill_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Types.Colors.Color_Enumeration);
   --  Color used to fill in the drawing

   procedure Fill_Gradient
     (Graphics : in out Graphics_2D_Type;
      Value    : in out Gradient_Type'Class);
   --  Gradient used to fill in drawing

   procedure Fill_Pattern
     (Graphics : in out Graphics_2D_Type;
      Value    : in out Pattern_Type'Class);
   --  Pattern used to fill in the drawing

   procedure Stroke_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Types.RGBA_Type);
   procedure Stroke_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     String);
   procedure Stroke_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Types.Colors.Color_Enumeration);
   --  Color used for strokes

   procedure Stroke_Gradient
     (Graphics : in out Graphics_2D_Type;
      Value    : in out Gradient_Type'Class);
   --  Gradient used for strokes

   procedure Stroke_Pattern
     (Graphics : in out Graphics_2D_Type;
      Value    : in out Pattern_Type'Class);

   procedure Shadow_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Types.RGBA_Type);
   procedure Shadow_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     String);
   procedure Shadow_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Types.Colors.Color_Enumeration);
   --  Color to use for shadows

   procedure Shadow_Blur
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Integer);
   --  Blur level for shadows

   procedure Shadow_Offset_X
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Integer);
   --  Horizontal distance of the shadow from the shape

   procedure Shadow_Offset_Y
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Integer);
   --  Vertical distance of the shadow from the shape

   --  Line Styles

   type Line_Cap_Type is (Butt, Round, Square);

   procedure Line_Cap
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Line_Cap_Type);
   --  Style of the end caps for a line

   type Line_Join_Type is (Bevel, Round, Miter);

   procedure Line_Join
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Line_Join_Type);
   --  Type of corner created when two lines meet

   procedure Line_Width
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Integer);

   procedure Miter_Limit
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Positive);
   --  Maximum miter length

   type Dash_Array_Type is array (Positive range <>) of Natural;

   Empty_Dash_List  : constant Dash_Array_Type (1 .. 0) := (others => 0);
   Dotted_Dash_List : constant Dash_Array_Type          := (2, 2);
   Center_Dash_List : constant Dash_Array_Type          := (4, 3, 6, 3);
   Dashed_Dash_List : constant Dash_Array_Type          := (5, 3);

   procedure Set_Line_Dash
     (Graphics  : in out Graphics_2D_Type;
      Dash_List : in     Dash_Array_Type);

   --  Text

   procedure Font
     (Graphics : in out Graphics_2D_Type;
      Family   : in     String                             := "sans-serif";
      Height   : in     String                             := "10px";
      Style : in Gnoga.Gui.Element.Font_Style_Type := Gnoga.Gui.Element.Normal;
      Weight   : in     Gnoga.Gui.Element.Font_Weight_Type :=
        Gnoga.Gui.Element.Weight_Normal;
      Variant : in Gnoga.Gui.Element.Font_Variant_Type :=
        Gnoga.Gui.Element.Normal);
   procedure Font
     (Graphics    : in out Graphics_2D_Type;
      System_Font : in     Gnoga.Gui.Element.System_Font_Type);
   --  Sets or returns the current font properties for text content

   procedure Text_Alignment
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Gui.Element.Alignment_Type);
   --  Text Alignment, At_Start = Left, and To_End = Right in ltr languages
   --  in rtl languages At_Start = Right, and To_End = Left.

   type Baseline_Type is
     (Alphabetic, Top, Hanging, Middle, Ideographic, Bottom);

   procedure Text_Baseline
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Baseline_Type);
   --  Baseline used when drawing text

   --  Image Data

   --  width    Returns the width of an ImageData object
   --  height   Returns the height of an ImageData object
   --  data     Returns an object that contains image data of a specified
   --           ImageData object

   --  Compositing

   --  globalAlpha
   procedure Global_Alpha
     (Graphics : in out Graphics_2D_Type;
      Alpha    :        Gnoga.Types.Alpha_Type);
   --  Global Alpha Transparency

   --  globalCompositeOperation
   type Composite_Method_Type is
     (Source_Over,
      Source_Atop,
      Source_In,
      Source_Out,
      Destination_Over,
      Destination_Atop,
      Destination_In,
      Destination_Out,
      Lighter,
      Copy,
      Xor_Copy);

   procedure Glogal_Composite_Operation
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Composite_Method_Type);
   --  How a new image are composited onto Graphics

   -------------------------------------------------------------------------
   --  Graphics_2D_Type - Methods
   -------------------------------------------------------------------------

   --  Colors, Styles, Shadows

   procedure Create_Linear_Gradient
     (Gradient : in out Gradient_Type;
      Graphics : in out Graphics_2D_Type'Class;
      X_1      : in     Integer;
      Y_1      : in     Integer;
      X_2      : in     Integer;
      Y_2      : in     Integer);
   --  Creates a linear gradient. Gradient Start Point = (X_1,Y_1),
   --  End Point = (X_2, Y_2)

   procedure Create_Radial_Gradient
     (Gradient : in out Gradient_Type;
      Graphics : in out Graphics_2D_Type'Class;
      X_1      : in     Integer;
      Y_1      : in     Integer;
      R_1      : in     Integer;
      X_2      : in     Integer;
      Y_2      : in     Integer;
      R_2      : in     Integer);
   --  Creates a radial gradient. Gradient Start Point = (X_1,Y_1) with radius
   --  R_1 and End Point = (X_2, Y_2) with Radius R_2

   procedure Add_Color_Stop
     (Gradient : in out Gradient_Type;
      Position : in     Gnoga.Types.Frational_Range_Type;
      Color    : in     Gnoga.Types.RGBA_Type);
   procedure Add_Color_Stop
     (Gradient : in out Gradient_Type;
      Position : in     Gnoga.Types.Frational_Range_Type;
      Color    : in     String);
   procedure Add_Color_Stop
     (Gradient : in out Gradient_Type;
      Position : in     Gnoga.Types.Frational_Range_Type;
      Color    : in     Gnoga.Types.Colors.Color_Enumeration);
   --  Specifies the colors and stop positions in a gradient object

   type Repeat_Type is (Repeat, Repeat_X_Only, Repeat_Y_Only, No_Repeat);

   procedure Create_Pattern
     (Pattern        : in out Pattern_Type;
      Graphics       : in out Graphics_2D_Type'Class;
      Image          : in out Gnoga.Gui.Element.Element_Type'Class;
      Repeat_Pattern : in     Repeat_Type := Repeat);
   --  Uses Image as a pattern according to Repeat. Image can be a
   --  Gnoga.Element.Common.IMG_Type, Canvas_Type or a
   --  Gnoga.Multimedia.Video_Type

   --  Rectangles

   procedure Rectangle
     (Graphics  : in out Graphics_2D_Type;
      Rectangle : in     Gnoga.Types.Rectangle_Type);
   --  Create a Rectangle path (Stroke and/or Fill must be called to draw it).

   procedure Fill_Rectangle
     (Graphics  : in out Graphics_2D_Type;
      Rectangle : in     Gnoga.Types.Rectangle_Type);

   procedure Stroke_Rectangle
     (Graphics  : in out Graphics_2D_Type;
      Renderer  : in out Renderer_Type;
      Rectangle : in     Gnoga.Types.Rectangle_Type);

   procedure Clear_Rectangle
     (Graphics  : in out Graphics_2D_Type;
      Rectangle : in     Gnoga.Types.Rectangle_Type);

   --  Paths

   procedure Begin_Fill
     (Graphics : in out Graphics_2D_Type;
      Color    : in     Gnoga.Types.Colors.Color_Enumeration :=
        Gnoga.Types.Colors.Black;
      Alpha : Gnoga.Types.Alpha_Type := 1.0);
   procedure End_Fill (Graphics : in out Graphics_2D_Type);
   --  Fills the current drawing path

   procedure Stroke
     (Graphics : in out Graphics_2D_Type;
      Renderer : in out Renderer_Type);
   --  Draws the current path

   procedure Begin_Path (Graphics : in out Graphics_2D_Type);
   --  Begins a path or reset current path

   procedure Move_To (Graphics : in out Graphics_2D_Type; X, Y : Integer);
   --  Moves the path to the specified point in the canvas without creating
   --  a line

   procedure Close_Path (Graphics : in out Graphics_2D_Type);
   --  Creates a path from the current point back to the first point of path

   procedure Line_To (Graphics : in out Graphics_2D_Type; X, Y : Integer);
   --  Adds a line from the current point to X, Y

   procedure Clip (Graphics : in out Graphics_2D_Type);
   --  Transforms the current path in to a clipping region

   procedure Quadractic_Curve_To
     (Graphics         : in out Graphics_2D_Type;
      CP_X, CP_Y, X, Y :        Integer);
   --  Creates a quadratic Bézier curve, using control point CP_X, CP_Y to
   --  point X, Y.

   procedure Bezier_Curve_To
     (Graphics                       : in out Graphics_2D_Type;
      CP_X_1, CP_Y_1, CP_X_2, CP_Y_2 : in     Integer;
      X, Y                           : in     Integer);
   --  Creates a cubic Bézier curve

   procedure Arc_Radians
     (Graphics                     : in out Graphics_2D_Type;
      X, Y                         : in     Integer;
      Radius                       : in     Integer;
      Starting_Angle, Ending_Angle : in     Float;
      Counter_Clockwise            : in     Boolean := False);

   procedure Arc_Degrees
     (Graphics                     : in out Graphics_2D_Type;
      X, Y                         : in     Integer;
      Radius                       : in     Integer;
      Starting_Angle, Ending_Angle : in     Float;
      Counter_Clockwise            : in     Boolean := False);
   --  Creates an arc / curve (used to create circles, or parts of circles)

   procedure Arc_To
     (Graphics           : in out Graphics_2D_Type;
      X_1, Y_1, X_2, Y_2 : in     Integer;
      Radius             : in     Integer);
   --  Creates an arc / curve between two tangents

   function Is_Point_In_Path
     (Graphics : Graphics_2D_Type;
      X, Y     : Integer) return Boolean;
   --  Returns true if the specified point is in the current path, otherwise
   --  false

   --  Transforms

   procedure Scale (Graphics : in out Graphics_2D_Type; Width, Height : Float);
   --  Scales the current drawing bigger or smaller, 1.0 = 100%

   procedure Rotate_Radians
     (Graphics : in out Graphics_2D_Type;
      Radians  : in     Float);
   procedure Rotate_Degrees
     (Graphics : in out Graphics_2D_Type;
      Degrees  : in     Float);
   --  Rotates the current drawing

   procedure Translate (Graphics : in out Graphics_2D_Type; X, Y : Integer);
   --  Remaps the (0, 0) position on the canvas

   procedure Transform
     (Graphics                          : in out Graphics_2D_Type;
      Scale_Horizontal, Skew_Horizontal : in     Float;
      Scale_Vertical, Skew_Vertical     : in     Float;
      Move_Horizontal, Move_Vertical    : in     Float);
   --  Sets the current transformation matrix for the drawing relative to last
   --  transformation

   procedure Set_Transform
     (Graphics                          : in out Graphics_2D_Type;
      Scale_Horizontal, Skew_Horizontal : in     Float;
      Scale_Vertical, Skew_Vertical     : in     Float;
      Move_Horizontal, Move_Vertical    : in     Float);
   --  Sets the current transformation matrix for the drawing

   --  Text

   procedure Fill_Text
     (Graphics   : in out Graphics_2D_Type;
      Text       : in     String;
      X, Y       : in     Integer;
      Max_Length : in     Natural := 0);
   --  Place Text and fill on Graphics at X, Y with Max_Lenght if > 0

   procedure Stroke_Text
     (Graphics   : in out Graphics_2D_Type;
      Renderer   : in out Renderer_Type;
      Text       : in     String;
      X, Y       : in     Integer;
      Max_Length : in     Natural := 0);
   --  Place Text without fill on Graphics at X, Y with Max_Lenght if > 0

   function Measure_Text_Width
     (Graphics : Graphics_2D_Type;
      Text     : String) return Float;
   --  Width of Text if drawn on Graphics

   --  Image Drawing

   procedure Draw_Image
     (Graphics : in out Graphics_2D_Type'Class;
      Image    : in out Gnoga.Gui.Element.Element_Type'Class;
      X, Y     : in     Integer);
   --  Draw Image at point X, Y. Image can be a
   --  Gnoga.Element.Common.IMG_Type, Canvas_Type or a
   --  Gnoga.Multimedia.Video_Type

   --  Image Data

   function Pixel
     (Graphics : Graphics_2D_Type;
      X, Y     : Integer) return Gnoga.Types.Pixel_Type;
   procedure Pixel
     (Graphics : in out Graphics_2D_Type;
      X, Y     : in     Integer;
      Color    : in     Gnoga.Types.Pixel_Type);
   procedure Pixel
     (Graphics : in out Graphics_2D_Type;
      X, Y     : in     Integer;
      Color    : in     Gnoga.Types.Colors.Color_Enumeration);
   --  Set or Get the Pixel at X, Y
   --  Note: Left and Top are absolute and not affected by Translate

   procedure Create_Image_Data
     (Graphics      : in out Graphics_2D_Type;
      Image_Data    : in out Image_Data_Type'Class;
      Width, Height : in     Integer);
   --  Create a blank Image_Data_Type with Width and Height matching the pixel
   --  properties of Graphics

   procedure Get_Image_Data
     (Graphics      : in out Graphics_2D_Type;
      Image_Data    : in out Image_Data_Type'Class;
      Left, Top     : in     Integer;
      Width, Height : in     Integer);
   --  Creates an Image_Data object containing pixel data from Graphics at
   --  Left, Top with Width and Height dimensions
   --  Note: Left and Top are absolute and not affected by Translate

   procedure Put_Image_Data
     (Graphics   : in out Graphics_2D_Type;
      Image_Data : in out Image_Data_Type'Class;
      Left, Top  : in     Integer);
   --   Put Image_Data at Left, Top of Graphics
   --  Note: Left and Top are absolute and not affected by Translate

   function Width0 (Image_Data : Image_Data_Type) return Natural;

   function Height0 (Image_Data : Image_Data_Type) return Natural;

   procedure Data
     (Image_Data : in out Image_Data_Type;
      Value      : in     Gnoga.Types.Pixel_Data_Type);
   function Data
     (Image_Data : Image_Data_Type) return Gnoga.Types.Pixel_Data_Type;
   --  Data property of Image_Data_Type

   procedure New_From_XPM
     (Image     : out Gnoga.Types.Pixel_Data_Access;
      File_Name :     String);
   --  Read XPM data from File_Name to Image which is allocated with XPM size
   procedure Free is new Ada.Unchecked_Deallocation
     (Gnoga.Types.Pixel_Data_Type,
      Gnoga.Types.Pixel_Data_Access);
   --  Free image data allocated with New_From_XPM

   --  Other

   procedure Save (Graphics : in out Graphics_2D_Type);
   --  Saves/Pushes the state of the current Graphics, states stack

   procedure Restore (Graphics : in out Graphics_2D_Type);
   --  Restores/Pops the state the the previous Graphics

private
   type Graphics_2D_Type is new Container_Type with null record;
   type Gradient_Type is new Container_Type with null record;
   type Pattern_Type is new Container_Type with null record;
   type Image_Data_Type is new Container_Type with null record;
end Gnoga.Gui.Plugin.Pixi.Graphics;
