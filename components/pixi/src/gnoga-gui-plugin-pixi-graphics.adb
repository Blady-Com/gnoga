------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--       G N O G A . G U I . P L U G I N . P I X I . G R A P H I C S        --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Strings.Fixed;
with Ada.Numerics;
with Ada.Text_IO;

with Gnoga.Server.Connection;
with Parsers.Multiline_Source.XPM;
with Parsers.Multiline_Source.Text_IO;
with Ada.Strings;

package body Gnoga.Gui.Plugin.Pixi.Graphics is
   procedure Data (Image_Data : in out Image_Data_Type; Value : in String);
   function Data (Image_Data : Image_Data_Type) return String;
   --  Raw data transfer of pixel data from Browser

   function String_To_Pixel_Data
     (Value         : String;
      Width, Height : Positive) return Gnoga.Types.Pixel_Data_Type;
   --  Translate raw result from browser to Pixel_Data_Type

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (Graphics : in out Graphics_2D_Type;
      Parent   : in out Container_Type'Class)
   is
      Graphics_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Graphics.ID (Graphics_ID, Gnoga.Types.Gnoga_ID);
      Graphics.Connection_ID (Parent.Connection_ID);
      Gnoga.Server.Connection.Execute_Script
        (Graphics.Connection_ID,
         "gnoga['" & Graphics_ID & "'] = new PIXI.Graphics();");
      Parent.Add_Child (Graphics);
   end Create;

   ----------------
   -- Fill_Color --
   ----------------

   procedure Fill_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Types.RGBA_Type)
   is
   begin
      Graphics.Fill_Color (Gnoga.Types.To_String (Value));
   end Fill_Color;

   procedure Fill_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     String)
   is
   begin
      Graphics.Property ("fillStyle", Value);
   end Fill_Color;

   procedure Fill_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Types.Colors.Color_Enumeration)
   is
   begin
      Graphics.Fill_Color (Gnoga.Types.Colors.To_String (Value));
   end Fill_Color;

   -------------------
   -- Fill_Gradient --
   -------------------

   procedure Fill_Gradient
     (Graphics : in out Graphics_2D_Type;
      Value    : in out Gradient_Type'Class)
   is
   begin
      Graphics.Execute ("fillStyle=gnoga['" & Value.ID & "'];");
   end Fill_Gradient;

   -------------------
   -- Fill_Pattern --
   -------------------

   procedure Fill_Pattern
     (Graphics : in out Graphics_2D_Type;
      Value    : in out Pattern_Type'Class)
   is
   begin
      Graphics.Execute ("fillStyle=gnoga['" & Value.ID & "'];");
   end Fill_Pattern;

   ------------------
   --  Stroke_Color --g
   ------------------

   procedure Stroke_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Types.RGBA_Type)
   is
   begin
      Graphics.Property ("lineColor", Gnoga.Types.To_Hex (Value));
   end Stroke_Color;

   procedure Stroke_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     String)
   is
   begin
      Graphics.Stroke_Color (Gnoga.Types.Colors.To_Color_Enumeration (Value));
   end Stroke_Color;

   procedure Stroke_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Types.Colors.Color_Enumeration)
   is
   begin
      Graphics.Stroke_Color (Gnoga.Types.Colors.To_RGBA (Value));
   end Stroke_Color;

   ---------------------
   -- Stroke_Gradient --
   ---------------------

   procedure Stroke_Gradient
     (Graphics : in out Graphics_2D_Type;
      Value    : in out Gradient_Type'Class)
   is
   begin
      Graphics.Execute ("strokeStyle=gnoga['" & Value.ID & "'];");
   end Stroke_Gradient;

   -------------------
   -- Stroke_Pattern --
   -------------------

   procedure Stroke_Pattern
     (Graphics : in out Graphics_2D_Type;
      Value    : in out Pattern_Type'Class)
   is
   begin
      Graphics.Execute ("strokeStyle=gnoga['" & Value.ID & "'];");
   end Stroke_Pattern;

   ------------------
   -- Shadow_Color --
   ------------------

   procedure Shadow_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Types.RGBA_Type)
   is
   begin
      Graphics.Shadow_Color (Gnoga.Types.To_String (Value));
   end Shadow_Color;

   procedure Shadow_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     String)
   is
   begin
      Graphics.Property ("shadowColor", Value);
   end Shadow_Color;

   procedure Shadow_Color
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Types.Colors.Color_Enumeration)
   is
   begin
      Graphics.Shadow_Color (Gnoga.Types.Colors.To_String (Value));
   end Shadow_Color;

   -----------------
   -- Shadow_Blur --
   -----------------

   procedure Shadow_Blur
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Integer)
   is
   begin
      Graphics.Property ("shadowBlur", Value);
   end Shadow_Blur;

   ---------------------
   -- Shadow_Offset_X --
   ---------------------

   procedure Shadow_Offset_X
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Integer)
   is
   begin
      Graphics.Property ("shadowOffsetX", Value);
   end Shadow_Offset_X;

   ---------------------
   -- Shadow_Offset_Y --
   ---------------------

   procedure Shadow_Offset_Y
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Integer)
   is
   begin
      Graphics.Property ("shadowOffsetY", Value);
   end Shadow_Offset_Y;

   --------------
   -- Line_Cap --
   --------------

   procedure Line_Cap
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Line_Cap_Type)
   is
   begin
      Graphics.Property ("lineCap", Value'Img);
   end Line_Cap;

   ---------------
   -- Line_Join --
   ---------------

   procedure Line_Join
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Line_Join_Type)
   is
   begin
      Graphics.Property ("lineJoin", Value'Img);
   end Line_Join;

   ----------------
   --  Line_Width --g
   ----------------

   procedure Line_Width
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Integer)
   is
   begin
      Graphics.Property ("lineWidth", Value);
   end Line_Width;

   -----------------
   -- Miter_Limit --
   -----------------

   procedure Miter_Limit
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Positive)
   is
   begin
      Graphics.Property ("miterLimit", Value);
   end Miter_Limit;

   -------------------
   -- Set_Line_Dash --
   -------------------

   procedure Set_Line_Dash
     (Graphics  : in out Graphics_2D_Type;
      Dash_List : in     Dash_Array_Type)
   is
      function Dash_String (Index : Natural) return String;
      --  Iterate over Dash_List to create JS Array for setLineDash

      function Dash_String (Index : Natural) return String is
      begin
         if Index > Dash_List'Last then
            return "";
         elsif Index = Dash_List'Last then
            return Natural'Image (Dash_List (Index));
         else
            return Natural'Image (Dash_List (Index)) &
              ',' &
              Dash_String (Index + 1);
         end if;
      end Dash_String;
   begin
      Graphics.Execute
      ("setLineDash([" & Dash_String (Dash_List'First) & "]);");
   end Set_Line_Dash;

   ----------
   -- Font --
   ----------

   procedure Font
     (Graphics : in out Graphics_2D_Type;
      Family   : in     String                             := "sans-serif";
      Height   : in     String                             := "10px";
      Style : in Gnoga.Gui.Element.Font_Style_Type := Gnoga.Gui.Element.Normal;
      Weight   : in     Gnoga.Gui.Element.Font_Weight_Type :=
        Gnoga.Gui.Element.Weight_Normal;
      Variant : in Gnoga.Gui.Element.Font_Variant_Type :=
        Gnoga.Gui.Element.Normal)
   is
      W : constant String := Weight'Img;
   begin
      Graphics.Property
      ("font", Style'Img &
       " " &
       Variant'Img &
       " " &
       W (W'First + 7 .. W'Last) &
       " " &
       Height &
       " " &
       Family);
   end Font;

   procedure Font
     (Graphics    : in out Graphics_2D_Type;
      System_Font : in     Gnoga.Gui.Element.System_Font_Type)
   is
   begin
      case System_Font is
         when Gnoga.Gui.Element.Caption |
           Gnoga.Gui.Element.Icon       |
           Gnoga.Gui.Element.Menu       =>
            Graphics.Property ("font", System_Font'Img);
         when Gnoga.Gui.Element.Message_Box =>
            Graphics.Property ("font", "message-box");
         when Gnoga.Gui.Element.Small_Caption =>
            Graphics.Property ("font", "small-caption");
         when Gnoga.Gui.Element.Status_Bar =>
            Graphics.Property ("font", "status-bar");
      end case;
   end Font;

   --------------------
   -- Text_Alignment --
   --------------------

   procedure Text_Alignment
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Gnoga.Gui.Element.Alignment_Type)
   is
      V : constant String := Value'Img;
   begin
      case Value is
         when Gnoga.Gui.Element.Left |
           Gnoga.Gui.Element.Right   |
           Gnoga.Gui.Element.Center  =>
            Graphics.Property ("textAlign", V);
         when Gnoga.Gui.Element.At_Start | Gnoga.Gui.Element.To_End =>
            Graphics.Property ("textAlign", V ((V'First + 3) .. V'Last));
      end case;
   end Text_Alignment;

   -------------------
   -- Text_Baseline --
   -------------------

   procedure Text_Baseline
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Baseline_Type)
   is
   begin
      Graphics.Property ("textBaseline", Value'Img);
   end Text_Baseline;

   ------------------
   -- Global_Alpha --
   ------------------

   procedure Global_Alpha
     (Graphics : in out Graphics_2D_Type;
      Alpha    : in     Gnoga.Types.Alpha_Type)
   is
   begin
      Graphics.Property ("globalAlpha", Alpha'Img);
   end Global_Alpha;

   --------------------------------
   -- Glogal_Composite_Operation --
   --------------------------------

   procedure Glogal_Composite_Operation
     (Graphics : in out Graphics_2D_Type;
      Value    : in     Composite_Method_Type)
   is
      V : constant String := Value'Img;
   begin
      case Value is
         when Lighter | Copy =>
            Graphics.Property ("globalCompositeOperation", V);
         when Source_Over | Source_Atop | Source_In | Source_Out =>
            Graphics.Property
            ("globalCompositeOperation", "source-" &
             V ((V'First + 7) .. V'Last));
         when Destination_Over |
           Destination_Atop    |
           Destination_In      |
           Destination_Out     =>
            Graphics.Property
            ("globalCompositeOperation", "destination-" &
             V ((V'First + 12) .. V'Last));
         when Xor_Copy =>
            Graphics.Property ("globalCompositeOperation", "xor");
      end case;
   end Glogal_Composite_Operation;

   ----------------------------
   -- Create_Linear_Gradient --
   ----------------------------

   procedure Create_Linear_Gradient
     (Gradient : in out Gradient_Type;
      Graphics : in out Graphics_2D_Type'Class;
      X_1      : in     Integer;
      Y_1      : in     Integer;
      X_2      : in     Integer;
      Y_2      : in     Integer)
   is
      GID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Gradient.ID (GID, Gnoga.Types.Gnoga_ID);
      Gradient.Connection_ID (Graphics.Connection_ID);

      Gnoga.Server.Connection.Execute_Script
        (Graphics.Connection_ID,
         "gnoga['" &
         GID &
         "']=" &
         "gnoga['" &
         Graphics.ID &
         "'].createLinearGradient(" &
         X_1'Img &
         "," &
         Y_1'Img &
         "," &
         X_2'Img &
         "," &
         Y_2'Img &
         ");");
   end Create_Linear_Gradient;

   ----------------------------
   -- Create_Radial_Gradient --
   ----------------------------

   procedure Create_Radial_Gradient
     (Gradient : in out Gradient_Type;
      Graphics : in out Graphics_2D_Type'Class;
      X_1      : in     Integer;
      Y_1      : in     Integer;
      R_1      : in     Integer;
      X_2      : in     Integer;
      Y_2      : in     Integer;
      R_2      : in     Integer)
   is
      GID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Gradient.ID (GID, Gnoga.Types.Gnoga_ID);
      Gradient.Connection_ID (Graphics.Connection_ID);

      Gnoga.Server.Connection.Execute_Script
        (Graphics.Connection_ID,
         "gnoga['" &
         GID &
         "']=" &
         "gnoga['" &
         Graphics.ID &
         "'].createRadialGradient(" &
         X_1'Img &
         "," &
         Y_1'Img &
         "," &
         R_1'Img &
         "," &
         X_2'Img &
         "," &
         Y_2'Img &
         "," &
         R_2'Img &
         ");");
   end Create_Radial_Gradient;

   --------------------
   -- Add_Color_Stop --
   --------------------

   procedure Add_Color_Stop
     (Gradient : in out Gradient_Type;
      Position : in     Gnoga.Types.Frational_Range_Type;
      Color    : in     Gnoga.Types.RGBA_Type)
   is
   begin
      Gradient.Add_Color_Stop (Position, Gnoga.Types.To_String (Color));
   end Add_Color_Stop;

   procedure Add_Color_Stop
     (Gradient : in out Gradient_Type;
      Position : in     Gnoga.Types.Frational_Range_Type;
      Color    : in     String)
   is
   begin
      Gradient.Execute
      ("addColorStop (" & Position'Img & ", '" & Color & "');");
   end Add_Color_Stop;

   procedure Add_Color_Stop
     (Gradient : in out Gradient_Type;
      Position : in     Gnoga.Types.Frational_Range_Type;
      Color    : in     Gnoga.Types.Colors.Color_Enumeration)
   is
   begin
      Gradient.Add_Color_Stop (Position, Gnoga.Types.Colors.To_String (Color));
   end Add_Color_Stop;

   ----------------------------
   -- Create_Radial_Gradient --
   ----------------------------

   procedure Create_Pattern
     (Pattern        : in out Pattern_Type;
      Graphics       : in out Graphics_2D_Type'Class;
      Image          : in out Gnoga.Gui.Element.Element_Type'Class;
      Repeat_Pattern : in     Repeat_Type := Repeat)
   is
      GID : constant String := Gnoga.Server.Connection.New_GID;

      function Repeat_to_String return String;

      function Repeat_to_String return String is
      begin
         case Repeat_Pattern is
            when Repeat =>
               return "repeat";
            when Repeat_X_Only =>
               return "repeat-x";
            when Repeat_Y_Only =>
               return "repeat-y";
            when No_Repeat =>
               return "no-repeat";
         end case;
      end Repeat_to_String;
   begin
      Pattern.ID (GID, Gnoga.Types.Gnoga_ID);
      Pattern.Connection_ID (Graphics.Connection_ID);

      Gnoga.Server.Connection.Execute_Script
        (Graphics.Connection_ID,
         "gnoga['" &
         GID &
         "']=" &
         "gnoga['" &
         Graphics.ID &
         "'].createPattern(" &
         Image.jQuery &
         ".get(0), '" &
         Repeat_to_String &
         "');");
   end Create_Pattern;

   ---------------
   --  Rectangle --g
   ---------------

   procedure Rectangle
     (Graphics  : in out Graphics_2D_Type;
      Rectangle : in     Gnoga.Types.Rectangle_Type)
   is
   begin
      Graphics.Execute
      ("drawRect (" &
       Rectangle.X'Img &
       "," &
       Rectangle.Y'Img &
       "," &
       Rectangle.Width'Img &
       "," &
       Rectangle.Height'Img &
       ");");
   end Rectangle;

   --------------------
   -- Fill_Rectangle --
   --------------------

   procedure Fill_Rectangle
     (Graphics  : in out Graphics_2D_Type;
      Rectangle : in     Gnoga.Types.Rectangle_Type)
   is
   begin
      Graphics.Rectangle (Rectangle);
--        Graphics.Stroke;
   end Fill_Rectangle;

   ----------------------
   --  Stroke_Rectangle --g
   ----------------------

   procedure Stroke_Rectangle
     (Graphics  : in out Graphics_2D_Type;
      Renderer  : in out Renderer_Type;
      Rectangle : in     Gnoga.Types.Rectangle_Type)
   is
   begin
      Graphics.Rectangle (Rectangle);
      Graphics.Stroke (Renderer);
   end Stroke_Rectangle;

   ---------------------
   -- Clear_Rectangle --
   ---------------------

   procedure Clear_Rectangle
     (Graphics  : in out Graphics_2D_Type;
      Rectangle : in     Gnoga.Types.Rectangle_Type)
   is
   begin
      Graphics.Execute
      ("clearRect (" &
       Rectangle.X'Img &
       "," &
       Rectangle.Y'Img &
       "," &
       Rectangle.Width'Img &
       "," &
       Rectangle.Height'Img &
       ");");
   end Clear_Rectangle;

   ----------
   --  Fill --g
   ----------

   procedure Begin_Fill
     (Graphics : in out Graphics_2D_Type;
      Color    : in     Gnoga.Types.Colors.Color_Enumeration :=
        Gnoga.Types.Colors.Black;
      Alpha : Gnoga.Types.Alpha_Type := 1.0)
   is
   begin
      Graphics.Execute
      ("beginFill(" &
       Gnoga.Types.To_Hex (Gnoga.Types.Colors.To_RGBA (Color)) &
       ", " &
       Alpha'Img &
       ");");
   end Begin_Fill;

   procedure End_Fill (Graphics : in out Graphics_2D_Type) is
   begin
      Graphics.Execute ("endFill();");
   end End_Fill;

   ------------
   -- Stroke --
   ------------

   procedure Stroke
     (Graphics : in out Graphics_2D_Type;
      Renderer : in out Renderer_Type)
   is
   begin
      Renderer.Render (Graphics);
   end Stroke;

   ----------------
   -- Begin_Path --
   ----------------

   procedure Begin_Path (Graphics : in out Graphics_2D_Type) is
   begin
      Graphics.Execute ("beginPath();");
   end Begin_Path;

   --------------
   --  Move_To --g
   --------------

   procedure Move_To (Graphics : in out Graphics_2D_Type; X, Y : Integer) is
   begin
      Graphics.Execute ("moveTo(" & X'Img & "," & Y'Img & ");");
   end Move_To;

   ----------------
   --  Close_Path --g
   ----------------

   procedure Close_Path (Graphics : in out Graphics_2D_Type) is
   begin
      Graphics.Execute ("closePath()");
   end Close_Path;

   --------------
   --  Line_To --g
   --------------

   procedure Line_To (Graphics : in out Graphics_2D_Type; X, Y : Integer) is
   begin
      Graphics.Execute ("lineTo(" & X'Img & "," & Y'Img & ");");
   end Line_To;

   ----------
   -- Clip --
   ----------

   procedure Clip (Graphics : in out Graphics_2D_Type) is
   begin
      Graphics.Execute ("clip();");
   end Clip;

   -------------------------
   --  Quadractic_Curve_To --g
   -------------------------

   procedure Quadractic_Curve_To
     (Graphics         : in out Graphics_2D_Type;
      CP_X, CP_Y, X, Y :        Integer)
   is
   begin
      Graphics.Execute
      ("quadraticCurveTo(" &
       CP_X'Img &
       "," &
       CP_Y'Img &
       "," &
       X'Img &
       "," &
       Y'Img &
       ");");
   end Quadractic_Curve_To;

   ---------------------
   --  Bezier_Curve_To --g
   ---------------------

   procedure Bezier_Curve_To
     (Graphics                       : in out Graphics_2D_Type;
      CP_X_1, CP_Y_1, CP_X_2, CP_Y_2 : in     Integer;
      X, Y                           : in     Integer)
   is
   begin
      Graphics.Execute
      ("bezierCurveTo(" &
       CP_X_1'Img &
       "," &
       CP_Y_1'Img &
       "," &
       CP_X_2'Img &
       "," &
       CP_Y_2'Img &
       "," &
       X'Img &
       "," &
       Y'Img &
       ");");
   end Bezier_Curve_To;

   -----------------
   --  Arc_Radians --g
   -----------------

   procedure Arc_Radians
     (Graphics                     : in out Graphics_2D_Type;
      X, Y                         : in     Integer;
      Radius                       : in     Integer;
      Starting_Angle, Ending_Angle : in     Float;
      Counter_Clockwise            : in     Boolean := False)
   is
   begin
      Graphics.Execute
      ("arc(" &
       X'Img &
       "," &
       Y'Img &
       "," &
       Radius'Img &
       "," &
       Starting_Angle'Img &
       "," &
       Ending_Angle'Img &
       "," &
       Counter_Clockwise'Img &
       ");");
   end Arc_Radians;

   -----------------
   --  Arc_Degrees --g
   -----------------

   procedure Arc_Degrees
     (Graphics                     : in out Graphics_2D_Type;
      X, Y                         : in     Integer;
      Radius                       : in     Integer;
      Starting_Angle, Ending_Angle : in     Float;
      Counter_Clockwise            : in     Boolean := False)
   is
   begin
      Arc_Radians
        (Graphics,
         X,
         Y,
         Radius,
         Starting_Angle * Ada.Numerics.Pi / 180.0,
         Ending_Angle * Ada.Numerics.Pi / 180.0,
         Counter_Clockwise);
   end Arc_Degrees;

   ------------
   --  Arc_To --g
   ------------

   procedure Arc_To
     (Graphics           : in out Graphics_2D_Type;
      X_1, Y_1, X_2, Y_2 : in     Integer;
      Radius             : in     Integer)
   is
   begin
      Graphics.Execute
      ("arcTo(" &
       X_1'Img &
       "," &
       Y_1'Img &
       "," &
       X_2'Img &
       "," &
       Y_2'Img &
       "," &
       Radius'Img &
       ");");
   end Arc_To;

   ----------------------
   -- Is_Point_In_Path --
   ----------------------

   function Is_Point_In_Path
     (Graphics : Graphics_2D_Type;
      X, Y     : Integer) return Boolean
   is
   begin
      return Graphics.Execute ("isPointInPath(" & X'Img & "," & Y'Img & ");") =
        "true";
   end Is_Point_In_Path;

   -----------
   -- Scale --
   -----------

   procedure Scale
     (Graphics      : in out Graphics_2D_Type;
      Width, Height :        Float)
   is
   begin
      Graphics.Execute ("scale (" & Width'Img & "," & Height'Img & ");");
   end Scale;

   --------------------
   -- Rotate_Radians --
   --------------------

   procedure Rotate_Radians
     (Graphics : in out Graphics_2D_Type;
      Radians  :        Float)
   is
   begin
      Graphics.Execute ("rotate(" & Radians'Img & ");");
   end Rotate_Radians;

   --------------------
   -- Rotate_Degrees --
   --------------------

   procedure Rotate_Degrees
     (Graphics : in out Graphics_2D_Type;
      Degrees  :        Float)
   is
   begin
      Rotate_Radians (Graphics, Degrees * Ada.Numerics.Pi / 180.0);
   end Rotate_Degrees;

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Graphics : in out Graphics_2D_Type; X, Y : Integer) is
   begin
      Graphics.Execute ("translate(" & X'Img & "," & Y'Img & ");");
   end Translate;

   ---------------
   -- Transform --
   ---------------

   procedure Transform
     (Graphics                          : in out Graphics_2D_Type;
      Scale_Horizontal, Skew_Horizontal : in     Float;
      Scale_Vertical, Skew_Vertical     : in     Float;
      Move_Horizontal, Move_Vertical    : in     Float)
   is
   begin
      Graphics.Execute
      ("transform(" &
       Scale_Horizontal'Img &
       "," &
       Skew_Horizontal'Img &
       "," &
       Scale_Vertical'Img &
       "," &
       Skew_Vertical'Img &
       "," &
       Move_Horizontal'Img &
       "," &
       Move_Vertical'Img &
       ");");
   end Transform;

   -------------------
   -- Set_Transform --
   -------------------

   procedure Set_Transform
     (Graphics                          : in out Graphics_2D_Type;
      Scale_Horizontal, Skew_Horizontal : in     Float;
      Scale_Vertical, Skew_Vertical     : in     Float;
      Move_Horizontal, Move_Vertical    : in     Float)
   is
   begin
      Graphics.Execute
      ("setTransform(" &
       Scale_Horizontal'Img &
       "," &
       Skew_Horizontal'Img &
       "," &
       Scale_Vertical'Img &
       "," &
       Skew_Vertical'Img &
       "," &
       Move_Horizontal'Img &
       "," &
       Move_Vertical'Img &
       ");");
   end Set_Transform;

   procedure Fill_Text
     (Graphics   : in out Graphics_2D_Type;
      Text       : in     String;
      X, Y       : in     Integer;
      Max_Length : in     Natural := 0)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Graphics.Connection_ID,
         "var p=new PIXI.Text('" &
         Escape_Quotes (Text) &
         "'); " &
         "p.x=" &
         X'Img &
         "; p.y=" &
         Y'Img &
         "; " &
         "gnoga['" &
         Graphics.ID &
         "'].addChild(p);");
   end Fill_Text;

   procedure Stroke_Text
     (Graphics   : in out Graphics_2D_Type;
      Renderer   : in out Renderer_Type;
      Text       : in     String;
      X, Y       : in     Integer;
      Max_Length : in     Natural := 0)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Graphics.Connection_ID,
         "var p=new PIXI.Text('" &
         Escape_Quotes (Text) &
         "', {" &
         "fontFamily : 'Arial'," &
         "fontSize : '36px'," &
         "fontStyle : 'italic'," &
         "fontWeight : 'bold'," &
         "fill : 'yellow'," &
         "stroke : 'blue'," &
         "strokeThickness : 5," &
         "dropShadow : true," &
         "dropShadowColor : '#000000'," &
         "dropShadowAngle : Math.PI / 6," &
         "dropShadowDistance : 6," &
         "wordWrap : true," &
         "wordWrapWidth : 440}); " &
         "p.x=" &
         X'Img &
         "; p.y=" &
         Y'Img &
         "; " &
         "gnoga['" &
         Graphics.ID &
         "'].addChild(p);");
      Graphics.Stroke (Renderer);
   end Stroke_Text;

   function Measure_Text_Width
     (Graphics : Graphics_2D_Type;
      Text     : String) return Float
   is
   begin
      return Float'Value
          (Gnoga.Server.Connection.Execute_Script
             (ID     => Graphics.Connection_ID,
              Script =>
                "gnoga['" &
                Graphics.ID &
                "'].measureText (" &
                Escape_Quotes (Text) &
                ").width"));
   end Measure_Text_Width;

   ----------------
   -- Draw_Image --
   ----------------

   procedure Draw_Image
     (Graphics : in out Graphics_2D_Type'Class;
      Image    : in out Gnoga.Gui.Element.Element_Type'Class;
      X, Y     : in     Integer)
   is
   begin
      Graphics.Execute
      ("drawImage (" & Image.jQuery & ".get(0)," & X'Img & "," & Y'Img & ")");
   end Draw_Image;

   -----------------------
   -- Create_Image_Data --
   -----------------------

   procedure Create_Image_Data
     (Graphics      : in out Graphics_2D_Type;
      Image_Data    : in out Image_Data_Type'Class;
      Width, Height : in     Integer)
   is
      GID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Image_Data.ID (GID, Gnoga.Types.Gnoga_ID);
      Image_Data.Connection_ID (Graphics.Connection_ID);

      Gnoga.Server.Connection.Execute_Script
        (Graphics.Connection_ID,
         "gnoga['" &
         GID &
         "']=gnoga['" &
         Graphics.ID &
         "'].createImageData(" &
         Width'Img &
         "," &
         Height'Img &
         ")");
   end Create_Image_Data;

   --------------------------
   -- String_To_Pixel_Data --
   --------------------------

   function String_To_Pixel_Data
     (Value         : String;
      Width, Height : Positive) return Gnoga.Types.Pixel_Data_Type
   is
      use Ada.Strings.Fixed;
      use Gnoga.Types;

      D : Gnoga.Types.Pixel_Data_Type (1 .. Width, 1 .. Height);

      S : Integer := Value'First;
      F : Integer := Value'First - 1;

      function Split return Color_Type;
      --  Split string and extract values

      function Split return Color_Type is
      begin
         S := F + 1;
         F := Index (Source => Value, Pattern => ",", From => S);

         if F = 0 then
            F := Value'Last;
            return Color_Type'Value (Value (S .. F));
         end if;

         return Color_Type'Value (Value (S .. F - 1));
      end Split;
   begin
      for X in 1 .. Width loop
         for Y in 1 .. Height loop
            D (X, Y) := (Split, Split, Split, Split);
         end loop;
      end loop;

      return D;
   end String_To_Pixel_Data;

   -----------
   -- Pixel --
   -----------

   function Pixel
     (Graphics : Graphics_2D_Type;
      X, Y     : Integer) return Gnoga.Types.Pixel_Type
   is
      D : constant String :=
        Gnoga.Server.Connection.Execute_Script
          (Graphics.Connection_ID,
           "Array.prototype.join.call" &
           "(gnoga['" &
           Graphics.ID &
           "'].getImageData(" &
           X'Img &
           "," &
           Y'Img &
           ",1,1).data);");

      P : constant Gnoga.Types.Pixel_Data_Type :=
        String_To_Pixel_Data (D, 1, 1);
   begin
      return P (1, 1);
   end Pixel;

   procedure Pixel
     (Graphics : in out Graphics_2D_Type;
      X, Y     : in     Integer;
      Color    : in     Gnoga.Types.Pixel_Type)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Graphics.Connection_ID,
         "var p=gnoga['" &
         Graphics.ID &
         "'].createImageData(1,1); " &
         "p.data.set (('" &
         Color.Red'Img &
         "," &
         Color.Green'Img &
         "," &
         Color.Blue'Img &
         "," &
         Color.Alpha'Img &
         "').split(',')); " &
         "gnoga['" &
         Graphics.ID &
         "'].putImageData(p," &
         X'Img &
         "," &
         Y'Img &
         ")");
   end Pixel;

   procedure Pixel
     (Graphics : in out Graphics_2D_Type;
      X, Y     : in     Integer;
      Color    : in     Gnoga.Types.Colors.Color_Enumeration)
   is
      C : constant Gnoga.Types.Pixel_Type :=
        Gnoga.Types.To_Pixel (Gnoga.Types.Colors.To_RGBA (Color));
   begin
      Pixel (Graphics, X, Y, C);
   end Pixel;

   --------------------
   -- Get_Image_Data --
   --------------------

   procedure Get_Image_Data
     (Graphics      : in out Graphics_2D_Type;
      Image_Data    : in out Image_Data_Type'Class;
      Left, Top     : in     Integer;
      Width, Height : in     Integer)
   is
      GID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Image_Data.ID (GID, Gnoga.Types.Gnoga_ID);
      Image_Data.Connection_ID (Graphics.Connection_ID);

      Gnoga.Server.Connection.Execute_Script
        (Graphics.Connection_ID,
         "gnoga['" &
         GID &
         "']=gnoga['" &
         Graphics.ID &
         "'].getImageData(" &
         Left'Img &
         "," &
         Top'Img &
         "," &
         Width'Img &
         "," &
         Height'Img &
         ")");
   end Get_Image_Data;

   --------------------
   -- Put_Image_Data --
   --------------------

   procedure Put_Image_Data
     (Graphics   : in out Graphics_2D_Type;
      Image_Data : in out Image_Data_Type'Class;
      Left, Top  : in     Integer)
   is
   begin
      Graphics.Execute
      ("putImageData(gnoga['" &
       Image_Data.ID &
       "']," &
       Left'Img &
       "," &
       Top'Img &
       ")");
   end Put_Image_Data;

   -----------
   -- Width --
   -----------

   function Width0 (Image_Data : Image_Data_Type) return Natural is
   begin
      return Image_Data.Property ("width");
   end Width0;

   ------------
   -- Height --
   ------------

   function Height0 (Image_Data : Image_Data_Type) return Natural is
   begin
      return Image_Data.Property ("height");
   end Height0;

   ----------
   -- Data --
   ----------

   procedure Data (Image_Data : in out Image_Data_Type; Value : in String) is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Image_Data.Connection_ID,
         "gnoga['" &
         Image_Data.ID &
         "'].data.set (('" &
         Value &
         "').split(','))");
   end Data;

   function Data (Image_Data : Image_Data_Type) return String is
   begin
      return Gnoga.Server.Connection.Execute_Script
          (Image_Data.Connection_ID,
           "Array.prototype.join.call (gnoga['" & Image_Data.ID & "'].data)");
   end Data;

   procedure Data
     (Image_Data : in out Image_Data_Type;
      Value      : in     Gnoga.Types.Pixel_Data_Type)
   is
      use Ada.Strings.Unbounded;

      C : constant String := ",";
      S : String (1 .. 16 * Value'Length (1) * Value'Length (2));
      P : Positive        := 1;
   begin
      for X in 1 .. Value'Length (1) loop
         for Y in 1 .. Value'Length (2) loop
            declare
               T : constant String :=
                 Gnoga.Left_Trim (Value (X, Y).Red'Img) &
                 C &
                 Gnoga.Left_Trim (Value (X, Y).Green'Img) &
                 C &
                 Gnoga.Left_Trim (Value (X, Y).Blue'Img) &
                 C &
                 Gnoga.Left_Trim (Value (X, Y).Alpha'Img) &
                 C;
            begin
               S (P .. P + T'Length - 1) := T;
               P                         := P + T'Length;
            end;
         end loop;
      end loop;

      Data (Image_Data, S (1 .. P - 2));
   end Data;

   function Data
     (Image_Data : Image_Data_Type) return Gnoga.Types.Pixel_Data_Type
   is
   begin
      return String_To_Pixel_Data
          (Data (Image_Data),
           Image_Data.Width,
           Image_Data.Height);
   end Data;

   ------------------
   -- New_From_XPM --
   ------------------

   procedure New_From_XPM
     (Image     : out Gnoga.Types.Pixel_Data_Access;
      File_Name :     String)
   is
      use Parsers.Multiline_Source.XPM;
      use Parsers.Multiline_Source.Text_IO;
      use Ada.Text_IO;
      function To_Red
        (Value : RGB_Color) return Gnoga.Types.Color_Type is
        (Gnoga.Types.Color_Type (Value / 16#10000#));
      function To_Green
        (Value : RGB_Color) return Gnoga.Types.Color_Type is
        (Gnoga.Types.Color_Type ((Value mod 16#10000#) / 16#100#));
      function To_Blue
        (Value : RGB_Color) return Gnoga.Types.Color_Type is
        (Gnoga.Types.Color_Type (Value mod 16#100#));
      XPM_File : aliased File_Type;
   begin
      Open (XPM_File, In_File, File_Name);
      declare
         XPM_Source : aliased Source (XPM_File'Access);
         XPM_Header : constant Descriptor         := Get (XPM_Source'Access);
         XMP_Map    : constant Color_Tables.Table :=
           Get (XPM_Source'Access, XPM_Header);
         XPM_Image : constant Pixel_Buffer :=
           Get (XPM_Source'Access, XPM_Header, XMP_Map);
      begin
         Image :=
           new Gnoga.Types
             .Pixel_Data_Type
           (1 .. XPM_Header.Width,
            1 .. XPM_Header.Height);
         for X in Image'Range (1) loop
            for Y in Image'Range (2) loop
               Image (X, Y) :=
                 (To_Red (XPM_Image (X, Y)),
                  To_Green (XPM_Image (X, Y)),
                  To_Blue (XPM_Image (X, Y)),
                  Alpha => 255);
            end loop;
         end loop;
      end;
      Close (XPM_File);
   end New_From_XPM;

   ----------
   -- Save --
   ----------

   procedure Save (Graphics : in out Graphics_2D_Type) is
   begin
      Graphics.Execute ("save();");
   end Save;

   -------------
   -- Restore --
   -------------

   procedure Restore (Graphics : in out Graphics_2D_Type) is
   begin
      Graphics.Execute ("restore();");
   end Restore;
end Gnoga.Gui.Plugin.Pixi.Graphics;
