------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--   G N O G A . G U I . E L E M E N T . C A N V A S . C O N T E X T _ 2 D  --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Numerics;

with Gnoga.Server.Connection;

package body Gnoga.Gui.Element.Canvas.Context_2D is

   ----------------------------
   -- Get_Drawing_Context_2D --
   ----------------------------

   procedure Get_Drawing_Context_2D (Context : in out Context_2D_Type;
                                     Canvas  : in out Canvas_Type'Class)
   is
      GID : String := Gnoga.Server.Connection.New_GID;
   begin
      Context.Context_ID := Ada.Strings.Unbounded.To_Unbounded_String (GID);
      Context.Connection_ID := Canvas.Connection_ID;

      Gnoga.Server.Connection.Execute_Script (Context.Connection_ID,
                                        "gnoga['" & GID & "']=" &
                                          Canvas.jQuery &
                                          ".get(0).getContext('2d');");
   end Get_Drawing_Context_2D;

   ----------------
   -- Fill_Color --
   ----------------

   procedure Fill_Color (Context : in out Context_2D_Type;
                         Value   : in     Gnoga.Types.RGBA_Type)
   is
   begin
      Context.Fill_Color (Gnoga.Types.To_String (Value));
   end Fill_Color;

   procedure Fill_Color (Context : in out Context_2D_Type;
                         Value   : in     String)
   is
   begin
      Context.Property ("fillStyle", Value);
   end Fill_Color;

   -------------------
   -- Fill_Gradient --
   -------------------

   procedure Fill_Gradient (Context : in out Context_2D_Type;
                            Value   : in out Gradient_Type'Class)
   is
   begin
      Context.Execute ("fillStyle=gnoga['" &
                         Ada.Strings.Unbounded.To_String (Value.Context_ID) &
                         "'];");
   end Fill_Gradient;

   -------------------
   -- Fill_Pattern --
   -------------------

   procedure Fill_Pattern (Context : in out Context_2D_Type;
                           Value   : in out Pattern_Type'Class)
   is
   begin
      Context.Execute ("fillStyle=gnoga['" &
                         Ada.Strings.Unbounded.To_String (Value.Context_ID) &
                         "'];");
   end Fill_Pattern;

   ------------------
   -- Stroke_Color --
   ------------------

   procedure Stroke_Color (Context : in out Context_2D_Type;
                           Value   : in     Gnoga.Types.RGBA_Type)
   is
   begin
      Context.Stroke_Color (Gnoga.Types.To_String (Value));
   end Stroke_Color;

   procedure Stroke_Color (Context : in out Context_2D_Type;
                           Value   : in     String)
   is
   begin
      Context.Property ("strokeStyle", Value);
   end Stroke_Color;

   ---------------------
   -- Stroke_Gradient --
   ---------------------

   procedure Stroke_Gradient (Context : in out Context_2D_Type;
                              Value   : in out Gradient_Type'Class)
   is
   begin
      Context.Execute ("strokeStyle=gnoga['" &
                         Ada.Strings.Unbounded.To_String (Value.Context_ID) &
                         "'];");
   end Stroke_Gradient;

   -------------------
   -- Stroke_Pattern --
   -------------------

   procedure Stroke_Pattern (Context : in out Context_2D_Type;
                             Value   : in out Pattern_Type'Class)
   is
   begin
      Context.Execute ("strokeStyle=gnoga['" &
                         Ada.Strings.Unbounded.To_String (Value.Context_ID) &
                         "'];");
   end Stroke_Pattern;

   ------------------
   -- Shadow_Color --
   ------------------

   procedure Shadow_Color (Context : in out Context_2D_Type;
                           Value   : in     Gnoga.Types.RGBA_Type)
   is
   begin
      Context.Shadow_Color (Gnoga.Types.To_String (Value));
   end Shadow_Color;

   procedure Shadow_Color (Context : in out Context_2D_Type;
                           Value   : in     String)
   is
   begin
      Context.Property ("shadowColor", Value);
   end Shadow_Color;

   -----------------
   -- Shadow_Blur --
   -----------------

   procedure Shadow_Blur (Context : in out Context_2D_Type;
                          Value   : in     Integer)
   is
   begin
      Context.Property ("shadowBlur", Value);
   end Shadow_Blur;

   ---------------------
   -- Shadow_Offset_X --
   ---------------------

   procedure Shadow_Offset_X (Context : in out Context_2D_Type;
                              Value   : in     Integer)
   is
   begin
      Context.Property ("shadowOffsetX", Value);
   end Shadow_Offset_X;

   ---------------------
   -- Shadow_Offset_Y --
   ---------------------

   procedure Shadow_Offset_Y (Context : in out Context_2D_Type;
                              Value   : in     Integer)
   is
   begin
      Context.Property ("shadowOffsetY", Value);
   end Shadow_Offset_Y;

   --------------
   -- Line_Cap --
   --------------

   procedure Line_Cap (Context : in out Context_2D_Type;
                       Value   : in     Line_Cap_Type)
   is
   begin
      Context.Property ("lineCap", Value'Img);
   end Line_Cap;

   ---------------
   -- Line_Join --
   ---------------

   procedure Line_Join (Context : in out Context_2D_Type;
                        Value   : in     Line_Join_Type)
   is
   begin
      Context.Property ("lineJoin", Value'Img);
   end Line_Join;

   ----------------
   -- Line_Width --
   ----------------

   procedure Line_Width (Context : in out Context_2D_Type;
                         Value   : in     Integer)
   is
   begin
      Context.Property ("lineWidth", Value);
   end Line_Width;

   -----------------
   -- Miter_Limit --
   -----------------

   procedure Miter_Limit (Context : in out Context_2D_Type;
                          Value   : in     Positive)
   is
   begin
      Context.Property ("miterLimit", Value);
   end Miter_Limit;

   -------------------
   -- Set_Line_Dash --
   -------------------

   procedure Set_Line_Dash (Context   : in out Context_2D_Type;
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
            return Natural'Image
              (Dash_List (Index)) & ',' & Dash_String (Index + 1);
         end if;
      end Dash_String;
   begin
      Context.Execute
        ("setLineDash([" & Dash_String (Dash_List'First) & "]);");
   end Set_Line_Dash;

   ----------
   -- Font --
   ----------

   procedure Font (Context : in out Context_2D_Type;
                   Family  : in     String            := "sans-serif";
                   Height  : in     String            := "10px";
                   Style   : in     Font_Style_Type   := Normal;
                   Weight  : in     Font_Weight_Type  := Weight_Normal;
                   Variant : in     Font_Variant_Type := Normal)
   is
      W : String := Weight'Img;
   begin
      Context.Property ("font", Style'Img & " " & Variant'Img & " " &
                          W (W'First + 7 .. W'Last) & " " &
                          Height & " " & Family);
   end Font;

   procedure Font (Context     : in out Context_2D_Type;
                   System_Font : in     System_Font_Type)
   is
   begin
      case System_Font is
         when Caption | Icon | Menu =>
            Context.Property ("font", System_Font'Img);
         when Message_Box =>
            Context.Property ("font", "message-box");
         when Small_Caption =>
            Context.Property ("font", "small-caption");
         when Status_Bar =>
            Context.Property ("font", "status-bar");
      end case;
   end Font;

   --------------------
   -- Text_Alignment --
   --------------------

   procedure Text_Alignment (Context : in out Context_2D_Type;
                             Value   : in     Alignment_Type)
   is
      V : String := Value'Img;
   begin
      case Value is
         when Left | Right | Center =>
            Context.Property ("textAlign", V);
         when At_Start | To_End =>
            Context.Property ("textAlign", V ((V'First + 3) .. V'Last));
      end case;
   end Text_Alignment;

   -------------------
   -- Text_Baseline --
   -------------------

   procedure Text_Baseline (Context : in out Context_2D_Type;
                            Value   : in     Baseline_Type)
   is
   begin
      Context.Property ("textBaseline", Value'Img);
   end Text_Baseline;

   ------------------
   -- Global_Alpha --
   ------------------

   procedure Global_Alpha (Context : in out Context_2D_Type;
                           Alpha   : in     Gnoga.Types.Alpha_Type)
   is
   begin
      Context.Property ("globalAlpha", Alpha'Img);
   end Global_Alpha;

   --------------------------------
   -- Glogal_Composite_Operation --
   --------------------------------

   procedure Glogal_Composite_Operation
     (Context : in out Context_2D_Type;
      Value   : in     Composite_Method_Type)
   is
      V : String := Value'Img;
   begin
      case Value is
         when Lighter | Copy =>
            Context.Property ("globalCompositeOperation", V);
         when Source_Over | Source_Atop | Source_In | Source_Out =>
            Context.Property ("globalCompositeOperation",
                              "source-" & V ((V'First + 7) .. V'Last));
         when Destination_Over | Destination_Atop |
              Destination_In | Destination_Out =>
            Context.Property ("globalCompositeOperation",
                              "destination-" & V ((V'First + 12) .. V'Last));
         when Xor_Copy =>
            Context.Property ("globalCompositeOperation", "xor");
      end case;
   end Glogal_Composite_Operation;

   ----------------------------
   -- Create_Linear_Gradient --
   ----------------------------

   procedure Create_Linear_Gradient (Gradient : in out Gradient_Type;
                                     Context  : in out Context_2D_Type'Class;
                                     X_1      : in     Integer;
                                     Y_1      : in     Integer;
                                     X_2      : in     Integer;
                                     Y_2      : in     Integer)
   is
      GID : String := Gnoga.Server.Connection.New_GID;
   begin
      Gradient.Context_ID := Ada.Strings.Unbounded.To_Unbounded_String (GID);
      Gradient.Connection_ID := Context.Connection_ID;

      Gnoga.Server.Connection.Execute_Script
        (Context.Connection_ID,
         "gnoga['" & GID & "']=" &
         "gnoga['" &
           Ada.Strings.Unbounded.To_String (Context.Context_ID) &
           "'].createLinearGradient(" &
           X_1'Img & "," &
           Y_1'Img & "," &
           X_2'Img & "," &
           Y_2'Img & ");");
   end Create_Linear_Gradient;

   ----------------------------
   -- Create_Radial_Gradient --
   ----------------------------

   procedure Create_Radial_Gradient (Gradient : in out Gradient_Type;
                                     Context  : in out Context_2D_Type'Class;
                                     X_1      : in     Integer;
                                     Y_1      : in     Integer;
                                     R_1      : in     Integer;
                                     X_2      : in     Integer;
                                     Y_2      : in     Integer;
                                     R_2      : in     Integer)
   is
      GID : String := Gnoga.Server.Connection.New_GID;
   begin
      Gradient.Context_ID := Ada.Strings.Unbounded.To_Unbounded_String (GID);
      Gradient.Connection_ID := Context.Connection_ID;

      Gnoga.Server.Connection.Execute_Script
        (Context.Connection_ID,
         "gnoga['" & GID & "']=" &
         "gnoga['" &
           Ada.Strings.Unbounded.To_String (Context.Context_ID) &
           "'].createRadialGradient(" &
           X_1'Img & "," &
           Y_1'Img & "," &
           R_1'Img & "," &
           X_2'Img & "," &
           Y_2'Img & "," &
           R_2'Img & ");");
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
      Gradient.Execute ("addColorStop (" & Position'Img &
                          ", '" & Color & "');");
   end Add_Color_Stop;

   ----------------------------
   -- Create_Radial_Gradient --
   ----------------------------

   procedure Create_Pattern (Pattern        : in out Pattern_Type;
                             Context        : in out Context_2D_Type'Class;
                             Image          : in out Element_Type'Class;
                             Repeat_Pattern : in     Repeat_Type := Repeat)
   is
      GID : String := Gnoga.Server.Connection.New_GID;

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
      Pattern.Context_ID := Ada.Strings.Unbounded.To_Unbounded_String (GID);
      Pattern.Connection_ID := Context.Connection_ID;

      Gnoga.Server.Connection.Execute_Script
        (Context.Connection_ID,
         "gnoga['" & GID & "']=" &
         "gnoga['" &
           Ada.Strings.Unbounded.To_String (Context.Context_ID) &
           "'].createPattern(" & Image.jQuery & ".get(0), '" &
           Repeat_to_String & "');");
   end Create_Pattern;

   ---------------
   -- Rectangle --
   ---------------

   procedure Rectangle (Context   : in out Context_2D_Type;
                        Rectangle : in     Gnoga.Types.Rectangle_Type)
   is
   begin
      Context.Execute ("rect (" &
                         Rectangle.X'Img & "," &
                         Rectangle.Y'Img & "," &
                         Rectangle.Width'Img & "," &
                         Rectangle.Height'Img &
                         ");");
   end Rectangle;

   --------------------
   -- Fill_Rectangle --
   --------------------

   procedure Fill_Rectangle (Context   : in out Context_2D_Type;
                               Rectangle : in     Gnoga.Types.Rectangle_Type)
   is
   begin
      Context.Execute ("fillRect (" &
                         Rectangle.X'Img & "," &
                         Rectangle.Y'Img & "," &
                         Rectangle.Width'Img & "," &
                         Rectangle.Height'Img &
                         ");");
   end Fill_Rectangle;

   ----------------------
   -- Stroke_Rectangle --
   ----------------------

   procedure Stroke_Rectangle (Context   : in out Context_2D_Type;
                               Rectangle : in     Gnoga.Types.Rectangle_Type)
   is
   begin
      Context.Execute ("strokeRect (" &
                         Rectangle.X'Img & "," &
                         Rectangle.Y'Img & "," &
                         Rectangle.Width'Img & "," &
                         Rectangle.Height'Img &
                         ");");
   end Stroke_Rectangle;

   ---------------------
   -- Clear_Rectangle --
   ---------------------

   procedure Clear_Rectangle (Context   : in out Context_2D_Type;
                              Rectangle : in     Gnoga.Types.Rectangle_Type)
   is
   begin
      Context.Execute ("clearRect (" &
                         Rectangle.X'Img & "," &
                         Rectangle.Y'Img & "," &
                         Rectangle.Width'Img & "," &
                         Rectangle.Height'Img &
                         ");");
   end Clear_Rectangle;

   ----------
   -- Fill --
   ----------

   procedure Fill (Context : in out Context_2D_Type) is
   begin
      Context.Execute ("fill();");
   end Fill;

   ------------
   -- Stroke --
   ------------

   procedure Stroke (Context : in out Context_2D_Type) is
   begin
      Context.Execute ("stroke();");
   end Stroke;

   ----------------
   -- Begin_Path --
   ----------------

   procedure Begin_Path (Context : in out Context_2D_Type) is
   begin
      Context.Execute ("beginPath();");
   end Begin_Path;

   --------------
   --  Move_To --
   --------------

   procedure Move_To (Context : in out Context_2D_Type; X, Y : Integer) is
   begin
      Context.Execute ("moveTo(" & X'Img & "," & Y'Img & ");");
   end Move_To;

   ----------------
   -- Close_Path --
   ----------------

   procedure Close_Path (Context : in out Context_2D_Type) is
   begin
      Context.Execute ("closePath()");
   end Close_Path;

   --------------
   --  Line_To --
   --------------

   procedure Line_To (Context : in out Context_2D_Type; X, Y : Integer) is
   begin
      Context.Execute ("lineTo(" & X'Img & "," & Y'Img & ");");
   end Line_To;

   ----------
   -- Clip --
   ----------

   procedure Clip (Context : in out Context_2D_Type) is
   begin
      Context.Execute ("clip();");
   end Clip;

   -------------------------
   -- Quadractic_Curve_To --
   -------------------------

   procedure Quadractic_Curve_To (Context           : in out Context_2D_Type;
                                  CP_X, CP_Y, X, Y  : Integer)
   is
   begin
      Context.Execute ("quadraticCurveTo(" &
                         CP_X'Img & "," &
                         CP_Y'Img & "," &
                         X'Img & "," &
                         Y'Img & ");");
   end Quadractic_Curve_To;

   ---------------------
   -- Bezier_Curve_To --
   ---------------------

   procedure Bezier_Curve_To
     (Context                        : in out Context_2D_Type;
      CP_X_1, CP_Y_1, CP_X_2, CP_Y_2 : in     Integer;
      X, Y                           : in     Integer)
   is
   begin
      Context.Execute ("bezierCurveTo(" &
                         CP_X_1'Img & "," & CP_Y_1'Img & "," &
                         CP_X_2'Img & "," & CP_Y_2'Img & "," &
                         X'Img & "," &
                         Y'Img & ");");
   end Bezier_Curve_To;

   -----------------
   -- Arc_Radians --
   -----------------

   procedure Arc_Radians
     (Context                      : in out Context_2D_Type;
      X, Y                         : in Integer;
      Radius                       : in Integer;
      Starting_Angle, Ending_Angle : in Float;
      Counter_Clockwise            : in Boolean := False)
   is
   begin
      Context.Execute ("arc(" &
                         X'Img & "," & Y'Img & "," &
                         Radius'Img & "," &
                         Starting_Angle'Img & "," & Ending_Angle'Img & "," &
                         Counter_Clockwise'Img & ");");
   end Arc_Radians;

   -----------------
   -- Arc_Degrees --
   -----------------

   procedure Arc_Degrees
     (Context                      : in out Context_2D_Type;
      X, Y                         : in Integer;
      Radius                       : in Integer;
      Starting_Angle, Ending_Angle : in Float;
      Counter_Clockwise            : in Boolean := False)
   is
   begin
      Arc_Radians (Context, X, Y, Radius,
                   Starting_Angle * Ada.Numerics.Pi / 180.0,
                   Ending_Angle * Ada.Numerics.Pi / 180.0,
                   Counter_Clockwise);
   end Arc_Degrees;

   ------------
   -- Arc_To --
   ------------

   procedure Arc_To
     (Context            : in out Context_2D_Type;
      X_1, Y_1, X_2, Y_2 : in Integer;
      Radius             : in Integer)
   is
   begin
      Context.Execute ("arcTo(" &
                         X_1'Img & "," & Y_1'Img & "," &
                         X_2'Img & "," & Y_2'Img & "," &
                         Radius'Img & ");");
   end Arc_To;

   ----------------------
   -- Is_Point_In_Path --
   ----------------------

   function Is_Point_In_Path (Context : Context_2D_Type; X, Y : Integer)
                              return Boolean
   is
   begin
      return Context.Execute
        ("isPointInPath(" & X'Img & "," & Y'Img & ");") = "true";
   end Is_Point_In_Path;

   -----------
   -- Scale --
   -----------

   procedure Scale (Context : in out Context_2D_Type; Width, Height : Float)
   is
   begin
      Context.Execute ("scale (" & Width'Img & "," & Height'Img & ");");
   end Scale;

   --------------------
   -- Rotate_Radians --
   --------------------

   procedure Rotate_Radians (Context : in out Context_2D_Type; Radians : Float)
   is
   begin
      Context.Execute ("rotate(" & Radians'Img & ");");
   end Rotate_Radians;

   --------------------
   -- Rotate_Degrees --
   --------------------

   procedure Rotate_Degrees (Context : in out Context_2D_Type; Degrees : Float)
   is
   begin
      Rotate_Radians (Context, Degrees * Ada.Numerics.Pi / 180.0);
   end Rotate_Degrees;

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Context : in out Context_2D_Type; X, Y : Integer) is
   begin
      Context.Execute ("translate(" & X'Img & "," & Y'Img & ");");
   end Translate;

   ---------------
   -- Transform --
   ---------------

   procedure Transform
     (Context                           : in out Context_2D_Type;
      Scale_Horizontal, Skew_Horizontal : in     Float;
      Scale_Vertical,   Skew_Vertical   : in     Float;
      Move_Horizontal,  Move_Vertical   : in     Float)
   is
   begin
      Context.Execute
        ("transform(" &
           Scale_Horizontal'Img & "," & Skew_Horizontal'Img & "," &
           Scale_Vertical'Img & "," & Skew_Vertical'Img & "," &
           Move_Horizontal'Img & "," & Move_Vertical'Img & ");");
   end Transform;

   -------------------
   -- Set_Transform --
   -------------------

   procedure Set_Transform
     (Context                           : in out Context_2D_Type;
      Scale_Horizontal, Skew_Horizontal : in     Float;
      Scale_Vertical,   Skew_Vertical   : in     Float;
      Move_Horizontal,  Move_Vertical   : in     Float)
   is
   begin
      Context.Execute
        ("setTransform(" &
           Scale_Horizontal'Img & "," & Skew_Horizontal'Img & "," &
           Scale_Vertical'Img & "," & Skew_Vertical'Img & "," &
           Move_Horizontal'Img & "," & Move_Vertical'Img & ");");
   end Set_Transform;

   procedure Fill_Text (Context    : in out Context_2D_Type;
                        Text       : in     String;
                        X, Y       : in     Integer;
                        Max_Length : in     Natural := 0)
   is
      function Max_To_String return String;

      function Max_To_String return String is
      begin
         if Max_Length > 0 then
            return "," & Max_Length'Img;
         else
            return "";
         end if;
      end Max_To_String;
   begin
      Context.Execute ("fillText(""" & Escape_Quotes (Text) & """," &
                         X'Img & "," & Y'Img & Max_To_String & ");");
   end Fill_Text;

   procedure Stroke_Text (Context    : in out Context_2D_Type;
                          Text       : in     String;
                          X, Y       : in     Integer;
                          Max_Length : in     Natural := 0)
   is
      function Max_To_String return String;

      function Max_To_String return String is
      begin
         if Max_Length > 0 then
            return "," & Max_Length'Img;
         else
            return "";
         end if;
      end Max_To_String;
   begin
      Context.Execute ("strokeText(""" & Escape_Quotes (Text) & """," &
                         X'Img & "," & Y'Img & Max_To_String & ");");
   end Stroke_Text;

   function Measure_Text_Width (Context : Context_2D_Type;
                                Text    : String)
                                return Float
   is
   begin
      return Float'Value (Gnoga.Server.Connection.Execute_Script
        (ID     => Context.Connection_ID,
         Script => "gnoga['" &
           Ada.Strings.Unbounded.To_String (Context.Context_ID) &
           "'].measureText (" & Escape_Quotes (Text) & ").width"));
   end Measure_Text_Width;

   ----------------
   -- Draw_Image --
   ----------------

   procedure Draw_Image (Context : in out Context_2D_Type'Class;
                         Image   : in out Element_Type'Class;
                         X, Y    : in     Integer)
   is
   begin
      Context.Execute ("drawImage (" & Image.jQuery & ".get(0)," &
                         X'Img & "," & Y'Img & ")");
   end Draw_Image;

   -----------------------
   -- Create_Image_Data --
   -----------------------

   procedure Create_Image_Data (Context       : in out Context_2D_Type;
                                Image_Data    : in out Image_Data_Type'Class;
                                Width, Height : in     Integer)
   is
      GID : String := Gnoga.Server.Connection.New_GID;
   begin
      Image_Data.Context_ID := Ada.Strings.Unbounded.To_Unbounded_String (GID);
      Image_Data.Connection_ID := Context.Connection_ID;

      Gnoga.Server.Connection.Execute_Script
        (Context.Connection_ID, "gnoga['" & GID & "']=gnoga['" &
           Context.ID & "'].createImageData(" &
           Width'Img & "," & Height'Img & ")");
   end Create_Image_Data;

   --------------------
   -- Get_Image_Data --
   --------------------

   procedure Get_Image_Data (Context       : in out Context_2D_Type;
                             Image_Data    : in out Image_Data_Type'Class;
                             Left, Top     : in     Integer;
                             Width, Height : in     Integer)
   is
      GID : String := Gnoga.Server.Connection.New_GID;
   begin
      Image_Data.Context_ID := Ada.Strings.Unbounded.To_Unbounded_String (GID);
      Image_Data.Connection_ID := Context.Connection_ID;

      Gnoga.Server.Connection.Execute_Script
        (Context.Connection_ID, "gnoga['" & GID &
           "']=gnoga['" & Context.ID & "'].getImageData(" &
           Left'Img & "," & Top'Img & "," &
           Width'Img & "," & Height'Img & ")");
   end Get_Image_Data;

   --------------------
   -- Put_Image_Data --
   --------------------

   procedure Put_Image_Data (Context       : in out Context_2D_Type;
                             Image_Data    : in out Image_Data_Type'Class;
                             Left, Top     : in     Integer)
   is
   begin
      Context.Execute ("putImageData(gnoga['" & Image_Data.ID & "']," &
                         Left'Img & "," & Top'Img & ")");
   end Put_Image_Data;

   -----------
   -- Width --
   -----------

   function Width (Image_Data : Image_Data_Type) return Natural
   is
   begin
      return Image_Data.Property ("width");
   end Width;

   ------------
   -- Height --
   ------------

   function Height (Image_Data : Image_Data_Type) return Natural
   is
   begin
      return Image_Data.Property ("height");
   end Height;

   ----------
   -- Data --
   ----------

   procedure Data (Image_Data : in out Image_Data_Type; Value : in String);
   function Data (Image_Data : Image_Data_Type) return String;

   procedure Data (Image_Data : in out Image_Data_Type; Value : in String) is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Image_Data.Connection_ID,
         "gnoga['" & Image_Data.ID & "'].data.set (('" & Value &
           "').split(','))");
   end Data;

   function Data (Image_Data : Image_Data_Type) return String is
   begin
      return Gnoga.Server.Connection.Execute_Script
        (Image_Data.Connection_ID,
         "Array.prototype.join.call (gnoga['" & Image_Data.ID & "'].data)");
   end Data;

   procedure Data (Image_Data : in out Image_Data_Type;
                   Value      : in     Gnoga.Types.Pixel_Data_Type)
   is
      use Ada.Strings.Unbounded;

      C : Unbounded_String := To_Unbounded_String (",");
      S : Unbounded_String;
   begin
      for X in 1 .. Value'Length (1) loop
         for Y in 1 .. Value'Length (2) loop
            S := S &
              Gnoga.Left_Trim (Value (X, Y).Red'Img) & C &
              Gnoga.Left_Trim (Value (X, Y).Green'Img) & C &
              Gnoga.Left_Trim (Value (X, Y).Blue'Img) & C &
              Gnoga.Left_Trim (Value (X, Y).Alpha'Img) & C;
         end loop;
      end loop;

      declare
         R : String := To_String (S);
      begin
         Data (Image_Data, (R (R'First .. R'Last - 1)));
      end;
   end Data;

   function Data (Image_Data : Image_Data_Type)
                  return Gnoga.Types.Pixel_Data_Type
   is
      use Ada.Strings.Fixed;
      use Gnoga.Types;

      Value : String := Data (Image_Data);

      W : Positive := Image_Data.Width;
      H : Positive := Image_Data.Height;

      D : Gnoga.Types.Pixel_Data_Type (1 .. W, 1 .. H);

      S    : Integer   := Value'First;
      F    : Integer   := Value'First - 1;

      function Split return Color_Type;
      --  Split string and extract values

      function Split return Color_Type is
      begin
         S := F + 1;
         F := Index (Source  => Value,
                     Pattern => ",",
                     From    => S);

         if F = 0 then
            F := Value'Last;
         end if;

         return Color_Type'Value (Value (S .. (F - 1)));
      end Split;
   begin
      for X in 1 .. W loop
         for Y in 1 .. H loop
            D (X, Y) := (Split, Split, Split, Split);
         end loop;
      end loop;

      return D;
   end Data;

   ----------
   -- Save --
   ----------

   procedure Save (Context : in out Context_2D_Type) is
   begin
      Context.Execute ("save();");
   end Save;

   -------------
   -- Restore --
   -------------

   procedure Restore (Context : in out Context_2D_Type) is
   begin
      Context.Execute ("restore();");
   end Restore;
end Gnoga.Gui.Element.Canvas.Context_2D;
