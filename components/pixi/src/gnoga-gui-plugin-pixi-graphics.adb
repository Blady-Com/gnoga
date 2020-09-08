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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

with Gnoga.Server.Connection;
with Ada.Strings.Unbounded;
with Ada.Numerics;

package body Gnoga.Gui.Plugin.Pixi.Graphics is

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (Graphics : in out Graphics_Type;
      Parent   : in out Container_Type'Class)
   is
      Graphics_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Graphics.ID (Graphics_ID, Gnoga.Types.Gnoga_ID);
      Graphics.Connection_ID (Parent.Connection_ID);
      Gnoga.Server.Connection.Execute_Script
        (Graphics.Connection_ID,
         "gnoga['" & Graphics_ID & "'] = new PIXI.Graphics();");
      Graphics.Parent (Parent);
   end Create;

   -----------
   -- Alpha --
   -----------

   procedure Alpha
     (Graphics : in out Graphics_Type;
      Value    : in     Gnoga.Types.Alpha_Type)
   is
   begin
      Graphics.Property ("alpha", Float (Value));
   end Alpha;

   -----------
   -- Alpha --
   -----------

   function Alpha
     (Graphics : in Graphics_Type) return Gnoga.Types.Alpha_Type
   is
   begin
      return Gnoga.Types.Alpha_Type (Float'(Graphics.Property ("alpha")));
   end Alpha;

   ----------------
   -- Blend_Mode --
   ----------------

   procedure Blend_Mode
     (Graphics : in out Graphics_Type;
      Value    : in     Blend_Modes_Type)
   is
   begin
      Graphics.Property ("blendMode", Value'Img);
   end Blend_Mode;

   ----------------
   -- Blend_Mode --
   ----------------

   function Blend_Mode (Graphics : in Graphics_Type) return Blend_Modes_Type is
   begin
      return Blend_Modes_Type'Value (Graphics.Property ("blendMode"));
   end Blend_Mode;

   --------------------
   -- Bounds_Padding --
   --------------------

   procedure Bounds_Padding
     (Graphics : in out Graphics_Type;
      Value    : in     Integer)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Bounds_Padding no more available.");
   end Bounds_Padding;

   --------------------
   -- Bounds_Padding --
   --------------------

   function Bounds_Padding (Graphics : in Graphics_Type) return Integer is
   begin
      pragma Compile_Time_Warning (Standard.True, "Bounds_Padding no more available.");
      return 0;
   end Bounds_Padding;

   ----------------
   -- Fill_Alpha --
   ----------------

   procedure Fill_Alpha
     (Graphics : in out Graphics_Type;
      Value    : in     Gnoga.Types.Alpha_Type)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Fill_Alpha no more available.");
   end Fill_Alpha;

   ----------------
   -- Fill_Alpha --
   ----------------

   function Fill_Alpha
     (Graphics : in Graphics_Type) return Gnoga.Types.Alpha_Type
   is
   begin
      return Gnoga.Types.Alpha_Type (Float'(Graphics.Execute ("fill.alpha")));
   end Fill_Alpha;

   -----------
   -- Width --
   -----------

   overriding procedure Width
     (Graphics : in out Graphics_Type;
      Value    : in     Integer)
   is
   begin
      Graphics.Property ("width", Value);
   end Width;

   -----------
   -- Width --
   -----------

   overriding function Width (Graphics : in Graphics_Type) return Integer is
   begin
      return Graphics.Property ("width");
   end Width;

   ------------
   -- Height --
   ------------

   overriding procedure Height
     (Graphics : in out Graphics_Type;
      Value    : in     Integer)
   is
   begin
      Graphics.Property ("height", Value);
   end Height;

   ------------
   -- Height --
   ------------

   overriding function Height (Graphics : in Graphics_Type) return Integer is
   begin
      return Graphics.Property ("height");
   end Height;

   ----------------
   -- Line_Color --
   ----------------

   procedure Line_Color
     (Graphics : in out Graphics_Type;
      Value    : in     Gnoga.Types.Colors.Color_Enumeration)
   is
   begin
      Graphics.Line_Color (Gnoga.Types.Colors.To_RGBA (Value));
   end Line_Color;

   procedure Line_Color
     (Graphics : in out Graphics_Type;
      Value    : in     Gnoga.Types.RGBA_Type)
   is
   begin
      Graphics.Line_Style (Graphics.Line_Width, Value);
   end Line_Color;

   ----------------
   -- Line_Color --
   ----------------

   function Line_Color
     (Graphics : in out Graphics_Type) return Gnoga.Types.RGBA_Type
   is
      Value : constant Natural := Graphics.Execute ("line.color");
   begin
      return
        (Red   => Gnoga.Types.Color_Type (Value / 256 / 256),
         Green => Gnoga.Types.Color_Type ((Value / 256) mod 256),
         Blue  => Gnoga.Types.Color_Type (Value mod 256),
         Alpha => 1.0);
   end Line_Color;

   ----------------
   -- Line_Width --
   ----------------

   procedure Line_Width
     (Graphics : in out Graphics_Type;
      Value    : in     Integer)
   is
   begin
      Graphics.Line_Style (Value, Graphics.Line_Color);
   end Line_Width;

   ----------------
   -- Line_Width --
   ----------------

   function Line_Width (Graphics : in Graphics_Type) return Integer is
   begin
      return Graphics.Execute ("line.width");
   end Line_Width;

   --------------
   -- Position --
   --------------

   procedure Position
     (Graphics    : in     Graphics_Type;
      Row, Column :    out Integer)
   is
   begin
      Row    := Graphics.Property ("y");
      Column := Graphics.Property ("x");
   end Position;

   ---------
   -- Row --
   ---------

   function Row (Graphics : in Graphics_Type) return Integer is
   begin
      return Graphics.Property ("y");
   end Row;

   ------------
   -- Column --
   ------------

   function Column (Graphics : in Graphics_Type) return Integer is
   begin
      return Graphics.Property ("x");
   end Column;

   --------------
   -- Rotation --
   --------------

   procedure Rotation (Graphics : in out Graphics_Type; Value : in Integer) is
   begin
      Graphics.Property ("rotation", Float (Value) * Ada.Numerics.Pi / 180.0);
   end Rotation;

   --------------
   -- Rotation --
   --------------

   function Rotation (Graphics : in Graphics_Type) return Integer is
   begin
      return Integer
          (Float'(Graphics.Property ("rotation")) * 180.0 / Ada.Numerics.Pi);
   end Rotation;

   -----------
   -- Scale --
   -----------

   procedure Scale
     (Graphics    : in out Graphics_Type;
      Row, Column : in     Positive)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Graphics.Connection_ID,
         "gnoga['" &
         Graphics.ID &
         "'].scale = {x:" &
         Column'Img &
         ",y:" &
         Row'Img &
         "};");
   end Scale;

   ---------------
   -- Row_Scale --
   ---------------

   function Row_Scale (Graphics : in Graphics_Type) return Positive is
   begin
      return Graphics.Execute ("scale.y");
   end Row_Scale;

   ------------------
   -- Column_Scale --
   ------------------

   function Column_Scale (Graphics : in Graphics_Type) return Positive is
   begin
      return Graphics.Execute ("scale.x");
   end Column_Scale;

   ----------
   -- Skew --
   ----------

   procedure Skew
     (Graphics    : in out Graphics_Type;
      Row, Column : in     Positive)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Graphics.Connection_ID,
         "gnoga['" &
         Graphics.ID &
         "'].skew = {x:" &
         Column'Img &
         ",y:" &
         Row'Img &
         "};");
   end Skew;

   --------------
   -- Row_Skew --
   --------------

   function Row_Skew (Graphics : in Graphics_Type) return Positive is
   begin
      return Graphics.Execute ("skew.y");
   end Row_Skew;

   -----------------
   -- Column_Skew --
   -----------------

   function Column_Skew (Graphics : in Graphics_Type) return Positive is
   begin
      return Graphics.Execute ("skew.x");
   end Column_Skew;

   ----------
   -- Tint --
   ----------

   procedure Tint (Graphics : in out Graphics_Type; Value : in Natural) is
   begin
      Graphics.Property ("tint", Value);
   end Tint;

   ----------
   -- Tint --
   ----------

   function Tint (Graphics : in Graphics_Type) return Natural is
   begin
      return Graphics.Property ("tint");
   end Tint;

   -------------
   -- Visible --
   -------------

   procedure Visible (Graphics : in out Graphics_Type; Value : in Boolean) is
   begin
      Graphics.Property ("visible", Value);
   end Visible;

   -------------
   -- Visible --
   -------------

   function Visible (Graphics : in Graphics_Type) return Boolean is
   begin
      return Graphics.Property ("visible");
   end Visible;

   --------------
   -- Add_Hole --
   --------------

   procedure Add_Hole (Graphics : in out Graphics_Type) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Add_Hole no more available.");
   end Add_Hole;

   -----------------
   -- Arc_Radians --
   -----------------

   procedure Arc_Radians
     (Graphics                     : in out Graphics_Type;
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
   -- Arc_Degrees --
   -----------------

   procedure Arc_Degrees
     (Graphics                     : in out Graphics_Type;
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
   -- Arc_To --
   ------------

   procedure Arc_To
     (Graphics           : in out Graphics_Type;
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

   ----------------
   -- Begin_Fill --
   ----------------

   procedure Begin_Fill
     (Graphics : in out Graphics_Type;
      Color    : in     Gnoga.Types.Colors.Color_Enumeration :=
        Gnoga.Types.Colors.Black;
      Alpha :    in     Gnoga.Types.Alpha_Type := 1.0)
   is
   begin
      Graphics.Begin_Fill (Gnoga.Types.Colors.To_RGBA (Color), Alpha);
   end Begin_Fill;

   procedure Begin_Fill
     (Graphics : in out Graphics_Type;
      Color    : in     Gnoga.Types.RGBA_Type;
      Alpha    : in     Gnoga.Types.Alpha_Type := 1.0)
   is
   begin
      Graphics.Execute
      ("beginFill(" &
       Gnoga.Types.To_Hex (Color) &
       ", " &
       Alpha'Img &
       ");");
   end Begin_Fill;

   --------------
   -- End_Fill --
   --------------

   procedure End_Fill (Graphics : in out Graphics_Type) is
   begin
      Graphics.Execute ("endFill();");
   end End_Fill;

   ---------------------
   -- Bezier_Curve_To --
   ---------------------

   procedure Bezier_Curve_To
     (Graphics                       : in out Graphics_Type;
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

   -----------
   -- Clear --
   -----------

   procedure Clear (Graphics : in out Graphics_Type) is
   begin
      Graphics.Execute ("clear();");
   end Clear;

   ----------------
   -- Close_Path --
   ----------------

   procedure Close_Path (Graphics : in out Graphics_Type) is
   begin
      Graphics.Execute ("closePath();");
   end Close_Path;

   --------------------
   -- Contains_Point --
   --------------------

   function Contains_Point
     (Graphics    : in Graphics_Type;
      Row, Column : in Integer) return Boolean
   is
   begin
      return Graphics.Execute
        ("containsPoint({x:" & Column'Img & ",y:" & Row'Img & "});") =
        "true";
   end Contains_Point;

   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle
     (Graphics : in out Graphics_Type;
      X, Y     : in     Integer;
      Radius   : in     Integer)
   is
   begin
      Graphics.Execute
      ("drawCircle(" & X'Img & ',' & Y'Img & ',' & Radius'Img & ");");
   end Draw_Circle;

   ------------------
   -- Draw_Ellipse --
   ------------------

   procedure Draw_Ellipse
     (Graphics      : in out Graphics_Type;
      X, Y          : in     Integer;
      Width, Height : in     Integer)
   is
   begin
      Graphics.Execute
      ("drawEllipse(" &
       X'Img &
       ',' &
       Y'Img &
       ',' &
       Width'Img &
       ',' &
       Height'Img &
       ");");
   end Draw_Ellipse;

   ------------------
   -- Draw_Polygon --
   ------------------

   procedure Draw_Polygon
     (Graphics : in out Graphics_Type;
      Path     : in     Gnoga.Types.Point_Array_Type)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
      S : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for Point of Path loop
         if S = Ada.Strings.Unbounded.Null_Unbounded_String then
            S :=
              Ada.Strings.Unbounded.To_Unbounded_String
                (Point.X'Img & ',' & Point.Y'Img);
         else
            S := S & ',' & Point.X'Img & ',' & Point.Y'Img;
         end if;
      end loop;
      Graphics.Execute
      ("drawPolygon(" & Ada.Strings.Unbounded.To_String (S) & ");");
   end Draw_Polygon;

   ---------------
   -- Draw_Rect --
   ---------------

   procedure Draw_Rect
     (Graphics            : in out Graphics_Type;
      X, Y, Width, Height : in     Integer)
   is
   begin
      Graphics.Execute
      ("drawRect(" &
       X'Img &
       ',' &
       Y'Img &
       ',' &
       Width'Img &
       ',' &
       Height'Img &
       ");");
   end Draw_Rect;

   -----------------------
   -- Draw_Rounded_Rect --
   -----------------------

   procedure Draw_Rounded_Rect
     (Graphics            : in out Graphics_Type;
      X, Y, Width, Height : in     Integer)
   is
   begin
      Graphics.Execute
      ("drawRoundedRect(" &
       X'Img &
       ',' &
       Y'Img &
       ',' &
       Width'Img &
       ',' &
       Height'Img &
       ");");
   end Draw_Rounded_Rect;

   -----------------------------
   -- Generate_Canvas_Texture --
   -----------------------------

   procedure Generate_Canvas_Texture
     (Graphics : in out Graphics_Type;
      Texture  :    out Texture_Type)
   is
      pragma Unreferenced (Texture);
   begin
      pragma Compile_Time_Warning (Standard.True, "Generate_Canvas_Texture no more available.");
   end Generate_Canvas_Texture;

   ----------------
   -- Line_Style --
   ----------------

   procedure Line_Style
     (Graphics   : in out Graphics_Type;
      Line_Width : in     Natural;
      Color      : in     Gnoga.Types.Colors.Color_Enumeration;
      Alpha      : in     Gnoga.Types.Alpha_Type := 1.0)
   is
   begin
      Graphics.Line_Style (Line_Width, Gnoga.Types.Colors.To_RGBA (Color));
   end Line_Style;

   procedure Line_Style
     (Graphics   : in out Graphics_Type;
      Line_Width : in     Natural;
      Color      : in     Gnoga.Types.RGBA_Type;
      Alpha      : in     Gnoga.Types.Alpha_Type := 1.0)
   is
   begin
      Graphics.Execute
      ("lineStyle(" &
       Line_Width'Img &
       ',' &
       Gnoga.Types.To_Hex (Color) &
       ',' &
       Alpha'Img &
       ");");
   end Line_Style;

   -------------
   -- Line_To --
   -------------

   procedure Line_To (Graphics : in out Graphics_Type; X, Y : in Integer) is
   begin
      Graphics.Execute ("lineTo(" & X'Img & "," & Y'Img & ");");
   end Line_To;

   -------------
   -- Move_To --
   -------------

   procedure Move_To (Graphics : in out Graphics_Type; X, Y : in Integer) is
   begin
      Graphics.Execute ("moveTo(" & X'Img & "," & Y'Img & ");");
   end Move_To;

   ------------------------
   -- Quadratic_Curve_To --
   ------------------------

   procedure Quadratic_Curve_To
     (Graphics         : in out Graphics_Type;
      CP_X, CP_Y, X, Y : in     Integer)
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
   end Quadratic_Curve_To;

end Gnoga.Gui.Plugin.Pixi.Graphics;
