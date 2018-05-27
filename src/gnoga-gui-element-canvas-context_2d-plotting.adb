------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--   G N O G A . G U I . E L E M E N T . C A N V A S . C O N T E X T _ 2 D  --
--                               P L O T T I N G                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2016 Jeffrey Carter                    --
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

package body Gnoga.Gui.Element.Canvas.Context_2D.Plotting is
   procedure Create (Plot   : in out Plot_Info;
                     Parent : in out Gnoga.Gui.Base.Base_Type'Class;
                     Width  : in     Positive;
                     Height : in     Positive;
                     X_Min  : in     Float;
                     X_Max  : in     Float;
                     Y_Min  : in     Float;
                     Y_Max  : in     Float;
                     ID     : in     String := "")
   is
   begin -- Create
      Canvas_Type (Plot).Create
   (Parent => Parent, Width => Width, Height => Height, ID => ID);
      Plot.X_Min   := Float'Min (X_Min, X_Max);
      Plot.X_Max   := Float'Max (X_Min, X_Max);
      Plot.Y_Min   := Float'Min (Y_Min, Y_Max);
      Plot.Y_Max   := Float'Max (Y_Min, Y_Max);
      Plot.X_Scale := Float (Width) / (Plot.X_Max - Plot.X_Min);
      Plot.Y_Scale := Float (Height) / (Plot.Y_Max - Plot.Y_Min);
   end Create;

   function Scale_X (Plot : Plot_Info; X : Float) return Integer is
   begin -- Scale_X
      return Integer (Plot.X_Scale * (X - Plot.X_Min));
   end Scale_X;

   function Scale_Y (Plot : Plot_Info; Y : Float) return Integer is
   begin -- Scale_Y
      return Plot.Height - Integer (Plot.Y_Scale * (Y - Plot.Y_Min));
   end Scale_Y;

   procedure Point (Plot     : in out Plot_Info;
          Position : in     Plot_Point;
          Color    : in     Gnoga.Types.RGBA_Type := Black)
   is
      Context : Context_2D_Type;
   begin -- Point
      Context.Get_Drawing_Context_2D (Canvas => Plot);
      Context.Begin_Path;
      Context.Stroke_Color (Value => Color);
      Context.Fill_Color (Value => Color);
      Context.Line_Width (Value => 1);
      Context.Arc_Degrees (X              => Plot.Scale_X (Position.X),
                           Y              => Plot.Scale_Y (Position.Y),
                           Radius         => 2,
                           Starting_Angle => 0.0,
                           Ending_Angle   => 360.0);
      Context.Fill;
   end Point;

   procedure Point (Plot     : in out Plot_Info;
          Position : in     Plot_Point;
          Color    : in     String)
   is
   begin -- Point
      Plot.Point (Position => Position,
        Color    => Gnoga.Types.Colors.To_RGBA
          (Gnoga.Types.Colors.To_Color_Enumeration (Color)));
   end Point;

   procedure Point (Plot     : in out Plot_Info;
                    Position : in     Plot_Point;
                    Color    : in     Gnoga.Types.Colors.Color_Enumeration)
   is
   begin -- Point
      Plot.Point (Position => Position,
        Color    => Gnoga.Types.Colors.To_RGBA (Color));
   end Point;

   procedure Line (Plot  : in out Plot_Info;
         From  : in     Plot_Point;
         To    : in     Plot_Point;
         Color : in Gnoga.Types.RGBA_Type := Black)
   is
      Context : Context_2D_Type;
   begin -- Line
      Context.Get_Drawing_Context_2D (Canvas => Plot);
      Context.Begin_Path;
      Context.Stroke_Color (Value => Color);
      Context.Move_To (X => Plot.Scale_X (From.X), Y => Plot.Scale_Y (From.Y));
      Context.Line_To (X => Plot.Scale_X (To.X), Y => Plot.Scale_Y (To.Y));
      Context.Stroke;
   end Line;

   procedure Line (Plot  : in out Plot_Info;
         From  : in     Plot_Point;
         To    : in     Plot_Point;
         Color : in     String)
   is
   begin -- Line
      Plot.Line (From  => From,
       To    => To,
       Color => Gnoga.Types.Colors.To_RGBA
         (Gnoga.Types.Colors.To_Color_Enumeration (Color)));
   end Line;

   procedure Line (Plot  : in out Plot_Info;
                   From  : in     Plot_Point;
                   To    : in     Plot_Point;
                   Color : in     Gnoga.Types.Colors.Color_Enumeration)
   is
   begin -- Line
      Plot.Line (From  => From,
       To    => To,
       Color => Gnoga.Types.Colors.To_RGBA (Color));
   end Line;

   procedure X_Axis (Plot     : in out Plot_Info;
           Interval : in     Positive_Float;
           Length   : in     Positive;
           Color    : in     Gnoga.Types.RGBA_Type := Black)
   is
      Y : constant Integer := Plot.Scale_Y (0.0);

      Context : Context_2D_Type;
      X       : Integer;
      Point   : Float;
   begin -- X_Axis
      Plot.Line (From => (X => Plot.X_Min, Y => 0.0),
       To   => (X => Plot.X_Max, Y => 0.0), Color => Color);
      Context.Get_Drawing_Context_2D (Canvas => Plot);
      Point := Interval;

      Plus : loop
         exit Plus when Point > Plot.X_Max;

         X := Plot.Scale_X (Point);
         Context.Begin_Path;
         Context.Stroke_Color (Value => Color);
         Context.Move_To (X => X, Y => Y - Length);
         Context.Line_To (X => X, Y => Y + Length);
         Context.Stroke;
         Point := Point + Interval;
      end loop Plus;

      Point := -Interval;

      Minus : loop
         exit Minus when Point < Plot.X_Min;

         X := Plot.Scale_X (Point);
         Context.Begin_Path;
         Context.Stroke_Color (Value => Color);
         Context.Move_To (X => X, Y => Y - Length);
         Context.Line_To (X => X, Y => Y + Length);
         Context.Stroke;
         Point := Point - Interval;
      end loop Minus;
   end X_Axis;

   procedure X_Axis (Plot     : in out Plot_Info;
           Interval : in     Positive_Float;
           Length   : in     Positive;
           Color    : in     String)
   is
   begin -- X_Axis
      Plot.X_Axis (Interval => Interval,
                   Length   => Length,
                   Color    => Gnoga.Types.Colors.To_RGBA
           (Gnoga.Types.Colors.To_Color_Enumeration (Color)));
   end X_Axis;

   procedure X_Axis (Plot     : in out Plot_Info;
                     Interval : in     Positive_Float;
                     Length   : in     Positive;
                     Color    : in     Gnoga.Types.Colors.Color_Enumeration)
   is
   begin -- X_Axis
      Plot.X_Axis (Interval => Interval,
         Length   => Length,
         Color     => Gnoga.Types.Colors.To_RGBA (Color));
   end X_Axis;

   procedure Y_Axis (Plot     : in out Plot_Info;
           Interval : in     Positive_Float;
           Length   : in     Positive;
           Color    : in     Gnoga.Types.RGBA_Type := Black)
   is
      X : constant Integer := Plot.Scale_X (0.0);

      Context : Context_2D_Type;
      Y       : Integer;
      Point   : Float;
   begin -- Y_Axis
      Plot.Line (From  => (X => 0.0, Y => Plot.Y_Min),
       To    => (X => 0.0, Y => Plot.Y_Max),
       Color => Color);
      Context.Get_Drawing_Context_2D (Canvas => Plot);
      Point := Interval;

      Plus : loop
         exit Plus when Point > Plot.Y_Max;

         Y := Plot.Scale_Y (Point);
         Context.Begin_Path;
         Context.Stroke_Color (Value => Color);
         Context.Move_To (X => X - Length, Y => Y);
         Context.Line_To (X => X + Length, Y => Y);
         Context.Stroke;
         Point := Point + Interval;
      end loop Plus;

      Point := -Interval;

      Minus : loop
         exit Minus when Point < Plot.Y_Min;

         Y := Plot.Scale_Y (Point);
         Context.Begin_Path;
         Context.Stroke_Color (Value => Color);
         Context.Move_To (X => X - Length, Y => Y);
         Context.Line_To (X => X + Length, Y => Y);
         Context.Stroke;
         Point := Point - Interval;
      end loop Minus;
   end Y_Axis;

   procedure Y_Axis (Plot     : in out Plot_Info;
           Interval : in     Positive_Float;
           Length   : in     Positive;
           Color    : in     String)
   is
   begin -- Y_Axis
      Plot.Y_Axis (Interval => Interval,
                   Length   => Length,
                   Color    => Gnoga.Types.Colors.To_RGBA
           (Gnoga.Types.Colors.To_Color_Enumeration (Color)));
   end Y_Axis;

   procedure Y_Axis (Plot     : in out Plot_Info;
                     Interval : in     Positive_Float;
                     Length   : in     Positive;
                     Color    : in     Gnoga.Types.Colors.Color_Enumeration)
   is
   begin -- Y_Axis
      Plot.Y_Axis (Interval => Interval,
         Length   => Length,
         Color    => Gnoga.Types.Colors.To_RGBA (Color));
   end Y_Axis;

   procedure Axes (Plot     : in out Plot_Info;
         Interval : in     Positive_Float;
         Length   : in     Positive;
         Color    : in     Gnoga.Types.RGBA_Type := Black)
   is
   begin -- Axes
      Plot.X_Axis (Interval => Interval, Length => Length, Color => Color);
      Plot.Y_Axis (Interval => Interval, Length => Length, Color => Color);
   end Axes;

   procedure Axes (Plot     : in out Plot_Info;
         Interval : in Positive_Float;
         Length   : in Positive;
         Color    : in String)
   is
   begin -- Axes
      Plot.Axes (Interval => Interval,
                 Length   => Length,
                 Color    => Gnoga.Types.Colors.To_RGBA
         (Gnoga.Types.Colors.To_Color_Enumeration (Color)));
   end Axes;

   procedure Axes (Plot     : in out Plot_Info;
                   Interval : in     Positive_Float;
                   Length   : in     Positive;
                   Color    : in     Gnoga.Types.Colors.Color_Enumeration)
   is
   begin -- Axes
      Plot.Axes (Interval => Interval,
       Length   => Length,
       Color    => Gnoga.Types.Colors.To_RGBA (Color));
   end Axes;

   procedure Graph (Plot  : in out Plot_Info;
          List  : in     Point_List;
          Color : in     Gnoga.Types.RGBA_Type := Black)
   is
   begin -- Graph
      All_Points : for I in List'First .. List'Last - 1 loop
         Plot.Line (From => List (I), To => List (I + 1), Color => Color);
      end loop All_Points;
   end Graph;

   procedure Graph (Plot  : in out Plot_Info;
          List  : in     Point_List;
          Color : in     String)
   is
   begin -- Graph
      Plot.Graph (List  => List,
        Color => Gnoga.Types.Colors.To_RGBA
          (Gnoga.Types.Colors.To_Color_Enumeration (Color)));
   end Graph;

   procedure Graph (Plot  : in out Plot_Info;
          List  : in     Point_List;
          Color : in     Gnoga.Types.Colors.Color_Enumeration) is
   begin -- Graph
      Plot.Graph (List => List, Color => Gnoga.Types.Colors.To_RGBA (Color));
   end Graph;
end Gnoga.Gui.Element.Canvas.Context_2D.Plotting;
