------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--   G N O G A . G U I . E L E M E N T . C A N V A S . C O N T E X T _ 2 D  --
--                               P L O T T I N G                            --
--                                                                          --
--                                 S p e c                                  --
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
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

package Gnoga.Gui.Element.Canvas.Context_2D.Plotting is
   type Plot_Info is new Canvas_Type with private;

   procedure Create (Plot   : in out Plot_Info;
                     Parent : in out Gnoga.Gui.Base.Base_Type'Class;
                     Width  : in     Positive;
                     Height : in     Positive;
                     X_Min  : in     Float;
                     X_Max  : in     Float;
                     Y_Min  : in     Float;
                     Y_Max  : in     Float;
                     ID     : in     String := "");
   --  Create a plot
   --  Width and Height are dimensions in pixels of the underlying Canvas_Type
   --  X_Min, X_Max, Y_Min, and Y_Max are the ranges of values that are mapped
   --    to the Width and Height
   --  X_Min corresponds to an X of zero and X_Max, of Width
   --  Y_Min corresponds to a Y of Height and Y_Max to a Y of zero

   function Scale_X (Plot : Plot_Info; X : Float) return Integer;
   --  Return the pixel-X-coordinate corresponding to X

   function Scale_Y (Plot : Plot_Info; Y : Float) return Integer;
   --  return the pixel-Y-coordinate corresponding to Y

   type Plot_Point is record
      X : Float;
      Y : Float;
   end record;

   Black : constant Gnoga.Types.RGBA_Type := (others => <>);

   procedure Point (Plot     : in out Plot_Info;
                    Position : in     Plot_Point;
                    Color    : in     Gnoga.Types.RGBA_Type := Black);
   procedure Point (Plot     : in out Plot_Info;
                    Position : in     Plot_Point;
                    Color    : in     String);
   procedure Point (Plot     : in out Plot_Info;
                    Position : in     Plot_Point;
                    Color    : in     Gnoga.Types.Colors.Color_Enumeration);
   --  Draws a point (filled circle with radius of 2 pixels)
   --  at position in color Color

   procedure Line (Plot  : in out Plot_Info;
                   From  : in     Plot_Point;
                   To    : in     Plot_Point;
                   Color : in     Gnoga.Types.RGBA_Type := Black);
   procedure Line (Plot  : in out Plot_Info;
                   From  : in     Plot_Point;
                   To    : in     Plot_Point;
                   Color : in     String);
   procedure Line (Plot  : in out Plot_Info;
                   From  : in     Plot_Point;
                   To    : in     Plot_Point;
                   Color : in     Gnoga.Types.Colors.Color_Enumeration);
   --  Draws a line (width 1 pixel) from From to To in color Color

   subtype Positive_Float is Float range Float'Succ (0.0) .. Float'Last;

   procedure X_Axis (Plot     : in out Plot_Info;
                     Interval : in     Positive_Float;
                     Length   : in     Positive;
                     Color    : in     Gnoga.Types.RGBA_Type := Black);
   procedure X_Axis (Plot     : in out Plot_Info;
           Interval : in     Positive_Float;
           Length   : in     Positive;
           Color    : in     String);
   procedure X_Axis (Plot     : in out Plot_Info;
                     Interval : in     Positive_Float;
                     Length   : in     Positive;
                     Color    : in     Gnoga.Types.Colors.Color_Enumeration);
   --  Draws the X axis with ticks every Interval away from zero
   --  Ticks extend Length pixels from the axis on both sides
   --  The axis and ticks will be in color Color

   procedure Y_Axis (Plot     : in out Plot_Info;
           Interval : in     Positive_Float;
           Length   : in     Positive;
           Color    : in     Gnoga.Types.RGBA_Type := Black);
   procedure Y_Axis (Plot : in out Plot_Info;
           Interval : in Positive_Float;
           Length   : in Positive;
           Color    : in String);
   procedure Y_Axis (Plot     : in out Plot_Info;
                     Interval : in     Positive_Float;
                     Length   : in     Positive;
                     Color    : in     Gnoga.Types.Colors.Color_Enumeration);
   --  Draws the Y axis with ticks every Interval away from zero
   --  Ticks extend Length pixels from the axis on both sides
   --  The axis and ticks will be in color Color

   procedure Axes (Plot     : in out Plot_Info;
         Interval : in     Positive_Float;
         Length   : in     Positive;
         Color    : in     Gnoga.Types.RGBA_Type := Black);
   procedure Axes (Plot     : in out Plot_Info;
         Interval : in     Positive_Float;
         Length   : in     Positive; Color : in String);
   procedure Axes (Plot     : in out Plot_Info;
                   Interval : in     Positive_Float;
                   Length   : in     Positive;
                   Color    : in     Gnoga.Types.Colors.Color_Enumeration);
   --  Draws both the X and Y axes with ticks every Interval away from zero
   --  Ticks extend Length pixels from the axis on both sides
   --  The axes and ticks will be in color Color

   type Point_List is array (Positive range <>) of Plot_Point;

   procedure Graph (Plot  : in out Plot_Info;
          List  : in     Point_List;
          Color : in     Gnoga.Types.RGBA_Type := Black);
   procedure Graph (Plot  : in out Plot_Info;
          List  : in     Point_List;
          Color : in     String);
   procedure Graph (Plot  : in out Plot_Info;
          List  : in     Point_List;
          Color : in     Gnoga.Types.Colors.Color_Enumeration);
   --  Draws lines from List (List'First) to List (List'First + 1),
   --  List (List'First + 1) to List (List'First + 2), ...,
   --  List (List'Last - 1) to List (List'Last) in color Color

private -- Gnoga.Gui.Element.Canvas.Context_2D.Plotting
   type Plot_Info is new Canvas_Type with record
      X_Min   : Float;
      X_Max   : Float;
      Y_Min   : Float;
      Y_Max   : Float;
      X_Scale : Float;
      Y_Scale : Float;
   end record;
end Gnoga.Gui.Element.Canvas.Context_2D.Plotting;
