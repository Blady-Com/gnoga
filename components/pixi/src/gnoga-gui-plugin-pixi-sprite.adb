------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--          G N O G A . G U I . P L U G I N . P I X I . S P R I T E         --
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

with Ada.Numerics.Elementary_Functions;
with Gnoga.Server.Connection;

package body Gnoga.Gui.Plugin.Pixi.Sprite is
   use Ada.Numerics.Elementary_Functions;

   function To_Radian
     (Angle : Integer)
      return Float is (Float (Angle) * Ada.Numerics.Pi / 180.0);
   --  Converts integer angle in degree to radian.

   function To_Degree
     (Angle : Float)
      return Integer is (Integer (Angle * 180.0 / Ada.Numerics.Pi));
   --  Converts integer angle in degree to radian.

   ------------
   -- Create --
   ------------

   procedure Create
     (Sprite                                : in out Sprite_Type;
      Parent                                : in out Container_Type'Class;
      Texture                               : in     Texture_Type;
      Row, Column                           : in     Integer;
      Row_Velocity, Column_Velocity         : in     Velocity_Type     := 0.0;
      Row_Acceleration, Column_Acceleration : in     Acceleration_Type := 0.0)
   is
      Sprite_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Sprite.ID (Sprite_ID, Gnoga.Types.Gnoga_ID);
      Sprite.Connection_ID (Parent.Connection_ID);
      Sprite.Attach_Using_Parent (Parent => Parent, ID => Sprite_ID, ID_Type => Gnoga.Types.Gnoga_ID);
      Gnoga.Server.Connection.Execute_Script
        (Sprite.Connection_ID,
         "gnoga['" & Sprite_ID & "'] = new PIXI.Sprite(gnoga['" & Texture.ID & "']);" & " gnoga['" & Sprite_ID &
         "'].sendEvent = function (e) {ws.send ('" & Image (Sprite.Unique_ID) & "|' + e + '|');}");
      Sprite.Locate (Row, Column);
      Sprite.Motion (Row_Velocity, Column_Velocity);
      Sprite.Acceleration (Row_Acceleration, Column_Acceleration);
      Sprite.Rotation_Velocity (0.0);
      Sprite.Rotation_Acceleration (0.0);
      Sprite.Loop_Times (0, 0);
      Sprite.Frame_Limit (0, 0, 0, 0, Null_Effect);
      Sprite.Angle_Limit (0, 0, Null_Effect);
      Sprite.Parent (Parent);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Sprite                                : in out Sprite_Type;
      Parent                                : in out Container_Type'Class;
      Image_Path                            : in     String;
      Row, Column                           : in     Integer;
      Row_Velocity, Column_Velocity         : in     Velocity_Type     := 0.0;
      Row_Acceleration, Column_Acceleration : in     Acceleration_Type := 0.0)
   is
      Sprite_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Sprite.ID (Sprite_ID, Gnoga.Types.Gnoga_ID);
      Sprite.Connection_ID (Parent.Connection_ID);
      Sprite.Attach_Using_Parent (Parent => Parent, ID => Sprite_ID, ID_Type => Gnoga.Types.Gnoga_ID);
      Gnoga.Server.Connection.Execute_Script
        (Sprite.Connection_ID,
         "gnoga['" & Sprite_ID & "'] = new PIXI.Sprite.from('" & Image_Path & "');" & " gnoga['" & Sprite_ID &
         "'].sendEvent = function (e) {ws.send ('" & Image (Sprite.Unique_ID) & "|' + e + '|');}");
      Sprite.Locate (Row, Column);
      Sprite.Motion (Row_Velocity, Column_Velocity);
      Sprite.Acceleration (Row_Acceleration, Column_Acceleration);
      Sprite.Rotation_Velocity (0.0);
      Sprite.Rotation_Acceleration (0.0);
      Sprite.Frame_Limit (0, 0, 0, 0, Null_Effect);
      Sprite.Angle_Limit (0, 0, Null_Effect);
      Sprite.Parent (Parent);
   end Create;

   ------------
   -- Locate --
   ------------

   procedure Locate
     (Sprite      : in out Sprite_Type;
      Row, Column : in     Integer)
   is
   begin
      Sprite.Property ("x", Column);
      Sprite.Property ("y", Row);
   end Locate;

   --------------
   -- Position --
   --------------

   procedure Position
     (Sprite      : in     Sprite_Type;
      Row, Column :    out Integer)
   is
   begin
      Row    := Sprite.Property ("y");
      Column := Sprite.Property ("x");
   end Position;

   ---------
   -- Row --
   ---------

   function Row
     (Sprite : in Sprite_Type)
      return Integer
   is
      V : constant Float := Sprite.Property ("y");
   begin
      return Integer (V);
   end Row;

   ------------
   -- Column --
   ------------

   function Column
     (Sprite : in Sprite_Type)
      return Integer
   is
      V : constant Float := Sprite.Property ("x");
   begin
      return Integer (V);
   end Column;

   -------------
   -- Pattern --
   -------------

   procedure Pattern
     (Sprite     : in Sprite_Type;
      Image_Data : in String)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Pattern unimplemented");
      null; -- TODO
   end Pattern;

   -------------
   -- Pattern --
   -------------

   function Pattern
     (Sprite : in Sprite_Type)
      return String
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Pattern unimplemented");
      return ""; -- TODO
   end Pattern;

   ------------
   -- Motion --
   ------------

   procedure Motion
     (Sprite                        : in out Sprite_Type;
      Row_Velocity, Column_Velocity : in     Velocity_Type)
   is
   begin
      Sprite.Property ("gnoga_vx", Column_Velocity / Frame_Rate);
      Sprite.Property ("gnoga_vy", Row_Velocity / Frame_Rate);
   end Motion;

   ------------------
   -- Row_Velocity --
   ------------------

   function Row_Velocity
     (Sprite : in Sprite_Type)
      return Velocity_Type
   is
   begin
      return Sprite.Property ("gnoga_vy") * Frame_Rate;
   end Row_Velocity;

   ---------------------
   -- Column_Velocity --
   ---------------------

   function Column_Velocity
     (Sprite : in Sprite_Type)
      return Velocity_Type
   is

   begin
      return Sprite.Property ("gnoga_vx") * Frame_Rate;
   end Column_Velocity;

   ------------
   -- Motion --
   ------------

   procedure Motion
     (Sprite              : in out Sprite_Type;
      Radial_Velocity     : in     Velocity_Type;
      Azimuth_Of_Velocity : in     Integer)
   is
   begin
      Sprite.Motion
        (Radial_Velocity * Sin (To_Radian (Azimuth_Of_Velocity)),
         Radial_Velocity * Cos (To_Radian (Azimuth_Of_Velocity)));
   end Motion;

   ---------------------
   -- Radial_Velocity --
   ---------------------

   procedure Radial_Velocity
     (Sprite : in out Sprite_Type;
      Value  : in     Velocity_Type)
   is
   begin
      Sprite.Motion (Value, Sprite.Azimuth_Of_Velocity);
   end Radial_Velocity;

   -------------------------
   -- Azimuth_Of_Velocity --
   -------------------------

   procedure Azimuth_Of_Velocity
     (Sprite : in out Sprite_Type;
      Value  : in     Integer)
   is
   begin
      Sprite.Motion (Sprite.Radial_Velocity, Value);
   end Azimuth_Of_Velocity;

   ---------------------
   -- Orient_Velocity --
   ---------------------

   procedure Orient_Velocity
     (Sprite            : in out Sprite_Type;
      To_Row, To_Column : in     Integer)
   is
   begin
      Sprite.Azimuth_Of_Velocity (To_Degree (Arctan (Float (To_Row - Sprite.Row), Float (To_Column - Sprite.Column))));
   end Orient_Velocity;

   ---------------------
   -- Radial_Velocity --
   ---------------------

   function Radial_Velocity
     (Sprite : in Sprite_Type)
      return Velocity_Type
   is
   begin
      return Sqrt (Sprite.Row_Velocity**2 + Sprite.Column_Velocity**2);
   end Radial_Velocity;

   -------------------------
   -- Azimuth_Of_Velocity --
   -------------------------

   function Azimuth_Of_Velocity
     (Sprite : in Sprite_Type)
      return Integer
   is
   begin
      return To_Degree (Arctan (Sprite.Row_Velocity, Sprite.Column_Velocity));
   end Azimuth_Of_Velocity;

   ------------------
   -- Acceleration --
   ------------------

   procedure Acceleration
     (Sprite                                : in out Sprite_Type;
      Row_Acceleration, Column_Acceleration : in     Acceleration_Type)
   is
   begin
      Sprite.Property ("gnoga_ax", Column_Acceleration / Frame_Rate / Frame_Rate);
      Sprite.Property ("gnoga_ay", Row_Acceleration / Frame_Rate / Frame_Rate);
   end Acceleration;

   ----------------------
   -- Row_Acceleration --
   ----------------------

   function Row_Acceleration
     (Sprite : in Sprite_Type)
      return Acceleration_Type
   is
   begin
      return Sprite.Property ("gnoga_ay") * (Frame_Rate * Frame_Rate);
   end Row_Acceleration;

   -------------------------
   -- Column_Acceleration --
   -------------------------

   function Column_Acceleration
     (Sprite : in Sprite_Type)
      return Acceleration_Type
   is

   begin
      return Sprite.Property ("gnoga_ax") * (Frame_Rate * Frame_Rate);
   end Column_Acceleration;

   ------------------
   -- Acceleration --
   ------------------

   procedure Acceleration
     (Sprite                  : in out Sprite_Type;
      Radial_Acceleration     : in     Acceleration_Type;
      Azimuth_Of_Acceleration : in     Integer)
   is
   begin
      Sprite.Acceleration
        (Radial_Acceleration * Sin (To_Radian (Azimuth_Of_Acceleration)),
         Radial_Acceleration * Cos (To_Radian (Azimuth_Of_Acceleration)));
   end Acceleration;

   -------------------------
   -- Radial_Acceleration --
   -------------------------

   procedure Radial_Acceleration
     (Sprite : in out Sprite_Type;
      Value  : in     Acceleration_Type)
   is
   begin
      Sprite.Acceleration (Value, Sprite.Azimuth_Of_Acceleration);
   end Radial_Acceleration;

   -----------------------------
   -- Azimuth_Of_Acceleration --
   -----------------------------

   procedure Azimuth_Of_Acceleration
     (Sprite : in out Sprite_Type;
      Value  : in     Integer)
   is
   begin
      Sprite.Acceleration (Sprite.Radial_Acceleration, Value);
   end Azimuth_Of_Acceleration;

   -------------------------
   -- Orient_Acceleration --
   -------------------------

   procedure Orient_Acceleration
     (Sprite            : in out Sprite_Type;
      To_Row, To_Column : in     Integer)
   is
   begin
      Sprite.Azimuth_Of_Acceleration
        (To_Degree (Arctan (Float (To_Row - Sprite.Row), Float (To_Column - Sprite.Column))));
   end Orient_Acceleration;

   -------------------------
   -- Radial_Acceleration --
   -------------------------

   function Radial_Acceleration
     (Sprite : in Sprite_Type)
      return Acceleration_Type
   is
   begin
      return Sqrt (Sprite.Row_Acceleration**2 + Sprite.Column_Acceleration**2);
   end Radial_Acceleration;

   -----------------------------
   -- Azimuth_Of_Acceleration --
   -----------------------------

   function Azimuth_Of_Acceleration
     (Sprite : in Sprite_Type)
      return Integer
   is
   begin
      return To_Degree (Arctan (Sprite.Row_Acceleration, Sprite.Column_Acceleration));
   end Azimuth_Of_Acceleration;

   -----------
   -- Alpha --
   -----------

   procedure Alpha
     (Sprite : in out Sprite_Type;
      Value  : in     Gnoga.Types.Alpha_Type)
   is
   begin
      Sprite.Property ("alpha", Float (Value));
   end Alpha;

   -----------
   -- Alpha --
   -----------

   function Alpha
     (Sprite : in Sprite_Type)
      return Gnoga.Types.Alpha_Type
   is
   begin
      return Gnoga.Types.Alpha_Type (Float'(Sprite.Property ("alpha")));
   end Alpha;

   ------------
   -- Anchor --
   ------------

   procedure Anchor
     (Sprite      : in out Sprite_Type;
      Row, Column : in     Gnoga.Types.Fractional_Range_Type)
   is
      function Image is new UXStrings.Conversions.Fixed_Point_Image (Gnoga.Types.Fractional_Range_Type);
   begin
      Gnoga.Server.Connection.Execute_Script
        (Sprite.Connection_ID, "gnoga['" & Sprite.ID & "'].anchor = {x:" & Image (Column) & ",y:" & Image (Row) & "};");
   end Anchor;

   ----------------
   -- Row_Anchor --
   ----------------

   function Row_Anchor
     (Sprite : in Sprite_Type)
      return Gnoga.Types.Fractional_Range_Type
   is
   begin
      return Gnoga.Types.Fractional_Range_Type (Float'(Sprite.Execute ("anchor.y")));
   end Row_Anchor;

   -------------------
   -- Column_Anchor --
   -------------------

   function Column_Anchor
     (Sprite : in Sprite_Type)
      return Gnoga.Types.Fractional_Range_Type
   is
   begin
      return Gnoga.Types.Fractional_Range_Type (Float'(Sprite.Execute ("anchor.x")));
   end Column_Anchor;

   ----------------
   -- Blend_Mode --
   ----------------

   procedure Blend_Mode
     (Sprite : in out Sprite_Type;
      Value  : in     Blend_Modes_Type)
   is
      function Image is new UXStrings.Conversions.Scalar_Image (Blend_Modes_Type);
   begin
      Sprite.Property ("blendMode", Image (Value));
   end Blend_Mode;

   ----------------
   -- Blend_Mode --
   ----------------

   function Blend_Mode
     (Sprite : in Sprite_Type)
      return Blend_Modes_Type
   is
      function Value is new UXStrings.Conversions.Scalar_Value (Blend_Modes_Type);
   begin
      return Value (Sprite.Property ("blendMode"));
   end Blend_Mode;

   -----------
   -- Pivot --
   -----------

   procedure Pivot
     (Sprite      : in out Sprite_Type;
      Row, Column : in     Integer)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Sprite.Connection_ID, "gnoga['" & Sprite.ID & "'].pivot = {x:" & Image (Column) & ",y:" & Image (Row) & "};");
   end Pivot;

   ---------------
   -- Row_Pivot --
   ---------------

   function Row_Pivot
     (Sprite : in Sprite_Type)
      return Integer
   is
   begin
      return Sprite.Execute ("pivot.x");
   end Row_Pivot;

   ------------------
   -- Column_Pivot --
   ------------------

   function Column_Pivot
     (Sprite : in Sprite_Type)
      return Integer
   is
   begin
      return Sprite.Execute ("pivot.y");
   end Column_Pivot;

   --------------
   -- Rotation --
   --------------

   procedure Rotation
     (Sprite : in out Sprite_Type;
      Value  : in     Integer)
   is
   begin
      Sprite.Property ("rotation", Float (Value) * Ada.Numerics.Pi / 180.0);
   end Rotation;

   --------------
   -- Rotation --
   --------------

   function Rotation
     (Sprite : in Sprite_Type)
      return Integer
   is
   begin
      return Integer (Float'(Sprite.Property ("rotation")) * 180.0 / Ada.Numerics.Pi);
   end Rotation;

   -----------------------
   -- Rotation_Velocity --
   -----------------------

   procedure Rotation_Velocity
     (Sprite : in out Sprite_Type;
      Value  :        Velocity_Type)
   is
   begin
      Sprite.Property ("gnoga_vr", Value / Frame_Rate * Ada.Numerics.Pi / 180.0);
   end Rotation_Velocity;

   -----------------------
   -- Rotation_Velocity --
   -----------------------

   function Rotation_Velocity
     (Sprite : in Sprite_Type)
      return Velocity_Type
   is
   begin
      return Float'(Sprite.Property ("gnoga_vr")) * Frame_Rate * 180.0 / Ada.Numerics.Pi;
   end Rotation_Velocity;

   -----------------------
   -- Rotation_Acceleration --
   -----------------------

   procedure Rotation_Acceleration
     (Sprite : in out Sprite_Type;
      Value  :        Acceleration_Type)
   is
   begin
      Sprite.Property ("gnoga_ar", Value / Frame_Rate / Frame_Rate * Ada.Numerics.Pi / 180.0);
   end Rotation_Acceleration;

   -----------------------
   -- Rotation_Acceleration --
   -----------------------

   function Rotation_Acceleration
     (Sprite : in Sprite_Type)
      return Acceleration_Type
   is
   begin
      return Float'(Sprite.Property ("gnoga_ar")) * Frame_Rate * Frame_Rate * 180.0 / Ada.Numerics.Pi;
   end Rotation_Acceleration;

   -----------
   -- Width --
   -----------

   overriding procedure Width
     (Sprite : in out Sprite_Type;
      Value  : in     Integer)
   is
   begin
      Sprite.Property ("width", Value);
   end Width;

   -----------
   -- Width --
   -----------

   overriding function Width
     (Sprite : in Sprite_Type)
      return Integer
   is
   begin
      return Sprite.Property ("width");
   end Width;

   ------------
   -- Height --
   ------------

   overriding procedure Height
     (Sprite : in out Sprite_Type;
      Value  : in     Integer)
   is
   begin
      Sprite.Property ("height", Value);
   end Height;

   ------------
   -- Height --
   ------------

   overriding function Height
     (Sprite : in Sprite_Type)
      return Integer
   is
   begin
      return Sprite.Property ("height");
   end Height;

   -----------
   -- Scale --
   -----------

   procedure Scale
     (Sprite      : in out Sprite_Type;
      Row, Column : in     Positive)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Sprite.Connection_ID, "gnoga['" & Sprite.ID & "'].scale = {x:" & Image (Column) & ",y:" & Image (Row) & "};");
   end Scale;

   ---------------
   -- Row_Scale --
   ---------------

   function Row_Scale
     (Sprite : in Sprite_Type)
      return Positive
   is
   begin
      return Sprite.Execute ("scale.y");
   end Row_Scale;

   ------------------
   -- Column_Scale --
   ------------------

   function Column_Scale
     (Sprite : in Sprite_Type)
      return Positive
   is
   begin
      return Sprite.Execute ("scale.x");
   end Column_Scale;

   ----------
   -- Skew --
   ----------

   procedure Skew
     (Sprite      : in out Sprite_Type;
      Row, Column : in     Positive)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Sprite.Connection_ID, "gnoga['" & Sprite.ID & "'].skew = {x:" & Image (Column) & ",y:" & Image (Row) & "};");
   end Skew;

   --------------
   -- Row_Skew --
   --------------

   function Row_Skew
     (Sprite : in Sprite_Type)
      return Positive
   is
   begin
      return Sprite.Execute ("skew.y");
   end Row_Skew;

   -----------------
   -- Column_Skew --
   -----------------

   function Column_Skew
     (Sprite : in Sprite_Type)
      return Positive
   is
   begin
      return Sprite.Execute ("skew.x");
   end Column_Skew;

   -----------------
   -- Get_Texture --
   -----------------

   procedure Get_Texture
     (Sprite : in     Sprite_Type;
      Value  : in out Texture_Type'Class)
   is
      Texture_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Value.ID (Texture_ID, Gnoga.Types.Gnoga_ID);
      Value.Connection_ID (Sprite.Connection_ID);
      Gnoga.Server.Connection.Execute_Script
        (Sprite.Connection_ID, "gnoga['" & Texture_ID & "'] = gnoga['" & Sprite.ID & "'].texture;");
   end Get_Texture;

   -----------------
   -- Put_Texture --
   -----------------

   procedure Put_Texture
     (Sprite : in out Sprite_Type;
      Value  : in     Texture_Type'Class)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Sprite.Connection_ID, "gnoga['" & Sprite.ID & "'].texture = gnoga['" & Value.ID & "'];");
   end Put_Texture;

   ----------
   -- Tint --
   ----------

   procedure Tint
     (Sprite : in out Sprite_Type;
      Value  : in     Natural)
   is
   begin
      Sprite.Property ("tint", Value);
   end Tint;

   ----------
   -- Tint --
   ----------

   function Tint
     (Sprite : in Sprite_Type)
      return Natural
   is
   begin
      return Sprite.Property ("tint");
   end Tint;

   -------------
   -- Visible --
   -------------

   procedure Visible
     (Sprite : in out Sprite_Type;
      Value  : in     Boolean)
   is
   begin
      Sprite.Property ("visible", Value);
   end Visible;

   -------------
   -- Visible --
   -------------

   function Visible
     (Sprite : in Sprite_Type)
      return Boolean
   is
   begin
      return Sprite.Property ("visible");
   end Visible;

   -----------------
   -- Coincidence --
   -----------------

   function Coincidence
     (Sprite1, Sprite2 : in Sprite_Type;
      Tolerance        : in Natural)
      return Boolean
   is
   begin
      return Distance (Sprite1, Sprite2) <= Tolerance;
   end Coincidence;

   -----------------
   -- Coincidence --
   -----------------

   function Coincidence
     (Sprite      : in Sprite_Type;
      Row, Column : in Integer;
      Tolerance   : in Natural)
      return Boolean
   is
   begin
      return Distance (Sprite, Row, Column) <= Tolerance;
   end Coincidence;

   -------------------
   -- Overlap_Point --
   -------------------

   function Overlap_Point
     (Sprite      : in Sprite_Type;
      Row, Column : in Integer)
      return Boolean
   is
   begin
      return
        Row in Sprite.Row - Sprite.Height .. Sprite.Row + Sprite.Height and
        Column in Sprite.Column - Sprite.Width .. Sprite.Column + Sprite.Width;
   end Overlap_Point;

   --------------
   -- Distance --
   --------------

   function Distance
     (Sprite1, Sprite2 : in Sprite_Type)
      return Natural
   is
   begin
      return
        Natural
          (Ada.Numerics.Elementary_Functions.Sqrt
             (Float (Sprite2.Row - Sprite1.Row)**2 + Float (Sprite2.Column - Sprite1.Column)**2));
   end Distance;

   --------------
   -- Distance --
   --------------

   function Distance
     (Sprite      : in Sprite_Type;
      Row, Column : in Integer)
      return Natural
   is
   begin
      return
        Natural
          (Ada.Numerics.Elementary_Functions.Sqrt (Float (Sprite.Row - Row)**2 + Float (Sprite.Column - Column)**2));
   end Distance;

   -------------
   -- Move_To --
   -------------

   procedure Loop_Times
     (Sprite         : in out Sprite_Type;
      Current, Final :        Natural)
   is
   begin
      Sprite.Property ("gnoga_tcur", Current);
      Sprite.Property ("gnoga_tfin", Final);
   end Loop_Times;

   procedure Move_To
     (Sprite              : in out Sprite_Type;
      Row, Column         : in     Integer;
      Radial_Velocity     : in     Velocity_Type;
      Radial_Acceleration : in     Acceleration_Type;
      Spent_Time          :    out Duration)
   is
      --  Distance: Sqrt ((Row - Sprite.Row)**2 + (Column - Sprite.Column)**2)
      Distance : constant Float := Sqrt (Float (Row - Sprite.Row)**2 + Float (Column - Sprite.Column)**2);
      TFin     : Natural;
      Norm_Row : constant Float := (if Distance > 0.0 then Float (Row - Sprite.Row) / Distance else 0.0);
      Norm_Col : constant Float := (if Distance > 0.0 then Float (Column - Sprite.Column) / Distance else 0.0);
   begin
      Spent_Time := 0.0;
      if abs Radial_Acceleration > 0.0 then
         declare
            --  Quadratic time equation: A/2 * t**2 + V * t - D = 0
            --  Discriminant: V**2 - 4 * (A/2) * (-D)
            --  Roots: (-V +/- Sqrt(V**2 - 4 * (A/2) * (-D))) / 2 (A/2)
            Discriminant : constant Float := Radial_Velocity**2 + 2.0 * Distance * Radial_Acceleration;
            Root1        : constant Float := (Sqrt (Discriminant) - Radial_Velocity) / Radial_Acceleration;
            Root2        : constant Float := (-Sqrt (Discriminant) - Radial_Velocity) / Radial_Acceleration;
         begin
            Spent_Time := Duration (if Root1 > Root2 then Root1 else Root2);
         end;
      elsif abs Radial_Velocity > 0.0 then
         Spent_Time := Duration (Distance / Radial_Velocity);
      elsif Distance > 0.0 then
         raise Constraint_Error with "Move to another point but no velocity nor acceleration";
      end if;
      TFin := Natural (Frame_Rate * Spent_Time);
      if TFin > 0 then
         Sprite.Motion (Radial_Velocity * Norm_Row, Radial_Velocity * Norm_Col);
         Sprite.Acceleration (Radial_Acceleration * Norm_Row, Radial_Acceleration * Norm_Col);
         Sprite.Loop_Times (0, TFin);
      end if;
   end Move_To;

   --------------
   -- Move_Rel --
   --------------

   procedure Move_Rel
     (Sprite              : in out Sprite_Type;
      Rel_Row, Rel_Column : in     Integer;
      Velocity            : in     Velocity_Type;
      Acceleration        : in     Acceleration_Type;
      Spent_Time          :    out Duration)
   is
   begin
      Sprite.Move_To (Sprite.Row + Rel_Row, Sprite.Column + Rel_Column, Velocity, Acceleration, Spent_Time);
   end Move_Rel;

   --------------
   -- Move_Rel --
   --------------

   procedure Move_Rel
     (Sprite       : in out Sprite_Type;
      Distance     : in     Integer;
      Velocity     : in     Velocity_Type;
      Acceleration : in     Acceleration_Type;
      Spent_Time   :    out Duration)
   is
      TFin : Natural;
   begin
      Spent_Time := 0.0;
      if abs Acceleration > 0.0 then
         declare
            --  Quadratic time equation: A/2 * t**2 + V * t - D = 0
            --  Discriminant: V**2 - 4 * (A/2) * (-D)
            --  Roots: (-V +/- Sqrt(V**2 - 4 * (A/2) * (-D))) / 2 (A/2)
            Discriminant : constant Float := Velocity**2 + 2.0 * Float (Distance) * Acceleration;
            Root1        : constant Float := (Sqrt (Discriminant) - Velocity) / Acceleration;
            Root2        : constant Float := (-Sqrt (Discriminant) - Velocity) / Acceleration;
         begin
            Spent_Time := Duration (if Root1 > Root2 then Root1 else Root2);
         end;
      elsif abs Velocity > 0.0 then
         Spent_Time := Duration (Float (Distance) / Velocity);
      elsif abs Distance > 0 then
         raise Constraint_Error with "Move to another point but no velocity nor acceleration";
      end if;
      TFin := Natural (Frame_Rate * Spent_Time);
      if TFin > 0 then
         Sprite.Motion (Velocity, Sprite.Rotation);
         Sprite.Acceleration (Acceleration, Sprite.Rotation);
         Sprite.Loop_Times (0, TFin);
      end if;
   end Move_Rel;

   ----------------
   -- Rotate_To --
   ----------------

   procedure Rotate_To
     (Sprite       : in out Sprite_Type;
      Angle        : in     Integer;
      Velocity     : in     Velocity_Type;
      Acceleration : in     Acceleration_Type;
      Spent_Time   :    out Duration)
   is
      Delta_Angle : constant Integer := Angle - Sprite.Rotation;
   begin
      if Delta_Angle >= 0 then
         Sprite.Rotate_Rel (Delta_Angle, Velocity, Acceleration, Spent_Time);
      else
         Sprite.Rotate_Rel (Delta_Angle, -Velocity, -Acceleration, Spent_Time);
      end if;
   end Rotate_To;

   ----------------
   -- Rotate_Rel --
   ----------------

   procedure Rotate_Rel
     (Sprite       : in out Sprite_Type;
      Rel_Angle    : in     Integer;
      Velocity     : in     Velocity_Type;
      Acceleration : in     Acceleration_Type;
      Spent_Time   :    out Duration)
   is
      TFin : Natural;
   begin
      Spent_Time := 0.0;
      if abs Acceleration > 0.0 then
         declare
            --  Quadratic time equation: A/2 * t**2 + V * t - RA = 0
            --  Discriminant: V**2 - 4 * (A/2) * (-RA)
            --  Roots: (-V +/- Sqrt(V**2 - 4 * (A/2) * (-RA))) / 2 (A/2)
            Discriminant : constant Float := Velocity**2 + 2.0 * Float (Rel_Angle) * Acceleration;
            Root1        : constant Float := (Sqrt (Discriminant) - Velocity) / Acceleration;
            Root2        : constant Float := (-Sqrt (Discriminant) - Velocity) / Acceleration;
         begin
            Spent_Time := Duration (if Root1 > Root2 then Root1 else Root2);
         end;
      elsif abs Velocity > 0.0 then
         Spent_Time := Duration (Float (Rel_Angle) / Velocity);
      elsif abs Rel_Angle > 0 then
         raise Constraint_Error with "Rotate to another point but no velocity nor acceleration";
      end if;
      TFin := Natural (Frame_Rate * Spent_Time);
      if TFin > 0 then
         Sprite.Rotation_Velocity (Velocity);
         Sprite.Rotation_Acceleration (Acceleration);
         Sprite.Loop_Times (0, TFin);
      end if;
   end Rotate_Rel;

   -----------------
   -- Frame_Limit --
   -----------------

   procedure Frame_Limit
     (Sprite                                   : in out Sprite_Type;
      Row_Min, Row_Max, Column_Min, Column_Max : in     Integer;
      Effect                                   : in     Effect_Type;
      Handler                                  : in     Sprite_Event := null)
   is
   begin
      Sprite.Property ("gnoga_row_min", Row_Min);
      Sprite.Property ("gnoga_row_max", Row_Max);
      Sprite.Property ("gnoga_col_min", Column_Min);
      Sprite.Property ("gnoga_col_max", Column_Max);
      Sprite.Property ("gnoga_frame_effect", Effect_Type'Pos (Effect));
      Sprite.On_Frame_Event := Handler;
   end Frame_Limit;

   procedure Frame_Effect
     (Sprite : in out Sprite_Type;
      Effect : in     Effect_Type)
   is
   begin
      Sprite.Property ("gnoga_frame_effect", Effect_Type'Pos (Effect));
   end Frame_Effect;

   function Frame_Effect
     (Sprite : in Sprite_Type)
      return Effect_Type
   is
   begin
      return Effect_Type'Val (Sprite.Property ("gnoga_frame_effect"));
   end Frame_Effect;

   procedure Fire_On_Frame
     (Sprite : in out Sprite_Type;
      Effect : in     Effect_Type)
   is
   begin
      if Sprite.On_Frame_Event /= null then
         Sprite.On_Frame_Event (Sprite, Effect);
      end if;
   end Fire_On_Frame;

   -----------------
   -- Angle_Limit --
   -----------------

   procedure Angle_Limit
     (Sprite               : in out Sprite_Type;
      Angle_Min, Angle_Max : in     Integer;
      Effect               : in     Effect_Type;
      Handler              : in     Sprite_Event := null)
   is
   begin
      Sprite.Property ("gnoga_angle_min", Float (Angle_Min) * Ada.Numerics.Pi / 180.0);
      Sprite.Property ("gnoga_angle_max", Float (Angle_Max) * Ada.Numerics.Pi / 180.0);
      Sprite.Property ("gnoga_angle_effect", Effect_Type'Pos (Effect));
      Sprite.On_Angle_Event := Handler;
   end Angle_Limit;

   procedure Angle_Effect
     (Sprite : in out Sprite_Type;
      Effect : in     Effect_Type)
   is
   begin
      Sprite.Property ("gnoga_angle_effect", Effect_Type'Pos (Effect));
   end Angle_Effect;

   function Angle_Effect
     (Sprite : in Sprite_Type)
      return Effect_Type
   is
   begin
      return Effect_Type'Val (Sprite.Property ("gnoga_angle_effect"));
   end Angle_Effect;

   procedure Fire_On_Angle
     (Sprite : in out Sprite_Type;
      Effect : in     Effect_Type)
   is
   begin
      if Sprite.On_Angle_Event /= null then
         Sprite.On_Angle_Event (Sprite, Effect);
      end if;
   end Fire_On_Angle;

   ----------------
   -- On_Message --
   ----------------

   overriding procedure On_Message
     (Object  : in out Sprite_Type;
      Event   : in     String;
      Message : in     String)
   is
   begin
      -- Sprite Event --
      if Event = "inside_frame" then
         Object.Fire_On_Frame (Inside_Event_Effect);
      elsif Event = "outside_frame" then
         Object.Fire_On_Frame (Outside_Event_Effect);
      elsif Event = "inside_angle" then
         Object.Fire_On_Angle (Inside_Event_Effect);
      elsif Event = "outside_angle" then
         Object.Fire_On_Angle (Outside_Event_Effect);
      else
         Gnoga.Gui.Base.Base_Type (Object).On_Message (Event, Message);
      end if;
   end On_Message;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Sprite : in out Sprite_Type;
      Parent : in out Container_Type'Class)
   is
      pragma Unreferenced (Parent);
   begin
      Sprite.Finalize;
   end Delete;

end Gnoga.Gui.Plugin.Pixi.Sprite;
