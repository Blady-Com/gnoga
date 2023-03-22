------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--            G N O G A . G U I . P L U G I N . P I X I . T E X T           --
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

package body Gnoga.Gui.Plugin.Pixi.Text is

   ------------
   -- Create --
   ------------

   procedure Create
     (Text        : in out Text_Type;
      Parent      : in out Container_Type'Class;
      Message     : in     String;
      Row, Column : in     Integer)
   is
      Text_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Text.ID (Text_ID, Gnoga.Types.Gnoga_ID);
      Text.Connection_ID (Parent.Connection_ID);
      Gnoga.Server.Connection.Execute_Script
        (Text.Connection_ID, "gnoga['" & Text_ID & "'] = new PIXI.Text('" & Escape_Quotes (Message) & "');");
      Text.Locate (Row, Column);
      Text.Motion (0.0, 0.0);
      Text.Acceleration (0.0, 0.0);
      Text.Rotation_Velocity (0.0);
      Text.Rotation_Acceleration (0.0);
      Text.Parent (Parent);
   end Create;

   ------------
   -- Locate --
   ------------

   procedure Locate
     (Text        : in out Text_Type;
      Row, Column : in     Integer)
   is
   begin
      Text.Property ("x", Column);
      Text.Property ("y", Row);
   end Locate;

   --------------
   -- Position --
   --------------

   procedure Position
     (Text        : in     Text_Type;
      Row, Column :    out Integer)
   is
   begin
      Row    := Text.Property ("y");
      Column := Text.Property ("x");
   end Position;

   ---------
   -- Row --
   ---------

   function Row
     (Text : in Text_Type)
      return Integer
   is
      V : constant Float := Text.Property ("y");
   begin
      return Integer (V);
   end Row;

   ------------
   -- Column --
   ------------

   function Column
     (Text : in Text_Type)
      return Integer
   is
      V : constant Float := Text.Property ("x");
   begin
      return Integer (V);
   end Column;

   ------------
   -- Motion --
   ------------

   procedure Motion
     (Text                          : in out Text_Type;
      Row_Velocity, Column_Velocity : in     Velocity_Type)
   is
   begin
      Text.Property ("gnoga_vx", Column_Velocity / Frame_Rate);
      Text.Property ("gnoga_vy", Row_Velocity / Frame_Rate);
   end Motion;

   ------------------
   -- Row_Velocity --
   ------------------

   function Row_Velocity
     (Text : in Text_Type)
      return Velocity_Type
   is
   begin
      return Text.Property ("gnoga_vy") * Frame_Rate;
   end Row_Velocity;

   ---------------------
   -- Column_Velocity --
   ---------------------

   function Column_Velocity
     (Text : in Text_Type)
      return Velocity_Type
   is
   begin
      return Text.Property ("gnoga_vx") * Frame_Rate;
   end Column_Velocity;

   ------------------
   -- Acceleration --
   ------------------

   procedure Acceleration
     (Text                                  : in out Text_Type;
      Row_Acceleration, Column_Acceleration : in     Acceleration_Type)
   is
   begin
      Text.Property ("gnoga_ax", Column_Acceleration / Frame_Rate / Frame_Rate);
      Text.Property ("gnoga_ay", Row_Acceleration / Frame_Rate / Frame_Rate);
   end Acceleration;

   ----------------------
   -- Row_Acceleration --
   ----------------------

   function Row_Acceleration
     (Text : in Text_Type)
      return Acceleration_Type
   is
   begin
      return Text.Property ("gnoga_ay") * (Frame_Rate * Frame_Rate);
   end Row_Acceleration;

   -------------------------
   -- Column_Acceleration --
   -------------------------

   function Column_Acceleration
     (Text : in Text_Type)
      return Acceleration_Type
   is

   begin
      return Text.Property ("gnoga_ax") * (Frame_Rate * Frame_Rate);
   end Column_Acceleration;

   -----------
   -- Alpha --
   -----------

   procedure Alpha
     (Text  : in out Text_Type;
      Value : in     Gnoga.Types.Alpha_Type)
   is
   begin
      Text.Property ("alpha", Float (Value));
   end Alpha;

   -----------
   -- Alpha --
   -----------

   function Alpha
     (Text : in Text_Type)
      return Gnoga.Types.Alpha_Type
   is
   begin
      return Gnoga.Types.Alpha_Type (Float'(Text.Property ("alpha")));
   end Alpha;

   ------------
   -- Anchor --
   ------------

   procedure Anchor
     (Text        : in out Text_Type;
      Row, Column : in     Gnoga.Types.Fractional_Range_Type)
   is
      function Image is new UXStrings.Conversions.Fixed_Point_Image (Gnoga.Types.Fractional_Range_Type);
   begin
      Gnoga.Server.Connection.Execute_Script
        (Text.Connection_ID, "gnoga['" & Text.ID & "'].anchor = {x:" & Image (Column) & ",y:" & Image (Row) & "};");
   end Anchor;

   ----------------
   -- Row_Anchor --
   ----------------

   function Row_Anchor
     (Text : in Text_Type)
      return Gnoga.Types.Fractional_Range_Type
   is
   begin
      return Gnoga.Types.Fractional_Range_Type (Float'(Text.Execute ("anchor.y")));
   end Row_Anchor;

   -------------------
   -- Column_Anchor --
   -------------------

   function Column_Anchor
     (Text : in Text_Type)
      return Gnoga.Types.Fractional_Range_Type
   is
   begin
      return Gnoga.Types.Fractional_Range_Type (Float'(Text.Execute ("anchor.x")));
   end Column_Anchor;

   ----------------
   -- Blend_Mode --
   ----------------

   procedure Blend_Mode
     (Text  : in out Text_Type;
      Value : in     Blend_Modes_Type)
   is
      function Image is new UXStrings.Conversions.Scalar_Image (Blend_Modes_Type);
   begin
      Text.Property ("blendMode", Image (Value));
   end Blend_Mode;

   ----------------
   -- Blend_Mode --
   ----------------

   function Blend_Mode
     (Text : in Text_Type)
      return Blend_Modes_Type
   is
      function Value is new UXStrings.Conversions.Scalar_Value (Blend_Modes_Type);
   begin
      return Value (Text.Property ("blendMode"));
   end Blend_Mode;

   -------------
   -- Message --
   -------------

   procedure Message
     (Text  : in out Text_Type;
      Value : in     String)
   is
   begin
      Text.Property ("text", Value);
   end Message;

   -------------
   -- Message --
   -------------

   function Message
     (Text : in Text_Type)
      return String
   is
   begin
      return Text.Property ("text");
   end Message;

   -----------
   -- Pivot --
   -----------

   procedure Pivot
     (Text        : in out Text_Type;
      Row, Column : in     Integer)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Text.Connection_ID, "gnoga['" & Text.ID & "'].pivot = {x:" & Image (Column) & ",y:" & Image (Row) & "};");
   end Pivot;

   ---------------
   -- Row_Pivot --
   ---------------

   function Row_Pivot
     (Text : in Text_Type)
      return Integer
   is
   begin
      return Text.Execute ("pivot.x");
   end Row_Pivot;

   ------------------
   -- Column_Pivot --
   ------------------

   function Column_Pivot
     (Text : in Text_Type)
      return Integer
   is
   begin
      return Text.Execute ("pivot.y");
   end Column_Pivot;

   --------------
   -- Rotation --
   --------------

   procedure Rotation
     (Text  : in out Text_Type;
      Value : in     Integer)
   is
   begin
      Text.Property ("rotation", Float (Value) * Ada.Numerics.Pi / 180.0);
   end Rotation;

   --------------
   -- Rotation --
   --------------

   function Rotation
     (Text : in Text_Type)
      return Integer
   is
   begin
      return Integer (Float'(Text.Property ("rotation")) * 180.0 / Ada.Numerics.Pi);
   end Rotation;

   -----------------------
   -- Rotation_Velocity --
   -----------------------

   procedure Rotation_Velocity
     (Text  : in out Text_Type;
      Value :        Velocity_Type)
   is
   begin
      Text.Property ("gnoga_vr", Value / Frame_Rate * Ada.Numerics.Pi / 180.0);
   end Rotation_Velocity;

   -----------------------
   -- Rotation_Velocity --
   -----------------------

   function Rotation_Velocity
     (Text : in Text_Type)
      return Velocity_Type
   is
   begin
      return Float'(Text.Property ("gnoga_vr")) * Frame_Rate * 180.0 / Ada.Numerics.Pi;
   end Rotation_Velocity;

   -----------------------
   -- Rotation_Acceleration --
   -----------------------

   procedure Rotation_Acceleration
     (Text  : in out Text_Type;
      Value :        Acceleration_Type)
   is
   begin
      Text.Property ("gnoga_ar", Value / Frame_Rate / Frame_Rate * Ada.Numerics.Pi / 180.0);
   end Rotation_Acceleration;

   -----------------------
   -- Rotation_Acceleration --
   -----------------------

   function Rotation_Acceleration
     (Text : in Text_Type)
      return Acceleration_Type
   is
   begin
      return Float'(Text.Property ("gnoga_ar")) * Frame_Rate * Frame_Rate * 180.0 / Ada.Numerics.Pi;
   end Rotation_Acceleration;

   -----------
   -- Width --
   -----------

   overriding procedure Width
     (Text  : in out Text_Type;
      Value : in     Integer)
   is
   begin
      Text.Property ("width", Value);
   end Width;

   -----------
   -- Width --
   -----------

   overriding function Width
     (Text : in Text_Type)
      return Integer
   is
   begin
      return Text.Property ("width");
   end Width;

   ------------
   -- Height --
   ------------

   overriding procedure Height
     (Text  : in out Text_Type;
      Value : in     Integer)
   is
   begin
      Text.Property ("height", Value);
   end Height;

   ------------
   -- Height --
   ------------

   overriding function Height
     (Text : in Text_Type)
      return Integer
   is
   begin
      return Text.Property ("height");
   end Height;

   -----------
   -- Scale --
   -----------

   procedure Scale
     (Text        : in out Text_Type;
      Row, Column : in     Positive)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Text.Connection_ID, "gnoga['" & Text.ID & "'].scale = {x:" & Image (Column) & ",y:" & Image (Row) & "};");
   end Scale;

   ---------------
   -- Row_Scale --
   ---------------

   function Row_Scale
     (Text : in Text_Type)
      return Positive
   is
   begin
      return Text.Execute ("scale.y");
   end Row_Scale;

   ------------------
   -- Column_Scale --
   ------------------

   function Column_Scale
     (Text : in Text_Type)
      return Positive
   is
   begin
      return Text.Execute ("scale.x");
   end Column_Scale;

   ----------
   -- Skew --
   ----------

   procedure Skew
     (Text        : in out Text_Type;
      Row, Column : in     Positive)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Text.Connection_ID, "gnoga['" & Text.ID & "'].skew = {x:" & Image (Column) & ",y:" & Image (Row) & "};");
   end Skew;

   --------------
   -- Row_Skew --
   --------------

   function Row_Skew
     (Text : in Text_Type)
      return Positive
   is
   begin
      return Text.Execute ("skew.y");
   end Row_Skew;

   -----------------
   -- Column_Skew --
   -----------------

   function Column_Skew
     (Text : in Text_Type)
      return Positive
   is
   begin
      return Text.Execute ("skew.x");
   end Column_Skew;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
     (Text  : in out Text_Type;
      Value : in     Style_Type'Class)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Text.Connection_ID, "gnoga['" & Text.ID & "'].style = gnoga['" & Value.ID & "'];");
   end Set_Style;

   ---------------
   -- Get_Style --
   ---------------

   procedure Get_Style
     (Text  : in     Text_Type;
      Value : in out Style_Type'Class)
   is
      Style_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Value.ID (Style_ID, Gnoga.Types.Gnoga_ID);
      Value.Connection_ID (Text.Connection_ID);
      Gnoga.Server.Connection.Execute_Script
        (Text.Connection_ID, "gnoga['" & Style_ID & "'] = gnoga['" & Text.ID & "'].style;");
   end Get_Style;

   ----------
   -- Tint --
   ----------

   procedure Tint
     (Text  : in out Text_Type;
      Value : in     Natural)
   is
   begin
      Text.Property ("tint", Value);
   end Tint;

   ----------
   -- Tint --
   ----------

   function Tint
     (Text : in Text_Type)
      return Natural
   is
   begin
      return Text.Property ("tint");
   end Tint;

   -------------
   -- Visible --
   -------------

   procedure Visible
     (Text  : in out Text_Type;
      Value : in     Boolean)
   is
   begin
      Text.Property ("visible", Value);
   end Visible;

   -------------
   -- Visible --
   -------------

   function Visible
     (Text : in Text_Type)
      return Boolean
   is
   begin
      return Text.Property ("visible");
   end Visible;

   -----------------
   -- Coincidence --
   -----------------

   function Coincidence
     (Text1, Text2 : in Text_Type;
      Tolerance    : in Natural)
      return Boolean
   is
   begin
      return Distance (Text1, Text2) <= Tolerance;
   end Coincidence;

   -----------------
   -- Coincidence --
   -----------------

   function Coincidence
     (Text        : in Text_Type;
      Row, Column : in Integer;
      Tolerance   : in Natural)
      return Boolean
   is
   begin
      return Distance (Text, Row, Column) <= Tolerance;
   end Coincidence;

   --------------
   -- Distance --
   --------------

   function Distance
     (Text1, Text2 : in Text_Type)
      return Natural
   is
   begin
      return
        Natural
          (Ada.Numerics.Elementary_Functions.Sqrt
             (Float (Text2.Row - Text1.Row)**2 + Float (Text2.Column - Text1.Column)**2));
   end Distance;

   --------------
   -- Distance --
   --------------

   function Distance
     (Text        : in Text_Type;
      Row, Column : in Integer)
      return Natural
   is
   begin
      return
        Natural (Ada.Numerics.Elementary_Functions.Sqrt (Float (Text.Row - Row)**2 + Float (Text.Column - Column)**2));
   end Distance;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Text   : in out Text_Type;
      Parent : in out Container_Type'Class)
   is
   begin
      Parent.Remove_Child (Text);
   end Delete;

   ----------------
   -- Delete_All --
   ----------------

   procedure Delete_All (Parent : in out Container_Type'Class) is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Parent.Connection_ID,
         "var gnoga_list = [];" & " for (var gnoga_text of gnoga['" & Parent.ID &
         "'].children) if (gnoga_text instanceof PIXI.Text) gnoga_list.push(gnoga_text);" &
         " for (var gnoga_text of gnoga_list)" & " gnoga['" & Parent.ID & "'].removeChild(gnoga_text);");
   end Delete_All;

end Gnoga.Gui.Plugin.Pixi.Text;
