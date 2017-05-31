------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                  G N O G A . G U I . P L U G I N . P I X I               --
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

with Gnoga.Server.Connection;

package body Gnoga.Gui.Plugin.Pixi is

   ---------------
   -- Load_PIXI --
   ---------------

   procedure Load_PIXI (Window : in out Gnoga.Gui.Window.Window_Type'Class) is
   begin
      Window.Document.Head_Element.jQuery_Execute
      ("append('<script src=""/js/pixi.js""" &
       " type=""text/javascript"" charset=""utf-8""></script>')");
   end Load_PIXI;

   ------------
   -- Create --
   ------------

   procedure Create
     (Renderer : in out Renderer_Type;
      Canvas   : in     Gnoga.Gui.Element.Canvas.Canvas_Type'Class)
   is
      Renderer_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Renderer.ID (Renderer_ID, Gnoga.Types.Gnoga_ID);
      Renderer.Connection_ID (Canvas.Connection_ID);

      Gnoga.Server.Connection.Execute_Script
        (Renderer.Connection_ID,
         "gnoga['" &
         Renderer_ID &
         "'] = new PIXI.CanvasRenderer(" &
         Canvas.Width'Img &
         ", " &
         Canvas.Height'Img &
         ", {view : document.getElementById('" &
         Canvas.ID &
         "')});");
      Renderer.Property ("gnoga_autoRendering", False);
   end Create;

   ------------
   -- Render --
   ------------

   procedure Render
     (Renderer  : in out Renderer_Type;
      Container : in     Container_Type'Class)
   is
   begin
      Renderer.Execute ("render(gnoga['" & Container.ID & "']);");
   end Render;

   --------------------
   -- Auto_Rendering --
   --------------------

   procedure Auto_Rendering
     (Renderer  : in out Renderer_Type;
      Container : in     Container_Type'Class;
      Enable    : in     Boolean)
   is
   begin
      Renderer.Property ("gnoga_autoRendering", Enable);
      if Enable then
         Gnoga.Server.Connection.Execute_Script
           (Renderer.Connection_ID,
            "gnoga['" &
            Renderer.ID &
            "'].gnoga_animate=function gnoga_gameLoop() {" &
            " if (gnoga['" &
            Renderer.ID &
            "'].gnoga_autoRendering) {requestAnimationFrame(gnoga_gameLoop);}" &
            " for (var gnoga_sprite of gnoga['" &
            Container.ID &
            "'].children) if (gnoga_sprite instanceof PIXI.Sprite)" &
            " {gnoga_sprite.gnoga_vx += gnoga_sprite.gnoga_ax; gnoga_sprite.gnoga_vy += gnoga_sprite.gnoga_ay;" &
            " gnoga_sprite.x += gnoga_sprite.gnoga_vx; gnoga_sprite.y += gnoga_sprite.gnoga_vy;" &
            " gnoga_sprite.gnoga_vr += gnoga_sprite.gnoga_ar;" &
            " gnoga_sprite.rotation += gnoga_sprite.gnoga_vr;" &
            " if (gnoga_sprite.gnoga_tfin > 0)" &
            " {gnoga_sprite.gnoga_tcur += 1;" &
            " if (gnoga_sprite.gnoga_tcur > gnoga_sprite.gnoga_tfin)" &
            " {gnoga_sprite.gnoga_tcur = 0; gnoga_sprite.gnoga_tfin = 0; gnoga_sprite.gnoga_vx = 0;" &
            " gnoga_sprite.gnoga_vy = 0; gnoga_sprite.gnoga_ax = 0; gnoga_sprite.gnoga_ay = 0;}}}" &
            " gnoga['" &
            Renderer.ID &
            "'].render(gnoga['" &
            Container.ID &
            "']);};");
         Renderer.Execute ("gnoga_animate();");
      end if;
   end Auto_Rendering;

   --------------------
   -- Auto_Rendering --
   --------------------

   function Auto_Rendering (Renderer : in out Renderer_Type) return Boolean is
   begin
      return Renderer.Property ("gnoga_autoRendering");
   end Auto_Rendering;

   ------------
   -- Create --
   ------------

   procedure Create
     (Container : in out Container_Type;
      Renderer  : in out Renderer_Type'Class)
   is
      Container_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Container.ID (Container_ID, Gnoga.Types.Gnoga_ID);
      Container.Connection_ID (Renderer.Connection_ID);
      Gnoga.Server.Connection.Execute_Script
        (Container.Connection_ID,
         "gnoga['" & Container_ID & "'] = new PIXI.Container();");
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Container : in out Container_Type;
      Parent    : in out Container_Type'Class)
   is
      Container_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Container.ID (Container_ID, Gnoga.Types.Gnoga_ID);
      Container.Connection_ID (Parent.Connection_ID);
      Gnoga.Server.Connection.Execute_Script
        (Container.Connection_ID,
         "gnoga['" & Container_ID & "'] = new PIXI.Container();");
   end Create;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (Container : in out Container_Type;
      Child     : in     Container_Type'Class)
   is
   begin
      Container.Execute ("addChild(gnoga['" & Child.ID & "']);");
   end Add_Child;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child
     (Container : in out Container_Type;
      Child     : in     Container_Type'Class)
   is
   begin
      Container.Execute ("removeChild(gnoga['" & Child.ID & "']);");
   end Remove_Child;

   ---------------------
   -- Remove_Children --
   ---------------------

   procedure Remove_Children (Container : in out Container_Type) is
   begin
      Container.Execute ("removeChildren();");
   end Remove_Children;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (Container : in out Container_Type;
      Parent    : in     Container_Type'Class)
   is
   begin
      Container.Execute ("setParent(gnoga['" & Parent.ID & "']);");
   end Set_Parent;

   ----------------
   -- Get_Bounds --
   ----------------

   function Get_Bounds
     (Container : in Container_Type) return Gnoga.Types.Rectangle_Type
   is
   begin
      return
        (Container.Property ("getBounds().x;"),
         Container.Property ("getBounds().y;"),
         Container.Property ("getBounds().width;"),
         Container.Property ("getBounds().height;"));
   end Get_Bounds;

   ----------------
   -- Get_Bounds --
   ----------------

   procedure Get_Bounds
     (Container : in     Container_Type;
      Rect      :    out Gnoga.Types.Rectangle_Type)
   is
   begin
      Rect :=
        (Container.Property ("getBounds().x;"),
         Container.Property ("getBounds().y;"),
         Container.Property ("getBounds().width;"),
         Container.Property ("getBounds().height;"));
   end Get_Bounds;

   ----------------------
   -- Get_Local_Bounds --
   ----------------------

   function Get_Local_Bounds
     (Container : in Container_Type) return Gnoga.Types.Rectangle_Type
   is
   begin
      return
        (Container.Property ("getLocalBounds().x;"),
         Container.Property ("getLocalBounds().y;"),
         Container.Property ("getLocalBounds().width;"),
         Container.Property ("getLocalBounds().height;"));
   end Get_Local_Bounds;

   ----------------------
   -- Get_Local_Bounds --
   ----------------------

   procedure Get_Local_Bounds
     (Container : in     Container_Type;
      Rect      :    out Gnoga.Types.Rectangle_Type)
   is
   begin
      Rect :=
        (Container.Property ("getLocalBounds().x;"),
         Container.Property ("getLocalBounds().y;"),
         Container.Property ("getLocalBounds().width;"),
         Container.Property ("getLocalBounds().height;"));
   end Get_Local_Bounds;

   ---------------
   -- To_Global --
   ---------------

   function To_Global
     (Container : in Container_Type;
      Position  :    Gnoga.Types.Point_Type) return Gnoga.Types.Point_Type
   is
   begin
      return
        (Container.Property
         ("toGlobal(" & Position.X'Img & ',' & Position.Y'Img & ").x;"),
         Container.Property
         ("toGlobal(" & Position.X'Img & ',' & Position.Y'Img & ").y;"));
   end To_Global;

   ---------------
   -- To_Global --
   ---------------

   procedure To_Global
     (Container : in     Container_Type;
      Position  :        Gnoga.Types.Point_Type;
      Point     :    out Gnoga.Types.Point_Type)
   is
   begin
      Point :=
        (Container.Property
         ("toGlobal(" & Position.X'Img & ',' & Position.Y'Img & ").x;"),
         Container.Property
         ("toGlobal(" & Position.X'Img & ',' & Position.Y'Img & ").y;"));
   end To_Global;

   --------------
   -- To_Local --
   --------------

   function To_Local
     (Container : in Container_Type;
      Position  :    Gnoga.Types.Point_Type) return Gnoga.Types.Point_Type
   is
   begin
      return
        (Container.Property
         ("toLocal(" & Position.X'Img & ',' & Position.Y'Img & ").x;"),
         Container.Property
         ("toLocal(" & Position.X'Img & ',' & Position.Y'Img & ").y;"));
   end To_Local;

   --------------
   -- To_Local --
   --------------

   procedure To_Local
     (Container : in     Container_Type;
      Position  :        Gnoga.Types.Point_Type;
      Point     :    out Gnoga.Types.Point_Type)
   is
   begin
      Point :=
        (Container.Property
         ("toLocal(" & Position.X'Img & ',' & Position.Y'Img & ").x;"),
         Container.Property
         ("toLocal(" & Position.X'Img & ',' & Position.Y'Img & ").y;"));
   end To_Local;

   -------------------
   -- Set_Transform --
   -------------------

   procedure Set_Transform
     (Container      : in out Container_Type;
      x, y           : in     Integer;
      scaleX, scaleY : in     Integer;
      rotation       : in     Integer;
      skewX, skewY   : in     Integer;
      pivotX, pivotY : in     Integer)
   is
   begin
      Container.Execute
      ("setTransform(" &
       x'Img &
       ',' &
       y'Img &
       ',' &
       scaleX'Img &
       ',' &
       scaleY'Img &
       ',' &
       rotation'Img &
       ',' &
       skewX'Img &
       ',' &
       skewY'Img &
       ',' &
       pivotX'Img &
       ',' &
       pivotY'Img &
       ");");
   end Set_Transform;

   ----------------------
   -- Update_Transform --
   ----------------------

   procedure Update_Transform (Container : in out Container_Type) is
   begin
      Container.Execute ("updateTransform();");
   end Update_Transform;

   ------------
   -- Create --
   ------------

   procedure Create
     (Texture   : in out Texture_Type;
      Renderer  : in out Renderer_Type'Class;
      Image_URL : in     String)
   is
      Texture_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Texture.ID (Texture_ID, Gnoga.Types.Gnoga_ID);
      Texture.Connection_ID (Renderer.Connection_ID);
      Gnoga.Server.Connection.Execute_Script
        (Texture.Connection_ID,
         "gnoga['" &
         Texture_ID &
         "'] = new PIXI.Texture.fromImage('" &
         Image_URL &
         "');");
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Texture  : in out Texture_Type;
      Renderer : in out Renderer_Type'Class;
      Canvas   : in     Gnoga.Gui.Element.Canvas.Canvas_Type'Class)
   is
      Texture_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Texture.ID (Texture_ID, Gnoga.Types.Gnoga_ID);
      Texture.Connection_ID (Renderer.Connection_ID);
      Gnoga.Server.Connection.Execute_Script
        (Texture.Connection_ID,
         "gnoga['" &
         Texture_ID &
         "'] = new PIXI.Texture.fromCanvas(gnoga['" &
         Canvas.ID &
         "']);");
   end Create;

   -----------
   -- Frame --
   -----------

   procedure Frame
     (Texture : in Texture_Type;
      Value   : in Gnoga.Types.Rectangle_Type)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Texture.Connection_ID,
         "gnoga['" &
         Texture.ID &
         "'].frame = new PIXI.Rectangle(" &
         Value.X'Img &
         ',' &
         Value.Y'Img &
         ',' &
         Value.Width'Img &
         ',' &
         Value.Height'Img &
         ");");
   end Frame;

   -----------
   -- Frame --
   -----------

   function Frame
     (Texture : in Texture_Type) return Gnoga.Types.Rectangle_Type
   is
   begin
      return
        (Texture.Property ("frame.x"),
         Texture.Property ("frame.y"),
         Texture.Property ("frame.width"),
         Texture.Property ("frame.height"));
   end Frame;

   -----------
   -- Width --
   -----------

   overriding procedure Width
     (Texture : in out Texture_Type;
      Value   : in     Integer)
   is
   begin
      Texture.Property ("width", Value);
   end Width;

   -----------
   -- Width --
   -----------

   overriding function Width (Texture : in Texture_Type) return Integer is
   begin
      return Texture.Property ("width");
   end Width;

   ------------
   -- Height --
   ------------

   overriding procedure Height
     (Texture : in out Texture_Type;
      Value   : in     Integer)
   is
   begin
      Texture.Property ("height", Value);
   end Height;

   ------------
   -- Height --
   ------------

   overriding function Height (Texture : in Texture_Type) return Integer is
   begin
      return Texture.Property ("height");
   end Height;

   ----------
   -- Orig --
   ----------

   procedure Orig
     (Texture : in Texture_Type;
      Value   : in Gnoga.Types.Rectangle_Type)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Texture.Connection_ID,
         "gnoga['" &
         Texture.ID &
         "'].orig = new PIXI.Rectangle(" &
         Value.X'Img &
         ',' &
         Value.Y'Img &
         ',' &
         Value.Width'Img &
         ',' &
         Value.Height'Img &
         ");");
   end Orig;

   ----------
   -- Orig --
   ----------

   function Orig
     (Texture : in Texture_Type) return Gnoga.Types.Rectangle_Type
   is
   begin
      return
        (Texture.Property ("orig.x"),
         Texture.Property ("orig.y"),
         Texture.Property ("orig.width"),
         Texture.Property ("orig.height"));
   end Orig;

   ----------
   -- Trim --
   ----------

   procedure Trim
     (Texture : in Texture_Type;
      Value   : in Gnoga.Types.Rectangle_Type)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Texture.Connection_ID,
         "gnoga['" &
         Texture.ID &
         "'].trim = new PIXI.Rectangle(" &
         Value.X'Img &
         ',' &
         Value.Y'Img &
         ',' &
         Value.Width'Img &
         ',' &
         Value.Height'Img &
         ");");
   end Trim;

   ----------
   -- Trim --
   ----------

   function Trim
     (Texture : in Texture_Type) return Gnoga.Types.Rectangle_Type
   is
   begin
      return
        (Texture.Property ("trim.x"),
         Texture.Property ("trim.y"),
         Texture.Property ("trim.width"),
         Texture.Property ("trim.height"));
   end Trim;

   ------------
   -- Create --
   ------------

   procedure Create
     (Style  : in out Style_Type;
      Parent : in out Container_Type'Class)
   is
      Style_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Style.ID (Style_ID, Gnoga.Types.Gnoga_ID);
      Style.Connection_ID (Parent.Connection_ID);
      Gnoga.Server.Connection.Execute_Script
        (Style.Connection_ID,
         "gnoga['" & Style_ID & "'] = new PIXI.TextStyle();");
   end Create;

   -----------
   -- Align --
   -----------

   procedure Align
     (Style : in out Style_Type;
      Value : in     Gnoga.Gui.Element.Alignment_Type)
   is
   begin
      Style.Property ("align", Value'Img);
   end Align;

   -----------
   -- Align --
   -----------

   function Align
     (Style : in out Style_Type) return Gnoga.Gui.Element.Alignment_Type
   is
   begin
      return Gnoga.Gui.Element.Alignment_Type'Value (Style.Property ("align"));
   end Align;

   -----------------
   -- Break_Words --
   -----------------

   procedure Break_Words (Style : in out Style_Type; Value : in Boolean) is
   begin
      Style.Property ("breakWords", Value);
   end Break_Words;

   -----------------
   -- Break_Words --
   -----------------

   function Break_Words (Style : in out Style_Type) return Boolean is
   begin
      return Style.Property ("breakWords");
   end Break_Words;

   -----------------
   -- Drop_Shadow --
   -----------------

   procedure Drop_Shadow (Style : in out Style_Type; Value : in Boolean) is
   begin
      Style.Property ("dropShadow", Value);
   end Drop_Shadow;

   -----------------
   -- Drop_Shadow --
   -----------------

   function Drop_Shadow (Style : in out Style_Type) return Boolean is
   begin
      return Style.Property ("dropShadow");
   end Drop_Shadow;

   -----------------------
   -- Drop_Shadow_Alpha --
   -----------------------

   procedure Drop_Shadow_Alpha
     (Style : in out Style_Type;
      Value : in     Gnoga.Types.Alpha_Type)
   is
   begin
      Style.Property ("dropShadowAlpha", Float (Value));
   end Drop_Shadow_Alpha;

   -----------------------
   -- Drop_Shadow_Alpha --
   -----------------------

   function Drop_Shadow_Alpha
     (Style : in out Style_Type) return Gnoga.Types.Alpha_Type
   is
   begin
      return Gnoga.Types.Alpha_Type
          (Float'(Style.Property ("dropShadowAlpha")));
   end Drop_Shadow_Alpha;

   -----------------------
   -- Drop_Shadow_Angle --
   -----------------------

   procedure Drop_Shadow_Angle
     (Style : in out Style_Type;
      Value : in     Integer)
   is
   begin
      Style.Property ("dropShadowAngle", Value);
   end Drop_Shadow_Angle;

   -----------------------
   -- Drop_Shadow_Angle --
   -----------------------

   function Drop_Shadow_Angle (Style : in out Style_Type) return Integer is
   begin
      return Style.Property ("dropShadowAngle");
   end Drop_Shadow_Angle;

   ----------------------
   -- Drop_Shadow_Blur --
   ----------------------

   procedure Drop_Shadow_Blur
     (Style : in out Style_Type;
      Value : in     Natural)
   is
   begin
      Style.Property ("dropShadowBlur", Value);
   end Drop_Shadow_Blur;

   ----------------------
   -- Drop_Shadow_Blur --
   ----------------------

   function Drop_Shadow_Blur (Style : in out Style_Type) return Natural is
   begin
      return Style.Property ("dropShadowBlur");
   end Drop_Shadow_Blur;

   -----------------------
   -- Drop_Shadow_Color --
   -----------------------

   procedure Drop_Shadow_Color
     (Style : in out Style_Type;
      Value : in     Gnoga.Types.Colors.Color_Enumeration)
   is
   begin
      Style.Property ("dropShadowColor", Gnoga.Types.Colors.To_String (Value));
   end Drop_Shadow_Color;

   -----------------------
   -- Drop_Shadow_Color --
   -----------------------

   function Drop_Shadow_Color
     (Style : in out Style_Type) return Gnoga.Types.Colors.Color_Enumeration
   is
   begin
      return Gnoga.Types.Colors.To_Color_Enumeration
          (Style.Property ("dropShadowColor"));
   end Drop_Shadow_Color;

   --------------------------
   -- Drop_Shadow_Distance --
   --------------------------

   procedure Drop_Shadow_Distance
     (Style : in out Style_Type;
      Value : in     Integer)
   is
   begin
      Style.Property ("dropShadowDistance", Value);
   end Drop_Shadow_Distance;

   --------------------------
   -- Drop_Shadow_Distance --
   --------------------------

   function Drop_Shadow_Distance (Style : in out Style_Type) return Integer is
   begin
      return Style.Property ("dropShadowDistance");
   end Drop_Shadow_Distance;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Style : in out Style_Type;
      Value : in     Gnoga.Types.Colors.Color_Enumeration)
   is
   begin
      Style.Property ("fill", Gnoga.Types.Colors.To_String (Value));
   end Fill;

   ----------
   -- Fill --
   ----------

   function Fill
     (Style : in out Style_Type) return Gnoga.Types.Colors.Color_Enumeration
   is
   begin
      return Gnoga.Types.Colors.To_Color_Enumeration (Style.Property ("fill"));
   end Fill;

   -----------------
   -- Font_Family --
   -----------------

   procedure Font_Family (Style : in out Style_Type; Value : in String) is
   begin
      Style.Property ("fontFamily", Value);
   end Font_Family;

   -----------------
   -- Font_Family --
   -----------------

   function Font_Family (Style : in out Style_Type) return String is
   begin
      return Style.Property ("fontFamily");
   end Font_Family;

   ---------------
   -- Font_Size --
   ---------------

   procedure Font_Size (Style : in out Style_Type; Value : in String) is
   begin
      Style.Property ("fontSize", Value);
   end Font_Size;

   ---------------
   -- Font_Size --
   ---------------

   function Font_Size (Style : in out Style_Type) return String is
   begin
      return Style.Property ("fontSize");
   end Font_Size;

   ----------------
   -- Font_Style --
   ----------------

   procedure Font_Style
     (Style : in out Style_Type;
      Value : in     Gnoga.Gui.Element.Font_Style_Type)
   is
   begin
      Style.Property ("fontStyle", Value'Img);
   end Font_Style;

   ----------------
   -- Font_Style --
   ----------------

   function Font_Style
     (Style : in out Style_Type) return Gnoga.Gui.Element.Font_Style_Type
   is
   begin
      return Gnoga.Gui.Element.Font_Style_Type'Value
          (Style.Property ("fontStyle"));
   end Font_Style;

   ------------------
   -- Font_Variant --
   ------------------

   procedure Font_Variant
     (Style : in out Style_Type;
      Value : in     Gnoga.Gui.Element.Font_Variant_Type)
   is
   begin
      Style.Property ("fontVariant", Value'Img);
   end Font_Variant;

   ------------------
   -- Font_Variant --
   ------------------

   function Font_Variant
     (Style : in out Style_Type) return Gnoga.Gui.Element.Font_Variant_Type
   is
   begin
      return Gnoga.Gui.Element.Font_Variant_Type'Value
          (Style.Property ("fontVariant"));
   end Font_Variant;

   -----------------
   -- Font_Weight --
   -----------------

   procedure Font_Weight
     (Style : in out Style_Type;
      Value : in     Gnoga.Gui.Element.Font_Weight_Type)
   is
   begin
      Style.Property ("fontWeight", Gnoga.Gui.Element.Image (Value));
   end Font_Weight;

   -----------------
   -- Font_Weight --
   -----------------

   function Font_Weight
     (Style : in out Style_Type) return Gnoga.Gui.Element.Font_Weight_Type
   is
   begin
      return Gnoga.Gui.Element.Value (Style.Property ("fontWeight"));
   end Font_Weight;

   --------------------
   -- Letter_Spacing --
   --------------------

   procedure Letter_Spacing (Style : in out Style_Type; Value : in Natural) is
   begin
      Style.Property ("letterSpacing", Value);
   end Letter_Spacing;

   --------------------
   -- Letter_Spacing --
   --------------------

   function Letter_Spacing (Style : in out Style_Type) return Natural is
   begin
      return Style.Property ("letterSpacing");
   end Letter_Spacing;

   -----------------
   -- Line_Height --
   -----------------

   procedure Line_Height (Style : in out Style_Type; Value : in Natural) is
   begin
      Style.Property ("lineHeight", Value);
   end Line_Height;

   -----------------
   -- Line_Height --
   -----------------

   function Line_Height (Style : in out Style_Type) return Natural is
   begin
      return Style.Property ("lineHeight");
   end Line_Height;

   ---------------
   -- Line_Join --
   ---------------

   procedure Line_Join
     (Style : in out Style_Type;
      Value : in     Line_Join_Type)
   is
   begin
      Style.Property ("lineJoin", Value'Img);
   end Line_Join;

   ---------------
   -- Line_Join --
   ---------------

   function Line_Join (Style : in out Style_Type) return Line_Join_Type is
   begin
      return Line_Join_Type'Value (Style.Property ("lineJoin"));
   end Line_Join;

   -----------------
   -- Miter_Limit --
   -----------------

   procedure Miter_Limit (Style : in out Style_Type; Value : in Natural) is
   begin
      Style.Property ("miterLimit", Value);
   end Miter_Limit;

   -----------------
   -- Miter_Limit --
   -----------------

   function Miter_Limit (Style : in out Style_Type) return Natural is
   begin
      return Style.Property ("miterLimit");
   end Miter_Limit;

   -------------
   -- Padding --
   -------------

   procedure Padding (Style : in out Style_Type; Value : in Natural) is
   begin
      Style.Property ("padding", Value);
   end Padding;

   -------------
   -- Padding --
   -------------

   function Padding (Style : in out Style_Type) return Natural is
   begin
      return Style.Property ("padding");
   end Padding;

   ------------
   -- Stroke --
   ------------

   procedure Stroke
     (Style : in out Style_Type;
      Value : in     Gnoga.Types.Colors.Color_Enumeration)
   is
   begin
      Style.Property ("stroke", Gnoga.Types.Colors.To_String (Value));
   end Stroke;

   ------------
   -- Stroke --
   ------------

   function Stroke
     (Style : in out Style_Type) return Gnoga.Types.Colors.Color_Enumeration
   is
   begin
      return Gnoga.Types.Colors.To_Color_Enumeration
          (Style.Property ("stroke"));
   end Stroke;

   ----------------------
   -- Stroke_Thickness --
   ----------------------

   procedure Stroke_Thickness
     (Style : in out Style_Type;
      Value : in     Natural)
   is
   begin
      Style.Property ("strokeThickness", Value);
   end Stroke_Thickness;

   ----------------------
   -- Stroke_Thickness --
   ----------------------

   function Stroke_Thickness (Style : in out Style_Type) return Natural is
   begin
      return Style.Property ("strokeThickness");
   end Stroke_Thickness;

   ----------
   -- Trim --
   ----------

   procedure Trim (Style : in out Style_Type; Value : in Boolean) is
   begin
      Style.Property ("trim", Value);
   end Trim;

   ----------
   -- Trim --
   ----------

   function Trim (Style : in out Style_Type) return Boolean is
   begin
      return Style.Property ("trim");
   end Trim;

   --------------------
   -- Text_Base_line --
   --------------------

   procedure Text_Baseline
     (Style : in out Style_Type;
      Value : in     Baseline_Type)
   is
   begin
      Style.Property ("textBaseline", Value'Img);
   end Text_Baseline;

   --------------------
   -- Text_Base_line --
   --------------------

   function Text_Baseline (Style : in out Style_Type) return Baseline_Type is
   begin
      return Baseline_Type'Value (Style.Property ("textBaseline"));
   end Text_Baseline;

   ---------------
   -- Word_Wrap --
   ---------------

   procedure Word_Wrap (Style : in out Style_Type; Value : in Boolean) is
   begin
      Style.Property ("wordWrap", Value);
   end Word_Wrap;

   ---------------
   -- Word_Wrap --
   ---------------

   function Word_Wrap (Style : in out Style_Type) return Boolean is
   begin
      return Style.Property ("wordWrap");
   end Word_Wrap;

   ---------------------
   -- Word_Wrap_Width --
   ---------------------

   procedure Word_Wrap_Width (Style : in out Style_Type; Value : in Natural) is
   begin
      Style.Property ("wordWrapWidth", Value);
   end Word_Wrap_Width;

   ---------------------
   -- Word_Wrap_Width --
   ---------------------

   function Word_Wrap_Width (Style : in out Style_Type) return Natural is
   begin
      return Style.Property ("wordWrapWidth");
   end Word_Wrap_Width;

end Gnoga.Gui.Plugin.Pixi;
