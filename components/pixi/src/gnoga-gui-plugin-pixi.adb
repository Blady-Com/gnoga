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
            " {gnoga_sprite.x += gnoga_sprite.gnoga_vx; gnoga_sprite.y += gnoga_sprite.gnoga_vy;" &
            " gnoga_sprite.rotation += gnoga_sprite.gnoga_vr;}" &
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

end Gnoga.Gui.Plugin.Pixi;
