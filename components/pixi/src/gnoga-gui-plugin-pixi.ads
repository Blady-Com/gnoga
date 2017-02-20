------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                  G N O G A . G U I . P L U G I N . P I X I               --
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

with Gnoga.Types;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.Element.Canvas;

package Gnoga.Gui.Plugin.Pixi is

   --  PIXI Ada API is inpired by https://github.com/pixijs/pixi.js.
   --  Pixi.js is released under the (http://opensource.org/licenses/MIT) MIT License.
   --  Some comments come from Pixi.js documentation:
   --  Pixi.js is a rendering library that will allow you to create rich, interactive graphics,
   --  cross platform applications, and games without having to dive into the WebGL API or
   --  deal with browser and device compatibility.

   procedure Load_PIXI (Window : in out Gnoga.Gui.Window.Window_Type'Class);
   --  Load PIXI code into Window

   -------------------------------------------------------------------------
   --  Renderer_Type and Container_Type
   -------------------------------------------------------------------------

   type Renderer_Type is new Gnoga.Gui.Base.Base_Type with private;
   type Renderer_Access is access all Renderer_Type;
   type Pointer_To_Renderer_Class is access all Renderer_Type'Class;

   type Container_Type is new Gnoga.Gui.Base.Base_Type with private;
   type Container_Access is access all Container_Type;
   type Pointer_To_Container_Class is access all Container_Type'Class;

   -------------------------------------------------------------------------
   --  Renderer_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Renderer : in out Renderer_Type;
      Canvas   : in     Gnoga.Gui.Element.Canvas.Canvas_Type'Class);
   --  Create renderer associated to the canvas

   -------------------------------------------------------------------------
   --  Renderer_Type - Methods
   -------------------------------------------------------------------------

   procedure Render
     (Renderer  : in out Renderer_Type;
      Container : in     Container_Type'Class);
   --  Render the graphic objects included in the container

   procedure Auto_Rendering
     (Renderer  : in out Renderer_Type;
      Container : in     Container_Type'Class;
      Enable    : in     Boolean);
   function Auto_Rendering (Renderer : in out Renderer_Type) return Boolean;
   --  Enables periodic rendering based on animation frame

   -------------------------------------------------------------------------
   --  Container_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Container : in out Container_Type;
      Renderer  : in out Renderer_Type'Class);

   procedure Create
     (Container : in out Container_Type;
      Parent    : in out Container_Type'Class);

   -------------------------------------------------------------------------
   --  Container_Type - Methods
   -------------------------------------------------------------------------

   procedure Add_Child
     (Container : in out Container_Type;
      Child     : in     Container_Type'Class);
   --  Add a child graphic object in the container

   procedure Remove_Child
     (Container : in out Container_Type;
      Child     : in     Container_Type'Class);
   --  Remove a child graphic object in the container

   type Blend_Modes_Type is
     (NORMAL,
      ADD,
      MULTIPLY,
      SCREEN,
      OVERLAY,
      DARKEN,
      LIGHTEN,
      COLOR_DODGE,
      COLOR_BURN,
      HARD_LIGHT,
      SOFT_LIGHT,
      DIFFERENCE,
      EXCLUSION,
      HUE,
      SATURATION,
      COLOR,
      LUMINOSITY);
   --  Various blend modes supported by PIXI.

   type Scale_Modes_Type is (LINEAR, NEAREST);
   --  The scale modes that are supported by pixi.
   --  - LINEAR: Smooth scaling
   --  - NEAREST: Pixelating scaling

   -------------------------------------------------------------------------
   --  Texture_Type
   -------------------------------------------------------------------------

   type Texture_Type is new Gnoga.Gui.Base.Base_Type with private;
   type Texture_Access is access all Texture_Type;
   type Pointer_To_Texture_Class is access all Texture_Type'Class;

   -------------------------------------------------------------------------
   --  Texture_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Texture   : in out Texture_Type;
      Renderer  : in out Renderer_Type'Class;
      Image_URL : in     String);
   --  Helper function that creates a Texture object from the given image url.

   procedure Create
     (Texture  : in out Texture_Type;
      Renderer : in out Renderer_Type'Class;
      Canvas   : in     Gnoga.Gui.Element.Canvas.Canvas_Type'Class);
   --  Helper function that creates a new Texture based on the given canvas element.

   -------------------------------------------------------------------------
   --  Texture_Type - Properties
   -------------------------------------------------------------------------

   procedure Frame
     (Texture : in Texture_Type;
      Value   : in Gnoga.Types.Rectangle_Type);
   function Frame
     (Texture : in Texture_Type) return Gnoga.Types.Rectangle_Type;
   --  The frame specifies the region of the base texture that this texture uses.

   overriding procedure Width
     (Texture : in out Texture_Type;
      Value   : in     Integer);
   overriding function Width (Texture : in Texture_Type) return Integer;
   --  The height of the Texture in pixels.

   overriding procedure Height
     (Texture : in out Texture_Type;
      Value   : in     Integer);
   overriding function Height (Texture : in Texture_Type) return Integer;
   --  The height of the Texture in pixels.

   procedure Orig
     (Texture : in Texture_Type;
      Value   : in Gnoga.Types.Rectangle_Type);
   function Orig (Texture : in Texture_Type) return Gnoga.Types.Rectangle_Type;
   --  This is the area of original texture, before it was put in atlas.

   procedure Trim
     (Texture : in Texture_Type;
      Value   : in Gnoga.Types.Rectangle_Type);
   function Trim (Texture : in Texture_Type) return Gnoga.Types.Rectangle_Type;
   --  This is the trimmed area of original texture, before it was put in atlas.

private
   type Renderer_Type is new Gnoga.Gui.Base.Base_Type with null record;
   type Container_Type is new Gnoga.Gui.Base.Base_Type with null record;
   type Texture_Type is new Gnoga.Gui.Base.Base_Type with null record;
end Gnoga.Gui.Plugin.Pixi;
