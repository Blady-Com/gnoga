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

with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.Element.Canvas;

package Gnoga.Gui.Plugin.Pixi is

   procedure Load_PIXI (Window : in out Gnoga.Gui.Window.Window_Type'Class);
   --  Load PIXI code into Window

   type Renderer_Type is new Gnoga.Gui.Base.Base_Type with private;
   type Renderer_Access is access all Renderer_Type;
   type Pointer_To_Renderer_Class is access all Renderer_Type'Class;

   type Container_Type is new Gnoga.Gui.Base.Base_Type with private;
   type Container_Access is access all Container_Type;
   type Pointer_To_Container_Class is access all Container_Type'Class;

   procedure Get_Drawing_Context_2D
     (Renderer : in out Renderer_Type;
      Canvas   : in     Gnoga.Gui.Element.Canvas.Canvas_Type'Class);
   --  Create renderer associated to the canvas

   procedure Render
     (Renderer  : in out Renderer_Type;
      Container : in     Container_Type'Class);
   --  Render the graphic objects in the container

   procedure Auto_Rendering
     (Renderer  : in out Renderer_Type;
      Container : in     Container_Type'Class;
      Enable    : in     Boolean);
   function Auto_Rendering (Renderer : in out Renderer_Type) return Boolean;

   procedure Create
     (Container : in out Container_Type;
      Renderer  : in out Renderer_Type'Class);

   procedure Create
     (Container : in out Container_Type;
      Parent    : in out Container_Type'Class);

   procedure Add_Child
     (Container : in out Container_Type;
      Child     : in     Container_Type'Class);
   --  Add a child graphic object in the container

   procedure Remove_Child
     (Container : in out Container_Type;
      Child     : in     Container_Type'Class);
   --  Remove a child graphic object in the container

private
   type Renderer_Type is new Gnoga.Gui.Base.Base_Type with null record;
   type Container_Type is new Gnoga.Gui.Base.Base_Type with null record;
end Gnoga.Gui.Plugin.Pixi;
