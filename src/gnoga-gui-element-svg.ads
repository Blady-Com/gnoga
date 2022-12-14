------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                 G N O G A . G U I . E L E M E N T . S V G                --
--                                                                          --
--                                 S p e c                                  --
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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

--  SVG - Scalable Vector Graphics
--     - The SVG DOM will be bound eventually for now can just pass in with
--     - content and can update SVG using innerHTML.
--     - I have not tried, but in theory you should be able to create and
--     - manipulate SVG elements already using Gnoga.Element.Create_With_HTML
--     - and using one of the place methods in to the SVG element.

with Gnoga.Gui.View;

package Gnoga.Gui.Element.SVG is
   -------------------------------------------------------------------------
   --  SVG_Types
   -------------------------------------------------------------------------

   type SVG_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type SVG_Access is access all SVG_Type;
   type Pointer_To_SVG_Class is access all SVG_Type'Class;

   -------------------------------------------------------------------------
   --  SVG_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (SVG     : in out SVG_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Content : in     String := "";
      ID      : in     String := "");
   --  Create a SVG container use Content for SVG XML if set.
   --  To create new SVG Elements use Element_Type.Create_XML_Element
   --  You can then use Element_Type.Style, Element_Type.Attribute
   --  Element_Type.Execute (to access the SVG DOM). e.g.
   --      Element.Execute ("height.baseVal.value", "50")
   --  You can also set mouse events on individual SVG elements using the
   --  On_*_Handlers.
   --
   --  To just load an SVG document as an img, use:
   --     Gnoga.Gui.Element.Common.IMG_Type

private
   type SVG_Type is new Gnoga.Gui.View.View_Base_Type with null record;
end Gnoga.Gui.Element.SVG;
