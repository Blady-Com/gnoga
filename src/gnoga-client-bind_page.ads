------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                 G N O G A . C L I E N T . B I N D _ P A G E              --
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
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

--  Given a page with existing HTML, this will create an Element_Type_Map of
--  every element on the page that has an ID.

with Gnoga.Gui.Element;
with Gnoga.Gui.View;

package Gnoga.Client.Bind_Page is

   procedure Bind_Page (View : in out Gnoga.Gui.View.View_Base_Type'Class);
   --  Attaches all HTML elements with an ID using View as parent. Elements
   --  will all be created and marked as dynamic and will be deallocated when
   --  View is finalized. Elements are accessible using the View Element array.
   --
   --  Note: If more than one element have the same ID they will be overwritten
   --        in the Element array.
   --  Note: The View does not have to be placed in to the View's partent DOM
   --  (or if there be visible) to be used, only that the View was created on
   --  the same page to be bound.
   --    e.g. My_View.Create (Main_Window, Attach => False);

end Gnoga.Client.Bind_Page;
