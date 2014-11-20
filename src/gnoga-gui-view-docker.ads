------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                  G N O G A . G U I . V I E W . D O C K E R               --
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

--  Docker allows for laying out child views by docking them to the sides
--  of the Docker_View_Type. Docker_View_Types can be nested like all
--  view types and not all sides need to be docked, allowing for complex
--  layouts easily.

package Gnoga.Gui.View.Docker is

   -------------------------------------------------------------------------
   --  Docker_View_Types
   -------------------------------------------------------------------------

   type Docker_View_Type is new View_Type with private;
   type Docker_View_Access is access all Docker_View_Type;
   type Pointer_To_Docker_View_Class is access all Docker_View_Type'Class;

   -------------------------------------------------------------------------
   --  Docker_View_Type - Creation Methods
   -------------------------------------------------------------------------

   overriding
   procedure Create
     (View          : in out Docker_View_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach        : in     Boolean := True;
      ID            : in     String  := "");
   --  Docked Views are set to Position (Absolute) when added. In order to
   --  use a Docker_View_Type statically positioned the position should be
   --  set Position (Relative) in order to insure the Docks are positioned
   --  relative to itm since Position (Absolute) elements are positioned
   --  relative to the first non Position (Static) element above them.

   -------------------------------------------------------------------------
   --  Docker_View_Type - Properties
   -------------------------------------------------------------------------

   procedure Top_Dock (View : in out Docker_View_Type;
                       Dock : access View_Base_Type'Class);
   function Top_Dock (View : Docker_View_Type)
                      return Pointer_To_View_Base_Class;

   procedure Bottom_Dock (View : in out Docker_View_Type;
                          Dock : access View_Base_Type'Class);
   function Bottom_Dock (View : Docker_View_Type)
                         return Pointer_To_View_Base_Class;

   procedure Fill_Dock (View : in out Docker_View_Type;
                        Dock : access View_Base_Type'Class);
   function Fill_Dock (View : Docker_View_Type)
                       return Pointer_To_View_Base_Class;

   procedure Left_Dock (View : in out Docker_View_Type;
                        Dock : access View_Base_Type'Class);
   function Left_Dock (View : Docker_View_Type)
                       return Pointer_To_View_Base_Class;

   procedure Right_Dock (View : in out Docker_View_Type;
                         Dock : access View_Base_Type'Class);
   function Right_Dock (View : Docker_View_Type)
                        return Pointer_To_View_Base_Class;

   -------------------------------------------------------------------------
   --  Docker_View_Type - Event Methods
   -------------------------------------------------------------------------

   procedure Update_Dock (View : in out Docker_View_Type);
   --  Updates layout to current View size

   overriding
   procedure On_Resize (View : in out Docker_View_Type);
   --  Handle layout of children
private
   type Docker_View_Type is new View_Type with null record;
end Gnoga.Gui.View.Docker;
