------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--     G N O G A . G U I . P L U G I N . J Q U E R Y U I . W I D G E T      --
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

with Gnoga.Gui.View;

package Gnoga.Gui.Plugin.jQueryUI.Widget is

   --  jQueryUI adopts properties from the window. To insure UI objects
   --  are sized reasonably, it is good to set a font on the body, e.g.
   --    Main_Window.Document.Body_Element.Font (Height  => "12px");

   -------------------------------------------------------------------------
   --  jQueryUI Accordion
   -------------------------------------------------------------------------

   type Accordion_Type is new Gnoga.Gui.View.View_Type with private;
   type Accordion_Access is access all Accordion_Type;
   type Pointer_To_Accordion_Class is access all Accordion_Type'Class;

   overriding
   procedure Create
     (View    : in out Accordion_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach  : in     Boolean := True;
      ID      : in     String  := "");

   procedure Create_Section
     (View : in out Accordion_Type; Heading : String);
   --  Create an accordion section this should be followed up immediately
   --  with the creation of another View_Base_Type'Class with View as the
   --  parent for each section.

   procedure Render_Accordion
     (View           : in out Accordion_Type;
      Allow_Collapse : in     Boolean := False);
   --  Execute after all sections and views have been added

   -------------------------------------------------------------------------
   --  jQueryUI Button
   -------------------------------------------------------------------------

   procedure Make_Button
     (Element    : in out Gnoga.Gui.Element.Element_Type'Class;
      Left_Icon  : in     String  := "";
      Right_Icon : in     String  := "";
      No_Text    : in     Boolean := False);
   --  Turns Element in to a Button. A_Type, Button_Type, etc. are
   --  will be single press buttons. A Check_Box_Types will become a
   --  toggle button.

   procedure Make_Button_Set
     (View : in out Gnoga.Gui.View.View_Base_Type'Class);
   --  Turn a view (no need to call Make_Button on each item) of
   --  Check_Box_Types Boxes or Radio_Button_Types in to a set of
   --  toggles or one per set toggles.

   -------------------------------------------------------------------------
   --  jQueryUI Menus
   -------------------------------------------------------------------------

   procedure Make_Menu
     (List : in out Gnoga.Gui.Element.List.Unordered_List_Type'Class);

   -------------------------------------------------------------------------
   --  jQueryUI Tool Tips
   -------------------------------------------------------------------------

   procedure Turn_On_Tool_Tips
     (Window : in out Gnoga.Gui.Window.Window_Type'Class);

   procedure Add_Tool_Tip
     (Element : in out Gnoga.Gui.Element.Element_Type'Class;
      Tip     : in     String);

private
   type Accordion_Type is new Gnoga.Gui.View.View_Type with null record;
end Gnoga.Gui.Plugin.jQueryUI.Widget;
