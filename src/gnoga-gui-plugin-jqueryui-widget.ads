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

   --  jQueryUI Autocomplete is not bound, use instead
   --     Gnoga.Gui.Element.Form.Data_List_Type

   --  jQueryUI Datapicker is not bound, use instead
   --     Gnoga.Gui.Element.Form.Date_Type and its related types

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
   --  jQueryUI Dialog
   -------------------------------------------------------------------------

   type Dialog_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Dialog_Access is access all Dialog_Type;
   type Pointer_To_Dialog_Class is access all Dialog_Type'Class;

   procedure Create
     (Dialog          : in out Dialog_Type;
      Parent          : in out Gnoga.Gui.Base.Base_Type'Class;
      Title           : in     String;
      Content         : in     String  := "";
      Height          : in     Natural := 0;
      Width           : in     Natural := 0;
      Position_My     : in     String  := "center";
      Position_At     : in     String  := "center";
      Resizable       : in     Boolean := False;
      Minimum_Height  : in     Natural := 150;
      Minimum_Width   : in     Natural := 150;
      Maximum_Height  : in     Natural := 0;
      Maximum_Width   : in     Natural := 0;
      Modal           : in     Boolean := True;
      Close_On_Escape : in     Boolean := True;
      Draggable       : in     Boolean := True;
      ID              : in     String  := "");
   --  Set Autofocus(True) and the control to make it first focus when Dialog
   --  is opened. Position is relative using the Dialog center relative to
   --  the window. If Height or Width are set to 0 will autosize to contents.
   --  If Maximum_Height or Maximum_Width = 0 then no maximum will be set.
   --  Content can be used to set some initial text or HTML in to the Dialog.

   procedure Open (Dialog : in out Dialog_Type);

   procedure Close (Dialog : in out Dialog_Type);

   function Is_Open (Dialog : in out Dialog_Type) return Boolean;

   procedure On_Open_Handler (Dialog  : in out Dialog_Type;
                               Handler : in     Gnoga.Gui.Base.Action_Event);
   procedure Fire_On_Open (Dialog : in out Dialog_Type);

   procedure On_Close_Handler (Dialog  : in out Dialog_Type;
                               Handler : in     Gnoga.Gui.Base.Action_Event);
   procedure Fire_On_Close (Dialog : in out Dialog_Type);

   overriding
   procedure On_Message (Object  : in out Dialog_Type;
                         Event   : in     String;
                         Message : in     String);

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

   type Dialog_Type is new Gnoga.Gui.View.View_Base_Type with
      record
         On_Open_Event  : Gnoga.Gui.Base.Action_Event := null;
         On_Close_Event : Gnoga.Gui.Base.Action_Event := null;
      end record;
end Gnoga.Gui.Plugin.jQueryUI.Widget;
