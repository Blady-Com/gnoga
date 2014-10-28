------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--           G N O G A . G U I . P L U G I N S . B O O T _ S T R A P        --
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

--  General binding to Boot Strap

with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Form;

package Gnoga.Gui.Plugin.Boot_Strap is

   -------------------------------------------------------------------------
   --  Boot Strap Plug In
   -------------------------------------------------------------------------
   --  http://getbootstrap.com/

   procedure Load_Boot_Strap
     (Window : in out Gnoga.Gui.Window.Window_Type'Class);
   --  Load Boot Strap CSS and Scripting code in to Window
   --  This proceure loads them from MaxCDN
   --  You can also modify boot.html to include them directly

   --  A nice summary of default typography and available CSS Classes is at:
   --     http://www.w3schools.com/bootstrap/bootstrap_typography.asp

   procedure Make_Container
     (View : in out Gnoga.Gui.View.View_Base_Type'Class);
   procedure Make_Fluid_Container
     (View : in out Gnoga.Gui.View.View_Base_Type'Class);
   --  The main view in the window must be made into a Boot Strap Container.
   --
   --  Make_Fluid_Container causes the View to fill the entire parent Window
   --
   --  Note: In Gnoga if View is attached as the main view of the window there
   --  is no difference between the two.

   --  The Bootstrap Grid
   --
   --  Bootstrap uses a responsive 12 column grid, with each View_Type taking
   --  from 1 to 12 columns of each row in the grid.
   --  This allows for the screen width gracefully reduce or expand placing
   --  some columnd in a row below each other as needed to fit the width.
   --
   --  Create a Row_Type then Create your Views with Row_Type as parent to
   --  to add them. Use Set_Columns immediately after create to specify
   --  how many columns your View should take of the 12. The total number
   --  of Views you add must Set_Columns so that their sum is 12
   --  Set_Columns can be called multiple times for creating alternate
   --  Column layouts based on device screen size.

   type Row_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Row_Access is access all Row_Type;
   type Pointer_To_Row_Class is access all Row_Type'Class;

   procedure Create
     (Row    : in out Row_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String  := "");

   type Device_Type is (Extra_Small, Small, Medium, Large);

   procedure Set_Columns (View   : in out Gnoga.Gui.View.View_Base_Type'Class;
                          Number : in     Natural;
                          Device : in     Device_Type := Extra_Small);

   type Jumbotron_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Jumbotron_Access is access all Jumbotron_Type;
   type Pointer_To_Jumbotron_Class is access all Jumbotron_Type'Class;

   procedure Create
     (Jumbotron : in out Row_Type;
      Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
      Content   : in     String  := "";
      ID        : in     String  := "");
   --  Create a Jumbotron area with option Content
   --  A jumbotron is a huge grey background area and enlarged fonts to call
   --  attention to the contents.

   type Form_Group_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Form_Group_Access is access all Form_Group_Type;
   type Pointer_To_Form_Group_Class is access all Form_Group_Type'Class;

   procedure Create
     (Form_Group : in out Form_Group_Type;
      Parent     : in out Gnoga.Gui.Base.Base_Type'Class;
      ID         : in     String  := "");
   --  Form Groups are used to create proper spacing between form label
   --  and target. Each label and target should be wrapped in one form group.

   procedure Make_Boot_Strap_Form_Item
     (Element : in out Gnoga.Gui.Element.Form.Form_Element_Type'Class);
   --  Call for all form items of type:
   --     <input>, <textarea>, and <select>

   procedure Add_Help_Block
     (Element : in out Gnoga.Gui.Element.Form.Form_Element_Type'Class;
      Text    : in     String;
      ID      : in     String := "");
   --  Adds a help block that will display under Element

   type Check_Box_Type is
     new Gnoga.Gui.Element.Form.Check_Box_Type with private;
   type Check_Box_Access is access all Check_Box_Type;
   type Pointer_To_Check_Box_Class is access all Check_Box_Type'Class;

   overriding
   procedure Create
     (Element : in out Check_Box_Type;
      Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Checked : in     Boolean := False;
      Value   : in     String := "";
      Name    : in     String := "";
      ID      : in     String := "");

   overriding
   procedure Disabled (Element : in out Check_Box_Type;
                       Value   : in     Boolean := True);
   --  Will toggle the "disabled" class on the checkbox style
   --  div wrapper used by Boot Strap

   type Radio_Button_Type is
     new Gnoga.Gui.Element.Form.Radio_Button_Type with private;
   type Radio_Button_Access is access all Radio_Button_Type;
   type Pointer_To_Radio_Button_Class is access all Radio_Button_Type'Class;

   overriding
   procedure Create
     (Element : in out Radio_Button_Type;
      Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Checked : in     Boolean := False;
      Value   : in     String := "";
      Name    : in     String := "";
      ID      : in     String := "");

   overriding
   procedure Disabled (Element : in out Radio_Button_Type;
                       Value   : in     Boolean := True);
   --  Will toggle the "disabled" class on the checkbox style
   --  div wrapper used by Boot Strap
private
   type Row_Type is new Gnoga.Gui.View.View_Base_Type with null record;
   type Jumbotron_Type is new Gnoga.Gui.View.View_Base_Type with null record;

   type Form_Group_Type is new Gnoga.Gui.View.View_Base_Type with null record;

   type Check_Box_Type is
     new Gnoga.Gui.Element.Form.Check_Box_Type with
      record
         Wrapper : Gnoga.Gui.Element.Element_Type;
      end record;

   type Radio_Button_Type is
     new Gnoga.Gui.Element.Form.Radio_Button_Type with
      record
         Wrapper : Gnoga.Gui.Element.Element_Type;
      end record;

end Gnoga.Gui.Plugin.Boot_Strap;
