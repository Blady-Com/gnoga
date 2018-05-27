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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

--  General binding to Boot Strap

with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Table;

package Gnoga.Gui.Plugin.Boot_Strap is

   -------------------------------------------------------------------------
   --  Boot Strap Plug In
   -------------------------------------------------------------------------
   --  http://getbootstrap.com/
   --  Bootstrap uses CSS classes to enhance layout of web pages.
   --  These "bindings" are just to assist in using Boot strap, however
   --  understanding its concepts and adding and removing classes as
   --  needed will give full access to boot strap's capabilities.

   procedure Load_Boot_Strap
     (Window : in out Gnoga.Gui.Window.Window_Type'Class);
   --  Load Boot Strap CSS and Scripting code in to Window
   --  This procedure loads them from MaxCDN
   --  You can also modify boot.html to include them directly
   --  See boot_bootstrap3.html for an example.

   --  Boot Strap Containers  --
   --
   --  Each top level view must be made in to a container or fluid container
   --  There can be multiple containers, for example a page could have:
   --  1. A Fluid container with the menu bar
   --  2. A container with a jumbo tron
   --  3. A container with multi column content

   type Container_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Container_Access is access all Container_Type;
   type Pointer_To_Container_Class is access all Container_Type'Class;

   procedure Create
     (Container : in out Container_Type;
      Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
      ID        : in     String  := "");

   type Fluid_Container_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Fluid_Container_Access is access all Fluid_Container_Type;
   type Pointer_To_Fluid_Container_Class is
     access all Fluid_Container_Type'Class;

   procedure Create
     (Container : in out Fluid_Container_Type;
      Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
      ID        : in     String  := "");

   procedure Make_Container
     (View : in out Gnoga.Gui.View.View_Base_Type'Class);
   --  Turns any existing View in to a container for Boot Strap

   procedure Make_Fluid_Container
     (View : in out Gnoga.Gui.View.View_Base_Type'Class);
   --  Make_Fluid_Container causes the View to span the entire parent Window
   --  width

   --  The Bootstrap Grid --
   --
   --  Bootstrap uses a responsive 12 column grid, with each View_Type taking
   --  from 1 to 12 columns of each row in the grid.
   --  This allows for the screen width gracefully reduce or expand placing
   --  some columns in a row below each other as needed to fit the width.
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
                          Device : in     Device_Type := Small);

   type Jumbotron_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Jumbotron_Access is access all Jumbotron_Type;
   type Pointer_To_Jumbotron_Class is access all Jumbotron_Type'Class;

   procedure Create
     (Jumbotron : in out Jumbotron_Type;
      Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
      Content   : in     String  := "";
      ID        : in     String  := "");
   --  Create a Jumbotron area with option Content
   --  A jumbotron is a huge grey background area and enlarged fonts to call
   --  attention to the contents.
   --  Jumbotron is often placed in its own boot strap container.

   --  For buttons consider adding the "btn" class and other available "btn-*"
   --  classes to add the Boot Strap styling to buttons, a elements, and input
   --  buttons. See:
   --     http://www.w3schools.com/bootstrap/bootstrap_ref_css_buttons.asp

   --  Some additional classes of benefit:
   --     http://www.w3schools.com/bootstrap/bootstrap_ref_css_text.asp
   --     http://www.w3schools.com/bootstrap/bootstrap_ref_css_helpers.asp
   --     http://www.w3schools.com/bootstrap/bootstrap_ref_css_images.asp

   procedure Put_Glyphicon
     (View : in out Gnoga.Gui.View.View_Base_Type'Class;
      Icon : in     String);
   --  There are 200 glyphs included with Boot Strap. You can use this to
   --  place one in to a View, for example Put_Glyphicon (My_View, "envelope");
   --  The Icon name is with out the glyphicon- prefix. See:
   --     http://www.w3schools.com/bootstrap/bootstrap_ref_comp_glyphs.asp

   type Table_Type is new Gnoga.Gui.Element.Element_Type with
      record
         Table : Gnoga.Gui.Element.Table.Table_Type;
      end record;
   type Table_Access is access all Table_Type;
   type Pointer_To_Table_Class is access all Table_Type'Class;

   -------------------------------------------------------------------------
   --  Table_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Element   : in out Table_Type;
      Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
      Striped   : in     Boolean := True;
      Bordered  : in     Boolean := True;
      Condensed : in     Boolean := True;
      ID        : in     String  := "");
   --  Creates a responsive table width of window is less than 768px
   --  Access the original table with Element.Table

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
     new Gnoga.Gui.Element.Element_Type with
      record
         Box : Gnoga.Gui.Element.Form.Check_Box_Type;
      end record;
   type Check_Box_Access is access all Check_Box_Type;
   type Pointer_To_Check_Box_Class is access all Check_Box_Type'Class;

   procedure Create
     (Element : in out Check_Box_Type;
      Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Checked : in     Boolean := False;
      Value   : in     String := "";
      Name    : in     String := "";
      ID      : in     String := "");
   --  Access the original checkbox directly using Element.Box

   procedure Disabled (Element : in out Check_Box_Type;
                       Value   : in     Boolean := True);
   --  Will toggle the "disabled" class and the Element.Box disabled

   type Radio_Button_Type is
     new Gnoga.Gui.Element.Element_Type with
      record
         Radio : Gnoga.Gui.Element.Form.Radio_Button_Type;
      end record;
   type Radio_Button_Access is access all Radio_Button_Type;
   type Pointer_To_Radio_Button_Class is access all Radio_Button_Type'Class;

   procedure Create
     (Element : in out Radio_Button_Type;
      Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Checked : in     Boolean := False;
      Value   : in     String := "";
      Name    : in     String := "";
      ID      : in     String := "");
   --  Access the original radio button directly using Element.Radio

   procedure Disabled (Element : in out Radio_Button_Type;
                       Value   : in     Boolean := True);
   --  Will toggle the "disabled" class and the Element.Radio disabled
private
   type Container_Type is new Gnoga.Gui.View.View_Base_Type with null record;
   type Fluid_Container_Type is
     new Gnoga.Gui.View.View_Base_Type with null record;
   type Row_Type is new Gnoga.Gui.View.View_Base_Type with null record;
   type Jumbotron_Type is new Gnoga.Gui.View.View_Base_Type with null record;
   type Form_Group_Type is new Gnoga.Gui.View.View_Base_Type with null record;
end Gnoga.Gui.Plugin.Boot_Strap;
