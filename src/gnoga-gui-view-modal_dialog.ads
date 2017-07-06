------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--           G N O G A . G U I . V I E W . M O D A L _ D I A L O G          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2017 Jeremiah Breeden                  --
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
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------
with Gnoga.Gui.Window;
with Gnoga.Types.Colors;

--  This unit provides a modal dialog type for custom modal dialog designs.
package Gnoga.Gui.View.Modal_Dialog is

   -------------------------------------------------------------------------
   --  Dialog_Type
   -------------------------------------------------------------------------
   --  Dialog_Type is the class encapsulating the a modal frame and a
   --  modal background.
   type Dialog_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Dialog_Access is access all Dialog_Type;
   type Pointer_To_Dialog_Class is access all Dialog_Type'Class;

   procedure Create
      (Object : in out Dialog_Type;
       Parent : in out Gnoga.Gui.Window.Window_Type'Class;
       ID     :        String := "");
   procedure Create
      (Object : in out Dialog_Type;
       Parent : in out Gnoga.Gui.View.View_Base_Type'Class;
       ID     :        String := "");
   --  Creates a modal dialog using either a Window_Type or a View_Base_Type
   --  as a parent.

   procedure Show
      (Dialog : in out Dialog_Type;
       Show   :        Boolean := True;
       Center :        Boolean := True);
   --  Shows or hides a dialog

   procedure Center (Dialog : in out Dialog_Type);
   --  Center a dialog within its parent window or view

   procedure Modal_Background_Color
      (Dialog : in out Dialog_Type;
       Color  :        String);
   --  Change the background color of the modal dialog's fill view

   procedure Modal_Background_Color
      (Dialog : in out Dialog_Type;
       Color  :        Gnoga.Types.Colors.Color_Enumeration);
   --  Change the background color of the modal dialog's fill view

   ----------------------------------------------------------------------------
   --  Gnoga.Gui.View.View_Base_Type overrides
   ----------------------------------------------------------------------------

   overriding
   procedure Fill_Parent (View : in out Dialog_Type);
   --  Cause View to expand its height and width to fill its parent's client
   --  area. View's parent must have Position set to Absolute, Fixed or
   --  Relative for Fill_Parent to work properly.
   --  Note:
   --     Position will be modified for View to either Absolute or Relative
   --     if Absolute positioning fails (i.e. on IE)

   ----------------------------------------------------------------------------
   --  Gnoga.Gui.Element.Element_Type overrides
   ----------------------------------------------------------------------------

   overriding
   procedure Auto_Place (Element : in out Dialog_Type; Value : Boolean);
   overriding
   function Auto_Place (Element : Dialog_Type) return Boolean;
   --  Elements by default are created outside the DOM and therefore not
   --  visible. If Auto_Place is set to false _before_ Create is called on
   --  an Element, View's will not place the Element in to the DOM as is
   --  the View's default behavior. Custom widgets that have child widgets
   --  should be designed to respect this property. Auto_Place if set to
   --  False will also prevent Auto_Set_View if Element's Parent is a Window

   overriding
   procedure Place_Inside_Top_Of
      (Element : in out Dialog_Type;
       Target  : in out Gnoga.Gui.Element.Element_Type'Class);
   overriding
   procedure Place_Inside_Bottom_Of
      (Element : in out Dialog_Type;
       Target  : in out Gnoga.Gui.Element.Element_Type'Class);
   overriding
   procedure Place_Before
      (Element : in out Dialog_Type;
       Target  : in out Gnoga.Gui.Element.Element_Type'Class);
   overriding
   procedure Place_After
      (Element : in out Dialog_Type;
       Target  : in out Gnoga.Gui.Element.Element_Type'Class);
   --  DOM placement method

   overriding
   procedure Remove (Element : in out Dialog_Type);
   --  Removes dialog from the DOM, if the ID_Type is DOM_ID, the ID
   --  will be changed to a unique Gnoga_ID before removal.

   ----------------------------------------------------------------------------
   --  Gnoga.Gui.Base.Base_Type overrides
   ----------------------------------------------------------------------------

   overriding
   function Parent
      (Object : Dialog_Type)
       return Gnoga.Gui.Base.Pointer_To_Base_Class;
   overriding
   procedure Parent
      (Object : in out Dialog_Type;
       Value  : in out Gnoga.Gui.Base.Base_Type'Class);
   overriding
   procedure Parent
      (Object : in out Dialog_Type;
       Value  :        Gnoga.Gui.Base.Pointer_To_Base_Class);
   --  Parent of Object. Setting/changing the parent will fire the
   --  On_Parent_Added event on the Parent object and if changing the parent
   --  On_Parent_Removed event on the old Parent
   --
   --  Setting the parent Object does not change the position Object may have
   --  in the DOM by default. That should be done using Element_Type.Place_*

   ----------------------------------------------------------------------------
   --  Unsupported functionality.  Do not use.
   ----------------------------------------------------------------------------

   Use_Error : exception;
   --  Called when an unsupported function is used

   overriding
   procedure Create_From_HTML
      (Element : in out Dialog_Type;
       Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
       HTML    : in     String;
       ID      : in     String := "");
   overriding
   procedure Create_XML_Element
      (Element      : in out Dialog_Type;
       Parent       : in out Gnoga.Gui.Base.Base_Type'Class;
       Namespace    : in     String;
       Element_Type : in     String;
       ID           : in     String := "");
   overriding
   procedure Create_With_Script
     (Object        : in out Dialog_Type;
      Connection_ID : in     Gnoga.Types.Connection_ID;
      ID            : in     String;
      Script        : in     String;
      ID_Type       : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.DOM_ID);
   overriding
   procedure Attach_Using_Parent
     (Object   : in out Dialog_Type;
      Parent   : in     Gnoga.Gui.Base.Base_Type'Class;
      ID       : in     String;
      ID_Type  : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.DOM_ID);
   overriding
   procedure Attach
     (Object        : in out Dialog_Type;
      Connection_ID : in     Gnoga.Types.Connection_ID;
      ID            : in     String;
      ID_Type       : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.DOM_ID);
   --  Unsupported.  Raises Use_Error if called

private

   type Dialog_Type is new Gnoga.Gui.View.View_Type with record
      Modal_Background : Gnoga.Gui.View.View_Type;
   end record;

end Gnoga.Gui.View.Modal_Dialog;
