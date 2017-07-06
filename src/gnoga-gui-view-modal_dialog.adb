------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--           G N O G A . G U I . V I E W . M O D A L _ D I A L O G          --
--                                                                          --
--                                 B o d y                                  --
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

package body Gnoga.Gui.View.Modal_Dialog is

   procedure Create
      (Object : in out Dialog_Type;
       Parent : in out Gnoga.Gui.Window.Window_Type'Class;
       ID     :        String := "")
   is
      use type Gnoga.Gui.Base.Pointer_To_Base_Class;
      Old_View : constant Gnoga.Gui.Base.Pointer_To_Base_Class
         := Parent.Get_View;
   begin

      --  Create the Dialog Background view
      Object.Modal_Background.Create
         (Parent => Parent,
          ID     => ID);

      --  Creating a view using a window as a parent sets the view as the
      --  window's main view.  This line sets it back to the original.
      if Old_View /= null then
         Parent.Set_View (Old_View.all);
      end if;

      --  Configure the Modal Background
      Object.Modal_Background.Fill_Parent;
      Object.Modal_Background.Background_Color ("Grey");
      Object.Modal_Background.Opacity (1.0);
      Object.Modal_Background.Z_Index (Integer'Last);

      --  Set the default show/hide state
      Object.Show (False);

      --  Create the containing view of the dialog
      Gnoga.Gui.View.View_Type (Object).Create (Object.Modal_Background);
      Object.Position (Gnoga.Gui.Element.Fixed);
      Object.Background_Color ("White");

   end Create;

   procedure Create
      (Object : in out Dialog_Type;
       Parent : in out Gnoga.Gui.View.View_Base_Type'Class;
       ID     :        String := "")
   is
   begin
      --  Create the Dialog Background view
      Object.Modal_Background.Create
         (Parent => Parent,
          ID     => ID);

      --  Want to place at the top of the parent's DOM tree
      Object.Modal_Background.Place_Inside_Top_Of (Parent);

      --  Configure the Modal Background
      Object.Modal_Background.Fill_Parent;
      Object.Modal_Background.Background_Color ("Grey");
      Object.Modal_Background.Opacity (1.0);
      Object.Modal_Background.Z_Index (Integer'Last);

      --  Set the default show/hide state
      Object.Show (False);

      --  Create the containing view of the dialog
      Gnoga.Gui.View.View_Type (Object).Create (Object.Modal_Background);
      Object.Position (Gnoga.Gui.Element.Fixed);
      Object.Background_Color ("White");

   end Create;

   procedure Show
      (Dialog : in out Dialog_Type;
       Show   :        Boolean := True;
       Center :        Boolean := True)
   is
   begin
      if Show then
         Dialog.Modal_Background.Hidden (False);
         if Center then
            Dialog.Center;
         end if;
         Dialog.Modal_Background.Visible;
      else
         Dialog.Modal_Background.Visible (False);
         Dialog.Modal_Background.Hidden;
      end if;

   end Show;

   procedure Center (Dialog : in out Dialog_Type) is
      Top_Offset  : Integer;
      Left_Offset : Integer;

      use type String;
   begin

      Dialog.Position (Gnoga.Gui.Element.Fixed);
      Top_Offset
         := (Dialog.Modal_Background.Height / 2
             - Dialog.Height / 2);
      Left_Offset
         := (Dialog.Modal_Background.Width / 2
             - Dialog.Width / 2);

      --  Bound to top left corner
      if Top_Offset < 0 then
         Top_Offset := 0;
      end if;
      if Left_Offset < 0 then
         Left_Offset := 0;
      end if;

      Dialog.Top
         (Dialog.Modal_Background.Offset_From_Top  + Top_Offset);
      Dialog.Left
         (Dialog.Modal_Background.Offset_From_Left + Left_Offset);
   end Center;

   procedure Modal_Background_Color
      (Dialog : in out Dialog_Type;
       Color  :        String)
   is
   begin
      Dialog.Modal_Background.Background_Color (Color);
   end Modal_Background_Color;

   procedure Modal_Background_Color
      (Dialog : in out Dialog_Type;
       Color  :        Gnoga.Types.Colors.Color_Enumeration)
   is
   begin
      Dialog.Modal_Background.Background_Color (Color);
   end Modal_Background_Color;

   ---------------------------------------------------------------------------
   --  Gnoga.Gui.View.View_Base_Type overrides
   ---------------------------------------------------------------------------

   overriding
   procedure Fill_Parent (View : in out Dialog_Type) is
   begin
      View.Modal_Background.Fill_Parent;
   end Fill_Parent;

   ----------------------------------------------------------------------------
   --  Gnoga.Gui.Element.Element_Type overrides
   ----------------------------------------------------------------------------

   overriding
   procedure Auto_Place (Element : in out Dialog_Type; Value : Boolean) is
   begin
      Element.Modal_Background.Auto_Place (Value);
   end Auto_Place;

   overriding
   function Auto_Place (Element : Dialog_Type) return Boolean is
   begin
      return Element.Modal_Background.Auto_Place;
   end Auto_Place;

   overriding
   procedure Place_Inside_Top_Of
      (Element : in out Dialog_Type;
       Target  : in out Gnoga.Gui.Element.Element_Type'Class)
   is
   begin
      Element.Modal_Background.Place_Inside_Top_Of (Target);
   end Place_Inside_Top_Of;

   overriding
   procedure Place_Inside_Bottom_Of
      (Element : in out Dialog_Type;
       Target  : in out Gnoga.Gui.Element.Element_Type'Class)
   is
   begin
      Element.Modal_Background.Place_Inside_Bottom_Of (Target);
   end Place_Inside_Bottom_Of;

   overriding
   procedure Place_Before
      (Element : in out Dialog_Type;
       Target  : in out Gnoga.Gui.Element.Element_Type'Class)
   is
   begin
      Element.Modal_Background.Place_Before (Target);
   end Place_Before;

   overriding
   procedure Place_After
      (Element : in out Dialog_Type;
       Target  : in out Gnoga.Gui.Element.Element_Type'Class)
   is
   begin
      Element.Modal_Background.Place_After (Target);
   end Place_After;

   overriding
   procedure Remove (Element : in out Dialog_Type) is
   begin
      Gnoga.Gui.View.View_Type (Element).Remove;
      Element.Modal_Background.Remove;
   end Remove;

   ----------------------------------------------------------------------------
   --  Gnoga.Gui.Base.Base_Type overrides
   ----------------------------------------------------------------------------

   overriding
   function Parent
      (Object : Dialog_Type)
       return Gnoga.Gui.Base.Pointer_To_Base_Class
   is
   begin
      return Object.Modal_Background.Parent;
   end Parent;

   overriding
   procedure Parent
      (Object : in out Dialog_Type;
       Value  : in out Gnoga.Gui.Base.Base_Type'Class)
   is
   begin
      Object.Modal_Background.Parent (Value);
   end Parent;

   overriding
   procedure Parent
      (Object : in out Dialog_Type;
       Value  :        Gnoga.Gui.Base.Pointer_To_Base_Class)
   is
   begin
      Object.Modal_Background.Parent (Value);
   end Parent;

   ----------------------------------------------------------------------------
   --  Unsupported functionality.  Do not use.
   ----------------------------------------------------------------------------

   overriding
   procedure Create_From_HTML
      (Element : in out Dialog_Type;
       Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
       HTML    : in     String;
       ID      : in     String := "")
   is
   begin
      raise Use_Error;
   end Create_From_HTML;

   overriding
   procedure Create_XML_Element
      (Element      : in out Dialog_Type;
       Parent       : in out Gnoga.Gui.Base.Base_Type'Class;
       Namespace    : in     String;
       Element_Type : in     String;
       ID           : in     String := "")
   is
   begin
      raise Use_Error;
   end Create_XML_Element;

   overriding
   procedure Create_With_Script
     (Object        : in out Dialog_Type;
      Connection_ID : in     Gnoga.Types.Connection_ID;
      ID            : in     String;
      Script        : in     String;
      ID_Type       : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.DOM_ID)
   is
   begin
      raise Use_Error;
   end Create_With_Script;

   overriding
   procedure Attach_Using_Parent
     (Object   : in out Dialog_Type;
      Parent   : in     Gnoga.Gui.Base.Base_Type'Class;
      ID       : in     String;
      ID_Type  : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.DOM_ID)
   is
   begin
      raise Use_Error;
   end Attach_Using_Parent;

   overriding
   procedure Attach
     (Object        : in out Dialog_Type;
      Connection_ID : in     Gnoga.Types.Connection_ID;
      ID            : in     String;
      ID_Type       : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.DOM_ID)
   is
   begin
      raise Use_Error;
   end Attach;

end Gnoga.Gui.View.Modal_Dialog;
