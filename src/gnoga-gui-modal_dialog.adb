------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . G U I . M O D A L _ D I A L O G               --
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
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;

package body Gnoga.Gui.Modal_Dialog is

   procedure Create
      (Dialog : in out Dialog_Type;
       Parent : in out Gnoga.Gui.Window.Window_Type'Class;
       ID     : in     String := "")
   is
      use type Gnoga.Gui.Base.Pointer_To_Base_Class;
      Old_View : constant Gnoga.Gui.Base.Pointer_To_Base_Class
         := Parent.Get_View;
   begin

      --  Create the Dialog Background view
      Dialog.Modal_Background.Create
         (Parent => Parent,
          ID     => ID);

      --  Creating a view using a window as a parent sets the view as the
      --  window's main view.  This line sets it back to the original.
      if Old_View /= null then
         Parent.Set_View (Old_View.all);
      end if;

      --  Configure the Modal Background
      Dialog.Modal_Background.Fill_Parent;
      Dialog.Modal_Background.Background_Color ("Grey");
      Dialog.Modal_Background.Opacity (1.0);
      Dialog.Modal_Background.Z_Index (Integer'Last);

      --  Set the default show/hide state
      Dialog.Show (False);

      --  Create the containing view of the dialog
      Dialog.Modal_Frame.Create (Dialog.Modal_Background);
      Dialog.Modal_Frame.Position (Gnoga.Gui.Element.Fixed);

   end Create;

   procedure Create
      (Dialog : in out Dialog_Type;
       Parent : in out Gnoga.Gui.View.View_Base_Type'Class;
       ID     : in     String := "")
   is
   begin
      --  Create the Dialog Background view
      Dialog.Modal_Background.Create
         (Parent => Parent,
          ID     => ID);

      --  Want to place at the top of the parent's DOM tree
      Dialog.Modal_Background.Place_Inside_Top_Of (Parent);

      --  Configure the Modal Background
      Dialog.Modal_Background.Fill_Parent;
      Dialog.Modal_Background.Background_Color ("Grey");
      Dialog.Modal_Background.Opacity (1.0);
      Dialog.Modal_Background.Z_Index (Integer'Last);

      --  Set the default show/hide state
      Dialog.Show (False);

      --  Create the containing view of the dialog
      Dialog.Modal_Frame.Create (Dialog.Modal_Background);
      Dialog.Modal_Frame.Position (Gnoga.Gui.Element.Fixed);

   end Create;

   procedure Create_Main_View
      (Dialog : in out Dialog_Type;
       View   : in out Gnoga.Gui.View.View_Type'Class;
       ID     : in     String := "")
   is
   begin
      View.Create (Dialog.Modal_Frame, ID);

      --  Set a default color so it doesn't inherit
      --  it from the background
      View.Background_Color ("White");

   end Create_Main_View;

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
      Top_Offset : Integer
         := (Dialog.Modal_Background.Height / 2
             - Dialog.Modal_Frame.Height / 2);
      Left_Offset : Integer
         := (Dialog.Modal_Background.Width / 2
             - Dialog.Modal_Frame.Width / 2);

      use type String;
   begin

      --  Bound to top left corner
      if Top_Offset < 0 then
         Top_Offset := 0;
      end if;
      if Left_Offset < 0 then
         Left_Offset := 0;
      end if;
      Dialog.Modal_Frame.Top
         (Dialog.Modal_Background.Offset_From_Top  + Top_Offset);
      Dialog.Modal_Frame.Left
         (Dialog.Modal_Background.Offset_From_Left + Left_Offset);
   end Center;

   procedure Top (Dialog : in out Dialog_Type; Top  : Integer) is
   begin
      Dialog.Modal_Frame.Top (Top);
   end Top;

   procedure Left (Dialog : in out Dialog_Type; Left : Integer) is
   begin
      Dialog.Modal_Frame.Left (Left);
   end Left;

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

   procedure Remove (Dialog : in out Dialog_Type) is
   begin
      Dialog.Modal_Frame.Remove;
      Dialog.Modal_Background.Remove;
   end Remove;

end Gnoga.Gui.Modal_Dialog;
