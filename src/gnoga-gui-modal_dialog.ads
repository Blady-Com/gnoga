------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . G U I . M O D A L _ D I A L O G               --
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
with Gnoga.Gui.View;

--  This unit provides a modal dialog type for custom modal dialog designs.
package Gnoga.Gui.Modal_Dialog is

   -------------------------------------------------------------------------
   --  Dialog_Type
   -------------------------------------------------------------------------
   --  Dialog_Type is the class encapsulating the a modal frame and a
   --  modal background.
   type Dialog_Type is tagged limited private;
   type Dialog_Access is access all Dialog_Type;
   type Pointer_To_Dialog_Class is access all Dialog_Type'Class;

   procedure Create
      (Dialog : in out Dialog_Type;
       Parent : in out Gnoga.Gui.Window.Window_Type'Class;
       ID     : in     String := "");
   --  Create a modal dialog using the window as a parent

   procedure Create
      (Dialog : in out Dialog_Type;
       Parent : in out Gnoga.Gui.View.View_Base_Type'Class;
       ID     : in     String := "");
   --  Create a modal dialog using a view as the parent

   procedure Create_Main_View
      (Dialog : in out Dialog_Type;
       View   : in out Gnoga.Gui.View.View_Type'Class;
       ID     : in     String := "");
   --  Attach and create a user created view to the dialog.  Call this
   --  in place of the Create procedure for the view supplied.

   procedure Show
      (Dialog : in out Dialog_Type;
       Show   :        Boolean := True;
       Center :        Boolean := True);
   --  Shows or hides a dialog

   procedure Center (Dialog : in out Dialog_Type);
   --  Center a dialog within its parent window or view

   procedure Top (Dialog : in out Dialog_Type; Top  : Integer);
   --  Set the "top" position of the dialog.

   procedure Left (Dialog : in out Dialog_Type; Left : Integer);
   --  Set the "left" position of the dialog.

   procedure Modal_Background_Color
      (Dialog : in out Dialog_Type;
       Color  :        String);
   --  Change the background color of the modal dialog's fill view

   procedure Modal_Background_Color
      (Dialog : in out Dialog_Type;
       Color  :        Gnoga.Types.Colors.Color_Enumeration);
   --  Change the background color of the modal dialog's fill view

   procedure Remove (Dialog : in out Dialog_Type);
   --  Removes dialog from the DOM, if the ID_Type is DOM_ID, the ID
   --  will be changed to a unique Gnoga_ID before removal.

private

   type Dialog_Type is tagged limited record
      Modal_Background : Gnoga.Gui.View.View_Type;
      Modal_Frame      : Gnoga.Gui.View.View_Type;
   end record;

end Gnoga.Gui.Modal_Dialog;
