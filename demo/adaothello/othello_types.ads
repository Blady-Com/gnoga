--------------------------------------------------------------------------------
--|   Othello - The Classic Othello Game written in Ada
--|
--|   Copyright (C) 2001 Adrian Hoe (byhoe@users.sourceforge.net)
--|
--| Othello is free software; you can redistribute it and/or modify
--| it under the terms of the GNU General Public License as published
--| by the Free Software Foundation; either version 2 of the License,
--| or (at your option) any later version.
--|
--| This software is distributed in the hope that it will be useful,
--| and entertaining but WITHOUT ANY WARRANTY; without even the
--| implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--| PURPOSE.  See the GNU General Public License for more details.
--|
--| You should have received a copy of the GNU General Public
--| License along with this software; if not, write to the
--| Free Software Foundation, Inc., 59 Temple Place - Suite 330,
--| Boston, MA 02111-1307, USA.
--|
--| Filename         : $Source: /othello_types.ads,v $
--| Author           : Adrian Hoe (byhoe)
--| Created On       : 2001/11/15
--| Last Modified By : $Author: JHB $
--| Last Modified On : $Date: 2016/03/06 09:18:25 $
--| Status           : $State: Exp $
--|
--------------------------------------------------------------------------------

-- FILENAME change from board_pkg.ads to othello_types.ads

with Gnoga.Gui.View;
with Gnoga.Gui.Window;
with Gnoga.Types;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.View.Grid;

package Othello_Types is

   subtype Valid_Row is Positive range 1 .. 8; -- Size of the mine field
   subtype Valid_Column is Positive range 1 .. 8;

   -- Even though this is normally a button, I am implementing this as a
   -- View_Type (which has an On_Click event as it is easier to center and
   -- size than a button
   type Cell_Button is new Gnoga.Gui.View.View_Type with record
      Row    : Valid_Row    := Valid_Row'First;
      Column : Valid_Column := Valid_Column'First;
   end record;

   type Cell_Status is (Empty, Blue, Red);
   type Cell_Record is record
      Cell         : Cell_Status := Empty;
      Button       : Cell_Button;
      Pixmap       : Gnoga.Gui.View.View_Type; -- using a view type instead
      Flag         : Boolean     := True;
      Button_Setup : Boolean     := False;
   end record;

   -- Removed top level menus.  Implementing the menu as a row of buttons
   type Othello_Record is tagged limited record
      Vbox1          : Gnoga.Gui.View.View_Type;
      Menubar1       : Gnoga.Gui.View.View_Type;
      File1          : Gnoga.Gui.Element.Common.Button_Type;
      New_Game1      : Gnoga.Gui.Element.Common.Button_Type;
      Pass1          : Gnoga.Gui.Element.Common.Button_Type;
      Exit1          : Gnoga.Gui.Element.Common.Button_Type;
      Help1          : Gnoga.Gui.Element.Common.Button_Type;
      About1         : Gnoga.Gui.Element.Common.Button_Type;
      Table1         : Gnoga.Gui.View.Grid.Grid_View_Type;
      Hbox1          : Gnoga.Gui.View.View_Type;
      Main_Statusbar : Gnoga.Gui.View.View_Type;
      Blue_Statusbar : Gnoga.Gui.View.View_Type;
      Red_Statusbar  : Gnoga.Gui.View.View_Type;
   end record;

   type Board_Matrix is array (Valid_Row, Valid_Column) of Cell_Record;

   subtype Bead_Color is Cell_Status;

   -- This is what each user's data looks like
   type App_Data_Type is new Gnoga.Types.Connection_Data_Type with record
      Window      : Gnoga.Gui.Window.Pointer_To_Window_Class;
      Window_View : Gnoga.Gui.View.View_Type;
      Main_View   : Gnoga.Gui.View.View_Type;

      Othello : Othello_Record;

      Playing_Board : Board_Matrix;
      Whose_Move    : Bead_Color := Blue;
      Game_Over     : Boolean    := False;
   end record;

   type App_Data_Access is access all App_Data_Type;

   End_Message : constant String := "Ada Othello ended.";
end Othello_Types;
