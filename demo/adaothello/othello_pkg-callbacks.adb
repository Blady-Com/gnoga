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
--| Filename         : $Source: /othello_pkg-callbacks.adb,v $
--| Author           : Adrian Hoe (byhoe)
--| Created On       : 2001/11/15
--| Last Modified By : $Author: JHB $
--| Last Modified On : $Date: 2016/03/06 09:18:25 $
--| Status           : $State: Exp $
--|
--------------------------------------------------------------------------------

with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Element;
with Gnoga.Types.Colors;

package body Othello_Pkg.Callbacks is

   -----------------------
   -- On_File1_Activate --
   -----------------------

   procedure On_File1_Activate (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
      null;
   end On_File1_Activate;

   ---------------------------
   -- On_New_Game1_Activate --
   ---------------------------

   procedure On_New_Game1_Activate (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App_Data : constant Othello_Types.App_Data_Access := Othello_Types.App_Data_Access (Object.Connection_Data);
   begin
      New_Game
        (Othello   => App_Data.Othello, Playing_Board => App_Data.Playing_Board, Whose_Move => App_Data.Whose_Move,
         Game_Over => App_Data.Game_Over);
   end On_New_Game1_Activate;

   -----------------------
   -- On_Pass1_Activate --
   -----------------------

   procedure On_Pass1_Activate (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App_Data : constant Othello_Types.App_Data_Access := Othello_Types.App_Data_Access (Object.Connection_Data);
   begin
      Pass
        (Othello   => App_Data.Othello, Playing_Board => App_Data.Playing_Board, Whose_Move => App_Data.Whose_Move,
         Game_Over => App_Data.Game_Over);
   end On_Pass1_Activate;

   -----------------------
   -- On_Exit1_Activate --
   -----------------------

   procedure On_Exit1_Activate (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App_Data : constant Othello_Types.App_Data_Access := Othello_Types.App_Data_Access (Object.Connection_Data);
   begin
--        Gnoga.Application.Multi_Connect.End_Application;

      App_Data.Main_View.Remove;
      App_Data.Window_View.Put_Line (Message => Othello_Types.End_Message);
      App_Data.Window.Close;
      App_Data.Window.Close_Connection;
   end On_Exit1_Activate;

   -----------------------
   -- On_Help1_Activate --
   -----------------------

   procedure On_Help1_Activate (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
      null;
   end On_Help1_Activate;

   ------------------------
   -- On_About1_Activate --
   ------------------------

   procedure On_About1_Activate (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
      null;
   end On_About1_Activate;

   procedure Make_Move (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Button   : Othello_Types.Cell_Button renames Othello_Types.Cell_Button (Object);
      Row      : constant Othello_Types.Valid_Row       := Button.Row;
      Column   : constant Othello_Types.Valid_Column    := Button.Column;
      App_Data : constant Othello_Types.App_Data_Access := Othello_Types.App_Data_Access (Object.Connection_Data);
   begin
      Make_Move
        (Othello   => App_Data.Othello, Playing_Board => App_Data.Playing_Board, Whose_Move => App_Data.Whose_Move,
         Game_Over => App_Data.Game_Over, Row => Row, Column => Column);
   end Make_Move;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      -- Create memory for variable data for this connection
      App : constant Othello_Types.App_Data_Access := new Othello_Types.App_Data_Type;
   begin
      -- Set the variable data for this connection
      Main_Window.Connection_Data (App);
      App.Window := Main_Window'Unchecked_Access;

      -- Initialize top level layout
      App.Window_View.Create (Main_Window);
      App.Main_View.Create (App.Window_View);

      -- Initialize Menu
      App.Othello.Menubar1.Create (App.Main_View);
      App.Othello.File1.Create (App.Othello.Menubar1, "File");
      App.Othello.New_Game1.Create (App.Othello.Menubar1, "New Game");
      App.Othello.Pass1.Create (App.Othello.Menubar1, "Pass");
      App.Othello.Exit1.Create (App.Othello.Menubar1, "Exit");
      App.Othello.Help1.Create (App.Othello.Menubar1, "Help");
      App.Othello.About1.Create (App.Othello.Menubar1, "About");

      -- Attache hanlders for enabled buttons
      App.Othello.New_Game1.On_Click_Handler (On_New_Game1_Activate'Access);
      App.Othello.Pass1.On_Click_Handler (On_Pass1_Activate'Access);
      App.Othello.Exit1.On_Click_Handler (On_Exit1_Activate'Access);

      -- Disable unimplemented buttons
      App.Othello.File1.Disabled;
      App.Othello.Help1.Disabled;
      App.Othello.About1.Disabled;

      -- Initialize Board layout
      App.Othello.Vbox1.Create (App.Main_View);
      App.Othello.Table1.Create
        (Parent      => App.Othello.Vbox1,
         Layout => (Othello_Types.Valid_Row'Range => (Othello_Types.Valid_Column'Range => Gnoga.Gui.View.Grid.COL)),
         Fill_Parent => False, Set_Sizes => False);

      -- Initialize and setup individual board cells
      for I in Othello_Types.Valid_Row loop
         for J in Othello_Types.Valid_Column loop
            -- Initialize Buttons
            App.Playing_Board (I, J).Button.Row    := I;
            App.Playing_Board (I, J).Button.Column := J;
            App.Playing_Board (I, J).Button.Create (App.Othello.Table1.Panel (I, J).all);

            -- Set button layout
            App.Playing_Board (I, J).Button.Width (50);
            App.Playing_Board (I, J).Button.Height (50);
            App.Playing_Board (I, J).Button.Border;
            App.Playing_Board (I, J).Button.Background_Color (Gnoga.Types.Colors.Light_Gray);

            -- Initialze circles and set their layout
            App.Playing_Board (I, J).Pixmap.Create (App.Playing_Board (I, J).Button);
            App.Playing_Board (I, J).Pixmap.Width (40);
            App.Playing_Board (I, J).Pixmap.Height (40);
            App.Playing_Board (I, J).Pixmap.Position (Gnoga.Gui.Element.Relative);
            App.Playing_Board (I, J).Pixmap.Top
              (App.Playing_Board (I, J).Button.Height / 2 - App.Playing_Board (I, J).Pixmap.Height / 2);
            App.Playing_Board (I, J).Pixmap.Left
              (App.Playing_Board (I, J).Button.Width / 2 - App.Playing_Board (I, J).Pixmap.Width / 2);
            App.Playing_Board (I, J).Pixmap.Border_Radius ("75%");

            -- Attach button handler.  Circles will inherit this
            App.Playing_Board (I, J).Button.On_Click_Handler (Make_Move'Access);
         end loop;
      end loop;

      -- Create status bar area
      App.Othello.Hbox1.Create (App.Main_View);
      App.Othello.Main_Statusbar.Create (App.Othello.Hbox1);
      App.Othello.Blue_Statusbar.Create (App.Othello.Hbox1);
      App.Othello.Red_Statusbar.Create (App.Othello.Hbox1);

      -- Start a new game!
      New_Game
        (Othello   => App.Othello, Playing_Board => App.Playing_Board, Whose_Move => App.Whose_Move,
         Game_Over => App.Game_Over);

   end On_Connect;

end Othello_Pkg.Callbacks;
