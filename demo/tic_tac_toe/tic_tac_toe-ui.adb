-- Tic_Tac_Toe: a program to play Tic-Tac-Toe
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
--
-- 2016 Aug 26     J. Carter      V1.0--Initial version
--
with Ada.Exceptions;

with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Types.Colors;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Window;

package body Tic_Tac_Toe.UI is
   subtype Row_ID is Integer range 1 .. 3;
   subtype Column_ID is Integer range 1 .. 3;

   type Piece_ID is (X, O, Empty);

   type Board_Set is array (Row_ID, Column_ID) of Piece_ID;

   Empty_Board : constant Board_Set := (Row_ID => (Column_ID => Empty));

   type Button_Set is array (Row_ID, Column_ID) of Gnoga.Gui.Element.Common.Button_Type;

   subtype Square_String is String (1 .. 1);

   X_Mark     : constant Square_String := "X";
   O_Mark     : constant Square_String := "O";
   Empty_Mark : constant Square_String := " ";

   type App_Info is new Gnoga.Types.Connection_Data_Type with record
      Window      : Gnoga.Gui.Window.Pointer_To_Window_Class;
      Grid        : Gnoga.Gui.View.Grid.Grid_View_Type;
      Left_View   : Gnoga.Gui.View.View_Type;
      Right_View  : Gnoga.Gui.View.View_Type;
      Square      : Button_Set;
      Message     : Gnoga.Gui.Element.Common.Span_Type;
      Control     : Gnoga.Gui.Element.Form.Form_Type;
      Won         : Gnoga.Gui.Element.Form.Text_Type;
      Won_Label   : Gnoga.Gui.Element.Form.Label_Type;
      Lost        : Gnoga.Gui.Element.Form.Text_Type;
      Lost_Label  : Gnoga.Gui.Element.Form.Label_Type;
      Kat         : Gnoga.Gui.Element.Form.Text_Type;
      Kat_Label   : Gnoga.Gui.Element.Form.Label_Type;
      First_Check : Gnoga.Gui.Element.Form.Check_Box_Type;
      First_Label : Gnoga.Gui.Element.Form.Label_Type;
      Again       : Gnoga.Gui.Element.Common.Button_Type;
      Quit        : Gnoga.Gui.Element.Common.Button_Type;

      Board         : Board_Set;
      Player        : Piece_ID      := X;
      Computer      : Piece_ID      := O;
      Player_Mark   : Square_String := X_Mark;
      Computer_Mark : Square_String := O_Mark;
      Num_Won       : Natural       := 0;
      Num_Lost      : Natural       := 0;
      Num_Kat       : Natural       := 0;
      Player_Move   : Natural       := 0;
      Computer_Move : Natural       := 0;
   end record;

   type App_Ptr is access all App_Info;

   Your_Turn : constant String := "Your turn";
   You_Won   : constant String := "You won";
   You_Lost  : constant String := "You lost";
   Kat_Game  : constant String := "Kat game";

   package Logic is
      procedure Process_Player_Move
        (App    : in out App_Info;
         Row    : in     Row_ID;
         Column : in     Column_ID);
      -- Respond to Player moving to (Row, Column)

      procedure Reset (App : in out App_Info);
      -- Reset the board for a new game
   end Logic;

   package body Logic is separate;

   procedure Square_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Name : constant String := Object.ID;

      Row    : constant Row_ID    := Integer'Value (Name (Name'First .. Name'First));
      Column : constant Column_ID := Integer'Value (Name (Name'Last .. Name'Last));

      App : constant App_Ptr := App_Ptr (Object.Connection_Data);
   begin -- Square_Click
      Logic.Process_Player_Move (App => App.all, Row => Row, Column => Column);
   end Square_Click;

   procedure New_Game (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);
   begin -- New_Game
      Logic.Reset (App => App.all);
   end New_Game;

   End_Message : constant String := "Tic-Tac-Toe ended.";

   procedure On_Quit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);

      View : Gnoga.Gui.View.View_Type;
   begin -- On_Quit
      App.Grid.Remove;
      View.Create (Parent => App.Window.all);
      View.Put_Line (Message => End_Message);
      App.Window.Close;
      App.Window.Close_Connection;
   exception -- On_Quit
      when E : others =>
         Gnoga.Log (Message => "On_Quit: " & Ada.Exceptions.Exception_Information (E));
   end On_Quit;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      function Image
        (Row    : Row_ID;
         Column : Column_ID)
         return String;
      -- Returns a 2-Character String of the form "RC", where
      --    R is the image of Row
      --    C is the image of Column

      function Image
        (Row    : Row_ID;
         Column : Column_ID)
         return String
      is
         Row_Image    : constant String := Integer'Image (Row);
         Column_Image : constant String := Integer'Image (Column);
      begin -- Image
         return Row_Image (Row_Image'Last) & Column_Image (Column_Image'Last);
      end Image;

      Button_Size : constant := 100;

      App : App_Ptr := new App_Info;
   begin -- On_Connect
      Main_Window.Connection_Data (Data => App);
      App.Window := Main_Window'Unchecked_Access;
      App.Grid.Create (Parent => Main_Window, Layout => Gnoga.Gui.View.Grid.Horizontal_Split);
      App.Grid.Background_Color (Enum => Gnoga.Types.Colors.Light_Blue);
      App.Left_View.Create (Parent => App.Grid.Panel (1, 1).all);
      App.Left_View.Background_Color (Enum => Gnoga.Types.Colors.Light_Blue);
      App.Left_View.Text_Alignment (Value => Gnoga.Gui.Element.Center);
      App.Board := Empty_Board;

      Square_Rows :
      for Row in App.Square'Range (1) loop
         Square_Columns :
         for Column in App.Square'Range (2) loop
            App.Square (Row, Column).Create (Parent => App.Left_View, Content => Empty_Mark, ID => Image (Row, Column));
            App.Square (Row, Column).Overflow (Value => Gnoga.Gui.Element.Hidden);
            App.Square (Row, Column).Vertical_Align (Value => Gnoga.Gui.Element.Middle);
            App.Square (Row, Column).Minimum_Width (Value => Button_Size);
            App.Square (Row, Column).Maximum_Width (Value => Button_Size);
            App.Square (Row, Column).Minimum_Height (Value => Button_Size);
            App.Square (Row, Column).Maximum_Height (Value => Button_Size);
            App.Square (Row, Column).Text_Alignment (Value => Gnoga.Gui.Element.Center);
            App.Square (Row, Column).Margin (Top => "1px", Right => "1px", Bottom => "1px", Left => "1px");
            App.Square (Row, Column).Border (Width => "thin");
            App.Square (Row, Column).Font (Height => "xx-large");
            App.Square (Row, Column).Background_Color (Enum => Gnoga.Types.Colors.Yellow);
            App.Square (Row, Column).On_Click_Handler (Handler => Square_Click'Access);
         end loop Square_Columns;

         App.Left_View.Put_HTML (HTML => "<br />");
      end loop Square_Rows;

      App.Message.Create (Parent => App.Left_View, Content => Your_Turn);

      App.Right_View.Create (Parent => App.Grid.Panel (1, 2).all);
      App.Right_View.Background_Color (Enum => Gnoga.Types.Colors.Light_Blue);
      App.Control.Create (Parent => App.Right_View);
      App.Control.Background_Color (Enum => Gnoga.Types.Colors.Light_Blue);
      App.Control.Text_Alignment (Value => Gnoga.Gui.Element.Center);
      App.Won.Create (Form => App.Control, Size => 4, Value => "0");
      App.Won_Label.Create (Form => App.Control, Label_For => App.Won, Content => "Won");
      App.Control.New_Line;
      App.Lost.Create (Form => App.Control, Size => 4, Value => "0");
      App.Lost_Label.Create (Form => App.Control, Label_For => App.Lost, Content => "Lost");
      App.Control.New_Line;
      App.Kat.Create (Form => App.Control, Size => 4, Value => "0");
      App.Kat_Label.Create (Form => App.Control, Label_For => App.Kat, Content => "Kat");
      App.Control.New_Line;
      App.First_Check.Create (Form => App.Control, Checked => False);
      App.First_Label.Create
        (Form => App.Control, Label_For => App.First_Check, Content => "Computer moves 1st", Auto_Place => False);
      App.Control.New_Line;
      App.Again.Create (Parent => App.Control, Content => "New Game");
      App.Again.On_Click_Handler (Handler => New_Game'Access);
      App.Control.New_Line;
      App.Quit.Create (Parent => App.Control, Content => "Quit");
      App.Quit.On_Click_Handler (Handler => On_Quit'Access);
   exception -- On_Connect
      when E : others =>
         Gnoga.Log (Message => "On_Connect: " & Ada.Exceptions.Exception_Information (E));
   end On_Connect;
begin -- Tic_Tac_Toe.UI
   Gnoga.Application.Title (Name => "Tic-Tac-Toe");
   Gnoga.Application.HTML_On_Close (HTML => End_Message);
   Gnoga.Application.Multi_Connect.Initialize;
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Access);
   Gnoga.Application.Multi_Connect.Message_Loop;
exception -- Tic_Tac_Toe.UI
   when E : others =>
      Gnoga.Log (Message => Ada.Exceptions.Exception_Information (E));
end Tic_Tac_Toe.UI;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
