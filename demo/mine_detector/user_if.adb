-- Mine Detector Game
-- Copyright (C) 2014 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- V7.1 2014 Dec 10          Protected field-updating operations
-- V7.0 2014 Dec 01          First Gnoga version
--

with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;

with System;

with Field.Operations;
pragma Elaborate (Field.Operations);

with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Table;
with Gnoga.Gui.View.Docker;
with Gnoga.Gui.Window;
with Gnoga.Types;

use Ada;
use Ada.Characters;
use Ada.Strings.Unbounded;

package body User_IF is
   Gray : constant Gnoga.Types.RGBA_Type := (Red => 224, Green => 224, Blue => 224, Alpha => 1.0);

   type Button_Set is array (Field.Valid_Row, Field.Valid_Column) of Gnoga.Gui.Element.Common.Button_Type;

   type Action_ID is (Button_Press, Right_Click, Restart, Quit);

   type Atomic_Boolean is new Boolean;
   pragma Atomic (Atomic_Boolean);

   type App_Info;
   type App_Ptr is access all App_Info;

   protected type Sequentialize is
      entry Respond (Action : in Action_ID; App_Data : in App_Ptr; Cell : in Field.Cell_Location := (Row => 1, Column => 1) );
   end Sequentialize;

   type App_Info is new Gnoga.Types.Connection_Data_Type with record
      Field                     : Standard.Field.Field_Info (App_Data => App_Info'Unchecked_Access);
      Window                    : Gnoga.Gui.Window.Pointer_To_Window_Class;
      Big_View                  : Gnoga.Gui.View.Docker.Docker_View_Type;
      Left_View                 : aliased Gnoga.Gui.View.View_Type;
      Right_View                : aliased Gnoga.Gui.View.View_Type;
      Mines_Left                : Gnoga.Gui.Element.Common.Span_Type;
      Button                    : Button_Set;
      Restart_Button            : Gnoga.Gui.Element.Common.Button_Type;
      Level_Form                : Gnoga.Gui.Element.Form.Form_Type;
      Level                     : Gnoga.Gui.Element.Form.Selection_Type;
      Mark_Form                 : Gnoga.Gui.Element.Form.Form_Type;
      Mark_Check                : Gnoga.Gui.Element.Form.Check_Box_Type;
      Mark_Label                : Gnoga.Gui.Element.Form.Label_Type;
      Step_Form                 : Gnoga.Gui.Element.Form.Form_Type;
      Step_Check                : Gnoga.Gui.Element.Form.Check_Box_Type;
      Step_Label                : Gnoga.Gui.Element.Form.Label_Type;
      Rules                     : Gnoga.Gui.Element.Common.Button_Type;
      About                     : Gnoga.Gui.Element.Common.Button_Type;
      Quit                      : Gnoga.Gui.Element.Common.Button_Type;
      Game_Over                 : Gnoga.Gui.Element.Common.Span_Type;
      Auto_Marking_Desired      : Atomic_Boolean := False;
      Extended_Stepping_Desired : Atomic_Boolean := False;
      Sequentializer            : Sequentialize;
   end record;

   You_Won_Message  : constant String := "You Won";
   You_Lost_Message : constant String := "BOOM!";

   type Level_Info is record
      Name  : String (1 .. 3);
      Mines : Natural;
   end record;

   type Level_List is array (Positive range <>) of Level_Info;

   Levels : constant Level_List := (1 => (Name => " 50", Mines =>  50),
                                    2 => (Name => "100", Mines => 100),
                                    3 => (Name => "150", Mines => 150),
                                    4 => (Name => "200", Mines => 200),
                                    5 => (Name => "250", Mines => 250) );

   Default_Level : constant := 2;

   subtype Cell_String is String (1 .. 1);

   procedure Show_Game_Over (App_Data : in App_Ptr) is
      -- null;
   begin -- Show_Game_Over
      case Field.Operations.Game_State (App_Data.Field) is
      when Field.Operations.Won =>
         App_Data.Game_Over.Text (Value => You_Won_Message);
      when Field.Operations.Lost =>
         App_Data.Game_Over.Text (Value => You_Lost_Message);
      when Field.Operations.In_Progress =>
         null;
      end case;
   end Show_Game_Over;
   pragma Inline (Show_Game_Over);

   use type Field.Operations.Game_State_ID;

   Button_Size : constant := 25;

   procedure Display (App_Data : in App_Ptr; Cell : in Field.Cell_Location; Text : in Cell_String; Active : in Boolean) is
      -- null;
   begin -- Display
      App_Data.Button (Cell.Row, Cell.Column).Text (Value => Text);

      if Active then
         App_Data.Button (Cell.Row, Cell.Column).Background_Color (RGBA => Gray);
         App_Data.Button (Cell.Row, Cell.Column).Shadow
            (Horizontal_Position => "1px", Vertical_Position => "1px", Inset_Shadow => True);
      else
         App_Data.Button (Cell.Row, Cell.Column).Background_Color (Value => "white");
         App_Data.Button (Cell.Row, Cell.Column).Shadow_None;
      end if;

      if Field.Operations.Game_State (App_Data.Field) /= Field.Operations.In_Progress then
         Show_Game_Over (App_Data => App_Data);
      end if;
   end Display;
   pragma Inline (Display);

   procedure Display_Blank (Data : in Gnoga.Types.Pointer_To_Connection_Data_Class; Cell : in Field.Cell_Location) is
      App_Data : App_Ptr := App_Ptr (Data);
   begin -- Display_Blank
      Display (App_Data => App_Data, Cell => Cell, Text => " ", Active => False);
   end Display_Blank;

   procedure Display_Count (Data     : in Gnoga.Types.Pointer_To_Connection_Data_Class;
                            Count    : in Field.Valid_Count;
                            Stepped  : in Boolean;
                            Cell     : in Field.Cell_Location)
   is
      Zero_Pos : constant := Character'Pos ('0');

      App_Data : App_Ptr := App_Ptr (Data);
   begin -- Display_Count
      Display (App_Data => App_Data,
               Cell     => Cell,
               Text     => Character'Val (Zero_Pos + Count) & "",
               Active   => Stepped);
   end Display_Count;

   procedure Display_Mark (Data : in Gnoga.Types.Pointer_To_Connection_Data_Class; Cell : in Field.Cell_Location) is
      App_Data : App_Ptr := App_Ptr (Data);
   begin -- Display_Mark
      Display (App_Data => App_Data, Cell => Cell, Text => "M", Active => False);
   end Display_Mark;

   procedure Display_Mine (Data : in Gnoga.Types.Pointer_To_Connection_Data_Class; Cell : in Field.Cell_Location) is
      App_Data : App_Ptr := App_Ptr (Data);
   begin -- Display_Mine
      Display (App_Data => App_Data, Cell => Cell, Text => "X", Active => True);
   end Display_Mine;

   procedure Display_To_Go (Data : in Gnoga.Types.Pointer_To_Connection_Data_Class; To_Go : in Integer) is
      Image : constant String := Integer'Image (To_Go);

      App_Data : App_Ptr := App_Ptr (Data);
   begin -- Display_To_Go
      App_Data.Mines_Left.Text (Value => Image);
   end Display_To_Go;

   procedure Reset_Screen (Data : in Gnoga.Types.Pointer_To_Connection_Data_Class) is
      App_Data : App_Ptr := App_Ptr (Data);
   begin -- Reset_Screen
      App_Data.Mines_Left.Text (Value => "0");
      App_Data.Game_Over.Text  (Value => "");

      Button_Row : for Row in Field.Valid_Row loop
         Button_Column : for Column in Field.Valid_Column loop
            Display_Blank (Data => Data, Cell => (Row => Row, Column => Column) );
         end loop Button_Column;
      end loop Button_Row;
   end Reset_Screen;

   function Auto_Marking (Data : Gnoga.Types.Pointer_To_Connection_Data_Class) return Boolean is
      App_Data : App_Ptr := App_Ptr (Data);
   begin -- Auto_Marking
      return Boolean (App_Data.Auto_Marking_Desired);
   end Auto_Marking;

   function Extended_Stepping (Data : in Gnoga.Types.Pointer_To_Connection_Data_Class) return Boolean is
      App_Data : App_Ptr := App_Ptr (Data);
   begin -- Extended_Stepping
      return Boolean (App_Data.Extended_Stepping_Desired);
   end Extended_Stepping;

   procedure When_Close (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App_Data : App_Ptr := App_Ptr (Object.Connection_Data);
   begin -- When_Close
      App_Data.Sequentializer.Respond (Action => Quit, App_Data => App_Data);
   end When_Close;

   procedure Mark_Toggle (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App_Data : App_Ptr := App_Ptr (Object.Connection_Data);
   begin -- Mark_Toggle
      App_Data.Auto_Marking_Desired := Atomic_Boolean (App_Data.Mark_Check.Checked);
   end Mark_Toggle;

   procedure Step_Toggle (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App_Data : App_Ptr := App_Ptr (Object.Connection_Data);
   begin -- Step_Toggle
      App_Data.Extended_Stepping_Desired := Atomic_Boolean (App_Data.Step_Check.Checked);
   end Step_Toggle;

   procedure Button_Press (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Name : constant String := Object.ID;

      Row    : constant Field.Valid_Row    := Field.Valid_Row'Value    (Name (Name'First    .. Name'First + 1) );
      Column : constant Field.Valid_Column := Field.Valid_Column'Value (Name (Name'Last - 1 .. Name'Last) );

      App_Data : App_Ptr := App_Ptr (Object.Connection_Data);
   begin -- Button_Press
      App_Data.Sequentializer.Respond (Action => Button_Press, App_Data => App_Data, Cell => (Row => Row, Column => Column) );
   end Button_Press;

   procedure Right_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Name : constant String := Object.ID;

      Row    : constant Field.Valid_Row    := Field.Valid_Row'Value    (Name (Name'First    .. Name'First + 1) );
      Column : constant Field.Valid_Column := Field.Valid_Column'Value (Name (Name'Last - 1 .. Name'Last) );

      App_Data : App_Ptr := App_Ptr (Object.Connection_Data);
   begin -- Right_Click
      App_Data.Sequentializer.Respond (Action => Right_Click, App_Data => App_Data, Cell => (Row => Row, Column => Column) );
   end Right_Click;

   procedure When_Restart_Button (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App_Data : App_Ptr := App_Ptr (Object.Connection_Data);
   begin -- When_Restart_Button
      App_Data.Sequentializer.Respond (Action => Restart, App_Data => App_Data);
   end When_Restart_Button;

   procedure Rules_Pressed (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App_Data : App_Ptr := App_Ptr (Object.Connection_Data);

      Rules : constant String :=
         "The object of the game is to mark all cells containing " &
         "mines and to step on all cells that do not contain a " &
         "mine." & Latin_1.LF &
         Latin_1.LF &
         "The game is played on a rectangular field of 16 x 30 " &
         "cells. A number of mines are hidden within the field." & Latin_1.LF &
         Latin_1.LF &
         "Some of the cells have numbers on them. The numbers represent " &
         "the total number of mines in that cell and its " &
         "immediate neighbors. As you play the game, additional cells " &
         "will become numbered." & Latin_1.LF &
         Latin_1.LF &
         "You step on a cell by clicking on it. You mark a cell by right " &
         "clicking on it. A marked cell has an M on it. Marking a " &
         "marked cell unmarks it. You can only mark or step " &
         "on a cell with a number on it." & Latin_1.LF &
         Latin_1.LF &
         "When you step on a cell, an auto-stepping algorithm " &
         "automatically steps on any of its neighbors that " &
         "obviously do not contain mines. Since this is then " &
         "done for the neighbors of the stepped-on neighbors, " &
         "the auto-stepping algorithm will spread across areas " &
         "of the field that obviously do not contain mines. The " &
         "auto-stepping algorithm is invoked even if the cell is " &
         "already stepped on. This can be useful to clear around " &
         "a new mark." & Latin_1.LF &
         Latin_1.LF &
         "If you step on a cell containing a mine, either " &
         "directly or indirectly through the auto-stepping " &
         "algorithm, the cell shows an X, and the game is over." &
         Latin_1.LF &
         Latin_1.LF &
         "The game is over when you step on a mine, or when you " &
         "have marked all mines and stepped on all other cells. " &
         "If you win, '" & You_Won_Message & "' appears below the " &
         "'Quit' button. If you lose, '" & You_Lost_Message &
         "' appears there." & Latin_1.LF &
         Latin_1.LF &
         "At the top right of the field is a number. At the " &
         "start of a game this is the number of mines in the " &
         "field. Each time you mark a cell, this number is " &
         "decreased by one. Each time you unmark a marked cell, " &
         "this number is increased by one. If you successfully " &
         "complete a game, this number will be zero." & Latin_1.LF &
         Latin_1.LF &
         "The 'New Game' button starts a new game. Any game in " &
         "progress is abandoned." & Latin_1.LF &
         Latin_1.LF &
         "The level drop-down allows you to choose how many mines " &
         "will be in the field at the start of the next game. You " &
         "can choose from" & Levels (Levels'First).Name & " to " &
         Levels (Levels'Last).Name & " mines. This goes into effect " &
         "the next time you start a new game. At higher numbers of " &
         "mines, it may not be possible to win the game without luck." & Latin_1.LF &
         Latin_1.LF &
         "The 'Auto Mark' check box enables an auto-marking " &
         "algorithm that marks any cells that obviously contain " &
         "a mine. At lower levels, the game does not present much " &
         "of an intellectual challenge with this option. At higher " &
         "levels, it's very difficult to play without this option." & Latin_1.LF &
         Latin_1.LF &
         "The 'Auto Step after Mark' check box enables the auto-" &
         "stepping algorithm after a cell is marked, either " &
         "directly or indirectly through the auto-marking " &
         "algorithm.";
   begin -- Rules_Pressed
      App_Data.Window.Alert (Message => Rules);
   end Rules_Pressed;

   procedure About_Pressed (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App_Data : App_Ptr := App_Ptr (Object.Connection_Data);
   begin -- About_Pressed
      App_Data.Window.Alert (Message => "Mine Detector" & Latin_1.LF &
                               		"Copyright (C) 2014 by" & Latin_1.LF &
                               		"PragmAda Software Engineering" & Latin_1.LF &
                               		"Released as Free Software under the terms" & Latin_1.LF &
                               		"of the GNU Public License" & Latin_1.LF &
                               		'"' & "Ada Inside" & '"');
   end About_Pressed;

   function Image (Row : Field.Valid_Row; Column : Field.Valid_Column) return String is
   -- Returns a 4-Character String of the form "RRCC", where
   --    RR is the zero-filled image of Row
   --    CC is the zero-filled image of Column
      Row_Image    : String   := Field.Valid_Row'Image    (Row);
      Column_Image : String   := Field.Valid_Column'Image (Column);
      Row_First    : Positive := Row_Image'First;
      Column_First : Positive := Column_Image'First;
   begin -- Image
      Row_Image (Row_Image'First) := '0';
      Column_Image (Column_Image'First) := '0';

      if Row >= 10 then
         Row_First := Row_First + 1;
      end if;

      if Column >= 10 then
         Column_First := Column_First + 1;
      end if;

      return Row_Image (Row_First .. Row_Image'Last) & Column_Image (Column_First .. Column_Image'Last);
   end Image;

   procedure Create_Level_Option_Menu (App_Data : in out App_Info) is
      -- null;
   begin -- Create_Level_Option_Menu
      Add_Options : for I in Levels'range loop
         App_Data.Level.Add_Option (Value => Levels (I).Name, Text => Levels (I).Name);
      end loop Add_Options;
   end Create_Level_Option_Menu;

   procedure On_Connect (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
                         Connection  : Access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App_Data : App_Ptr := new App_Info;
   begin -- On_Connect
      App_Data.Window := Main_Window'Unchecked_Access;
      Main_Window.Connection_Data (Data => App_Data);
      Field.Operations.Set_Mine_Count (Field => App_Data.Field, New_Mine_Count => Levels (Default_Level).Mines);
      Main_Window.Buffer_Connection;
      App_Data.Big_View.Create (Parent => Main_Window);
      App_Data.Left_View.Create (Parent => App_Data.Big_View);
      App_Data.Big_View.Left_Dock (Dock => App_Data.Left_View'Access);
      App_Data.Left_View.Hidden;

      Button_Row    : for Row in App_Data.Button'Range(1) loop
         Button_Column : for Column in App_Data.Button'Range (2) loop
            App_Data.Button (Row, Column).Create (Parent => App_Data.Left_View, Content => " ", ID => Image (Row, Column) );
            App_Data.Button (Row, Column).Overflow (Value => Gnoga.Gui.Element.Hidden);
            App_Data.Button (Row, Column).Vertical_Align (Value => Gnoga.Gui.Element.Middle);
            App_Data.Button (Row, Column).Minimum_Width  (Value => Button_Size);
            App_Data.Button (Row, Column).Maximum_Width  (Value => Button_Size);
            App_Data.Button (Row, Column).Minimum_Height (Value => Button_Size);
            App_Data.Button (Row, Column).Maximum_Height (Value => Button_Size);
            App_Data.Button (Row, Column).Text_Alignment (Value => Gnoga.Gui.Element.Center);
            App_Data.Button (Row, Column).Margin (Top => "1px", Right => "1px", Bottom => "1px", Left => "1px");
            App_Data.Button (Row, Column).Border (Width => "thin");
            App_Data.Button (Row, Column).On_Click_Handler (Handler => Button_Press'Access);
            App_Data.Button (Row, Column).On_Context_Menu_Handler (Handler => Right_Click'Access);
         end loop Button_Column;

         App_Data.Left_View.Put_HTML (HTML => "<br />");
      end loop Button_Row;

      App_Data.Left_View.Hidden (Value => False);
      App_Data.Right_View.Create (Parent => App_Data.Big_View);
      App_Data.Big_View.Right_Dock (Dock => App_Data.Right_View'Access);
      App_Data.Mines_Left.Create (Parent => App_Data.Right_View, Content => "0");
      App_Data.Mines_Left.Width (Value => 100);
      App_Data.Mines_Left.Text_Alignment (Value => Gnoga.Gui.Element.Center);
      App_Data.Mines_Left.Display (Value => "block");
      App_Data.Restart_Button.Create (Parent => App_Data.Right_View, Content => "New Game");
      App_Data.Restart_Button.Display (Value => "block");
      App_Data.Restart_Button.On_Click_Handler (Handler => When_Restart_Button'Access);
      App_Data.Level_Form.Create (Parent => App_Data.Right_View);
      App_Data.Level_Form.Display (Value => "block");
      App_Data.Level.Create (Form => App_Data.Level_Form);
      App_Data.Level.Width (Value => 57);
      Create_Level_Option_Menu (App_Data => App_Data.all);
      App_Data.Level.Selected (Index => Default_Level);
      App_Data.Mark_Form.Create (Parent => App_Data.Right_View);
      App_Data.Mark_Form.Display (Value => "block");
      App_Data.Mark_Check.Create (Form => App_Data.Mark_Form);
      App_Data.Mark_Check.Checked (Value => False);
      App_Data.Mark_Check.On_Click_Handler (Handler => Mark_Toggle'Access);
      App_Data.Mark_Label.Create
         (Form => App_Data.Mark_Form, Label_For => App_Data.Mark_Check, Contents => "Auto Mark", Auto_Place => False);
      App_Data.Step_Form.Create (Parent => App_Data.Right_View);
      App_Data.Step_Form.Display (Value => "block");
      App_Data.Step_Check.Create (Form => App_Data.Step_Form);
      App_Data.Step_Check.Checked (Value => False);
      App_Data.Step_Check.On_Click_Handler (Handler => Step_Toggle'Access);
      App_Data.Step_Label.Create
         (Form => App_Data.Step_Form, Label_For => App_Data.Step_Check, Contents => "Auto Step after Mark", Auto_Place => False);
      App_Data.Rules.Create (Parent => App_Data.Right_View, Content => "Rules");
      App_Data.Rules.Display (Value => "block");
      App_Data.Rules.On_Click_Handler (Handler => Rules_Pressed'Access);
      App_Data.About.Create (Parent => App_Data.Right_View, Content => "About");
      App_Data.About.Display (Value => "block");
      App_Data.About.On_Click_Handler (Handler => About_Pressed'Access);
      App_Data.Quit.Create (Parent => App_Data.Right_View, Content => "Quit");
      App_Data.Quit.Display (Value => "block");
      App_Data.Quit.On_Click_Handler (Handler => When_Close'Access);
      App_Data.Game_Over.Create (Parent => App_Data.Right_View, Content => You_Won_Message);
      App_Data.Game_Over.Width (Value => 100);
      App_Data.Game_Over.Text_Alignment (Value => Gnoga.Gui.Element.Center);
      App_Data.Game_Over.Display (Value => "block");
      Main_Window.Buffer_Connection (Value => False);
      Field.Operations.Reset (Field => App_Data.Field);
   end On_Connect;

   protected body Sequentialize is
      entry Respond (Action : in Action_ID; App_Data : in App_Ptr; Cell : in Field.Cell_Location := (Row => 1, Column => 1) )
      when True is
      begin -- Respond
         case Action is
         when Button_Press =>
               if Field.Operations.Game_State (App_Data.Field) /= Field.Operations.In_Progress then
                  Show_Game_Over (App_Data => App_Data);
               else
                  Field.Operations.Step (Field => App_Data.Field, Cell => Cell);
               end if;
         when Right_Click =>
               if Field.Operations.Game_State (App_Data.Field) /= Field.Operations.In_Progress then
                  Show_Game_Over (App_Data => App_Data);
               else
                  Field.Operations.Mark (Field => App_Data.Field, Cell => Cell);
               end if;
         when Restart =>
            Field.Operations.Set_Mine_Count
               (Field => App_Data.Field, New_Mine_Count => Levels (App_Data.Level.Selected_Index).Mines);
            Field.Operations.Reset (Field => App_Data.Field);
         when Quit =>
            Disable_Rows : for Row in App_Data.Button'Range (1) loop
               Disable_Columns : for Column in App_Data.Button'Range (2) loop
                  App_Data.Button (Row, Column).Disabled;
               end loop Disable_Columns;
            end loop Disable_Rows;

            App_Data.Restart_Button.Disabled;
            App_Data.Level.Disabled;
            App_Data.Mark_Check.Disabled;
            App_Data.Step_Check.Disabled;
            App_Data.Rules.Disabled;
            App_Data.About.Disabled;
            App_Data.Quit.Disabled;

            Move_Down : for I in 1 .. 25 loop
               App_Data.Big_View.New_Line;
            end loop Move_Down;

            App_Data.Big_View.Put_Line (Message => "Mine Detector ended.");
         end case;
      end Respond;
   end Sequentialize;
begin -- User_IF
   Gnoga.Application.Title (Name => "Mine Detector");
   Gnoga.Application.HTML_On_Close (HTML => "Mine Detector ended.");
   Gnoga.Application.Multi_Connect.Initialize (Port => 8081);
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Access, Path  => "default");
   Gnoga.Application.Multi_Connect.Message_Loop;
end User_IF;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
