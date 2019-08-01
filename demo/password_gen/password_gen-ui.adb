-- Password_Gen: a password-generator program
-- Copyright (C) 2017 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- User Interface
--
-- V1.2  2017 Nov 15     Move password-generation logic into a package
-- V1.1  2016 Jun 01     Correct handling when no digit
-- V1.0  2016 Jan 15     Initial version
--
with Ada.Exceptions;

with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Types;
with Gnoga.Gui.View;
with Gnoga.Gui.Window;

with Password_Generation;

package body Password_Gen.UI is
   type App_Info is new Gnoga.Types.Connection_Data_Type with record
      Window       : Gnoga.Gui.Window.Pointer_To_Window_Class;
      View         : Gnoga.Gui.View.View_Type;
      Input        : Gnoga.Gui.Element.Form.Form_Type;
      Domain       : Gnoga.Gui.Element.Form.Text_Type;
      Domain_Label : Gnoga.Gui.Element.Form.Label_Type;
      Master       : Gnoga.Gui.Element.Form.Password_Type;
      Master_Label : Gnoga.Gui.Element.Form.Label_Type;
      Length       : Gnoga.Gui.Element.Form.Range_Type;
      Length_Label : Gnoga.Gui.Element.Form.Label_Type;
      Length_Img   : Gnoga.Gui.Element.Form.Text_Type;
      Symbol       : Gnoga.Gui.Element.Form.Selection_Type;
      Symbol_Label : Gnoga.Gui.Element.Form.Label_Type;
      Hash_Symbol  : Gnoga.Gui.Element.Form.Check_Box_Type;
      Hash_Lable   : Gnoga.Gui.Element.Form.Label_Type;
      Result       : Gnoga.Gui.Element.Form.Text_Type;
      Generate     : Gnoga.Gui.Element.Form.Submit_Button_Type;
      Quit         : Gnoga.Gui.Element.Common.Button_Type;
   end record;

   type App_Ptr is access all App_Info;

   procedure On_Length_Change (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);
   begin -- On_Length_Change
      App.Length_Img.Value (Value => String'(App.Length.Value) );
   exception -- On_Length_Change
   when E : others =>
      Gnoga.Log (Message => "On_Length_Change: " & Ada.Exceptions.Exception_Information (E) );
   end On_Length_Change;

   procedure On_Generate (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);
   begin -- On_Generate
      App.Result.Value (Value => Password_Generation.Generate (App.Domain.Value,
                                                               App.Master.Value,
                                                               App.Length.Value,
                                                               App.Symbol.Value,
                                                               App.Hash_Symbol.Checked) );
   exception -- On_Generate
   when E : others =>
      Gnoga.Log (Message => "On_Generate: " & Ada.Exceptions.Exception_Information (E) );
   end On_Generate;

   End_Message : constant String := "Password Generator ended.";

   procedure On_Quit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);

      View : Gnoga.Gui.View.View_Type;
   begin -- On_Quit
      App.View.Remove;
      View.Create (Parent => App.Window.all);
      View.Put_Line (Message => End_Message);
      App.Window.Close;
      App.Window.Close_Connection;
   exception -- On_Quit
   when E : others =>
      Gnoga.Log (Message => "On_Quit: " & Ada.Exceptions.Exception_Information (E) );
   end On_Quit;

   procedure On_Connect (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
                         Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App : constant App_Ptr := new App_Info;
   begin -- On_Connect
      Main_Window.Connection_Data (Data => App);
      App.Window := Main_Window'Unchecked_Access;
      App.View.Create (Parent => Main_Window);
      App.View.Box_Width (Value => 500);
      App.Input.Create (Parent => App.View);
      App.Input.Text_Alignment (Value => Gnoga.Gui.Element.Right);
      App.Domain.Create (Form => App.Input);
      App.Domain.Required;
      App.Domain.Place_Holder (Value => "example.com, example.fr, example.co.uk");
      App.Domain_Label.Create (Form => App.Input, Label_For => App.Domain, Content => "Domain:");
      App.Input.New_Line;
      App.Master.Create (Form => App.Input);
      App.Master.Required;
      App.Master_Label.Create (Form => App.Input, Label_For => App.Master, Content => "Master Password:");
      App.Input.New_Line;
      App.Length.Create (Form => App.Input, Value => "12");
      App.Length.Minimum (Value => 8);
      App.Length.Maximum (Value => 16);
      App.Length.On_Change_Handler (Handler => On_Length_Change'Access);
      App.Length_Label.Create (Form => App.Input, Label_For => App.Length, Content => "Password Length:");
      App.Length_Img.Create (Form => App.Input, Size => 2, Value => App.Length.Value);
      App.Input.New_Line;
      App.Symbol.Create (Form => App.Input);
      App.Symbol_Label.Create (Form => App.Input, Label_For => App.Symbol, Content => "Symbol:");
      App.Symbol.Add_Option (Value => Password_Generation.No_Symbol, Text => Password_Generation.No_Symbol);
      App.Symbol.Add_Option (Value => Password_Generation.Auto_Symbol, Text => Password_Generation.Auto_Symbol, Selected => True);

      All_Symbols : for I in Password_Generation.Symbol_Set'Range loop
         App.Symbol.Add_Option
            (Value => (1 => Password_Generation.Symbol_Set (I) ), Text => (1 => Password_Generation.Symbol_Set (I) ) );
      end loop All_Symbols;

      App.Hash_Symbol.Create (Form => App.Input, Checked => True);
      App.Hash_Lable.Create
         (Form => App.Input, Label_For => App.Hash_Symbol, Content => "Include Symbol in hash", Auto_Place => False);
      App.Input.New_Line;
      App.Result.Create (Form => App.Input);
      App.Result.Font (Family => "monospace");
      App.Result.Size (Value => App.Length.Maximum);
      App.Generate.Create (Form => App.Input, Value => "Generate");
      App.Input.On_Submit_Handler (Handler => On_Generate'Access);
      App.Input.New_Line;
      App.Input.New_Line;
      App.Quit.Create (Parent => App.Input, Content => "Quit");
      App.Quit.On_Click_Handler (Handler => On_Quit'Access);
   exception -- On_Connect
   when E : others =>
      Gnoga.Log (Message => "On_Connect: " & Ada.Exceptions.Exception_Information (E) );
   end On_Connect;
begin -- Password_Gen.UI
   Gnoga.Application.Title (Name => "Password Generator");
   Gnoga.Application.HTML_On_Close (HTML => End_Message);
   Gnoga.Application.Multi_Connect.Initialize;
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Access);
   Gnoga.Application.Multi_Connect.Message_Loop;
exception -- Password_Gen.UI
when E : others =>
   Gnoga.Log (Message => Ada.Exceptions.Exception_Information (E) );
end Password_Gen.UI;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
