-- Password_Gen: a password-generator program
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- User Interface
--
-- V1.1  2016 Jun 01     Correct handling when no digit
-- V1.0  2016 Jan 15     Initial version
--
with Ada.Characters.Handling;
with Ada.Exceptions;

with GNAT.SHA512;

with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Types;
with Gnoga.Gui.View;
with Gnoga.Gui.Window;

with PragmARC.Unbounded_Integers;

package body Password_Gen.UI is
   subtype Digit is Character range '0' .. '9';
   subtype Lower is Character range 'a' .. 'z';
   subtype Upper is Character range 'A' .. 'Z';

   subtype Symbol_Range is Natural range 0 .. 9;

   type Symbol_List is array (Symbol_Range) of Character;

   Symbol_Set : constant Symbol_List := "!@#$%^&*/?";

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

   No_Symbol   : constant String := "None";
   Auto_Symbol : constant String := "Auto";

   procedure On_Generate (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      function Digest (Source : String) return String;
      -- Perform an SHA512 digest on Source and convert the result to base 36

      function Digest (Source : String) return String is
         -- Empty
      begin -- Digest
         return PragmARC.Unbounded_Integers.Image (PragmARC.Unbounded_Integers.Value ("16#" & Gnat.SHA512.Digest (Source) & '#'),
                                                   Base => 36);
      end Digest;

      App : constant App_Ptr := App_Ptr (Object.Connection_Data);

      Domain : constant String := Ada.Characters.Handling.To_Lower (App.Domain.Value);

      Salted_Input : constant String := ':' & Domain & ':' & App.Master.Value & ':';

      Digit_1_Index : Natural := 0;
      Digit_1       : Natural := 0;
      Digit_1_Rem   : Natural;
      Length        : Positive := App.Length.Value; -- The requested password length, 8 .. 16
      Letter_Count  : Natural  := 0;
      Has_Lower     : Boolean  := False;
      Has_Upper     : Boolean  := False;
      Symbol        : String (1 .. 1);
      Result        : String := Digest (Salted_Input & Integer'Image (Length) & ':');
   begin -- On_Generate
      App.Domain.Value (Value => Domain);

      if App.Symbol.Value /= No_Symbol then
         Length := Length - 1;
      end if;
      -- Length is now the number of characters from Result to include in the password, 7 .. 16

      Find_Digit_1 : for I in 1 .. Length loop
         if Result (I) in Digit then
            Digit_1_Index := I;
            Digit_1 := Integer'Value (Result (I .. I) );

            exit Find_Digit_1;
         end if;
      end loop Find_Digit_1;

      if Digit_1_Index = 0 then -- Needs a digit
         Digit_1_Index := 1;
         Digit_1 := 0;
         Result (1) := '0';
      end if;

      Digit_1_Rem := Digit_1 rem 2;

      Lower_Some   : for I in 1 .. Length loop
         if Result (I) in Upper then
            Letter_Count := Letter_Count + 1;

            if Letter_Count rem 2 = Digit_1_Rem then
               Result (I) := Ada.Characters.Handling.To_Lower (Result (I) );
            end if;
         end if;
      end loop Lower_Some;

      Check_Letters : for I in 1 .. Length loop
         if Result (I) in Lower then
            Has_Lower := True;
         end if;

         if Result (I) in Upper then
            Has_Upper := True;
         end if;
      end loop Check_Letters;

      -- Length >= 7, so if not Has_Lower, then Result (1 .. Length) has at most 1 letter, which was not lowered by Lower_Some
      -- if not Has_Upper, then Result (1 .. Length) has at most 1 letter, which was lowered by Lower_Some
      -- if both not Has_Lower and not Has_Upper, then Result (1 .. Length) is all digits
      -- In all of these cases, there are digits afer Digit_1_Index that can be replaced by the desired kind of letter

      if not Has_Lower then -- Needs a lower-case letter
         Find_Digit_Lower : for I in Digit_1_Index + 1 .. Length loop
            if Result (I) in Digit then
               Result (I) := 'a';

               exit Find_Digit_Lower;
            end if;
         end loop Find_Digit_Lower;
      end if;

      if not Has_Upper then -- Needs an upper-case letter
         Find_Digit_Upper : for I in Digit_1_Index + 1 .. Length loop
            if Result (I) in Digit then
               Result (I) := 'A';

               exit Find_Digit_Upper;
            end if;
         end loop Find_Digit_Upper;
      end if;

      if App.Symbol.Value = No_Symbol then
         App.Result.Value (Value => Result (1 .. Length) );

         return;
      end if;

      if App.Symbol.Value = Auto_Symbol then
         Symbol (1) := Symbol_Set (Digit_1);
      else
         Symbol := App.Symbol.Value;
      end if;

      App.Result.Value (Value => Result (1 .. Integer'Min (Digit_1, Length) ) & Symbol & Result (Digit_1 + 1 .. Length) );
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
      App : App_Ptr := new App_Info;
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
      App.Symbol.Add_Option (Value => No_Symbol, Text => No_Symbol);
      App.Symbol.Add_Option (Value => Auto_Symbol, Text => Auto_Symbol, Selected => True);

      All_Symbols : for I in Symbol_Set'Range loop
         App.Symbol.Add_Option (Value => (1 => Symbol_Set (I) ), Text => (1 => Symbol_Set (I) ) );
      end loop All_Symbols;

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
