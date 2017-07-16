-- Chattanooga: a simple chat program
-- Copyright (C) 2017 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- User Interface
--
-- V1.3B  2017 Jul 15     2017 Feb 15 fix caused problems; this is intended to fix it correctly
-- V1.2B  2017 Feb 15     Corrected error in On_Connect_Submit
-- V1.1B  2015 Jul 01     New version of Gnoga.Types.Colors
-- V1.0B  2015 Jan 30     1st beta release, now with limited messaging area
--
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Strings.Fixed;

with Chattanooga.DB;

with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Types.Colors;

package body Chattanooga.UI is
   procedure Create_Email_Screen (App : in App_Ptr; Main_Window : in out Gnoga.Gui.Window.Window_Type'Class);
   -- Initialize the window for entering the user's e-mail address

   procedure Create_Chat_Screen (App : in App_Ptr);
   -- Switch to the screen for chatting

   procedure On_Connect (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
                         Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Connect_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Connect_Help (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Send (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Add (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Remove (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Disconnect (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Chat_Help (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   LF : constant Character := Ada.Characters.Latin_1.LF;

   procedure Show (From : in Unbounded_String; Message : in String; App_Data : in App_Ptr; Ding : in Boolean := True) is
      -- Empty declarative part
   begin -- Show
      App_Data.Messaging.Value (Value => App_Data.Messaging.Value & LF & (+From) & ": " & Message);
      App_Data.Messaging.Scroll_Top (Value => Integer'Last);

      if Ding then
         App_Data.Ding.Play;
      end if;
   exception -- Show
   when E : others =>
      Gnoga.Log (Message => "Show: " & Ada.Exceptions.Exception_Information (E) );
   end Show;

   Star_Suffix : constant String := " *";

   procedure New_Friend (Friend : in Unbounded_String; App_Data : in App_Ptr; Connected : in Boolean) is
      Display : Unbounded_String := Friend;
   begin -- New_Friend
      if Connected then
         Append (Source => Display, New_Item => Star_Suffix);
      end if;

      App_Data.List.Add_Option (Value => +Friend, Text => +Display);
   end New_Friend;

   procedure Remove_Friend (Friend : in Unbounded_String; App_Data : in App_Ptr) is
      -- Empty declarative part
   begin -- Remove_Friend
      Find : for Index in 1 .. App_Data.List.Length loop
         if App_Data.List.Value (Index) = Friend then
            App_Data.List.Remove_Option (Index => Index);

            return;
         end if;
      end loop Find;
   end Remove_Friend;

   procedure Change_Status (Friend : in Unbounded_String; App_Data : in App_Ptr; Connected : in Boolean) is
      Display : Unbounded_String := Friend;
   begin -- Change_Status
      if Connected then
         Append (Source => Display, New_Item => Star_Suffix);
      end if;

      Find : for Index in 1 .. App_Data.List.Length loop
         if App_Data.List.Value (Index) = Friend then
            App_Data.List.Text (Index => Index, Value => +Display);
         end if;
      end loop Find;
   end Change_Status;

   procedure Create_Email_Screen (App : in App_Ptr; Main_Window : in out Gnoga.Gui.Window.Window_Type'Class) is
      -- Empty declarative part
   begin -- Create_Email_Screen
      App.Window := Main_Window'Unchecked_Access;
      App.View.Create (Parent => Main_Window);
      App.Email_Form.Create (Parent => App.View);
      App.Email_Entry.Create (Form => App.Email_Form);
      App.Email_Entry.Display (Value => "inline");
      App.Email_Entry.Required;
      App.Email_Entry.Place_Holder (Value => "E-mail address");
      App.Email_Entry.Multiple_Emails (Value => False);
      App.Connect_Message.Create (Form => App.Email_Form, Label_For => App.Email_Entry, Content => "Enter your e-mail address:");
      App.Connect_Button.Create (Form => App.Email_Form, Value => "Connect");
      App.Email_Form.New_Line;
      App.Error.Create (Parent => App.Email_Form, Content => "");
      App.Error.Color (Value => Gnoga.Types.Colors.To_String (Gnoga.Types.Colors.Red) );
      App.Error.Display (Value => "block");
      App.Email_Form.New_Line;
      App.Email_Form.New_Line;
      App.Connect_Help.Create (Parent => App.Email_Form, Content => "Help");
      App.Connect_Help.On_Click_Handler (Handler => On_Connect_Help'Access);
      App.Email_Form.On_Submit_Handler (Handler => On_Connect_Submit'Access);
   end Create_Email_Screen;

   procedure Create_Chat_Screen (App : in App_Ptr) is
      -- Empty declarative part
   begin -- Create_Chat_Screen
      App.Email_Form.Remove;

      App.Dock.Create (Parent => App.View);
      App.Chat.Create (Parent => App.Dock);
      App.Dock.Left_Dock (Dock => App.Chat'Access);
      App.Chat_Title.Create (Parent => App.Chat, Content => "Chat");
      App.Chat_Title.Text_Alignment (Value => Gnoga.Gui.Element.Center);
      App.Chat_Title.Display (Value => "block");
      App.Connect_Info.Create (Parent => App.Chat, Content => "You are connected as " & (+App.Email) );
      App.Disconnect.Create (Parent => App.Chat, Content => "Disconnect");
      App.Disconnect.Place_After (Target => App.Connect_Info);
      App.Disconnect.On_Click_Handler (Handler => On_Disconnect'Access);
      App.Chat.New_Line;
      App.Messaging.Create (Form => App.Chat, Columns => 75, Rows => 35);
      App.Messaging.Read_Only;
      App.Chat.New_Line;
      App.Message_Entry.Create (Form => App.Chat, Columns => 65, Rows => 3);
      App.Message_Entry.Display (Value => "inline");
      App.Send.Create (Form => App.Chat, Value => "Send");
      App.Chat.New_Line;
      App.Chat.On_Submit_Handler (Handler => On_Send'Access);

      App.Friends.Create (Parent => App.Dock);
      App.Dock.Right_Dock (Dock => App.Friends'Access);
      App.Friend_Title.Create (Parent => App.Friends, Content => "Friends");
      App.Friend_Title.Text_Alignment (Value => Gnoga.Gui.Element.Center);
      App.Friend_Title.Display (Value => "block");
      App.Friend_Form.Create (Parent => App.Friends);
      App.Friend_Entry.Create (Form => App.Friend_Form);
      App.Friend_Entry.Display (Value => "inline");
      App.Friend_Label.Create (Form => App.Friend_Form, Label_For => App.Friend_Entry, Content => "Friend's e-mail:");
      App.Add.Create (Form => App.Friend_Form, Value => "Add");
      App.Friend_Form.On_Submit_Handler (Handler => On_Add'Access);
      App.List_Form.Create (Parent => App.Friends);
      App.List.Create (Form => App.List_Form, Visible_Lines => 10);
      App.List.Display (Value => "inline");
      App.Remove.Create (Form => App.List_Form, Value => "Remove");
      App.List_Form.New_Line;
      App.Explanation.Create (Parent => App.List_Form, Content => "* These friends are connected");
      App.List_Form.New_Line;
      App.List_Form.New_Line;
      App.Chat_Help.Create (Parent => App.List_Form, Content => "Help");
      App.Chat_Help.Display (Value => "block");
      App.Chat_Help.On_Click_Handler (Handler => On_Chat_Help'Access);
      App.Ding.Create (Parent => App.List_Form, Source => "glass.ogg", Controls => False, Preload =>  True);
      App.Ding.Hidden;
      App.List_Form.On_Submit_Handler (Handler => On_Remove'Access);
      App.Window.Document.Title (Value => "Chattanooga - " & (+App.Email) );
   end Create_Chat_Screen;

   procedure On_Connect (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
                         Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App : App_Ptr := new App_Info;
   begin -- On_Connect
      Main_Window.Connection_Data (Data => App);
      Create_Email_Screen (App => App, Main_Window => Main_Window);

      Connection.Hold;
      DB.Remove (User => App.Email);
   exception -- On_Connect
   when E : others =>
      Gnoga.Log (Message => "On_Connect: " & Ada.Exceptions.Exception_Information (E) );
   end On_Connect;

   procedure On_Connect_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);

      Email : constant String :=
         Ada.Strings.Fixed.Trim (Ada.Characters.Handling.To_Lower (App.Email_Entry.Value), Ada.Strings.Both);
   begin -- On_Connect_Submit
      if Email = "" then

        return;
      end if;

      if Db.Exists (+Email) then
         App.Error.Text (Value => Email & " is already connected. Try again.");

        return;
      end if;

      App.Email := +Email;
      Create_Chat_Screen (App => App);
      DB.Add (User => App.Email, App_Data => App);
   exception -- Add
   when Constraint_Error =>
      App.Error.Text (Value => Email & " is already connected. Try again.");
   when E : others =>
      Gnoga.Log (Message => "On_Connect_Submit: " & Ada.Exceptions.Exception_Information (E) );
   end On_Connect_Submit;

   procedure On_Connect_Help (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);
   begin -- On_Connect_Help
      App.Window.Alert (Message => "Here you connect to Chattanooga. Your e-mail address is used because it's unique. " &
                                   "Chattanooga does not permanently store any information about you. E-mail addresses, " &
                                   "being case-insensitive, are converted to all lower case. Once connected, you " &
                                   "will be taken to the chat screen.");
   exception -- On_Connect_Help
   when E : others =>
      Gnoga.Log (Message => "On_Connect_Help: " & Ada.Exceptions.Exception_Information (E) );
   end On_Connect_Help;

   procedure On_Send (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);

      Count : Natural;
   begin -- On_Send
      Get_Message : declare
         Message : constant String := App.Message_Entry.Value;
      begin -- Get_Message
         if Message = "" then
            return;
         end if;
         Count := DB.Send (From => App.Email, Message => Message);
         Show (From => App.Email, Message => Message, App_Data => App, Ding => False);
      end Get_Message;

      App.Message_Entry.Value (Value => "");
   exception -- On_Send
   when E : others =>
      Gnoga.Log (Message => "On_Send: " & Ada.Exceptions.Exception_Information (E) );
   end On_Send;

   procedure On_Add (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);

      Friend : constant String :=
         Ada.Strings.Fixed.Trim (Ada.Characters.Handling.To_Lower (App.Friend_Entry.Value), Ada.Strings.Both);
   begin -- On_Add
      if Friend = "" or Friend = App.Email then
         App.Friend_Entry.Value (Value => "");

         return;
      end if;

      DB.Add_Friend (User => App.Email, Friend => +Friend);
      App.Friend_Entry.Value (Value => "");
   exception -- On_Add
   when E : others =>
      Gnoga.Log (Message => "On_Add: " & Ada.Exceptions.Exception_Information (E) );
   end On_Add;

   procedure On_Remove (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);

      Index : constant Natural := App.List.Selected_Index;
   begin -- On_Remove
      if Index > 0 then
         Get_Name : declare
            Name : constant String := App.List.Value (Index);
         begin -- Get_Name
            DB.Remove_Friend (User => App.Email, Friend => +Name);
         end Get_Name;
      end if;
   exception -- On_Remove
   when E : others =>
      Gnoga.Log (Message => "On_Remove: " & Ada.Exceptions.Exception_Information (E) );
   end On_Remove;

   End_Message : constant String := "Chattanooga ended.";

   procedure On_Disconnect (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);

      View : Gnoga.Gui.View.View_Type;
   begin -- On_Disconnect
      DB.Remove (User => App.Email);
      App.Dock.Remove;
      View.Create (Parent => App.Window.all);
      View.Put_Line (Message => End_Message);
      App.Window.Document.Title (Value => "Chattanooga");
      App.Window.Close;
      App.Window.Close_Connection;
   exception -- On_Disconnect
   when E : others =>
      Gnoga.Log (Message => "On_Disconnect: " & Ada.Exceptions.Exception_Information (E) );
   end On_Disconnect;

   procedure On_Chat_Help (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);
   begin -- On_Chat_Help
      App.Window.Alert (Message => "Here you can chat with any of your friends who are also connected. If any friends " &
                                   "connected before you did and added you as a friend, they will show up in your friend list " &
                                   "on the right. You can add friends using the box in the upper right of this screen and " &
                                   "clicking on the Add button; you will also be added as a friend to any connected friends " &
                                   "you add. You can remove a friend from your friend list by highlighting the friend and " &
                                   "clicking on the Remove button; if the friend is connected, this will remove you from your " &
                                   "friend's list as well." &
                                   LF & LF &
                                   "You chat by typing your messages in the box at the " &
                                   "bottom left and clicking on the Send button. This sends your message to any connected " &
                                   "friends. Messages are not queued and will not be shown to any friends who connect later. " &
                                   "Your messages and those sent by your friends will appear in the chat box on the left. " &
                                   LF & LF &
                                   "The Disconnect button will disconnect you from Chattanooga. You will then appear as " &
                                   "disconnected in your friends' lists.");
   exception -- On_Chat_Help
   when E : others =>
      Gnoga.Log (Message => "On_Chat_Help: " & Ada.Exceptions.Exception_Information (E) );
   end On_Chat_Help;
begin -- Chattanooga.UI
   Gnoga.Application.Title (Name => "Chattanooga");
   Gnoga.Application.HTML_On_Close (HTML => End_Message);
   Gnoga.Application.Multi_Connect.Initialize;
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Access);
   Gnoga.Application.Multi_Connect.Message_Loop;
exception -- Chattanooga.UI
when E : others =>
   Gnoga.Log (Message => "UI: " & Ada.Exceptions.Exception_Information (E) );
end Chattanooga.UI;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.

