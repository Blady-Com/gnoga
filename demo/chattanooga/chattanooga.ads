-- Chattanooga: a simple chat program
-- Copyright (C) 2015 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Globally useful declarations
--
-- V1.0B  2015 Jan 15     1st beta release, now with autoscrolling and dings
--
with Ada.Strings.Unbounded;

with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Multimedia;
with Gnoga.Gui.View.Docker;
with Gnoga.Gui.Window;
with Gnoga.Types;

package Chattanooga is
   use Ada.Strings.Unbounded;

   type App_Info is new Gnoga.Types.Connection_Data_Type with record
      Email  : Unbounded_String;
      Window : Gnoga.Gui.Window.Pointer_To_Window_Class;
      View   : Gnoga.Gui.View.View_Type;

      Email_Form      : Gnoga.Gui.Element.Form.Form_Type; -- Connection screen
      Email_Entry     : Gnoga.Gui.Element.Form.Email_Type;
      Connect_Message : Gnoga.Gui.Element.Form.Label_Type;
      Connect_Button  : Gnoga.Gui.Element.Form.Submit_Button_Type;
      Error           : Gnoga.Gui.Element.Common.Span_Type;
      Connect_Help    : Gnoga.Gui.Element.Common.Button_Type;

      Dock          : Gnoga.Gui.View.Docker.Docker_View_Type;
      Chat          : aliased Gnoga.Gui.Element.Form.Form_Type; -- Chat side
      Chat_Title    : Gnoga.Gui.Element.Common.Span_Type;
      Connect_Info  : Gnoga.Gui.Element.Common.Span_Type;
      Disconnect    : Gnoga.Gui.Element.Common.Button_Type;
      Messaging     : Gnoga.Gui.Element.Form.Text_Area_Type;
      Message_Entry : Gnoga.Gui.Element.Form.Text_Area_Type;
      Send          : Gnoga.Gui.Element.Form.Submit_Button_Type;
      Friends       : aliased Gnoga.Gui.View.View_Type; -- Friends side
      Friend_Title  : Gnoga.Gui.Element.Common.Span_Type;
      Friend_Form   : Gnoga.Gui.Element.Form.Form_Type;
      Friend_Entry  : Gnoga.Gui.Element.Form.Email_Type;
      Friend_Label  : Gnoga.Gui.Element.Form.Label_Type;
      Add           : Gnoga.Gui.Element.Form.Submit_Button_Type;
      List_Form     : Gnoga.Gui.Element.Form.Form_Type;
      List          : Gnoga.Gui.Element.Form.Selection_Type;
      Remove        : Gnoga.Gui.Element.Form.Submit_Button_Type;
      Explanation   : Gnoga.Gui.Element.Common.Span_Type;
      Chat_Help     : Gnoga.Gui.Element.Common.Button_Type;
      Ding          : Gnoga.Gui.Element.Multimedia.Audio_Type;
   end record;

   type App_Ptr is access all App_Info;

   function "+" (Right : String) return Unbounded_String renames To_Unbounded_String;
   function "+" (Right : Unbounded_String) return String renames To_String;
end Chattanooga;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
