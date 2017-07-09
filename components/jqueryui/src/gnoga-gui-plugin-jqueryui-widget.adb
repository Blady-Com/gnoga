------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--     G N O G A . G U I . P L U G I N . J Q U E R Y U I . W I D G E T      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
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
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

package body Gnoga.Gui.Plugin.jQueryUI.Widget is
   use type Gnoga.Gui.Base.Action_Event;

   -------------
   --  Create --
   -------------

   overriding
   procedure Create
     (View    : in out Accordion_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      ID      : in     String  := "")
   is
   begin
      Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
   end Create;

   --------------------
   -- Create_Section --
   --------------------

   procedure Create_Section
     (View : in out Accordion_Type; Heading : String)
   is
   begin
      View.Put_HTML ("<h3>" & Escape_Quotes (Heading) & "</h3>");
   end Create_Section;

   ----------------------
   -- Render_Accordion --
   ----------------------

   procedure Render_Accordion
     (View           : in out Accordion_Type;
      Allow_Collapse : in     Boolean := False)
   is
      function params return String;

      function params return String is
      begin
         if Allow_Collapse = False then
            return "";
         else
            return "{ collapsible: true }";
         end if;
      end params;
   begin
      View.jQuery_Execute ("accordion(" & params & ")");
   end Render_Accordion;

   -----------------
   -- Make_Button --
   -----------------

   procedure Make_Button
     (Element    : in out Gnoga.Gui.Element.Element_Type'Class;
      Left_Icon  : in     String  := "";
      Right_Icon : in     String  := "";
      No_Text    : in     Boolean := False)
   is
      function params return String;

      function params return String is
         function Is_No_Text return String;
         function Is_Icon return String;

         function Is_No_Text return String is
         begin
            if No_Text then
               return "false";
            else
               return "true";
            end if;
         end Is_No_Text;

         function Is_Icon return String is
         begin
            if Left_Icon = "" and Right_Icon = "" then
               return "";
            else
               return ", icons: { primary: '" & Left_Icon & "'," &
                 "secondary: '" & Right_Icon & "'}";
            end if;
         end Is_Icon;
      begin
         return "{ text: " & Is_No_Text & Is_Icon & "}";
      end params;
   begin
      Element.jQuery_Execute ("button(" & Escape_Quotes (params) & ")");
   end Make_Button;

   ---------------------
   -- Make_Button_Set --
   ---------------------

   procedure Make_Button_Set
     (View : in out Gnoga.Gui.View.View_Base_Type'Class)
   is
   begin
      View.jQuery_Execute ("buttonset()");
   end Make_Button_Set;

   ------------
   -- Create --
   ------------

   procedure Create
     (Dialog          : in out Dialog_Type;
      Parent          : in out Gnoga.Gui.Base.Base_Type'Class;
      Title           : in     String;
      Content         : in     String  := "";
      Height          : in     Natural := 0;
      Width           : in     Natural := 0;
      Position_My     : in     String  := "center";
      Position_At     : in     String  := "center";
      Resizable       : in     Boolean := False;
      Minimum_Height  : in     Natural := 150;
      Minimum_Width   : in     Natural := 150;
      Maximum_Height  : in     Natural := 0;
      Maximum_Width   : in     Natural := 0;
      Modal           : in     Boolean := True;
      Close_On_Escape : in     Boolean := True;
      Draggable       : in     Boolean := True;
      ID              : in     String  := "")
   is
      function Is_Auto (N : Natural) return String;
      function Is_False (N : Natural) return String;

      function Is_Auto (N : Natural) return String is
      begin
         if N = 0 then
            return " 'auto'";
         else
            return N'Img;
         end if;
      end Is_Auto;

      function Is_False (N : Natural) return String is
      begin
         if N = 0 then
            return " 'false'";
         else
            return N'Img;
         end if;
      end Is_False;
   begin
      Dialog.Auto_Place (False);

      Dialog.Create_From_HTML
        (Parent, "<div>" & Escape_Quotes (Content) & "</div>", ID);

      Dialog.jQuery_Execute
        ("dialog({title: '" & Title & "'," &
           "height:" & Is_Auto (Height) & "," &
           "width:" & Is_Auto (Width) & "," &
           "position: { my: '" & Position_My & "', at: '" &
           Position_At & "', of: window }," &
           "resizable: " & Resizable'Img & "," &
           "minHeight:" & Minimum_Height'Img & "," &
           "minWidth:" & Minimum_Width'Img & "," &
           "height:" & Is_Auto (Height) & "," &
           "width:" & Is_Auto (Width) & "," &
           "maxHeight:" & Is_False (Maximum_Height) & "," &
           "maxWidth:" & Is_False (Maximum_Width) & "," &
           "modal: " & Modal'Img & "," &
           "closeOnEscape: " & Close_On_Escape'Img & "," &
           "draggable: " & Draggable'Img & "})");

      Dialog.Bind_Event (Event   => "dialogresizestop",
                         Message => "");
   end Create;

   ----------
   -- Open --
   ----------

   procedure Open (Dialog : in out Dialog_Type) is
   begin
      Dialog.jQuery_Execute ("dialog('open')");
      Dialog.Fire_On_Open;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Dialog : in out Dialog_Type) is
   begin
      Dialog.jQuery_Execute ("dialog('close')");
   end Close;

   -----------------
   -- Move_To_Top --
   -----------------

   procedure Move_To_Top (Dialog : in out Dialog_Type) is
   begin
      Dialog.jQuery_Execute ("dialog('moveToTop')");
   end Move_To_Top;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Dialog : in out Dialog_Type) return Boolean is
   begin
      return Dialog.jQuery_Execute ("dialog('isOpen')") = "true";
   end Is_Open;

   -------------
   -- On_Open --
   -------------

   procedure On_Open_Handler (Dialog  : in out Dialog_Type;
                              Handler : in     Gnoga.Gui.Base.Action_Event)
   is
   begin
      Dialog.On_Open_Event := Handler;
   end On_Open_Handler;

   procedure Fire_On_Open (Dialog : in out Dialog_Type) is
   begin
      if Dialog.On_Open_Event /= null then
         Dialog.On_Open_Event (Dialog);
      end if;
   end Fire_On_Open;

   --------------
   -- On_Close --
   --------------

   procedure On_Close_Handler (Dialog  : in out Dialog_Type;
                               Handler : in     Gnoga.Gui.Base.Action_Event)
   is
   begin
      if Dialog.On_Close_Event /= null then
         Dialog.Unbind_Event ("dialogclose");
      end if;

      Dialog.On_Close_Event := Handler;

      if Handler /= null then
         Dialog.Bind_Event (Event   => "dialogclose",
                            Message => "");
      end if;
   end On_Close_Handler;

   procedure Fire_On_Close (Dialog : in out Dialog_Type) is
   begin
      if Dialog.On_Close_Event /= null then
         Dialog.On_Close_Event (Dialog);
      end if;
   end Fire_On_Close;

   ----------------
   -- On_Message --
   ----------------

   overriding
   procedure On_Message (Object  : in out Dialog_Type;
                         Event   : in     String;
                         Message : in     String)
   is
   begin
      if Event = "dialogclose" then
         Object.Fire_On_Close;
      elsif Event = "dialogresizestop" then
         Dialog_Type'Class (Object).On_Resize;
      else
         Gnoga.Gui.Base.Base_Type (Object).On_Message (Event, Message);
      end if;
   end On_Message;

   ---------------
   -- Make_Menu --
   ---------------

   procedure Make_Menu
     (List : in out Gnoga.Gui.Element.List.Unordered_List_Type'Class)
   is
   begin
      List.jQuery_Execute ("menu()");
   end Make_Menu;

   ------------
   -- Create --
   ------------

   procedure Create (Progress_Bar : in out Progress_Bar_Type;
                     Parent       : in out Gnoga.Gui.Base.Base_Type'Class;
                     Value        : in     Integer := 0;
                     Maximum      : in     Integer := 100;
                     ID           : in     String := "")
   is
      pragma Unreferenced (ID);
   begin
      Progress_Bar.Create_From_HTML (Parent, "<div/>");
      Progress_Bar.jQuery_Execute
        ("progressbar ({ max:" & Maximum'Img & ", value:" & Value'Img & "})");
   end Create;

   -----------
   -- Value --
   -----------

   procedure Value (Progress_Bar : in out Progress_Bar_Type;
                    Value        : in     Integer)
   is
   begin
      Progress_Bar.jQuery_Execute
        ("progressbar ('option', 'value'," & Value'Img & ")");
   end Value;

   function Value (Progress_Bar : Progress_Bar_Type) return Integer is
   begin
      return Progress_Bar.jQuery_Execute ("progressbar ('option', 'value')");
   end Value;

   ----------------------
   -- Make_Select_Menu --
   ----------------------

   procedure Make_Select_Menu
     (Element : in out Gnoga.Gui.Element.Form.Selection_Type'Class) is
   begin
      Element.jQuery_Execute ("selectmenu()");
   end Make_Select_Menu;

   ------------
   -- Create --
   ------------

   procedure Create (Tabs         : in out Tabs_Type;
                     Parent       : in out Gnoga.Gui.Base.Base_Type'Class;
                     ID           : in     String       := "")
   is
   begin
      Tabs.Create_From_HTML (Parent, "<div />", ID);
      Tabs.Labels.Create_From_HTML (Tabs, "<ul />");
   end Create;

   -------------
   -- Add_Tab --
   -------------

   procedure Add_Tab (Tabs  : in out Tabs_Type;
                      Label : in     String;
                      View  : in out Gui.View.View_Base_Type'Class)
   is
      L : Gnoga.Gui.Element.Element_Type;
   begin
      L.Create_From_HTML (Tabs.Labels,
                          Escape_Quotes ("<li><a href='" & View.DOM_Selector & "'>" &
                            Label &
                            "</a></li>"));
      L.Place_Inside_Bottom_Of (Tabs.Labels);
      View.Place_Inside_Bottom_Of (Tabs);
   end Add_Tab;

   -----------------
   -- Render_Tabs --
   -----------------

   procedure Render_Tabs (Tabs : in out Tabs_Type) is
   begin
      Tabs.jQuery_Execute ("tabs ({heightStyle: 'auto'})");
   end Render_Tabs;

   -----------------------
   -- Turn_On_Tool_Tips --
   -----------------------

   procedure Turn_On_Tool_Tips
     (Window : in out Gnoga.Gui.Window.Window_Type'Class)
   is
   begin
      Window.Document.jQuery_Execute ("tooltip()");
   end Turn_On_Tool_Tips;

   ------------------
   -- Add_Tool_Tip --
   ------------------

   procedure Add_Tool_Tip
     (Element : in out Gnoga.Gui.Element.Element_Type'Class;
      Tip     : in     String)
   is
   begin
      Element.Attribute ("title", Tip);
   end Add_Tool_Tip;

end Gnoga.Gui.Plugin.jQueryUI.Widget;
