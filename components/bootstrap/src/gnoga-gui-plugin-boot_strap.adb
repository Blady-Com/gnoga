------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--           G N O G A . G U I . P L U G I N S . B O O T _ S T R A P        --
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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

package body Gnoga.Gui.Plugin.Boot_Strap is

   ---------------------
   -- Load_Boot_Strap --
   ---------------------

   procedure Load_Boot_Strap
     (Window : in out Gnoga.Gui.Window.Window_Type'Class)
   is
   begin
      Window.Document.Head_Element.jQuery_Execute
        ("append ('" & Escape_Quotes ("<link rel='stylesheet' " &
           "href='https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/" &
           "css/bootstrap.min.css' />\'") & "'");
      Window.Document.Head_Element.jQuery_Execute
        ("append ('" & Escape_Quotes ("<link rel='stylesheet' " &
           "href='https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/" &
           "css/bootstrap-theme.min.css' />") & "')");
      Window.Document.Head_Element.jQuery_Execute
        ("append ('" & Escape_Quotes ("<script src='https://maxcdn.bootstrapcdn.com/" &
           "bootstrap/3.2.0/js/bootstrap.min.js'></script>") & "')");
   end Load_Boot_Strap;

   ------------
   -- Create --
   ------------

   procedure Create
     (Container : in out Container_Type;
      Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
      ID        : in     String  := "")
   is
   begin
      Container.Create_From_HTML (Parent, Escape_Quotes ("<div class='container' />"), ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Container : in out Fluid_Container_Type;
      Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
      ID        : in     String  := "")
   is
   begin
      Container.Create_From_HTML
        (Parent, Escape_Quotes ("<div class='container-fluid' />"), ID);
   end Create;

   --------------------
   -- Make_Container --
   --------------------

   procedure Make_Container
     (View : in out Gnoga.Gui.View.View_Base_Type'Class)
   is
   begin
      View.Add_Class ("container");
   end Make_Container;

   --------------------------
   -- Make_Fluid_Container --
   --------------------------

   procedure Make_Fluid_Container
     (View : in out Gnoga.Gui.View.View_Base_Type'Class)
   is
   begin
      View.Add_Class ("container-fluid");
   end Make_Fluid_Container;

   ------------
   -- Create --
   ------------

   procedure Create
     (Row    : in out Row_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String  := "")
   is
   begin
      Row.Create_From_HTML (Parent, Escape_Quotes ("<div class='row' />"), ID);
   end Create;

   -----------------
   -- Set_Columns --
   -----------------

   procedure Set_Columns (View   : in out Gnoga.Gui.View.View_Base_Type'Class;
                          Number : in     Natural;
                          Device : in     Device_Type := Small)
   is
      function To_String (Device : Device_Type) return String;

      function To_String (Device : Device_Type) return String is
      begin
         if Device = Extra_Small then
            return "col-xs-";
         elsif Device = Small then
            return "col-sm-";
         elsif Device = Medium then
            return "col-md-";
         else
            return "col-lg-";
         end if;
      end To_String;
   begin
      View.Add_Class (To_String (Device) & Left_Trim (Number'Img));
   end Set_Columns;

   ------------
   -- Create --
   ------------

   procedure Create
     (Jumbotron : in out Jumbotron_Type;
      Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
      Content   : in     String  := "";
      ID        : in     String  := "")
   is
   begin
      Jumbotron.Create_From_HTML (Parent,
                                  Escape_Quotes ("<div class='jumbotron'>" &
                                    Content & "</div>"),
                                  ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Element   : in out Table_Type;
      Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
      Striped   : in     Boolean := True;
      Bordered  : in     Boolean := True;
      Condensed : in     Boolean := True;
      ID        : in     String  := "")
   is
   begin
      Element.Create_From_HTML (Parent, Escape_Quotes ("<div class='table-responsive'/>"), ID);
      Element.Table.Create (Element);
      Element.Table.Add_Class ("table");

      if Striped then
         Element.Table.Add_Class ("table-striped");
      end if;

      if Bordered then
         Element.Table.Add_Class ("table-bordered");
      end if;

      if Condensed then
         Element.Table.Add_Class ("table-condensed");
      end if;
   end Create;

   -------------------
   -- Put_Glyphicon --
   -------------------

   procedure Put_Glyphicon
     (View : in out Gnoga.Gui.View.View_Base_Type'Class;
      Icon : in     String)
   is
   begin
      View.Put_HTML ("<span class='glyphicon glyphicon-" & Icon & "' />");
   end Put_Glyphicon;

   ------------
   -- Create --
   ------------

   procedure Create
     (Form_Group : in out Form_Group_Type;
      Parent     : in out Gnoga.Gui.Base.Base_Type'Class;
      ID         : in     String  := "")
   is
   begin
      Form_Group.Create_From_HTML (Parent, Escape_Quotes ("<div class='form-group' />"), ID);
   end Create;

   procedure Make_Boot_Strap_Form_Item
     (Element : in out Gnoga.Gui.Element.Form.Form_Element_Type'Class)
   is
   begin
      Element.Add_Class ("form-control");
   end Make_Boot_Strap_Form_Item;

   --------------------
   -- Add_Help_Block --
   --------------------

   procedure Add_Help_Block
     (Element : in out Gnoga.Gui.Element.Form.Form_Element_Type'Class;
      Text    : in     String;
      ID      : in     String := "")
   is
      Dummy_H : Gnoga.Gui.Element.Element_Type;
   begin
      Dummy_H.Create_From_HTML (Parent => Element.Parent.all,
                          HTML   => Escape_Quotes ("<span class='help-block'>" &
                            Text & "</span>"),
                          ID     => ID);
   end Add_Help_Block;

   ------------
   -- Create --
   ------------

   procedure Create
     (Element : in out Check_Box_Type;
      Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Checked : in     Boolean := False;
      Value   : in     String := "";
      Name    : in     String := "";
      ID      : in     String := "")
   is
   begin
      Element.Create_From_HTML (Form, Escape_Quotes ("<div class='checkbox'/>"), ID);
      Element.Box.Create (Form, Checked, Value, Name, ID);
      Element.Box.Place_Inside_Top_Of (Element);
   end Create;

   procedure Disabled (Element : in out Check_Box_Type;
                       Value   : in     Boolean := True)
   is
   begin
      Element.Box.Disabled (Value);

      if Value then
         Element.Add_Class ("disabled");
      else
         Element.Remove_Class ("disabled");
      end if;
   end Disabled;

   ------------
   -- Create --
   ------------

   procedure Create
     (Element : in out Radio_Button_Type;
      Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Checked : in     Boolean := False;
      Value   : in     String := "";
      Name    : in     String := "";
      ID      : in     String := "")
   is
   begin
      Element.Create_From_HTML (Form, Escape_Quotes ("<div class='radio'/>"), ID);
      Element.Radio.Create (Form, Checked, Value, Name, ID);
      Element.Radio.Place_Inside_Top_Of (Element);
   end Create;

   --------------
   -- Disabled --
   --------------

   procedure Disabled (Element : in out Radio_Button_Type;
                       Value   : in     Boolean := True)
   is
   begin
      Element.Radio.Disabled (Value);

      if Value then
         Element.Add_Class ("disabled");
      else
         Element.Remove_Class ("disabled");
      end if;
   end Disabled;
end Gnoga.Gui.Plugin.Boot_Strap;
