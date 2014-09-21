------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . E L E M E N T . F O R M                   --
--                                                                          --
--                                 S p e c                                  --
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
------------------------------------------------------------------------------                                                                          --

package Gnoga.Element.Form is

   -------------------------------------------------------------------------
   --  Form_Types
   -------------------------------------------------------------------------

   type Form_Type is new Gnoga.Element.Element_Type with private;
   type Form_Access is access all Form_Type;
   type Pointer_To_Botton_Class is access all Form_Type'Class;

   -------------------------------------------------------------------------
   --  Form_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Form    : in out Form_Type;
                     Parent  : in out Gnoga.Base.Base_Type'Class;
                     ID      : in     String := "");
   --  Create a Form element. This is used to group and set the action
   --  taken on a submit of the form.
   --
   --  In Gnoga forms in general should be processed by handling the
   --  On_Submit event and accessing each element's value directly.
   --  However it is possible to set an action (URL for form to submit to)
   --  and that page can process the results. Note that the POST method
   --  for forms does not work wih Gnoga currently. For the GET method
   --  the results can be accessed via Gnoga.Window.Location.Search or
   --  using Gnoga.Window.Connections.Search_Parameters.

   -------------------------------------------------------------------------
   --  From_Type - Properties
   -------------------------------------------------------------------------

   procedure Action (Form : in out Form_Type; Value : in String);
   function Action (Form : Form_Type) return String;
   -- URL to submit form to using Method

   type Form_Method_Type is (Get, Post);

   procedure Method (Form : in out Form_Type; Value : in Form_Method_Type);
   function Method (Form : Form_Type) return Form_Method_Type;

   type Encoding_Type is (URL_Encode, Multi_Part, Text);
   -- application/x-www-form-urlencoded / multipart/form-data / text/plain
   -- The default for forms is URL_Encode

   procedure Encoding (Form : in out Form_Type; Value : in Encoding_Type);
   function Encoding (Form : Form_Type) return  Encoding_Type;
   --  Encoding affects how the form is send via the POST method only.

   procedure Auto_Complete (Form  : in out Form_Type;
                            Value : in  Boolean := True);
   function Auto_Complete (Form : Form_Type) return Boolean;

   procedure Validate_On_Submit (Form  : in out Form_Type;
                                 Value : in  Boolean := True);
   function Validate_On_Submit (Form : Form_Type) return Boolean;

   function Number_Of_Elements_In_Form (Form : Form_Type) return Integer;

   procedure Target (Form : in out Form_Type; Value : in String);
   function Target (Form : Form_Type) return String;
   --  Target of link, name (set as attribute on a frame or windows) of
   --  a frame or:
   --  _blank  = new window
   --  _top    = top most frame (full browser window)
   --  _parent = parent frame or window
   --  _self   = current frame or window

   -------------------------------------------------------------------------
   --  Form_Type - Methods
   -------------------------------------------------------------------------

   procedure Submit (Form : in out Form_Type);
   --  Submit form

   procedure Reset (Form : in out Form_Type);
   --  Reset form to original defaults


   -------------------------------------------------------------------------
   --  Form_Element_Types
   -------------------------------------------------------------------------
   -- Parent type for form elements

   type Form_Element_Type is new Gnoga.Element.Element_Type with private;
   type Form_Element_Access is access all Form_Element_Type;
   type Pointer_To_Form_Element_Class is access all Form_Element_Type'Class;

   -------------------------------------------------------------------------
   --  Form_Element_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create_Element (Element    : in out Form_Element_Type;
                             Form       : in out Form_Type'Class;
                             Input_Type : in     String;
                             Value      : in     String := "";
                             Name       : in     String := "";
                             ID         : in     String := "");
   --  Create a form element of Input_Type. Setting Name on form element
   --  is only important if the form will be submitted via a GET or POST.
   --  Value is the inital value for the element.
   --
   --  Valid HTML5 Input_Types are:
   --     button, checkbox, color, date, datetime, datetime-local, email
   --     file, hidden, image, month, number, password, radio, range
   --     reset, search, submit, tel, text, time, url, week

   -------------------------------------------------------------------------
   --  Form_Element_Type - Properties
   -------------------------------------------------------------------------

   procedure Auto_Complete (Element : in out Form_Element_Type;
                            Value   : in     Boolean := True);
   function Auto_Complete (Element : Form_Element_Type) return Boolean;

   procedure Auto_Focus (Element : in out Form_Element_Type;
                         Value   : in     Boolean := True);
   function Auto_Focus (Element : Form_Element_Type) return Boolean;

   procedure Disabled (Element : in out Form_Element_Type;
                       Value   : in     Boolean := True);
   function Disabled (Element : Form_Element_Type) return Boolean;

   procedure Read_Only (Element : in out Form_Element_Type;
                        Value   : in     Boolean := True);
   function Read_Only (Element : Form_Element_Type) return Boolean;

   procedure Validate_On_Submit (Element : in out Form_Element_Type;
                                 Value   : in     Boolean := True);
   function Validate_On_Submit (Element : Form_Element_Type) return Boolean;

   procedure Name (Element : in out Form_Element_Type; Value : in String);
   function Name (Element : Form_Element_Type) return String;
   --  Form element name, name is not the id of the element but rather how
   --  the data returned from the element will be named in the submit to the
   --  server. For example if Name is My_Field a GET request could look like
   --  http://localhost:8080?My_Field=xxxx

   procedure Default_Value (Element : in out Form_Element_Type;
                            Value   : in     String);
   procedure Default_Value (Element : in out Form_Element_Type;
                            Value   : in     Integer);
   function Default_Value (Element : Form_Element_Type) return String;
   function Default_Value (Element : Form_Element_Type) return Integer;
   --  If the form is reset the value will be set to default value
   --  If Value is set at time of creation it also sets it as the Default_Value

   procedure Value (Element : in out Form_Element_Type; Value : in String);
   procedure Value (Element : in out Form_Element_Type; Value : in Integer);
   function Value (Element : Form_Element_Type) return String;
   function Value (Element : Form_Element_Type) return Integer;
   --  Form element values are not accessible through the Text property but
   --  instead through the value property.

   -------------------------------------------------------------------------
   --  Input_Button_Types
   -------------------------------------------------------------------------

   type Input_Button_Type is new Form_Element_Type with private;
   type Input_Button_Access is access all Input_Button_Type;
   type Pointer_To_Input_Button_Class is access all Input_Button_Type'Class;

   -------------------------------------------------------------------------
   --  Input_Button_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Input_Button_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");

   -------------------------------------------------------------------------
   --  Submit_Button_Types
   -------------------------------------------------------------------------
   -- An Input Button that On_Click will fire On_Submit

   type Submit_Button_Type is new Input_Button_Type with private;
   type Submit_Button_Access is access all Submit_Button_Type;
   type Pointer_To_Submit_Button_Class is access all Submit_Button_Type'Class;

   -------------------------------------------------------------------------
   --  Submit_Button_Type - Creation Methods
   -------------------------------------------------------------------------

   overriding
   procedure Create (Element    : in out Submit_Button_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");

   -------------------------------------------------------------------------
   --  Check_Box_Types
   -------------------------------------------------------------------------

   type Check_Box_Type is new Form_Element_Type with private;
   type Check_Box_Access is access all Check_Box_Type;
   type Pointer_To_Check_Box_Class is access all Check_Box_Type'Class;

   -------------------------------------------------------------------------
   --  Check_Box_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Check_Box_Type;
                     Form       : in out Form_Type'Class;
                     Checked    : in     Boolean := False;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");
   --  Value will be what is submitted if Checked is true for Name.

   -------------------------------------------------------------------------
   --  Check_Box_Type - Properties
   -------------------------------------------------------------------------

   procedure Checked (Element : in out Form_Element_Type;
                        Value   : in     Boolean := True);
   function Checked (Element : Form_Element_Type) return Boolean;

   procedure Indeterminate (Element : in out Form_Element_Type;
                        Value   : in     Boolean := True);
   function Indeterminate (Element : Form_Element_Type) return Boolean;

   procedure Required (Element : in out Form_Element_Type;
                        Value   : in     Boolean := True);
   function Required (Element : Form_Element_Type) return Boolean;

   -------------------------------------------------------------------------
   --  Color_Picker_Types
   -------------------------------------------------------------------------

   type Color_Picker_Type is new Form_Element_Type with private;
   type Color_Picker_Access is access all Color_Picker_Type;
   type Pointer_To_Color_Picker_Class is access all Color_Picker_Type'Class;

   -------------------------------------------------------------------------
   --  Color_Picker_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Color_Picker_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");
   procedure Create (Element    : in out Color_Picker_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     Gnoga.Types.RGBA_Type;
                     Name       : in     String := "";
                     ID         : in     String := "");

   -------------------------------------------------------------------------
   --  Color_Picker_Type - Properties
   -------------------------------------------------------------------------

   procedure Color (Element : in out Color_Picker_Type;
                    Value   : in     Gnoga.Types.RGBA_Type);
   function Color (Element : Color_Picker_Type) return Gnoga.Types.RGBA_Type;

private
   type Form_Type is new Gnoga.Element.Element_Type with null record;
   type Form_Element_Type is new Gnoga.Element.Element_Type with null record;
   type Input_Button_Type is new Form_Element_Type with null record;
   type Submit_Button_Type is new Input_Button_Type with null record;
   type Check_Box_Type is new Form_Element_Type with null record;
   type Color_Picker_Type is new Form_Element_Type with null record;
end Gnoga.Element.Form;
