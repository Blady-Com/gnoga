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
   --  From_Element_Types
   -------------------------------------------------------------------------
   -- Parent type for form elements

   type Form_Element_Type is new Gnoga.Element.Element_Type with private;
   type Form_Element_Access is access all Form_Element_Type;
   type Pointer_To_Form_Element_Class is access all Form_Element_Type'Class;

   -------------------------------------------------------------------------
   --  Form_Element_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Form_Element_Type;
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

   procedure Value (Element : in out Form_Element_Type; Value : in String);
   procedure Value (Element : in out Form_Element_Type; Value : in Integer);
   function Value (Element : Form_Element_Type) return String;
   function Value (Element : Form_Element_Type) return Integer;
   --  Form element values are not accessible through the Text property but
   --  instead through the value property.

private
   type Form_Type is new Gnoga.Element.Element_Type with null record;
   type Form_Element_Type is new Gnoga.Element.Element_Type with null record;
end Gnoga.Element.Form;
