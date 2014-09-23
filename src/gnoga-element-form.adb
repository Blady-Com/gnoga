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
------------------------------------------------------------------------------

package body Gnoga.Element.Form is

   -------------------------------------------------------------------------
   --  Form_Element_Type
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (Form    : in out Form_Type;
      Parent  : in out Gnoga.Base.Base_Type'Class;
      ID      : in     String := "")
   is
   begin
      Form.Create_From_HTML (Parent, "<form />", ID);
   end Create;

   ------------
   -- Action --
   ------------

   procedure Action (Form : in out Form_Type; Value : in String) is
   begin
      Form.Property ("action", Value);
   end Action;

   ------------
   -- Action --
   ------------

   function Action (Form : Form_Type) return String is
   begin
      return Form.Property ("action");
   end Action;

   ------------
   -- Method --
   ------------

   procedure Method (Form : in out Form_Type; Value : in Form_Method_Type) is
   begin
      Form.Property ("method", Value'Img);
   end Method;

   ------------
   -- Method --
   ------------

   function Method (Form : Form_Type) return Form_Method_Type is
      Value : String := Form.Property ("method");
   begin
      if Value = "post" then
         return Post;
      else
         return Get;
      end if;
   end Method;

   --------------
   -- Encoding --
   --------------

   procedure Encoding (Form : in out Form_Type; Value : in Encoding_Type) is
   begin
      if Value = URL_Encode then
         Form.Property ("enctype", "application/x-www-form-urlencoded");
      elsif Value = Multi_Part then
         Form.Property ("enctype", "multipart/form-data");
      else
         Form.Property ("enctype", "text/plain");
      end if;
   end Encoding;

   --------------
   -- Encoding --
   --------------

   function Encoding (Form : Form_Type) return Encoding_Type is
      Value : String := Form.Property ("enctype");
   begin
      if Value = "text/plain" then
         return Text;
      elsif Value = "multipart/form-data" then
         return Multi_Part;
      else
         return URL_Encode;
      end if;
   end Encoding;

   -------------------
   -- Auto_Complete --
   -------------------

   procedure Auto_Complete
     (Form  : in out Form_Type;
      Value : in  Boolean := True)
   is
   begin
      Form.Property ("autocomplete", Value);
   end Auto_Complete;

   -------------------
   -- Auto_Complete --
   -------------------

   function Auto_Complete (Form : Form_Type) return Boolean is
   begin
      return Form.Property ("autocomplete");
   end Auto_Complete;

   ------------------------
   -- Validate_On_Submit --
   ------------------------

   procedure Validate_On_Submit
     (Form  : in out Form_Type;
      Value : in  Boolean := True)
   is
   begin
      Form.Property ("noValidate", not (Value));
   end Validate_On_Submit;

   ------------------------
   -- Validate_On_Submit --
   ------------------------

   function Validate_On_Submit (Form : Form_Type) return Boolean is
   begin
      return not (Form.Property ("noValidate"));
   end Validate_On_Submit;

   --------------------------------
   -- Number_Of_Elements_In_Form --
   --------------------------------

   function Number_Of_Elements_In_Form (Form : Form_Type) return Integer is
   begin
      return Form.Property ("length");
   end Number_Of_Elements_In_Form;

   ------------
   -- Target --
   ------------

   procedure Target (Form : in out Form_Type; Value : in String) is
   begin
      Form.Property ("target", Value);
   end Target;

   ------------
   -- Target --
   ------------

   function Target (Form : Form_Type) return String is
   begin
      return Form.Property ("target");
   end Target;

   ------------
   -- Submit --
   ------------

   procedure Submit (Form : in out Form_Type) is
   begin
      Form.Execute ("submit();");
   end Submit;

   -----------
   -- Reset --
   -----------

   procedure Reset (Form : in out Form_Type) is
   begin
      Form.Execute ("reset();");
   end Reset;

   ------------
   -- Create --
   ------------

   procedure Create_Element
     (Element    : in out Form_Element_Type;
      Form       : in out Form_Type'Class;
      Input_Type : in     String;
      Value      : in     String := "";
      Name       : in     String := "";
      ID         : in     String := "")
   is
   begin
      Element.Create_From_HTML (Form, "<input type='" & Input_Type & "' " &
                                  "form='" & Form.ID & "' value='" &
                                  Escape_Quotes (Value) & "' name='" &
                                  Name & "'/>", ID);
   end Create_Element;

   -------------------
   -- Auto_Complete --
   -------------------

   procedure Auto_Complete
     (Element : in out Form_Element_Type;
      Value   : in     Boolean := True)
   is
   begin
      Element.Property ("autocomplete", Value);
   end Auto_Complete;

   -------------------
   -- Auto_Complete --
   -------------------

   function Auto_Complete (Element : Form_Element_Type) return Boolean is
   begin
      return Element.Property ("autocomplete");
   end Auto_Complete;

   ----------------
   -- Auto_Focus --
   ----------------

   procedure Auto_Focus
     (Element : in out Form_Element_Type;
      Value   : in     Boolean := True)
   is
   begin
      Element.Property ("autofocus", Value);
   end Auto_Focus;

   ----------------
   -- Auto_Focus --
   ----------------

   function Auto_Focus (Element : Form_Element_Type) return Boolean is
   begin
      return Element.Property ("autofocus");
   end Auto_Focus;

   --------------
   -- Disabled --
   --------------

   procedure Disabled
     (Element : in out Form_Element_Type;
      Value   : in     Boolean := True)
   is
   begin
      Element.Property ("disabled", Value);
   end Disabled;

   --------------
   -- Disabled --
   --------------

   function Disabled (Element : Form_Element_Type) return Boolean is
   begin
      return Element.Property ("disabled");
   end Disabled;

   ---------------
   -- Read_Only --
   ---------------

   procedure Read_Only
     (Element : in out Form_Element_Type;
      Value   : in     Boolean := True)
   is
   begin
      Element.Property ("readonly", Value);
   end Read_Only;

   ---------------
   -- Read_Only --
   ---------------

   function Read_Only (Element : Form_Element_Type) return Boolean is
   begin
      return Element.Property ("readonly");
   end Read_Only;

   ------------------------
   -- Validate_On_Submit --
   ------------------------

   procedure Validate_On_Submit
     (Element : in out Form_Element_Type;
      Value   : in     Boolean := True)
   is
   begin
      Element.Property ("noValidate", not (Value));
   end Validate_On_Submit;

   ------------------------
   -- Validate_On_Submit --
   ------------------------

   function Validate_On_Submit (Element : Form_Element_Type) return Boolean is
   begin
      return Element.Property ("length");
   end Validate_On_Submit;

   ----------
   -- Name --
   ----------

   procedure Name (Element : in out Form_Element_Type; Value : in String) is
   begin
      Element.Property ("name", Value);
   end Name;

   function Name (Element : Form_Element_Type) return String is
   begin
      return Element.Property ("name");
   end Name;

   -------------------
   -- Default_Value --
   -------------------

   procedure Default_Value (Element : in out Form_Element_Type;
                            Value   : in     String)
   is
   begin
      Element.Property ("defaultValue", Value);
   end Default_Value;


   procedure Default_Value (Element : in out Form_Element_Type;
                            Value   : in     Integer)
   is
   begin
      Element.Property ("defaultValue", Value);
   end Default_Value;

   function Default_Value (Element : Form_Element_Type) return String is
   begin
      return Element.Property ("defaultValue");
   end Default_Value;


   function Default_Value (Element : Form_Element_Type) return Integer is
   begin
      return Element.Property ("defaultValue");
   end Default_Value;

   -----------
   -- Value --
   -----------

   procedure Value (Element : in out Form_Element_Type; Value : in String) is
   begin
      Element.Property ("value", Value);
   end Value;

   procedure Value (Element : in out Form_Element_Type; Value : in Integer) is
   begin
      Element.Property ("value", Value);
   end Value;

   function Value (Element : Form_Element_Type) return String is
   begin
      return Element.Property ("value");
   end Value;

   function Value (Element : Form_Element_Type) return Integer is
   begin
      return Element.Property ("value");
   end Value;

   ------------------
   -- Place_Holder --
   ------------------

   procedure Place_Holder (Element : in out Form_Element_Type;
                           Value   : in     String)
   is
   begin
      Element.Property ("placeholder", Value);
   end Place_Holder;

   function Place_Holder (Element : Form_Element_Type) return String is
   begin
      return Element.Property ("placeholder");
   end Place_Holder;

   -------------
   -- Pattern --
   -------------

   procedure Pattern (Element : in out Form_Element_Type; Value : in String) is
   begin
      Element.Property ("pattern", Value);
   end Pattern;

   function Pattern (Element : Form_Element_Type) return String is
   begin
      return Element.Property ("pattern");
   end Pattern;

   --------------
   -- Required --
   --------------

   procedure Required (Element : in out Form_Element_Type;
                       Value   : in     Boolean := True)
   is
   begin
      Element.Property ("required", Value);
   end Required;

   function Required (Element : Form_Element_Type) return Boolean is
   begin
      return Element.Property ("required");
   end Required;

   -------------
   -- Minimum --
   -------------

   procedure Minimum (Element : in out Form_Element_Type; Value : in String) is
   begin
      Element.Property ("min", Value);
   end Minimum;

   function Minimum (Element : Form_Element_Type) return String is
   begin
      return Element.Property ("min");
   end Minimum;

   -------------
   -- Maximum --
   -------------

   procedure Maximum (Element : in out Form_Element_Type; Value : in String) is
   begin
      Element.Property ("max", Value);
   end Maximum;

   function Maximum (Element : Form_Element_Type) return String is
   begin
      return Element.Property ("max");
   end Maximum;

   ----------
   -- Step --
   ----------

   procedure Step (Element : in out Form_Element_Type; Value : in String) is
   begin
      Element.Property ("step", Value);
   end Step;

   procedure Step (Element : in out Form_Element_Type; Value : in Integer) is
   begin
      Element.Property ("step", Value);
   end Step;

   function Step (Element : Form_Element_Type) return String is
   begin
      return Element.Property ("step");
   end Step;

   function Step (Element : Form_Element_Type) return Integer is
   begin
      return Element.Property ("step");
   end Step;

   -----------------
   -- Select_Text --
   -----------------

   procedure Select_Text (Element : in out Form_Element_Type) is
   begin
      Element.Execute ("select()");
   end Select_Text;

   -------------------------------------------------------------------------
   --  Text_Area_Type
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (Element    : in out Text_Area_Type;
                     Form       : in out Form_Type'Class;
                     Columns    : in     Natural := 20;
                     Rows       : in     Natural := 2;
                     Value      : in     String  := "";
                     Name       : in     String  := "";
                     ID         : in     String  := "")
   is
   begin
      Element.Create_From_HTML (Form, "<textarea " &
                                  "form='" & Form.ID & "' name='" &
                                  Name & "' cols=" & Columns'Img &
                                  " rows=" & Rows'Img & ">" &
                                  Escape_Quotes (Value) &
                                  "</textarea>", ID);
   end Create;

   ---------------
   -- Word_Wrap --
   ---------------

   procedure Word_Wrap (Element : in out Text_Area_Type;
                        Value   : in      Boolean := True)
   is
   begin
      Element.Property ("wrap", Value);
   end Word_Wrap;

   function Word_Wrap (Element : Text_Area_Type) return Boolean is
   begin
      return Element.Property ("wrap");
   end Word_Wrap;

   -------------
   -- Columns --
   -------------

   procedure Columns (Element : in out Text_Area_Type;
                      Value   : in     Integer)
   is
   begin
      Element.Property ("cols", Value);
   end Columns;

   function Columns (Element : Text_Area_Type) return Integer
   is
   begin
      return Element.Property ("cols");
   end Columns;

   ----------
   -- Rows --
   ----------

   procedure Rows (Element : in out Text_Area_Type;
                   Value   : in     Integer)
   is
   begin
      Element.Property ("rows", Value);
   end Rows;

   function Rows (Element : Text_Area_Type) return Integer is
   begin
      return Element.Property ("rows");
   end Rows;

   -------------------------------------------------------------------------
   --  Hidden_Type
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (Element    : in out Hidden_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "hidden",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
   end Create;

   -------------------------------------------------------------------------
   --  Input_Button_Type
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (Element    : in out Input_Button_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "button",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
   end Create;

   -------------------------------------------------------------------------
   --  Submit_Button_Type
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Submit_Button_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "submit",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
   end Create;

   -------------------------------------------------------------------------
   --  Reset_Button_Type
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Reset_Button_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "reset",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
   end Create;

   -------------------------------------------------------------------------
   --  Input_Image_Type
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Input_Image_Type;
                     Form       : in out Form_Type'Class;
                     Source     : in     String    := "";
                     Value      : in     String    := "";
                     Name       : in     String    := "";
                     ID         : in     String    := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "image",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);

      if Source /= "" then
         Element.Source (Source);
      end if;
   end Create;

   ------------
   -- Source --
   ------------

   procedure Source (Element : in out Input_Image_Type; Value : String) is
   begin
      Element.Property ("src", Value);
   end Source;

   function Source (Element : Input_Image_Type) return String is
   begin
      return Element.Property ("src");
   end Source;

   -------------------------------------------------------------------------
   --  Text_Type
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Text_Type;
                     Form       : in out Form_Type'Class;
                     Size       : in     Integer   := 20;
                     Value      : in     String    := "";
                     Name       : in     String    := "";
                     ID         : in     String    := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "text",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
      Element.Size (Size);
   end Create;

   ----------
   -- Size --
   ----------

   procedure Size (Element : in out Text_Type; Value : Integer) is
   begin
      Element.Property ("size", Value);
   end Size;

   function Size (Element : Text_Type) return Integer is
   begin
      return Element.Property ("size");
   end Size;

   --------------
   -- Max_Size --
   --------------

   procedure Max_Length (Element : in out Text_Type; Value : Integer) is
   begin
      Element.Property ("maxLength", Value);
   end Max_Length;

   function Max_Length (Element : Text_Type) return Integer is
   begin
      return Element.Property ("maxLength");
   end Max_Length;

   -------------------------------------------------------------------------
   --  Email_Type
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Email_Type;
                     Form       : in out Form_Type'Class;
                     Size       : in     Integer   := 20;
                     Value      : in     String    := "";
                     Name       : in     String    := "";
                     ID         : in     String    := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "email",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
      Element.Size (Size);
   end Create;

   procedure Multiple_Emails (Element : in out Email_Type;
                              Value   : in     Boolean := True)
   is
   begin
      Element.Property ("multiple", Value);
   end Multiple_Emails;

   function Multiple_Emails (Element : Email_Type) return Boolean is
   begin
      return Element.Property ("multiple");
   end Multiple_Emails;

   -------------------------------------------------------------------------
   --  Password_Type
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Password_Type;
                     Form       : in out Form_Type'Class;
                     Size       : in     Integer   := 20;
                     Value      : in     String    := "";
                     Name       : in     String    := "";
                     ID         : in     String    := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "password",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
      Element.Size (Size);
   end Create;

   -------------------------------------------------------------------------
   --  Search_Type
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Search_Type;
                     Form       : in out Form_Type'Class;
                     Size       : in     Integer   := 20;
                     Value      : in     String    := "";
                     Name       : in     String    := "";
                     ID         : in     String    := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "search",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
      Element.Size (Size);
   end Create;

   -------------------------------------------------------------------------
   --  URL_Type
   -------------------------------------------------------------------------

   procedure Create (Element    : in out URL_Type;
                     Form       : in out Form_Type'Class;
                     Size       : in     Integer   := 20;
                     Value      : in     String    := "";
                     Name       : in     String    := "";
                     ID         : in     String    := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "url",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
      Element.Size (Size);
   end Create;

   -------------------------------------------------------------------------
   --  Check_Box_Type
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (Element    : in out Check_Box_Type;
                     Form       : in out Form_Type'Class;
                     Checked    : in     Boolean := False;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "checkbox",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
      Element.Checked (Checked);
   end Create;

   -------------------------------------------------------------------------
   --  Check_Box_Type - Properties
   -------------------------------------------------------------------------

   --------------
   -- Checked --
   --------------

   procedure Checked (Element : in out Check_Box_Type;
                      Value   : in     Boolean := True)
   is
   begin
      Element.Property ("checked", Value);
   end Checked;

   function Checked (Element : Check_Box_Type) return Boolean is
   begin
      return Element.Property ("checked");
   end Checked;

   -------------------
   -- Indeterminate --
   -------------------

   procedure Indeterminate (Element : in out Check_Box_Type;
                            Value   : in     Boolean := True)
   is
   begin
      Element.Property ("indeterminate", Value);
   end Indeterminate;

   function Indeterminate (Element : Check_Box_Type) return Boolean is
   begin
      return Element.Property ("indeterminate");
   end Indeterminate;

   -------------------------------------------------------------------------
   --  Radio_Button_Type
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (Element    : in out Radio_Button_Type;
                     Form       : in out Form_Type'Class;
                     Checked    : in     Boolean := False;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "radio",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
      Element.Checked (Checked);
   end Create;

   -------------------------------------------------------------------------
   --  Radio_Button_Type - Properties
   -------------------------------------------------------------------------

   --------------
   -- Checked --
   --------------

   procedure Checked (Element : in out Radio_Button_Type;
                      Value   : in     Boolean := True)
   is
   begin
      Element.Property ("checked", Value);
   end Checked;

   function Checked (Element : Radio_Button_Type) return Boolean is
   begin
      return Element.Property ("checked");
   end Checked;
   -------------------------------------------------------------------------
   --  Color_Picker_Type
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (Element    : in out Color_Picker_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "color",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
   end Create;

   procedure Create (Element    : in out Color_Picker_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     Gnoga.Types.RGBA_Type;
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create (Form       => Form,
                      Value      => Gnoga.Types.To_String (Value),
                      Name       => Name,
                      ID         => ID);
   end Create;

   -------------------------------------------------------------------------
   --  Color_Picker_Type - Properties
   -------------------------------------------------------------------------

   -----------
   -- Value --
   -----------

   procedure Color (Element : in out Color_Picker_Type;
                    Value   : in     Gnoga.Types.RGBA_Type)
   is
   begin
      Element.Property ("value", Gnoga.Types.To_String (Value));
   end Color;

   function Color (Element : Color_Picker_Type) return Gnoga.Types.RGBA_Type is
   begin
      return Gnoga.Types.To_RGBA (Element.Property ("value"));
   end Color;

   -------------------------------------------------------------------------
   --  Date_Type
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (Element    : in out Date_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "date",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
   end Create;

   -------------------------------------------------------------------------
   --  Time_Type
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (Element    : in out Time_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "time",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
   end Create;

   -------------------------------------------------------------------------
   --  Month_Type
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Month_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "month",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
   end Create;

   -------------------------------------------------------------------------
   --  Week_Type
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (Element    : in out Week_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "week",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
   end Create;

   -------------------------------------------------------------------------
   --  Date_Time_Type
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (Element    : in out Date_Time_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "datetime",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
   end Create;

   -------------------------------------------------------------------------
   --  Date_Time_Local_Type
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (Element    : in out Date_Time_Local_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "datetime-local",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
   end Create;

   -------------------------------------------------------------------------
   --  Number_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Number_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "number",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
   end Create;

   -------------------------------------------------------------------------
   --  Range_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Range_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "")
   is
   begin
      Element.Create_Element (Form       => Form,
                              Input_Type => "range",
                              Value      => Value,
                              Name       => Name,
                              ID         => ID);
   end Create;
end Gnoga.Element.Form;
