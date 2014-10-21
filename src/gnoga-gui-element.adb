------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                     G N O G A . G U I . E L E M E N T                    --
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

with Ada.Strings.Unbounded;
with Gnoga.Server.Connection;

package body Gnoga.Gui.Element is

   -------------------------------------------------------------------------
   --  Element_Type - Creation Methods
   -------------------------------------------------------------------------

   ----------------------
   -- Create_From_HTML --
   ----------------------

   procedure Create_From_HTML (Element : in out Element_Type;
                               Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                               HTML    : in     String;
                               ID      : in     String := "")
   is
      function Adjusted_ID return String;

      function Adjusted_ID return String is
      begin
         if ID = "" then
            return Gnoga.Server.Connection.New_GID;
         else
            return ID;
         end if;
      end Adjusted_ID;

      GID : String := Adjusted_ID;
   begin
      Element.Create_With_Script
        (Connection_ID => Parent.Connection_ID,
         ID            => GID,
         Script        => "gnoga['" & GID & "']=$(""" & Escape_Quotes (HTML) &
             """).prop('id','" & GID & "');",
         ID_Type       => Gnoga.Types.Gnoga_ID);

      Element.Parent (Parent);
   end Create_From_HTML;

   -------------------------------------------------------------------------
   --  Element_Type - Properties
   -------------------------------------------------------------------------

   -----------
   -- Style --
   -----------

   procedure Style (Element : in out Element_Type;
                    Name    : in String;
                    Value   : in String)
   is
   begin
      Element.jQuery_Execute ("css ('" & Name & "', """ &
                                Escape_Quotes (Value) & """);");
   end Style;

   procedure Style (Element : in out Element_Type;
                    Name    : in String;
                    Value   : in Integer)
   is
   begin
      Element.jQuery_Execute ("css ('" & Name & "'," & Value'Img & ");");
   end Style;

   function Style (Element : Element_Type; Name : String) return String is
   begin
      return Element.jQuery_Execute ("css ('" & Name & "');");
   end Style;

   function Style (Element : Element_Type; Name : String) return Integer is
   begin
      return Integer'Value (Element.Style (Name));
   exception
      when others =>
         return 0;
   end Style;

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (Element : in out Element_Type;
                        Name    : in String;
                        Value   : in String)
   is
   begin
      Element.jQuery_Execute ("attr ('" & Name & "',""" &
                                Escape_Quotes (Value) & """);");
   end Attribute;

   function Attribute (Element : Element_Type; Name : String) return String is
   begin
      return Element.jQuery_Execute ("attr ('" & Name & "');");
   end Attribute;

   ----------------
   -- Access_Key --
   ----------------

   procedure Access_Key (Element : in out Element_Type; Value : in String) is
   begin
      Element.Property ("accessKey", Value);
   end Access_Key;

   function Access_Key (Element : Element_Type) return String is
   begin
      return Element.Property ("accessKey");
   end Access_Key;

   --------------------
   -- Advisory_Title --
   --------------------

   procedure Advisory_Title (Element : in out Element_Type;
                             Value   : in     String)
   is
   begin
      Element.Property ("title", Value);
   end Advisory_Title;

   function Advisory_Title (Element : Element_Type) return String is
   begin
      return Element.Property ("title");
   end Advisory_Title;

   ----------------
   -- Class_Name --
   ----------------

   procedure Class_Name (Element : in out Element_Type; Value : in String) is
   begin
      Element.Property ("className", Value);
   end Class_Name;

   function Class_Name (Element : Element_Type) return String is
   begin
      return Element.Property ("className");
   end Class_Name;

   --------------
   -- Editable --
   --------------

   procedure Editable (Element : in out Element_Type;
                       Value   : in     Boolean := True)
   is
   begin
      Element.Property ("contentEditable", Value);
   end Editable;

   function Editable (Element : Element_Type) return Boolean is
   begin
      return Element.Property ("isContentEditable");
   end Editable;

   ----------------
   -- Box_Sizing --
   ----------------

   procedure Box_Sizing (Element : in out Element_Type;
                         Value   : in     Box_Sizing_Type)
   is
   begin
      case Value is
         when Content_Box =>
            Element.Style ("box-sizing", "content-box");
         when Border_Box =>
            Element.Style ("box-sizing", "border-box");
      end case;
   end Box_Sizing;

   function Box_Sizing (Element : Element_Type) return Box_Sizing_Type
   is
   begin
      if Element.Style ("box-sizing") = "border-box" then
         return Border_Box;
      else
         return Content_Box;
      end if;
   end Box_Sizing;

   ----------------
   -- Clear_Side --
   ----------------

   procedure Clear_Side (Element : in out Element_Type;
                         Value   : in     Clear_Side_Type)
   is
   begin
      Element.Style ("clear", Value'Img);
   end Clear_Side;

   -------------
   -- Display --
   -------------

   procedure Display (Element : in out Element_Type;
                      Value   : in     String)
   is
   begin
      Element.Style ("display", Value);
   end Display;

   function Display (Element : Element_Type) return String
   is
   begin
      return Element.Style ("display");
   end Display;

   --------------
   -- Overflow --
   --------------

   procedure Overflow (Element : in out Element_Type;
                       Value   : in     Overflow_Type)
   is
   begin
      Element.Style ("overflow", Value'Img);
   end Overflow;

   function Overflow (Element : Element_Type) return Overflow_Type is
   begin
      return Overflow_Type'Value (Element.Style ("overflow"));
   exception
      when others =>
         return Visible;
   end Overflow;

   --------------
   -- Resizable --
   --------------

   procedure Resizable (Element : in out Element_Type;
                        Value   : in     Resizable_Type)
   is
   begin
      Element.Style ("resize", Value'Img);
   end Resizable;

   function Resizable (Element : Element_Type) return Resizable_Type is
   begin
      return Resizable_Type'Value (Element.Style ("resize"));
   exception
      when others =>
         return None;
   end Resizable;

   --------------
   -- Position --
   --------------

   procedure Position (Element : in out Element_Type;
                       Value   : in     Position_Type)
   is
   begin
      Element.Style ("position", Value'Img);
   end Position;

   function Position (Element : Element_Type) return Position_Type is
   begin
      return Position_Type'Value (Element.Style ("position"));
   exception
      when others =>
         return Static;
   end Position;

   ----------
   -- Left --
   ----------

   procedure Left (Element : in out Element_Type;
                   Value   : in     Integer;
                   Unit    : in     String := "px")
   is
   begin
      Element.Style ("left", Left_Trim (Value'Img) & Unit);
   end Left;

   procedure Left (Element : in out Element_Type;
                   Value   : in     String)
   is
   begin
      Element.Style ("left", Value);
   end Left;

   function Left (Element : Element_Type) return String
   is
   begin
      return Element.Style ("left");
   end Left;

   -----------
   -- Right --
   -----------

   procedure Right (Element : in out Element_Type;
                    Value   : in     Integer;
                    Unit    : in     String := "px")
   is
   begin
      Element.Style ("right", Left_Trim (Value'Img) & Unit);
   end Right;

   procedure Right (Element : in out Element_Type;
                    Value   : in     String)
   is
   begin
      Element.Style ("right", Value);
   end Right;

   function Right (Element : Element_Type) return String
   is
   begin
      return Element.Style ("right");
   end Right;

   ---------
   -- Top --
   ---------

   procedure Top (Element : in out Element_Type;
                  Value   : in     Integer;
                  Unit    : in     String := "px")
   is
   begin
      Element.Style ("top", Left_Trim (Value'Img) & Unit);
   end Top;

   procedure Top (Element : in out Element_Type;
                  Value   : in     String)
   is
   begin
      Element.Style ("top", Value);
   end Top;

   function Top (Element : Element_Type) return String
   is
   begin
      return Element.Style ("top");
   end Top;

   ------------
   -- Bottom --
   ------------

   procedure Bottom (Element : in out Element_Type;
                     Value   : in     Integer;
                     Unit    : in     String := "px")
   is
   begin
      Element.Style ("bottom", Left_Trim (Value'Img) & Unit);
   end Bottom;

   procedure Bottom (Element : in out Element_Type;
                     Value   : in     String)
   is
   begin
      Element.Style ("bottom", Value);
   end Bottom;

   function Bottom (Element : Element_Type) return String
   is
   begin
      return Element.Style ("bottom");
   end Bottom;

   ----------------
   -- Box_Height --
   ----------------

   procedure Box_Height (Element : in out Element_Type;
                         Value   : in     Integer;
                         Unit    : in     String := "px")
   is
   begin
      Element.Style ("height", Left_Trim (Value'Img) & Unit);
      Element_Type'Class (Element).On_Resize;
   end Box_Height;

   procedure Box_Height (Element : in out Element_Type;
                         Value   : in     String)
   is
   begin
      Element.Style ("height", Value);
      Element_Type'Class (Element).On_Resize;
   end Box_Height;

   function Box_Height (Element : Element_Type) return String
   is
   begin
      return Element.Style ("height");
   end Box_Height;

   --------------------
   -- Minimum_Height --
   --------------------

   procedure Minimum_Height (Element : in out Element_Type;
                             Value   : in     Integer;
                             Unit    : in     String := "px")
   is
   begin
      Element.Style ("min-height", Left_Trim (Value'Img) & Unit);
      Element_Type'Class (Element).On_Resize;
   end Minimum_Height;

   procedure Minimum_Height (Element : in out Element_Type;
                             Value   : in     String)
   is
   begin
      Element.Style ("min-height", Value);
      Element_Type'Class (Element).On_Resize;
   end Minimum_Height;

   function Minimum_Height (Element : Element_Type) return String
   is
   begin
      return Element.Style ("min-height");
   end Minimum_Height;

   --------------------
   -- Maximum_Height --
   --------------------

   procedure Maximum_Height (Element : in out Element_Type;
                             Value   : in     Integer;
                             Unit    : in     String := "px")
   is
   begin
      Element.Style ("max-height", Left_Trim (Value'Img) & Unit);
      Element_Type'Class (Element).On_Resize;
   end Maximum_Height;

   procedure Maximum_Height (Element : in out Element_Type;
                             Value   : in     String)
   is
   begin
      Element.Style ("max-height", Value);
      Element_Type'Class (Element).On_Resize;
   end Maximum_Height;

   function Maximum_Height (Element : Element_Type) return String
   is
   begin
      return Element.Style ("max-height");
   end Maximum_Height;

   ---------------
   -- Box_Width --
   ---------------

   procedure Box_Width (Element : in out Element_Type;
                        Value   : in     Integer;
                        Unit    : in     String := "px")
   is
   begin
      Element.Style ("width", Left_Trim (Value'Img) & Unit);
      Element_Type'Class (Element).On_Resize;
   end Box_Width;

   procedure Box_Width (Element : in out Element_Type;
                        Value   : in     String)
   is
   begin
      Element.Style ("width", Value);
      Element_Type'Class (Element).On_Resize;
   end Box_Width;

   function Box_Width (Element : Element_Type) return String
   is
   begin
      return Element.Style ("width");
   end Box_Width;

   -------------------
   -- Minimum_Width --
   -------------------

   procedure Minimum_Width (Element : in out Element_Type;
                            Value   : in     Integer;
                            Unit    : in     String := "px")
   is
   begin
      Element.Style ("min-width", Left_Trim (Value'Img) & Unit);
      Element_Type'Class (Element).On_Resize;
   end Minimum_Width;

   procedure Minimum_Width (Element : in out Element_Type;
                            Value   : in     String)
   is
   begin
      Element.Style ("min-width", Value);
      Element_Type'Class (Element).On_Resize;
   end Minimum_Width;

   function Minimum_Width (Element : Element_Type) return String
   is
   begin
      return Element.Style ("min-width");
   end Minimum_Width;

   -------------------
   -- Maximum_Width --
   -------------------

   procedure Maximum_Width (Element : in out Element_Type;
                            Value   : in     Integer;
                            Unit    : in     String := "px")
   is
   begin
      Element.Style ("max-width", Left_Trim (Value'Img) & Unit);
      Element_Type'Class (Element).On_Resize;
   end Maximum_Width;

   procedure Maximum_Width (Element : in out Element_Type;
                            Value   : in     String)
   is
   begin
      Element.Style ("max-width", Value);
      Element_Type'Class (Element).On_Resize;
   end Maximum_Width;

   function Maximum_Width (Element : Element_Type) return String
   is
   begin
      return Element.Style ("max-width");
   end Maximum_Width;

   ---------------
   -- Draggable --
   ---------------

   procedure Draggable (Element    : in out Element_Type;
                        Value      : in     Boolean := True)
   is
   begin
      Element.Property ("draggable", Value);
   end Draggable;

   function Draggable (Element : Element_Type) return Boolean is
   begin
      return Element.Property ("draggable");
   end Draggable;

   ------------
   -- Hidden --
   ------------

   procedure Hidden (Element : in out Element_Type;
                     Value   : in     Boolean := True)
   is
   begin
      Element.Property ("hidden", Value);
   end Hidden;

   function Hidden (Element : Element_Type) return Boolean is
   begin
      return Element.Property ("hidden");
   end Hidden;

   ----------------
   -- Inner_HTML --
   ----------------

   procedure Inner_HTML (Element : in out Element_Type;
                         Value   : in     String)
   is
   begin
      Element.jQuery_Execute ("html (""" & Escape_Quotes (Value) & """);");
   end Inner_HTML;

   function Inner_HTML (Element : Element_Type) return String is
   begin
      return Element.jQuery_Execute ("html();");
   end Inner_HTML;

   -----------------
   -- Spell_Check --
   -----------------

   procedure Spell_Check (Element : in out Element_Type;
                          Value   : in     Boolean := True)
   is
   begin
      Element.Property ("spellcheck", Value);
   end Spell_Check;

   function Spell_Check (Element : Element_Type) return Boolean is
   begin
      return Element.Property ("spellcheck");
   end Spell_Check;

   ---------------
   -- Tab_Index --
   ---------------

   procedure Tab_Index (Element : in out Element_Type; Value : in Natural)
   is
   begin
      Element.Property ("tabIndex", Value);
   end Tab_Index;

   function Tab_Index (Element : Element_Type) return Natural is
   begin
      return Element.Property ("tabIndex");
   end Tab_Index;

   ----------
   -- Text --
   ----------

   procedure Text (Element : in out Element_Type; Value : in String) is
   begin
      Element.jQuery_Execute ("text (""" & Escape_Quotes (Value) & """);");
   end Text;

   function Text (Element : Element_Type) return String is
   begin
      return Element.jQuery_Execute ("text();");
   end Text;

   --------------------
   -- Text_Direction --
   --------------------

   procedure Text_Direction (Element : in out Element_Type;
                             Value   : in     Text_Direction_Type)
   is
      function To_String return String;

      function To_String return String is
      begin
         if Value = Right_To_Left then
            return "rtl";
         else
            return "ltr";
         end if;
      end To_String;
   begin
      Element.Property ("dir", To_String);
   end Text_Direction;

   function Text_Direction (Element : Element_Type) return Text_Direction_Type
   is
      function To_TDT return Text_Direction_Type;

      function To_TDT return Text_Direction_Type is
      begin
         if Element.Property ("dir") = "rtl" then
            return Right_To_Left;
         else
            return Left_To_Right;
         end if;
      end To_TDT;
   begin
      return To_TDT;
   end Text_Direction;

   -------------------
   -- Language_Code --
   -------------------

   procedure Language_Code (Element : in out Element_Type;
                            Value   : in     String)
   is
   begin
      Element.Property ("lang", Value);
   end Language_Code;

   function Language_Code (Element : Element_Type) return String is
   begin
      return Element.Property ("lang");
   end Language_Code;

   -------------
   -- Visible --
   -------------

   procedure Visible (Element : in out Element_Type;
                      Value   : in     Boolean := True)
   is
   begin
      if Value then
         Element.Style ("visibility", "visible");
      else
         Element.Style ("visibility", "hidden");
      end if;
   end Visible;

   function Visible (Element : Element_Type) return Boolean
   is
   begin
      return Element.Style ("visibility") = "visible";
   end Visible;

   -------------------
   -- Client_Height --
   -------------------

   function Client_Height (Element : Element_Type) return Natural is
   begin
      return Element.Property ("clientHeight");
   end Client_Height;

   ------------------
   -- Client_Width --
   ------------------

   function Client_Width (Element : Element_Type) return Natural is
   begin
      return Element.Property ("clientWidth");
   end Client_Width;

   ------------------
   -- Client_Left --
   ------------------

   function Client_Left (Element : Element_Type) return Natural is
   begin
      return Element.Property ("clientLeft");
   end Client_Left;

   ------------------
   -- Client_Top --
   ------------------

   function Client_Top (Element : Element_Type) return Natural is
   begin
      return Element.Property ("clientTop");
   end Client_Top;

   -------------------
   -- Offset_Height --
   -------------------

   function Offset_Height (Element : Element_Type) return Integer is
   begin
      return Element.Property ("offsetHeight");
   end Offset_Height;

   ------------------
   -- Offset_Width --
   ------------------

   function Offset_Width (Element : Element_Type) return Integer is
   begin
      return Element.Property ("offsetWidth");
   end Offset_Width;

   ------------------
   -- Offset_Left --
   ------------------

   function Offset_Left (Element : Element_Type) return Integer is
   begin
      return Element.Property ("offsetLeft");
   end Offset_Left;

   ------------------
   -- Offset_Top --
   ------------------

   function Offset_Top (Element : Element_Type) return Integer is
   begin
      return Element.Property ("offsetTop");
   end Offset_Top;

   -------------------
   -- Scroll_Height --
   -------------------

   function Scroll_Height (Element : Element_Type) return Natural is
   begin
      return Element.Property ("scrollHeight");
   end Scroll_Height;

   ------------------
   -- Scroll_Width --
   ------------------

   function Scroll_Width (Element : Element_Type) return Natural is
   begin
      return Element.Property ("scrollWidth");
   end Scroll_Width;

   ------------------
   -- Scroll_Left --
   ------------------

   procedure Scroll_Left (Element : in out Element_Type; Value : Integer) is
   begin
      Element.Property ("scrollLeft", Value);
   end Scroll_Left;

   function Scroll_Left (Element : Element_Type) return Integer is
   begin
      return Element.Property ("scrollLeft");
   end Scroll_Left;

   ------------------
   -- Scroll_Top --
   ------------------

   procedure Scroll_Top (Element : in out Element_Type; Value : Integer) is
   begin
      Element.Property ("scrollTop", Value);
   end Scroll_Top;

   function Scroll_Top (Element : Element_Type) return Integer is
   begin
      return Element.Property ("scrollTop");
   end Scroll_Top;

   -----------
   -- Color --
   -----------

   procedure Color (Element : in out Element_Type; Value : String) is
   begin
      Element.Style ("color", Value);
   end Color;

   procedure Color (Element : in out Element_Type;
                    RGBA    : Gnoga.Types.RGBA_Type)
   is
   begin
      Element.Style ("color", Gnoga.Types.To_String (RGBA));
   end Color;

   function Color (Element : Element_Type) return Gnoga.Types.RGBA_Type is
   begin
      return Gnoga.Types.To_RGBA (Element.Style ("color"));
   end Color;

   -------------
   -- Opacity --
   -------------

   procedure Opacity (Element : in out Element_Type;
                      Alpha   : in     Gnoga.Types.Alpha_Type)
   is
   begin
      Element.Style ("opacity", Alpha'Img);
   end Opacity;

   function Opacity (Element : Element_Type) return Gnoga.Types.Alpha_Type is
   begin
      return Gnoga.Types.Alpha_Type'Value (Element.Style ("opacity"));
   exception
      when others =>
         Log ("Error converting opacity to Alpha_Type");
         return 1.0;
   end Opacity;

   ---------------------------
   -- Background_Attachment --
   ---------------------------

   procedure Background_Attachment
     (Element : in out Element_Type;
      Value   : in     Background_Attachment_type)
   is
   begin
      Element.Style ("background-attachment", Value'Img);
   end Background_Attachment;

   function Background_Attachment (Element : Element_Type)
                                   return Background_Attachment_type
   is
      Value : String := Element.Style ("background-color");
   begin
      if Value = "" then
         return Scroll;
      else
         return Background_Attachment_type'Value (Value);
      end if;
   end Background_Attachment;

   ----------------------
   -- Background_Color --
   ----------------------

   procedure Background_Color (Element : in out Element_Type; Value : String)
   is
   begin
      Element.Style ("background-color", Value);
   end Background_Color;

   procedure Background_Color (Element : in out Element_Type;
                               RGBA    : Gnoga.Types.RGBA_Type)
   is
   begin
      Element.Style ("background-color", Gnoga.Types.To_String (RGBA));
   end Background_Color;

   function Background_Color (Element : Element_Type)
                              return Gnoga.Types.RGBA_Type
   is
   begin
      return Gnoga.Types.To_RGBA (Element.Style ("background-color"));
   end Background_Color;

   ----------------------
   -- Background_Image --
   ----------------------

   procedure Background_Image (Element : in out Element_Type;
                               Value   : in     String)
   is
   begin
      if Value = "" then
         Element.Style ("background-image", "none");
      else
         Element.Style ("background-image", "url('" & Value & "')");
      end if;
   end Background_Image;

   function Background_Image (Element : Element_Type) return String is
   begin
      return Element.Style ("background-image");
   end Background_Image;

   -------------------------
   -- Background_Position --
   -------------------------

   procedure Background_Position (Element : in out Element_Type;
                                  Value   : in     String)
   is
   begin
      Element.Style ("background-position", Value);
   end Background_Position;

   function Background_Position (Element : Element_Type) return String is
   begin
      return Element.Style ("background-position");
   end Background_Position;

   -----------------------
   -- Background_Origin --
   -----------------------

   procedure Background_Origin (Element : in out Element_Type;
                                Value   : in     String)
   is
   begin
      Element.Style ("background-origin", Value);
   end Background_Origin;

   function Background_Origin (Element : Element_Type) return String is
   begin
      return Element.Style ("background-origin");
   end Background_Origin;

   -----------------------
   -- Background_Repeat --
   -----------------------

   procedure Background_Repeat (Element : in out Element_Type;
                                Value   : in     String)
   is
   begin
      Element.Style ("background-repeat", Value);
   end Background_Repeat;

   function Background_Repeat (Element : Element_Type) return String is
   begin
      return Element.Style ("background-repeat");
   end Background_Repeat;

   ---------------------
   -- Background_Clip --
   ---------------------

   procedure Background_Clip (Element : in out Element_Type;
                              Value   : in     String)
   is
   begin
      Element.Style ("background-clip", Value);
   end Background_Clip;

   function Background_Clip (Element : Element_Type) return String is
   begin
      return Element.Style ("background-clip");
   end Background_Clip;

   ---------------------
   -- Background_Size --
   ---------------------

   procedure Background_Size (Element : in out Element_Type;
                              Value   : in     String)
   is
   begin
      Element.Style ("background-size", Value);
   end Background_Size;

   function Background_Size (Element : Element_Type) return String is
   begin
      return Element.Style ("background-size");
   end Background_Size;

   ------------
   -- Border --
   ------------

   procedure Border (Element : in out Element_Type;
                     Width   : in     String       := "medium";
                     Style   : in     Border_Style := Solid;
                     Color   : in     String       := "black")
   is
   begin
      Element.Style ("border", Width & " " & Style'Img & " " & Color);
   end Border;

   -------------------
   -- Border_Radius --
   -------------------

   procedure Border_Radius (Element : in out Element_Type;
                            Radius  : in     String := "0")
   is
   begin
      Element.Style ("border-radius", Radius);
   end Border_Radius;

   ------------
   -- Shadow --
   ------------

   procedure Shadow (Element             : in out Element_Type;
                     Horizontal_Position : in     String;
                     Vertical_Position   : in     String;
                     Blur                : in     String := "";
                     Spread              : in     String := "";
                     Color               : in     String := "black";
                     Inset_Shadow        : in     Boolean := False)
   is
      function Inset return String;

      function Inset return String is
      begin
         if Inset_Shadow then
            return "inset";
         else
            return "";
         end if;
      end Inset;
   begin
      Element.Style ("box-shadow", Horizontal_Position & " " &
                       Vertical_Position & " " & Blur & " " & Spread &
                       " " & Color & " " & Inset);
   end Shadow;

   -----------------
   -- Shadow_None --
   -----------------

   procedure Shadow_None (Element : in out Element_Type) is
   begin
      Element.Style ("box-shadow", "none");
   end Shadow_None;

   -------------
   -- Outline --
   -------------

   procedure Outline (Element : in out Element_Type;
                      Color   : in     String             := "invert";
                      Style   : in     Outline_Style_Type := None;
                      Width   : in     String             := "medium")
   is
   begin
      Element.Style ("outline", Color & " " & Style'Img & " " & Width);
   end Outline;

   ------------
   -- Margin --
   ------------

   procedure Margin (Element : in out Element_Type;
                     Top     : in     String := "0";
                     Right   : in     String := "0";
                     Bottom  : in     String := "0";
                     Left    : in     String := "0")
   is
   begin
      Element.Style ("margin", Top & " " & Right & " " & Bottom & " " & Left);
   end Margin;

   -------------
   -- Padding --
   -------------

   procedure Padding (Element : in out Element_Type;
                     Top     : in     String := "0";
                     Right   : in     String := "0";
                     Bottom  : in     String := "0";
                     Left    : in     String := "0")
   is
   begin
      Element.Style ("padding", Top & " " & Right & " " & Bottom & " " & Left);
   end Padding;

   ------------
   -- Cursor --
   ------------

   procedure Cursor (Element : in out Element_Type;
                     Value   : in     String)
   is
   begin
      Element.Style ("cursor", Value);
   end Cursor;

   function Cursor (Element : Element_Type) return String
   is
   begin
      return Element.Style ("cursor");
   end Cursor;

   ----------
   -- Font --
   ----------

   procedure Font (Element : in out Element_Type;
                   Family  : in     String            := "sans-serif";
                   Height  : in     String            := "medium";
                   Style   : in     Font_Style_Type   := Normal;
                   Weight  : in     Font_Weight_Type  := Weight_Normal;
                   Variant : in     Font_Variant_Type := Normal)
   is
      W : String := Weight'Img;
   begin
      Element.Style ("font", Style'Img & " " & Variant'Img & " " &
                       W (W'First + 7 .. W'Last) & " " & Height &
                       " " & Family);
   end Font;

   procedure Font (Element     : in out Element_Type;
                   System_Font : in     System_Font_Type)
   is
   begin
      case System_Font is
         when Caption | Icon | Menu =>
            Element.Style ("font", System_Font'Img);
         when Message_Box =>
            Element.Style ("font", "message-box");
         when Small_Caption =>
            Element.Style ("font", "small-caption");
         when Status_Bar =>
            Element.Style ("font", "status-bar");
      end case;
   end Font;

   --------------------
   -- Text_Alignment --
   --------------------

   procedure Text_Alignment (Element : in out Element_Type;
                             Value   : in     Alignment_Type)
   is
      V : String := Value'Img;
   begin
      case Value is
         when Left | Right | Center =>
            Element.Style ("text-align", V);
         when At_Start | To_End =>
            Element.Style ("text-align", V ((V'First + 3) .. V'Last));
      end case;
   end Text_Alignment;

   --------------------
   -- Vertical_Align --
   --------------------

   procedure Vertical_Align (Element : in out Element_Type;
                             Value   : in     Vertical_Align_Type)
   is
   begin
      if Value = Text_Top then
         Element.Style ("vertical-align", "text-top");
      elsif Value = Text_Bottom then
         Element.Style ("vertical-align", "text-bottom");
      else
         Element.Style ("vertical-align", Value'Img);
      end if;
   end Vertical_Align;

   -----------------
   -- First_Child --
   -----------------

   procedure First_Child (Element : in out Element_Type;
                          Child   : in out Element_Type'Class)
   is
   begin
      Child.Attach (Connection_ID => Element.Connection_ID,
                    ID            => Element.jQuery_Execute
                      ("children().first().attr('id');"),
                    ID_Type       => Gnoga.Types.DOM_ID);
   end First_Child;

   ------------------
   -- Next_Sibling --
   ------------------

   procedure Next_Sibling (Element : in out Element_Type;
                           Sibling : in out Element_Type'Class)
   is
   begin
      Sibling.Attach (Connection_ID => Element.Connection_ID,
                      ID            => Element.jQuery_Execute
                        ("next().attr('id');"),
                      ID_Type       => Gnoga.Types.DOM_ID);
   end Next_Sibling;

   --------------
   -- HTML_Tag --
   --------------

   function HTML_Tag (Element : Element_Type) return String is
   begin
      return Element.Property ("tagName");
   end HTML_Tag;

   -------------------------------------------------------------------------
   --  Element_Type - Methods
   -------------------------------------------------------------------------

   -------------------------
   -- Place_Inside_Top_Of --
   -------------------------

   procedure Place_Inside_Top_Of (Element : in out Element_Type;
                                  Target  : in out Element_Type'Class)
   is
   begin
      Target.jQuery_Execute ("prepend(" & Element.jQuery & ")");
   end Place_Inside_Top_Of;

   procedure Place_Inside_Bottom_Of (Element : in out Element_Type;
                                     Target  : in out Element_Type'Class)
   is
   begin
      Target.jQuery_Execute ("append(" & Element.jQuery & ")");
   end Place_Inside_Bottom_Of;

   procedure Place_Before (Element : in out Element_Type;
                           Target  : in out Element_Type'Class)
   is
   begin
      Element.jQuery_Execute ("insertBefore(" & Target.jQuery & ")");
   end Place_Before;

   procedure Place_After (Element : in out Element_Type;
                          Target  : in out Element_Type'Class)
   is
   begin
      Element.jQuery_Execute ("insertAfter(" & Target.jQuery & ")");
   end Place_After;

   ------------
   -- Remove --
   ------------

   procedure Remove (Element : in out Element_Type) is
      use type Gnoga.Types.ID_Enumeration;
   begin
      if Element.ID_Type = Gnoga.Types.DOM_ID then
         declare
            GID : constant String := Gnoga.Server.Connection.New_GID;
         begin
            Element.jQuery_Execute ("gnoga['" & GID & "']=" & Element.jQuery);
            Element.ID (GID, Gnoga.Types.Gnoga_ID);
         end;
      end if;

      Element.jQuery_Execute ("remove()");
   end Remove;

   -----------
   -- Click --
   -----------

   procedure Click (Element : in out Element_Type) is
   begin
      Element.Execute ("click();");
   end Click;

end Gnoga.Gui.Element;
