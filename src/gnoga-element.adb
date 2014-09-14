------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                       G N O G A . E L E M E N T                          --
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
------------------------------------------------------------------------------                                                                          --

with Ada.Strings.Unbounded;
with Gnoga.Connections;

package body Gnoga.Element is

   -------------------------------------------------------------------------
   --  Element_Type - Creation Methods
   -------------------------------------------------------------------------

   ----------------------
   -- Create_From_HTML --
   ----------------------
   
   procedure Create_From_HTML (Element : in out Element_Type;
                               Parent  : in out Gnoga.Base.Base_Type'Class;
                               HTML    : in     String;
                               ID      : in     String := "")
   is
      
      function Adjusted_ID return String is
         New_ID : Gnoga.Types.Unique_ID;
      begin
         if ID = "" then
            Gnoga.Connections.New_Unique_ID (New_ID);
            return "g" & Left_Trim (New_ID'Img);
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
   
   function Style (Element : Element_Type; Name : String) return String is
   begin
      return Element.jQuery_Execute ("css ('" & Name & "');");
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
   
   procedure Access_Key (Element : in out Element_Type; Value : String) is
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
   
   procedure Advisory_Title (Element : in out Element_Type; Value : String) is
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

   procedure Class_Name (Element : in out Element_Type; Value : String) is
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
   
   procedure Editable (Element : in out Element_Type; Value : Boolean := True) is
   begin
      Element.Property ("contentEditable", Value);
   end Editable;
   
   function Editable (Element : Element_Type) return Boolean is
   begin
      return Element.Property ("isContentEditable");
   end Editable;

   ---------------
   -- Draggable --
   ---------------
   
   procedure Draggable (Element : in out Element_Type; Value : Boolean := True) is
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
   
   procedure Hidden (Element : in out Element_Type; Value : Boolean := True) is
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
   
   procedure Inner_Html (Element : in out Element_Type; Value : String) is
   begin
      Element.jQuery_Execute ("html (""" & Escape_Quotes (Value) & """);");
   end Inner_Html;
   
   function Inner_Html (Element : Element_Type) return String is
   begin
      return Element.jQuery_Execute ("html();");
   end Inner_Html;

   -----------------
   -- Spell_Check --
   -----------------
   
   procedure Spell_Check (Element : in out Element_Type; Value : Boolean := True) is
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
   
   procedure Tab_Index (Element : in out Element_Type; Value : Natural)
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
   
   procedure Text (Element : in out Element_Type; Value : String) is
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
   
   procedure Text_Direction (Element : in out Element_Type; Value : String) is
   begin
      Element.Property ("dir", Value);
   end Text_Direction;

   function Text_Direction (Element : Element_Type) return String is
   begin
      return Element.Property ("dir");
   end Text_Direction;
   
   --------------------
   -- Language_Code --
   --------------------
   
   procedure Language_Code (Element : in out Element_Type; Value : String) is
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
   
   procedure Visible (Element : in out Element_Type; Value : Boolean := True)
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
   
   function Offset_Height (Element : Element_Type) return Natural is
   begin
      return Element.Property ("offsetHeight");
   end Offset_Height;
   
   ------------------
   -- Offset_Width --
   ------------------
   
   function Offset_Width (Element : Element_Type) return Natural is
   begin
      return Element.Property ("offsetWidth");
   end Offset_Width;

   ------------------
   -- Offset_Left --
   ------------------
   
   function Offset_Left (Element : Element_Type) return Natural is
   begin
      return Element.Property ("offsetLeft");
   end Offset_Left;

   ------------------
   -- Offset_Top --
   ------------------
   
   function Offset_Top (Element : Element_Type) return Natural is
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

   -----------------
   -- First_Child --
   -----------------
   
   procedure First_Child (Element : in out Element_Type;
                          Child   : in out Element_Type'Class)
   is
   begin
      Child.Attach (Connection_ID => Element.Connection_ID,
                    ID            => Element.Execute
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
                      ID            => Element.Execute
                        ("siblings().first().attr('id');"),
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
   
   -----------
   -- Click --
   -----------
   
   procedure Click (Element : in out Element_Type) is
   begin
      Element.Execute ("click();");
   end Click;
   
   -------------------------------------------------------------------------
   --  Element_Type - Events
   -------------------------------------------------------------------------

end Gnoga.Element;
