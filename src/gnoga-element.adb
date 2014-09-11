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

   -------------------
   -- Create_Inside --
   -------------------
   
   procedure Create_Inside (Object        : in out Element_Type;
                            Parent        : in out Element_Type'Class;
                            ID            : in     String;
                            HTML          : in     String)
   is
   begin
      Object.Create_With_Script
        (Connection_ID => Parent.Connection_ID,
         ID            => ID,
         Script        => Element_Type (Parent).jQuery &
           ".html(""" & Escape_Quotes (HTML) & """);");
   end Create_Inside;
   
   --------------------------
   -- Create_Inside_At_Top --
   --------------------------
   
   procedure Create_Inside_At_Top (Object        : in out Element_Type;
                                   Parent        : in out Element_Type'Class;
                                   ID            : in     String;
                                   HTML          : in     String)
   is
   begin
      Object.Create_With_Script
        (Connection_ID => Parent.Connection_ID,
         ID            => ID,
         Script        => Element_Type (Parent).jQuery &
           ".prepend($(""" & Escape_Quotes (HTML) &
           """).prop('id','" & ID & "'));");      
   end Create_Inside_At_Top;
   
   -----------------------------
   -- Create_Inside_At_Bottom --
   -----------------------------
   
   procedure Create_Inside_At_Bottom (Object        : in out Element_Type;
                                      Parent        : in out Element_Type'Class;
                                      ID            : in     String;
                                      HTML          : in     String)
   is
   begin
      Object.Create_With_Script
        (Connection_ID => Parent.Connection_ID,
         ID            => ID,
         Script        => Element_Type (Parent).jQuery &
           ".append($(""" & Escape_Quotes (HTML) &
           """).prop('id','" & ID & "'));");      
   end Create_Inside_At_Bottom;
   
   ------------------
   -- Create_After --
   ------------------
   
   procedure Create_After (Object        : in out Element_Type;
                           Target        : in out Element_Type'Class;
                           ID            : in     String;
                           HTML          : in     String)
   is
   begin
      Object.Create_With_Script
        (Connection_ID => Target.Connection_ID,
         ID            => ID,
         Script        => "$(""" & Escape_Quotes (HTML) &
           """).insertAfter(""#" &
           Target.ID & """).prop('id','" & ID & "');");
   end Create_After;

   -------------------
   -- Create_Before --
   -------------------
   
   procedure Create_Before (Object        : in out Element_Type;
                            Target        : in out Element_Type'Class;
                            ID            : in     String;
                            HTML          : in     String)
   is
   begin
      Object.Create_With_Script
        (Connection_ID => Target.Connection_ID,
         ID            => ID,
         Script        => "$(""" & Escape_Quotes (HTML) &
           """).insertBefore(""#" & Target.ID &
           """).prop('id','" & ID & "');");
   end Create_Before;


   -------------------------------------------------------------------------
   --  Element_Type - Properties
   -------------------------------------------------------------------------
   
   -----------
   -- Style --
   -----------
   
   procedure Style (Object : in out Element_Type;
                    Name   : in String;
                    Value  : in String)
   is
      Message_Script : constant String := jQuery(Object) &
        ".css ('" & Name & "', """ & Escape_Quotes (Value) & """);";
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);               
   end Style;
   
   function Style (Object : Element_Type; Name : String) return String is
      Message_Script : constant String := jQuery(Object) &
        ".css ('" & Name & "');";
   begin
      return Gnoga.Connections.Execute_Script (ID     => Object.Connection_ID,
                                               Script => Message_Script);
   end Style;

   
   ---------------
   -- Attribute --
   ---------------
   
   procedure Attribute (Object : in out Element_Type;
                        Name   : in String;
                        Value  : in String)
   is
      Message_Script : constant String := jQuery(Object) &
        ".attr ('" & Name & "',""" & Escape_Quotes (Value) & """);";
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);               
   end Attribute;
         
   function Attribute (Object : Element_Type; Name : String) return String is
      Message_Script : constant String := jQuery(Object) &
        ".attr ('" & Name & "');";
   begin
      return Gnoga.Connections.Execute_Script (ID     => Object.Connection_ID,
                                               Script => Message_Script);
   end Attribute;

   ----------------
   -- Access_Key --
   ----------------
   
   procedure Access_Key (Object : in out Element_Type; Value : String) is
   begin
      Object.Property ("accessKey", Value);
   end Access_Key;

   function Access_Key (Object : Element_Type) return String is
   begin
      return Object.Property ("accessKey");
   end Access_Key;

   --------------------
   -- Advisory_Title --
   --------------------
   
   procedure Advisory_Title (Object : in out Element_Type; Value : String) is
   begin
      Object.Property ("title", Value);
   end Advisory_Title;

   function Advisory_Title (Object : Element_Type) return String is
   begin
      return Object.Property ("title");
   end Advisory_Title;


   ----------------
   -- Class_Name --
   ----------------

   procedure Class_Name (Object : in out Element_Type; Value : String) is
   begin
      Object.Property ("className", Value);
   end Class_Name;

   function Class_Name (Object : Element_Type) return String is
   begin
      return Object.Property ("className");
   end Class_Name;
   
   ---------------
   -- Editable --
   ---------------
   
   procedure Editable (Object : in out Element_Type; Value : Boolean := True) is
   begin
      Object.Property ("contentEditable", Value);
   end Editable;
   
   function Editable (Object : Element_Type) return Boolean is
   begin
      return Object.Property ("isContentEditable");
   end;

   procedure Tab_Index (Object : in out Element_Type; Value : Natural)
   is
   begin
      Object.Property ("tabIndex", Value);
   end Tab_Index;
   
   function Tab_Index (Object : Element_Type) return Natural is
   begin
      return Object.Property ("tabIndex");
   end Tab_Index;
   
   --------------------
   -- Text_Direction --
   --------------------
   
   procedure Text_Direction (Object : in out Element_Type; Value : String) is
   begin
      Object.Property ("dir", Value);
   end Text_Direction;

   function Text_Direction (Object : Element_Type) return String is
   begin
      return Object.Property ("dir");
   end Text_Direction;
   
   --------------------
   -- Language_Code --
   --------------------
   
   procedure Language_Code (Object : in out Element_Type; Value : String) is
   begin
      Object.Property ("lang", Value);
   end Language_Code;

   function Language_Code (Object : Element_Type) return String is
   begin
      return Object.Property ("lang");
   end Language_Code;

   
   -------------
   -- Visible --
   -------------
   
   procedure Visible (Object : in out Element_Type; Value : Boolean := True)
   is
   begin
      if Value then
         Object.Style ("visibility", "visible");
      else
         Object.Style ("visibility", "hidden");
      end if;
   end Visible;
   
   function Visible (Object : Element_Type) return Boolean
   is
   begin
      return Object.Style ("visibility") = "visible";
   end Visible;

   -------------------
   -- Client_Height --
   -------------------
   
   function Client_Height (Object : Element_Type) return Integer is
   begin
      return Object.Property ("clientHeight");
   end Client_Height;
   
   ------------------
   -- Client_Width --
   ------------------
   
   function Client_Width (Object : Element_Type) return Integer is
   begin
      return Object.Property ("clientWidth");
   end Client_Width;
   
   -------------------
   -- Offset_Height --
   -------------------
   
   function Offset_Height (Object : Element_Type) return Integer is
   begin
      return Object.Property ("offsetHeight");
   end Offset_Height;
   
   ------------------
   -- Offset_Width --
   ------------------
   
   function Offset_Width (Object : Element_Type) return Integer is
   begin
      return Object.Property ("offsetWidth");
   end Offset_Width;

   ------------------
   -- Offset_Left --
   ------------------
   
   function Offset_Left (Object : Element_Type) return Integer is
   begin
      return Object.Property ("offsetLeft");
   end Offset_Left;

   ------------------
   -- Offset_Top --
   ------------------
   
   function Offset_Top (Object : Element_Type) return Integer is
   begin
      return Object.Property ("offsetTop");
   end Offset_Top;   
   
   -----------------
   -- First_Child --
   -----------------
   
   procedure First_Child (Object : in out Element_Type;
                          Child  : in out Element_Type'Class)
   is
   begin
      Child.Attach (Connection_ID => Object.Connection_ID,
                    ID            => Object.Execute
                      ("children().first().attr('id');"),
                    ID_Type       => Gnoga.Types.DOM_ID);
   end First_Child;
   
   ------------------
   -- Next_Sibling --
   ------------------
   
   procedure Next_Sibling (Object : in out Element_Type;
                           Sibling : in out Element_Type'Class)
   is
   begin
      Sibling.Attach (Connection_ID => Object.Connection_ID,
                      ID            => Object.Execute
                        ("siblings().first().attr('id');"),
                      ID_Type       => Gnoga.Types.DOM_ID);
   end Next_Sibling;

   --------------
   -- HTML_Tag --
   --------------
   
   function HTML_Tag (Object : Element_Type) return String is
   begin
      return Object.Property ("tagName");
   end HTML_Tag;
   
   -------------------------------------------------------------------------
   --  Element_Type - Methods
   -------------------------------------------------------------------------

   
   -------------------------------------------------------------------------
   --  Element_Type - Events
   -------------------------------------------------------------------------

end Gnoga.Element;
