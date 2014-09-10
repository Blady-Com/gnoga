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
           ".prepend(""" & Escape_Quotes (HTML) & """);");      
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
           ".append(""" & Escape_Quotes (HTML) & """);");      
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
         Script        => "$(""" & Escape_Quotes (HTML) & """).insertAfter(""#" &
           Target.ID & """);");
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
           """).insertBefore(""#" & Target.ID & """);");
   end Create_Before;


   -------------------------------------------------------------------------
   --  Element_Type - Properties
   -------------------------------------------------------------------------

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
        ".attr ('" & Name & "')=""" & Escape_Quotes (Value) & """";
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

   -------------------------------------------------------------------------
   --  Element_Type - Methods
   -------------------------------------------------------------------------

   
   -------------------------------------------------------------------------
   --  Element_Type - Events
   -------------------------------------------------------------------------

end Gnoga.Element;
