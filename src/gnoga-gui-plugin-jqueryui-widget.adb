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

   -------------
   --  Create --
   -------------

   overriding
   procedure Create
     (View    : in out Accordion_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach  : in     Boolean := True;
      ID      : in     String  := "")
   is
   begin
      Gnoga.Gui.View.View_Type (View).Create (Parent, Attach, ID);
   end Create;

   procedure Create_Section
     (View : in out Accordion_Type; Heading : String)
   is
   begin
      View.Put_HTML ("<h3>" & Escape_Quotes (Heading) & "</h3>");
   end Create_Section;

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

   ---------------
   -- Make_Menu --
   ---------------

   procedure Make_Menu
     (List : in out Gnoga.Gui.Element.List.Unordered_List_Type'Class)
   is
   begin
      List.jQuery_Execute ("menu()");
   end Make_Menu;

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
