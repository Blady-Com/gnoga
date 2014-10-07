------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--           G N O G A . G U I . P L U G I N . A C E _ E D I T O R          --
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

package body Gnoga.Gui.Plugin.Ace_Editor is

   procedure Load_Ace_Editor
     (Window : in out Gnoga.Gui.Window.Window_Type'Class)
   is
   begin
      Window.Document.Head_Element.jQuery_Execute
        ("append('<script src=""js/ace-builds/src-noconflict/ace.js""" &
           " type=""text/javascript"" charset=""utf-8""></script>')");
   end Load_Ace_Editor;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View          : in out Ace_Editor_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach        : in     Boolean := True;
      ID            : in     String  := "")
   is
      GID : String := Gnoga.Server.Connection.New_GID;
   begin
      Gnoga.Gui.View.View_Type (View).Create (Parent, Attach, ID);
      View.Script_ID := Ada.Strings.Unbounded.To_Unbounded_String
        ("gnoga['" & GID & "']");
      Gnoga.Server.Connection.Execute_Script
        (View.Connection_ID,
         View.Editor_Var & "=ace.edit(""" & View.ID & """);");
   end Create;

   ----------
   -- Text --
   ----------

   procedure Text (View : in out Ace_Editor_Type;
                   Text : in     String)
   is
   begin
      View.Editor_Execute ("setValue (""" & Escape_Quotes (Text) & """)");
   end Text;

   function Text (View : Ace_Editor_Type) return String is
   begin
      return View.Editor_Execute ("getValue ()");
   end Text;

   ---------------
   -- Set_Theme --
   ---------------

   procedure Set_Theme
     (View : in out Ace_Editor_Type;
      Name : in     String)
   is
   begin
      View.Editor_Execute ("setTheme(""ace/theme/" & Name & """)");
   end Set_Theme;

   -----------------------
   -- Set_Language_Mode --
   -----------------------

   procedure Set_Language_Mode
     (View     : in out Ace_Editor_Type;
      Language : in     String)
   is
   begin
      View.Editor_Execute
        ("getSession().setMode(""ace/mode/" & Language & """)");
   end Set_Language_Mode;

   ----------------
   -- Editor_Var --
   ----------------

   function Editor_Var (Editor : Ace_Editor_Type) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Editor.Script_ID);
   end Editor_Var;

   --------------------
   -- Editor_Execute --
   --------------------

   procedure Editor_Execute (Editor : in out Ace_Editor_Type; Method : String)
   is
      Message_Script : constant String := Editor.Editor_Var & "." & Method;
   begin
      Gnoga.Server.Connection.Execute_Script
        (ID     => Editor.Connection_ID,
         Script => Message_Script);
   end Editor_Execute;

   function Editor_Execute (Editor : Ace_Editor_Type; Method : String)
                            return String
   is
      Message_Script : constant String := Editor.Editor_Var & "." & Method;
   begin
      return Gnoga.Server.Connection.Execute_Script
        (ID     => Editor.Connection_ID,
         Script => Message_Script);
   end Editor_Execute;

   function Editor_Execute (Editor : Ace_Editor_Type; Method : String)
                            return Integer
   is
   begin
      return Integer'Value (Editor.Editor_Execute (Method));
   exception
      when others =>
         return 0;
   end Editor_Execute;

end Gnoga.Gui.Plugin.Ace_Editor;
