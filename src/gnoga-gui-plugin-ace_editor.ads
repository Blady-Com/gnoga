------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--           G N O G A . G U I . P L U G I N . A C E _ E D I T O R          --
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

with Gnoga.Types;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Base;

package Gnoga.Gui.Plugin.Ace_Editor is

   -------------------------------------------------------------------------
   --  Ace_Editor_Type
   -------------------------------------------------------------------------
   --  http://ace.c9.io/#nav=about
   --  Binding to the Ace Code Editor
   --
   --  use: make ace_editor to clone in the editor to Gnoga
   --  it will run cd js && git clone https://github.com/ajaxorg/ace-builds.git

   type Ace_Editor_Type is new Gnoga.Gui.View.View_Type with private;
   type Ace_Editor_Access is access all Ace_Editor_Type;
   type Pointer_To_Ace_Editor_Class is access all Ace_Editor_Type'Class;

   procedure Load_Ace_Editor
     (Window : in out Gnoga.Gui.Window.Window_Type'Class);
   --  Load Ace_Editor code in to Window

   overriding
   procedure Create
     (View          : in out Ace_Editor_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach        : in     Boolean := True;
      ID            : in     String  := "");

   -------------------------------------------------------------------------
   --  Ace_Editor_Type - Properties
   -------------------------------------------------------------------------

   overriding
   procedure Text (View : in out Ace_Editor_Type;
                   Text : in     String);
   overriding
   function Text (View : Ace_Editor_Type) return String;

   function Selected_Text (View : Ace_Editor_Type) return String;

   procedure Insert_Text_At_Cursor (View : in out Ace_Editor_Type;
                                    Text : in     String);

   procedure Current_Line (View  : in out Ace_Editor_Type;
                           Value : in     Positive);

   --   function Current_Line (View : Ace_Editor_Type) return Natural;

   --   function Current_Column (View : Ace_Editor_Type) return Natural;

   function Length (View : Ace_Editor_Type) return Natural;
   --  Total lines in View

   procedure Default_Tab_Size (View  : in out Ace_Editor_Type;
                               Value : in     Positive);

   procedure Soft_Tabs (View  : in out Ace_Editor_Type;
                        Value : in     Boolean := True);

   procedure Word_Wrap (View  : in out Ace_Editor_Type;
                        Value : in     Boolean := True);

   procedure Line_Highlighting (View  : in out Ace_Editor_Type;
                                Value : in     Boolean := True);

   procedure Print_Margin_Visible (View  : in out Ace_Editor_Type;
                                   Value : in     Boolean := True);

   procedure Read_Only (View  : in out Ace_Editor_Type;
                        Value : in     Boolean := True);

-------------------------------------------------------------------------
   --  Ace_Editor_Type - Methods
   -------------------------------------------------------------------------

   procedure Find_Text (View           : in out Ace_Editor_Type;
                        Text           : in     String;
                        Backwards      : in     Boolean := False;
                        Wrap           : in     Boolean := False;
                        Whole_Word     : in     Boolean := False;
                        Case_Sensitive : in     Boolean := False;
                        Reg_Exp        : in     Boolean := False);

   procedure Find_Next (View : in out Ace_Editor_Type);
   procedure Replace_Text (View : in out Ace_Editor_Type;
                           Text : in     String);
   procedure Replace_All (View : in out Ace_Editor_Type;
                          Text : in     String);
   --  Requires a previous Find_Text

   procedure Set_Theme (View : in out Ace_Editor_Type;
                        Name : in     String);

   procedure Set_Language_Mode (View     : in out Ace_Editor_Type;
                                Language : in     String);

   -------------------------------------------------------------------------
   --  Ace_Editor_Type - Internal Methods
   -------------------------------------------------------------------------

   function Editor_Var (Editor : Ace_Editor_Type) return String;

   procedure Editor_Execute (Editor : in out Ace_Editor_Type;
                             Method : in     String);
   function Editor_Execute (Editor : Ace_Editor_Type; Method : String)
                            return String;
   function Editor_Execute (Editor : Ace_Editor_Type; Method : String)
                            return Integer;
   --  Execute Method on Editor

   -------------------------------------------------------------------------
   --  Ace_Editor_Type - Event Methods
   -------------------------------------------------------------------------

   overriding
   procedure On_Resize (View : in out Ace_Editor_Type);
   --  Let editor know View resized

private
   type Ace_Editor_Type is new Gnoga.Gui.View.View_Type with
      record
         Script_ID : Gnoga.Types.Web_ID;
      end record;
end Gnoga.Gui.Plugin.Ace_Editor;
