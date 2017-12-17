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
   --  use: make ace_editor to clone in the editor javascript code for demo
   --  it will run cd js && git clone https://github.com/ajaxorg/ace-builds.git
   --  the custom Ace javascript code should then be copied in to or checked
   --  out in the same way for your project.

   type Ace_Editor_Type is new Gnoga.Gui.View.View_Type with private;
   type Ace_Editor_Access is access all Ace_Editor_Type;
   type Pointer_To_Ace_Editor_Class is access all Ace_Editor_Type'Class;

   type Anchor_Type is new Gnoga.Gui.Base.Base_Type with private;
   type Anchor_Access is access all Anchor_Type;
   type Pointer_To_Anchor_Class is access all Anchor_Type'Class;

   type Position_Type is record
      Row, Column : Integer;
   end record;

   type Range_Type is record
      Start_Row, Start_Column, End_Row, End_Column : Integer;
   end record;

   procedure Load_Ace_Editor
     (Window : in out Gnoga.Gui.Window.Window_Type'Class);
   --  Load Ace_Editor code in to Window

   overriding
   procedure Create
     (View          : in out Ace_Editor_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class;
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
   --  Starts at 1

   function Current_Line (View : Ace_Editor_Type) return Natural;
   --  Starts at 0

   function Current_Column (View : Ace_Editor_Type) return Natural;
   --  Starts at 0

   function Length (View : Ace_Editor_Type) return Natural;
   --  Total lines in View

   function Last_Column (View : Ace_Editor_Type; Row : Natural) return Natural;
   --  Last column of given row, starts at 0

   function Get_New_Line_Character
     (View : Gnoga.Gui.Plugin.Ace_Editor.Ace_Editor_Type) return String;

   procedure Default_Tab_Size (View  : in out Ace_Editor_Type;
                               Value : in     Positive);

   procedure Soft_Tabs (View  : in out Ace_Editor_Type;
                        Value : in     Boolean := True);

   procedure Word_Wrap (View  : in out Ace_Editor_Type;
                        Value : in     Boolean := True);

   procedure Line_Highlighting (View  : in out Ace_Editor_Type;
                                Value : in     Boolean := True);
   pragma Obsolescent (Line_Highlighting);

   procedure Print_Margin_Visible (View  : in out Ace_Editor_Type;
                                   Value : in     Boolean := True);

   procedure Show_Gutter (View  : in out Ace_Editor_Type;
                          Value : in     Boolean := True);

   procedure Show_Invisibles (View  : in out Ace_Editor_Type;
                              Value : in     Boolean := True);

   procedure Read_Only (View  : in out Ace_Editor_Type;
                        Value : in     Boolean := True);

   procedure Set_Highlight_Active_Line
     (View             : in out Ace_Editor_Type;
      Should_Highlight :        Boolean);

   procedure Set_Highlight_Selected_Word
     (View             : in out Ace_Editor_Type;
      Should_Highlight :        Boolean);

   procedure Set_Overwrite
     (View      : in out Ace_Editor_Type;
      Overwrite :        Boolean);

   procedure Set_Show_Print_Margin
     (View              : in out Ace_Editor_Type;
      Show_Print_Margin :        Boolean);

   procedure Set_Use_Wrap_Mode
     (View          : in out Ace_Editor_Type;
      Use_Wrap_Mode :        Boolean);
   pragma Obsolescent (Set_Use_Wrap_Mode);

   procedure Set_Wrap_Limit_Range
     (View     : in out Ace_Editor_Type;
      Min, Max :        Natural);

   procedure Wrap_Limit
     (View     : in out Ace_Editor_Type;
      To :        Natural);

   function Wrap_Limit (View : Ace_Editor_Type) return Natural;

   function First_Visible_Row (View : Ace_Editor_Type) return Natural;

   function Last_Visible_Row (View : Ace_Editor_Type) return Natural;

   -------------------------------------------------------------------------
   --  Ace_Editor_Type - Methods
   -------------------------------------------------------------------------

   procedure Find_Text (View           : in out Ace_Editor_Type;
                        Text           : in     String;
                        Backwards      : in     Boolean := False;
                        Wrap           : in     Boolean := False;
                        Whole_Word     : in     Boolean := False;
                        Case_Sensitive : in     Boolean := False;
                        Reg_Exp        : in     Boolean := False;
                        Search_Range   : in     Range_Type := (-1, -1, -1, -1);
                        Start          : in     Position_Type := (-1, -1);
                        Skip_Current   : in     Boolean := False;
                        Animate        : in     Boolean := False);

   procedure Find_Next (View : in out Ace_Editor_Type);

   procedure Find_Previous (View : in out Ace_Editor_Type);

   procedure Replace_Text (View : in out Ace_Editor_Type;
                           Text : in     String);

   procedure Replace_All (View : in out Ace_Editor_Type;
                          Text : in     String);
   --  Requires a previous Find_Text

   procedure Set_Theme (View : in out Ace_Editor_Type;
                        Name : in     String);

   procedure Set_Language_Mode (View     : in out Ace_Editor_Type;
                                Language : in     String);

   procedure Scroll_To_Line
     (View            : in out Ace_Editor_Type;
      Line            :        Natural;
      Center, Animate :        Boolean := False);

   procedure Move_Cursor_To
     (View        : in out Ace_Editor_Type;
      Row, Column :        Natural);

   procedure Navigate_To
     (View        : in out Ace_Editor_Type;
      Row, Column :        Natural);

   procedure Navigate_Left
     (View  : in out Ace_Editor_Type;
      Times :        Natural);

   procedure Navigate_Right
     (View  : in out Ace_Editor_Type;
      Times :        Natural);

   procedure Navigate_Up
     (View  : in out Ace_Editor_Type;
      Times :        Natural);

   procedure Navigate_Down
     (View  : in out Ace_Editor_Type;
      Times :        Natural);

   procedure Navigate_Line_End (View : in out Ace_Editor_Type);

   procedure Navigate_Line_Start (View : in out Ace_Editor_Type);

   procedure Delete (View : in out Ace_Editor_Type);

   procedure Backspace (View : in out Ace_Editor_Type);

   procedure Remove_To_Line_End (View : in out Ace_Editor_Type);

   procedure Remove_To_Line_Start (View : in out Ace_Editor_Type);

   procedure Remove_In_Line
     (View                          : in out Ace_Editor_Type;
      Row, Start_Column, End_Column :        Natural);

   procedure Remove_Lines
     (View                : in out Ace_Editor_Type;
      First_Row, Last_Row :        Natural);

   procedure Insert
     (View             : in out Ace_Editor_Type;
      Start_Row, Index :        Natural;
      Text             :        String);

   procedure InsertNewLine
     (View             : in out Ace_Editor_Type;
      Start_Row, Index :        Natural);

   function Line (View : Ace_Editor_Type; Row : Natural) return String;

   function Text_Range (View : Ace_Editor_Type; From, To : Position_Type) return String;

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

   -------------------------------------------------------------------------
   --  Anchor_Type - Creation Method
   -------------------------------------------------------------------------

   procedure Create
     (Anchor : in out Anchor_Type;
      Parent    : in out Ace_Editor_Type'Class;
      Row, Column : Natural);

   -------------------------------------------------------------------------
   --  Anchor_Type - Properties
   -------------------------------------------------------------------------

   procedure Position (Anchor : in out Anchor_Type; Pos : Position_Type; No_Clip : Boolean := False);

   function Position (Anchor : Anchor_Type) return Position_Type;

   -------------------------------------------------------------------------
   --  Anchor_Type - Methods
   -------------------------------------------------------------------------

   procedure Insert_Text_At_Anchor (Anchor : in out Anchor_Type;
                                    Text : in     String);

   procedure Insert_NewLine_At_Anchor (Anchor : in out Anchor_Type);

private
   type Ace_Editor_Type is new Gnoga.Gui.View.View_Type with
      record
         Script_ID : Gnoga.Types.Web_ID;
      end record;
   type Anchor_Type is new Gnoga.Gui.Base.Base_Type with null record;
end Gnoga.Gui.Plugin.Ace_Editor;
