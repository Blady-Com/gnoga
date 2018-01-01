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

   --  Binding to the Ace Code Editor.
   --  Ace is an embeddable code editor written in JavaScript.
   --  http://ace.c9.io/#nav=about
   --  Ace is released under the BSD License.
   --  Some comments come from Ace documentation.

   procedure Load_Ace_Editor
     (Window : in out Gnoga.Gui.Window.Window_Type'Class);
   --  Load Ace_Editor code into Window

   -------------------------------------------------------------------------
   --  Ace_Editor_Type
   -------------------------------------------------------------------------

   type Ace_Editor_Type is new Gnoga.Gui.View.View_Type with private;
   type Ace_Editor_Access is access all Ace_Editor_Type;
   type Pointer_To_Ace_Editor_Class is access all Ace_Editor_Type'Class;

   --  All positions start at 0 if not stated otherwise
   type Position_Type is record
      Row, Column : Integer;
   end record;

   type Range_Type is record
      Start_Row, Start_Column, End_Row, End_Column : Integer;
   end record;

   -------------------------------------------------------------------------
   --  Ace_Editor_Type - Creation Method
   -------------------------------------------------------------------------

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
   --  Sets the current text to val.

   overriding
   function Text (View : Ace_Editor_Type) return String;
   --  Returns the current text's content.

   function Selected_Text (View : Ace_Editor_Type) return String;
   --  Returns the content for the selected text.

   procedure Insert_Text_At_Cursor (View : in out Ace_Editor_Type;
                                    Text : in     String);
   --  Inserts text into wherever the cursor is pointing.

   procedure Position (View : in out Ace_Editor_Type;
                       Pos  : in     Position_Type);
   --  Moves the cursor to the specified row and column.
   --  Note that this does not de-select the current selection.

   function Position (View : Ace_Editor_Type) return Position_Type;
   --  Gets the current position of the cursor.

   procedure Current_Line (View  : in out Ace_Editor_Type;
                           Value : in     Natural);
   --  Moves the cursor to the specified row.

   function Current_Line (View : Ace_Editor_Type) return Natural;
   --  Gets the current line of the cursor.

   procedure Current_Column (View  : in out Ace_Editor_Type;
                             Value : in     Natural);
   --  Moves the cursor to the specified column.

   function Current_Column (View : Ace_Editor_Type) return Natural;
   --  Gets the current column of the cursor.

   function Length (View : Ace_Editor_Type) return Natural;
   --  Total lines in View

   function Last_Column (View : Ace_Editor_Type; Row : Natural) return Natural;
   --  Last column of given row

   function Get_New_Line_Character (View : Ace_Editor_Type) return String;
   --  Returns the new line character that's being used, depending on the value of newLineMode.

   procedure Default_Tab_Size (View  : in out Ace_Editor_Type;
                               Value : in     Positive);
   pragma Obsolescent (Default_Tab_Size, "use Tab_Size instead");

   procedure Tab_Size
     (View  : in out Ace_Editor_Type;
      Value : in     Natural);
   --  Set the number of spaces that define a soft tab; for example, passing in 4 transforms
   --  the soft tabs to be equivalent to four spaces. This function also emits the changeTabSize event.

   function Tab_Size (View : Ace_Editor_Type) return Natural;
   --  Returns the current tab size.

   procedure Soft_Tabs (View  : in out Ace_Editor_Type;
                        Value : in     Boolean := True);
   --  Pass true to enable the use of soft tabs. Soft tabs means you're using spaces
   --  instead of the tab character ('\t').

   function Soft_Tabs (View : Ace_Editor_Type) return Boolean;
   --  Returns true if soft tabs are being used, false otherwise.

   procedure Word_Wrap (View  : in out Ace_Editor_Type;
                        Value : in     Boolean := True);
   pragma Obsolescent (Word_Wrap, "use Use_Wrap_Mode instead");

   procedure Line_Highlighting (View  : in out Ace_Editor_Type;
                                Value : in     Boolean := True);
   pragma Obsolescent (Line_Highlighting, "use Highlight_Active_Line instead");

   procedure Print_Margin_Visible (View  : in out Ace_Editor_Type;
                                   Value : in     Boolean := True);
   pragma Obsolescent (Print_Margin_Visible, "use Show_Print_Margin instead");

   procedure Show_Gutter (View  : in out Ace_Editor_Type;
                          Value : in     Boolean := True);
   --  Identifies whether you want to show the gutter or not.

   function Show_Gutter (View : Ace_Editor_Type) return Boolean;
   --  Returns true if the gutter is being shown.

   procedure Show_Invisibles (View  : in out Ace_Editor_Type;
                              Value : in     Boolean := True);
   --  If Value is set to true, invisible characters —like spaces or
   --  new lines— are shown in the editor.

   function Show_Invisibles (View : Ace_Editor_Type) return Boolean;
   --  Returns true if invisible characters are being shown.

   procedure Read_Only (View  : in out Ace_Editor_Type;
                        Value : in     Boolean := True);
   --  If Value is true, then the editor is set to read-only mode, and none of the content can change.

   function Read_Only (View : Ace_Editor_Type) return Boolean;
   --  Returns true if the editor is set to read-only mode.

   procedure Set_Highlight_Active_Line
     (View             : in out Ace_Editor_Type;
      Should_Highlight : in     Boolean);
   pragma Obsolescent (Set_Highlight_Active_Line, "use Highlight_Active_Line instead");

   procedure Highlight_Active_Line
     (View  : in out Ace_Editor_Type;
      Value : in     Boolean := True);
   --  Determines whether or not the current line should be highlighted.

   function Highlight_Active_Line (View : Ace_Editor_Type) return Boolean;
   --  Returns true if current lines are always highlighted.

   procedure Set_Highlight_Selected_Word
     (View             : in out Ace_Editor_Type;
      Should_Highlight : in     Boolean);
   pragma Obsolescent (Set_Highlight_Selected_Word, "use Highlight_Selected_Word instead");

   procedure Highlight_Selected_Word
     (View  : in out Ace_Editor_Type;
      Value : in     Boolean := True);
   --  Determines if the currently selected word should be highlighted.

   function Highlight_Selected_Word (View : Ace_Editor_Type) return Boolean;
   --  Returns true if currently highlighted words are to be highlighted.

   procedure Set_Overwrite
     (View      : in out Ace_Editor_Type;
      Overwrite : in     Boolean);
   pragma Obsolescent (Set_Overwrite, "use Overwrite instead");

   procedure Overwrite
     (View      : in out Ace_Editor_Type;
      Overwrite : in     Boolean := True);
   --  Pass in true to enable overwrites in your session, or false to disable.
   --  If overwrites is enabled, any text you enter will type over any text after it.
   --  If the value of overwrite changes, this function also emits the changeOverwrite event.

   function Overwrite (View : Ace_Editor_Type) return Boolean;
   --  Returns true if overwrites are enabled; false otherwise.

   procedure Set_Show_Print_Margin
     (View              : in out Ace_Editor_Type;
      Show_Print_Margin : in     Boolean);
   pragma Obsolescent (Set_Show_Print_Margin, "use Show_Print_Margin instead");

   procedure Show_Print_Margin
     (View  : in out Ace_Editor_Type;
      Value : in     Boolean := True);
   --  If Value is set to true, the print margin is shown in the editor.

   function Show_Print_Margin (View : Ace_Editor_Type) return Boolean;
   --  Returns true if the print margin is being shown.

   procedure Print_Margin
     (View  : in out Ace_Editor_Type;
      Value : in     Natural);
   --  Sets the column defining where the print margin should be.

   function Print_Margin (View : Ace_Editor_Type) return Natural;
   --  Returns the column number of where the print margin is.

   procedure Set_Use_Wrap_Mode
     (View          : in out Ace_Editor_Type;
      Use_Wrap_Mode : in     Boolean);
   pragma Obsolescent (Set_Use_Wrap_Mode, "use Use_Wrap_Mode instead");

   procedure Use_Wrap_Mode
     (View  : in out Ace_Editor_Type;
      Value : in     Boolean := True);
   --  Sets whether or not line wrapping is enabled. If useWrapMode is different than the current value,
   --  the 'changeWrapMode' event is emitted.

   function Use_Wrap_Mode (View : Ace_Editor_Type) return Boolean;
   --  Returns true if wrap mode is being used; false otherwise.

   procedure Set_Wrap_Limit_Range
     (View     : in out Ace_Editor_Type;
      Min, Max : in     Natural);
   --  Sets the boundaries of wrap. They can be the same number to pin the limit.
   --  If the wrap limits for min or max are different, this method also emits the 'changeWrapMode' event.

   procedure Wrap_Limit
     (View : in out Ace_Editor_Type;
      To   : in     Natural);
   --  Sets the limit of wrap.

   function Wrap_Limit (View : Ace_Editor_Type) return Natural;
   --  Returns the value of wrap limit.

   function First_Visible_Row (View : Ace_Editor_Type) return Natural;
   --  Returns the index of the first visible row.

   function Last_Visible_Row (View : Ace_Editor_Type) return Natural;
   --  Returns the index of the last visible row.

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
   --  Attempts to find text within the document.

   procedure Find_Next (View : in out Ace_Editor_Type);
   --  Performs another search for text in the document.

   procedure Find_Previous (View : in out Ace_Editor_Type);
   --  Performs a search for text backwards.

   procedure Replace_Text (View : in out Ace_Editor_Type;
                           Text : in     String);
   --  Replaces the first occurrence of searched text with the value in Text.

   procedure Replace_All (View : in out Ace_Editor_Type;
                          Text : in     String);
   --  Replaces all occurrences of searched text with the value in Text.

   procedure Set_Theme (View : in out Ace_Editor_Type;
                        Name : in     String);
   pragma Obsolescent (Set_Theme, "use Theme instead");

   procedure Theme (View : in out Ace_Editor_Type;
                    Name : in     String);
   --  Sets a new theme for the editor. theme should exist, and be a directory path, like ace/theme/textmate.

   function Theme (View : Ace_Editor_Type) return String;
   --  Returns the path of the current theme.

   procedure Set_Language_Mode (View     : in out Ace_Editor_Type;
                                Language : in     String);
   --  Sets a new language for the editor. it should exist in path ace/mode/.

   procedure Scroll_To_Line
     (View            : in out Ace_Editor_Type;
      Line            : in     Natural;
      Center, Animate : in     Boolean := False);
   --  Scrolls to a line. If center is true, it puts the line in middle of screen (or attempts to).

   procedure Move_Cursor_To
     (View        : in out Ace_Editor_Type;
      Row, Column : in     Natural);
   --  Moves the cursor to the specified row and column.
   --  Note that this does not de-select the current selection.

   procedure Navigate_To
     (View        : in out Ace_Editor_Type;
      Row, Column : in     Natural);
   --  Moves the cursor to the specified row and column.
   --  Note that this does de-select the current selection.

   procedure Navigate_Left
     (View  : in out Ace_Editor_Type;
      Times : in     Natural);
   --  Moves the cursor left in the document the specified number of times.
   --  Note that this does de-select the current selection.

   procedure Navigate_Right
     (View  : in out Ace_Editor_Type;
      Times : in     Natural);
   --  Moves the cursor right in the document the specified number of times.
   --  Note that this does de-select the current selection.

   procedure Navigate_Up
     (View  : in out Ace_Editor_Type;
      Times : in     Natural);
   --  Moves the cursor up in the document the specified number of times.
   --  Note that this does de-select the current selection.

   procedure Navigate_Down
     (View  : in out Ace_Editor_Type;
      Times : in     Natural);
   --  Moves the cursor down in the document the specified number of times.
   --  Note that this does de-select the current selection.

   procedure Navigate_Line_End (View : in out Ace_Editor_Type);
   --  Moves the cursor to the end of the current line.
   --  Note that this does de-select the current selection.

   procedure Navigate_Line_Start (View : in out Ace_Editor_Type);
   --  Moves the cursor to the start of the current line.
   --  Note that this does de-select the current selection.

   procedure Delete (View : in out Ace_Editor_Type);
   --  Removes after the cursor

   procedure Backspace (View : in out Ace_Editor_Type);
   --  Removes before the cursor

   procedure Remove_To_Line_End (View : in out Ace_Editor_Type);
   --  Removes all the words to the right of the current selection, until the end of the line.

   procedure Remove_To_Line_Start (View : in out Ace_Editor_Type);
   --  Removes all the words to the left of the current selection, until the start of the line.

   procedure Remove_In_Line
     (View                          : in out Ace_Editor_Type;
      Row, Start_Column, End_Column : in    Natural);
   --  Removes the specified columns from the row. This method also triggers the 'change' event.

   procedure Remove_Lines
     (View                : in out Ace_Editor_Type;
      First_Row, Last_Row : in     Natural);
   --  Removes a range of full lines. This method also triggers the 'change' event.

   procedure Insert
     (View        : in out Ace_Editor_Type;
      Row, Column : in     Natural;
      Text        : in     String);
   --  Inserts text into the position. This method also triggers the 'change' event.

   procedure InsertNewLine
     (View             : in out Ace_Editor_Type;
      Start_Row, Index :        Natural);
   pragma Obsolescent (InsertNewLine, "use Insert_New_Line instead");

   procedure Insert_New_Line
     (View        : in out Ace_Editor_Type;
      Row, Column : in     Natural);
   --  Inserts a new line into the document at the current row's position.
   --  This method also triggers the 'change' event.

   function Line (View : Ace_Editor_Type; Row : Natural) return String;
   --  Returns a verbatim copy of the given line as it is in the document

   function Text_Range (View : Ace_Editor_Type; From, To : Position_Type) return String;
   --  Given a range within the document, this function returns all the text within that range as a single string.

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
   function Editor_Execute (Editor : Ace_Editor_Type; Method : String)
                            return Boolean;
   --  Execute Methods on Editor

   -------------------------------------------------------------------------
   --  Ace_Editor_Type - Event Methods
   -------------------------------------------------------------------------

   overriding
   procedure On_Resize (View : in out Ace_Editor_Type);
   --  Let editor know View resized

   -------------------------------------------------------------------------
   --  Anchor_Type
   -------------------------------------------------------------------------

   type Anchor_Type is new Gnoga.Gui.Base.Base_Type with private;
   type Anchor_Access is access all Anchor_Type;
   type Pointer_To_Anchor_Class is access all Anchor_Type'Class;

   -------------------------------------------------------------------------
   --  Anchor_Type - Creation Method
   -------------------------------------------------------------------------

   procedure Create
     (Anchor      : in out Anchor_Type;
      Parent      : in out Ace_Editor_Type'Class;
      Row, Column : in     Natural);

   -------------------------------------------------------------------------
   --  Anchor_Type - Properties
   -------------------------------------------------------------------------

   procedure Position (Anchor  : in out Anchor_Type;
                       Pos     : in     Position_Type;
                       No_Clip : in     Boolean := False);
   --  Sets the anchor position to the specified row and column. If noClip is true, the position is not clipped.

   function Position (Anchor : Anchor_Type) return Position_Type;
   --  Returns the position identifying the row and column position of the current anchor.

   -------------------------------------------------------------------------
   --  Anchor_Type - Methods
   -------------------------------------------------------------------------

   procedure Insert_Text_At_Anchor (Anchor : in out Anchor_Type;
                                    Text   : in     String);
   --  Inserts text into wherever the anchor is pointing.

   procedure Insert_NewLine_At_Anchor (Anchor : in out Anchor_Type);
   pragma Obsolescent (Insert_NewLine_At_Anchor, "use Insert_New_Line_At_Anchor instead");

   procedure Insert_New_Line_At_Anchor (Anchor : in out Anchor_Type);
   --  Inserts a new line into the document at the anchor's position.

private
   type Ace_Editor_Type is new Gnoga.Gui.View.View_Type with
      record
         Script_ID : Gnoga.Types.Web_ID;
      end record;
   type Anchor_Type is new Gnoga.Gui.Base.Base_Type with null record;
end Gnoga.Gui.Plugin.Ace_Editor;
