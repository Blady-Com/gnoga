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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Exceptions;

with Gnoga.Server.Connection;

package body Gnoga.Gui.Plugin.Ace_Editor is

   procedure Load_Ace_Editor
     (Window : in out Gnoga.Gui.Window.Window_Type'Class)
   is
   begin
      Window.Document.Head_Element.jQuery_Execute
      ("append('" & Escape_Quotes ("<script src='/js/ace-builds/src-min-noconflict/ace.js'" &
       " type='text/javascript' charset='utf-8'></script>") & "')");
   end Load_Ace_Editor;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View   : in out Ace_Editor_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String := "")
   is
      GID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
      View.Script_ID :=
        Ada.Strings.Unbounded.To_Unbounded_String ("gnoga['" & GID & "']");
      Gnoga.Server.Connection.Execute_Script
        (View.Connection_ID,
         View.Editor_Var & "=ace.edit('" & View.ID & "');");
   end Create;

   ----------
   -- Text --
   ----------

   overriding procedure Text
     (View : in out Ace_Editor_Type;
      Text : in     String)
   is
   begin
      View.Editor_Execute ("setValue ('" & Escape_Quotes (Text) & "')");
   end Text;

   overriding function Text (View : Ace_Editor_Type) return String is
   begin
      return View.Editor_Execute ("getValue ()");
   end Text;

   -------------------
   -- Selected_Text --
   -------------------

   function Selected_Text (View : Ace_Editor_Type) return String is
   begin
      return View.Editor_Execute
        ("getSession().getTextRange(" & View.Editor_Var & ".getSelectionRange())");
   end Selected_Text;

   ---------------------------
   -- Insert_Text_At_Cursor --
   ---------------------------

   procedure Insert_Text_At_Cursor
     (View : in out Ace_Editor_Type;
      Text : in     String)
   is
   begin
      View.Editor_Execute ("insert('" & Escape_Quotes (Text) & "')");
   end Insert_Text_At_Cursor;

   --------------
   -- Position --
   --------------

   procedure Position (View : in out Ace_Editor_Type;
                       Pos  : in     Position_Type) is
   begin
      View.Editor_Execute ("moveCursorTo(" & Pos.Row'Img & ',' & Pos.Column'Img & ')');
   end Position;

   function Position (View : Ace_Editor_Type) return Position_Type is
   begin
      return (View.Editor_Execute ("getCursorPosition().row"),
              View.Editor_Execute ("getCursorPosition().column"));
   end Position;

   ------------------
   -- Current_Line --
   ------------------

   procedure Current_Line
     (View  : in out Ace_Editor_Type;
      Value : in     Natural)
   is
   begin
      View.Position ((View.Position.Row, Value));
   end Current_Line;

   function Current_Line (View : Ace_Editor_Type) return Natural is
   begin
      return View.Position.Row;
   end Current_Line;

   ------------------
   -- Current_Column --
   ------------------

   procedure Current_Column
     (View  : in out Ace_Editor_Type;
      Value : in     Natural)
   is
   begin
      View.Position ((Value, View.Position.Column));
   end Current_Column;

   function Current_Column (View : Ace_Editor_Type) return Natural is
   begin
      return View.Position.Column;
   end Current_Column;

   ------------
   -- Length --
   ------------

   function Length (View : Ace_Editor_Type) return Natural is
   begin
      return View.Editor_Execute ("getSession().getLength()");
   end Length;

   -----------------
   -- Last_Column --
   -----------------

   function Last_Column (View : Ace_Editor_Type; Row : Natural) return Natural is
   begin
      return View.Editor_Execute ("getSession().getDocumentLastRowColumn(" & Row'Img & ",0)");
   end Last_Column;

   --------------
   -- Tab_Size --
   --------------

   procedure Tab_Size
     (View  : in out Ace_Editor_Type;
      Value : in     Natural)
   is
   begin
      View.Editor_Execute
      ("getSession().setTabSize(" & Value'Img & ')');
   end Tab_Size;

   function Tab_Size (View : Ace_Editor_Type) return Natural
   is
   begin
      return View.Editor_Execute ("getSession().getTabSize()");
   end Tab_Size;

   procedure Default_Tab_Size
     (View  : in out Ace_Editor_Type;
      Value : in     Positive)
   is
   begin
      View.Editor_Execute ("getSession().setTabSize(" & Value'Img & ")");
   end Default_Tab_Size;

   ---------------
   -- Soft_Tabs --
   ---------------

   procedure Soft_Tabs
     (View  : in out Ace_Editor_Type;
      Value : in     Boolean := True)
   is
   begin
      View.Editor_Execute ("getSession().setUseSoftTabs(" & Value'Img & ")");
   end Soft_Tabs;

   function Soft_Tabs (View : Ace_Editor_Type) return Boolean
   is
   begin
      return View.Editor_Execute ("getSession().getUseSoftTabs()");
   end Soft_Tabs;

   ---------------
   -- Word_Wrap --
   ---------------

   procedure Word_Wrap
     (View  : in out Ace_Editor_Type;
      Value : in     Boolean := True)
   is
   begin
      View.Editor_Execute ("getSession().setUseWrapMode(" & Value'Img & ")");
   end Word_Wrap;

   -----------------------
   -- Line_Highlighting --
   -----------------------

   procedure Line_Highlighting
     (View  : in out Ace_Editor_Type;
      Value : in     Boolean := True)
   is
   begin
      View.Editor_Execute ("setHighlightActiveLine(" & Value'Img & ")");
   end Line_Highlighting;

   --------------------------
   -- Print_Margin_Visible --
   --------------------------

   procedure Print_Margin_Visible
     (View  : in out Ace_Editor_Type;
      Value : in     Boolean := True)
   is
   begin
      View.Editor_Execute ("setShowPrintMargin(" & Value'Img & ")");
   end Print_Margin_Visible;

   -----------------
   -- Show_Gutter --
   -----------------

   procedure Show_Gutter
     (View  : in out Ace_Editor_Type;
      Value : in     Boolean := True)
   is
   begin
      View.Editor_Execute ("renderer.setShowGutter(" & Value'Img & ")");
   end Show_Gutter;

   function Show_Gutter (View : Ace_Editor_Type) return Boolean
   is
   begin
      return View.Editor_Execute ("renderer.getShowGutter()");
   end Show_Gutter;

   ---------------------
   -- Show_Invisibles --
   ---------------------

   procedure Show_Invisibles
     (View  : in out Ace_Editor_Type;
      Value : in     Boolean := True)
   is
   begin
      View.Editor_Execute ("setShowInvisibles(" & Value'Img & ")");
   end Show_Invisibles;

   function Show_Invisibles (View : Ace_Editor_Type) return Boolean
   is
   begin
      return View.Editor_Execute ("getShowInvisibles()");
   end Show_Invisibles;

   ---------------
   -- Read_Only --
   ---------------

   procedure Read_Only
     (View  : in out Ace_Editor_Type;
      Value : in     Boolean := True)
   is
   begin
      View.Editor_Execute ("setReadOnly(" & Value'Img & ")");
   end Read_Only;

   function Read_Only (View : Ace_Editor_Type) return Boolean
   is
   begin
      return View.Editor_Execute ("getReadOnly()");
   end Read_Only;

   ---------------
   -- Find_Text --
   ---------------

   procedure Find_Text
     (View           : in out Ace_Editor_Type;
      Text           : in     String;
      Backwards      : in     Boolean       := False;
      Wrap           : in     Boolean       := False;
      Whole_Word     : in     Boolean       := False;
      Case_Sensitive : in     Boolean       := False;
      Reg_Exp        : in     Boolean       := False;
      Search_Range   : in     Range_Type    := (-1, -1, -1, -1);
      Start          : in     Position_Type := (-1, -1);
      Skip_Current   : in     Boolean       := False;
      Animate        : in     Boolean       := False)
   is
   begin
      View.Editor_Execute
      ("find('" &
       Escape_Quotes (Text) &
       "',{" &
       "backwards: " &
       Backwards'Img &
       "," &
       "wrap: " &
       Wrap'Img &
       "," &
       "caseSensitive: " &
       Case_Sensitive'Img &
       "," &
       "wholeWord: " &
       Whole_Word'Img &
       "," &
       "regExp: " &
       Reg_Exp'Img &
       (if
          Search_Range.Start_Row = -1
        then
          ",range: {start:{row:" &
          Search_Range.Start_Row'Img &
          ",column:" &
          Search_Range.Start_Column'Img &
          "},end:{row:" &
          Search_Range.End_Row'Img &
          ",column:" &
          Search_Range.End_Column'Img &
          "}}"
        else "") &
       (if
          Start.Row = -1
        then
          ",start: {row:" & Start.Row'Img & ",column:" & Start.Column'Img & '}'
        else "") &
       "," &
       "skipCurrent: " &
       Skip_Current'Img &
       "}," &
       Animate'Img &
       ")");
   end Find_Text;

   ---------------
   -- Find_Next --
   ---------------

   procedure Find_Next (View : in out Ace_Editor_Type) is
   begin
      View.Editor_Execute ("findNext()");
   end Find_Next;

   -------------------
   -- Find_Previous --
   -------------------

   procedure Find_Previous (View : in out Ace_Editor_Type) is
   begin
      View.Editor_Execute ("findPrevious()");
   end Find_Previous;

   ------------------
   -- Replace_Text --
   ------------------

   procedure Replace_Text (View : in out Ace_Editor_Type; Text : in String) is
   begin
      View.Editor_Execute ("replace('" & Escape_Quotes (Text) & "')");
   end Replace_Text;

   -----------------
   -- Replace_All --
   -----------------

   procedure Replace_All (View : in out Ace_Editor_Type; Text : in String) is
   begin
      View.Editor_Execute ("replaceAll('" & Escape_Quotes (Text) & "')");
   end Replace_All;

   -----------
   -- Theme --
   -----------

   procedure Theme (View : in out Ace_Editor_Type; Name : in String) is
   begin
      View.Editor_Execute ("setTheme('ace/theme/" & Name & "')");
   end Theme;

   function Theme (View : Ace_Editor_Type) return String
   is
   begin
      return View.Editor_Execute ("getTheme()");
   end Theme;

   procedure Set_Theme (View : in out Ace_Editor_Type; Name : in String) is
   begin
      View.Editor_Execute ("setTheme('ace/theme/" & Name & "')");
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
      ("getSession().setMode('ace/mode/" & Language & "')");
   end Set_Language_Mode;

   --------------------
   -- Scroll_To_Line --
   --------------------

   procedure Scroll_To_Line
     (View            : in out Ace_Editor_Type;
      Line            :        Natural;
      Center, Animate :        Boolean := False)
   is
   begin
      View.Editor_Execute
      ("scrollToLine(" &
       Line'Img &
       ',' &
       Center'Img &
       ',' &
       Animate'Img &
       ')');
   end Scroll_To_Line;

   --------------------
   -- Move_Cursor_To --
   --------------------

   procedure Move_Cursor_To
     (View        : in out Ace_Editor_Type;
      Row, Column :        Natural)
   is
   begin
      View.Editor_Execute ("moveCursorTo(" & Row'Img & ',' & Column'Img & ')');
   end Move_Cursor_To;

   -----------------
   -- Navigate_To --
   -----------------

   procedure Navigate_To
     (View        : in out Ace_Editor_Type;
      Row, Column :        Natural)
   is
   begin
      View.Editor_Execute ("navigateTo(" & Row'Img & ',' & Column'Img & ')');
   end Navigate_To;

   -------------------
   -- Navigate_Left --
   -------------------

   procedure Navigate_Left (View : in out Ace_Editor_Type; Times : Natural) is
   begin
      View.Editor_Execute ("navigateLeft(" & Times'Img & ')');
   end Navigate_Left;

   --------------------
   -- Navigate_Right --
   --------------------

   procedure Navigate_Right (View : in out Ace_Editor_Type; Times : Natural) is
   begin
      View.Editor_Execute ("navigateRight(" & Times'Img & ')');
   end Navigate_Right;

   -----------------
   -- Navigate_Up --
   -----------------

   procedure Navigate_Up (View : in out Ace_Editor_Type; Times : Natural) is
   begin
      View.Editor_Execute ("navigateUp(" & Times'Img & ')');
   end Navigate_Up;

   -------------------
   -- Navigate_Down --
   -------------------

   procedure Navigate_Down (View : in out Ace_Editor_Type; Times : Natural) is
   begin
      View.Editor_Execute ("navigateDown(" & Times'Img & ')');
   end Navigate_Down;

   -----------------------
   -- Navigate_Line_End --
   -----------------------

   procedure Navigate_Line_End (View : in out Ace_Editor_Type) is
   begin
      View.Editor_Execute ("navigateLineEnd()");
   end Navigate_Line_End;

   -------------------------
   -- Navigate_Line_Start --
   -------------------------

   procedure Navigate_Line_Start (View : in out Ace_Editor_Type) is
   begin
      View.Editor_Execute ("navigateLineStart()");
   end Navigate_Line_Start;

   ------------
   -- Delete --
   ------------

   procedure Delete (View : in out Ace_Editor_Type) is
   begin
      View.Editor_Execute ("remove('right')");
   end Delete;

   ---------------
   -- Backspace --
   ---------------

   procedure Backspace (View : in out Ace_Editor_Type) is
   begin
      View.Editor_Execute ("remove('left')");
   end Backspace;

   ----------------------------
   -- Get_New_Line_Character --
   ----------------------------

   function Get_New_Line_Character (View : Ace_Editor_Type) return String is
   begin
      return View.Editor_Execute
        ("getSession().getDocument().getNewLineCharacter()");
   end Get_New_Line_Character;

   ------------------------
   -- Remove_To_Line_End --
   ------------------------

   procedure Remove_To_Line_End (View : in out Ace_Editor_Type) is
   begin
      View.Editor_Execute ("removeToLineEnd()");
   end Remove_To_Line_End;

   --------------------------
   -- Remove_To_Line_Start --
   --------------------------

   procedure Remove_To_Line_Start (View : in out Ace_Editor_Type) is
   begin
      View.Editor_Execute ("removeToLineStart()");
   end Remove_To_Line_Start;

   --------------------
   -- Remove_In_Line --
   --------------------

   procedure Remove_In_Line
     (View                          : in out Ace_Editor_Type;
      Row, Start_Column, End_Column :        Natural)
   is
   begin
      View.Editor_Execute
      ("getSession().getDocument().removeInLine(" &
       Row'Img &
       ',' &
       Start_Column'Img &
       ',' &
       End_Column'Img &
       ')');
   end Remove_In_Line;

   ------------------
   -- Remove_Lines --
   ------------------

   procedure Remove_Lines
     (View                : in out Ace_Editor_Type;
      First_Row, Last_Row :        Natural)
   is
   begin
      View.Editor_Execute
      ("getSession().getDocument().removeLines(" &
       First_Row'Img &
       ',' &
       Last_Row'Img &
       ')');
   end Remove_Lines;

   ---------------------------
   -- Highlight_Active_Line --
   ---------------------------

   procedure Highlight_Active_Line
     (View  : in out Ace_Editor_Type;
      Value : in     Boolean := True)
   is
   begin
      View.Editor_Execute
      ("setHighlightActiveLine(" & Value'Img & ')');
   end Highlight_Active_Line;

   function Highlight_Active_Line (View : Ace_Editor_Type) return Boolean
   is
   begin
      return View.Editor_Execute ("getHighlightActiveLine()");
   end Highlight_Active_Line;

   procedure Set_Highlight_Active_Line
     (View             : in out Ace_Editor_Type;
      Should_Highlight :        Boolean)
   is
   begin
      View.Editor_Execute
      ("setHighlightActiveLine(" & Should_Highlight'Img & ')');
   end Set_Highlight_Active_Line;

   -----------------------------
   -- Highlight_Selected_Word --
   -----------------------------

   procedure Highlight_Selected_Word
     (View  : in out Ace_Editor_Type;
      Value : in     Boolean := True)
   is
   begin
      View.Editor_Execute
      ("setHighlightSelectedWord(" & Value'Img & ')');
   end Highlight_Selected_Word;

   function Highlight_Selected_Word (View : Ace_Editor_Type) return Boolean
   is
   begin
      return View.Editor_Execute ("getHighlightSelectedWord()");
   end Highlight_Selected_Word;

   procedure Set_Highlight_Selected_Word
     (View             : in out Ace_Editor_Type;
      Should_Highlight :        Boolean)
   is
   begin
      View.Editor_Execute
      ("setHighlightSelectedWord(" & Should_Highlight'Img & ')');
   end Set_Highlight_Selected_Word;

   ---------------
   -- Overwrite --
   ---------------

   procedure Overwrite
     (View      : in out Ace_Editor_Type;
      Overwrite :        Boolean := True)
   is
   begin
      View.Editor_Execute ("setOverwrite(" & Overwrite'Img & ')');
   end Overwrite;

   function Overwrite (View : Ace_Editor_Type) return Boolean
   is
   begin
      return View.Editor_Execute ("getOverwrite()");
   end Overwrite;

   procedure Set_Overwrite
     (View      : in out Ace_Editor_Type;
      Overwrite :        Boolean)
   is
   begin
      View.Editor_Execute ("setOverwrite(" & Overwrite'Img & ')');
   end Set_Overwrite;

   -----------------------
   -- Show_Print_Margin --
   -----------------------

   procedure Show_Print_Margin
     (View  : in out Ace_Editor_Type;
      Value :        Boolean := True)
   is
   begin
      View.Editor_Execute
      ("setShowPrintMargin(" & Value'Img & ')');
   end Show_Print_Margin;

   function Show_Print_Margin (View : Ace_Editor_Type) return Boolean
   is
   begin
      return View.Editor_Execute ("getShowPrintMargin()");
   end Show_Print_Margin;

   procedure Set_Show_Print_Margin
     (View              : in out Ace_Editor_Type;
      Show_Print_Margin :        Boolean)
   is
   begin
      View.Editor_Execute
      ("setShowPrintMargin(" & Show_Print_Margin'Img & ')');
   end Set_Show_Print_Margin;

   ------------------
   -- Print_Margin --
   ------------------

   procedure Print_Margin
     (View  : in out Ace_Editor_Type;
      Value :        Natural)
   is
   begin
      View.Editor_Execute
      ("setPrintMarginColumn(" & Value'Img & ')');
   end Print_Margin;

   function Print_Margin (View : Ace_Editor_Type) return Natural
   is
   begin
      return View.Editor_Execute ("getPrintMarginColumn()");
   end Print_Margin;

   -------------------
   -- Use_Wrap_Mode --
   -------------------

   procedure Use_Wrap_Mode
     (View  : in out Ace_Editor_Type;
      Value :        Boolean := True)
   is
   begin
      View.Editor_Execute
      ("getSession().setUseWrapMode(" & Value'Img & ')');
   end Use_Wrap_Mode;

   function Use_Wrap_Mode (View : Ace_Editor_Type) return Boolean
   is
   begin
      return View.Editor_Execute ("getSession().getUseWrapMode()");
   end Use_Wrap_Mode;

   procedure Set_Use_Wrap_Mode
     (View          : in out Ace_Editor_Type;
      Use_Wrap_Mode :        Boolean)
   is
   begin
      View.Editor_Execute ("getSession().setUseWrapMode(" & Use_Wrap_Mode'Img & ')');
   end Set_Use_Wrap_Mode;

   --------------------------
   -- Set_Wrap_Limit_Range --
   --------------------------

   procedure Set_Wrap_Limit_Range
     (View     : in out Ace_Editor_Type;
      Min, Max :        Natural)
   is
   begin
      View.Editor_Execute
      ("getSession().setWrapLimitRange(" & Min'Img & ',' & Max'Img & ')');
   end Set_Wrap_Limit_Range;

   ----------------
   -- Wrap_Limit --
   ----------------

   procedure Wrap_Limit
     (View     : in out Ace_Editor_Type;
      To :        Natural)
   is
   begin
      View.Editor_Execute ("getSession().setWrapLimit(" & To'Img & ')');
   end Wrap_Limit;

   function Wrap_Limit (View : Ace_Editor_Type) return Natural
   is
   begin
      return View.Editor_Execute ("getSession().getWrapLimit()");
   end Wrap_Limit;

   -----------------------
   -- First_Visible_Row --
   -----------------------

   function First_Visible_Row (View : Ace_Editor_Type) return Natural
   is
   begin
      return View.Editor_Execute ("getFirstVisibleRow()");
   end First_Visible_Row;

   ----------------------
   -- Last_Visible_Row --
   ----------------------

   function Last_Visible_Row (View : Ace_Editor_Type) return Natural
   is
   begin
      return View.Editor_Execute ("getLastVisibleRow()");
   end Last_Visible_Row;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (View        : in out Ace_Editor_Type;
      Row, Column :        Natural;
      Text        :        String)
   is
   begin
      View.Editor_Execute ("getSession().getDocument().insertInLine({row:" &
                             Row'Img & ",column:" & Column'Img & "}, '" &
                             Escape_Quotes (Text) & "')");
   end Insert;

   ---------------------
   -- Insert_New_Line --
   ---------------------

   procedure Insert_New_Line
     (View             : in out Ace_Editor_Type;
      Row, Column :        Natural)
   is
   begin
      View.Editor_Execute ("getSession().getDocument().insertNewLine({row:" &
                             Row'Img & ",column:" & Column'Img & "})");
   end Insert_New_Line;

   procedure InsertNewLine
     (View             : in out Ace_Editor_Type;
      Start_Row, Index :        Natural)
   is
   begin
      Gnoga.Server.Connection.Execute_Script
        (View.Connection_ID,
         "var p=" &
         View.Editor_Var &
         ".getSession().getDocument().indexToPosition(" &
         Index'Img &
         ',' &
         Start_Row'Img &
         "); " &
         View.Editor_Var &
         ".getSession().getDocument().insertNewLine(p)");
   end InsertNewLine;

   ----------
   -- Line --
   ----------

   function Line (View : Ace_Editor_Type; Row : Natural) return String is
   begin
      return View.Editor_Execute ("getSession().getLine(" & Row'Img & ')');
   end Line;

   ----------------
   -- Text_Range --
   ----------------

   function Text_Range (View : Ace_Editor_Type; From, To : Position_Type) return String is
   begin
      return View.Editor_Execute ("getSession().getTextRange({start:{column:" & From.Column'Img &
                                  ",row:" & From.Row'Img &
                                  "},end:{column:" & To.Column'Img &
                                  ",row:" & To.Row'Img &
                                  "}})");
   end Text_Range;

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

   procedure Editor_Execute
     (Editor : in out Ace_Editor_Type;
      Method :        String)
   is
      Message_Script : constant String := Editor.Editor_Var & "." & Method;
   begin
      Gnoga.Server.Connection.Execute_Script
        (ID     => Editor.Connection_ID,
         Script => Message_Script);
   end Editor_Execute;

   function Editor_Execute
     (Editor : Ace_Editor_Type;
      Method : String) return String
   is
      Message_Script : constant String := Editor.Editor_Var & "." & Method;
   begin
      return Gnoga.Server.Connection.Execute_Script
          (ID     => Editor.Connection_ID,
           Script => Message_Script);
   end Editor_Execute;

   function Editor_Execute
     (Editor : Ace_Editor_Type;
      Method : String) return Integer
   is
   begin
      return Integer'Value (Editor.Editor_Execute (Method));
   exception
      when E : others =>
         Log ("Error Editor_Execute converting to Integer (forced to 0).");
         Log (Ada.Exceptions.Exception_Information (E));
         return 0;
   end Editor_Execute;

   function Editor_Execute
     (Editor : Ace_Editor_Type;
      Method : String) return Boolean
   is
   begin
      return Editor.Editor_Execute (Method) = "true";
   end Editor_Execute;

   ---------------
   -- On_Resize --
   ---------------

   overriding procedure On_Resize (View : in out Ace_Editor_Type) is
   begin
      if View.Editor_Var /= "" then
         View.Editor_Execute ("resize()");
      end if;

      Gnoga.Gui.View.View_Type (View).On_Resize;
   end On_Resize;

   ------------
   -- Create --
   ------------

   procedure Create
     (Anchor      : in out Anchor_Type;
      Parent      : in out Ace_Editor_Type'Class;
      Row, Column : Natural) is
      Anchor_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Anchor.ID (Anchor_ID, Gnoga.Types.Gnoga_ID);
      Anchor.Connection_ID (Parent.Connection_ID);
      Gnoga.Server.Connection.Execute_Script
        (Anchor.Connection_ID,
         "gnoga['" & Anchor_ID & "'] = " & Parent.Editor_Var & "." & "getSession().getDocument().createAnchor(" &
           Row'Img & ',' & Column'Img & ");");
   end Create;

   --------------
   -- Position --
   --------------

   procedure Position (Anchor : in out Anchor_Type; Pos : Position_Type; No_Clip : Boolean := False) is
   begin
      Anchor.Execute ("setPosition(" & Pos.Row'Img & ',' & Pos.Column'Img & ',' & No_Clip'Img & ')');
   end Position;

   function Position (Anchor : Anchor_Type) return Position_Type is
   begin
      return (Anchor.jQuery_Execute ("get(0).getPosition().row"),
              Anchor.jQuery_Execute ("get(0).getPosition().column"));
   end Position;

   ---------------------------
   -- Insert_Text_At_Anchor --
   ---------------------------

   procedure Insert_Text_At_Anchor (Anchor : in out Anchor_Type;
                                    Text   : in     String) is
   begin
      Anchor.Execute ("getDocument().insert(gnoga['" & Anchor.ID & "'].getPosition(), '" &
                        Escape_Quotes (Text) & "')");
   end Insert_Text_At_Anchor;

   -------------------------------
   -- Insert_New_Line_At_Anchor --
   -------------------------------

   procedure Insert_New_Line_At_Anchor (Anchor : in out Anchor_Type) is
   begin
      Anchor.Execute ("getDocument().insertNewLine(gnoga['" & Anchor.ID & "'].getPosition())");
   end Insert_New_Line_At_Anchor;

   procedure Insert_NewLine_At_Anchor (Anchor : in out Anchor_Type) is
   begin
      Anchor.Execute ("getDocument().insertNewLine(gnoga['" & Anchor.ID & "'].getPosition())");
   end Insert_NewLine_At_Anchor;

end Gnoga.Gui.Plugin.Ace_Editor;
