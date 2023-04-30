------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--           G N O G A . G U I . P L U G I N . A C E _ E D I T O R .        --
--                            C O N S O L E _ I O                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2017 Pascal Pignard                    --
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

with Ada.Characters.Wide_Wide_Latin_1;

package body Gnoga.Gui.Plugin.Ace_Editor.Console_IO is

   protected body Text_Buffer is
      procedure Write (Line : in String) is
      begin
         Append (Buffer, Line);
         Append (Buffer, Ada.Characters.Wide_Wide_Latin_1.CR);
         NL := True;
      end Write;

      entry Read
        (Line : out String;
         Last : out Natural) when NL
      is
         Ind : constant Natural := Index (Buffer, From_Unicode (Ada.Characters.Wide_Wide_Latin_1.CR)) - 1;
      begin
         Last := Line.First + Ind - 1;
         Line := Slice (Buffer, Buffer.First, Ind);
         Delete (Buffer, Buffer.First, Ind + 1); --  Supress CR in addition
         NL := Index (Buffer, From_Unicode (Ada.Characters.Wide_Wide_Latin_1.CR)) > 0;
      end Read;

      entry Read (Line : out String) when NL is
         Ind : constant Natural := Index (Buffer, From_Unicode (Ada.Characters.Wide_Wide_Latin_1.CR)) - 1;
      begin
         Line := Slice (Buffer, Buffer.First, Ind);
         Delete (Buffer, Buffer.First, Ind + 1); --  Supress CR in addition
         NL := Index (Buffer, From_Unicode (Ada.Characters.Wide_Wide_Latin_1.CR)) > 0;
      end Read;

      entry Read (Ch : out Unicode_Character) when Length (Buffer) > 0 is
      begin
         Ch := Element (Buffer, Buffer.First);
         Delete (Buffer, Buffer.First, Buffer.First);
         NL := Index (Buffer, From_Unicode (Ada.Characters.Wide_Wide_Latin_1.CR)) > 0;
      end Read;

      procedure Get
        (Ch        : out Unicode_Character;
         Available : out Boolean)
      is
      begin
         Available := Length (Buffer) > 0;
         if Available then
            Ch := Element (Buffer, Buffer.First);
            Delete (Buffer, Buffer.First, Buffer.First);
         end if;
         NL := Index (Buffer, From_Unicode (Ada.Characters.Wide_Wide_Latin_1.CR)) > 0;
      end Get;

      procedure Look
        (Ch        : out Unicode_Character;
         Available : out Boolean)
      is
      begin
         Available := Length (Buffer) > 0;
         if Available then
            Ch := Element (Buffer, Buffer.First);
         end if;
      end Look;
   end Text_Buffer;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (Console : in out Console_IO_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      ID      : in     String := "")
   is
   begin
      Ace_Editor_Type (Console).Create (Parent, ID);
      Console.Show_Gutter (False);
      Console.Set_Highlight_Active_Line (False);
      Console.Set_Show_Print_Margin (False);
      Console.Anchor.Create (Console, 0, 0);
      Console.Editor_Execute ("gnoga_prompt=0;");
      Console.Editor_Execute
        ("sendEvent = function (e, m) {ws.send ('" & Image (Console.Unique_ID) & "|' + e + '|' + m);}");
      Console.Editor_Execute
        ("commands.on('exec', function(e) {" & "    if (e.command.readOnly) return;" & "    var editableRow = " &
         Console.Editor_Var & ".session.getLength() - 1;" &
         "    var deletesLeft = e.command.name == 'backspace' || e.command.name == 'removewordleft';" &
         "    var notEditable = " & Console.Editor_Var & ".selection.getAllRanges().some(function(r) {" &
         "        if (deletesLeft && r.start.column <= " & Console.Editor_Var & ".gnoga_prompt && r.end.column <= " &
         Console.Editor_Var & ".gnoga_prompt) return true;" & "        if (deletesLeft && (r.start.column < " &
         Console.Editor_Var & ".gnoga_prompt || r.end.column < " & Console.Editor_Var & ".gnoga_prompt)) return true;" &
         "        if (r.start.column < " & Console.Editor_Var & ".gnoga_prompt && r.end.column < " &
         Console.Editor_Var & ".gnoga_prompt) return true;" &
         "        return r.start.row != editableRow || r.end.row != editableRow; " & "    });" &
         "    if (notEditable)" & "        e.preventDefault();" & "});");
      Console.Editor_Execute
        ("commands.bindKeys({" & "    'Shift-Return|Ctrl-Return|Alt-Return': function(cmdLine) {}," &
         "    'Esc|Shift-Esc': function(cmdLine){" & "        cmdLine.session.remove(" &
         "          {start:{row:cmdLine.session.getLength()-1," & "                  column: cmdLine.gnoga_prompt}," &
         "           end: {row:cmdLine.session.getLength()-1," &
         "                 column:cmdLine.session.getDocumentLastRowColumn(cmdLine.session.getLength()-1,0)}});}," &
         "    'Return': function(cmdLine){" & "        cmdLine.sendEvent ('get_line'," &
         "          cmdLine.session.getTextRange(" & "            {start:{row:cmdLine.session.getLength()-1," &
         "                    column: cmdLine.gnoga_prompt}," &
         "             end: {row:cmdLine.session.getLength()-1," &
         "                   column:cmdLine.session.getDocumentLastRowColumn(cmdLine.session.getLength()-1,0)}}));" &
         "        cmdLine.navigateFileEnd();" & "        cmdLine.insert('\n');}" & "});");
   end Create;

   ---------------------
   -- Set_Line_Length --
   ---------------------

   procedure Set_Line_Length
     (Console : in out Console_IO_Type;
      To      :        Count)
   is
   begin
      Console.Set_Use_Wrap_Mode (To /= Unbounded);
      Console.Wrap_Limit (To);
   end Set_Line_Length;

   ---------------------
   -- Set_Page_Length --
   ---------------------

   procedure Set_Page_Length
     (Console : in Console_IO_Type;
      To      :    Count)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Set_Page_Length unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Page_Length";
   end Set_Page_Length;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length
     (Console : in Console_IO_Type)
      return Count
   is
   begin
      return Console.Wrap_Limit;
   end Line_Length;

   -----------------
   -- Page_Length --
   -----------------

   function Page_Length
     (Console : in Console_IO_Type)
      return Count
   is
   begin
      return Console.Last_Visible_Row - Console.First_Visible_Row;
   end Page_Length;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line (Console : in out Console_IO_Type) is
   begin
      Console.New_Line (1);
   end New_Line;

   procedure New_Line
     (Console : in out Console_IO_Type;
      Spacing :        Positive_Count)
   is
   begin
      for I in 1 .. Spacing loop
         Console.Anchor.Insert_New_Line_At_Anchor;
      end loop;
      Console.Editor_Execute ("gnoga_prompt=" & Image (Console.Anchor.Position.Column) & ';');
   end New_Line;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line
     (Console : in out Console_IO_Type;
      Spacing :        Positive_Count := 1)
   is
      Ch : Unicode_Character;
   begin
      for Line in 1 .. Spacing loop
         loop
            Console.Text.Read (Ch);
            exit when Ch = Ada.Characters.Wide_Wide_Latin_1.CR;
         end loop;
      end loop;
   end Skip_Line;

   -----------------
   -- End_Of_Line --
   -----------------

   function End_Of_Line
     (Console : in out Console_IO_Type)
      return Boolean
   is
      Item : Unicode_Character;
      EOL  : Boolean;
   begin
      Console.Look_Ahead (Item, EOL);
      return EOL;
   end End_Of_Line;

   --------------
   -- New_Page --
   --------------

   procedure New_Page (Console : in out Console_IO_Type) is
   begin
      Console.New_Line (Console.Page_Length);
   end New_Page;

   ---------------
   -- Skip_Page --
   ---------------

   procedure Skip_Page (Console : in Console_IO_Type) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Skip_Page unimplemented");
      raise Program_Error with "Unimplemented procedure Skip_Page";
   end Skip_Page;

   -----------------
   -- End_Of_Page --
   -----------------

   function End_Of_Page
     (Console : in Console_IO_Type)
      return Boolean
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "End_Of_Page unimplemented");
      raise Program_Error with "Unimplemented function End_Of_Page";
      return End_Of_Page (Console => Console);
   end End_Of_Page;

   -------------
   -- Set_Col --
   -------------

   procedure Set_Col
     (Console : in out Console_IO_Type;
      To      :        Positive_Count)
   is
      Pos : constant Position_Type := Console.Anchor.Position;
   begin
      Console.Anchor.Position ((Pos.Row, To));
   end Set_Col;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line
     (Console : in out Console_IO_Type;
      To      :        Positive_Count)
   is
      Pos : constant Position_Type := Console.Anchor.Position;
   begin
      Console.Anchor.Position ((To, Pos.Column));
   end Set_Line;

   ---------
   -- Col --
   ---------

   function Col
     (Console : in Console_IO_Type)
      return Positive_Count
   is
   begin
      return Console.Anchor.Position.Column;
   end Col;

   ----------
   -- Line --
   ----------

   function Line
     (Console : in Console_IO_Type)
      return Positive_Count
   is
   begin
      return Console.Anchor.Position.Row;
   end Line;

   ----------
   -- Page --
   ----------

   function Page
     (Console : in Console_IO_Type)
      return Positive_Count
   is
   begin
      return Console.Line / Console.Page_Length;
   end Page;

   ---------
   -- Get --
   ---------

   procedure Get
     (Console : in out Console_IO_Type;
      Item    :    out Unicode_Character)
   is
   begin
      Console.Text.Read (Item);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (Console : in out Console_IO_Type;
      Item    :        Unicode_Character)
   is
   begin
      Console.Anchor.Insert_Text_At_Anchor (From_Unicode (Item));
      Console.Editor_Execute ("gnoga_prompt=" & Image (Console.Anchor.Position.Column) & ';');
   end Put;

   ----------------
   -- Look_Ahead --
   ----------------

   procedure Look_Ahead
     (Console     : in out Console_IO_Type;
      Item        :    out Unicode_Character;
      End_Of_Line :    out Boolean)
   is
      Available : Boolean;
   begin
      Console.Text.Look (Item, Available);
      End_Of_Line := Available and Item = Ada.Characters.Wide_Wide_Latin_1.CR;
   end Look_Ahead;

   -------------------
   -- Get_Immediate --
   -------------------

   procedure Get_Immediate
     (Console : in out Console_IO_Type;
      Item    :    out Unicode_Character)
   is
      Available : Boolean;
   begin
      Console.Text.Get (Item, Available);
      if not Available then
         raise End_Error;
      end if;
   end Get_Immediate;

   -------------------
   -- Get_Immediate --
   -------------------

   procedure Get_Immediate
     (Console   : in out Console_IO_Type;
      Item      :    out Unicode_Character;
      Available :    out Boolean)
   is
   begin
      Console.Text.Get (Item, Available);
   end Get_Immediate;

   ---------
   -- Get --
   ---------

   procedure Get
     (Console : in out Console_IO_Type;
      Item    :    out String)
   is
   begin
      Console.Text.Read (Item);
   end Get;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Console : in out Console_IO_Type;
      Message : in     String;
      Class   : in     String := "";
      ID      : in     String := "")
   is
      pragma Unreferenced (Class, ID);
   begin
      Console.Anchor.Insert_Text_At_Anchor (Message);
      Console.Editor_Execute ("gnoga_prompt=" & Image (Console.Anchor.Position.Column) & ';');
   end Put;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Console : in out Console_IO_Type;
      Item    :    out String;
      Last    :    out Natural)
   is
   begin
      Console.Text.Read (Item, Last);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Console : in out Console_IO_Type)
      return String
   is
      Line : String;
   begin
      Console.Text.Read (Line);
      return Line;
   end Get_Line;

   --------------
   -- Put_Line --
   --------------

   overriding procedure Put_Line
     (Console : in out Console_IO_Type;
      Message : in     String;
      Class   : in     String := "";
      ID      : in     String := "")
   is
      pragma Unreferenced (Class, ID);
   begin
      Console.Anchor.Insert_Text_At_Anchor (Message);
      Console.Anchor.Insert_New_Line_At_Anchor;
      Console.Editor_Execute ("gnoga_prompt=" & Image (Console.Anchor.Position.Column) & ';');
   end Put_Line;

   ----------------------
   -- Fire_On_Get_Line --
   ----------------------

   procedure Fire_On_Get_Line
     (Console : in out Console_IO_Type;
      Line    : in     String)
   is
   begin
      Console.Text.Write (Line);
   end Fire_On_Get_Line;

   ----------------
   -- On_Message --
   ----------------

   overriding procedure On_Message
     (Object  : in out Console_IO_Type;
      Event   : in     String;
      Message : in     String)
   is
   begin
      -- Get_Line Event --
      if Event = "get_line" then
         Object.Fire_On_Get_Line (Message);
      else
         Gnoga.Gui.Base.Base_Type (Object).On_Message (Event, Message);
      end if;
   end On_Message;

end Gnoga.Gui.Plugin.Ace_Editor.Console_IO;
