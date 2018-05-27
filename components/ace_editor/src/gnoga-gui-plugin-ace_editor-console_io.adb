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

with Ada.Characters.Conversions;
with Ada.Characters.Latin_1;

with Gnoga.Types.Key_Codes;

package body Gnoga.Gui.Plugin.Ace_Editor.Console_IO is

   --------------------------
   -- Key input bufferring --
   --------------------------

   protected body Ring_Char is
      entry Write (Ch : in Character) when Count < Ring_Size is
      begin
         Buffer (Write_Index) := Ch;
         Write_Index          := Write_Index + 1;
         Count                := Count + 1;
      end Write;

      entry Read (Ch : out Character) when Count > 0 is
      begin
         Ch         := Buffer (Read_Index);
         Read_Index := Read_Index + 1;
         Count      := Count - 1;
      end Read;

      procedure Get (Ch : out Character; Available : out Boolean) is
      begin
         Available := Count /= 0;
         if Available then
            Ch         := Buffer (Read_Index);
            Read_Index := Read_Index + 1;
            Count      := Count - 1;
         end if;
      end Get;

      procedure Look (Ch : out Character; Available : out Boolean) is
      begin
         Available := Count /= 0;
         Ch        := Buffer (Read_Index);
      end Look;
   end Ring_Char;

   procedure On_Key_Press_Handler
     (Object         : in out Gnoga.Gui.Base.Base_Type'Class;
      Keyboard_Event : in     Gnoga.Gui.Base.Keyboard_Event_Record);

   procedure On_Key_Press_Handler
     (Object         : in out Gnoga.Gui.Base.Base_Type'Class;
      Keyboard_Event : in     Gnoga.Gui.Base.Keyboard_Event_Record)
   is
      Ch : constant Character :=
        Ada.Characters.Conversions.To_Character (Keyboard_Event.Key_Char);
      use type Gnoga.Gui.Base.Keyboard_Message_Type;
   begin
--        Gnoga.Log
--          (Keyboard_Event.Key_Code'Img & ',' & Keyboard_Event.Key_Char'Img);
      --  ASCII Character
      if Ch not in Ada.Characters.Latin_1.NUL | Ada.Characters.Latin_1.CR | Ada.Characters.Latin_1.ESC then
         Console_IO_Type (Object).Ring.Write (Ch);
         return;
      end if;
      --  Other special keys
      if Keyboard_Event.Message = Gnoga.Gui.Base.Key_Down then
         case Keyboard_Event.Key_Code is
            when Gnoga.Types.Key_Codes.Key_BackSpace =>
               Console_IO_Type (Object).Ring.Write (Ada.Characters.Latin_1.BS);
            when Gnoga.Types.Key_Codes.Key_Tab =>
               Console_IO_Type (Object).Ring.Write (Ada.Characters.Latin_1.HT);
            when Gnoga.Types.Key_Codes.Key_Enter =>
               Console_IO_Type (Object).Ring.Write (Ada.Characters.Latin_1.CR);
            when Gnoga.Types.Key_Codes.Key_Esc =>
               Console_IO_Type (Object).Ring.Write (Ada.Characters.Latin_1.ESC);
            when
                Gnoga.Types.Key_Codes.Key_F1 ..
                  Gnoga.Types.Key_Codes.Key_F10 =>
               Console_IO_Type (Object).Ring.Write
                 (Ada.Characters.Latin_1.NUL);
               --  Alt modifier
               if Keyboard_Event.Alt then
                  Console_IO_Type (Object).Ring.Write
                    (Character'Val
                       (Keyboard_Event.Key_Code -
                        Gnoga.Types.Key_Codes.Key_F1 +
                        104));
               --  Control modifier
               elsif Keyboard_Event.Control then
                  Console_IO_Type (Object).Ring.Write
                    (Character'Val
                       (Keyboard_Event.Key_Code -
                        Gnoga.Types.Key_Codes.Key_F1 +
                        94));
               --  Shift modifier
               elsif Keyboard_Event.Shift then
                  Console_IO_Type (Object).Ring.Write
                    (Character'Val
                       (Keyboard_Event.Key_Code -
                        Gnoga.Types.Key_Codes.Key_F1 +
                        84));
               --  No modifier
               else
                  Console_IO_Type (Object).Ring.Write
                    (Character'Val
                       (Keyboard_Event.Key_Code -
                        Gnoga.Types.Key_Codes.Key_F1 +
                        59));
               end if;
            when Gnoga.Types.Key_Codes.Key_Home =>
               Console_IO_Type (Object).Ring.Write
                 (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Console_IO_Type (Object).Ring.Write ('w');
               else
                  Console_IO_Type (Object).Ring.Write ('G');
               end if;
            when Gnoga.Types.Key_Codes.Key_Left =>
               Console_IO_Type (Object).Ring.Write
                 (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Console_IO_Type (Object).Ring.Write ('s');
               else
                  Console_IO_Type (Object).Ring.Write ('K');
               end if;
            when Gnoga.Types.Key_Codes.Key_Up =>
               Console_IO_Type (Object).Ring.Write
                 (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Console_IO_Type (Object).Ring.Write (Character'Val (141));
               else
                  Console_IO_Type (Object).Ring.Write ('H');
               end if;
            when Gnoga.Types.Key_Codes.Key_Right =>
               Console_IO_Type (Object).Ring.Write
                 (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Console_IO_Type (Object).Ring.Write ('t');
               else
                  Console_IO_Type (Object).Ring.Write ('M');
               end if;
            when Gnoga.Types.Key_Codes.Key_Down =>
               Console_IO_Type (Object).Ring.Write
                 (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Console_IO_Type (Object).Ring.Write (Character'Val (145));
               else
                  Console_IO_Type (Object).Ring.Write ('P');
               end if;
            when Gnoga.Types.Key_Codes.Key_Page_Up =>
               Console_IO_Type (Object).Ring.Write
                 (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Console_IO_Type (Object).Ring.Write (Character'Val (132));
               else
                  Console_IO_Type (Object).Ring.Write ('I');
               end if;
            when Gnoga.Types.Key_Codes.Key_Page_Down =>
               Console_IO_Type (Object).Ring.Write
                 (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Console_IO_Type (Object).Ring.Write ('v');
               else
                  Console_IO_Type (Object).Ring.Write ('Q');
               end if;
            when Gnoga.Types.Key_Codes.Key_End =>
               Console_IO_Type (Object).Ring.Write
                 (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Console_IO_Type (Object).Ring.Write ('u');
               else
                  Console_IO_Type (Object).Ring.Write ('O');
               end if;
            when Gnoga.Types.Key_Codes.Key_Delete =>
               Console_IO_Type (Object).Ring.Write
                 (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Console_IO_Type (Object).Ring.Write (Character'Val (147));
               else
                  Console_IO_Type (Object).Ring.Write ('S');
               end if;
            when others =>
               null;
         end case;
      end if;
   end On_Key_Press_Handler;

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
      Console.Read_Only;
      Console.Show_Gutter (False);
      Console.Set_Highlight_Active_Line (False);
      Console.Set_Show_Print_Margin (False);
      Console.Anchor.Create (Console, 0, 0);
      --  Deactivate ACE key handler
      Console.Editor_Execute
      ("keyBinding.addKeyboardHandler(function() {return {passEvent: true, command: 'null' }})");
      Console.On_Key_Press_Handler (On_Key_Press_Handler'Access);
      --  Needed for Safari special keys
      Console.On_Key_Down_Handler (On_Key_Press_Handler'Access);
   end Create;

   ---------------------
   -- Set_Line_Length --
   ---------------------

   procedure Set_Line_Length (Console : in out Console_IO_Type; To : Count) is
   begin
      Console.Set_Use_Wrap_Mode (To /= Unbounded);
      Console.Wrap_Limit (To);
   end Set_Line_Length;

   ---------------------
   -- Set_Page_Length --
   ---------------------

   procedure Set_Page_Length (Console : in Console_IO_Type; To : Count) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning
        (Standard.True,
         "Set_Page_Length unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Page_Length";
   end Set_Page_Length;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length (Console : in Console_IO_Type) return Count is
   begin
      return Console.Wrap_Limit;
   end Line_Length;

   -----------------
   -- Page_Length --
   -----------------

   function Page_Length (Console : in Console_IO_Type) return Count is
   begin
      return Console.Last_Visible_Row - Console.First_Visible_Row;
   end Page_Length;

   --------------
   -- New_Line --
   --------------

   procedure New_Line
     (Console : in out Console_IO_Type;
      Spacing :        Positive_Count := 1)
   is
   begin
      for I in 1 .. Spacing loop
         Console.Anchor.Insert_NewLine_At_Anchor;
      end loop;
   end New_Line;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line
     (Console : in out Console_IO_Type;
      Spacing :        Positive_Count := 1)
   is
      Ch : Character;
   begin
      for Line in 1 .. Spacing loop
         loop
            Console.Ring.Read (Ch);
            exit when Ch = Ada.Characters.Latin_1.CR;
         end loop;
      end loop;
   end Skip_Line;

   -----------------
   -- End_Of_Line --
   -----------------

   function End_Of_Line (Console : in out Console_IO_Type) return Boolean is
      Item : Character;
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

   function End_Of_Page (Console : in Console_IO_Type) return Boolean is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "End_Of_Page unimplemented");
      raise Program_Error with "Unimplemented function End_Of_Page";
      return End_Of_Page (Console => Console);
   end End_Of_Page;

   -------------
   -- Set_Col --
   -------------

   procedure Set_Col (Console : in out Console_IO_Type; To : Positive_Count) is
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

   function Col (Console : in Console_IO_Type) return Positive_Count is
   begin
      return Console.Anchor.Position.Column;
   end Col;

   ----------
   -- Line --
   ----------

   function Line (Console : in Console_IO_Type) return Positive_Count is
   begin
      return Console.Anchor.Position.Row;
   end Line;

   ----------
   -- Page --
   ----------

   function Page (Console : in Console_IO_Type) return Positive_Count is
   begin
      return Console.Line / Console.Page_Length;
   end Page;

   ---------
   -- Get --
   ---------

   procedure Get (Console : in out Console_IO_Type; Item : out Character) is
   begin
      Console.Ring.Read (Item);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (Console : in out Console_IO_Type; Item : Character) is
   begin
      Console.Anchor.Insert_Text_At_Anchor ((1 => Item));
   end Put;

   ----------------
   -- Look_Ahead --
   ----------------

   procedure Look_Ahead
     (Console     : in out Console_IO_Type;
      Item        :    out Character;
      End_Of_Line :    out Boolean)
   is
      Available : Boolean;
   begin
      Console.Ring.Look (Item, Available);
      End_Of_Line := Available and Item = Ada.Characters.Latin_1.CR;
   end Look_Ahead;

   -------------------
   -- Get_Immediate --
   -------------------

   procedure Get_Immediate
     (Console : in out Console_IO_Type;
      Item    :    out Character)
   is
      Available : Boolean;
   begin
      Console.Ring.Get (Item, Available);
      if not Available then
         raise End_Error;
      end if;
   end Get_Immediate;

   -------------------
   -- Get_Immediate --
   -------------------

   procedure Get_Immediate
     (Console   : in out Console_IO_Type;
      Item      :    out Character;
      Available :    out Boolean)
   is
   begin
      Console.Ring.Get (Item, Available);
   end Get_Immediate;

   ---------
   -- Get --
   ---------

   procedure Get (Console : in out Console_IO_Type; Item : out String) is
   begin
      for Ch of Item loop
         Console.Ring.Read (Ch);
      end loop;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (Console : in out Console_IO_Type; Item : String) is
   begin
      Console.Anchor.Insert_Text_At_Anchor (Item);
   end Put;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Console : in out Console_IO_Type;
      Item    :    out String;
      Last    :    out Natural)
   is
      Line : constant String := Console.Get_Line;
   begin
      Last := Item'First + Natural'Min (Item'Length, Line'Length) - 1;
      Item (Item'First .. Last) :=
        Line (Line'First .. Line'First + Last - Item'First);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Console : in out Console_IO_Type) return String is
      Pos          : Position_Type    := Console.Anchor.Position;
      Start_Row    : constant Natural := Pos.Row;
      Start_Column : constant Natural := Pos.Column;
      End_Column   : Natural          := Start_Column;
      CC           : Natural;
      Ch           : Character;
   begin
      Console.Navigate_To (Start_Row, Start_Column);
      loop
--           Gnoga.Log (Pos.Row'Img & ',' & Pos.Column'Img);
         Console.Ring.Read (Ch);
         Pos := Console.Anchor.Position;
         CC  := Console.Current_Column;
         if CC /= Pos.Column
           and then Console.Current_Line = Start_Row
           and then CC in Start_Column .. End_Column
         then
            Console.Anchor.Position ((Start_Row, CC));
         else
            CC := Pos.Column;
         end if;
--           Gnoga.Log (Ch'Img);
         case Ch is
            when Ada.Characters.Latin_1.NUL =>
               Console.Ring.Read (Ch);
--                 Gnoga.Log (Ch'Img);
               case Ch is
                  when 'G' =>  -- Home
                     Console.Navigate_To (Start_Row, Start_Column);
                     Console.Anchor.Position ((Start_Row, Start_Column));
                  when 'K' =>  -- Left
                     if Pos.Column > Start_Column then
                        Pos.Column := Pos.Column - 1;
                        Console.Navigate_To (Pos.Row, Pos.Column);
                        Console.Anchor.Position (Pos);
                     end if;
                  when 'H' =>  -- Up
                     null;
                  when 'M' =>  -- Right
                     if Pos.Column < End_Column then
                        Pos.Column := Pos.Column + 1;
                        Console.Navigate_To (Pos.Row, Pos.Column);
                        Console.Anchor.Position (Pos);
                     end if;
                  when 'P' =>  -- Down
                     null;
                  when 'O' =>  -- End
                     Console.Navigate_To (Start_Row, End_Column);
                     Console.Anchor.Position ((Start_Row, End_Column));
                  when 'S' =>  -- Delete
                     Console.Navigate_To (Pos.Row, Pos.Column);
                     if Pos.Column < End_Column then
                        Console.Delete;
                        End_Column := End_Column - 1;
                     end if;
                  when others =>
                     null;
               end case;
            when Ada.Characters.Latin_1.BS => -- BackSpace
               Console.Navigate_To (Pos.Row, Pos.Column);
               if Pos.Column > Start_Column then
                  Console.Backspace;
                  End_Column := End_Column - 1;
               end if;
            when Ada.Characters.Latin_1.ESC => -- Escape
               Console.Remove_In_Line (Start_Row, Start_Column, End_Column);
               Console.Navigate_To (Start_Row, Start_Column);
               Console.Anchor.Position ((Start_Row, Start_Column));
               End_Column := Start_Column;
            when Ada.Characters.Latin_1.CR => -- Carriage Return
               Console.Navigate_To (Start_Row, End_Column);
               Console.Anchor.Position ((Start_Row, End_Column));
               Console.Anchor.Insert_NewLine_At_Anchor;
               exit; -- Exit loop
            when others => -- Regular characters
               Console.Anchor.Insert_Text_At_Anchor ((1 => Ch));
               Console.Navigate_To (Pos.Row, CC + 1);
               End_Column := End_Column + 1;
         end case;
      end loop;
      return Console.Text_Range
        ((Start_Row, Start_Column), (Start_Row, End_Column));
   end Get_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Console : in out Console_IO_Type; Item : String) is
   begin
      Console.Anchor.Insert_Text_At_Anchor (Item);
      Console.Anchor.Insert_NewLine_At_Anchor;
   end Put_Line;

end Gnoga.Gui.Plugin.Ace_Editor.Console_IO;
