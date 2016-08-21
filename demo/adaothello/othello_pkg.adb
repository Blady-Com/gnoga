--------------------------------------------------------------------------------
--|   Othello - The Classic Othello Game written in Ada
--|
--|   Copyright (C) 2001 Adrian Hoe (byhoe@users.sourceforge.net)
--|
--| Othello is free software; you can redistribute it and/or modify
--| it under the terms of the GNU General Public License as published
--| by the Free Software Foundation; either version 2 of the License,
--| or (at your option) any later version.
--|
--| This software is distributed in the hope that it will be useful,
--| and entertaining but WITHOUT ANY WARRANTY; without even the
--| implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--| PURPOSE.  See the GNU General Public License for more details.
--|
--| You should have received a copy of the GNU General Public
--| License along with this software; if not, write to the
--| Free Software Foundation, Inc., 59 Temple Place - Suite 330,
--| Boston, MA 02111-1307, USA.
--|
--| Filename         : $Source: /othello_pkg.adb,v $
--| Author           : Adrian Hoe (byhoe)
--| Created On       : 2001/11/15
--| Last Modified By : $Author: Jeff Carter $
--| Last Modified On : $Date: 2016/08/03 $
--| Status           : $State: Exp $
--|
--------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Ada.Numerics.Elementary_Functions;
with Ada.Integer_Text_Io;    use Ada.Integer_Text_Io;

with Othello_Pkg.Callbacks;

with Othello_Types;          use Othello_Types;

with Gnoga.Application.Multi_Connect;

package body Othello_Pkg is
   ----------------------------------------------------------------------------

   -- Player color constants
   Player_Move   : constant Othello_Types.Bead_Color := Blue;
   Computer_Move : constant Othello_Types.Bead_Color := Red;

   type Possible_Moves_Record is
      record
         Row    : Othello_Types.Valid_Row;
         Column : Othello_Types.Valid_Column;
         Check  : Integer;
         Side   : Integer;
      end record;
   type Possible_Moves_Matrix is array (Positive range <>) of Possible_Moves_Record;

   -----------------------
   --   Unsigned Byte   --
   -----------------------
   type Othello_Integer is mod 256;

   ----------------------------------------------------------------------------
   procedure Beep(Othello : in out Othello_Types.Othello_Record'Class)
   is
      --Bell : constant Character := Ada.Characters.Latin_1.Bel;
   begin
      if Othello.Main_Statusbar.Text(1..7) /= "Invalid" then
         Othello.Main_Statusbar.Text("Invalid Move.  " & Othello.Main_Statusbar.Text);
      end if;
      --Ada.Text_Io.Put (Bell);
   end Beep;

   ----------------------------------------------------------------------------
   procedure Put_Bead
      (Location : in out Othello_Types.Cell_Record)
   is

   begin

      if Location.Cell = Blue then
         Location.Pixmap.Background_Color("Blue");
      else
         Location.Pixmap.Background_Color("Red");
      end if;

   end Put_Bead;
   ----------------------------------------------------------------------------
   procedure Remove_Bead (Location : in out Othello_Types.Cell_Record)
   is
   begin
      Location.Pixmap.Background_Color(Location.Button.Background_Color);
   end Remove_Bead;
   ----------------------------------------------------------------------------
   procedure Flip_Bead (Location : in out Cell_Record)
   is
   begin
      Remove_Bead (Location);
      Put_Bead (Location);
   end Flip_Bead;
   ----------------------------------------------------------------------------
   function Check_Move_1 (This_Board : in Board_Matrix;
                          Row        : in Othello_Types.Valid_Row;
                          Column     : in Othello_Types.Valid_Column;
                          Dx         : in Integer;
                          Dy         : in Integer;
                          Player     : in Bead_Color)
                         return Integer
   is
      DRow    : Integer := Integer (Row);
      DColumn : Integer := Integer (Column);
      Factor  : Integer := 0;
   begin -- Check_Move_1
      loop
         DRow    := DRow    + Dy;
         DColumn := DColumn + Dx;
         exit when not (DColumn in Othello_Types.Valid_Column and DRow in Othello_Types.Valid_Row);
         if This_Board (Othello_Types.Valid_Row (DRow), Othello_Types.Valid_Column (DColumn)).Cell = Empty then
            return 0;
         elsif This_Board (Othello_Types.Valid_Row (DRow), Othello_Types.Valid_Column(DColumn)).Cell = Player then
            return Factor;
         elsif (DColumn = 1 or DColumn = 8 or DRow = 1 or DRow = 8) then
            Factor := Factor + 10;
         else
            Factor := Factor + 1;
         end if;
      end loop;
      return 0;
   end Check_Move_1;
   ----------------------------------------------------------------------------
   function Check_Move (This_Board : in Board_Matrix;
                        Row        : in Othello_Types.Valid_Row;
                        Column     : in Othello_Types.Valid_Column;
                        Player     : in Bead_color)
                       return Integer
   is
   begin -- Check_Move
      if This_Board (Row, Column).Cell /= Empty then
         return 0;
      else
         return (Check_Move_1 (This_Board, Row, Column,  0,  1, Player) +
                 Check_Move_1 (This_Board, Row, Column,  1,  0, Player) +
                 Check_Move_1 (This_Board, Row, Column,  0, -1, Player) +
                 Check_Move_1 (This_Board, Row, Column, -1,  0, Player) +
                 Check_Move_1 (This_Board, Row, Column,  1,  1, Player) +
                 Check_Move_1 (This_Board, Row, Column,  1, -1, Player) +
                 Check_Move_1 (This_Board, Row, Column, -1,  1, Player) +
                 Check_Move_1 (This_Board, Row, Column, -1, -1, Player));
      end if;
   end Check_Move;
   ----------------------------------------------------------------------------
   function Count_Bead
      (Playing_Board : in Othello_Types.Board_Matrix;
       Color         : in Bead_Color)
       return integer
   is
      Count : Integer := 0;
   begin
      for Row in Othello_Types.Valid_Row loop
         for Column in Othello_Types.Valid_Column loop
            if Playing_Board (Row, Column).Cell = Color then
               Count := Count + 1;
            end if;
         end loop;
      end loop;
      return Count;
   end Count_Bead;

   ----------------------------------------------------------------------------
   procedure Update_Count
      (Othello       : in out Othello_Types.Othello_Record'Class;
       Playing_Board : in out Othello_Types.Board_Matrix)
   is
      S          : String (1 .. 2);
   begin
      Put (S, Count_Bead (Playing_Board,Blue));
      Othello.Blue_Statusbar.Text("Blue = " & S);

      Put (S, Count_Bead (Playing_Board,Red));
      Othello.Red_Statusbar.Text("Red = " & S);
   end Update_Count;
   ----------------------------------------------------------------------------
   procedure Put_Move_1 (This_Board : in out Board_Matrix;
                         Row        : in     Othello_Types.Valid_Row;
                         Column     : in     Othello_Types.Valid_Column;
                         Dx         : in     Integer;
                         Dy         : in     Integer;
                         Player     : in     Bead_Color)
   is
      DRow    : Integer := Integer (Row);
      DColumn : Integer := Integer (Column);
   begin
      loop
         DRow    := DRow    + Dy;
         DColumn := DColumn + Dx;
         exit when not (DColumn in Othello_Types.Valid_Column and DRow in Othello_Types.Valid_Row);
         if This_Board (DRow, DColumn).Cell = Empty or This_Board (DRow, DColumn).Cell = Player then
            exit;
         end if;
         This_Board (DRow, DColumn).Cell := Player;

         if This_Board (DRow, DColumn).Flag then
            Flip_Bead (This_Board (DRow, DColumn));
         end if;
      end loop;
   end Put_Move_1;
   ----------------------------------------------------------------------------
   procedure Put_Move (Othello    : in out Othello_Types.Othello_Record'Class;
                       This_Board : in out Board_Matrix;
                       Row        : in     Othello_Types.Valid_Row;
                       Column     : in     Othello_Types.Valid_Column;
                       Player     : in     Bead_Color)
   is
   begin
      This_Board (Row, Column).Cell := Player;

      if This_Board (Row, Column).Flag then
         Put_Bead (This_Board (Row, Column));
      end if;

      for Dx in -1 .. 1 loop
         for Dy in -1 .. 1 loop
            if (Dx /= 0 or Dy /= 0) and Check_Move_1 (This_Board, Row, Column, Dx, Dy, Player) > 0 then
               Put_Move_1 (This_Board, Row, Column, Dx, Dy, Player);
            end if;
         end loop;
      end loop;
   end Put_Move;
   ----------------------------------------------------------------------------
   procedure Copy_Board (Target_Board :    out Board_Matrix;
                         Source_Board : in     Board_Matrix;
                         Flag         : in     Boolean := True)
   is
      procedure Copy_Cell
         (Source : in  Othello_Types.Cell_Record;
          Target : out Othello_Types.Cell_Record)
      is
      begin
         Target.Cell          := Source.Cell;
         Target.Flag          := Source.Flag;
         Target.Button_Setup  := Source.Button_Setup;
         Target.Button.Row    := Source.Button.Row;
         Target.Button.Column := Source.Button.Column;
      end Copy_Cell;
   begin
      for I in Othello_Types.Valid_Row loop
         for J in Othello_Types.Valid_Column loop
            Copy_Cell
               (Source => Source_Board(I,J),
                Target => Target_Board(I,J));
            --Target_Board (I, J) := Source_Board (I, J);
            Target_Board (I, J).Flag := Flag;
         end loop;
      end loop;
   end Copy_Board;
   ----------------------------------------------------------------------------
   function No_Take (This_Board : in Board_Matrix;
                     Row        : in Othello_Types.Valid_Row;
                     Column     : in Othello_Types.Valid_Column)
                    return Boolean
   is

      function No_Take_1 (This_Board : in Board_Matrix;
                          Row        : in Othello_Types.Valid_Row;
                          Column     : in Othello_Types.Valid_Column;
                          Dx         : in Integer;
                          Dy         : in Integer)
                         return Boolean
      is
         ----------------------------------------------------------------------
         function No_Take_2 (This_Board : in Board_Matrix;
                             Row        : in Othello_Types.Valid_Row;
                             Column     : in Othello_Types.Valid_Column;
                             Dx         : in Integer;
                             Dy         : in Integer)
                            return Othello_Types.Cell_Status
         is
            DRow    : Integer := Row;
            DColumn : integer := Column;
         begin -- No_Take_2
            DRow    := DRow    + Dy;
            DColumn := DColumn + Dx;
            if DRow in Othello_Types.Valid_Row and DColumn in Othello_Types.Valid_Column then
               while This_Board (DRow, DColumn).Cell = Empty loop
                  DRow    := DRow    + Dy;
                  DColumn := DColumn + Dx;
                  if (DRow < 1 or DRow > 8 or DColumn < 1 or DColumn > 8) or else
                    This_Board (DRow, DColumn).Cell = Empty then
                     return Player_Move;
                  end if;
               end loop;
            end if;
            while (DRow >= 1 and DRow <= 8 and DColumn >= 1 and DColumn <= 8) and then
              This_Board (DRow, DColumn).Cell = Computer_Move loop
               DRow    := DRow    + Dy;
               DColumn := DColumn + Dx;
            end loop;
            if DRow < 1 or DRow > 8 or DColumn < 1 or DColumn > 8 then
               return Computer_Move;
            end if;
            return This_Board (DRow, DColumn).Cell;
         end No_Take_2;
         ----------------------------------------------------------------------
         C1, C2 : Othello_Types.Cell_Status;

      begin -- No_Take_1
         C1 := No_Take_2 (This_Board, Row, Column,  Dx,  Dy);
         C2 := No_Take_2 (This_Board, Row, Column, -Dx, -Dy);
         return not ((C1 = Player_Move and C2 = Empty) or
                     (C1 = Empty and C2 = Player_Move));
      end No_Take_1;

   begin -- No_Take
      return (No_Take_1 (This_Board, Row, Column, 0,  1) and
              No_Take_1 (This_Board, Row, Column, 1,  1) and
              No_Take_1 (This_Board, Row, Column, 1,  0) and
              No_Take_1 (This_Board, Row, Column, 1, -1));
   end No_Take;
   ----------------------------------------------------------------------------
   function Side_Move
      (Othello       : in out Othello_Types.Othello_Record'Class;
       Playing_Board : in out Othello_Types.Board_Matrix;
       Row           : in Othello_Types.Valid_Row;
       Column        : in Othello_Types.Valid_Column;
       Computer      : in Bead_Color)
       return Integer
   is
      Dummy_Board : Board_Matrix;
      Ok, Dkl     : Integer;
      C, S        : Integer;
      Side        : Integer := 0;
      Oside       : Othello_Integer;
   begin
      Copy_Board (Dummy_Board, Playing_Board, False);
      Put_Move (Othello,Dummy_Board, Row, Column, Computer);

      if Row = Othello_Types.Valid_Row'First or Row = Othello_Types.Valid_Row'Last then
         Side := Side + 1;
      end if;

      if Column = Othello_Types.Valid_Column'First or Column = Othello_Types.Valid_Column'Last then
         Side := Side + 1;
      end if;

      S  := 0;
      Ok := 0;

      if Side = 2 or No_Take (Playing_Board, Row, Column) then
         Ok := Ok + 1;
      end if;

      Oside := 0;
      for K in Othello_Types.Valid_Row loop
         for L in Othello_Types.Valid_Column loop
            C := Check_Move (Dummy_Board, K, L, Computer);
            if C > 0 then
               Dkl := 1;
               if K = Othello_Types.Valid_Row'First or K = Othello_Types.Valid_Row'Last then
                  Dkl   := Dkl + 2;
                  Oside := Oside or 4;
               end if;
               if L = Othello_Types.Valid_Column'First or L = Othello_Types.Valid_Column'Last then
                  Dkl   := Dkl + 2;
                  Oside := Oside or 4;
               end if;
               if Dkl = 5 then
                  Dkl   := 10;
                  Oside := Oside or 16;
                  Oside := Oside or 1;
                  S     := S - Dkl;
                  if C >= 10 then
                     S     := S - 4;
                     Oside := Oside or 8;
                  end if;
               elsif No_Take (Dummy_Board, K, L) then
                  Oside := Oside or Othello_Integer (1);
                  S     := S - Dkl;
                  if C >= 10 then
                     S     := S - 4;
                     Oside := Oside or 8;
                  end if;
               end if;
            end if;
         end loop;
      end loop;

      if S < Integer (-Oside) then
         S := Integer (-Oside);
      end if;

      if Side > 0 then
         return (S + Side - 7 + 10 * Ok);
      end if;

      if (Row = Othello_Types.Valid_Row'First + 1) or (Row = Othello_Types.Valid_Row'Last - 1) then
         S    := S - 1;
         Side := Side + 1;
      end if;

      if (Column = Othello_Types.Valid_Column'First  + 1) or (Column = Othello_Types.Valid_Column'Last - 1) then
         S    := S - 1;
         Side := Side + 1;
      end if;

      if Side > 0 then
         return S;
      end if;

      if (Row = Othello_Types.Valid_Row'First + 2) or (Row = Othello_Types.Valid_Row'Last - 2) then
         S    := S + 1;
      end if;

      if (Column = Othello_Types.Valid_Column'First  + 2) or (Column = Othello_Types.Valid_Column'Last - 2) then
         S    := S + 1;
      end if;

      return S;
   end Side_Move;
   ----------------------------------------------------------------------------
   procedure Get_Possible_Moves
      (Othello        : in out Othello_Types.Othello_Record'Class;
       Playing_Board  : in out Othello_Types.Board_Matrix;
       Computer       : in     Bead_Color;
       Possible_Moves :    out Possible_Moves_Matrix;
       Factor         :    out Integer)
   is
      K : Integer := 1;
   begin
      Factor := 0;
      for I in Othello_Types.Valid_Row loop
         for J in Othello_Types.Valid_Column loop
            Possible_Moves (K).Check := Check_Move (Playing_Board, I, J, Computer);
            if Possible_Moves (K).Check > 0 then
               Possible_Moves (K).Row    := I;
               Possible_Moves (K).Column := J;
               Possible_Moves (K).Side   := Side_Move (Othello,Playing_Board,I, J, Computer);
               Factor := Factor + 1;
               K      := K + 1;
            end if;
         end loop;
      end loop;
   end Get_Possible_Moves;
   ----------------------------------------------------------------------------
   procedure Pick_Move (Possible_Moves : in     Possible_Moves_Matrix;
                        Board          : in     Othello_Types.Board_Matrix;
                        Row            :    out Othello_Types.Valid_Row;
                        Column         :    out Othello_Types.Valid_Column)
   is
      package Math renames Ada.Numerics.Elementary_Functions;

      type Weight_Set is array (Othello_Types.Valid_Row, Othello_Types.Valid_Column) of Float;

      Weight : constant Weight_Set := (1 | 8 => (1 | 8 => 21.0, 2 .. 7 => 18.0),
                                       2 | 7 => (1 | 8 => 18.0, 2 | 7 => 3.0, 3 .. 6 => 6.0),
                                       3 | 6 => (1 | 8 => 18.0, 2 | 7 => 6.0, 3 | 6 => 12.0, 4 .. 5 => 9.0),
                                       4 | 5 => (1 | 8 => 18.0, 2 | 7 => 6.0, 3 .. 6 => 9.0) );

      Count : constant Float := Float (Count_Bead (Board, Blue) + Count_Bead (Board, Red) );
      Fp    : constant Float := Math.Exp (-Count / 48.0); -- Factor for positioning weight
      Fm    : constant Float := 1.0 - Fp;                 -- Factor for material weight (Check component of Possible_Moves)
      -- Factors give more weight to positioning early and to material later

      Wt         : Float; -- Total weighting
      Max_Weight : Float; -- Best Wt seen so far
   begin
      if Possible_Moves'Length = 0 then
         raise Program_Error with "Pick_Move: No possible moves";
      end if;

      Row := Possible_Moves (Possible_Moves'First).Row;
      Column := Possible_Moves (Possible_Moves'First).Column;
      Max_Weight := Fm * Float (Possible_Moves (Possible_Moves'First).Check) + Fp * Weight (Row, Column);

      Find_Max : for I in Possible_Moves'First + 1 .. Possible_Moves'Last loop
         Wt := Fm * Float (Possible_Moves (I).Check) + Fp * Weight (Possible_Moves (I).Row, Possible_Moves (I).Column);

         if Wt > Max_Weight then
            Row := Possible_Moves (I).Row;
            Column := Possible_Moves (I).Column;
            Max_Weight := Wt;
         end if;
      end loop Find_Max;
   end Pick_Move;
   ----------------------------------------------------------------------------
   procedure Computer_Make_Move
      (Othello       : in out Othello_Types.Othello_Record'Class;
       Playing_Board : in out Othello_Types.Board_Matrix;
       Whose_Move    : in out Othello_Types.Bead_Color;
       Game_Over     : in out Boolean)
   is
      Row             : Othello_Types.Valid_Row;
      Column          : Othello_Types.Valid_Column;
      Possible_Moves  : Possible_Moves_Matrix (1 .. 64);
      Computer_Factor : Integer;
      Player_Factor   : Integer;
   begin
      Othello.Main_Statusbar.Text("Computer Move");

      Get_Possible_Moves (Othello,Playing_Board,Computer_Move, Possible_Moves, Computer_Factor);
      if Computer_Factor > 0 then
         Pick_Move (Possible_Moves (1 .. Computer_Factor), Playing_Board, Row, Column);
         Put_Move (Othello,Playing_Board, Row, Column, Computer_Move);
      end if;
      Get_Possible_Moves (Othello,Playing_Board, Player_Move, Possible_Moves, Player_Factor);
      if Player_Factor > 0 then
         Whose_Move := Player_Move;
         Othello.Main_Statusbar.Text("Your Move");
      else
         Get_Possible_Moves (Othello,Playing_Board,Computer_Move, Possible_Moves, Computer_Factor);
         if Computer_Factor > 0 then
            Othello.Main_Statusbar.Text("You must pass!");
         else
            declare
               Blue_Count : Integer := Count_Bead (Playing_Board,Blue);
               Red_Count  : Integer := Count_Bead (Playing_Board,Red);
            begin
               if Blue_Count = Red_Count then
                  Othello.Main_Statusbar.Text("Tie");
               elsif Blue_Count > Red_Count then
                  Othello.Main_Statusbar.Text("You Win!");
               else
                  Othello.Main_Statusbar.Text("You Lose");
               end if;
               Game_Over := True;
            end ;
         end if;
      end if;
   end Computer_Make_Move;
   ----------------------------------------------------------------------------
   procedure Pass
      (Othello       : in out Othello_Types.Othello_Record'Class;
       Playing_Board : in out Othello_Types.Board_Matrix;
       Whose_Move    : in out Othello_Types.Bead_Color;
       Game_Over     : in out Boolean)
   is
   begin
      Computer_Make_Move
         (Othello       => Othello,
          Playing_Board => Playing_Board,
          Whose_Move    => Whose_Move,
          Game_Over     => Game_Over);
      Update_Count
         (Othello       => Othello,
          Playing_Board => Playing_Board);
   end Pass;
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   procedure Make_Move
      (Othello       : in out Othello_Types.Othello_Record'Class;
       Playing_Board : in out Othello_Types.Board_Matrix;
       Whose_Move    : in out Othello_Types.Bead_Color;
       Game_Over     : in out Boolean;
       Row           : in     Othello_Types.Valid_Row;
       Column        : in     Othello_Types.Valid_Column)
   is

   begin
      if Game_Over = True then
         null; -- do nothing if game is over
      elsif Playing_Board (Row, Column).Cell /= Othello_Types.Empty then
         Beep(Othello);
      else
         if Whose_Move = Computer_Move then
            Beep(Othello);
         else
            if Check_Move (Playing_Board, Row, Column, Player_Move) > 0 then
               Put_Move (Othello,Playing_Board, Row, Column, Player_Move);
               --Set_Cursor (Left_Pointer);
               Whose_Move := Computer_Move;
               Computer_Make_Move
                  (Othello       => Othello,
                   Playing_Board => Playing_Board,
                   Whose_Move    => Whose_Move,
                   Game_Over     => Game_Over);
            else
               Beep(Othello);
            end if;
         end if;
      end if;
      Update_Count
         (Othello       => Othello,
          Playing_Board => Playing_Board);
   end Make_Move;

   ----------------------------------------------------------------------------
   procedure Setup_Board
      (Othello       : in out Othello_Types.Othello_Record'Class;
       Playing_Board : in out Othello_Types.Board_Matrix)
   is
   begin
      for Row in Othello_Types.Valid_Row loop
         for Column in Othello_Types.Valid_Column loop
            if Playing_Board (Row, Column).Cell /= Empty then
               Remove_Bead (Playing_Board (Row, Column));
               Playing_Board (Row, Column).Cell := Empty;
            end if;
            if not Playing_Board (Row, Column).Button_Setup then
               Playing_Board (Row, Column).Button_Setup := True;
            end if;
            Playing_Board (Row, Column).Flag := True;
         end loop;
      end loop;

      Playing_Board (4, 4).Cell := Red;
      Put_Bead (Playing_Board (4, 4));

      Playing_Board (5, 5).Cell := Red;
      Put_Bead (Playing_Board (5, 5));

      Playing_Board (4, 5).Cell := Blue;
      Put_Bead (Playing_Board (4, 5));

      Playing_Board (5, 4).Cell := Blue;
      Put_Bead (Playing_Board (5, 4));

      Othello.Main_Statusbar.Text("Your Move");
      Update_Count
         (Othello       => Othello,
          Playing_Board => Playing_Board);
   end Setup_Board;
   ----------------------------------------------------------------------------
   procedure New_Game
      (Othello       : in out Othello_Types.Othello_Record'Class;
       Playing_Board : in out Othello_Types.Board_Matrix;
       Whose_Move    : in out Othello_Types.Bead_Color;
       Game_Over     : in out Boolean)
   is
   begin
      Whose_Move := Player_Move;
      Game_Over  := False;
      Setup_Board
         (Othello       => Othello,
          Playing_Board => Playing_Board);
   end New_Game;


   procedure Initialize is

   begin
      Gnoga.Application.Title ("Ada Othello");

      Gnoga.Application.HTML_On_Close (Othello_Types.End_Message);

      Gnoga.Application.Multi_Connect.Initialize;

      Gnoga.Application.Multi_Connect.On_Connect_Handler
         (Event => Othello_Pkg.Callbacks.On_Connect'Access,
          Path  => "default");

      Gnoga.Application.Multi_Connect.Message_Loop;
   end Initialize;
   ----------------------------------------------------------------------------
end Othello_Pkg;
