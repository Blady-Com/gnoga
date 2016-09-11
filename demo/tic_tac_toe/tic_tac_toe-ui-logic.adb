-- Tic_Tac_Toe: a program to play Tic-Tac-Toe
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
--
-- 2016 Aug 26     J. Carter      V1.0--Initial version
--
separate (Tic_Tac_Toe.UI)
package body Logic is
   procedure Process_Player_Move (App : in out App_Info; Row : in Row_ID; Column : in Column_ID) is
      procedure Disable_All (App : in out App_Info);
      -- Disables all the board squares

      procedure Enable_Empty (App : in out App_Info);
      -- Enables board squares that are empty

      function Game_Won (App : App_Info) return Boolean;
      -- Returns True if (Row, Column) results in a row, column, or diagonal with 3 of App.Player in a row; False otherwise

      function Count_Row (App : App_Info; Row : Row_ID; Value : Piece_ID) return Natural;
      -- Returns the number of squares in Row with value Value

      function Count_Column (App : App_Info; Column : Column_ID; Value : Piece_ID) return Natural;
      -- Returns the number of squares in Column with value Value

      function Count_UL_Diag (App : App_Info; Value : Piece_ID) return Natural;
      -- Returns the number of squares in the UL-LR diagonal with value Value

      function Count_UR_Diag (App : App_Info; Value : Piece_ID) return Natural;
      -- Returns the number of squares in the UR-LL diagonal with value Value

      procedure Computer_Move (App : in out App_Info; Won : out Boolean);
      -- Chooses and makes Computer's move
      -- Won is True if this move causes Computer to win; False otherwise

      procedure Disable_All (App : in out App_Info) is
         -- Empty
      begin -- Disable_All
         All_Rows : for Row in App.Square'Range (1) loop
            All_Columns : for Column in App.Square'Range (2) loop
               App.Square (Row, Column).Disabled;
            end loop All_Columns;
         end loop All_Rows;
      end Disable_All;

      procedure Enable_Empty (App : in out App_Info) is
         -- Empty
      begin -- Enable_Empty
         All_Rows : for Row in App.Square'Range (1) loop
            All_Columns : for Column in App.Square'Range (2) loop
               if App.Board (Row, Column) = Empty then
                  App.Square (Row, Column).Disabled (Value => False);
               end if;
            end loop All_Columns;
         end loop All_Rows;
      end Enable_Empty;

      function Game_Won (App : App_Info) return Boolean is
         Count : Natural := Count_Row (App, Row, App.Player);
      begin -- Game_Won
         if Count = 3 then
            return True;
         end if;

         Count := Count_Column (App, Column, App.Player);

         if Count = 3 then
            return True;
         end if;

         if Row = Column then -- On UL-LR diagonal
            Count := Count_UL_Diag (App, App.Player);

            if Count = 3 then
               return True;
            end if;
         end if;

         if (Row = 1 and Column = 3) or (Row = 2 and Column = 2) or (Row = 3 and Column = 1) then -- On UR-LL diagonal
            Count := Count_UR_Diag (App, App.Player);

            return Count = 3;
         end if;

         return False;
      end Game_Won;

      function Count_Row (App : App_Info; Row : Row_ID; Value : Piece_ID) return Natural is
         Result : Natural := 0;
      begin -- Count_Row
         All_Columns : for Column in App.Board'Range (2) loop
            if App.Board (Row, Column) = Value then
               Result := Result + 1;
            end if;
         end loop All_Columns;

         return Result;
      end Count_Row;

      function Count_Column (App : App_Info; Column : Column_ID; Value : Piece_ID) return Natural is
         Result : Natural := 0;
      begin -- Count_Column
         All_Rows : for Row in App.Board'Range (1) loop
            if App.Board (Row, Column) = Value then
               Result := Result + 1;
            end if;
         end loop All_Rows;

         return Result;
      end Count_Column;

      function Count_UL_Diag (App : App_Info; Value : Piece_ID) return Natural is
         Result : Natural := 0;
      begin -- Count_UL_Diag
         Count : for RC in App.Board'Range (1) loop
            if App.Board (RC, RC) = Value then
               Result := Result + 1;
            end if;
         end loop Count;

         return Result;
      end Count_UL_Diag;

      function Count_UR_Diag (App : App_Info; Value : Piece_ID) return Natural is
         Result : Natural := 0;
         Column : Column_ID := Column_ID'Last;
      begin -- Count_UR_Diag
         Count : for Row in App.Board'Range (1) loop
            if App.Board (Row, Column) = Value then
               Result := Result + 1;
            end if;

            if Column > Column_ID'First then
               Column := Column - 1;
            end if;
         end loop Count;

         return Result;
      end Count_UR_Diag;

      procedure Computer_Move (App : in out App_Info; Won : out Boolean) is
         type Count_Set is array (Row_ID, Column_ID) of Natural;

         procedure Find_Winning_Move (App : in out App_Info; Won : out Boolean);
         -- If there's a winning move, makes it and sets Won to True
         -- Otherwise, sets Won to False

         procedure Block_Player (App : in out App_Info; Moved : out Boolean);
         -- If Player can win, moves to block and sets Moved to True
         -- Otherwise, sets Moved to False

         procedure Check_Special_Cases (App : in out App_Info; Moved : out Boolean);
         -- If any of the special cases apply, makes the correct move and sets Moved to True
         -- Otherwise, sets Moved to False
         --
         -- These replace the heuristic "prevent Player from getting 2 in a row in more than one direction"
         -- Except for 6 cases on the Computer's 2nd move when Player is X and the Computer has moved in the center, this is
         -- accomplished by the other heuristics, so it's easier to just check those special cases

         function Board_Count (App : App_Info) return Count_Set;
         -- Returns the number of directions Computer can get 2 in a row for each square

         procedure Make_2_In_A_Row (App : in out App_Info; Count : in Count_Set; Moved : out Boolean);
         -- If Computer can make 2 in a row, makes that move

         procedure Move_Empty (App : in out App_Info);
         -- Finds an empty square and moves there

         procedure Find_Winning_Move (App : in out App_Info; Won : out Boolean) is
            -- Empty
         begin -- Find_Winning_Move
            Won := False;

            if App.Computer_Move <= 2 then
               return;
            end if;

            Win_Rows : for Row in App.Board'Range (1) loop
               Win_Columns : for Column in App.Board'Range (2) loop
                  if App.Board (Row, Column) = Empty then
                     if Count_Row (App, Row, App.Computer) = 2 or Count_Column (App, Column, App.Computer) = 2 then
                        App.Board (Row, Column) := App.Computer;
                        App.Square (Row, Column).Text (Value => App.Computer_Mark);
                        Won := True;

                        return;
                     end if;

                     if Row = Column and then Count_UL_Diag (App, App.Computer) = 2 then -- On UL-LR diagonal
                        App.Board (Row, Column) := App.Computer;
                        App.Square (Row, Column).Text (Value => App.Computer_Mark);
                        Won := True;

                        return;
                     end if;

                     if ( (Row = 1 and Column = 3) or (Row = 2 and Column = 2) or (Row = 3 and Column = 1) ) and then
                        Count_UR_Diag (App, App.Computer) = 2 -- On UR-LL diagonal
                     then
                        App.Board (Row, Column) := App.Computer;
                        App.Square (Row, Column).Text (Value => App.Computer_Mark);
                        Won := True;

                        return;
                     end if;
                  end if;
               end loop Win_Columns;
            end loop Win_Rows;
         end Find_Winning_Move;

         procedure Block_Player (App : in out App_Info; Moved : out Boolean) is
            -- Empty
         begin -- Block_Player
            Moved := False;

            if App.Player_Move <= 1 then
               return;
            end if;

            Block_Rows : for Row in App.Board'Range (1) loop
               Block_Columns : for Column in App.Board'Range (2) loop
                  if App.Board (Row, Column) = Empty then
                     if Count_Row (App, Row, App.Player) = 2 or else
                        Count_Column (App, Column, App.Player) = 2 or else
                        (Row = Column and then Count_UL_Diag (App, App.Player) = 2) or else -- On UL-LR diagonal
                        ( ( (Row = 1 and Column = 3) or (Row = 2 and Column = 2) or (Row = 3 and Column = 1) ) and then
                         Count_UR_Diag (App, App.Player) = 2) -- On UR-LL diagonal
                     then
                        App.Board (Row, Column) := App.Computer;
                        App.Square (Row, Column).Text (Value => App.Computer_Mark);
                        Moved := True;

                        return;
                     end if;
                  end if;
               end loop Block_Columns;
            end loop Block_Rows;
         end Block_Player;

         procedure Check_Special_Cases (App : in out App_Info; Moved : out Boolean) is
            -- Empty
         begin -- Check_Special_Cases
            Moved := False;

            if App.Computer_Move /= 2 or App.Board (2, 2) /= App.Computer or App.Player /= X then
               return;
            end if;

            if (App.Board (1, 1) = App.Player and App.Board (3, 3) = App.Player) or
               (App.Board (1, 3) = App.Player and App.Board (3, 1) = App.Player)
            then
               App.Board (1, 2) := App.Computer;
               App.Square (1, 2).Text (Value => App.Computer_Mark);
               Moved := True;

               return;
            end if;

            if App.Board (1, 1) = App.Player and App.Board (3, 2) = App.Player then
               App.Board (3, 1) := App.Computer;
               App.Square (3, 1).Text (Value => App.Computer_Mark);
               Moved := True;

               return;
            end if;

            if (App.Board (1, 3) = App.Player and App.Board (3, 2) = App.Player) or
               (App.Board (2, 3) = App.Player and App.Board (3, 2) = App.Player)
            then
               App.Board (3, 3) := App.Computer;
               App.Square (3, 3).Text (Value => App.Computer_Mark);
               Moved := True;

               return;
            end if;

            if App.Board (2, 1) = App.Player and App.Board (3, 3) = App.Player then
               App.Board (3, 1) := App.Computer;
               App.Square (3, 1).Text (Value => App.Computer_Mark);
               Moved := True;
            end if;
         end Check_Special_Cases;

         function Board_Count (App : App_Info) return Count_Set is
            Count : Count_Set := (Row_ID => (Column_ID => 0) );
         begin -- Board_Count
            Count_Rows : for Row in App.Board'Range (1) loop
               Count_Columns : for Column in App.Board'Range (2) loop
                  if App.Board (Row, Column) = Empty then
                     if Count_Row (App, Row, App.Computer) = 1 and then Count_Row (App, Row, Empty) = 2 then
                        Count (Row, Column) := Count (Row, Column) + 1;
                     end if;

                     if Count_Column (App, Column, App.Computer) = 1 and then Count_Column (App, Column, Empty) = 2 then
                        Count (Row, Column) := Count (Row, Column) + 1;
                     end if;

                     if Row = Column and then Count_UL_Diag (App, App.Computer) = 1 and then Count_UL_Diag (App, Empty) = 2 then
                        Count (Row, Column) := Count (Row, Column) + 1;
                     end if;

                     if ( (Row = 1 and Column = 3) or (Row = 2 and Column = 2) or (Row = 3 and Column = 1) ) and then
                        Count_UR_Diag (App, App.Computer) = 1 and then Count_UR_Diag (App, Empty) = 2
                     then
                        Count (Row, Column) := Count (Row, Column) + 1;
                     end if;
                  end if;
               end loop Count_Columns;
            end loop Count_Rows;

            return Count;
         end Board_Count;

         procedure Make_2_In_A_Row (App : in out App_Info; Count : in Count_Set; Moved : out Boolean) is
            procedure Move_If_2
               (App : in out App_Info; Count : in Count_Set; Row : in Row_ID; Column : in Column_ID; Moved : out Boolean);
            -- If (Row, Column) is empty and makes 2 in a row for Computer, moves there and sets Moved to True
            -- Otherwise, sets Moved to False

            procedure Move_If_2
               (App : in out App_Info; Count : in Count_Set; Row : in Row_ID; Column : in Column_ID; Moved : out Boolean)
            is
               -- Empty
            begin -- Move_If_2
               Moved := False;

               if Count (Row, Column) > 0 and App.Board (Row, Column) = Empty then
                  App.Board (Row, Column) := App.Computer;
                  App.Square (Row, Column).Text (Value => App.Computer_Mark);
                  Moved := True;
               end if;
            end Move_If_2;
         begin -- Make_2_In_A_Row
            Move_If_2 (App => App, Count => Count, Row => 2, Column => 2, Moved => Moved); -- Center is best

            if Moved then
               return;
            end if;

            Move_If_2 (App => App, Count => Count, Row => 1, Column => 1, Moved => Moved); -- Corners 2nd best

            if Moved then
               return;
            end if;

            Move_If_2 (App => App, Count => Count, Row => 1, Column => 3, Moved => Moved);

            if Moved then
               return;
            end if;

            Move_If_2 (App => App, Count => Count, Row => 3, Column => 1, Moved => Moved);

            if Moved then
               return;
            end if;

            Move_If_2 (App => App, Count => Count, Row => 3, Column => 3, Moved => Moved);

            if Moved then
               return;
            end if;

            Move_If_2 (App => App, Count => Count, Row => 1, Column => 2, Moved => Moved); -- Edge last choice

            if Moved then
               return;
            end if;

            Move_If_2 (App => App, Count => Count, Row => 2, Column => 1, Moved => Moved);

            if Moved then
               return;
            end if;

            Move_If_2 (App => App, Count => Count, Row => 2, Column => 3, Moved => Moved);

            if Moved then
               return;
            end if;

            Move_If_2 (App => App, Count => Count, Row => 3, Column => 2, Moved => Moved);
         end Make_2_In_A_Row;

         procedure Move_Empty (App : in out App_Info) is
            procedure Move_If_Empty (App : in out App_Info; Row : in Row_ID; Column : in Column_ID; Moved : out Boolean);
            -- If (Row, Column) is empty, moves there and sets Moved to True; otherwise, sets Moved to False

            procedure Move_If_Empty (App : in out App_Info; Row : in Row_ID; Column : in Column_ID; Moved : out Boolean) is
               -- Empty
            begin -- Move_If_Empty
               Moved := False;

               if App.Board (Row, Column) = Empty then
                  App.Board (Row, Column) := App.Computer;
                  App.Square (Row, Column).Text (Value => App.Computer_Mark);
                  Moved := True;
               end if;
            end Move_If_Empty;

            Moved : Boolean;
         begin -- Move_Empty
            Move_If_Empty (App => App, Row => 2, Column => 2, Moved => Moved); -- Move in the center if possible

            if Moved then
               return;
            end if;

            Move_If_Empty (App => App, Row => 1, Column => 1, Moved => Moved); -- Move in a corner if possible

            if Moved then
               return;
            end if;

            Move_If_Empty (App => App, Row => 1, Column => 3, Moved => Moved);

            if Moved then
               return;
            end if;

            Move_If_Empty (App => App, Row => 3, Column => 1, Moved => Moved);

            if Moved then
               return;
            end if;

            Move_If_Empty (App => App, Row => 3, Column => 3, Moved => Moved);

            if Moved then
               return;
            end if;

            Move_If_Empty (App => App, Row => 1, Column => 2, Moved => Moved); -- Move to an edge otherwise

            if Moved then
               return;
            end if;

            Move_If_Empty (App => App, Row => 2, Column => 1, Moved => Moved);

            if Moved then
               return;
            end if;

            Move_If_Empty (App => App, Row => 2, Column => 3, Moved => Moved);

            if Moved then
               return;
            end if;

            Move_If_Empty (App => App, Row => 3, Column => 2, Moved => Moved);

            if not Moved then
               raise Program_Error with "Move_Empty: impossible";
            end if;
         end Move_Empty;

         Moved : Boolean;
         Count : Count_Set;
      begin -- Computer_Move
         App.Computer_Move := App.Computer_Move + 1;
         Won := False;

         Find_Winning_Move (App => App, Won => Won); -- See if Computer can win

         if Won then
            return;
         end if;

         Block_Player (App => App, Moved => Moved); -- See if Player will win if not blocked

         if Moved then
            return;
         end if;


         Check_Special_Cases (App => App, Moved => Moved); -- Prevent Player from getting 2 in a row in more than one direction

         if Moved then
            return;
         end if;

         if App.Computer_Move > 1 then -- Count the # of directions that a move in each square gives 2 computer and 1 empty
            Count := Board_Count (App);

            Force_Rows : for Row in App.Board'Range (1) loop -- If any count > 1 then Computer can force a win
               Force_Columns : for Column in App.Board'Range (2) loop
                  if Count (Row, Column) > 1 then
                     App.Board (Row, Column) := App.Computer;
                     App.Square (Row, Column).Text (Value => App.Computer_Mark);

                     return;
                  end if;
               end loop Force_Columns;
            end loop Force_Rows;

            -- Moving to make 2 in a row forces PLayer to block
            Make_2_In_A_Row (App => App, Count => Count, Moved => Moved);

            if Moved then
               return;
            end if;
         end if;

         Move_Empty (App => App); -- No moves that make 2 in a row, so move to an empty square
      end Computer_Move;

      Won : Boolean;
   begin -- Process_Player_Move
      Disable_All (App => App);
      App.Player_Move := App.Player_Move + 1;
      App.Message.Text (Value => "");
      App.Board (Row, Column) := App.Player;
      App.Square (Row, Column).Text (Value => App.Player_Mark);

      if Game_Won (App) then
         App.Num_Won := App.Num_Won + 1;
         App.Won.Value (Value => App.Num_Won);
         App.Message.Text (Value => You_Won);

         return;
      end if;

      if App.Player_Move >= 5 and then App.Player = X then -- Player moved 1st, didn't win, no more moves, so kat
         App.Num_Kat := App.Num_Kat + 1;
         App.Kat.Value (Value => App.Num_Kat);
         App.Message.Text (Value => Kat_Game);

         return;
      end if;

      Computer_Move (App => App, Won => Won);

      if Won then -- Computer won, Player lost
         App.Num_Lost := App.Num_Lost + 1;
         App.Lost.Value (Value => App.Num_Lost);
         App.Message.Text (Value => You_Lost);

         return;
      end if;

      if App.Computer_Move >= 5 and then App.Computer = X then -- Comuter moved 1st, didn't win, no more moves, so kat
         App.Num_Kat := App.Num_Kat + 1;
         App.Kat.Value (Value => App.Num_Kat);
         App.Message.Text (Value => Kat_Game);

         return;
      end if;

      Enable_Empty (App => App);
      App.Message.Text (Value => Your_Turn);
   exception -- Process_Player_Move
   when E : others =>
      Gnoga.Log (Message => "Process_Player_Move: " & Ada.Exceptions.Exception_Information (E) );
   end Process_Player_Move;

   procedure Reset (App : in out App_Info) is
      -- Empty
   begin -- Reset
      All_Rows : for Row in App.Square'Range (1) loop
         All_Columns : for Column in App.Square'Range (2) loop
            App.Square (Row, Column).Text (Value => Empty_Mark);
            App.Square (Row, Column).Disabled (Value => False);
         end loop All_Columns;
      end loop All_Rows;

      App.Player_Move := 0;
      App.Computer_Move := 0;
      App.Board := Empty_Board;
      App.Message.Text (Value => Your_Turn);

      if not App.First_Check.Checked then
         App.Player := X;
         App.Player_Mark := X_Mark;
         App.Computer := O;
         App.Computer_Mark := O_Mark;
      else
         App.Player := O;
         App.Player_Mark := O_Mark;
         App.Computer := X;
         App.Computer_Mark := X_Mark;
         App.Board (2, 2) := App.Computer;
         App.Square (2, 2).Text (Value => App.Computer_Mark);
         App.Square (2, 2).Disabled;
         App.Computer_Move := 1;
      end if;
   exception -- Reset
   when E : others =>
      Gnoga.Log (Message => "Reset: " & Ada.Exceptions.Exception_Information (E) );
   end Reset;
end Logic;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
