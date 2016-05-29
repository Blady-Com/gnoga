-- Mine Detector Game
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- v7.2 2016 May 15          Speed improvement
-- v7.1 2016 Feb 15          Cleaned up unreferenced packages and variables that are not modified
-- V7.0 2014 Dec 01          First Gnoga version
--
with Ada.Numerics.Discrete_Random;

with User_IF;

package body Field.Operations is
   procedure Detect (Field : in out Field_Info; Cell : in Cell_Location) is
      -- null;
   begin -- Detect
      if Field.Dead then
         return;
      end if;

      if Field.Mine_Field (Cell.Row, Cell.Column).State = Marked then
         return; -- Can't count a marked cell
      end if;

      if Field.Mine_Field (Cell.Row, Cell.Column).Count not in Valid_Count then -- Cell has not been counted
         Field.Mine_Field (Cell.Row, Cell.Column).Count := 0;

         Count_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
            Count_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
               if Field.Mine_Field (Row, Column).Mine then
                  Field.Mine_Field (Cell.Row, Cell.Column).Count := Field.Mine_Field (Cell.Row, Cell.Column).Count + 1;
               end if;
            end loop Count_Columns;
         end loop Count_Rows;

         User_IF.Display_Count (Data    => Field.App_Data,
                                Count   => Field.Mine_Field (Cell.Row, Cell.Column).Count,
                                Stepped => Field.Mine_Field (Cell.Row, Cell.Column).State = Stepped_On,
                                Cell    => Cell);
      end if;
   end Detect;

   procedure Set_Mine_Count (Field : in out Field_Info; New_Mine_Count : in Natural) is
      -- null;
   begin -- Set_Mine_Count
      Field.Num_Mines := New_Mine_Count;
   end Set_Mine_Count;

   procedure Reset (Field : in out Field_Info) is
      subtype Rand_Set_Index is Integer range 1 .. Valid_Row'Last * Valid_Column'Last;
      type Rand_Set is array (Rand_Set_Index) of Cell_Location; -- For randomly placing mines

      package Random is new Ada.Numerics.Discrete_Random (Rand_Set_Index);

      Rand_List : Rand_Set;
      Index     : Positive := Positive'Last;
      Gen       : Random.Generator;
      Temp      : Cell_Location;
   begin -- Reset
      Field.Dead := False;
      Field.Mine_Field := Field_Set'(others => (others => Cell_Info'(State   => Normal,
                                                                     Mine    => False,
                                                                     Count   => Count_Value'First,
                                                                     Stepped => False) ) );
      Field.To_Mark := Field.Num_Mines;
      Field.Step_Count := 0;

      -- Set the extra ring around the field to stepped_on
      Step_On_Sides : for Row in Field.Mine_Field'range (1) loop
         Field.Mine_Field (Row, Field.Mine_Field'First (2) ).State := Stepped_On;
         Field.Mine_Field (Row, Field.Mine_Field'Last  (2) ).State := Stepped_On;
      end loop Step_On_Sides;

      Step_On_Top_Bottom : for Column in Field.Mine_Field'range (2) loop
         Field.Mine_Field (Field.Mine_Field'First (1), Column).State := Stepped_On;
         Field.Mine_Field (Field.Mine_Field'Last  (1), Column).State := Stepped_On;
      end loop Step_On_Top_Bottom;

      -- Fill Rand_List with all cell locations in preparation for placing mines
      Fill_Rows : for Row in Valid_Row loop
         Fill_Columns : for Column in Valid_Column loop
            Rand_List (Valid_Column'Last * (Row - 1) + Column) := Cell_Location'(Row => Row, Column => Column);
         end loop Fill_Columns;
      end loop Fill_Rows;

      Random.Reset (Gen);

      -- Shuffle Rand_List, a list of cell locations
      Shuffle : for I in Rand_List'range loop
         Index := Random.Random (Gen);
         Temp := Rand_List (I);
         Rand_List (I) := Rand_List (Index);
         Rand_List (Index) := Temp;
      end loop Shuffle;

      -- Put mines in the first Num_Mines locations in Rand_List
      Set_Mines : for I in 1 .. Field.Num_Mines loop
         Field.Mine_Field (Rand_List (I).Row, Rand_List (I).Column).Mine := True;
      end loop Set_Mines;

      -- Display the mine field
      User_IF.Reset_Screen (Field.App_Data);

      Display_Rows : for Row in Valid_Row loop
         Display_Columns : for Column in Valid_Column loop
            if Row    = Valid_Row'First    or else Row    = Valid_Row'Last or else
               Column = Valid_Column'First or else Column = Valid_Column'Last
            then -- Cell is on the edge; automatically count these for the player
               Detect (Field => Field, Cell => Cell_Location'(Row => Row, Column => Column) );
            end if;
         end loop Display_Columns;
      end loop Display_Rows;

      User_IF.Display_To_Go (Data => Field.App_Data, To_Go => Field.To_Mark);
   end Reset;

   function Stepped_On_Neighbor (Field : Field_Info; Cell : Cell_Location) return Boolean is
   -- See if a cell has a stepped-on neighbor

      -- null;
   begin -- Stepped_On_Neighbor
      Check_Row : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
         Check_Column : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
            if (Row /= Cell.Row or else Column /= Cell.Column) and then Field.Mine_Field (Row, Column).State = Stepped_On then
               return True;
            end if;
         end loop Check_Column;
      end loop Check_Row;

      return False;
   end Stepped_On_Neighbor;

   function Marked_Neighbor (Field : Field_Info; Cell : Cell_Location) return Boolean is -- See if a cell has a marked neighbor
      -- null;
   begin -- Marked_Neighbor
      Check_Row : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
         Check_Column : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
            if (Row /= Cell.Row or else Column /= Cell.Column) and then Field.Mine_Field (Row, Column).State = Marked then
               return True;
            end if;
         end loop Check_Column;
      end loop Check_Row;

      return False;
   end Marked_Neighbor;

   function Num_Marked_Neighbors (Field : Field_Info; Cell : Cell_Location) return Valid_Count is
      Result : Valid_Count := 0;
   begin -- Num_Marked_Neighbors
      Count_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
         if Row in Valid_Row then
            Count_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
               if Column in Valid_Column and then Field.Mine_Field (Row, Column).State = Marked then
                  Result := Result + 1;
               end if;
            end loop Count_Columns;
         end if;
      end loop Count_Rows;

      return Result;
   end Num_Marked_Neighbors;

   function Mark_Count_Satisfied (Field : Field_Info; Cell : Cell_Location) return Boolean is
      -- null;
   begin -- Mark_Count_Satisfied
      return Field.Mine_Field (Cell.Row, Cell.Column).Count = Num_Marked_Neighbors (Field, Cell);
   end Mark_Count_Satisfied;

   procedure Auto_Step (Field : in out Field_Info; Cell : in Cell_Location) is -- Doug's version
   -- Automatically step on any (unstepped-upon) neighbors of Cell if:
   --   (1) Cell has as many marked neighbors its count, or
   --   (2) the neighbor has as many marked neighbors as its count.

      Cell_Satisfied : constant Boolean := Mark_Count_Satisfied (Field, Cell);
   begin -- Auto_Step
      Step_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
         if Row in Valid_Row then
            Step_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
               if Column in Valid_Column and then Field.Mine_Field (Row, Column).State /= Marked then
                  if Cell_Satisfied or Mark_Count_Satisfied (Field, (Row => Row, Column => Column) ) then
                     Step (Field => Field, Cell => (Row => Row, Column => Column) );
                  end if;
               end if;
            end loop Step_Columns;
         end if;
      end loop Step_Rows;
   end Auto_Step;

   procedure Mark (Field : in out Field_Info; Cell : in Cell_Location) is
      Old_State : constant State_Id := Field.Mine_Field (Cell.Row, Cell.Column).State;
   begin -- Mark
      if Field.Dead then
         return;
      end if;

      if Stepped_On_Neighbor (Field, Cell) or else Marked_Neighbor (Field, Cell) then
         Field.Mine_Field (Cell.Row, Cell.Column).State := Marked; -- Force detect to count cell's neighbors

         Count_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop -- Automatically detect around marked cell
            if Row in Valid_Row then
               Count_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
                  if Column in Valid_Column then
                     Detect (Field => Field, Cell => Cell_Location'(Row => Row, Column => Column) );
                  end if;
               end loop Count_Columns;
            end if;
         end loop Count_Rows;

         Field.Mine_Field (Cell.Row, Cell.Column).State := Old_State;

         case Old_State is
         when Normal => -- Mark it
            Field.Mine_Field (Cell.Row, Cell.Column).State := Marked;
            User_IF.Display_Mark (Data => Field.App_Data, Cell => Cell);
            Field.To_Mark := Field.To_Mark - 1;
         when Marked => -- Unmark it
            Field.Mine_Field (Cell.Row, Cell.Column).State := Normal;

            User_IF.Display_Count (Data    => Field.App_Data,
                                   Count   => Field.Mine_Field (Cell.Row, Cell.Column).Count,
                                   Stepped => Field.Mine_Field (Cell.Row, Cell.Column).State = Stepped_On,
                                   Cell    => Cell);

            Field.To_Mark := Field.To_Mark + 1;
         when Stepped_On =>
            null; -- Can't marked a stepped-on cell
         end case;

         User_IF.Display_To_Go (Data => Field.App_Data, To_Go => Field.To_Mark);

         if User_IF.Extended_Stepping (Field.App_Data) then
            Auto_Step (Field => Field, Cell => Cell);
         end if;
      end if;
   end Mark;

   procedure Step (Field : in out Field_Info; Cell : in Cell_Location) is
      function Num_Normal_Neighbors (Cell : Cell_Location) return Valid_Count is
         Result : Valid_Count := 0;
      begin -- Num_Normal_Neighbors
         Count_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
            if Row in Valid_Row then
               Count_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
                  if Column in Valid_Column and then Field.Mine_Field (Row, Column).State = Normal then
                     Result := Result + 1;
                  end if;
               end loop Count_Columns;
            end if;
         end loop Count_Rows;

         return Result;
      end Num_Normal_Neighbors;
   begin -- Step
      if Field.Dead then
         return;
      end if;

      if Field.Mine_Field (Cell.Row, Cell.Column).State = Marked then
         User_IF.Display_Mark (Data => Field.App_Data, Cell => Cell);

         return;
      end if;

      if Field.Mine_Field (Cell.Row, Cell.Column).Stepped then -- Avoid inifinite recursion.
         return;
      end if;

      if not Stepped_On_Neighbor (Field, Cell) and then not Marked_Neighbor (Field, Cell) then
         User_IF.Display_Blank (Data => Field.App_Data, Cell => Cell);
      else
         Field.Step_Count := Field.Step_Count + 1;
         Field.Mine_Field (Cell.Row, Cell.Column).State := Stepped_On;
         Field.Mine_Field (Cell.Row, Cell.Column).Stepped := True;

         Count_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop -- Automatically detect around stepped-on cell
            if Row in Valid_Row then
               Count_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
                  if Column in Valid_Column then
                     Detect (Field => Field, Cell => Cell_Location'(Row => Row, Column => Column) );
                  end if;
               end loop Count_Columns;
            end if;
         end loop Count_Rows;

         if Field.Mine_Field (Cell.Row, Cell.Column).Mine then -- Stepped on a mine!
            Field.Dead := True;
            User_IF.Display_Mine (Data => Field.App_Data, Cell => Cell);

            return;
         end if;

         User_IF.Display_Count (Data    => Field.App_Data,
                                Count   => Field.Mine_Field (Cell.Row, Cell.Column).Count,
                                Stepped => Field.Mine_Field (Cell.Row, Cell.Column).State = Stepped_On,
                                Cell    => Cell);

         Auto_Step (Field => Field, Cell => Cell);

         if Field.Dead then
            return;
         end if;

         if User_IF.Auto_Marking (Field.App_Data) then
            -- See if stepping here has created any normal cells that obviously contain mines;
            -- if so, mark them.
            if Field.Mine_Field (Cell.Row, Cell.Column).Count - Num_Marked_Neighbors (Field, Cell) =
               Num_Normal_Neighbors (Cell)
            then
               Mark_Rows : for Row in Cell.Row - 1 .. Cell.Row + 1 loop
                  Mark_Columns : for Column in Cell.Column - 1 .. Cell.Column + 1 loop
                     if Field.Mine_Field (Row, Column).State = Normal then
                        Mark (Field => Field, Cell => (Row => Row, Column => Column) );
                     end if;
                  end loop Mark_Columns;
               end loop Mark_Rows;
            end if;
         end if;

         Field.Step_Count := Field.Step_Count - 1;

         if Field.Step_Count <= 0 then
            Release_Rows : for Row in Valid_Row loop
               Release_Columns : for Column in Valid_Column loop
                  Field.Mine_Field (Row, Column).Stepped := False;
               end loop Release_Columns;
            end loop Release_Rows;
         end if;
      end if;
   end Step;

   -- The game is Lost when a mine has been stepped on, Won when all mines have been marked & all other cells stepped on,
   -- and In_Progress otherwise
   function Game_State (Field : Field_Info) return Game_State_ID is
      -- null;
   begin -- Game_State
      if Field.Dead then -- A mine has been stepped on
         return Lost;
      end if;

      Check_Rows : for Row in Valid_Row loop
         Check_Columns : for Column in Valid_Column loop
            if Field.Mine_Field (Row, Column).State = Normal or else
               (Field.Mine_Field (Row, Column).State = Marked) /= Field.Mine_Field (Row, Column).Mine
            then
               return In_Progress;
            end if;
         end loop Check_Columns;
      end loop Check_Rows;

      return Won;
   end Game_State;
end Field.Operations;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
