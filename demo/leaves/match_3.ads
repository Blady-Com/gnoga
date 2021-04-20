--  Match_3: library for managing the logics (not the graphics) of a "Match-3"-style game.

--  Legal licensing note:

--  Copyright (c) 2017 Gautier de Montmollin

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php

package Match_3 is

   type Cell is (empty, tile_1, tile_2, tile_3, tile_4, tile_5, tile_6, tile_7, tile_8, tile_9, tile_10);
   pragma Ordered (Cell);

   subtype Simple_tile is Cell range tile_1 .. tile_10;

   subtype Movable_tile is Simple_tile;

   subtype Real is Long_Float;

   subtype Prob is Real range 0.0 .. 1.0;

   type Tile_prob is array (Simple_tile) of Prob;

   --  Column, Row (top-down)
   --
   type Board is array (Positive range <>, Positive range <>) of Cell;

   procedure Fill
     (b : in out Board;
      p :        Tile_prob);

   function Find_any_match
     (b : Board)
      return Boolean;

   type Match_kind is (Hor_3, Hor_4, Hor_5p, Ver_3, Ver_4, Ver_5p);
   pragma Ordered (Match_kind);

   type Cell_pos is record
      x : Natural;  --  x = 0 only for the null position
      y : Integer;  --  y = 1 is the top row; y can be <= 0 for new cells "falling from the sky"
   end record;

   null_cell_pos : constant Cell_pos := (0, 0);

   ---------------
   --  Matches  --
   ---------------

   type Match_item is record
      kind : Match_kind;
      pos  : Cell_pos;  --  Pos is the top / left cell of a match
   end record;

   type Match_list is array (Positive range <>) of Match_item;

   function Matches
     (b : Board)
      return Match_list;

   procedure Empty_matching_cells
     (b  : in out Board;
      ma :        Match_list);

   ------------------------------------------------------------------------
   --  Gravity: fill holes with above tiles; on top row, with new tiles  --
   ------------------------------------------------------------------------

   type Gravity_item is record
      new_content : Cell;
      from, to    : Cell_pos;
   end record;

   type Gravity_list is array (Positive range <>) of Gravity_item;

   function Gravity_step
     (b : Board;
      p : Tile_prob)
      return Gravity_list;

   procedure Apply_gravity_moves
     (b  : in out Board;
      gl :        Gravity_list);

   --  TBD:
   --        - detect when there is no possible successful swap
   --        - reshuffle (to be used when no possible swap)

end Match_3;
