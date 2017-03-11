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

with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;

package body Match_3 is

   function Find_any_match (b : Board) return Boolean is
   begin
      for x in b'Range (1) loop
         for y in b'Range (2) loop
            if b (x, y) in Movable_tile then
               --  Search right
               for x2 in x + 1 .. b'Last (1) loop
                  exit when b (x2, y) /= b (x, y);
                  if x2 = x + 2 then
                     return True;
                  end if;
               end loop;
               --  Search below
               for y2 in y + 1 .. b'Last (2) loop
                  exit when b (x, y2) /= b (x, y);
                  if y2 = y + 2 then
                     return True;
                  end if;
               end loop;
            end if;
         end loop;
      end loop;
      return False;
   end Find_any_match;

   function Random_cell (gen : Generator; p : Tile_prob) return Cell is
      v, vt : Prob;
      res : Cell;
   begin
      v := Prob (Random (gen));
      vt := 0.0;
      --  This is for having b (x, y) set in the 0-prob. case where v = 1.
      res := Simple_tile'Last;
      for t in Simple_tile loop
         vt := vt + p (t);  --  Cumulative probability
         if v < vt then
            res := t;
            exit;
         end if;
      end loop;
      return res;
   end Random_cell;

   procedure Fill (b : in out Board; p : Tile_prob) is
      gen : Generator;
   begin
      Reset (gen);
      loop
         for x in b'Range (1) loop
            for y in b'Range (2) loop
               b (x, y) := Random_cell (gen, p);
            end loop;
         end loop;
         exit when not Find_any_match (b);
      end loop;
   end Fill;

   function Matches (b : Board) return Match_list is
      ma : Match_list (1 .. b'Length (1) * b'Length (2));
      nm : Match_kind;
      n : Natural := 0;
      found : Boolean;
      --  Variable 'seen' is to prevent multiple-searching [[and multiple-counting matches]]
      seen : array (b'Range (1), b'Range (2)) of Boolean := (others => (others => False));
   begin
      for x in b'Range (1) loop
         for y in b'Range (2) loop
            if b (x, y) in Movable_tile and not seen (x, y) then
               found := False;
               --  Search right
               for x2 in x + 1 .. b'Last (1) loop
                  exit when b (x2, y) /= b (x, y) or seen (x2, y);
                  if x2 = x + 2 then
                     seen (x + 1, y) := True;
                     seen (x2, y) := True;
                     found := True;
                     nm := Hor_3;
                  elsif x2 = x + 3 then
                     seen (x2, y) := True;
                     nm := Hor_4;
                  elsif x2 >= x + 4 then
                     seen (x2, y) := True;
                     nm := Hor_5p;
                  end if;
               end loop;
               if not found then
                  --  Search below
                  for y2 in y + 1 .. b'Last (2) loop
                     exit when b (x, y2) /= b (x, y) or seen (x, y2);
                     if y2 = y + 2 then
                        seen (x, y + 1) := True;
                        seen (x, y2) := True;
                        found := True;
                        nm := Ver_3;
                     elsif y2 = y + 3 then
                        seen (x, y2) := True;
                        nm := Ver_4;
                     elsif y2 >= y + 4 then
                        seen (x, y2) := True;
                        nm := Ver_5p;
                     end if;
                  end loop;
               end if;
               if found then
                  n := n + 1;
                  ma (n) := (nm, (x, y));
               end if;
            end if;
         end loop;
      end loop;
      return ma (1 .. n);
   end Matches;

   procedure Empty_matching_cells (b : in out Board; ma : Match_list)  is
      x, y : Integer;
      c : Cell;
   begin
      for n in ma'Range loop
         case ma (n).kind is
            when Hor_3 .. Hor_5p =>
               y := ma (n).pos.y;
               c := b (ma (n).pos.x, y);
               for x in ma (n).pos.x .. b'Last (1) loop
                  exit when b (x, y) /= c;
                  b (x, y) := empty;
               end loop;
            when Ver_3 .. Ver_5p =>
               x := ma (n).pos.x;
               c := b (x, ma (n).pos.y);
               for y in ma (n).pos.y .. b'Last (2) loop
                  exit when b (x, y) /= c;
                  b (x, y) := empty;
               end loop;
         end case;
      end loop;
   end Empty_matching_cells;

   function Gravity_step (b : Board; p : Tile_prob) return Gravity_list is
      gl : Gravity_list (1 .. b'Length (1) * (b'Length (2) ** 2));
      n : Natural := 0;
      gen : Generator;
      bsim : Board := b;
      found_in_column : Boolean;
      --
      procedure Single_move (gi : Gravity_item) is
      begin
         n := n + 1;
         gl (n) := gi;
         if gi.from.y > 0 then
            bsim (gi.from.x, gi.from.y) := empty;
         end if;
         bsim (gi.to.x, gi.to.y) := gi.new_content;
      end Single_move;
      --
      --  Position of the new tiles coming above board; there can be a whole column of them!...
      new_tile_y : array (b'Range (1)) of Integer := (others => 0);
      --
      procedure New_tile (x, y : Positive) is
      begin
         Single_move ((new_content => Random_cell (gen, p), from => (x, new_tile_y (x)), to => (x, y)));
         new_tile_y (x) := new_tile_y (x) - 1;
      end New_tile;
   begin
      Reset (gen);
      --  Rows below top row
      for y in reverse 2 .. b'Last (2) loop
         for x in b'Range (1) loop
            if bsim (x, y) = empty then
               found_in_column := False;
               --  Search in the column above
               for yy in reverse 1 .. y - 1 loop
                  case bsim (x, yy) is
                     when empty =>
                        if yy = 1 then  --  whole column above (x,y) is empty, get 1 new tile
                           New_tile (x, y);
                           found_in_column := True;
                           exit;
                        else
                           null;  --  Continue searching
                        end if;
                     when Movable_tile =>
                        Single_move ((new_content => bsim (x, yy), from => (x, yy), to => (x, y)));
                        found_in_column := True;
                        exit;
                     when others =>  --  Any non-movable tile
                        exit;
                  end case;
               end loop;
               if found_in_column then
                  null;  --  Found a movable tile above, or the sky (then, a new random tile)
               elsif x > 1 and then bsim (x - 1, y - 1) in Movable_tile then  --  Diagonal: above, left
                  Single_move ((new_content => bsim (x - 1, y - 1), from => (x - 1, y - 1), to => (x, y)));
               elsif x < b'Last (1) and then bsim (x + 1, y - 1) in Movable_tile then  --  Diagonal: above, right
                  Single_move ((new_content => bsim (x + 1, y - 1), from => (x + 1, y - 1), to => (x, y)));
               end if;
            end if;
         end loop;
      end loop;
      --  Top row
      for x in b'Range (1) loop
         if bsim (x, 1) = empty then
            New_tile (x, 1);
         end if;
      end loop;
      return gl (1 .. n);
   end Gravity_step;

   procedure Apply_gravity_moves (b : in out Board; gl : Gravity_list) is
   begin
      for g of gl loop
         if g.from.y > 0 then
            b (g.from.x, g.from.y) := empty;
         end if;
         b (g.to.x, g.to.y) := g.new_content;
      end loop;
   end Apply_gravity_moves;

end Match_3;
