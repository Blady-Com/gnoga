-----------------------------------------------------------------------
--
--     Copyright (C) 2003 Dúlio Matos Leite de Carvalho e Silva
--
--     This file is part of LinXtris.
--
--     LinXtris is free software; you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation; either version 2 of the License, or
--     (at your option) any later version.
--
--     This program is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.
--
--     You should have received a copy of the GNU General Public License
--     along with this program; if not, write to the Free Software
--     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-----------------------------------------------------------------------

package Block_Engine is
   type Object (Width : Positive; Height : Positive) is limited private;
   type Move is (None, Right, Left, Down, Rotate, Drop);
   type Color is (Blue, Green, Red, Grey, Yellow, Cyan, Magenta, Blank, Ghost);
   type Lines_Indication is array (1 .. 4) of Natural;
   procedure Init (The : in out Object);
   procedure Set_Next_Move
     (The : in out Object;
      To  :        Move);
   procedure Set_Auto_Down (The : in out Object);
   procedure Set_Random_Piece_Color
     (The : in out Object;
      To  :        Boolean);
   function Get_Random_Piece_Color
     (The : in Object)
      return Boolean;
   function Get_Pieces_Number
     (The : Object)
      return Integer;
   function Get_Piece_Color
     (The : Object)
      return Color;
   function Get_Next_Piece_Color
     (The : Object)
      return Color;
   function Get_Score
     (The : Object)
      return Integer;
   function Get_Level
     (The : Object)
      return Integer;
   function Get_Initial_Level
     (The : Object)
      return Integer;
   function Get_Initial_Lines
     (The : Object)
      return Integer;
   function Get_Lines_Completed
     (The : Object)
      return Integer;
   function Get_Lowest_Line_Completed
     (The : Object)
      return Integer;
   function Get_Completed_Lines_Number
     (The : Object)
      return Lines_Indication;
   function Get_Lines_Number
     (The : Object)
      return Integer;
   function Game_Over
     (The : Object)
      return Boolean;
   function Level_Changed
     (The : Object)
      return Boolean;
   procedure Process_Move (The : in out Object);
   procedure Set_Initial_Level
     (The : in out Object;
      To  :        Positive);
   procedure Set_Initial_Lines
     (The : in out Object;
      To  :        Natural);
   procedure Set_Has_Ghost
     (The : in out Object;
      To  :        Boolean);
   function Get_Has_Ghost
     (The : in Object)
      return Boolean;
   function Get_Position_Color
     (The : Object;
      X   : Positive;
      Y   : Positive)
      return Color;
   function Piece_X
     (The : Object;
      i   : Integer)
      return Integer;
   function Piece_Y
     (The : Object;
      i   : Integer)
      return Integer;
   function Ghost_X
     (The : Object;
      i   : Integer)
      return Integer;
   function Ghost_Y
     (The : Object;
      i   : Integer)
      return Integer;
   function Piece_Can_Move_Down
     (The : in Object)
      return Boolean;
private
   type Block_Color_Type is array (Positive range <>, Positive range <>) of Color;
   type Position is record
      X : Positive;
      Y : Positive;
   end record;
   type Piece_Shape_Type is (Piece_I, Piece_Q, Piece_L, Piece_J, Piece_T, Piece_S, Piece_Z);
   type Piece_Type is array (1 .. 4) of Position;
   type Object (Width : Positive; Height : Positive) is limited record
      Game_Width            : Positive         := Width;
      Game_Height           : Positive         := Height;
      Score                 : Natural          := 0;
      Bonus_Down            : Natural          := 0;
      Bonus_Completed       : Natural          := 0;
      Level                 : Positive         := 1;
      Initial_Level         : Positive         := 1;
      Initial_Lines         : Natural          := 0;
      Lines_Number          : Natural          := 0;
      Lines_Completed       : Natural          := 0;
      Lowest_Line_Completed : Natural          := 1;
      Completed_Lines_List  : Lines_Indication;
      Level_Has_Changed     : Boolean          := False;
      Pieces_Number         : Natural          := 0;
      Piece_Color           : Color            := Blue;
      Piece                 : Piece_Type;
      Ghost                 : Piece_Type;
      Next_Move             : Move             := None;
      Auto_Down             : Boolean          := False;
      Game_Is_Over          : Boolean          := False;
      Has_Ghost             : Boolean          := False;
      Piece_Shape           : Piece_Shape_Type := Piece_I;
      Next_Piece_Shape      : Piece_Shape_Type := Piece_I;
      Random_Piece_Color    : Boolean          := False;
      Block_Color           : Block_Color_Type (1 .. Width, 1 .. Height);
   end record;
   procedure Put_Initial_Lines (The : in out Object);
   procedure Move_Right (The : in out Object);
   procedure Move_Left (The : in out Object);
   procedure Move_Down
     (The     : in out Object;
      Success :    out Boolean);
   procedure Drop (The : in out Object);
   procedure Rotate (the : in out Object);
   procedure Check_Completed_Lines (The : in out Object);
   procedure Put_Piece
     (The     : in out Object;
      Success :    out Boolean);
   function Shape_To_Color
     (Shape : Piece_Shape_Type)
      return Color;
   procedure Set_Ghost (The : in out Object);
end Block_Engine;
