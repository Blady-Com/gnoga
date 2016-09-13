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
--| Filename         : $Source: /othello_pkg.ads,v $
--| Author           : Adrian Hoe (byhoe)
--| Created On       : 2001/11/15
--| Last Modified By : $Author: JHB $
--| Last Modified On : $Date: 2016/03/06 09:18:25 $
--| Status           : $State: Exp $
--|
--------------------------------------------------------------------------------

with Othello_Types;

package Othello_Pkg is

   procedure Initialize;

private

   procedure New_Game
      (Othello       : in out Othello_Types.Othello_Record'Class;
       Playing_Board : in out Othello_Types.Board_Matrix;
       Whose_Move    : in out Othello_Types.Bead_Color;
       Game_Over     : in out Boolean);

   procedure Pass
      (Othello       : in out Othello_Types.Othello_Record'Class;
       Playing_Board : in out Othello_Types.Board_Matrix;
       Whose_Move    : in out Othello_Types.Bead_Color;
       Game_Over     : in out Boolean);

   procedure Make_Move
      (Othello       : in out Othello_Types.Othello_Record'Class;
       Playing_Board : in out Othello_Types.Board_Matrix;
       Whose_Move    : in out Othello_Types.Bead_Color;
       Game_Over     : in out Boolean;
       Row           : in     Othello_Types.Valid_Row;
       Column        : in     Othello_Types.Valid_Column);

end Othello_Pkg;
