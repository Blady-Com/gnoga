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
--| Filename         : $Source: /othello_pkg-callbacks.ads,v $
--| Author           : Adrian Hoe (byhoe)
--| Created On       : 2001/11/15
--| Last Modified By : $Author: JHB $
--| Last Modified On : $Date: 2016/03/06 09:18:25 $
--| Status           : $State: Exp $
--|
--------------------------------------------------------------------------------
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Application.Multi_Connect;

package Othello_Pkg.Callbacks is

   procedure On_File1_Activate
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_New_Game1_Activate
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Pass1_Activate
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Exit1_Activate
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Help1_Activate
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_About1_Activate
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Connect
      (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
       Connection  : access
          Gnoga.Application.Multi_Connect.Connection_Holder_Type);

end Othello_Pkg.Callbacks;
