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

--  with Gtk.Window; use Gtk.Window;
--  with Gtk.Button; use Gtk.Button;
with Gnoga_Extras;
with Gnoga.Gui.Element.Common;

package About_Dialog_Pkg is

   type About_Dialog_Record is new Gnoga_Extras.View_Type with record
      OK_Button    : Gnoga.Gui.Element.Common.Button_Type;
      Pause_Status : Boolean;
   end record;

   type About_Dialog_Type is access all About_Dialog_Record'Class;

   About_Dialog : About_Dialog_Type;

   procedure Gtk_New (Win : out About_Dialog_Type);
   procedure Initialize (Win : access About_Dialog_Record'Class);

end About_Dialog_Pkg;
