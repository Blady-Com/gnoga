------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--              G N O G A . G U I . E L E M E N T . P H R A S E             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
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
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Gnoga.Gui.View;

package Gnoga.Gui.Element.Phrase is
   --  Phrase_Type is a View that describes an inline phrase section.
   --  Most of the time it is better to use these with in a Common.Span_Type
   --  or to style as desired.

   -------------------------------------------------------------------------
   --  Phrase_Types
   -------------------------------------------------------------------------

   type Phrase_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Phrase_Access is access all Phrase_Type;
   type Pointer_To_Phrase_Class is access all Phrase_Type'Class;

   -------------------------------------------------------------------------
   --  Phrase_Type - Creation Methods
   -------------------------------------------------------------------------

   type Phrase_Description_Type is
     (Abbr, Code, Strong, Em, Dfn, Samp, Kbd, Var, Marked, Del, Ins, S, Q,
      Big, Small, Time, Tt, Cite, I, B, U, Sub, Sup);

   procedure Create
     (View   : in out Phrase_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      Phrase : in     Phrase_Description_Type;
      Attach : in     Boolean := True;
      ID     : in     String  := "");

private
   type Phrase_Type is new Gnoga.Gui.View.View_Base_Type with null record;
end Gnoga.Gui.Element.Phrase;
