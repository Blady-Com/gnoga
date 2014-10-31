------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--            G N O G A . G U I . E L E M E N T . S E C T I O N             --
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

package Gnoga.Gui.Element.Section is
   --  Section_Type is a View that describes a symantic section.
   --  Practically there is no difference with View_Type other than
   --  their underlying tag for styling purposes. Some sections
   --  have some default styling, e.g. as address being italic.

   -------------------------------------------------------------------------
   --  Section_Types
   -------------------------------------------------------------------------

   type Section_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Section_Access is access all Section_Type;
   type Pointer_To_Section_Class is access all Section_Type'Class;

   -------------------------------------------------------------------------
   --  Section_Type - Creation Methods
   -------------------------------------------------------------------------

   type Section_Description_Type is
     (Address, Article, Aside, Header, Main, Nav, P, Pre, Section,
      BlockQuote, H1, H2, H3, H4, H5, H6, HGroup);

   procedure Create
     (View    : in out Section_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Section : in     Section_Description_Type;
      Attach  : in     Boolean := True;
      ID      : in     String  := "");

private
   type Section_Type is new Gnoga.Gui.View.View_Base_Type with null record;
end Gnoga.Gui.Element.Section;
