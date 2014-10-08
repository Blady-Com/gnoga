------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                  G N O G A . G U I . E L E M E N T . T A B               --
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

with Ada.Strings.Unbounded;

with Gnoga.Gui.View;
with Gnoga.Gui.View.Card;

package Gnoga.Gui.Element.Tab is
   -------------------------------------------------------------------------
   --  Tab_Types
   -------------------------------------------------------------------------

   type Tab_Type is new Gnoga.Gui.View.View_Type with private;
   type Tab_Access is access all Tab_Type;
   type Pointer_To_Tab_Class is
     access all Tab_Type'Class;

   -------------------------------------------------------------------------
   --  Tab_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Tab          : in out Tab_Type;
      Parent       : in out Gnoga.Gui.Base.Base_Type'Class;
      Card_View    : in out Gnoga.Gui.View.Card.Card_View_Type'Class;
      Text_Color   : in     Gnoga.Types.RGBA_Type := (255, 255, 255, 1.0);
      Tab_Color    : in     Gnoga.Types.RGBA_Type := (0, 0, 0, 1.0);
      Select_Color : in     Gnoga.Types.RGBA_Type := (128, 128, 128, 1.0);
      Attach       : in     Boolean               := True;
      ID           : in     String                := "");

   -------------------------------------------------------------------------
   --  Tab_Item_Types
   -------------------------------------------------------------------------

   type Tab_Item_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Tab_Item_Access is access all Tab_Item_Type;
   type Pointer_To_Tab_Item_Class is access all Tab_Item_Type'Class;

   -------------------------------------------------------------------------
   --  Tab_Item_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Item   : in out Tab_Item_Type;
                     Parent : in out Tab_Type'Class;
                     Card   : in     String;
                     Text   : in     String := "";
                     ID     : in     String := "");
   --  Create a Tab Item with Text that when clicked will show Card on
   --  on associated Card View
   --
   --  If the On_Click handler is set for Item Tab_Select must be called
   --  from with in the handler for default card open behavior to take
   --  place.

   -------------------------------------------------------------------------
   --  Tab_Item_Type - Properties
   -------------------------------------------------------------------------

   function Tab_Selected (Item : Tab_Item_Type) return Boolean;

   -------------------------------------------------------------------------
   --  Tab_Item_Type - Methods
   -------------------------------------------------------------------------

   procedure Tab_Select (Item  : in out Tab_Item_Type);

private
   type Tab_Type is new Gnoga.Gui.View.View_Type with
      record
         Card_View    : Gnoga.Gui.View.Card.Pointer_To_Card_View_Class := null;
         Select_Color : Gnoga.Types.RGBA_Type;
         Tab_Color    : Gnoga.Types.RGBA_Type;
      end record;

   type Tab_Item_Type is new Gnoga.Gui.Element.Element_Type with
      record
         Card_Name    : Ada.Strings.Unbounded.Unbounded_String;
      end record;
end Gnoga.Gui.Element.Tab;
