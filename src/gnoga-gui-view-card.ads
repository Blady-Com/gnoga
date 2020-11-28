------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                   G N O G A . G U I . V I E W . C A R D                  --
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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

--  A card view allows for multiple "cards" (views) to be placed on a
--  virtual stack with only one shown and the others hidden at any given
--  time.

with Ada.Strings.Unbounded;
with Gnoga.Types.Colors;

package Gnoga.Gui.View.Card is

   -------------------------------------------------------------------------
   --  Card_View_Types
   -------------------------------------------------------------------------

   type Card_View_Type is new View_Type with private;
   type Card_View_Access is access all Card_View_Type;
   type Pointer_To_Card_View_Class is access all Card_View_Type'Class;

   -------------------------------------------------------------------------
   --  Card_View_Type - Creation Methods
   -------------------------------------------------------------------------

   overriding procedure Create
     (View   : in out Card_View_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String := "");

   -------------------------------------------------------------------------
   --  Card_View_Type - Properties
   -------------------------------------------------------------------------

   function Current_Card
     (View : Card_View_Type)
      return Pointer_To_View_Base_Class;
   --  The currently shown card, null if no card has been added and shown

   function Card
     (View : Card_View_Type;
      Name : String)
      return Pointer_To_View_Base_Class;
   --  The Card with Name.

   -------------------------------------------------------------------------
   --  Card_View_Type - Methods
   -------------------------------------------------------------------------

   procedure Add_Card
     (View : in out Card_View_Type;
      Name : in     String;
      Card :        access View_Base_Type'Class := null;
      Show : in     Boolean                     := False);
   --  Adds a new Card (a View_Type) called Name and if Show true Show Card
   --
   --  Internally the card named "current" always points to the currently
   --  displayed card so that name is reserved.

   procedure Show_Card
     (View : in out Card_View_Type;
      Name : in     String);
   --  Show card named Name
   --  If Name does not exist, the current card will be hidden and no card
   --  will be displayed.

   -------------------------------------------------------------------------
   --  Card_View_Type - Event Methods
   -------------------------------------------------------------------------

   overriding procedure On_Resize (View : in out Card_View_Type);
   --  Handle layout of children

   -------------------------------------------------------------------------
   --  Tab_Types - For use with providing cards with tabs
   -------------------------------------------------------------------------

   type Tab_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Tab_Access is access all Tab_Type;
   type Pointer_To_Tab_Class is access all Tab_Type'Class;

   -------------------------------------------------------------------------
   --  Tab_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Tab          : in out Tab_Type;
      Parent       : in out Gnoga.Gui.Base.Base_Type'Class;
      Card_View    : in out Card_View_Type'Class;
      Text_Color   : in     Gnoga.Types.Colors.Color_Enumeration := Gnoga.Types.Colors.White;
      Tab_Color    : in     Gnoga.Types.Colors.Color_Enumeration := Gnoga.Types.Colors.Black;
      Select_Color : in     Gnoga.Types.Colors.Color_Enumeration := Gnoga.Types.Colors.Gray;
      ID           : in     String                               := "");

   -------------------------------------------------------------------------
   --  Tab_Types - Methods
   -------------------------------------------------------------------------

   procedure Add_Tab
     (Tab      : in out Tab_Type;
      Card     : in     String;
      Label    : in     String;
      Selected : in     Boolean := False);
   --  Tabs can also be added using Tab_Item_Types
   --  Tab's have two properties, the Card they are associated with and
   --  their Label displayed on the tab. By default the Show_Card is called
   --  on the associated Card_View with Card when the tab is clicked.

   procedure Select_Tab
     (Tab  : in out Tab_Type;
      Card : in     String);
   --  Selects the Tab that is associated with Card

   -------------------------------------------------------------------------
   --  Tab_Item_Types
   -------------------------------------------------------------------------

   type Tab_Item_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Tab_Item_Access is access all Tab_Item_Type;
   type Pointer_To_Tab_Item_Class is access all Tab_Item_Type'Class;

   -------------------------------------------------------------------------
   --  Tab_Item_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Item   : in out Tab_Item_Type;
      Parent : in out Tab_Type'Class;
      Card   : in     String;
      Label  : in     String;
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

   function Tab_Selected
     (Item : Tab_Item_Type)
      return Boolean;

   -------------------------------------------------------------------------
   --  Tab_Item_Type - Methods
   -------------------------------------------------------------------------

   procedure Tab_Select (Item : in out Tab_Item_Type);
private
   type Card_View_Type is new View_Type with null record;

   type Tab_Type is new Gnoga.Gui.View.View_Base_Type with record
      Card_View    : Pointer_To_Card_View_Class := null;
      Select_Color : Gnoga.Types.RGBA_Type;
      Tab_Color    : Gnoga.Types.RGBA_Type;
   end record;

   type Tab_Item_Type is new Gnoga.Gui.Element.Element_Type with record
      Card_Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;
end Gnoga.Gui.View.Card;
