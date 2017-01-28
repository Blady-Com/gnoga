------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . G U I . E L E M E N T . T A B L E             --
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

package Gnoga.Gui.Element.Table is
   -------------------------------------------------------------------------
   --  Table_Types
   -------------------------------------------------------------------------

   type Table_Type is new Gnoga.Gui.View.View_Type with private;
   type Table_Access is access all Table_Type;
   type Pointer_To_Table_Class is access all Table_Type'Class;

   -------------------------------------------------------------------------
   --  Table_Types - Creation Methods
   -------------------------------------------------------------------------

   overriding
   procedure Create
     (Table  : in out Table_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String  := "");

   -------------------------------------------------------------------------
   --  Table_Types - Properties
   -------------------------------------------------------------------------

   function Caption (Table : Table_Type) return String;

   -------------------------------------------------------------------------
   --  Table_Types - Methods
   -------------------------------------------------------------------------

   procedure Add_Caption (Table : in out Table_Type; Value : in String);

   -------------------------------------------------------------------------
   --  Table_Row_Types
   -------------------------------------------------------------------------

   type Table_Row_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Table_Row_Access is access all Table_Row_Type;
   type Pointer_To_Table_Row_Class is access all Table_Row_Type'Class;

   -------------------------------------------------------------------------
   --  Table_Row_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Row    : in out Table_Row_Type;
      Parent : in out Element_Type'Class;
      ID     : in     String  := "");
   --  Use initial Content. Can be replaced or changed later with the Text or
   --  Inner_HTML property.
   --  Parent can be Table_Type, Table_Header_Type, Table_Body_Type or
   --  Table_Footer type if not Constraint_Error will be raised.

   -------------------------------------------------------------------------
   --  Table_Column_Types
   -------------------------------------------------------------------------

   type Table_Column_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Table_Column_Access is access all Table_Column_Type;
   type Pointer_To_Table_Column_Class is access all Table_Column_Type'Class;

   -------------------------------------------------------------------------
   --  Table_Column_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Column      : in out Table_Column_Type;
      Row         : in out Table_Row_Type'Class;
      Content     : in     String   := "";
      Column_Span : in     Positive := 1;
      Row_Span    : in     Positive := 1;
      ID          : in     String   := "");
   --  Use initial Content. Can be replaced or changed later with the Text or
   --  Inner_HTML property.

   -------------------------------------------------------------------------
   --  Table_Heading_Types
   -------------------------------------------------------------------------

   type Table_Heading_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Table_Heading_Access is access all Table_Heading_Type;
   type Pointer_To_Table_Heading_Class is access all Table_Heading_Type'Class;

   -------------------------------------------------------------------------
   --  Table_Heading_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Heading     : in out Table_Heading_Type;
      Row         : in out Table_Row_Type'Class;
      Content     : in     String  := "";
      Column_Span : in     Positive := 1;
      Row_Span    : in     Positive := 1;
      ID          : in     String  := "");
   --  Use initial Content. Can be replaced or changed later with the Text or
   --  Inner_HTML property.

   -------------------------------------------------------------------------
   --  Table_Header_Types
   -------------------------------------------------------------------------

   type Table_Header_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Table_Header_Access is access all Table_Header_Type;
   type Pointer_To_Table_Header_Class is access all Table_Header_Type'Class;

   -------------------------------------------------------------------------
   --  Table_Header_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Header  : in out Table_Header_Type;
      Table   : in out Table_Type'Class;
      ID      : in     String  := "");
   --  Create an optional table header section <thead>

   -------------------------------------------------------------------------
   --  Table_Body_Types
   -------------------------------------------------------------------------

   type Table_Body_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Table_Body_Access is access all Table_Body_Type;
   type Pointer_To_Table_Body_Class is access all Table_Body_Type'Class;

   -------------------------------------------------------------------------
   --  Table_Body_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (TBody : in out Table_Body_Type;
      Table : in out Table_Type'Class;
      ID    : in     String  := "");
   --  Create an optional table body section <tbody>

   -------------------------------------------------------------------------
   --  Table_Footer_Types
   -------------------------------------------------------------------------

   type Table_Footer_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Table_Footer_Access is access all Table_Footer_Type;
   type Pointer_To_Table_Footer_Class is access all Table_Footer_Type'Class;

   -------------------------------------------------------------------------
   --  Table_Footer_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Footer : in out Table_Footer_Type;
      Table  : in out Table_Type'Class;
      ID     : in     String  := "");
   --  Create an optional table footer section <tfoot>

   -------------------------------------------------------------------------
   --  Column_Group_Types
   -------------------------------------------------------------------------

   type Column_Group_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Column_Group_Access is access all Column_Group_Type;
   type Pointer_To_Column_Group_Class is access all Column_Group_Type'Class;

   -------------------------------------------------------------------------
   --  Column_Group_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Group : in out Column_Group_Type;
      Table : in out Table_Type'Class;
      ID    : in     String  := "");
   --  Create an optional column group <colgroup> to ease formatting
   --  Must be created or placed at top of table_type.

   -------------------------------------------------------------------------
   --  Column_Types
   -------------------------------------------------------------------------

   type Column_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Column_Access is access all Column_Type;
   type Pointer_To_Column_Class is access all Column_Type'Class;

   -------------------------------------------------------------------------
   --  Column_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Column      : in out Column_Type;
      Group       : in out Column_Group_Type'Class;
      Content     : in     String   := "";
      Column_Span : in     Positive := 1;
      ID          : in     String   := "");
   --  Use initial Content. Can be replaced or changed later with the Text or
   --  Inner_HTML property.

private
   type Table_Type is new Gnoga.Gui.View.View_Type with null record;
   type Table_Row_Type is new Gnoga.Gui.View.View_Base_Type with null record;
   type Table_Column_Type is
     new Gnoga.Gui.View.View_Base_Type with null record;
   type Table_Heading_Type is
     new Gnoga.Gui.View.View_Base_Type with null record;
   type Table_Header_Type is
     new Gnoga.Gui.View.View_Base_Type with null record;
   type Table_Body_Type is new Gnoga.Gui.View.View_Base_Type with null record;
   type Table_Footer_Type is
     new Gnoga.Gui.View.View_Base_Type with null record;
   type Column_Group_Type is
     new Gnoga.Gui.View.View_Base_Type with null record;
   type Column_Type is new Gnoga.Gui.Element.Element_Type with null record;
end Gnoga.Gui.Element.Table;
