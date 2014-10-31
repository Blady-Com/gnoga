------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . G U I . E L E M E N T . S T Y L E             --
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

package Gnoga.Gui.Element.Style_Block is
   --  Style elements are for creating CSS styles. In general it is better
   --  to create CSS style sheets as external files and load with
   --  Window_Type.Document.Load_CSS or use element properties. However
   --  it is possible to generate CSS style sheets in Gnoga. They can be
   --  added like any element to views, windows, etc.

   -------------------------------------------------------------------------
   --  Style_Types
   -------------------------------------------------------------------------

   type Style_Type is new Element_Type with private;
   type Style_Access is access all Style_Type;
   type Pointer_To_Style_Class is access all Style_Type'Class;

   -------------------------------------------------------------------------
   --  Style_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Style   : in out Style_Type;
                     Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                     Content : in     String  := "";
                     ID      : in     String  := "");
   --  Create a style block with Content

   -------------------------------------------------------------------------
   --  Style_Types - Methods
   -------------------------------------------------------------------------

   procedure Add_Style (Style : in out Style_Type;
                        Text  : in     String);
   --  Appends Text to Style bock. Should be valid CSS:
   --  selector {property:value; ...}

   procedure Add_Style_for_Element (Style : in out Style_Type;
                                    Name  : in     String;
                                    Text  : in     String);
   --  Appends Name {Text} to Style Block

   procedure Add_Style_for_ID (Style : in out Style_Type;
                               ID    : in     String;
                               Text  : in     String);
   --  Appends a #ID {Text} to Style block

   procedure Add_Style_for_Class (Style : in out Style_Type;
                                  Name  : in     String;
                                  Text  : in     String);
   --  Appends a .Name {Text} to Style Block

private
   type Style_Type is new Element_Type with null record;
end Gnoga.Gui.Element.Style_Block;
