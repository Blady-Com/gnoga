------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                 G N O G A . E L E M E N T . C O M M O N                  --
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
------------------------------------------------------------------------------                                                                          --

package Gnoga.Element.Common is

   -------------------------------------------------------------------------
   --  A_Types
   -------------------------------------------------------------------------

   type A_Type is new Gnoga.Element.Element_Type with private;
   type A_Access is access all A_Type;
   type Pointer_To_A_Class is access all A_Type'Class;

   -------------------------------------------------------------------------
   --  A_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (A       : in out A_Type;
                     Parent  : in out Gnoga.Base.Base_Type'Class;
                     Link    : in     String := "";
                     Content : in     String := "";
                     Target  : in     String := "_self";
                     ID      : in     String := "");
   --  Create an Anchor link

   -------------------------------------------------------------------------
   --  A_Type - Properties
   -------------------------------------------------------------------------

   procedure Link (A : in out A_Type; Value : String);
   function Link (A : A_Type) return String;
   --  The HREF link of the Anchor

   procedure Target (A : in out A_Type; Value : String);
   function Target (A : A_Type) return String;
   --  Target of link, name of a frame or:
   --  _blank  = new window
   --  _top    = top most frame (full browser window)
   --  _parent = parent frame or window
   --  _self   = current frame or window

   -------------------------------------------------------------------------
   --  DIV_Types
   -------------------------------------------------------------------------

   type DIV_Type is new Gnoga.Element.Element_Type with private;
   type DIV_Access is access all DIV_Type;
   type Pointer_To_DIV_Class is access all DIV_Type'Class;

   -------------------------------------------------------------------------
   --  DIV_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (DIV     : in out DIV_Type;
                     Parent  : in out Gnoga.Base.Base_Type'Class;
                     Content : in     String := "";
                     ID      : in     String := "");
   --  Create a div container with optional HTML content

   -------------------------------------------------------------------------
   --  IMG_Types
   -------------------------------------------------------------------------

   type IMG_Type is new Gnoga.Element.Element_Type with private;
   type IMG_Access is access all IMG_Type;
   type Pointer_To_IMG_Class is access all IMG_Type'Class;

   -------------------------------------------------------------------------
   --  IMG_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (IMG              : in out IMG_Type;
                     Parent           : in out Gnoga.Base.Base_Type'Class;
                     URL_Source       : in     String := "";
                     Alternative_Text : in     String := "";
                     ID               : in     String := "");
   --  Create an image element. Use width and height properties before
   --  placing image to constrain image size.

   -------------------------------------------------------------------------
   --  HR_Types
   -------------------------------------------------------------------------

   type HR_Type is new Gnoga.Element.Element_Type with private;
   type HR_Access is access all HR_Type;
   type Pointer_To_HR_Class is access all HR_Type'Class;

   -------------------------------------------------------------------------
   --  HR_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (HR     : in out HR_Type;
                     Parent : in out Gnoga.Base.Base_Type'Class;
                     ID     : in     String := "");
   --  Create a horizontal rule

   -------------------------------------------------------------------------
   --  Span_Types
   -------------------------------------------------------------------------

   type Span_Type is new Gnoga.Element.Element_Type with private;
   type Span_Access is access all Span_Type;
   type Pointer_To_Span_Class is access all Span_Type'Class;

   -------------------------------------------------------------------------
   --  Span_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Span    : in out Span_Type;
                     Parent  : in out Gnoga.Base.Base_Type'Class;
                     Content : in     String := "";
                     ID      : in     String := "");
   --  Create a Span container

private
   type A_Type is new Gnoga.Element.Element_Type with null record;
   type DIV_Type is new Gnoga.Element.Element_Type with null record;
   type IMG_Type is new Gnoga.Element.Element_Type with null record;
   type HR_Type is new Gnoga.Element.Element_Type with null record;
   type Span_Type is new Gnoga.Element.Element_Type with null record;
end Gnoga.Element.Common;
