------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                       G N O G A . E L E M E N T                          --
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

with Ada.Finalization;

with Gnoga.Types;
with Gnoga.Base;

package Gnoga.Element is

   -------------------------------------------------------------------------
   --  Element_Type
   -------------------------------------------------------------------------
   --  Element_Type is the parent class of all Gnoga GUI elements.
   --  It is generally used internally to create and bind Gnoga elements to
   --  HTML5 DOM elements.

   type Element_Type is new Gnoga.Base.Base_Type with private;
   type Element_Access is access all Element_Type;
   type Pointer_To_Element_Class is access all Element_Type'Class;

   -------------------------------------------------------------------------
   --  Element_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create_From_HTML (Element : in out Element_Type;
                               Parent  : in out Gnoga.Base.Base_Type'Class;
                               HTML    : in     String;
                               ID      : in     String := "");
   --  Create a Gnoga element with HTML not attached to the DOM. If ID is blank
   --  Gnoga will generate a unique one for it. The created object will be
   --  stored on the browser but will not be inserted in to the DOM until
   --  Place_Inside_Top_Of, Place_Inside_Botton_Of, Place_Before, Place_ After

   -------------------------------------------------------------------------
   --  Element_Type - Properties
   -------------------------------------------------------------------------

   --  Element Properties --

   procedure Access_Key (Element : in out Element_Type; Value : String);
   function Access_Key (Element : Element_Type) return String;
   --  Used for hot key access to element. [special key] + Access_Key
   --  The [special key] per browser and platform is:
   --
   --  Browser              Windows       Linux           Mac
   --  -----------------    -------       -----           ---
   --  Internet Explorer     [Alt]         N/A            N/A
   --  Chrome                [Alt]        [Alt]     [Control][Alt]
   --  Firefox 	         [Alt][Shift] [Alt][Shift]  [Control][Alt]
   --  Safari                [Alt]         N/A      [Control][Alt]
   --  Opera 15+             [Alt]        [Alt]          [Alt]

   procedure Advisory_Title (Element : in out Element_Type; Value : String);
   function Advisory_Title (Element : Element_Type) return String;
   --  Advisory_Title of Element, usually used for body and image maps

   procedure Class_Name (Element : in out Element_Type; Value : String);
   function Class_Name (Element : Element_Type) return String;
   --  CSS Class name, can be multiple seperated by <space>

   procedure Editable (Element : in out Element_Type; Value : Boolean := True);
   function Editable (Element : Element_Type) return Boolean;

   procedure Draggable (Element : in out Element_Type; Value : Boolean := True);
   function Draggable (Element : Element_Type) return Boolean;

   procedure Hidden (Element : in out Element_Type; Value : Boolean := True);
   function Hidden (Element : Element_Type) return Boolean;
   --  The hidden property is practically speaking like Visible, however
   --  visible uses CSS to hide the Element. Hidden implies the element is
   --  semantically not relevant not just visually.

   procedure Inner_HTML (Element : in out Element_Type; Value : in String);
   function Inner_HTML (Element : Element_Type) return String;
   --  This will completely replace the inner html of an element. This
   --  will require all Gnoga elements to be Placed again in the DOM to
   --  display again.

   procedure Language_Code (Element : in out Element_Type; Value : String);
   function Language_Code (Element : Element_Type) return String;

   procedure Tab_Index (Element : in out Element_Type; Value : Natural);
   function Tab_Index (Element : Element_Type) return Natural;

   procedure Spell_Check (Element : in out Element_Type; Value : Boolean := True);
   function Spell_Check (Element : Element_Type) return Boolean;
   --  If true Element is subject to browser spell checking if Editable is
   --  also true.

   procedure Text (Element : in out Element_Type; Value : in String);
   function Text (Element : Element_Type) return String;
   -- Text content of element.

   procedure Text_Direction (Element : in out Element_Type; Value : String);
   function Text_Direction (Element : Element_Type) return String;
   --  BiDi text direction

   procedure Visible (Element : in out Element_Type; Value : Boolean := True);
   function Visible (Element : Element_Type) return Boolean;


   --  Properties curently not being supported:
   --  contentmenu - property is currently only supported in FireFox
   --  data-* - may be considered in future versions
   --  dropzone - no browser support
   --  translate - no browser support

   -- Location Properties --

   --  For reference:
   --  | Margin | Border | Padding | Scroll | [Element] | Scroll | Padding ...

   function Client_Width (Element : Element_Type) return Natural;
   --  Inner width of an element in pixels.
   --  CSS width + CSS padding - width of vertical scrollbar (if present)
   --  Does not include the border or margin.

   function Client_Height (Element : Element_Type) return Natural;
   --  Inner height of an element in pixels.
   --  CSS height + CSS padding - height of horizontal scrollbar (if present)
   --  Does not include the border or margin.

   function Client_Left (Element : Element_Type) return Natural;
   --  The width of the left border of an element in pixels.
   --. It does not include the margin or padding.

   function Client_Top (Element : Element_Type) return Natural;
   --  The width of the top border of an element in pixels.
   --. It does not include the margin or padding.

   function Offset_Width (Element : Element_Type) return Natural;
   --  CSS width + CSS padding + width of vertical scrollbar (if present) +
   --  Border

   function Offset_Height (Element : Element_Type) return Natural;
   --  CSS height + CSS padding + height of horizontal scrollbar (if present) +
   --  Border

   function Offset_Left (Element : Element_Type) return Natural;
   --  The width from parent element border to child border left

   function Offset_Top (Element : Element_Type) return Natural;
   --  The width from parent element border to child border top

   function Scroll_Width (Element : Element_Type) return Natural;
   --  Either the width in pixels of the content of an element or the width of
   --  the element itself, whichever is greater

   function Scroll_Height (Element : Element_Type) return Natural;
   --  Eeight of an element's content, including content not visible on the
   --  screen due to overflow.

   procedure Scroll_Left (Element : in out Element_Type; Value : Integer);
   function Scroll_Left (Element : Element_Type) return Integer;
   --  The number of pixels that an element's content is scrolled to the left.
   --  For RTL languages is negative.

   procedure Scroll_Top (Element : in out Element_Type; Value : Integer);
   function Scroll_Top (Element : Element_Type) return Integer;
   --  The number of pixels thant an element's content has been scrolled upward.

   --  General Access to Element --

   procedure Style (Element : in out Element_Type;
                    Name    : in String;
                    Value   : in String);
   function Style (Element : Element_Type; Name : String) return String;
   --  General access to style Name

   procedure Attribute (Element : in out Element_Type;
                        Name    : in String;
                        Value   : in String);
   function Attribute (Element : Element_Type; Name : String) return String;
   --  General access to attribute Name

   -- Traversal Properties --

   procedure First_Child (Element : in out Element_Type;
                          Child  : in out Element_Type'Class);
   --  If Child does not have an html id than Element_Type will have an
   --  ID of undefined and therefore attached to no actual HTML element.

   procedure Next_Sibling (Element : in out Element_Type;
                           Sibling : in out Element_Type'Class);
   --  If Sibling does not have an html id than Element_Type will have an
   --  ID of undefined and therefore attached to no actual HTML element.

   -- Internal Properties --

   function HTML_Tag (Element : Element_Type) return String;

   -------------------------------------------------------------------------
   --  Element_Type - Methods
   -------------------------------------------------------------------------

   -- DOM Placement Methods --

   procedure Place_Inside_Top_Of (Element : in out Element_Type;
                                  Target  : in out Element_Type'Class);

   procedure Place_Inside_Bottom_Of (Element : in out Element_Type;
                                     Target  : in out Element_Type'Class);

   procedure Place_Before (Element : in out Element_Type;
                           Target  : in out Element_Type'Class);

   procedure Place_After (Element : in out Element_Type;
                          Target  : in out Element_Type'Class);


   -- Element Methods --

   procedure Click (Element : in out Element_Type);
   --  Simulate click on element

   -------------------------------------------------------------------------
   --  Element_Type - Event Handlers
   -------------------------------------------------------------------------

   -- transionend

private
   type Element_Type is new Gnoga.Base.Base_Type with
      record
         null;
      end record;
end Gnoga.Element;
