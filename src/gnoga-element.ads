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
   --  Element_Type is the parent class of all Gnoga GUI Objects.
   --  It is generally used internally to create and bind Gnoga objects to
   --  HTML5 DOM element objects.

   type Element_Type is new Gnoga.Base.Base_Type with private;
   type Element_Access is access all Element_Type;
   type Pointer_To_Element_Class is access all Element_Type'Class;

   -------------------------------------------------------------------------
   --  Element_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create_Root (Object        : in out Element_Type;
                          Connection_ID : in     Gnoga.Types.Connection_ID;
                          ID            : in     String;
                          HTML          : in     String);
   --  Create a Gnoga oject on Connection ID with DOM ID using HTML in <body>
   --  Warning: This will destroy all DOM objects in body.

   procedure Create_Inside (Object        : in out Element_Type;
                            Parent        : in out Element_Type'Class;
                            ID            : in     String;
                            HTML          : in     String);
   --  Create a Gnoga object on with DOM ID using HTML inside Parent.
   --  Warning: This will destroy all DOM objects already in Parent.

   procedure Create_Inside_At_Top (Object        : in out Element_Type;
                                   Parent        : in out Element_Type'Class;
                                   ID            : in     String;
                                   HTML          : in     String);
   --  Create a Gnoga object on with DOM ID using HTML inside Parent at top
   --  of existing objects.

   procedure Create_Inside_At_Bottom (Object        : in out Element_Type;
                                      Parent        : in out Element_Type'Class;
                                      ID            : in     String;
                                      HTML          : in     String);
   --  Create a Gnoga object on with DOM ID using HTML inside Parent at bottom
   --  of existing objects.


   procedure Create_After (Object        : in out Element_Type;
                           Target        : in out Element_Type'Class;
                           ID            : in     String;
                           HTML          : in     String);
   --  Create a Gnoga object with DOM ID using HTML after Target.

   procedure Create_Before (Object        : in out Element_Type;
                            Target        : in out Element_Type'Class;
                            ID            : in     String;
                            HTML          : in     String);
   --  Create a Gnoga object with DOM ID using HTML before Target.

   -------------------------------------------------------------------------
   --  Element_Type - Properties
   -------------------------------------------------------------------------

   procedure Style (Object : in out Element_Type;
                    Name   : in String;
                    Value  : in String);
   function Style (Object : Element_Type; Name : String) return String;
   --  General access to style Name

   procedure Attribute (Object : in out Element_Type;
                        Name   : in String;
                        Value  : in String);
   function Attribute (Object : Element_Type; Name : String) return String;
   --  General access to attribute Name

   --  Object Properties --

   procedure Visible (Object : in out Element_Type; Value : Boolean := True);
   function Visible (Object : Element_Type) return Boolean;

   -------------------------------------------------------------------------
   --  Element_Type - Methods
   -------------------------------------------------------------------------

   -------------------------------------------------------------------------
   --  Element_Type - Event Handlers
   -------------------------------------------------------------------------
   -- When an event handler is set any event binding to the browser will be
   -- installed automatically.


   -------------------------------------------------------------------------
   --  Element_Type - Event Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed and internal functionality of the event is handled
   --  properly, always call the base class event method.
   --
   --  Event Methods are always bound on creation of Gnoga object or do not
   --  require event binding.


private
   type Element_Type is new Gnoga.Base.Base_Type with
      record
         null;
      end record;
end Gnoga.Element;
