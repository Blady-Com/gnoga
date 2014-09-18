------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                 G N O G A . E L E M E N T . C A N V A S                  --
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

with Gnoga.Types;

package Gnoga.Element.Canvas is
   -------------------------------------------------------------------------
   --  Canvas_Types
   -------------------------------------------------------------------------

   type Canvas_Type is new Gnoga.Element.Element_Type with private;
   type Canvas_Access is access all Canvas_Type;
   type Pointer_To_Canvas_Class is access all Canvas_Type'Class;

   -------------------------------------------------------------------------
   --  Canvas_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Canvas  : in out Canvas_Type;
                     Parent  : in out Gnoga.Base.Base_Type'Class;
                     Width   : in     Integer;
                     Height  : in     Integer;
                     ID      : in     String := "");
   --  Create a Canvas container

   type Context_2D_Type is
     new Ada.Finalization.Limited_Controlled with private;
   type Context_2D_Access is access all Context_2D_Type;
   type Pointer_To_Context_2D_Class is access all Context_2D_Type'Class;

   -------------------------------------------------------------------------
   --  Canvas_Type - Methods
   -------------------------------------------------------------------------

   procedure Get_Drawing_Context_2D (Canvas  : in out Canvas_Type;
                                     Context : in out Context_2D_Type'Class);
   --  Get a two dimensional drawing Context from Canvas

   -------------------------------------------------------------------------
   --  Context_2D_Type - Properties
   -------------------------------------------------------------------------




   --  Generic Properties --

   procedure Property (Context : in out Context_2D_Type;
                       Name    : in     String;
                       Value   : in     String);
   function Property (Context : Context_2D_Type; Name : String) return String;

   -------------------------------------------------------------------------
   --  Context_2D_Type - Methods
   -------------------------------------------------------------------------

   --  Generic Methods --

   procedure Execute (Context : in out Context_2D_Type; Method : String);
   function Execute (Context : Context_2D_Type; Method : String)
                     return String;

   --  Internal Methods --

   function Connection_ID (Context : Context_2D_Type)
                           return Gnoga.Types.Connection_ID;

private
   type Canvas_Type is new Gnoga.Element.Element_Type with null record;

   type Context_2D_Type is
     new Ada.Finalization.Limited_Controlled with
      record
         Connection_ID : Gnoga.Types.Connection_ID := Gnoga.Types.No_Connection;
         Context_ID    : Gnoga.Types.Web_ID;
      end record;
end Gnoga.Element.Canvas;
