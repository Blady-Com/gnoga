------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                         G N O G A . T Y P E S                            --
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
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;

package Gnoga.Types is
   package Data_Arrays is
     new Ada.Containers.Indefinite_Vectors (Positive, String);
   subtype Data_Array_Type is Data_Arrays.Vector;

   package Data_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps (String,
                                                 String,
                                                 Ada.Strings.Hash,
                                                 Equivalent_Keys => "=");
   subtype Data_Map_Type is Data_Maps.Map;

   package Maps_of_Data_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (String,
                                                Data_Maps.Map,
                                                Ada.Strings.Hash,
                                                Equivalent_Keys => "=",
                                                "=" => Data_Maps."=");
   subtype Map_of_Data_Maps_Type is Maps_of_Data_Maps.Map;

   subtype Web_ID is Ada.Strings.Unbounded.Unbounded_String;

   type ID_Enumeration is (No_ID, DOM_ID, Script, Gnoga_ID);

   subtype Connection_ID is Integer;

   No_Connection : constant Connection_ID := -1;

   subtype Unique_ID is Integer;

   No_Unique_ID : constant Unique_ID := -1;

   type Connection_Data_Type is tagged limited null record;
   type Connection_Data_Access is access all Connection_Data_Type;
   type Pointer_to_Connection_Data_Class is
     access all Connection_Data_Type'Class;

   type Frational_Range_Type is delta 0.001 range 0.0 .. 1.0;

   subtype Alpha_Type is Frational_Range_Type;

   type Color_Type is range 0 .. 255;

   type RGBA_Type is
      record
         Red   : Color_Type := 0;
         Green : Color_Type := 0;
         Blue  : Color_Type := 0;
         Alpha : Alpha_Type := 1.0;
      end record;

   function To_String (RGBA : RGBA_Type) return String;
   --  Returns and rgba(r,g,b,a) representation of RGBA

   function To_RGBA (Value : String) return RGBA_Type;
   --  Will convert rgb(r,g,b) and rgba(r,g,b,a), or
   --  Hex color (include Hex with Alpha) to RGBA_Type

   type Pixel_Type is
      record
         Red   : Color_Type := 0;
         Green : Color_Type := 0;
         Blue  : Color_Type := 0;
         Alpha : Color_Type := 0;
      end record;

   function To_RGBA (Value : in Pixel_Type) return RGBA_Type;

   function To_Pixel (Value : in RGBA_Type) return Pixel_Type;

   type Pixel_Data_Type is
     array (Positive range <>, Positive range <>) of Pixel_Type;

   type Point_Type is
      record
         X, Y : Integer;
      end record;

   type Point_Array_Type is
     array (Positive range <>) of Point_Type;

   type Rectangle_Type is
      record
         X, Y, Width, Height : Integer;
      end record;

   type Size_Type is
      record
         Width, Height : Integer;
      end record;
end Gnoga.Types;
