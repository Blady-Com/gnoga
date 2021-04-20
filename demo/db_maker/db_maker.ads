-- A generic for creating simple DBs (one table in an RDBMS) with PragmARC.Persistent_Skip_List_Unbounded and a Gnoga UI.
--
-- Copyright (C) 2017 by Jeffrey R. Carter
--
generic -- DB_Maker
   Max_Field_Length : Positive;
   File_Name : String; -- ".psl" will be appended

   type Field_Number is range <>;

   with function Field_Name
     (Field : in Field_Number)
      return String;

   type Element is private; -- Cannot contain access values

   with function Value
     (Item  : in Element;
      Field : in Field_Number)
      return String;
   with procedure Put
     (Item  : in out Element;
      Field : in     Field_Number;
      Value : in     String);
   with function "<"
     (Left  : in Element;
      Right : in Element)
      return Boolean is <>;
   with function "="
     (Left  : in Element;
      Right : in Element)
      return Boolean is <>;
package DB_Maker is
   pragma Elaborate_Body;
end DB_Maker;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
