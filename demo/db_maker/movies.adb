-- Demo for DB_Maker: Catalog your extensive collection of BetaMax videotape cassettes!
--
-- Copyright (C) 2017 by Jeffrey R. Carter
--
with DB_Maker;
with PragmARC.B_Strings;

procedure Movies is
   subtype Strng is PragmARC.B_Strings.B_String (Max_Length => 100);
   use type Strng;

   type Movie_Info is record
      Title       : Strng;
      Year        : Strng;
      Director    : Strng;
      Writer      : Strng;
      Male_Lead   : Strng;
      Female_Lead : Strng;
   end record;

   function "<" (Left : Movie_Info; Right : Movie_Info) return Boolean is
      -- Empty
   begin -- "<"
      if Left.Title /= Right.Title then
         return Left.Title < Right.Title;
      end if;

      if Left.Year /= Right.Year then
         return Left.Year < Right.Year;
      end if;

      return Left.Director < Right.Director;
   end "<";

   function "=" (Left : Movie_Info; Right : Movie_Info) return Boolean is
      (Left.Title = Right.Title and Left.Year = Right.Year and Left.Director = Right.Director);

   subtype Field_Number is Integer range 1 .. 6;

   function Field_Name (Field : in Field_Number) return String is
      -- Empty
   begin -- Field_Name
      case Field is
      when 1 =>
         return "Title";
      when 2 =>
         return "Year";
      when 3 =>
         return "Director";
      when 4 =>
         return "Screenplay";
      when 5 =>
         return "Male Lead";
      when 6 =>
         return "Female Lead";
      end case;
   end Field_Name;

   function Value (Item : in Movie_Info; Field : in Field_Number) return String is
      -- Empty
   begin -- Value
      case Field is
      when 1 =>
         return +Item.Title;
      when 2 =>
         return +Item.Year;
      when 3 =>
         return +Item.Director;
      when 4 =>
         return +Item.Writer;
      when 5 =>
         return +Item.Male_Lead;
      when 6 =>
         return +Item.Female_Lead;
      end case;
   end Value;

   procedure Put (Item : in out Movie_Info; Field : in Field_Number; Value : in String) is
      -- Empty
   begin -- Put
      case Field is
      when 1 =>
         Item.Title.Assign (From => Value);
      when 2 =>
         Item.Year.Assign (From => Value);
      when 3 =>
         Item.Director.Assign (From => Value);
      when 4 =>
         Item.Writer.Assign (From => Value);
      when 5 =>
         Item.Male_Lead.Assign (From => Value);
      when 6 =>
         Item.Female_Lead.Assign (From => Value);
      end case;
   end Put;

   package Movie_DB is new DB_Maker (Max_Field_Length => 100,
                                     File_Name        => "Movies",
                                     Field_Number     => Field_Number,
                                     Field_Name       => Field_Name,
                                     Element          => Movie_Info,
                                     Value            => Value,
                                     Put              => Put);
begin -- Movies
   null;
end Movies;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
