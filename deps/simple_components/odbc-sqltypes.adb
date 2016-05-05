--                                                                    --
--  package ODBC.SQLTypes           Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2012       --
--                                                                    --
--                                Last revision :  10:00 09 Apr 2016  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Interfaces;  use Interfaces;

package body ODBC.SQLTypes is

   FRACTION_Span : constant Long_Float :=
                            Long_Float (SQL_TIME_FRACTION'Last) + 1.0;

   function "<" (Left, Right : SQLGUID) return Boolean is
   begin
      if Left.Data1 /= Right.Data1 then
         return Left.Data1 < Right.Data1;
      end if;
      if Left.Data2 /= Right.Data2 then
         return Left.Data2 < Right.Data2;
      end if;
      if Left.Data3 /= Right.Data3 then
         return Left.Data3 < Right.Data3;
      end if;
      for Index in SQLGUID_Octet'Range loop
         if Left.Data4 (Index) /= Right.Data4 (Index) then
            return Left.Data4 (Index) < Right.Data4 (Index);
         end if;
      end loop;
      return False;
   end "<";

   function "<=" (Left, Right : SQLGUID) return Boolean is
   begin
      if Left.Data1 /= Right.Data1 then
         return Left.Data1 < Right.Data1;
      end if;
      if Left.Data2 /= Right.Data2 then
         return Left.Data2 < Right.Data2;
      end if;
      if Left.Data3 /= Right.Data3 then
         return Left.Data3 < Right.Data3;
      end if;
      for Index in SQLGUID_Octet'Range loop
         if Left.Data4 (Index) /= Right.Data4 (Index) then
            return Left.Data4 (Index) < Right.Data4 (Index);
         end if;
      end loop;
      return True;
   end "<=";

   function ">" (Left, Right : SQLGUID) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">=" (Left, Right : SQLGUID) return Boolean is
   begin
      return Right <= Left;
   end ">=";

   function From_Time (Value : Time) return SQL_TIMESTAMP_STRUCT is
      Year     : Year_Number;
      Month    : Month_Number;
      Day      : Day_Number;
      Seconds  : Day_Duration;
      Result   : SQL_TIMESTAMP_STRUCT;
   begin
      Split (Value, Year, Month, Day, Seconds);
      Result.Year  := SQLSMALLINT (Year);
      Result.Month := SQLUSMALLINT (Month);
      Result.Day   := SQLUSMALLINT (Day);
      declare
         D : constant Long_Float  := Long_Float (Seconds);
         S : constant Long_Float  := Long_Float'Truncation (D);
         I : constant SQLUINTEGER := SQLUINTEGER (S);
      begin
         Result.Fraction := SQL_TIME_FRACTION ((D - S) * FRACTION_Span);
         Result.Hour     := SQLUSMALLINT (I / 3600);
         Result.Minute   := SQLUSMALLINT ((I / 60) mod 60);
         Result.Second   := SQLUSMALLINT (I mod 60);
      end;
      return Result;
   end From_Time;

   function To_Time (Value : SQL_TIMESTAMP_STRUCT) return Time is
   begin
      return
         Time_Of
         (  Year    => Year_Number  (Value.Year),
            Month   => Month_Number (Value.Month),
            Day     => Day_Number   (Value.Day),
            Seconds => (  (  Duration (Value.Hour)
                          *  60
                          +  Duration (Value.Minute)
                          )
                       *  60
                       +  Duration (Value.Second)
                       +  Duration
                          (  Long_Float (Value.Fraction) / FRACTION_Span
         )             )  );
   end To_Time;

end ODBC.SQLTypes;
