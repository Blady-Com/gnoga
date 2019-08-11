--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_ISO_8601                               Luebeck            --
--  Test                                           Summer, 2019       --
--                                                                    --
--                                Last revision :  18:40 01 Aug 2019  --
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

with Ada.Calendar.Formatting;  use Ada.Calendar;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Text_IO;              use Ada.Text_IO;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.ISO_8601;    use Strings_Edit.ISO_8601;

procedure Test_ISO_8601 is
   procedure Test
             (  Text   : String;
                Value  : Duration;
                Length : Integer  := -1;
                Eps    : Duration := 0.01
             )  is
      Pointer : Integer := Text'First;
      Next    : Integer;
      Result  : Duration;
   begin
      if Length = -1 then
         Next := Text'Last + 1;
      else
         Next := Pointer + Length;
      end if;
      Get (Text, Pointer, Result);
      if Pointer /= Next then
         Put_Line
         (  "["
         &  Text (Text'First..Pointer - 1)
         &  "|"
         &  Text (Pointer..Text'Last)
         &  "] Pointer "
         &  Image (Pointer)
         &  "/="
         &  Image (Next)
         &  " (expected)"
         );
         raise Data_Error;
      elsif abs (Result - Value) > Eps then
         Put_Line
         (  "["
         &  Text (Text'First..Pointer - 1)
         &  "|"
         &  Text (Pointer..Text'Last)
         &  "] "
         &  Duration'Image (Result)
         &  "/="
         &  Duration'Image (Value)
         &  " (expected)"
         );
         raise Data_Error;
      elsif abs (Strings_Edit.ISO_8601.Value (Text) - Value) > Eps then
         Put_Line
         (  "["
         &  Text (Text'First..Pointer - 1)
         &  "|"
         &  Text (Pointer..Text'Last)
         &  "] "
         &  Duration'Image (Result)
         &  "/="
         &  Duration'Image (Value)
         &  " (expected)"
         );
         raise Data_Error;
      end if;
   end Test;

   procedure Test
             (  Text   : String;
                Value  : Time;
                Length : Integer  := -1;
                Eps    : Duration := 0.01
             )  is
      Pointer : Integer := Text'First;
      Next    : Integer;
      Result  : Time;
   begin
      if Length = -1 then
         Next := Text'Last + 1;
      else
         Next := Pointer + Length;
      end if;
      Get (Text, Pointer, Result);
      if Pointer /= Next then
         Put_Line
         (  Text (Text'First..Pointer - 1)
         &  "|"
         &  Text (Pointer..Text'Last)
         &  " Pointer "
         &  Image (Pointer)
         &  "/="
         &  Image (Next)
         &  " (expected)"
         );
         raise Data_Error;
      elsif abs (Result - Value) > Eps then
         Put_Line
         (  Text (Text'First..Pointer - 1)
         &  "|"
         &  Text (Pointer..Text'Last)
         &  " "
         &  Image (Result)
         &  "/="
         &  Image (Value)
         &  " (expected)"
         );
         raise Data_Error;
      end if;
   end Test;

   procedure Test_Value
             (  Text     : String;
                Fraction : Second_Fraction;
                Value    : Duration
             )  is
   begin
      if Image (Value, Fraction) /= Text then
         Put_Line
         (  Image (Value, Fraction)
         &  "/="
         &  Text
         &  " (expected)"
         );
         raise Data_Error;
      end if;
   end Test_Value;

   procedure Test_Value
             (  Text     : String;
                Fraction : Second_Fraction;
                Value    : Time
             )  is
   begin
      if Image (Value, Fraction) /= Text then
         Put_Line
         (  Image (Value)
         &  "/="
         &  Text
         &  " (expected)"
         );
         raise Data_Error;
      end if;
   end Test_Value;

begin
   Test ("P1W", 7.0 * 24.0 * 60.0 * 60.0);
   Test ("PT0S", 0.0);
   Test ("P1.5D", 1.5 * 24.0 * 60.0 * 60.0);
   Test ("PT2M3.5S", 2.0 * 60.0 + 3.5);
   Test ("PT2M3,5S", 2.0 * 60.0 + 3.5);
   Test_Value ("PT0S", 0, 0.0);
   Test_Value ("PT10S", 0, 10.0);
   Test_Value ("PT10.000S", 3, 10.0);
   Test_Value ("PT1H", 0, 3600.0);
   Test_Value ("PT1H1S", 0, 3601.0);
   Test ("1998-05-28", Ada.Calendar.Time_Of (1998, 05, 28, 0.0));
   Test ("19980528",   Ada.Calendar.Time_Of (1998, 05, 28, 0.0));
   Test ("2009-W01-1", Ada.Calendar.Time_Of (2008, 12, 29, 0.0));
   Test ("2009W011",   Ada.Calendar.Time_Of (2008, 12, 29, 0.0));
   Test
   (  "2007-04-05T24:00",
      Ada.Calendar.Time_Of (2007, 4, 6, 0.0)
   );
   Test
   (  "2019-10-11T10:30Z",
      Ada.Calendar.Formatting.Time_Of
      (  Year      => 2019,
         Month     => 10,
         Day       => 11,
         Hour      => 10,
         Minute    => 30,
         Second    => 0,
         Time_Zone => 0
   )  );
   Test
   (  "20191011T1030Z",
      Ada.Calendar.Formatting.Time_Of
      (  Year      => 2019,
         Month     => 10,
         Day       => 11,
         Hour      => 10,
         Minute    => 30,
         Second    => 0,
         Time_Zone => 0
   )  );
   Test
   (  "20191011T1030+01",
      Ada.Calendar.Formatting.Time_Of
      (  Year      => 2019,
         Month     => 10,
         Day       => 11,
         Hour      => 10,
         Minute    => 30,
         Second    => 0,
         Time_Zone => 60
   )  );
   Test
   (  "20191011 1030+01",
      Ada.Calendar.Formatting.Time_Of
      (  Year      => 2019,
         Month     => 10,
         Day       => 11,
         Hour      => 10,
         Minute    => 30,
         Second    => 0,
         Time_Zone => 60
   )  );
   Test
   (  "20191011T1030+01:30",
      Ada.Calendar.Formatting.Time_Of
      (  Year      => 2019,
         Month     => 10,
         Day       => 11,
         Hour      => 10,
         Minute    => 30,
         Second    => 0,
         Time_Zone => 90
   )  );
   Test_Value
   (  "2019-10-11T10:30:00Z",
      0,
      Ada.Calendar.Formatting.Time_Of
      (  Year      => 2019,
         Month     => 10,
         Day       => 11,
         Hour      => 10,
         Minute    => 30,
         Second    => 0,
         Time_Zone => 0
   )  );
   Test_Value
   (  "2019-10-11T10:30:00.500000Z",
      6,
      Ada.Calendar.Formatting.Time_Of
      (  Year       => 2019,
         Month      => 10,
         Day        => 11,
         Hour       => 10,
         Minute     => 30,
         Second     => 0,
         Sub_Second => 0.5,
         Time_Zone  => 0
   )  );
   Test
   (  "2019-010T10:30Z",
      Ada.Calendar.Formatting.Time_Of
      (  Year       => 2019,
         Month      => 1,
         Day        => 10,
         Hour       => 10,
         Minute     => 30,
         Second     => 0,
         Sub_Second => 0.0,
         Time_Zone  => 0
   )  );
exception
   when Error : others =>
      Put ("Error: ");
      Put_Line (Exception_Information (Error));
end Test_ISO_8601;
