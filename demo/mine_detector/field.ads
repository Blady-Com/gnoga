-- Mine Detector Game
-- Copyright (C) 2014 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Encapsulates the definition of the mine field
--
-- V7.1 2014 Dec 10          Protected field-updating operations
-- V7.0 2014 Dec 01          First Gnoga version
--
with Gnoga.Types;

package Field is
   subtype Valid_Row    is Positive range 1 .. 16; -- Size of the mine field
   subtype Valid_Column is Positive range 1 .. 30;

   type Cell_Location is record
      Row    : Valid_Row    := 1;
      Column : Valid_Column := 1;
   end record;

   subtype Valid_Count is Natural range 0 .. 9; -- Count of # of mines in a cell & its neighbors

   type Field_Info (App_Data : Gnoga.Types.Pointer_To_Connection_Data_Class) is limited private;
private -- Field
   subtype Row_Id    is Integer range Valid_Row'First    - 1 .. Valid_Row'Last    + 1; -- Row around field makes things easier
   subtype Column_Id is Integer range Valid_Column'First - 1 .. Valid_Column'Last + 1;

   type State_Id is (Normal, Marked, Stepped_On); -- Possible states of a cell

   subtype Count_Value is Integer range Valid_Count'First - 1 .. Valid_Count'Last; -- Extra value means not yet counted

   type Cell_Info is record
      State   : State_Id    := Normal;
      Mine    : Boolean     := False;
      Count   : Count_Value := Count_Value'First;
      Stepped : Boolean     := False;
   end record;

   type Field_Set is array (Row_Id, Column_Id) of Cell_Info; -- A mine field

   type Field_Info (App_Data : Gnoga.Types.Pointer_To_Connection_Data_Class) is record
      Num_Mines  : Natural := 0;  -- Changed when first game is started.
      Mine_Field : Field_Set;
      Dead       : Boolean := False;
      To_Mark    : Integer := 0;
      Step_Count : Natural := 0;
   end record;
end Field;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
