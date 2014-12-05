-- Mine Detector Game
-- Copyright (C) 2014 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Provide the user interface
--
-- V7.0 2014 Dec 01          First Gnoga version
--
with Field;

with Gnoga.Types;

package User_IF is
   procedure Display_Count (Data     : in Gnoga.Types.Pointer_To_Connection_Data_Class;
                            Count    : in Field.Valid_Count;
                            Stepped  : in Boolean;
                            Cell     : in Field.Cell_Location);

   procedure Display_Mark (Data : in Gnoga.Types.Pointer_To_Connection_Data_Class; Cell : in Field.Cell_Location);
   -- Display a marked cell

   procedure Display_Mine (Data : in Gnoga.Types.Pointer_To_Connection_Data_Class; Cell : in Field.Cell_Location);
   -- Display a mine.

   procedure Display_Blank (Data : in Gnoga.Types.Pointer_To_Connection_Data_Class; Cell : in Field.Cell_Location);
   -- Display a blank cell

   procedure Display_To_Go (Data : in Gnoga.Types.Pointer_To_Connection_Data_Class; To_Go : in Integer);
   -- Display # of mines still to mark; can be negative

   procedure Reset_Screen (Data : in Gnoga.Types.Pointer_To_Connection_Data_Class); -- Return to start of game condition

   function Auto_Marking (Data : Gnoga.Types.Pointer_To_Connection_Data_Class) return Boolean; -- Get auto-marking state

   function Extended_Stepping (Data : in Gnoga.Types.Pointer_To_Connection_Data_Class) return Boolean;
   -- Get extended-stepping (after mark) state
end User_IF;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
