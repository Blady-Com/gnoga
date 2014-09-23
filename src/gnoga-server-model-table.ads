------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--               G N O G A . S E R V E R . M O D E L . T A B L E            --
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
------------------------------------------------------------------------------

with Gnoga.Server.Database;
with Gnoga.Server.Model.Queries;

generic
   Name       : String;
   Connection : access Gnoga.Server.Database.Connection;
package Gnoga.Server.Model.Table is

   Table_Name : aliased constant String := Name;

   type Active_Record is new Gnoga.Server.Model.Active_Record
     (Table_Name'Access, Connection)
   with null record;

   function Find_All
     (Like     : String := "";
      Order_By : String := "")
      return Gnoga.Server.Model.Queries.Active_Record_Array.Vector;

   function Find_Items
     (Parent         : Gnoga.Server.Model.Active_Record'Class;
      Like           : String := "";
      Order_By       : String := "")
      return Gnoga.Server.Model.Queries.Active_Record_Array.Vector;
   --  Find all children in a one to many relationship with
   --  Parent. (keyed on parent(less the s)_id in child table)


end Gnoga.Server.Model.Table;
