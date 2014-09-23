------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . S E R V E R . M I G R A T I O N               --
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

with Gnoga.Types;
with Gnoga.Server.Database;

package Gnoga.Server.Migration is
--  Migrations are used to manage the evolution of a database schema
--  each change to the base schema should be coded as an Add_Migration_Up
--  for the change needed for the schema and as an Add_Migration_Down
--  for how the change would be backed out.

   type Migration_Collection is tagged limited private;

   procedure Add_Migration_Up (Collection : in out Migration_Collection;
                               SQL        : in     String);
   --  Add a SQL statement to migrate the schema one step up

   procedure Add_Migration_Down (Collection : in out Migration_Collection;
                                 SQL        : in     String);
   --  Add a SQL statement to migrate the schema down one step

   procedure Migrate_To
     (Collection    : in out Migration_Collection;
      Connection    : in out Gnoga.Server.Database.Connection'Class;
      Level         : in     Natural);
   --  Migrate to level upgrading or degrading the schema as needed

   procedure Setup
     (Collection    : in out Migration_Collection;
      Connection    : in out Gnoga.Server.Database.Connection'Class);
   --  Determines state of the schema and if needed performs
   --  setup for migrations support. It will then Migrate_To
   --  the last added Migration_Up

   function Migrations_Handled_Command_Line
     (Connection          : in     Gnoga.Server.Database.Connection_Access;
      Migration_Procedure : access
        procedure (Collection : in out Migration_Collection))
      return Boolean;
   --  Provides command line support for migrations. Returns True if the
   --  command line was handled but this function.
   --  command line options are:
   --     executable setup
   --         setsup the migration parameter table if needed and migrate to
   --         last migration.
   --     executable migration X
   --         migrate up or down to the specific migration level X

private
   type Migration_Collection is tagged limited
      record
         Migrations_Up   : Gnoga.Types.Data_Array_Type;
         Migrations_Down : Gnoga.Types.Data_Array_Type;
      end record;
end Gnoga.Server.Migration;
