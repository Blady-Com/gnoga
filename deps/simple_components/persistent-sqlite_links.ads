--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.SQLite_Links                     Luebeck            --
--  Interface                                      Winter, 2009       --
--                                                                    --
--                                Last revision :  09:24 09 Apr 2010  --
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
--
--  This package is used to deal with a  reference  table.  A  reference
--  table has two columns:
--
--  dependant : INTEGER (object identifier)
--  reference : INTEGER (object identifier)
--
--  A  record is present in the table if dependant depends on reference.
--  Not collected objects depend on themselves.
--
with Persistent.SQLite_Keys;         use Persistent.SQLite_Keys;
with Persistent.SQLite_Keys.Arrays;  use Persistent.SQLite_Keys.Arrays;
with SQLite;                         use SQLite;

with Persistent.SQLite_Keys.Sets;

package Persistent.SQLite_Links is
   pragma Elaborate_Body (Persistent.SQLite_Links);
--
-- Create_Table -- Create a dependency table if not exists
--
--    Base       - The database
--    Table_Name - The table to create
--    Data_Type  - The SQL data type used for indentifiers (name)
--
-- Table is created only if it does not exist.
--
-- Exceptions :
--
--    Data_Error - A data base error
--
-- Effects :
--
--    Command is modified
--
   procedure Create_Table
             (  Base       : Data_Base;
                Table_Name : String;
                Data_Type  : String := "INTEGER"
             );
--
-- Delete -- An object from the table
--
--    Base       - The database
--    Table_Name - The table name
--    Object     - The object (an identifier of)
--
-- Exceptions :
--
--    Data_Error - Data base error
--
-- Effects :
--
--    Command is modified
--
   procedure Delete
             (  Base       : Data_Base;
                Table_Name : String;
                Object     : Object_ID
             );
--
-- Depends_On -- Dependency test
--
--    Base       - The database
--    Table_Name - The table name
--    Dependant  - An object to test (an identifier of)
--    Referent   - An object to test (an identifier of)
--
-- Returns :
--
--    True if Dependant depends on Referent
--
-- Exceptions :
--
--    Data_Error - Data base error
--
-- Effects :
--
--    Command is modified
--
   function Depends_On
            (  Base       : Data_Base;
               Table_Name : String;
               Dependant  : Object_ID;
               Referent   : Object_ID
            )  return Boolean;
--
-- Get_Dependants -- Get the dependency list
--
--    Base       - The database
--    Table_Name - The table name
--    Referent   - An object (an identifier of)
--    Dependants - The set of dependants (identifiers of)
--
-- This  procedure  adds the identifiers of the objects dependent on the
-- object identified by Referent  to  the  set  Dependants.  This  never
-- includes Referent itself.
--
-- Exceptions :
--
--    Data_Error - Data base error
--
-- Effects :
--
--    Command is modified
--
   procedure Get_Dependants
             (  Base       : Data_Base;
                Table_Name : String;
                Referent   : Object_ID;
                Dependants : in out Sets.Set
             );
--
-- Get_References -- Get the references list
--
--    Base       - The database
--    Table_Name - The table name
--    Dependant  - An object (an identifier of)
--    Referents  - The set of references (identifiers of)
--
-- This  procedure  adds  the  identifiers  of the objects referring the
-- object identified by Dependant  to  the  set  Referents.  This  never
-- includes Dependant itself.
--
-- Exceptions :
--
--    Data_Error - Data base error
--
-- Effects :
--
--    Command is modified
--
   procedure Get_References
             (  Base       : Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referents  : in out Sets.Set
             );
--
-- Get_References -- Get the references list
--
--    Base       - The database
--    Table_Name - The table name
--    Dependant  - An object (an identifier of)
--    Referents  - The array of references to fill (identifiers of)
--    Pointer    - The index in Referents to start at
--
-- This  procedure  adds  the  identifiers  of the objects referring the
-- object identified by Dependant to the  array  Referents.  This  never
-- includes Dependant itself. The first element is added at the position
-- Pointer. Pointer is advanced to the next position.
--
-- Exceptions :
--
--    Data_Error - Data base error
--
-- Effects :
--
--    Command is modified
--
   procedure Get_References
             (  Base       : Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referents  : in out Unbounded_Array;
                Pointer    : in out Integer
             );
--
-- Has_Dependants -- Check the dependency list
--
--    Base       - The database
--    Table_Name - The table name
--    Data_Type  - The SQL data type used for indentifiers
--    Referent   - An object (an identifier of)
--
-- Returns :
--
--    True if the list of dependants is not empty
--
-- Exceptions :
--
--    Data_Error - Data base error
--
-- Effects :
--
--    Command is modified
--
   function Has_Dependants
            (  Base       : Data_Base;
               Table_Name : String;
               Referent   : Object_ID
            )  return Boolean;
--
-- Reference -- Add a reference
--
--    Base       - The database
--    Table_Name - The table name
--    Dependant  - Dependent object  (an identifier of)
--    Referent   - Referenced object (an identifier of)
--
-- Exceptions :
--
--    Data_Error - Data base error
--
-- Effects :
--
--    Command is modified
--
   procedure Reference
             (  Base       : Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referent   : Object_ID
             );
--
-- Unreference -- Remove a reference
--
--    Base       - The database
--    Table_Name - The table name
--    Dependant  - Dependent object  (an identifier of)
--  [ Referent ] - Referenced object (an identifier of)
--
-- Exceptions :
--
--    Data_Error - Data base error
--
-- Effects :
--
--    Command is modified
--
   procedure Unreference
             (  Base       : Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referent   : Object_ID
             );
   procedure Unreference
             (  Base       : Data_Base;
                Table_Name : String;
                Dependant  : Object_ID
             );
end Persistent.SQLite_Links;
