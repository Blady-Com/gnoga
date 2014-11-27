--                                                                    --
--  package APQ.Links               Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  18:59 10 Feb 2008  --
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
with APQ.Common;       use APQ.Common;
with APQ.Keys;         use APQ.Keys;
with APQ.Keys.Arrays;  use APQ.Keys.Arrays;
with APQ.Keys.Sets;    use APQ.Keys.Sets;

package APQ.Links is
   pragma Elaborate_Body (APQ.Links);
--
-- Create_Table -- Create a dependency table if not exists
--
--    Data_Base  - The data base
--    Table_Name - The table to create
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Data base access violation
--
   procedure Create_Table
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String
             );
--
-- Delete -- An object from the table
--
--    Data_Base  - The data base
--    Table_Name - The table name
--    Object     - The object (an identifier of)
--
-- Exceptions :
--
--    Data_Error - Data base error
--    Use_Error  - Data base access violation
--
   procedure Delete
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String;
                Object     : Object_ID
             );
--
-- Depends_On -- Dependency test
--
--    Data_Base  - The data base
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
--    Use_Error  - Data base access violation
--
   function Depends_On
            (  Data_Base  : APQ_Data_Base;
               Table_Name : String;
               Dependant  : Object_ID;
               Referent   : Object_ID
            )  return Boolean;
--
-- Get_Dependants -- Get the dependency list
--
--    Data_Base  - The data base
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
--    Use_Error  - Data base access violation
--
   procedure Get_Dependants
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String;
                Referent   : Object_ID;
                Dependants : in out Set
             );
--
-- Get_References -- Get the references list
--
--    Data_Base  - The data base
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
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referents  : in out Unbounded_Array;
                Pointer    : in out Integer
             );
--
-- Get_References -- Get the references list
--
--    Data_Base  - The data base
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
--    Use_Error  - Data base access violation
--
   procedure Get_References
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referents  : in out Set
             );
--
-- Has_Dependants -- Check the dependency list
--
--    Data_Base  - The data base
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
--    Use_Error  - Data base access violation
--
   function Has_Dependants
            (  Data_Base  : APQ_Data_Base;
               Table_Name : String;
               Referent   : Object_ID
            )  return Boolean;
--
-- Reference -- Add a reference
--
--    Data_Base  - The data base
--    Table_Name - The table name
--    Dependant  - Dependent object  (an identifier of)
--    Referent   - Referenced object (an identifier of)
--
-- Exceptions :
--
--    Data_Error - Data base error
--    Use_Error  - Data base access violation
--
   procedure Reference
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referent   : Object_ID
             );
--
-- Unreference -- Remove a reference
--
--    Data_Base  - The data base
--    Table_Name - The table name
--    Dependant  - Dependent object  (an identifier of)
--    Referent   - Referenced object (an identifier of)
--
-- Exceptions :
--
--    Data_Error - Data base error
--    Use_Error  - Data base access violation
--
   procedure Unreference
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referent   : Object_ID
             );
end APQ.Links;
