--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     ODBC.Bound_Parameters                       Luebeck            --
--  Interface                                      Autumn, 2012       --
--                                                                    --
--                                Last revision :  11:56 13 Oct 2012  --
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
--  This  package describes  types for binding  parameters  to  prepared
--  statements.
--
with Ada.Calendar;   use Ada.Calendar;
with Interfaces.C;   use Interfaces.C;
with ODBC.SQLTypes;  use ODBC.SQLTypes;

package ODBC.Bound_Parameters is
--
-- SQLTINYINT_Parameter -- Bound parameter holding value
--
   type SQLTINYINT_Parameter is record
      Size  : aliased SQLLEN;
      Value : aliased SQLTINYINT;
   end record;
   function Create (Value : SQLTINYINT) return SQLTINYINT_Parameter;
--
-- SQLUTINYINT_Parameter -- Bound parameter holding value
--
   type SQLUTINYINT_Parameter is record
      Size  : aliased SQLLEN;
      Value : aliased SQLUTINYINT;
   end record;
   function Create (Value : SQLUTINYINT) return SQLUTINYINT_Parameter;
--
-- SQLSMALLINT_Parameter -- Bound parameter holding value
--
   type SQLSMALLINT_Parameter is record
      Size  : aliased SQLLEN;
      Value : aliased SQLSMALLINT;
   end record;
   function Create (Value : SQLSMALLINT) return SQLSMALLINT_Parameter;
--
-- SQLUSMALLINT_Parameter -- Bound parameter holding fixed-length values
--
   type SQLUSMALLINT_Parameter is record
      Size  : aliased SQLLEN;
      Value : aliased SQLUSMALLINT;
   end record;
   function Create (Value : SQLUSMALLINT) return SQLUSMALLINT_Parameter;
--
-- SQLINTEGER_Parameter -- Bound parameter holding fixed-length values
--
   type SQLINTEGER_Parameter is record
      Size  : aliased SQLLEN;
      Value : aliased SQLINTEGER;
   end record;
   function Create (Value : SQLINTEGER) return SQLINTEGER_Parameter;
--
-- SQLUINTEGER_Parameter -- Bound parameter holding fixed-length values
--
   type SQLUINTEGER_Parameter is record
      Size  : aliased SQLLEN;
      Value : aliased SQLUINTEGER;
   end record;
   function Create (Value : SQLUINTEGER) return SQLUINTEGER_Parameter;
--
-- SQLBIGINT_Parameter -- Bound parameter holding fixed-length values
--
   type SQLBIGINT_Parameter is record
      Size  : aliased SQLLEN;
      Value : aliased SQLBIGINT;
   end record;
   function Create (Value : SQLBIGINT) return SQLBIGINT_Parameter;
--
-- SQLUBIGINT_Parameter -- Bound parameter holding fixed-length values
--
   type SQLUBIGINT_Parameter is record
      Size  : aliased SQLLEN;
      Value : aliased SQLUBIGINT;
   end record;
   function Create (Value : SQLUBIGINT) return SQLUBIGINT_Parameter;
--
-- SQLDOUBLE_Parameter -- Bound parameter holding fixed-length values
--
   type SQLDOUBLE_Parameter is record
      Size  : aliased SQLLEN;
      Value : aliased SQLDOUBLE;
   end record;
   function Create (Value : SQLDOUBLE) return SQLDOUBLE_Parameter;
--
-- SQLGUID_Parameter -- Bound parameter holding fixed-length values
--
   type SQLGUID_Parameter is record
      Size  : aliased SQLLEN;
      Value : aliased SQLGUID;
   end record;
   function Create (Value : SQLGUID) return SQLGUID_Parameter;
--
-- SQL_TIMESTAMP_STRUCT_Parameter -- Bound parameter holding timestamp
--
   type SQL_TIMESTAMP_STRUCT_Parameter is record
      Size  : aliased SQLLEN;
      Value : aliased SQL_TIMESTAMP_STRUCT;
   end record;
   function Create (Value : Time) return SQL_TIMESTAMP_STRUCT_Parameter;
--
-- Set -- Assign a value to the parameter
--
--    Parameter - To assign the value to
--    Value     - The value
--
-- Exception :
--
--    Constraint_Error - Value is too large
--
   procedure Set
             (  Parameter : in out SQL_TIMESTAMP_STRUCT_Parameter;
                Value     : Time
             );
--
-- String_Parameter -- Bound parameter holding a string value
--
--    Length - The maximal number of characters
--
   type String_Parameter (Length : size_t) is record
      Size  : aliased SQLLEN := 0;
      Value : char_array (0..Length);
   end record;
   function Create (Value : String) return String_Parameter;
--
-- Set -- Assign a value to the parameter
--
--    Parameter - To assign the value to
--    Value     - The value
--
-- Exception :
--
--    Constraint_Error - Value is too large
--
   procedure Set
             (  Parameter : in out String_Parameter;
                Value     : String
             );
--
-- Wide_String_Parameter -- Bound parameter holding a string value
--
--    Length - The maximal number of characters
--
   type Wide_String_Parameter (Length : size_t) is record
      Size  : aliased SQLLEN := SQLLEN (Length);
      Value : SQLWCHAR_Array (0..Length);
   end record;
   function Create (Value : Wide_String) return Wide_String_Parameter;
--
-- Set -- Assign a value to the parameter
--
--    Parameter - To assign the value to
--    Value     - The value
--
-- Exception :
--
--    Constraint_Error - Value is too large
--
   procedure Set
             (  Parameter : in out Wide_String_Parameter;
                Value     : Wide_String
             );

end ODBC.Bound_Parameters;
