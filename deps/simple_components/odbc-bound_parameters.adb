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

with Ada.Exceptions;  use Ada.Exceptions;

package body ODBC.Bound_Parameters is

   function Create (Value : SQLTINYINT) return SQLTINYINT_Parameter is
   begin
      return (0, Value);
   end Create;

   function Create (Value : SQLUTINYINT) return SQLUTINYINT_Parameter is
   begin
      return (0, Value);
   end Create;

   function Create (Value : SQLSMALLINT) return SQLSMALLINT_Parameter is
   begin
      return (0, Value);
   end Create;

   function Create (Value : SQLUSMALLINT)
      return SQLUSMALLINT_Parameter is
   begin
      return (0, Value);
   end Create;

   function Create (Value : SQLINTEGER) return SQLINTEGER_Parameter is
   begin
      return (0, Value);
   end Create;

   function Create (Value : SQLUINTEGER) return SQLUINTEGER_Parameter is
   begin
      return (0, Value);
   end Create;

   function Create (Value : SQLBIGINT) return SQLBIGINT_Parameter is
   begin
      return (0, Value);
   end Create;

   function Create (Value : SQLUBIGINT) return SQLUBIGINT_Parameter is
   begin
      return (0, Value);
   end Create;

   function Create (Value : SQLDOUBLE) return SQLDOUBLE_Parameter is
   begin
      return (0, Value);
   end Create;

   function Create (Value : SQLGUID) return SQLGUID_Parameter is
   begin
      return (0, Value);
   end Create;

   function Create (Value : Time)
      return SQL_TIMESTAMP_STRUCT_Parameter is
   begin
      return (0, From_Time (Value));
   end Create;

   function Create (Value : String) return String_Parameter is
   begin
      return (Value'Length, Value'Length, To_C (Value));
   end Create;

   function Create (Value : Wide_String) return Wide_String_Parameter is
      Result : Wide_String_Parameter (Value'Length);
   begin
      Set (Result, Value);
      return Result;
   end Create;

   procedure Set
             (  Parameter : in out String_Parameter;
                Value     : String
             )  is
      Pointer : size_t := 0;
   begin
      if Value'Length > Parameter.Length then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Value is too large"
         );
      end if;
      for Index in Value'Range loop
         Parameter.Value (Pointer) := To_C (Value (Index));
         Pointer := Pointer + 1;
      end loop;
      Parameter.Value (Pointer) := char'Val (0);
      Parameter.Size := SQLLEN (Pointer);
   end Set;

   procedure Set
             (  Parameter : in out Wide_String_Parameter;
                Value     : Wide_String
             )  is
      Pointer : size_t := 0;
   begin
      if Value'Length > Parameter.Length then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Value is too large"
         );
      end if;
      for Index in Value'Range loop
         Parameter.Value (Pointer) := Value (Index);
         Pointer := Pointer + 1;
      end loop;
      Parameter.Value (Pointer) := SQLWCHAR'Val (0);
      Parameter.Size := SQLLEN (Pointer) * 2;
   end Set;

   procedure Set
             (  Parameter : in out SQL_TIMESTAMP_STRUCT_Parameter;
                Value     : Time
             )  is
   begin
      Parameter.Value := From_Time (Value);
   end Set;

end ODBC.Bound_Parameters;
