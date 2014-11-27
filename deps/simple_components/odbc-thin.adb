--                                                                    --
--  package ODBC.Thin               Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2012       --
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

package body ODBC.Thin is

   function SQL_LEN_BINARY_ATTR (Length : Positive) return SQLINTEGER is
   begin
      return SQL_LEN_BINARY_ATTR_OFFSET - SQLINTEGER (Length);
   end SQL_LEN_BINARY_ATTR;

   function SQL_LEN_BINARY_ATTR (Length : Positive) return SQLSMALLINT is
   begin
      return SQL_LEN_BINARY_ATTR_OFFSET - SQLSMALLINT (Length);
   end SQL_LEN_BINARY_ATTR;

   function To_Ada (Value : SQLWCHAR_Array) return Wide_String is
      Result  : Wide_String (1..Value'Length);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         exit when Value (Index) = SQLWCHAR'Val (0);
         Result (Pointer) := Value (Index);
         Pointer := Pointer + 1;
      end loop;
      return Result (1..Pointer - 1);
   end To_Ada;

   function To_C (Value : Wide_String) return SQLWCHAR_Array is
      Result  : SQLWCHAR_Array (0..Value'Length);
      Pointer : size_t := Result'First;
   begin
      for Index in Value'Range loop
         exit when Value (Index) = SQLWCHAR'Val (0);
         Result (Pointer) := Value (Index);
         Pointer := Pointer + 1;
      end loop;
      Result (Pointer) := SQLWCHAR'Val (0);
      return Result (0..Pointer);
   end To_C;

end ODBC.Thin;
