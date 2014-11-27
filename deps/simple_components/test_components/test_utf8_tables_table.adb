--                                                                    --
--  package Test_UTF8_Tables_Table  Copyright (c)  Dmitry A. Kazakov  --
--  Test (a table instantiation)                   Luebeck            --
--                                                 Spring, 2008       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2010  --
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

with Ada.Exceptions;          use Ada.Exceptions;
with Strings_Edit.UTF8;       use Strings_Edit.UTF8;
with Strings_Edit.UTF8.Maps;  use Strings_Edit.UTF8.Maps;

with Ada.IO_Exceptions;

with Strings_Edit.UTF8.Maps.Constants;
use  Strings_Edit.UTF8.Maps.Constants;

package body Test_UTF8_Tables_Table is

   Valid : constant Unicode_Set :=
              (  Identifier_Start_Set
              or Identifier_Extend_Set
              or To_Set (16#09#)
              or Space_Set
              or Other_Format_Set
              );

   procedure Check_Spelling (Name : String) is
      Pointer : Integer := Name'First;
      Code    : UTF8_Code_Point;
   begin
      if Name'Length = 0 then
         Raise_Exception (Constraint_Error'Identity, "Empty name");
      end if;
      Get (Name, Pointer, Code);
      if not Is_In (Code, Identifier_Start_Set) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Name first character is illegal"
         );
      end if;
      while Pointer < Name'Last loop
         Get (Name, Pointer, Code);
         if not Is_In (Code, Valid) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Name character is illegal"
            );
         end if;
      end loop;
   exception
      when Ada.IO_Exceptions.Data_Error =>
         Raise_Exception
         (  Constraint_Error'Identity,
            "Name is not an UTF-8 string"
         );
   end Check_Spelling;

   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean is
      Index : Integer := Pointer;
      Code  : UTF8_Code_Point;
   begin
      Get (Source, Index, Code);
      return not Is_In (Code, Valid) or else Is_In (Code, Space_Set);
   exception
      when Ada.IO_Exceptions.Data_Error =>
         return False;
   end Check_Matched;

end Test_UTF8_Tables_Table;
