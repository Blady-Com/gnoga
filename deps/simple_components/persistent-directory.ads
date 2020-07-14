--                                                                    --
--  package Persistent.Directory    Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2007       --
--                                                                    --
--                                Last revision :  10:09 24 May 2020  --
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
-- This package  defines  the  type  Directory_Object.  Directories  are
-- persistent  objects  holding  names  of  other  objects.  The have no
-- functionality other than thet.
--
with Object.Archived;    use Object.Archived;
with Persistent.Handle;  use Persistent.Handle;

with Ada.Unchecked_Conversion;

package Persistent.Directory is
   pragma Elaborate_Body (Persistent.Directory);
--
-- Create -- A new directory
--
--    Storage   - To create the directory in
--    Directory - The result
--    Name      - Of the directory
--    Parent    - The parent of
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, non-persistent Parent
--    Data_Error       - Inconsistent storage
--    Name_Error       - Name conflict, illegal name, anonymous parent
--
   procedure Create
             (  Storage   : in out Storage_Handle;
                Directory : out Deposit_Handle;
                Name      : String;
                Parent    : Deposit_Handle := Root_Directory
             );
--
-- Is_Directory -- Check if the object is a directory
--
--    Object - A handle to the object to test
--
-- Returns :
--
--    True if Object is a valid handle to a directory
--
   function Is_Directory (Object : Deposit_Handle) return Boolean;
--
-- Directory_Class -- Directory object's class
--
   Directory_Class : constant String := "Directory";
private
   pragma Inline (Is_Directory);
--
-- Directory_Object -- The base type for the catalogue types
--
   type Directory_Object is new Deposit with null record;
   type Directory_Object_Ptr is access Directory_Object'Class;
   for Directory_Object_Ptr'Storage_Pool use Deposit_Ptr'Storage_Pool;
--
-- Get_Class -- Overrides Object.Archived...
--
   function Get_Class (Object : Directory_Object) return String;
--
-- Is_Modified -- Overrides Object.Archived...
--
   function Is_Modified (Directory : Directory_Object) return Boolean;
--
-- Reset_Modified -- Overrides Object.Archived...
--
   procedure Reset_Modified (Directory : in out Directory_Object);
--
-- To_Deposit_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to archived object
--
   function To_Deposit_Ptr is
      new Ada.Unchecked_Conversion
          (  Directory_Object_Ptr,
             Deposit_Ptr
          );
--
-- To_Directory_Object_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to feature
--
-- Exceptions :
--
--    Constraint_Error - The pointed object is not a feature
--
   function To_Directory_Object_Ptr (Ptr : Deposit_Ptr)
      return Directory_Object_Ptr;
--
-- Store -- Overrides Object.Archived...
--
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Object      : Directory_Object
             );

   pragma Inline (To_Directory_Object_Ptr);
end Persistent.Directory;
