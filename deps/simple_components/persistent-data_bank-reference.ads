--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Data_Bank.Reference              Luebeck            --
--  Interface                                      Autumn, 2004       --
--                                                                    --
--                                Last revision :  10:05 22 Nov 2014  --
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
--  Persistent   objects   that  are  not  fully  represented  by  their
--  memory-resident counterparts require a reference to the storage they
--  persist in. This is necessary at least to prevent persistent storage
--  interface object  from  being  prematurely  finalized.  Further  the
--  operation  Object.Archived.Restore  does  not  contain  a  parameter
--  referencing the storage. Special objects of the type  Self_Reference
--  serve this purpose. An  object  may  put  a  Self_Reference  in  its
--  dependency  list  (see Object.Archived.Get_Referents) by calling the
--  procedure  Add  from this package. If it does so then in its Restore
--  it  will  find  a  Self_Reference  again.  The fields of that object
--  denote the persistent storage and the object's key there. 
--
with Persistent.Handle;  use Persistent.Handle;

with Tables;

package Persistent.Data_Bank.Reference is
   pragma Elaborate_Body (Persistent.Data_Bank.Reference);

   Class : constant String := "Persistent.Storage.Reference";
--
-- Self_Reference -- An object referencing persistent storage
--
   type Self_Reference is new Deposit with record
      Storage : Storage_Handle;
      Key     : Persistent_Key_Ptr;
   end record;
--
-- Add -- A storage reference into the dependency list
--
--    List    - The list
--    Storage - A handle to
--    Object  - A key of
--
-- This  procedure  adds  to  List  a  reference  to Storage. Key is the
-- object's key in Storage.
--
   procedure Add
             (  List    : in out Deposit_Container'Class;
                Storage : Storage_Handle;
                Key     : Persistent_Key'Class
             );
--
-- Get_Class -- Implements Object.Archived...
--
   function Get_Class (Object : Self_Reference) return String;
--
-- Image -- When appears in parameters
--
--    Object - The reference
--
-- Returns :
--
--    Textual representation
--
   function Image (Object : Self_Reference) return String;
--
-- Finalize -- Destruction
--
--    Reference - Persistent object reference
--
   procedure Finalize (Reference : in out Self_Reference);
--
-- Is_Modified -- Overrides Object.Archived...
--
   function Is_Modified (Reference : Self_Reference) return Boolean;
--
-- Store -- Implements Object.Archived...
--
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Object      : Self_Reference
             );
--
-- Reset_Modified -- Overrides Object.Archived...
--
   procedure Reset_Modified (Reference : in out Self_Reference);
--
-- Restore -- Implements Object.Archived...
--
   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Object  : out Deposit_Ptr
             );
--
-- Value -- Restore reference from parameters
--
--    Source  - The string as returned by Image
--    Class   - The parent object's class
--    Storage - A handle to
--    Key     - The key
--
-- Returns :
--
--    Reference object
--
   function Value
            (  Source  : String;
               Class   : String;
               Storage : Storage_Handle;
               Key     : Persistent_Key'Class
            )  return Deposit_Handle;
-----------------------------------------------------------------------
--
-- Create -- Reference constructor
--
--    Source  - The string as returned by Image
--    Storage - A handle to
--    Key     - The key
--
-- Returns :
--
--    Reference object
--
   type Create is access function
        (  Source  : String;
           Storage : Storage_Handle;
           Key     : Persistent_Key'Class
        )  return Deposit_Handle;
--
-- Register -- A reference constructor
--
--   Class       - The parent object's class
--   Constructor - The constructing function to use with
--
   procedure Register (Class : String; Constructor : Create);

private
   package Class_Map is new Tables (Create);

end Persistent.Data_Bank.Reference;
