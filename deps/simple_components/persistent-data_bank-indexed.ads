--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Data_Bank.Indexed                Luebeck            --
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
--  This generic package implements persistent storage  interface  using
--  the  operations  defined  in Persistent.Data_Bank. The abstract type
--  Indexed_Storage_Object  can  be used as the base type for a concrete
--  implementation  of  a  data  base  interface. The derived type shall
--  implement the following operations
--
--  of Persistent.Data_Bank:
--
--  (o)  Commit a transaction
--  (o)  Delete to delete an object by key
--  (o)  Find to find an object by name
--  (o)  Get_Class to get object's class string
--  (o)  Get_Creation_Time to get object's creation time
--  (o)  Get_Data to get object's description string
--  (o)  Get_Dependant to enumerate dependants with backward links
--  (o)  Has_Dependants to check if an object can be deleted
--  (o)  Is_In to check if the object in the storage, by key
--  (o)  Rename to set the object name
--  (o)  Roll_Back a transaction
--  (o)  Seize_Read to initiate a read-only transaction
--  (o)  Seize_Write to initiate a read/write transaction
--  (o)  Store to create a new persistent object
--  (o)  Unname to remove object's name
--  (o)  Update to modify a persistent object
--
--  of Persistent:
--
--  (o)  Get_List to get the list of all persistent objects
--
with Ada.Unchecked_Conversion;
with Persistent.Data_Bank.Index;
with Persistent.Data_Bank.Mutexes;

generic
   type Data_Bank is abstract new Data_Bank_Object with private;
   type Key is new Persistent_Key with private;
   type Key_Array is new Persistent_Key_Array with private;
package Persistent.Data_Bank.Indexed is
   type Indexed_Storage_Object is abstract new Data_Bank with private;
--
-- Create -- A resident object
--
--    Storage        - The indexed storage
--    Object         - The object (a handle to)
--    ID             - The object key (output)
--  [ Name ]         - The name the object will have in the storage
--    Class          - The object class
--    Data           - The object data
--    Parameters     - Additional parameters
--    Direct_Links   - The set of objects the catalogued one depends on
--    Backward_Links - The set of backward links
--  [ Parent ]       - The parent object
--
-- This  procedure is used to create a persistent object resident in the
-- data  storage.  Such  object  is  just  a  proxy  for the data in the
-- storage. The procedure calls Persistent.Data_Bank.Store and then puts
-- the object in the index, to make Object bound.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, non-persistent parent
--    Data_Error       - Inconsistent data base
--    Name_Error       - Name conflict, anonymous parent
--
   procedure Create
             (  Storage        : in out Indexed_Storage_Object;
                Object         : Deposit_Handle;
                ID             : out Key;
                Name           : String;
                Class          : String;
                Data           : String;
                Parameters     : String;
                Direct_Links   : Deposit_Set;
                Backward_Links : Deposit_Set;
                Parent         : Deposit_Handle := Root_Directory
            );
   procedure Create
             (  Storage        : in out Indexed_Storage_Object;
                Object         : Deposit_Handle;
                ID             : out Key;
                Class          : String;
                Data           : String;
                Parameters     : String;
                Direct_Links   : Deposit_Set;
                Backward_Links : Deposit_Set
            );
--
-- Get -- Implements Persistent...
--
-- Effects :
--
--    Seize_Read
--
   function Get
            (  Storage : access Indexed_Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Deposit_Handle;
--
-- Get -- Implements Persistent.Data_Bank...
--
   function Get
            (  Storage : access Indexed_Storage_Object;
               Key     : Persistent_Key'Class
            )  return Deposit_Handle;
--
-- Get_Class -- Implements Persistent...
--
-- Effects :
--
--    Seize_Read
--
   function Get_Class
            (  Storage : access Indexed_Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return String;
--
-- Get_Creation_Time -- Implements Persistent...
--
-- Effects :
--
--    Seize_Read
--
   function Get_Creation_Time
            (  Storage : access Indexed_Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Time;
--
-- Get_Key -- Object's key
--
--    Storage  - The indexed storage
--    Object   - The object which key is requested (a pointer to)
--
-- This  function is used to get a key of the persistent object bound to
-- a  memory-mapped  one.  No  attempt  to  bind  them   is   made,   so
-- Constraint_Error is propagated if Object is not bound or null.
--
-- Returns :
--
--    The object's key in Storage
--
-- Exceptions :
--
--    Constraint_Error - The object is not persistent in Storage
--
   function Get_Key
            (  Storage : access Indexed_Storage_Object;
               Object  : Deposit_Handle
            )  return Key;
--
-- Get_Name -- Implements Persistent...
--
   function Get_Name
            (  Storage : access Indexed_Storage_Object;
               Object  : Deposit_Handle
            )  return String;
--
-- Get_New_Parent_Key -- Get the key of a parent
--
--    List   - Of the bound handles
--    Parent - The new parent of an object
--
-- Returns :
--
--    The key of Parent or Null_Key when Parent is invalid
--
-- Exceptions :
--
--    Constraint_Error - Non-persistent parent
--    Name_Error       - Anonymous parent
--
   function Get_New_Parent_Key
            (  Storage : access Indexed_Storage_Object;
               Parent  : Deposit_Handle
            )  return Key;
--
-- Get_Parent -- Implements Persistent...
--
   function Get_Parent
            (  Storage : access Indexed_Storage_Object;
               Object  : Deposit_Handle
            )  return Deposit_Handle;
--
-- Is_Descendant -- Implements Persistent...
--
   function Is_Descendant
            (  Storage : access Indexed_Storage_Object;
               Object  : Deposit_Handle;
               Parent  : Deposit_Handle
            )  return Boolean;
--
-- Is_In -- Implements Persistent...
--
-- Effects :
--
--    Seize_Read
--
   function Is_In
            (  Storage : access Indexed_Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Boolean;
   function Is_In
            (  Storage : access Indexed_Storage_Object;
               Object  : Deposit_Handle
            )  return Boolean;
--
-- Is_Indexed -- Check if a name is in the index
--
--    Storage - The indexed storage
--    Name    - To check
--    Parent  - Of the object
--
-- Returns :
--
--    True if Name is in the Storage's index
--
-- Exceptions :
--
--    Constraint_Error - Parent does not persist
--    Data_Error       - Inconsistent data base
--
   function Is_Indexed
            (  Storage : access Indexed_Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Boolean;
--
-- Is_Named -- Implements Persistent...
--
   function Is_Named
            (  Storage : access Indexed_Storage_Object;
               Object  : Deposit_Handle
            )  return Boolean;
--
-- Is_Named -- Name check
--
--    Storage - The indexed storage
--    Key     - Of the object
--
-- Returns :
--
--    True when Key refers to a named mapped object
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--
   function Is_Named
            (  Storage : access Indexed_Storage_Object;
               Object  : Key
            )  return Boolean;
--
-- Purge -- The catalogue of bound objects
--
--    Storage  - The indexed storage
--
-- This procedure is called  upon  destruction  of  a  derived  strorage
-- object.  It  deletes all records of any remaining bound objects. Such
-- objects can be only ones non-resident in the  storage.  The  resident
-- ones should contain a handle to the storage  object  preventing  that
-- from destruction. Purge should be the first procedure of Finalize  to
-- ensure accessibility of the storage for updating.
--
-- Effects :
--
--    Updating bound objects in the storage
--
   procedure Purge (Storage : in out Indexed_Storage_Object'Class);
--
-- Put -- Implements Persistent...
--
-- Effects :
--
--    Seize_Write
--
   procedure Put
             (  Storage : in out Indexed_Storage_Object;
                Object  : in out Deposit_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             );
   procedure Put
             (  Storage : in out Indexed_Storage_Object;
                Object  : in out Deposit_Handle
             );
--
-- Put -- Implements Persistent.Data_Bank...
--
   procedure Put
             (  Storage : in out Indexed_Storage_Object;
                Key     : Persistent_Key'Class;
                Object  : Deposit'Class
             );
--
-- Rename -- Implements Persistent...
--
-- Effects :
--
--    Seize_Write
--
   procedure Rename
             (  Storage    : in out Indexed_Storage_Object;
                Old_Name   : String;
                Old_Parent : Deposit_Handle := Root_Directory;
                New_Name   : String;
                New_Parent : Deposit_Handle := Root_Directory
             );
   procedure Rename
             (  Storage    : in out Indexed_Storage_Object;
                Object     : in out Deposit_Handle;
                New_Name   : String;
                New_Parent : Deposit_Handle := Root_Directory
             );
--
-- Store_Object -- Implements operation Put with write mutex seized
--
--    Storage - The indexed storage
--    Object  - A pointer to the object
--
-- This  operation  is  called  after Seize_Write in order to put Object
-- indicated by a pointer to into Storage.
--
-- Exceptions :
--
--    Constraint_Error - Object is null
--    other            - Exceptions of Put
--
   procedure Store_Object
             (  Storage : in out Indexed_Storage_Object'Class;
                Object  : Deposit_Ptr
             );
--
-- Unname -- Implements Persistent...
--
-- Effects :
--
--    Seize_Write
--
   procedure Unname
             (  Storage : in out Indexed_Storage_Object;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             );
   procedure Unname
             (  Storage : in out Indexed_Storage_Object;
                Object  : in out Deposit_Handle
             );

private
   use Persistent.Data_Bank.Mutexes;
   pragma Inline (Is_Descendant);

   package Storage_Index is
      new Persistent.Data_Bank.Index (Data_Bank, Key, Key_Array);

   type Indexed_Storage_Object is abstract new Data_Bank with record
      Index : Storage_Index.Catalogue
                 (Indexed_Storage_Object'Unchecked_Access);
      Lock  : aliased Mutex;
   end record;

   type Indexed_Storage_Object_Ptr is
      access all Indexed_Storage_Object'Class;
   function To_Persistent_Storage_Ptr is
      new Ada.Unchecked_Conversion
          (  Indexed_Storage_Object_Ptr,
             Storage_Object_Ptr
          );

end Persistent.Data_Bank.Indexed;
