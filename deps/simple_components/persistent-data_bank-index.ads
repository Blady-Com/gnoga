--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Data_Bank.Index                  Luebeck            --
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
--  This  package  provides an index for a persistent storage. The index
--  can  be  searched  by object's name, pointer, key. The purpose is to
--  hold  cached  names  of  memory-resident  objects.  The  package  is
--  generic. The generic parameters are:
--
--  (o)  Data_Bank is the base type of the persistent  storage  objects.
--       It can be any descendant of Data_Bank_Object.
--  (o)  Key  is  a  descendant  of  Persistent_Key  to be used with the
--       persistent storage.
--  (o)  Key_Array is a descendant of Persistent_Key_Array  to  use  for
--       unbounded  arrays of keys. It has to be compatible with Key, in
--       the sense that the array elements are of the type Key.
--
with Generic_Set;

generic
   type Data_Bank is abstract new Data_Bank_Object with private;
   type Key is new Persistent_Key with private;
   type Key_Array is new Persistent_Key_Array with private;
package Persistent.Data_Bank.Index is
   type Catalogue (Storage : access Data_Bank'Class) is
      new Ada.Finalization.Limited_Controlled with private;
   type Catalogue_Ptr is access all Catalogue;
--
-- Add - A new record to the catalogue
--
--    List     - Of the bound handles
--    Object   - The object
--    External - The external key
--  [ Name     - The name
--    Parent ] - The parent object key
--
-- When the object already has a record, then nothing happens. When Name
-- is  specified but empty, Name_Error is propagated. For root catalogue
-- records  Parent  is specified as null. Otherwise, the parent shall be
-- already in the catalogue.
--
-- Exceptions :
--
--    Constraint_Error - Non-persistent parent
--    Name_Error       - Name conflict, anonymous parent
--
   procedure Add
             (  List     : in out Catalogue;
                Object   : Deposit_Ptr;
                External : Key;
                Name     : String;
                Parent   : Key
             );
   procedure Add
             (  List     : in out Catalogue;
                Object   : Deposit_Ptr;
                External : Key
             );
--
-- Delete - Delete from the catalogue
--
--    List      - Of the bound handles
--    ID / Name - Of the object
--  [ Parent ]  - The parent of
--
-- Nothing happens if it is not there
--
-- Exceptions :
--
--    Data_Error - Non-persistent or anonymous parent
--
   procedure Delete (List : in out Catalogue; ID : Deposit_Ptr);
   procedure Delete (List : in out Catalogue; ID : Key);
   procedure Delete
             (  List   : in out Catalogue;
                Name   : String;
                Parent : Deposit_Ptr
             );
   procedure Delete
             (  List   : in out Catalogue;
                Name   : String;
                Parent : Key
             );
--
-- Erase -- Deletes all index records
--
--    List - Of the bound handles
--
   procedure Erase (List : in out Catalogue);
--
-- Finalize -- Destructor
--
--    List - Of the bound handles
--
   procedure Finalize (List : in out Catalogue);
--
-- Get -- From the catalogue
--
--    List      - Of the bound handles
--    ID / Name - To search for
--  [ Parent ]  - The parent of
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    End_Error  - Nothing found
--    Data_Error - Non-persistent or anonymous parent
--
   function Get (List : Catalogue; ID : Deposit_Ptr) return Key;
   function Get (List : Catalogue; ID : Deposit_Ptr) return String;
   function Get (List : Catalogue; ID : Key) return Deposit_Ptr;
   function Get (List : Catalogue; ID : Key) return String;
   function Get
            (  List   : Catalogue;
               Name   : String;
               Parent : Deposit_Ptr
            )  return Deposit_Ptr;
   function Get
            (  List   : Catalogue;
               Name   : String;
               Parent : Key
            )  return Deposit_Ptr;
   function Get
            (  List   : Catalogue;
               Name   : String;
               Parent : Deposit_Ptr
            )  return Key;
   function Get
            (  List   : Catalogue;
               Name   : String;
               Parent : Key
            )  return Key;
--
-- Get_Key -- Of a persistent object from the catalogue
--
--    List - Of the bound handles
--    ID   - To search for
--
-- Returns :
--
--    The key of or Null_Key if ID is null
--
-- Exceptions :
--
--    Data_Error - Non-persistent
--
   function Get_Key (List : Catalogue; ID : Deposit_Ptr) return Key;
--
-- Get_Parent -- Of an object
--
--    List - Of the bound handles
--    ID   - To search for
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    End_Error - No such object
--
   function Get_Parent (List : Catalogue; ID : Deposit_Ptr) return Key;
   function Get_Parent (List : Catalogue; ID : Key) return Deposit_Ptr;
   function Get_Parent (List : Catalogue; ID : Key) return Key;
   function Get_Parent (List : Catalogue; ID : Deposit_Ptr)
      return Deposit_Ptr;
--
-- Get_New_Parent_Key -- Get the key of a parent
--
--    List   - Of the bound handles
--    Parent - The new parent of an object
--
-- Returns :
--
--    The key of Parent or Null_Key when Parent is null
--
-- Exceptions :
--
--    Constraint_Error - Non-persistent parent
--    Name_Error       - Anonymous parent
--
   function Get_New_Parent_Key
            (  List   : Catalogue;
               Parent : Deposit_Ptr
            )  return Key;
--
-- Is_Descendant -- A catalogue record
--
--    List   - The catalogue
--    Object - The object to check
--    Parent - The record of the parent
--
-- Returns :
--
--    True if Object is a descendant of Parent
--
   function Is_Descendant
            (  List   : Catalogue;
               Object : Deposit_Ptr;
               Parent : Deposit_Ptr
            )  return Boolean;
--
-- Is_In -- Check if in the Catalogue
--
--    List      - Of the bound handles
--    ID / Name - To search for
--  [ Parent ]  - The parent of
--
-- Returns :
--
--    True if it is in
--
-- Exceptions :
--
--    Data_Error - Non-persistent or anonymous parent
--
   function Is_In (List : Catalogue; ID : Key) return Boolean;
   function Is_In (List : Catalogue; ID : Deposit_Ptr) return Boolean;
   function Is_In
            (  List   : Catalogue;
               ID     : String;
               Parent : Deposit_Ptr
            )  return Boolean;
   function Is_In
            (  List   : Catalogue;
               ID     : String;
               Parent : Key
            )  return Boolean;
--
-- Rename -- In the catalogue and storage
--
--    Store             - To perform renaming in
--    List              - Of the bound handles
--    Object / Old_Name - The object to rename or its name
--  [ Old_Parent ]      - The old parent or null
--    New_Name          - The new name
--  [ New_Parent ]      - The new parent object or null
--
-- These procedures perform renaming. The remaning is don  in  both  the
-- catalogue and  in  the  external  storage.  When  renaming  with  the
-- parameter  New_Parent  specified,  which  is  not  Null_Key, then the
-- corresponding to it object  shall  be  in  the  catalogue.  Otherwise
-- End_Error is propagated.
--
-- Exceptions :
--
--    Constraint_Error - Wrong parent, non-persistent object
--    Data_Error       - Data base error
--    End_Error        - No such object
--    Name_Error       - Name conflict, New_Parent is  a  descendant  or
--                       anonymous
--
-- Note :
--
--    The data base write mutex shall be taken before call
--
   procedure Rename
             (  Store      : in out Data_Bank'Class;
                List       : in out Catalogue;
                Old_Name   : String;
                Old_Parent : Deposit_Ptr;
                New_Name   : String;
                New_Parent : Deposit_Ptr
             );
   procedure Rename
             (  Store      : in out Data_Bank'Class;
                List       : in out Catalogue;
                Object     : Deposit_Ptr;
                New_Name   : String;
                New_Parent : Deposit_Ptr
             );
--
-- Unname -- In the Catalogue
--
--    Store         - To perform renaming in
--    List          - Of the bound handles
--    Object / Name - The object to unname or its name
--  [ Parent ]      - The parent object or null
--
-- Exceptions :
--
--    Constraint_Error - Wrong parent, non-persistent object object
--    Data_Error       - Data base error
--    End_Error        - No such object
--
-- Note :
--
--    The data base write mutex shall be taken before call
--
   procedure Unname
             (  Store  : in out Data_Bank'Class;
                List   : in out Catalogue;
                Name   : String;
                Parent : Deposit_Ptr
             );
   procedure Unname
             (  Store  : in out Data_Bank'Class;
                List   : in out Catalogue;
                Object : Deposit_Ptr
             );
private
   pragma Inline (Get);
   pragma Inline (Get_Parent);
   pragma Inline (Is_In);
--
-- Catalogue_Record -- Bound object record
--
--    External    - The object's external key
--    List        - The Catalogue of bound objects
--    Name        - The object's name
--    Name_Length - The object's name length
--    Parent      - The parent's record (reference count +1)
--
   type Catalogue_Record;
   type Catalogue_Record_Ptr is access all Catalogue_Record;
   type Catalogue_Record (Name_Length : Natural) is
      new Backward_Link with
   record
      List     : Catalogue_Ptr;
      Parent   : Catalogue_Record_Ptr;
      External : Key;
      Name     : String (1..Name_Length);
   end record;
--
-- Deleted -- Overrides Object.Archived...
--
   procedure Deleted
             (  Item  : in out Catalogue_Record;
                Temps : in out Deposit_Container'Class
             );
--
-- Destroyed -- Overrides Object.Archived...
--
   procedure Destroyed (Item : in out Catalogue_Record);

   type By_Name_Ptr is new Catalogue_Record_Ptr;
   function Equal (Left, Right : By_Name_Ptr)
      return Boolean renames "=";
   function "<" (Left, Right : By_Name_Ptr) return Boolean;
   function "=" (Left, Right : By_Name_Ptr) return Boolean;

   type By_Entity_Ptr is access all Object.Entity'Class;
   function Equal (Left, Right : By_Entity_Ptr)
      return Boolean renames "=";
   function "<" (Left, Right : By_Entity_Ptr) return Boolean;
   function "=" (Left, Right : By_Entity_Ptr) return Boolean;

   type By_Key_Ptr is new Catalogue_Record_Ptr;
   function Equal (Left, Right : By_Key_Ptr)
      return Boolean renames "=";
   function "<" (Left, Right : By_Key_Ptr) return Boolean;
   function "=" (Left, Right : By_Key_Ptr) return Boolean;

   package Name_Map is new Generic_Set (By_Name_Ptr,   null);
   package Ptr_Map  is new Generic_Set (By_Entity_Ptr, null);
   package Key_Map  is new Generic_Set (By_Key_Ptr,    null);

   type Catalogue
        (  Storage : access Data_Bank'Class
        )  is new Ada.Finalization.Limited_Controlled with
   record
      By_Name : Name_Map.Set;
      By_Ptr  : Ptr_Map.Set;
      By_Key  : Key_Map.Set;
   end record;
--
-- Find -- Search the Catalogue (a map of)
--
--    List     - To be searched
--    ID       - To search for
--  [ Parent ] - The parent
--
-- Returns :
--
--    Catalogue record index in the corresponding map
--
   function Find (List : Catalogue; ID : Deposit_Ptr) return Integer;
   function Find (List : Catalogue; ID : Key) return Integer;
   function Find
            (  List   : Catalogue;
               ID     : String;
               Parent : Catalogue_Record_Ptr
            )  return Integer;

   type Query_Mode is (As_Old, As_New);
--
-- Get_As -- Get object record
--
--    List - To be searched
--    ID   - To search for
--    As   - Exception propagation modes
--
-- This function is used to check a parent onject. It returns the record
-- of or null if ID is null or Null_Key. When ID is not null but is  not
-- persistent, Constraint_Error is propagated. When result is anonymous,
-- Name_Error is propagated.
--
-- Returns :
--
--    The kex of
--
-- Exceptions :
--
--    Constraint_Error - Non persistent object (As_New)
--    Name_Error       - Anonymous object (As_New)
--    Data_Error       - Any errors (As_Old)
--
   function Get_As
            (  List : Catalogue;
               ID   : Deposit_Ptr;
               As   : Query_Mode
            )  return Catalogue_Record_Ptr;
   function Get_As
            (  List : Catalogue;
               ID   : Key;
               As   : Query_Mode
            )  return Catalogue_Record_Ptr;
--
-- Get_Descendants -- Of a record
--
--    List   - To be searched
--    Item   - To search for the descendants of
--    Result - The set to accumulate the direct and indirect descendants
--
   procedure Get_Descendants
             (  List   : Catalogue;
                Item   : Catalogue_Record_Ptr;
                Result : in out Ptr_Map.Set
             );
--
-- Is_Descendant_Or_Same -- A catalogue record
--
--    List   - The catalogue
--    Object - The object to check
--    Parent - The record of the parent
--
-- Returns :
--
--    True if Object is Parent or a descendant of
--
-- Exceptions :
--
--    Data_Error - One of Object parents was not found
--
   function Is_Descendant_Or_Same
            (  List   : Catalogue;
               Object : Catalogue_Record_Ptr;
               Parent : Catalogue_Record_Ptr
            )  return Boolean;
--
-- Key_Set -- Sets of Keys
--
   package Key_Sets is new Generic_Set (Key, Null_Key);
   use Key_Sets;
--
-- Unname_By_Key -- Unnaming an object by key
--
--    Store - To perform operation in
--    List  - The catalogue
--    ID    - OF the object
--
-- This procedure  recursively  unnames  an  object  in  the  store.  It
-- collects  objects  as necessary. The procedure shall only be used for
-- objects outside the index.
--
-- Exceptions :
--
--    Data_Error - Inconsistent storage
--
   procedure Unname_By_Key
             (  Store : in out Data_Bank'Class;
                List  : in out Catalogue;
                ID    : Key
             );

end Persistent.Data_Bank.Index;
