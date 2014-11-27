--                                                                    --
--  package Persistent.Handle       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2002       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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
--  This  package  defines  the  type  Storage_Handle which is used to
--  access data base objects.
--
package Persistent.Handle is
   pragma Elaborate_Body (Persistent.Handle);

   type Storage_Handle is tagged private;
--
-- Get -- Get a persistent object by name
--
--    Storage - A handle to
--    Name    - Name of the object (UTF-8 encoded if string)
--    Parent  - The parent object
--
-- This function searches for the specified object by its name.  If  the
-- object  is already available a handle to it is returned. Otherwise it
-- first is restored from the persistent storage. The  parameter  Parent
-- specifies  the  parent  object,  usually  a diretory where the object
-- should be searched for. The parameter  Name  is  uniquely  identifies
-- object relatively to Parent. The root-level objects have no parents.
--
-- Returns :
--
--    A handle to the object
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, Parent is not persistent
--    Data_Error       - Inconsistent storage
--    End_Error        - No such object
--    Use_Error        - Object's class is unknown
--
   function Get
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Deposit_Handle;
   function Get
            (  Storage : Storage_Handle;
               Name    : Wide_String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Deposit_Handle;
--
-- Get_Class -- Of a persistent object
--
--    Storage - A handle to
--    Name    - Name of a persistent object (UTF-8 encoded if string)
--
-- Returns :
--
--    The object's class in Storage
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or not a persistent object
--    Data_Error       - Inconsistent storage
--    End_Error        - No such object
--
   function Get_Class
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return String;
   function Get_Class
            (  Storage : Storage_Handle;
               Name    : Wide_String;
               Parent  : Deposit_Handle := Root_Directory
            )  return String;
--
-- Get_Creation_Time -- Of a persistent object creation
--
--    Storage - A handle to
--    Name    - The name of an object there
--    Parent  - The parent object
--
-- Returns :
--
--    The object's creation time stamp in Storage
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or not a persistent object
--    Data_Error       - Inconsistent data base
--    End_Error        - No such object
--
   function Get_Creation_Time
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Time;
   function Get_Creation_Time
            (  Storage : Storage_Handle;
               Name    : Wide_String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Time;
--
-- Get_List -- List of persistent objects
--
--    Storage     - A handle to
--    Prefix      - Of names to get (UTF-8 encoded if string)
--    Suffix      - Of the names (UTF-8 encoded if string)
--    Equivalence - Of the characters when compared
--    Parent      - The parent object
--
-- This function returns a complete list of all named objects persistent
-- in Storage. The list does not include anonymous  persistent  objects.
-- Only names starting with Prefix and ending with Suffix are  returned.
-- When  names  are compared two characters are considered same if their
-- corresponding values  in  the  mapping  Equivalence  are  same.  When
-- Equivalence   is   null,   identity   mapping   is    assumed.    Use
-- Strings_Edit.Mapping.To_Lowercase  for  a  case-insensitive  mapping.
-- Observe that Prefix may  not  overlap  Suffix  when  matched.  So  if
-- Prefix="AB"  and  Suffix="BC",  then  "ABC"  does not fit, but "ABBC"
-- does. The parameter Parent specifies the parent object.
--
-- Returns :
--
--    Names of the objects in the data base
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, Parent is not persistent
--    Data_Error       - Inconsistent storage
--
   function Get_List
            (  Storage     : Storage_Handle;
               Prefix      : String         := "";
               Suffix      : String         := "";
               Equivalence : Unicode_Mapping_Function := null;
               Parent      : Deposit_Handle := Root_Directory
            )  return Catalogue.Set;
   function Get_List
            (  Storage     : Storage_Handle;
               Prefix      : Wide_String;
               Suffix      : Wide_String;
               Equivalence : Unicode_Mapping_Function := null;
               Parent      : Deposit_Handle := Root_Directory
            )  return Catalogue.Set;
--
-- Get_Name -- Of a persistent object
--
--    Storage - A handle to
--    Object  - A handle to
--
-- Returns :
--
--    The object name in Storage
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or not a persistent object
--    Data_Error       - Inconsistent storage
--    Name_Error       - Object is anonymous
--
   function Get_Name
            (  Storage : Storage_Handle;
               Object  : Deposit_Handle
            )  return String;
--
-- Get_Parent -- Of a persistent object
--
--    Storage - A handle to
--    Object  - A handle to
--
-- Returns :
--
--    A handle to the object's parent
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or not a persistent object
--    Data_Error       - Inconsistent storage
--    Name_Error       - Object is anonymous
--
   function Get_Parent
            (  Storage : Storage_Handle;
               Object  : Deposit_Handle
            )  return Deposit_Handle;
--
-- Invalidate -- Detach handle from the object
--
--    Storage - A handle to
--
-- This procedure makes handle pointing to nothing. If it was  the  last
-- reference to the object, the latter is destroyed.
--
   procedure Invalidate (Storage : in out Storage_Handle);
--
-- Is_Descendant -- Check if an object is a descendant of another
--
--    Storage - A handle to the persistent storage
--    Object  - A handle to the object check
--    Parent  - A handle to the alleged parent object
--
-- This  functions check is Object is a descendant of Parent. The result
-- is false if Object is invalid or does  either  does  not  persist  in
-- Storage. Otherwise it is true when Parent is invalid.
--
-- Returns :
--
--    True if Object is a descendant of Parent
--
-- Exceptions :
--
--    Constrain_Error - Invalid Storage handle
--    Data_Error      - Inconsistent data base
--
   function Is_Descendant
            (  Storage : Storage_Handle;
               Object  : Deposit_Handle;
               Parent  : Deposit_Handle
            )  return Boolean;
--
-- Is_In -- Check if an object persists in the storage
--
--    Storage       - A handle to
--    Name / Object - Name of or a handle to a persistent object
--  [ Parent ]      - The parent object
--
-- This function checks whether Name / Object persists  in  Storage.  If
-- Object is not a valid handle, the result is False.
--
-- Returns :
--
--    True if Object persists in Storage
--
-- Exceptions :
--
--    Constraint_Error - Invalid storage handle, non-persistent Parent
--    Data_Error       - Inconsistent storage
--
   function Is_In
            (  Storage : Storage_Handle;
               Object  : Deposit_Handle
            )  return Boolean;
   function Is_In
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Boolean;
   function Is_In
            (  Storage : Storage_Handle;
               Name    : Wide_String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Boolean;
--
-- Is_Named -- Check if an object has a name
--
--    Storage - A handle to
--    Object  - A handle to
--
-- The result is false when Object is not a valid handle.
--
-- Returns :
--
--    The object is persistent and named in Storage
--
-- Exceptions :
--
--    Constraint_Error - Invalid storage handle
--    Data_Error       - Inconsistent data base
--
   function Is_Named
            (  Storage : Storage_Handle;
               Object  : Deposit_Handle
            )  return Boolean;
--
-- Is_Valid -- Check if the handle is associated with an object
--
--    Storage - The handle
--
-- Returns :
--
--    True if the handle can de dereferenced
--
   function Is_Valid (Storage : Storage_Handle) return Boolean;
--
-- Ptr -- Get the pointer to the object by a handle
--
--    Storage - The handle
--
-- Returns :
--
--    The referenced object
--
   function Ptr (Storage : Storage_Handle) return Storage_Object_Ptr;
--
-- Put -- Make an object persistent
--
--    Storage - A handle to
--    Object  - A handle to the object to store in Storage
--  [ Name     - The name of the object being stored
--    Parent ] - The parent object
--
-- This procedure is used to store Object in Storage. The parameter Name
-- specifies the object name there. When omitted the  object  is  stored
-- anonymous. Anonymous persistent objects are collected  when  no  more
-- used. When Object already persists in Storage and Name is  specified,
-- then  it  is  checked  that  it is same. If this check fails, Name is
-- illegal. Then or else when Name is empty, illegal or  conflicts  with
-- the name of another immediate child object of Parent, then Name_Error
-- is  propagated.  It is also propagated when Parent is anonymous. When
-- name  is  not  specified  no  check  is  made.  Constraint_Error   is
-- propagated when Parent is legal but does not indicate a persistent in
-- Storage object or else when Parent is a descendant of Object.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, non-persistent or child Parent
--    Data_Error       - Inconsistent storage
--    Name_Error       - Name conflict, illegal name, anonymous parent
--
   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Deposit_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             );
   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Deposit_Handle;
                Name    : Wide_String;
                Parent  : Deposit_Handle := Root_Directory
             );
   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Deposit_Handle
             );
--
-- Ref -- Get handle to a persistent storage object
--
--    Storage - The object
--
-- Returns :
--
--    Handle to the object
--
   function Ref (Storage : Storage_Object_Ptr) return Storage_Handle;
--
-- Rename -- A persistent objects
--
--    Storage           - A handle to
--    Old_Name / Object - The object to be renamed (handle or name)
--  [ Old_Parent ]      - The old parent object
--    New_Name          - The new object name
--  [ New_Parent ]      - The new parent object
--
-- This procedure changes the name of the object specified by either its
-- old name or by a handle to it.  When  renamed  object  was  anonymous
-- before renaming it becomes a named one. When  Object  is  an  invalid
-- handle or does not refer a persistent object then Constraint_Error is
-- propagated. It is also propagates when the new object parent  is  not
-- in Store or else is a descendant of Object. End_Error  is  propagated
-- when Old_Name  does  not  refer  any  persistent  object.  Name_Error
-- indicates  an  illegal, empty or conflicting name, or when new parent
-- is anonymous or a descendant of the object.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or not a persistent object
--    Data_Error       - Inconsistent storage
--    End_Error        - No such object
--    Name_Error       - Name conflict, there is another object named so
--
   procedure Rename
             (  Storage    : in out Storage_Handle;
                Old_Name   : String;
                Old_Parent : Deposit_Handle := Root_Directory;
                New_Name   : String;
                New_Parent : Deposit_Handle := Root_Directory
             );
   procedure Rename
             (  Storage    : in out Storage_Handle;
                Old_Name   : Wide_String;
                Old_Parent : Deposit_Handle := Root_Directory ;
                New_Name   : Wide_String;
                New_Parent : Deposit_Handle := Root_Directory
             );
   procedure Rename
             (  Storage    : in out Storage_Handle;
                Object     : in out Deposit_Handle;
                New_Name   : String;
                New_Parent : Deposit_Handle := Root_Directory
             );
   procedure Rename
             (  Storage    : in out Storage_Handle;
                Object     : in out Deposit_Handle;
                New_Name   : Wide_String;
                New_Parent : Deposit_Handle := Root_Directory
             );
--
-- Set -- Handle to an object
--
--    Storage - A handle to set
--    Object  - A pointer to the persistent storage object
--
   procedure Set
             (  Storage : in out Storage_Handle;
                Object  : Storage_Object_Ptr
             );
--
-- Unname -- A persistent objects
--
--    Storage       - A handle to
--    Name / Object - To be unnamed, a handle to or the name of
--  [ Parent ]      - The parent object
--
-- This procedure  makes  the  object  anonymous.  Unnamed  objects  are
-- automatically  deleted  when  no  more in use. Nothing happens if the
-- object is already unnamed or does not persist in Storage.  Note  that
-- anonymous  objects  are  not  deleted  as  long  as  they have memory
-- resident counterparts. Observe a difference between Unname and Delete
-- called on an object handle. Delete requests object deletion from both
-- memory  and persistent storage. Unname does it for persistent storage
-- only.  Both  may  have  no immediate effect if the object is still in
-- use.
--
-- Exceptions :
--
--    Constraint_Error - Invalid Storage handle, non-persistent Parent
--    Data_Error       - Inconsistent storage
--
   procedure Unname
             (  Storage : in out Storage_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             );
   procedure Unname
             (  Storage : in out Storage_Handle;
                Name    : Wide_String;
                Parent  : Deposit_Handle := Root_Directory
             );
   procedure Unname
             (  Storage : in out Storage_Handle;
                Object  : in out Deposit_Handle
             );
--
-- = -- Comparison compares referenced objects
--
   function "=" (Left, Right : Storage_Handle) return Boolean;

private
   pragma Inline (Get);
   pragma Inline (Get_Class);
   pragma Inline (Get_List);
   pragma Inline (Get_Name);
   pragma Inline (Get_Parent);
   pragma Inline (Invalidate);
   pragma Inline (Is_Descendant);
   pragma Inline (Is_In);
   pragma Inline (Is_Valid);
   pragma Inline (Ptr);
   pragma Inline (Put);
   pragma Inline (Ref);
   pragma Inline (Set);
   pragma Inline (Rename);
   pragma Inline (Unname);
   pragma Inline ("=");

   type Storage_Handle is new Handles.Handle with null record;

end Persistent.Handle;
