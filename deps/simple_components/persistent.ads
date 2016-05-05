--                                                                    --
--  package Persistent              Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2002       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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
--  This package defines an interface to an abstract persistent storage.
--  The abstract type Storage_Object describes the interface. Persistent
--  object are identified by their names. Additionally anonymous objects
--  can be created and deleted as required by the named  ones.  When  an
--  object depends on some other objects,  then  when  stored  into  the
--  storage,  the  referred  objects  are  stored as well if they do not
--  already persist there. In such case these objects will be anonymous.
--  Anonymous persistent objects are subject of garbage collection.  The
--  way of collection is determined by the implementation. Named objects
--  are  deleted only on request. For this Object.Archived.Delete can be
--  applied to the object's handle. If  the  object  cannot  be  deleted
--  immediatelly it becomes anonymous for later  collection.  Persistent
--  storage  interfaces  are itself objects and are a subject of garbage
--  collection as well. Named object constitute a forest-like  structure
--  of parents and children. The pair (parent, name) uniquely identifies
--  an object. An object may have only one parent. The root-level object
--  have no  parents.  The  object  names  are  in  UTF-8  encoded.  The
--  implementation should provide encoding when the  persistent  storage
--  does not  natively  support  Unicode.  Note  that  the  parent-child
--  relation is  independent  on  the  uses-used  relation.  The  parent
--  objects  are  not restricted in what they could be, however, usually
--  it is "directory" objects having no distinct functionality.
--
with Ada.Calendar;            use Ada.Calendar;
with Ada.Exceptions;          use Ada.Exceptions;
with Deposit_Handles;         use Deposit_Handles;
with Strings_Edit.UTF8;       use Strings_Edit.UTF8;
with Strings_Edit.UTF8.Maps;  use Strings_Edit.UTF8.Maps;

with Generic_Indefinite_Set;
with Object.Handle;

package Persistent is
--
-- Storage_Object -- Represents a persistent storage interface
--
   type Storage_Object is abstract new Object.Entity with private;
   type Storage_Object_Ptr is access Storage_Object'Class;
   for Storage_Object_Ptr'Storage_Pool
      use Object.Entity_Ptr'Storage_Pool;
   subtype Deposit_Handle is Deposit_Handles.Handle;
--
-- Catalogue -- List of object names
--
   package Catalogue is new Generic_Indefinite_Set (String);
--
-- Root_Directory -- The root directory object (an invalid handle)
--
   Root_Directory : constant Deposit_Handle;
--
-- Get -- Get a persistent object by name
--
--    Storage - A pesistent storage object (a pointer to)
--    Name    - Of the object (UTF-8 encoded)
--    Parent  - The parent object
--
-- This function returns a handle to a persistent object by its name. An
-- implementation should  first  check  if  the  the  persistent  object
-- already has a memory-resident counterpart. Otherwise it should create
-- one from the persistent storage. The parameter Parent  specifies  the
-- parent object, usually a diretory where the object should be searched
-- for. The parameter Name is uniquely identifies object  relatively  to
-- Parent. The root-level objects have no parents.
--
-- Returns :
--
--    A handle to the object
--
-- Exceptions :
--
--    Constraint_Error - Parent is not persistent
--    Data_Error       - Inconsistent data base
--    End_Error        - No such object
--    Use_Error        - Object's class is unknown
--
   function Get
            (  Storage : access Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Deposit_Handle is abstract;
--
-- Get_Class -- Of a persistent object
--
--    Storage - A pesistent storage object (a pointer to)
--    Name    - The name of an object there
--    Parent  - The parent object
--
-- This function returns the object's class in Storage. Constraint_Error
-- is propagated when Parent does not persist in Storage.
--
-- Returns :
--
--    The object's name in Storage
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or not a persistent object
--    Data_Error       - Inconsistent data base
--    End_Error        - No such object
--
   function Get_Class
            (  Storage : access Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return String is abstract;
--
-- Get_Creation_Time -- Of a persistent object creation
--
--    Storage - A pesistent storage object (a pointer to)
--    Name    - The name of an object there
--    Parent  - The parent object
--
-- This   function   returns   the   object's   creation   time   stamp.
-- Constraint_Error is  propagated  when  Parent  does  not  persist  in
-- Storage.
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
            (  Storage : access Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Time is abstract;
--
-- Get_List -- List of persistent objects
--
--    Storage     - A pesistent storage object (a pointer to)
--    Prefix      - Of names to get (UTF-8 encoded)
--    Suffix      - Of the names (UTF-8 encoded)
--    Equivalence - Of the characters when compared
--    Parent      - The parent object
--
-- This function returns a complete list of all named objects persistent
-- in Storage. The list does not include anonymous  persistent  objects.
-- Only names starting with Prefix and ending with Suffix are  returned.
-- When  names  are compared two characters are considered same if their
-- corresponding values  in  the  mapping  Equivalence  are  same.  When
-- Equivalence   is   null,   identity   mapping   is    assumed.    Use
-- Strings_Edit.UTF8.Mapping.To_Lowercase   for    a    case-insensitive
-- mapping. Prefix and Suffix may not overlap when matched.
--
-- Returns :
--
--    Names of the objects in the data base
--
-- Exceptions :
--
--    Constraint_Error - Parent is not persistent
--    Data_Error       - Inconsistent data base
--
   function Get_List
            (  Storage     : access Storage_Object;
               Prefix      : String := "";
               Suffix      : String := "";
               Equivalence : Unicode_Mapping_Function := null;
               Parent      : Deposit_Handle := Root_Directory
            )  return Catalogue.Set is abstract;
--
-- Get_Name -- Of a persistent object
--
--    Storage - A pesistent storage object (a pointer to)
--    Object  - A handle to
--
-- This function returns the object's name in Storage.  Constraint_Error
-- is propagated when Object does not persist in Storage. When  it  does
-- but is anonymous, then Name_Error is propagated.
--
-- Returns :
--
--    The object's name in Storage
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or not a persistent object
--    Data_Error       - Inconsistent data base
--    Name_Error       - Object is anonymous
--
   function Get_Name
            (  Storage : access Storage_Object;
               Object  : Deposit_Handle
            )  return String is abstract;
--
-- Get_Parent -- Of a persistent object
--
--    Storage - A pesistent storage object (a pointer to)
--    Object  - A handle to
--
-- This function returns the object's parent in Storage.  Named  objects
-- can  have  parents.  In  this case the object name is relative to its
-- parent.  Parents  can be objects of any kind. An object can have only
-- one  parent. No object can be a parent of itself. Constraint_Error is
-- propagated  when Object does not persist in Storage. When it does but
-- is anonymous, then Name_Error is propagated.
--
-- Returns :
--
--    The object's parent in Storage, invalid when Object is a root one
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or not a persistent object
--    Data_Error       - Inconsistent data base
--    Name_Error       - Object is anonymous
--
   function Get_Parent
            (  Storage : access Storage_Object;
               Object  : Deposit_Handle
            )  return Deposit_Handle is abstract;
--
-- Is_Descendant -- Check if an object is a descendant of another
--
--    Storage - A pesistent storage object (a pointer to)
--    Object  - A handle to the object check
--    Parent  - A handle to the alleged parent object
--
-- This  function checks if Object is a direct or indirect descendant of
-- Parent. The result is false if Object is invalid, or  else  specifies
-- an  anonymous  or  non-persisting  in  Storage  object. Otherwise the
-- result is true when Parent is  invalid  (i.e.  identifies  root-level
-- objects) and false when Parent does not persist in Storage.
--
-- Returns :
--
--    True if Object is a descendant of Parent
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--
   function Is_Descendant
            (  Storage : access Storage_Object;
               Object  : Deposit_Handle;
               Parent  : Deposit_Handle
            )  return Boolean is abstract;
--
-- Is_In -- Check if an object is persistent
--
--    Storage       - A pesistent storage object (a pointer to)
--    Name / Object - The name of or a handle to an object there
--  [ Parent ]      - The parent object
--
-- These  functions  check  whether  an  object persists in Storage. The
-- object can be identified either by its name or by  a  handle  to  it.
-- When  Object  is  not  a  valid  handle the result is False. When the
-- parameter Name is used  then  the  parameter  Parent  identifies  the
-- parent object.
--
-- Returns :
--
--    True if the object persists in Storage
--
-- Exceptions :
--
--    Constraint_Error - Non-persistent Parent
--    Data_Error       - Inconsistent data base
--
   function Is_In
            (  Storage : access Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Boolean is abstract;
   function Is_In
            (  Storage : access Storage_Object;
               Object  : Deposit_Handle
            )  return Boolean is abstract;
--
-- Is_Named -- Check if an object has a name
--
--    Storage - A pesistent storage object (a pointer to)
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
--    Data_Error - Inconsistent data base
--
   function Is_Named
            (  Storage : access Storage_Object;
               Object  : Deposit_Handle
            )  return Boolean is abstract;
--
-- On_Error -- Error trace
--
--    Storage - The persistent storage object
--    Text    - Error text (context)
--    Error   - Exception occurrence
--
-- This procedure  is called  upon  fatal  exceptions  which  cannot  be
-- propagated  (e.g. in Finalization).  The default  implementation does
-- nothing. It can be overriden, e.g. to write a trace file.
--
   procedure On_Error
             (  Storage : Storage_Object;
                Text    : String;
                Error   : Exception_Occurrence
             );
--
-- Put -- Make an object persistent
--
--    Storage  - A pesistent storage
--    Object   - A handle to the object to store in Storage
--  [ Name     - The name of the object being stored
--    Parent ] - The parent object
--
-- These procedures are used to store Object in Storage. The  parameters
-- Name and Parent specify the object's  name  and  parent  there.  When
-- omitted  the  object  is  stored  as  anonymous. Anonymous persistent
-- objects  are  collected  when  not   used,   but   not   before   its
-- memory-resident counterpart vanishes. When Object already persists in
-- Storage  and its name is specified, then it is checked to be same. If
-- this check fails, Name is empty or illegal,  or  conflicts  with  the
-- name  of  another  immediate descendant of Parent, then Name_Error is
-- propagated.  It  is  also  propagated  when  Parent is a decendant of
-- Object  or else when Parent is anonymous. When name is not specified,
-- no check is made.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, Parent is not in Storage
--    Data_Error       - Inconsistent data base
--    Name_Error       - Name conflict, illegal or empty name
--
   procedure Put
             (  Storage : in out Storage_Object;
                Object  : in out Deposit_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             )  is abstract;
   procedure Put
             (  Storage : in out Storage_Object;
                Object  : in out Deposit_Handle
             )  is abstract;
--
-- Rename -- A persistent object
--
--    Storage           - A pesistent storage object
--    Old_Name / Object - Object to be renamed
--  [ Old_Parent ]      - The old parent object
--    New_Name          - The new name
--  [ New_Parent ]      - The new parent object
--
-- These procedures change the name of an object specified by either its
-- old name or by a handle to it.  When  renamed  object  was  anonymous
-- before renaming it becomes a named one. When  Object  is  an  invalid
-- handle or does not refer a persistent object then Constraint_Error is
-- propagated. It is also propagated when a parent  is  not  persistent.
-- End_Error is propagated when Old_Name does not refer  any  persistent
-- object.  Name_Error  indicates  an illegal or conflicting name. It is
-- also  propagates when New_Parent is a descendant of object or else is
-- anonymous.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or not a persistent object
--    Data_Error       - Inconsistent storage
--    End_Error        - No such object
--    Name_Error       - Name conflict, there is another object named so
--
   procedure Rename
             (  Storage    : in out Storage_Object;
                Old_Name   : String;
                Old_Parent : Deposit_Handle := Root_Directory;
                New_Name   : String;
                New_Parent : Deposit_Handle := Root_Directory
             )  is abstract;
   procedure Rename
             (  Storage    : in out Storage_Object;
                Object     : in out Deposit_Handle;
                New_Name   : String;
                New_Parent : Deposit_Handle := Root_Directory
             )  is abstract;
--
-- Unname -- A persistent object
--
--    Storage       - A pesistent storage object
--    Name / Object - To be unnamed, a handle to or the name of
--  [ Parent ]      - The parent object
--
-- These  procedures  make  object  anonymous.   Unnamed   objects   are
-- automatically  deleted  when  no  more in use. Nothing happens if the
-- object is already unnamed. Nothing also happens if Object is invalid,
-- not a handle to a persistent object or does not exist. When Parent is
-- not persistent (but valid), then Constraint_Error is propagated. Note
-- that  anonymous  objects  are not deleted as long as they have memory
-- resident counterparts. Observe difference between Unname  and  Delete
-- called on an object handle. Delete requests object deletion from both
-- memory  and  persistent  storage.  Unname  does it for the persistent
-- storage  only.  Both  may  have  no immediate effect if the object is
-- still in use.
--
-- Exceptions :
--
--    Constraint_Error - Parent is valid but not persistent
--    Data_Error       - Inconsistent storage
--
   procedure Unname
             (  Storage : in out Storage_Object;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             )  is abstract;
   procedure Unname
             (  Storage : in out Storage_Object;
                Object  : in out Deposit_Handle
             )  is abstract;

private
   type Storage_Object is abstract new Object.Entity with null record;
--
-- Handles -- Handles to persistent objects
--
   package Handles is
      new Object.Handle (Storage_Object, Storage_Object_Ptr);

   Root_Directory : constant Deposit_Handle :=
      (Deposit_Handles.Handles.Null_Handle with null record);

end Persistent;
