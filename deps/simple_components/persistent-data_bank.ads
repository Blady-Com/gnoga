--                                                                    --
--  package Persistent.Data_Bank    Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2002       --
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
--  This  package  defines  an  interface  to  the  abstract  persistent
--  storages that use keys to index objects stored there. The  primitive
--  operations  defined  in terms of keys can be used for implementation
--  of the operations defined on Storage_Object.
--
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Object.Archived;        use Object.Archived;
with Object.Archived.Lists;  use Object.Archived.Lists;
with Object.Archived.Sets;   use Object.Archived.Sets;

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

package Persistent.Data_Bank is
--
-- Data_Bank_Object -- Represents an external storage
--
   type Data_Bank_Object is abstract
      new Storage_Object with private;
--
-- Persistent_Key -- Identifies persistent object in the storage
--
   type Persistent_Key is abstract
      new Ada.Finalization.Controlled with null record;
   type Persistent_Key_Ptr is access Persistent_Key'Class;
--
-- Image -- Key to string conversion
--
--    Storage - The persistent storage
--    Key     - A persistent key
--
-- Key image unambiguously identifies the key.
--
-- Returns :
--
--    Its image
--
-- Exceptions :
--
--    Constraint_Error - Illegal key for this storage
--
   function Image
            (  Storage : Data_Bank_Object'Class;
               Key     : Persistent_Key
            )  return String is abstract;
--
-- Null_Key -- The null key
--
-- Returns :
--
--    The value that cannot be a valid key
--
   function Null_Key return Persistent_Key is abstract;
--
-- Comparisons -- Keys are comparable
--
   function "<" (Left, Right : Persistent_Key)
      return Boolean is abstract;
   function "=" (Left, Right : Persistent_Key)
      return Boolean is abstract;
--
-- Persistent_Key_Array -- An abstract unbounded array of keys
--
   type Persistent_Key_Array is abstract
      new Ada.Finalization.Limited_Controlled with null record;
--
-- Get -- A key array element
--
--    Container - The array
--    Index     - The index
--
-- Returns :
--
--    The key
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function Get (Container : Persistent_Key_Array; Index : Integer)
      return Persistent_Key'Class is abstract;
--
-- Put -- A key array element
--
--    Container - The array
--    Index     - The index
--    Key       - To put
--
-- The array is expanded as necessary. The gaps (if any) are filled with
-- Null_Key.
--
   procedure Put
             (  Container : in out Persistent_Key_Array;
                Index     : Integer;
                Key       : Persistent_Key'Class
             )  is abstract;
--
-- Sharing_Type -- The access level to the data base
--
   type Sharing_Type is (Fully, Read_Only, Read_Write);
--
-- Access_Mutex -- Persistent storage access mutex (abstract)
--
--    Storage - The persistent storage object to access
--
-- This  type  is  used  as  the  base for storage-specific objects that
-- represent   atomic   actions  on  the  storage,  such  as  data  base
-- transactions. Two concrete types are derived from it.  Read_Mutex  is
-- used for viewing storage content without modification. Write_Mutex is
-- used for full access. They are used as follows:
--
--    declare
--       Transaction : Write_Mutex (DB'Access);
--    begin
--       -- Do something with DB
--       Commit (Transaction);
--    end;
--
-- When Commit has not called on mutex, because of exception propagation
-- for instance, then Roll_Back will be.
--
   type Access_Mutex (Storage : access Data_Bank_Object'Class) is
      abstract new Ada.Finalization.Limited_Controlled with private;
--
-- Commit -- Commit changes in the persistent storage
--
--    Storage - The persistent storage
--
-- This  abstract procedure is called at the end of each transaction: an
-- atomic modification of the persistent storage.  There  should  be  no
-- difference between  Roll_Back  and  Commit  if  the  transaction  was
-- initiated  by  Seize_Read. Normally, Commit is never called directly,
-- but only through Commit of a mutex object.
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    Use_Error  - No transaction active (optional)
--
   procedure Commit (Storage : in out Data_Bank_Object) is abstract;
--
-- Commit -- Called before finalization to commit changes, if any
--
--    Mutex - The transaction mutex
--
-- This procedure calls Commit (Mutex.Storage.all); Commit can be called
-- only once. Multiple commits cause Use_Error propagation.
--
-- Exceptions :
--
--    Use_Error - Multiple commit
--    Any other propagate from Commit called
--
   procedure Commit (Mutex : in out Access_Mutex);
--
-- Delete -- Removes a persistent object by key
--
--    Storage - The persistent storage
--    Key     - The object (a key of)
--
-- An  implementation may proceed from the assumption that all dependent
-- objects  are already deleted and no object refers to the deleted one.
-- Delete  can  called  only  within  a  transaction  following  a  call
-- Seize_Write.  The procedure my not be called on a parent object which
-- still has chidlren or on an object in use. Though  an  implementation
-- need not to check this.
--
-- Exceptions :
--
--    Data_Error - Data base error
--    Use_Error  - No transaction initiated (optional)
--
   procedure Delete
             (  Storage : in out Data_Bank_Object;
                Key     : Persistent_Key'Class
             )  is abstract;
--
-- Finalize -- Destructor
--
--    Mutex - The transaction mutex
--
-- The destructor calls Roll_Back if no Commit was called before
--
   procedure Finalize (Mutex : in out Access_Mutex);
--
-- Find -- A persistent object by name
--
--    Storage - The persistent storage (a pointer to)
--    Name    - Name of
--    Parent  - The parent object to search through
--
-- This  procedure is used to determine the object's key by the object's
-- name and the key to its parent. For the ruut-level objects the parent
-- key is specified as Null_Key. The function is allowed only  within  a
-- transaction initiated either by Seize_Read or Seize_Write.   Null_Key
-- is returned when the object does not exist.
--
-- Returns :
--
--    The key identifying Object in Storage or Null_Key
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    Use_Error  - No transaction initiated (optional)
--
   function Find
            (  Storage : access Data_Bank_Object;
               Name    : String;
               Parent  : Persistent_Key'Class
            )  return Persistent_Key'Class is abstract;
--
-- Get -- A persistent object by key
--
--    Storage - The persistent storage (a pointer to)
--    Key     - Of the object
--
-- This   procedure   restores  a  persistent  object  by  its  key.  An
-- implementation  shall  check  if  the object for the specified key is
-- already memory-resident.  Get  is  allowed  to  call  only  within  a
-- transaction  initiated  either  by  Seize_Read  or  Seize_Write.   An
-- implementation  need not to check that, but if it does then Use_Error
-- should indicate failed check.
--
-- Returns :
--
--    A handle to the object
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    End_Error  - No such object
--    Use_Error  - No transaction initiated (optional)
--
   function Get
            (  Storage : access Data_Bank_Object;
               Key     : Persistent_Key'Class
            )  return Deposit_Handle is abstract;
--
-- Get_Access_Mode -- The current access mode
--
--    Storage - The persistent storage
--
-- Returns :
--
--    Access mode gained by the task on the storage
--
   function Get_Access_Mode (Storage : Data_Bank_Object)
      return Sharing_Type is abstract;
--
-- Get_Children -- Of a persistent object
--
--    Storage  - The persistent storage (a pointer to)
--    Key      - The key of the persistent object
--    Children - An array of keys
--    Pointer  - To place the first child
--
-- Implementation adds the keys of the immediate children of the  object
-- specified by Key into the array Children. The first item is placed at
-- Pointer.  Then  Pointer  is advanced. Get_Children is allowed to call
-- only   within   a  transaction  initiated  either  by  Seize_Read  or
-- Seize_Write. An implementation need not to check that, but if it does
-- then Use_Error should indicate failed check.
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    Use_Error  - No transaction initiated (optional)
--
   procedure Get_Children
             (  Storage  : in out Data_Bank_Object;
                Key      : Persistent_Key'Class;
                Children : in out Persistent_Key_Array'Class;
                Pointer  : in out Integer
             )  is abstract;
--
-- Get_Class -- The class of a persistent object
--
--    Storage - The persistent storage (a pointer to)
--    Key     - The key of the persistent object
--
-- Implementation  returns  the  object's  class stored in Storage under
-- Key.  Get_Class is allowed only within a transaction initiated either
-- by Seize_Read or Seize_Write. It may raise Use_Error otherwise.
--
-- Returns :
--
--    The object's class
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    End_Error  - No such object
--    Use_Error  - No transaction initiated (optional)
--
   function Get_Class
            (  Storage : access Data_Bank_Object;
               Key     : Persistent_Key'Class
            )  return String is abstract;
--
-- Get_Creation_Time -- The class of a persistent object
--
--    Storage - The persistent storage (a pointer to)
--    Key     - The key of the persistent object
--
-- Implementation  returns  the  object's  class stored in Storage under
-- Key. Get_Creation_Time is allowed only within a transaction initiated
-- either   by   Seize_Read  or  Seize_Write.  It  may  raise  Use_Error
-- otherwise.
--
-- Returns :
--
--    The object's creation time
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    End_Error  - No such object
--    Use_Error  - No transaction initiated (optional)
--
   function Get_Creation_Time
            (  Storage : access Data_Bank_Object;
               Key     : Persistent_Key'Class
            )  return Time is abstract;
--
-- Get_Data -- Retrieving description of a persistent object
--
--    Storage    - The persistent storage
--    Key        - The key of the persistent object
--    Class      - Of the object
--    Data       - The object value (string representation)
--    Parameters - Additional parameters
--
-- Implementation returns the object's  description  stored  in  Storage
-- under Key (see Store). The description is used to restore the object.
-- The  output  parameters  are the object's class and data as they were
-- generated by Object.Archived.Store and  internally  used  Parameters,
-- which  describe  the  dependency  list  of the object being restored.
-- Get_Data is allowed only within a  transaction  initiated  either  by
-- Seize_Read or Seize_Write. The implementation is encouraged to  throw
-- Use_Error when called outside any transaction.
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    End_Error  - No such object
--    Use_Error  - No transaction initiated (optional)
--
   procedure Get_Data
             (  Storage    : in out Data_Bank_Object;
                Key        : Persistent_Key'Class;
                Class      : out Unbounded_String;
                Data       : out Unbounded_String;
                Parameters : out Unbounded_String
             )  is abstract;
--
-- Get_Dependant -- Enumeration of dependants
--
--    Storage - The persistent storage (a pointer to)
--    Key     - The object (a key of)
--    No      - The number of a dependant
--
-- This  function  is used to enumerate objects having backward links to
-- the  object  specified  by  Key.  That  are ones which have specified
-- object in the list of backward links (the parameter Backward_Links of
-- Store and Update). All dependants are enumerated starting from 1. The
-- parameter  No  specifies  the  number  of  a  dependant  to  get.  An
-- implementation  is  allowed  to use a cache, so the caller should not
-- undertake any actions which may lead to updating the dependency  list
-- of the object. Get_Dependant is allowed  only  within  a  transaction
-- initiated either by Seize_Read or Seize_Write. It may raise Use_Error
-- otherwise.
--
-- Returns :
--
--    The key of a dependent object
--
-- Exceptions :
--
--    Data_Error - Data base error
--    End_Error  - No dependant found (end of list)
--    Use_Error  - Illegal key, no transaction initiated
--
   function Get_Dependant
            (  Storage : access Data_Bank_Object;
               Key     : Persistent_Key'Class;
               No      : Positive
            )  return Persistent_Key'Class is abstract;
--
-- Get_Name -- The name and parent of a persistent object
--
--    Storage - The persistent storage (a pointer to)
--    Key     - The key of the persistent object
--    Parent  - The object parent's key (a pointer to output)
--
-- Implementation returns the object's name stored in Storage under Key.
-- Get_Name is allowed only within a  transaction  initiated  either  by
-- Seize_Read or Seize_Write. It  may  raise  Use_Error  otherwise.  The
-- parent's key of object is returned via the parameter Parent.
--
-- Returns :
--
--    The object's name
--
-- Exceptions :
--
--    Constraint_Error - Wrong type of Parent requested
--    Data_Error       - Inconsistent data base
--    End_Error        - No such object
--    Name_Error       - Anonymous object
--    Use_Error        - No transaction initiated (optional)
--
   function Get_Name
            (  Storage : access Data_Bank_Object;
               Key     : Persistent_Key'Class;
               Parent  : access Persistent_Key'Class
            )  return String is abstract;
--
-- Get_References -- Of a persistent object
--
--    Storage    - The persistent storage (a pointer to)
--    Key        - The key of the persistent object
--    References - An array of keys
--    Pointer    - To place the first child
--
-- Implementation adds the keys of the objects referenced by the  object
-- specified by Key. The first item is placed at Pointer into the  array
-- References.  Then  Pointer is advanced. The implementation should not
-- to go after the references of references. Get_References  is  allowed
-- to  call  only within a transaction initiated either by Seize_Read or
-- Seize_Write. An implementation need not to check that, but if it does
-- then Use_Error should indicate failed check.
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    Use_Error  - No transaction initiated (optional)
--
   procedure Get_References
             (  Storage    : in out Data_Bank_Object;
                Key        : Persistent_Key'Class;
                References : in out Persistent_Key_Array'Class;
                Pointer    : in out Integer
             )  is abstract;
--
-- Has_Dependants -- Checks if a persistent object has dependants
--
--    Storage   - The persistent storage (a pointer to)
--    Key       - The object (a key of)
--    All_Links - Count all links flag
--
-- An  anonymous  object  that has no dependants can be deleted, but not
-- before its memory-resident counterpart disappears. When key does  not
-- specify any object, the result is False. The parameter All_Links when
-- true  requires  to count all references to the object. Otherwise only
-- direct references are counted. Has_Dependants is allowed only  within
-- a transaction initiated either by Seize_Read or Seize_Write.  It  may
-- raise   Use_Error   otherwise.  An  implementation  shall  not  count
-- references of the object to itself if any.
--
-- Returns :
--
--    True if the object identified by Key has no dependants
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    Use_Error  - No transaction initiated (optional)
--
   function Has_Dependants
            (  Storage   : access Data_Bank_Object;
               Key       : Persistent_Key'Class;
               All_Links : Boolean
            )  return Boolean is abstract;
--
-- Is_In -- Search for an object persistent in the data bank
--
--    Storage - The persistent storage (a pointer to)
--    Key     - Of the object
--
-- This  function  checks  whether  Key  specify an object persistent in
-- Storage. It is allowed only within a transaction initiated either  by
-- Seize_Read or Seize_Write. It may raise Use_Error otherwise.
--
-- Returns :
--
--    True if the object persists in Storage
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    Use_Error  - No transaction initiated (optional)
--
   function Is_In
            (  Storage : access Data_Bank_Object;
               Key     : Persistent_Key'Class
            )  return Boolean is abstract;
--
-- Put -- A persistent object by key
--
--    Storage - The persistent storage
--    Key     - The object key
--    Object  - The object
--
-- This  procedure  updates  a  persistent object by its key. Usually it
-- calls  Update  for  this  purpose.  It  is  allowed  only  within   a
-- transaction   initiated   by  Seize_Write.  It  may  raise  Use_Error
-- otherwise.
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    End_Error  - Key does not identify the object
--    Use_Error  - No transaction initiated (optional)
--
   procedure Put
             (  Storage : in out Data_Bank_Object;
                Key     : Persistent_Key'Class;
                Object  : Deposit'Class
             )  is abstract;
--
-- Rename -- A persistent object
--
--    Storage - The persistent storage
--    Key     - Identifies the object
--    Name    - The new object name (UTF-8 encoded)
--    Parent  - The key to the parent object
--
-- This procedure changes the name of the object  specified  by  Key  to
-- Name. When renamed object was anonymous before renaming it becomes  a
-- named  one.  I.e.  it  will  not  deleted  when  no  more  in use. An
-- implementation  can  proceed  from the assumption that the caller has
-- already  checked for illegal and conflicting names. Rename is allowed
-- only within a transaction initiated either  by  Seize_Write.  It  may
-- raise  Use_Error  otherwise. When Parent is not Null_Key it shall not
-- specify  a  descendant  object  of  the  renamed  object.   Otherwise
-- Name_Error  is  propagated.  This  check  should  be made only if the
-- renamed object was named before, because  anonymous  objects  do  not
-- have children.
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    End_Error  - Wrong key (optional)
--    Name_Error - Name conflict, there is another object named so
--    Use_Error  - No transaction initiated (optional)
--
   procedure Rename
             (  Storage : in out Data_Bank_Object;
                Key     : Persistent_Key'Class;
                Name    : String;
                Parent  : Persistent_Key'Class
             )  is abstract;
--
-- Roll_Back -- Roll back the changes
--
--    Storage - The persistent storage
--
-- This procedure  is  called  when  a  transaction  fails,  due  to  an
-- exception.  It  is  always  called  from  an  exception handler which
-- reraises  the exception. There is no difference between Roll_Back and
-- Commit if the transaction was initiated by Seize_Read.
--
   procedure Roll_Back (Storage : in out Data_Bank_Object) is abstract;
--
-- Seize_{Read|Write} -- Access to the storage
--
--    Storage - The persistent storage
--
-- These  procedures  are  called  to  initiate  a  transaction with the
-- storage. Only one transaction can be active at a time. The the  level
-- of  mutual  exclusion  required  by  the transaction is read-only for
-- Seize_Read  and  read/write  for  Seize_Write.  The  transaction   is
-- finished by either a call to Commit or to Roll_Back.
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    Use_Error  - Nested transactions (consequent Seizes, optional)
--
   procedure Seize_Read
             (  Storage : in out Data_Bank_Object
             )  is abstract;
   procedure Seize_Write
             (  Storage : in out Data_Bank_Object
             )  is abstract;
--
-- Store -- Create a new persistent object in the data bank
--
--    Storage        - The persistent storage (a pointer to)
--  [ Name           - The name the object will have in the storage
--    Parent ]       - The parent object of
--    Class          - The object class
--    Data           - The object value (string representation)
--    Parameters     - Additional parameters
--    Direct_Links   - The set of objects the catalogued one depends on
--    Backward_Links - The set of backward links
--
-- These  functions  are  used  to  write  a persistent object. They are
-- called internally. The parameter  Name  specifies  the  name  of  the
-- object  in  the  storage.  This  should  be  an unique name among the
-- immediate  children  of  the  object specified by the key Parent. The
-- parameter  Parent  is  Null_Key  for  root-level  objects.  When  not
-- specified, the object is anonymous. The  parameter  Data  contains  a
-- string  unambiguously describing the object of the class specified by
-- the    parameter    Class.    It    is    normally   obtained   using
-- Object.Archived.Store.   Parameters   is  used  to  store  additional
-- information   about   links.   The   parameters   Direct_Links    and
-- Backward_Links  define  the  set  of  objects  in  Storage the object
-- depends on. Objects mentioned in the  set  Backward_Links  are  those
-- which can be deleted without deletion of the object itself. The union
-- of  Direct_Links  and  Backward_Links specifies only directly visible
-- dependencies, it is not a closure. An implementation  usually  stores
-- Class and  Data  under  the  name  Name  and  corrects  a  persistent
-- dependency table according to Direct_Links and  Backward_Links.  Note
-- that initially written object is not  referenced.  Store  is  allowed
-- only  within  a  transaction  initiated  by Seize_Write. It may raise
-- Use_Error otherwise.
--
-- Returns :
--
--    Key of the object
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    Name_Error - Name conflict, empty name
--    Use_Error  - No transaction initiated (optional)
--
   function Store
            (  Storage        : access Data_Bank_Object;
               Name           : String;
               Parent         : Persistent_Key'Class;
               Class          : String;
               Data           : String;
               Parameters     : String;
               Direct_Links   : Deposit_Set;
               Backward_Links : Deposit_Set
            )  return Persistent_Key'Class is abstract;
   function Store
            (  Storage        : access Data_Bank_Object;
               Class          : String;
               Data           : String;
               Parameters     : String;
               Direct_Links   : Deposit_Set;
               Backward_Links : Deposit_Set
            )  return Persistent_Key'Class is abstract;
--
-- Unname -- A persistent objects in the data bank
--
--    Storage - The persistent storage
--    Key     - Identifies the object
--
-- This  procedure  makes  the  object  specified  by  Key anonymous. An
-- implementation can  proceed  from  the  assumption  that  the  caller
-- already checked for object existence, its possible collection  rather
-- than renaming and its children to  be  unnamed  as  well.  Unname  is
-- allowed  only  within  a transaction initiated by Seize_Write. It may
-- raise Use_Error otherwise.
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    End_Error  - Wrong key (optional)
--    Use_Error  - No transaction initiated (optional)
--
   procedure Unname
             (  Storage : in out Data_Bank_Object;
                Key     : Persistent_Key'Class
             )  is abstract;
--
-- Update -- A persistent object in the data bank
--
--    Storage        - The persistent storage
--    Key            - The object key
--    Class          - The object class
--    Data           - The object data
--    Parameters     - Additional parameters
--    Direct_Links   - The set of objects the catalogued one depends on
--    Backward_Links - The set of backward links
--
-- This  procedure is used to update a modified persistent object. It is
-- called internally. The parameter Data contains a string unambiguously
-- describing the object of the class specified by the parameter  Class.
-- Parameters  is  used internally to store additional information about
-- links. It  is  normally  obtained  using  Object.Archived.Store.  The
-- parameters Direct_Links and Backward_Links are same as in  Store.  An
-- implementation usually updates Class and Data and corrects persistent
-- dependency  table.  Unname  is  allowed  only  within  a  transaction
-- initiated by Seize_Write. It may raise Use_Error otherwise.
--
-- Exceptions :
--
--    Data_Error - Inconsistent data base
--    End_Error  - Wrong key (optional)
--    Use_Error  - No transaction initiated (optional)
--
   procedure Update
             (  Storage        : in out Data_Bank_Object;
                Key            : Persistent_Key'Class;
                Class          : String;
                Data           : String;
                Parameters     : String;
                Direct_Links   : Deposit_Set;
                Backward_Links : Deposit_Set
             )  is abstract;
--
-- Value -- String to key conversion
--
--    Storage - The persistent storage
--    Key     - Image of a persistent key
--
-- Returns :
--
--    Its value
--
-- Exceptions :
--
--    Data_Error - Invalid key image
--
   function Value
            (  Storage : Data_Bank_Object;
               Key     : String
            )  return Persistent_Key'Class is abstract;
--
-- Read_Mutex -- Read-only access
--
--    Mutex - The transaction mutex
--
   type Read_Mutex is new Access_Mutex with private;
--
-- Initialize -- Constructor
--
--    Mutex - The transaction mutex
--
-- This procedure calls Seize_Read.
--
   procedure Initialize (Mutex : in out Read_Mutex);
--
-- Write_Mutex -- Read/write mutex
--
   type Write_Mutex is new Access_Mutex with private;
--
-- Initialize -- Constructor
--
--    Mutex - The transaction mutex
--
-- This procedure calls Seize_Write.
--
   procedure Initialize (Mutex : in out Write_Mutex);

private
   type Data_Bank_Object is abstract
      new Storage_Object with null record;

   type Access_Mutex (Storage : access Data_Bank_Object'Class) is
      abstract new Ada.Finalization.Limited_Controlled with
   record
      Committed : Boolean := False;
   end record;

   type Read_Mutex  is new Access_Mutex with null record;
   type Write_Mutex is new Access_Mutex with null record;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Persistent_Key'Class,
             Persistent_Key_Ptr
          );
end Persistent.Data_Bank;
