--                                                                    --
--  package Object.Archived         Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2003       --
--                                                                    --
--                                Last revision :  12:40 06 Nov 2010  --
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
--  This  package  defines  the  abstract  base  type  Deposit  for  all
--  potentially archived objects. Because using Ada.Streams with limited
--  types presently faces difficulties, a factory pattern is implemented
--  here  on  an independent basis.
--
--  The types derived from Deposit should:
--
--  (o)  Override the procedure Store which will be used to get a string
--       description of an object;
--
--  (o)  Define a procedure with  the  profile  of  Restore  which  will
--       create objects from string description;
--
--  (o)  Register the restore procedure as a new class.  This  could  be
--       done during elaboration of the package deriving the  type  from
--       Deposit;
--
--  (o)  Override Get_Class to return the object class for all object of
--       the type;
--
--  (o)  Optionally, override Get_Referents if  the  object  depends  on
--       other objects which have to be then stored/restored with it.
--
--  Objects  may depend on other objects. Any object can be converted to
--  a string of characters and restored  from  the  string.  String  was
--  chosen instead of Stream_Element_Array to make  it  portable  across
--  different systems.
--
--  The following procedure is used to store an object:
--
--  (1)  Get_Referents is called. Each  object  it  refers  is  archived
--       first.  The  order  of the objects in the list is important and
--       has to be preserved;
--
--  (2)  Get_Class is called and its result is archived;
--
--  (3)  Store is called and its result is finally archived.
--
-- When restored:
--
--  (1)  A list of objects the archived object depends on is built;
--
--  (2)  The object's class string is obtained;
--
--  (3)  Restore is finally called with these parameters.
--
with Ada.Unchecked_Deallocation;
with Object.Handle;
with Tables;

package Object.Archived is
--
-- Deposit -- The abstract base type
--
-- The objects derived from Deposit supporting  garbage  collection  via
-- reference counting and store / restore protocol for archiving.
--
   type Deposit is abstract new Entity with private;
   type Deposit_Ptr is access Deposit'Class;
--
-- Backward_Link -- To an object
--
-- This abstract type is used when it is necessary to  monitor  deletion
-- of an object. The garbage collection  prevents  deletion  of  objects
-- being  used. But sometimes it is necessary to break the dependency of
-- one object from another to delete the latter.  For  this  the  former
-- object may become a notice about a desire to delete  a  referent.  It
-- invalidates the handle to the referent and so allows the collector to
-- delete it.
--
   type Backward_Link is abstract new Entity with private;
   type Backward_Link_Ptr is access Backward_Link'Class;
--
-- Deposit_Container -- A set of objects
--
-- All  objects  in the container are enumerated by from 1. The index is
-- Positive.  In  some  containers  (bags)  the  same  object can occupy
-- several slots. In such containers a list can be stored as a  sequence
-- of objects, where objects may repeat. Other containers may have  only
-- one slot per object.
--
   type Deposit_Container is abstract
      new Ada.Finalization.Controlled with private;
--
-- Add -- An object into the container
--
--    Container - The to place the object into
--    Object    - The object to be placed into the list (a pointer to)
--    Backward  - The backward link flag
--
-- The  object  is  put into Container. The implementation should ensure
-- that Object will not be destroyed  until  it  is  in.  The  parameter
-- Backward, when True indicates a backward  link.  Backward  links  are
-- used  when  the  dependent  object  associated with the container can
-- survive deletion of Object. It is an optional parameter which  may be
-- ignored by some implementations. Nothing happens if Object is null.
--
   procedure Add
             (  Container : in out Deposit_Container;
                Object    : Deposit_Ptr;
                Backward  : Boolean := False
             )  is abstract;
--
-- Attach -- Link an object
--
--    Link   - The backward link
--    Object - The object to link
--
-- Link is placed at the end of Object's delivery list. If it is already
-- in  another list then it is removed from there first. Nothing happens
-- if Object is null.
--
   procedure Attach
             (  Link   : Backward_Link_Ptr;
                Object : Deposit_Ptr
             );
--
-- Close -- Notification about object deletion
--
--    Object - The object
--
-- This  procedure is called before finalization of an object. It has to
-- be called from Finalize:
--
--    procedure Finalize (Object : in out Derived) is
--    begin
--       Close (Object);
--       <finalization of Derived>
--       Finalize (Deposit (Object));
--    end Finalize;
--
-- Close cleans the list of backward  links.  It  is  safe  to  call  it
-- multiple  times,  though  it is essential to call it before any vital
-- object data get finalized. So it  plays  the  role  of  a  class-wide
-- destructor.
--
   procedure Close (Object : in out Deposit'Class);
--
-- Create -- An object from a string
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Class   - Of the object being restored
--    List    - The list of objects the restored object depends on
--
-- This procedure calls Restore for Class  (see  Restore),  simulating a
-- dispatching  call.  Name_Error  is  propagated  if  Class  is  not  a
-- registered object class.  Pointer is advanced  to  the  first  object
-- following from the used ones.
--
-- Returns :
--
--    Pointer to the (newly allocated) object
--
-- Exceptions:
--
--    Data_Error   - Syntax error
--    End_Error    - Nothing matched
--    Layout_Error - Pointer not in Source'First..Source'Last + 1
--    Name_Error   - Class is not a registered class
--    Use_Error    - Insufficient dependencies list
--
   procedure Create
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Object  : out Deposit_Ptr
             );
--
-- Deleted -- Notification about future object deletion
--
--    Link  - From the object requested for delete
--    Temps - The list of temporal objects
--
-- This procedure is used when an object is  requested  to  be  deleted.
-- Deleted is called as a result of object deletion request via call  to
-- Delete.  The  parameter  Temps  is  the  list of temporal objects the
-- implementation  might  create.  For  example,  some  objects might be
-- created  to  be  notified in the course of the operation performed by
-- the  caller.  Note  that  the caller should hold a handle to Link, to
-- allow  the  callee to undertake actions which would otherwise lead to
-- Link deletion. Note also that object's finalization does not cause  a
-- call to Delete it calls Destroyed instead.
--
   procedure Deleted
             (  Link  : in out Backward_Link;
                Temps : in out Deposit_Container'Class
             )  is abstract;
--
-- Delete -- Notification about future object deletion
--
--    Object - The object
--
-- This procedure is used when an object is being deleted. On each  item
-- in the Object's obituary notices delivery list Delete is called. This
-- has  the  effect  that some references to Object may disappear and so
-- the  object  will  be  collected. Note that a call to Delete does not
-- guaranty  object's deletion, because some references to it, may still
-- be present. It is safe to add new  backward  links  to  the  object's
-- notification list from Delete, because the items are  appended.  This
-- also means that they will receive a Deleted callback in the course of
-- the same notification.
--
-- Effect :
--
--    Object might get deleted  as  the  result.  To  prevent  undefined
--    behavior have a handle to Object when call Delete.
--
   procedure Delete (Object : in out Deposit'Class);
--
-- Destroyed -- Notification object deletion
--
--    Link - From the object being destroyed
--
-- This procedure is used when an object is destroyed. It is called when
-- object  is  fully operable so an implementation may safely access it.
-- The caller should hold a handle to Link.
--
   procedure Destroyed (Link : in out Backward_Link) is abstract;
--
-- Detach -- Unlink an object
--
--    Link - To be detached
--
   procedure Detach (Link : in out Backward_Link);
--
-- Erase -- Remove all objects from a container
--
--    Container - The container
--
-- When Container holds the last reference  to  an  object,  the  object
-- itself is deleted.
--
   procedure Erase (Container : in out Deposit_Container) is abstract;
--
-- Finalize -- Destruction
--
--    Object - The object being destructed
--
-- Upon finalization backward links  list  is  cleaned.  All  interested
-- parties receive a notification via call to Destroyed.
--
   procedure Finalize (Object : in out Deposit);
--
-- Finalize -- Destruction
--
--    Link - The lin destructed
--
-- Link is removed for object's delivery list.
--
   procedure Finalize (Link : in out Backward_Link);
--
-- Free -- Delete object
--
--    Object - The object (a pointer to)
--
   procedure Free (Object : in out Deposit_Ptr);
--
-- Get -- An object from the container by its index
--
--    Container - The container
--    Index     - The index there 1..
--
-- This function is used  to  enumerate  the  objects  in  a  container.
-- Objects indices start with 1.
--
-- Returns :
--
--    Pointer to object
--
-- Exceptions :
--
--    Contraint_Error - No such object (wrong index)
--
   function Get
            (  Container : Deposit_Container;
               Index     : Positive
            )  return Deposit_Ptr is abstract;
--
-- Get_Class -- Of an object
--
--    Object - The object
--
-- This  function  returns  the  class  of Object. The class is a string
-- uniquely  describing  the  object's type. It is analogous to external
-- type  tag representation. Though, different types of objects may have
-- same class if necessary.
--
-- Returns :
--
--    The object class
--
   function Get_Class (Object : Deposit) return String is abstract;
--
-- Get_Referents -- Objects referred to
--
--    Object    - The object
--    Container - The object container to put referents into
--
-- This  procedure  adds  to Container objects, the given object depends
-- on. Only immediately viewed objects are stored there. No deep  search
-- has  to  be  made  to  detect  all  objects. Objects shall not depend
-- recursively.  The default implementation does nothing, which behavior
-- corresponds to an independent object.
--
-- Exceptions :
--
--    Use_Error - Wrong object
--
   procedure Get_Referents
             (  Object    : Deposit;
                Container : in out Deposit_Container'Class
             );
--
-- Get_Size -- Of a container
--
--    Container - The object's container
--
-- Returns :
--
--    The largest possible index allowed in Get or 0
--
   function Get_Size (Container : Deposit_Container)
      return Natural is abstract;
--
-- Is_Backward -- Check a link to an object from the list
--
--    Container - The object's container
--    Object    - The object
--
-- Returns :
--
--    True if a backward link used for the object
--
-- Exceptions :
--
--    Contstraint_Error - Object is not in Container
--    Use_Error         - The container does not distinguish direct  and
--                        backward links
--
   function Is_Backward
            (  Container : Deposit_Container;
               Object    : Deposit_Ptr
            )  return Boolean is abstract;
--
-- Is_Empty -- Test if the container is empty
--
--    Container - The container
--
-- Returns :
--
--    True if it has no items
--
   function Is_Empty (Container : Deposit_Container'Class)
      return Boolean;
--
-- Is_In -- Check if an object is in the container
--
--    Container - The container
--    Object    - The object
--
-- Null is in no container.
--
-- Returns :
--
--    True if Object is Container
--
   function Is_In
            (  Container : Deposit_Container;
               Object    : Deposit_Ptr
            )  return Boolean is abstract;
--
-- Is_Modified -- Get the modification flag
--
--    Object - The object
--
-- This function is used to check  is  the  object  state  was  changed.
-- Archived  objects  serving  as  proxies  to a persistent storage will
-- require synchronization if this function returns True.
--
-- Returns :
--
--    True if the object state was changed
--
   function Is_Modified (Object : Deposit) return Boolean is abstract;
--
-- Is_Registered -- Check if a class of objects is registerd
--
--    Class - The new class of objects
--
-- Returns :
--
--    True if Class is already registered
--
   function Is_Registered (Class : String) return Boolean;
--
-- Reset_Modified -- Reset the modification flag
--
--    Object - The object
--
-- This  procedure  is used to reset the object state modification flag.
-- It  is  called  immediately after synchronization the object with the
-- persistent storage.
--
   procedure Reset_Modified (Object : in out Deposit) is abstract;
--
-- Restore -- An object from a string (a pointer to)
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Class   - Of the object being restored
--    List    - The list of objects the restored object depends on
--    Object  - The result
--
-- This  procedure  is  called to construct a new object from its string
-- representation.  It  parses  Source  starting  from Source (Pointer).
-- Pointer is advanced to the first  character  following  the  object's
-- description  in  the  string.  The  procedure  has  to be dispatching
-- depending on the object's class. Which is impossible in Ada. For this
-- reason  it  is  defined  as  an access to procedure type. Each object
-- class  has  to  define  such  a  function  and   register   it   (see
-- Register_Class). The parameter Class contains the actual object class
-- according to which dispatch to an implementation of Restore was made.
-- The parameter  List  contains  the  references  to  the  objects  the
-- restored  object  depends on. The order of the objects in the list is
-- same as  one  returned  in  Get_Referents.  The  result  is  a  newly
-- allocated object pointed by the Object parameter.
--
-- Exceptions:
--
--    Data_Error   - Syntax error
--    End_Error    - Nothing matched
--    Layout_Error - Pointer not in Source'First..Source'Last + 1
--    Use_Error    - Insufficient dependencies list
--
   type Restore is access procedure
        (  Source  : String;
           Pointer : in out Integer;
           Class   : String;
           List    : Deposit_Container'Class;
           Object  : out Deposit_Ptr
        );
--
-- Register_Class -- Of objects
--
--    Class       - The new class of objects
--    Constructor - The function used to restore objects (a pointer to)
--
-- This procedure is used for each new class of objects to register  it.
-- It is analogous to creating a dispatching table. It is  necessary  to
-- register a class to make Restore functions working.  Nothing  happens
-- if  the  class  is  already  registered  and  has  same  constructor.
-- Name_Error  is  propagated  when class is registered with a different
-- constructor.
--
-- Exceptions :
--
--    Name_Error - Duplicated class name
--
   procedure Register_Class
             (  Class       : String;
                Constructor : Restore
             );
--
-- Self -- Get pointer to the link object
--
--    Link - A backward link object
--
-- This  function returns a pointer to the link object.
--
-- Returns :
--
--    Pointer to the link object
--
-- Exceptions :
--
--    Constraint_Error - Link is not bound to any object
--
   function Self (Link : Backward_Link) return Backward_Link_Ptr;
--
-- Store -- Store a string describing an object
--
--    Destination - The string to place object description into
--    Pointer     - The current position in the string
--    Object      - The object
--
-- A string describing Object is placed into Destination  starting  from
-- the position specified by Pointer. Pointer is advanced  to  the  next
-- position following the output.
--
-- Exceptions :
--
--    Layout_Error - Pointer  not in Source'First..Source'Last + 1 or no
--                   room for output
--    Use_Error    - Wrong object to store
--
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Object      : Deposit
             )  is abstract;
--
-- This -- Get pointer to the object
--
--    Link - A backward link object
--
-- This  function returns a pointer to the target of Link.
--
-- Returns :
--
--    Pointer to the object
--
-- Exceptions :
--
--    Constraint_Error - Link is not bound to any object
--
   function This (Link : Backward_Link) return Deposit_Ptr;
--
-- To_Object_Ptr -- Identity conversion of pointers
--
--    Link - A backward link object
--
-- Returns :
--
--    The argument
--
   function To_Object_Ptr (Link : Backward_Link_Ptr)
      return Backward_Link_Ptr;
--
-- < -- Ordering pointers
--
--    Left, Right - Pointers to compare
--
-- Returns :
--
--    An order that can be used to create sets of links
--
   function "<" (Left, Right : Backward_Link_Ptr) return Boolean;

private
   pragma Inline (Is_Empty);
   pragma Inline (Self);
   pragma Inline (This);
   pragma Inline (To_Object_Ptr);
   pragma Inline ("<");
--
-- Deposit_Container -- Abstract container has null implementation
--
   type Deposit_Container is abstract
      new Ada.Finalization.Controlled with null record;
--
-- Deposit -- Implementation
--
-- The  field  Delivery  is the head of the list of backward links to to
-- the object.
--
   type Deposit is abstract new Object.Entity with record
      Delivery : Backward_Link_Ptr;
   end record;
--
-- Backward_Link -- Implementation
--
--    Object - The object linked
--    Prev   - The previous item in the chain
--    Next   - The next item
--
   type Backward_Link is abstract new Object.Entity with record
      Object : Deposit_Ptr;
      Prev   : Backward_Link_Ptr;
      Next   : Backward_Link_Ptr;
   end record;
--
-- Free -- Implementation
--
   procedure Free_Deposit is
      new Ada.Unchecked_Deallocation (Deposit'Class, Deposit_Ptr);
   procedure Free (Object : in out Deposit_Ptr) renames Free_Deposit;
--
-- Dispatch_Tables -- Mappings from string to Restore constructor
--
   package Dispatch_Tables is new Tables (Restore);
--
-- Dispatch_Table -- The table of registered classes
--
   Dispatch_Table : Dispatch_Tables.Table;
--
-- Handles -- For private use
--
   package Handles is new Object.Handle (Deposit, Deposit_Ptr);
--
-- Walker_Stub -- A backward link used as a stub while walking a list of
--                backward links
--
   type Walker_Stub is new Backward_Link with null record;
   procedure Deleted
             (  Link  : in out Walker_Stub;
                Temps : in out Deposit_Container'Class
             );
   procedure Destroyed (Link : in out Walker_Stub);
end Object.Archived;
