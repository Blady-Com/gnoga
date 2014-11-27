--                                                                    --
--  package Object.Archived.Handle  Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2003       --
--                                                                    --
--                                Last revision :  17:44 09 May 2009  --
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

with Object.Archived.Iterators;  use Object.Archived.Iterators;
with Object.Archived.Sets;       use Object.Archived.Sets;

generic
   type Object_Type is abstract new Deposit with private;
   type Object_Ptr_Type is access Object_Type'Class;
package Object.Archived.Handle is
   pragma Elaborate_Body (Object.Archived.Handle);
   package Handles is new Object.Handle (Object_Type, Object_Ptr_Type);
--
-- Object_Handle -- Handles to objects
--
   type Handle is new Handles.Handle with null record;
--
-- Add -- An object into the container
--
--    Container - The to place the object into
--    Object    - The object to be placed into the list
--    Backward  - The backward link flag
--
-- The object is put into Container. Nothing happens  if  Object  is  an
-- invalid handle.
--
   procedure Add
             (  Container : in out Deposit_Container'Class;
                Object    : Handle;
                Backward  : Boolean := False
             );
--
-- Delete -- An object
--
--    Object - A handle to
--
-- This procedure requests deletion of the object pointed by Object.  As
-- the  result  of  the  operation Object becomes an invalid handle. The
-- object itself is deleted if possible. Nothing happens  if  Object  is
-- not a valid handle.
--
   procedure Delete (Object : in out Handle);
--
-- Get_Class -- Of an object
--
--    Object - A handle to
--
-- This  function  returns  the  class  of Object. The class is a string
-- uniquely describing the object's type. It is analogous to an external
-- type  tag representation. Though, different types of objects may have
-- same class if necessary.
--
-- Returns :
--
--    The object class
--
   function Get_Class (Object : Handle) return String;
--
-- Get_References -- Of an object
--
--    Object    - A handle to
--    Container - To place the references
--
-- This procedure adds to Container references to all objects the object
-- specified  by  the  parameter  Object depends on. No objects added if
-- Object is an invalid handle.
--
   procedure Get_References
             (  Object    : Handle;
                Container : in out Deposit_Container'Class
             );
--
-- Invalidate -- Detach handle from the object
--
--    Object - The handle
--
-- This procedure makes handle pointing to nothing. If it was  the  last
-- reference to the object, the latter is destroyed.
--
   procedure Invalidate (Object : in out Handle);
--
-- Is_Backward -- Check a link to an object from the list
--
--    Container - The object's container
--    Object    - A handle to the object
--
-- Returns :
--
--    True if a backward link used for the object
--
-- Exceptions :
--
--    Contstraint_Error - Object is not in Container or invalid handle
--    Use_Error         - The container does not distinguish direct  and
--                        backward links
--
   function Is_Backward
            (  Container : Deposit_Container'Class;
               Object    : Handle
            )  return Boolean;
--
-- Is_Dependent -- Check if one object depends on other(s)
--
--    Dependant   - A handle to the dependent object
--    Referent(s) - A handle to or list of potential referents
--
-- These functions check whether Dependant refers to Referent  or,  when
-- the parameter is a container, then whether Dependant refers to any of
-- the  objects  from  the  container. The result is False if Dependant,
-- Referent is invalid or Referent is empty.
--
-- Returns :
--
--    True if Dependant depends on Referent(s)
--
   function Is_Dependent
            (  Dependant : Handle;
               Referent  : Handle
            )  return Boolean;
   function Is_Dependent
            (  Dependant : Handle;
               Referents : Deposit_Container'Class
            )  return Boolean;
--
-- Is_In -- Check if an object is in the container
--
--    Container - The container
--    Object    - A handle to the object
--
-- Returns :
--
--    True if Object is Container
--
   function Is_In
            (  Container : Deposit_Container'Class;
               Object    : Handle
            )  return Boolean;
--
-- Is_Valid -- Check if the handle is associated with an object
--
--    Object - The handle
--
-- Returns :
--
--    True if the handle can de dereferenced
--
   function Is_Valid (Object : Handle) return Boolean;
--
-- Ref -- Get handle to an object
--
--    Thing - The object to get a handle from
--
-- Returns :
--
--    Handle to the object
--
   function Ref (Thing : Object_Ptr_Type) return Handle;
--
-- Ref -- Get an object from the container by its index
--
--    Container - The container
--    Index     - The index there 1..
--
-- This function is used  to  enumerate  the  objects  in  a  container.
-- Objects indices start with 1.
--
-- Returns :
--
--    A handle to the object
--
-- Exceptions :
--
--    Contraint_Error - Wrong index or not an Object_Type object
--
   function Ref
            (  Container : Deposit_Container'Class;
               Index     : Positive
            )  return Handle;
--
-- References -- Of an object
--
--    Object - A handle to
--
-- This function is used to query all objects its argument  depends  on.
-- The  result  is a set of objects. It is empty if Object is an invalid
-- handle.
--
-- Returns :
--
--    The set of objects it depends on
--
   function References (Object : Handle) return Deposit_Set;
--
-- Set -- Handle to an object
--
--    Object - A handle to set
--    Thing  - A pointer to the object
--
   procedure Set (Object : in out Handle; Thing : Object_Ptr_Type);

private
   pragma Inline (Add);
   pragma Inline (Delete);
   pragma Inline (Get_Class);
   pragma Inline (Invalidate);
   pragma Inline (Is_Backward);
   pragma Inline (Is_In);
   pragma Inline (Is_Valid);
   pragma Inline (Ptr);
   pragma Inline (Ref);
   pragma Inline (Set);
--
-- Reference_To_Object
--
   type Reference_To_Object is new References_Iterator with record
      Search_For : Handle;
   end record;

   procedure On_Each
             (  Iterator : in out Reference_To_Object;
                Referent : Deposit_Ptr
             );
--
-- Reference_To_Any_Of
--
   type Container_Ptr is access constant Deposit_Container'Class;
   type Reference_To_Any_Of is new References_Iterator with record
      Search_For : Container_Ptr;
   end record;

   procedure On_Each
             (  Iterator : in out Reference_To_Any_Of;
                Referent : Deposit_Ptr
             );

end Object.Archived.Handle;
