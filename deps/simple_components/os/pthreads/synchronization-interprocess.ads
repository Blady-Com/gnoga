--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess                Luebeck            --
--  Interface                                      Summer, 2018       --
--                                                                    --
--                                Last revision :  01:09 01 May 2018  --
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
--  The  parent  package of the child packages providing implementations
--  of various inter-process  synchronization primitives.  This is Linux
--  variant of the package.
--
with Ada.Streams;              use Ada.Streams;
with Interfaces.C.Strings;     use Interfaces;
with System.Storage_Elements;  use System.Storage_Elements;
with System.Storage_Pools;     use System.Storage_Pools;

with Ada.Finalization;
with Synchronization.Pthreads;

package Synchronization.Interprocess is
--
-- Shared_Directory -- The location  of shared memory mapped files.  The
--                     shared  mapping  file  is  constructed  from this
--                     prefix and the name supplied in Create or Open.
--
   Shared_Directory : constant String := "/tmp/";
--
--  Environment is an object handling inter-process syncronization. Each
--  process must have an instance of the environment.  The first process
--  creates  environment   under   a  system-wide  name.  The  following
--  processes connect to the environment using this name.
--
   type Abstract_Shared_Environment is abstract
      new Ada.Finalization.Limited_Controlled with private;
--
-- Abstract_Shared_Object -- Base type of a shared object
--
   type Abstract_Shared_Object is abstract
      new Ada.Finalization.Limited_Controlled with private;
--
-- Create -- Environment
--
--    Shared - The environment object
--    Name   - The unique system-wide name to identify the environment
--
-- The name must be a legal simple file name. The first process must use
-- this call to create and initialize the environment.
--
-- Exceptions :
--
--    Data_Error   - I/O errors
--    Mode_Error   - Invalid configuration of the environment
--    Name_Error   - Invalid name
--    Status_Error - The environment is alreay open
--    Use_Error    - The environment already exists
--
   procedure Create
             (  Shared : in out Abstract_Shared_Environment;
                Name   : String
             );
--
-- Close -- Environment
--
--    Shared - The environment object
--
-- This  procedure  is void if  the system  automatically  collects  the
-- shared environment resource.  Under an OS like  Linux it  can be used
-- to ensure  that the resource,  e.g.  file is  deleted  upon  abnormal
-- completion of the program.  The effect of a call to close can be that
-- no more processes can join using the environment.
--
-- Exceptions :
--
--    Status_Error - The environment is not open
--
   procedure Close (Shared : in out Abstract_Shared_Environment);
--
-- Finalize -- Destruction
--
--    Shared - The environment object
--
-- When the process that created environment destroys the object no more
-- processes can connect to it.  The  processes  already  connected  may
-- continue using it.  The environment  is ultimately destroyed when the
-- last process destroys its object.
--
   procedure Finalize (Shared : in out Abstract_Shared_Environment);
--
-- Get_Offset -- The offset in the shared memory
--
--    Object - The shared object
--
-- Returns :
--
--    The offset to the object data in the shared memory
--
   function Get_Offset
            (  Object : Abstract_Shared_Object
            )  return Storage_Offset;
--
-- Get_Signature -- The signature of the object, used for checking
--
--    Object - The shared object
--
-- The signature  is used  in order to verify  if the shared environment
-- contains  same objects in all instances.  The default  implementation
-- calculates the signature from the external tag name.
--
-- Returns :
--
--    The signature
--
   function Get_Signature
            (  Object : Abstract_Shared_Object
            )  return Unsigned_16;
--
-- Get_Size -- The amount of shared memory required by the object
--
--    Object - The shared object
--
-- Returns :
--
--    Memory in storage elements
--
   function Get_Size
            (  Object : Abstract_Shared_Object
            )  return Storage_Count is abstract;
--
-- Get_Size -- The amount of shared memory used by the environment
--
-- Returns :
--
--    Memory in storage elements
--
   function Get_Size
            (  Shared : Abstract_Shared_Environment
            )  return Storage_Count;
--
-- Initialize -- Construction
--
--    Shared - The environment object
--
-- The environment is first usable after Create or Open.
--
   procedure Initialize (Shared : in out Abstract_Shared_Environment);
--
-- Map -- Object mapping notification
--
--    Object   - The object
--    Shared   - The shared environment holding the object
--    Location - The address assigned to the object in the shared memory
--    Size     - Of the object as reported by Get_Size
--    Owner    - The shared memory is owned by the process
--
-- This  procedure  is called  once upon  shared memory  is mapped.  The
-- parameter Owner is true when the process created the memory map. Thus
-- if the  shared object requires  initialization  done once,  this is a
-- hint when  to perform  initialization.  After  returning from Map the
-- object must become fully operational.
--
-- Exceptions :
--
--    Mode_Error - Invalid configuration of the environment
--
   procedure Map
             (  Object   : in out Abstract_Shared_Object;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is abstract;
--
-- Open -- Connect to an existing environment
--
--    Shared - The environment object
--    Name   - The name
--    Create - Create if does not exist
--
-- Exceptions :
--
--    Data_Error   - I/O errors
--    Mode_Error   - Invalid configuration of the environment
--    Name_Error   - Invalid name
--    Status_Error - The environment is alreay open
--    Use_Error    - The environment does not exist (Create is False)
--
   procedure Open
             (  Shared : in out Abstract_Shared_Environment;
                Name   : String;
                Create : Boolean := False
             );
--
-- Start -- Object start notification
--
--    Object - The object
--    Shared - The shared environment holding the object
--    Owner  - The shared memory is owned by the process
--
-- This  procedure  is called after all objects are mapped.  The default
-- implementation does nothing.
--
-- Exceptions :
--
--    Mode_Error - Invalid configuration of the environment
--
   procedure Start
             (  Object : in out Abstract_Shared_Object;
                Shared : in out Abstract_Shared_Environment'Class;
                Owner  : Boolean
             );
--
-- Unmap -- The object
--
--    Object - The object
--    Owner  - The shared memory is owned by the process
--
-- This procedure  is reverse  to Map.  The default  implementation does
-- nothing.
--
   procedure Unmap
             (  Object : in out Abstract_Shared_Object;
                Shared : in out Abstract_Shared_Environment'Class;
                Owner  : Boolean
             );
--
-- Generic_Map -- Mapping and initializing objects
--
--    Location - The address of the object
--    Owner    - True if the object is to be initialized
--
-- Returns :
--
--    Pointer to the object
--
   generic
      type Object_Type is limited private;
   package Generic_Memory_Mapper is
      type Object_Type_Ptr is access all Object_Type;
      function Map
               (  Location : System.Address;
                  Owner    : Boolean
               )  return Object_Type_Ptr;
   end Generic_Memory_Mapper;
--
-- Get_Signature -- From a string
--
--    Data - To compute signature
--
-- Returns :
--
--    The signature
--
   function Get_Signature (Data : String) return Unsigned_16;
--
-- Process_ID -- The process ID
--
   subtype Process_ID is Synchronization.Pthreads.pid_t;
   Null_Process : constant Process_ID := 0;
--
-- Get_Process_ID -- Get process ID
--
-- Returns :
--
--    The process ID
--
   function Get_Process_ID return Process_ID
      renames Synchronization.Pthreads.getpid;

private
   use Interfaces.C;
   use Interfaces.C.Strings;
   use Synchronization.Pthreads;
   use System;

   type Abstract_Shared_Object_Ptr is
      access all Abstract_Shared_Object'Class;
   type Abstract_Shared_Object is abstract
      new Ada.Finalization.Limited_Controlled with
   record
      Self        : Abstract_Shared_Object_Ptr :=
                    Abstract_Shared_Object'Unchecked_Access;
      Next        : Abstract_Shared_Object_Ptr;
      Shared_Size : Storage_Count  := 0;
      Offset      : Storage_Offset := 0;
   end record;
--
-- Enumerate -- Fake stream I/O procedure
--
-- This procedure  is used internally in order to enumerate the contents
-- of the record type, a descendant of Connection.  The elements  of the
-- record  type derived  from Data_Item are  ones which will be fed with
-- data received from the socket.
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Object : Abstract_Shared_Object
             );
   for Abstract_Shared_Object'Write use Enumerate;

   type Head is record
      Size        : Storage_Count;       -- Size of the mapping
      Checksum    : Unsigned_32;         -- Of the contents
      Initialized : Boolean;
      Owner       : pid_t;               -- The owner process ID
      Open_Count  : aliased Unsigned_32; -- Number of participants
   end record;
   type Head_Ptr is access all Head;

   type Abstract_Shared_Environment is abstract
      new Ada.Finalization.Limited_Controlled with
   record
      File  : int           := -1;        -- FD of mapping
      Owner : Boolean       := False;     -- Created here
      Name  : chars_ptr     := Null_Ptr;  -- File mapping name
      Map   : Head_Ptr;                   -- Address of the memory
      First : Abstract_Shared_Object_Ptr; -- The first object inside
   end record;
   procedure Write
             (  Stream      : access Root_Stream_Type'Class;
                Environment : Abstract_Shared_Environment
             );
   for Abstract_Shared_Environment'Write use Write;

   type Walker is new Root_Stream_Type with record
      Sum_1    : Unsigned_32   := 0; -- Checksum accumulators
      Sum_2    : Unsigned_32   := 0;
      Position : Unsigned_32   := 0;
      Size     : Storage_Count := 0;         -- The memory map size
      First    : Abstract_Shared_Object_Ptr; -- First object inside
      Last     : Abstract_Shared_Object_Ptr; -- First object inside
   end record;
   procedure Read
             (  Stream : in out Walker;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
    procedure Write
              (  Stream : in out Walker;
                 Item   : in Stream_Element_Array
              );
--
-- Round -- To the memory marging
--
--    Offset - The memory offset
--
   function Round (Offset : Storage_Count) return Storage_Count;
--
-- Memory_Mapper -- Initialization of external objects
--
   type Memory_Mapper is new Root_Storage_Pool with record
      Location : Address;
   end record;
   procedure Allocate
             (  Pool            : in out Memory_Mapper;
                Storage_Address : out Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             );
   procedure Deallocate
             (  Pool            : in out Memory_Mapper;
                Storage_Address : Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             );
   function Storage_Size (Pool : Memory_Mapper) return Storage_Count;

   function Add_And_Fetch
            (  Target : access Unsigned_32;
               Value  : Unsigned_32 := 1
            )  return Unsigned_32;
   pragma Import
          (  Intrinsic,
             Add_And_Fetch,
             "__sync_add_and_fetch_4"
          );
   function Compare_And_Swap
            (  Target  : access short;
               Old_Val : short;
               New_Val : short
            )  return Boolean;
   pragma Import
          (  Intrinsic,
             Compare_And_Swap,
             "__sync_bool_compare_and_swap_2"
          );
   function Sub_And_Fetch
            (  Target : access Unsigned_32;
               Value  : Unsigned_32 := 1
            )  return Unsigned_32;
   pragma Import
          (  Intrinsic,
             Sub_And_Fetch,
             "__sync_sub_and_fetch_4"
          );
end Synchronization.Interprocess;
