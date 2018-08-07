--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Pthreads                    Luebeck            --
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
--  This package  provides  binding to some  UNIX   functions  useful to
--  communicate between processes using POSIX threads.  The main targets
--  intended  are   OS X  and  Free  BSD.  Note  that  it  is  certainly
--  incompatible with Linux. For Linux there is an implementation  which
--  is specifically designed for it and is more efficient too.
--
with Ada.Exceptions;           use Ada.Exceptions;
with Interfaces;               use Interfaces;
with Interfaces.C;             use Interfaces.C;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;

with Ada.Finalization;

package Synchronization.Pthreads is

   MAP_FAILED : constant Address :=
                         Storage_Elements.To_Address (-1);

   type Protection is new int range 0..7;
   PROT_READ  : constant Protection := 16#04#; -- Pages can be read
   PROT_WRITE : constant Protection := 16#02#; -- Pages can be written
   PROT_EXEC  : constant Protection := 16#01#; -- Pages can be executed

   type Map_Flags is new int;
   -- Sharing types (must choose one and only one of these)
   MAP_SHARED     : constant Map_Flags := 16#01#; -- Share changes
   MAP_PRIVATE    : constant Map_Flags := 16#02#; -- Changes are private

   -- Other flags
   MAP_FIXED      : constant Map_Flags := 16#10#;   -- Addr exactly
   MAP_FILE	  : constant Map_Flags := 16#00#;
   MAP_ANONYMOUS  : constant Map_Flags := 16#0800#; -- Don't use a file
   MAP_ANON       : constant Map_Flags := MAP_ANONYMOUS;
   MAP_RENAME     : constant Map_Flags := MAP_ANONYMOUS;

   type off_t is new long;

   type Open_Flags is new int;
   O_RDONLY   : constant Open_Flags := 16#0000#; -- reading only
   O_WRONLY   : constant Open_Flags := 16#0001#; -- writing only
   O_RDWR     : constant Open_Flags := 16#0002#; -- reading and writing
   O_NONBLOCK : constant Open_Flags := 16#0004#; -- no delay
   O_APPEND   : constant Open_Flags := 16#0008#; -- set append mode
   O_CREAT    : constant Open_Flags := 16#0200#; -- create if new
   O_TRUNC    : constant Open_Flags := 16#0400#; -- truncate
   O_EXCL     : constant Open_Flags := 16#0800#; -- error if exists

   type mode_t is new unsigned_short;
   S_IRWXU : constant mode_t := 8#700#;
   S_IRUSR : constant mode_t := 8#400#;
   S_IWUSR : constant mode_t := 8#200#;
   S_IXUSR : constant mode_t := 8#100#;
   S_IRWXG : constant mode_t := 8#070#;
   S_IRGRP : constant mode_t := 8#040#;
   S_IWGRP : constant mode_t := 8#020#;
   S_IXGRP : constant mode_t := 8#010#;
   S_IRWXO : constant mode_t := 8#007#;
   S_IROTH : constant mode_t := 8#004#;
   S_IWOTH : constant mode_t := 8#002#;
   S_IXOTH : constant mode_t := 8#001#;

   TIME_UTC : constant := 1;

   PTHREAD_MUTEX_T_Size     : constant := 64 * 8; -- Bits, Ada size
   PTHREAD_MUTEXATTR_T_Size : constant := 16 * 8;
   PTHREAD_COND_T_Size      : constant := 48 * 8;
   PTHREAD_CONDATTR_T_Size  : constant := 16 * 8;

   type pthread_cond_data is
      array (1..(PTHREAD_COND_T_Size + long'Size - 1) / long'Size)
         of long;
   pragma Convention (C, pthread_cond_data);
   type pthread_cond_t is record
      Data : pthread_cond_data;
   end record;
   pragma Convention (C, pthread_cond_t);

   type pthread_condattr_data is
      array (1..(PTHREAD_CONDATTR_T_Size + long'Size - 1) / long'Size)
         of long;
   pragma Convention (C, pthread_condattr_data);
   type pthread_condattr_t is record
      Data : pthread_condattr_data;
   end record;
   pragma Convention (C, pthread_condattr_t);

   type pthread_mutex_data is
      array (1..(PTHREAD_MUTEX_T_Size + long'Size - 1) / long'Size)
         of long;
   pragma Convention (C, pthread_mutex_data);
   type pthread_mutex_t is record
      Data : pthread_mutex_data;
   end record;
   pragma Convention (C, pthread_mutex_t);

   type pthread_mutexattr_data is
      array (1..(PTHREAD_MUTEXATTR_T_Size + long'Size - 1) / long'Size)
         of long;
   pragma Convention (C, pthread_mutexattr_data);
   type pthread_mutexattr_t is record
      Data : pthread_mutexattr_data;
   end record;
   pragma Convention (C, pthread_mutexattr_t);

   type pid_t is new int;
   type pthread_t is new unsigned_long;

   type timespec is record
      tv_sec  : long; -- seconds
      tv_nsec : long; -- nanoseconds
   end record;
   pragma Convention (C, timespec);

   type timeval is record
      tv_sec  : long; -- seconds
      tv_usec : int;  -- microseconds
   end record;
   pragma Convention (C, timeval);

   function "<"  (Left, Right : timespec) return Boolean;
   function "<=" (Left, Right : timespec) return Boolean;
   function From_Duration
            (  Value    : Duration;
               Absolute : Boolean
            )  return timespec;

   function close (File : int) return int;
   function errno return int;
   function ftruncate (File : int; Length : off_t) return int;
   function getpagesize return int;
   function getpid return pid_t;
   function gettimeofday
            (  TP  : access timeval;
               TZP : System.Address := System.Null_Address
            )  return int;
   function open
            (  Path_Mame : char_array;
               Flags     : Open_Flags;
               Mode      : mode_t := S_IRWXU
            )  return int;
   function open
            (  Path_Mame : chars_ptr;
               Flags     : Open_Flags;
               Mode      : mode_t := S_IRWXU
            )  return int;

   function mkfifo
            (  pathname : char_array;
               mode     : mode_t := 8#666#
            )  return int;
   function mmap
            (  Addr   : Address    := Null_Address;
               Length : size_t;
               Prot   : Protection := PROT_READ + PROT_WRITE;
               Flags  : Map_Flags  := MAP_SHARED;
               FD     : int;
               Offset : off_t      := 0
            )  return Address;
   function mremap
            (  Old_Address : Address;
               Old_Size    : size_t;
               New_Size    : size_t;
               Flags       : Map_Flags
            )  return Address;
   function munmap (Addr : Address; Length : size_t) return int;

   function mkostemp
            (  Template : access char;
               Flags    : Open_Flags
            )  return int;
   function pthread_cond_broadcast
            (  Cond : access pthread_cond_t
            )  return int;
   function pthread_cond_destroy
            (  Cond : access pthread_cond_t
            )  return int;
   function pthread_cond_init
            (  Cond : access pthread_cond_t;
               Attr : pthread_condattr_t
            )  return int;
   function pthread_cond_timedwait
            (  Cond    : access pthread_cond_t;
               Mutex   : access pthread_mutex_t;
               AbsTime : timespec
            )  return int;
   function pthread_condattr_destroy
            (  Attr : access pthread_condattr_t
            )  return int;
   function pthread_condattr_init
            (  Attr : access pthread_condattr_t
            )  return int;
   function pthread_condattr_setpshared
            (  Attr    : access pthread_condattr_t;
               Pshared : int
            )  return int;
   function pthread_mutex_destroy
            (  Mutex : access pthread_mutex_t
            )  return int;
   function pthread_mutex_init
            (  Mutex     : access pthread_mutex_t;
               Attribute : pthread_mutexattr_t
            )  return int;
   function pthread_mutex_lock
            (  Mutex : access pthread_mutex_t
            )  return int;
   function pthread_mutex_timedlock
            (  Mutex   : access pthread_mutex_t;
               Timeout : timespec
            )  return int;
   function pthread_mutex_trylock
            (  Mutex : access pthread_mutex_t
            )  return int;
   function pthread_mutex_unlock
            (  Mutex : access pthread_mutex_t
            )  return int;
   function pthread_mutexattr_destroy
            (  Attr : access pthread_mutexattr_t
            )  return int;
   function pthread_mutexattr_init
            (  Attr : access pthread_mutexattr_t
            )  return int;
   function pthread_mutexattr_setprotocol
            (  Attr     : access pthread_mutexattr_t;
               Protocol : int
            )  return int;
   function pthread_mutexattr_setpshared
            (  Attr    : access pthread_mutexattr_t;
               Pshared : int
            )  return int;
   function pthread_mutexattr_setrobust
            (  Attr       : access pthread_mutexattr_t;
               Robustness : int
            )  return int;
   function pthread_threadid_np
            (  thread    : pthread_t;
               thread_id : access Unsigned_64
            )  return int;
   function timespec_get
            (  TS   : access timespec;
               Base : int := TIME_UTC
            )  return int;

   procedure Raise_From_Errno
             (  ID     : Exception_ID;
                Prefix : String := "";
                Error  : int := errno
             );
   function unlink (pathname : char_array) return int;
   function unlink (pathname : chars_ptr)  return int;

   PTHREAD_MUTEX_NORMAL     : constant := 0;
   PTHREAD_MUTEX_RECURSIVE  : constant := 1;
   PTHREAD_MUTEX_ERRORCHECK : constant := 2;
   PTHREAD_MUTEX_DEFAULT    : constant := PTHREAD_MUTEX_NORMAL;

   PTHREAD_PRIO_NONE        : constant := 0;
   PTHREAD_PRIO_INHERIT     : constant := 1;

   PTHREAD_PRIO_PROTECT     : constant := 2;
   PTHREAD_MUTEX_STALLED    : constant := 0;
   PTHREAD_MUTEX_ROBUST     : constant := 1;

   PTHREAD_CREATE_JOINABLE  : constant := 0;
   PTHREAD_INHERIT_SCHED    : constant := 0;
   PTHREAD_ONCE_INIT        : constant := 0;
   PTHREAD_PROCESS_PRIVATE  : constant := 0;
   PTHREAD_CREATE_DETACHED  : constant := 1;
   PTHREAD_EXPLICIT_SCHED   : constant := 1;
   PTHREAD_PROCESS_SHARED   : constant := 1;

   procedure Initialize (Cond  : access pthread_cond_t);
   procedure Initialize (Mutex : access pthread_mutex_t);

   type Mutex_Holder (Mutex : access pthread_mutex_t) is
      new Ada.Finalization.Limited_Controlled with null record;
   procedure Finalize   (Lock : in out Mutex_Holder);
   procedure Initialize (Lock : in out Mutex_Holder);

private
   pragma Import (C, close);
   pragma Import (C, ftruncate);
   pragma Import (C, getpagesize);
   pragma Import (C, getpid);
   pragma Import (C, gettimeofday);
   pragma Import (C, mmap);
   pragma Import (C, mkfifo);
   pragma Import (C, mkostemp);
   pragma Import (C, mremap);
   pragma Import (C, munmap);
   pragma Import (C, open);
   pragma Import (C, pthread_cond_broadcast);
   pragma Import (C, pthread_cond_destroy);
   pragma Import (C, pthread_cond_init);
   pragma Import (C, pthread_cond_timedwait);
   pragma Import (C, pthread_condattr_destroy);
   pragma Import (C, pthread_condattr_init);
   pragma Import (C, pthread_condattr_setpshared);
   pragma Import (C, pthread_mutex_destroy);
   pragma Import (C, pthread_mutex_init);
   pragma Import (C, pthread_mutex_lock);
   pragma Import (C, pthread_mutex_timedlock);
   pragma Import (C, pthread_mutex_trylock);
   pragma Import (C, pthread_mutex_unlock);
   pragma Import (C, pthread_mutexattr_destroy);
   pragma Import (C, pthread_mutexattr_init);
   pragma Import (C, pthread_mutexattr_setprotocol);
   pragma Import (C, pthread_mutexattr_setpshared);
   pragma Import (C, pthread_mutexattr_setrobust);
   pragma Import (C, pthread_threadid_np);
   pragma Import (C, timespec_get);
   pragma Import (C, unlink);

   pragma Inline ("<", "<=");

end Synchronization.Pthreads;
