--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Linux                       Luebeck            --
--  Interface                                      Spring, 2018       --
--                                                                    --
--                                Last revision :  00:04 22 Jul 2018  --
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
--  This package  provides  binding to some  Linux  functions  useful to
--  communicate between processes.  Note that  this implementation  will
--  not work with OS X or Free BSD.  For them use  a POSIX thread  based
--  one.
--
with Ada.Exceptions;           use Ada.Exceptions;
with Interfaces;               use Interfaces;
with Interfaces.C;             use Interfaces.C;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;

package Synchronization.Linux is

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

   -- These are Linux-specific.
   MAP_NORESERVE  : constant Map_Flags := 16#0400#;
   MAP_GROWSDOWN  : constant Map_Flags := 16#1000#;
   MAP_DENYWRITE  : constant Map_Flags := 16#2000#;
   MAP_EXECUTABLE : constant Map_Flags := 16#4000#;
   MAP_LOCKED     : constant Map_Flags := 16#8000#;

   type off_t is new long;

   type Open_Flags is new int;
   O_RDONLY   : constant Open_Flags := 8#0000#; -- reading only
   O_WRONLY   : constant Open_Flags := 8#0001#; -- writing only
   O_RDWR     : constant Open_Flags := 8#0002#; -- reading and writing
   O_NONBLOCK : constant Open_Flags := 8#4000#; -- no delay
   O_APPEND   : constant Open_Flags := 8#2000#; -- set append mode
   O_CREAT    : constant Open_Flags := 8#0100#; -- create if new
   O_EXCL     : constant Open_Flags := 8#0200#; -- error if exists
   O_TRUNC    : constant Open_Flags := 8#1000#; -- truncate

   AF_UNIX    : constant := 1;
   SOCK_DGRAM : constant := 2;

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

   type pid_t is new int;

   type timespec is record
      tv_sec  : long; -- seconds
      tv_nsec : long; -- nanoseconds
   end record;
   pragma Convention (C, timespec);
   function From_Duration (Value : Duration) return timespec;

   function close (File : int) return int;
   function Compare_And_Swap
            (  Target  : access int;
               Old_Val : int;
               New_Val : int
            )  return Boolean;

   function errno return int;
   function futex_wake (Data : access int; Val : int) return int;
   function futex_wait
            (  Data    : access int;
               Val     : int;
               Timeout : timespec
            )  return int;
   function ftruncate (File : int; Length : off_t) return int;
   function getpagesize return int;
   function getpid return pid_t;
   function gettid return pid_t;
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
   procedure Raise_From_Errno
             (  ID     : Exception_ID;
                Prefix : String := "";
                Error  : int := errno
             );
   function unlink (pathname : char_array) return int;
   function unlink (pathname : chars_ptr)  return int;

   type sockaddr_un is record
      sun_family : unsigned_short      := AF_UNIX;          -- family
      sun_path   : char_array (1..108) := (others => Nul);  -- pathname
   end record;
   pragma Convention (C, sockaddr_un);

private
   pragma Import
          (  Intrinsic,
             Compare_And_Swap,
             "__sync_bool_compare_and_swap_4"
          );
   pragma Import (C, close);
   pragma Import (C, ftruncate);
   pragma Import (C, futex_wake);
   pragma Import (C, futex_wait);
   pragma Import (C, getpagesize);
   pragma Import (C, getpid);
   pragma Import (C, gettid);
   pragma Import (C, mmap);
   pragma Import (C, mkfifo);
   pragma Import (C, mkostemp);
   pragma Import (C, mremap);
   pragma Import (C, munmap);
   pragma Import (C, open);
   pragma Import (C, unlink);

end Synchronization.Linux;
