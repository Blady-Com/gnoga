--                                                                    --
--  package System_Errno            Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2018       --
--                                                                    --
--                                Last revision :  22:26 24 Jul 2018  --
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

package System_Errno is
--
-- Windows error codes
--
   EPERM        : constant :=  1; -- Operation not permitted
   ENOENT       : constant :=  2; -- No such file or directory
   ESRCH        : constant :=  3; -- No such process
   EINTR        : constant :=  4; -- Interrupted function
   EIO          : constant :=  5; -- I/O error
   ENXIO        : constant :=  6; -- No such device or address
   E2BIG        : constant :=  7; -- Argument list too long
   ENOEXEC      : constant :=  8; -- Exec format error
   EBADF        : constant :=  9; -- Bad file number
   ECHILD       : constant := 10; -- No spawned processes
   EAGAIN       : constant := 11; -- No more processes or not enough memory or maximum nesting level reached
   ENOMEM       : constant := 12; -- Not enough memory
   EACCES       : constant := 13; -- Permission denied
   EFAULT       : constant := 14; -- Bad address
   EBUSY        : constant := 16; -- Device or resource busy
   EEXIST       : constant := 17; -- File exists
   EXDEV        : constant := 18; -- Cross-device link
   ENODEV       : constant := 19; -- No such device
   ENOTDIR      : constant := 20; -- Not a directory
   EISDIR       : constant := 21; -- Is a directory
   EINVAL       : constant := 22; -- Invalid argument
   ENFILE       : constant := 23; -- Too many files open in system
   EMFILE       : constant := 24; -- Too many open files
   ENOTTY       : constant := 25; -- Inappropriate I/O control operation
   EFBIG        : constant := 27; -- File too large
   ENOSPC       : constant := 28; -- No space left on device
   ESPIPE       : constant := 29; -- Invalid seek
   EROFS        : constant := 30; -- Read-only file system
   EMLINK       : constant := 31; -- Too many links
   EPIPE        : constant := 32; -- Broken pipe
   EDOM         : constant := 33; -- Math argument
   ERANGE       : constant := 34; -- Result too large
   EDEADLK      : constant := 36; -- Resource deadlock would occur
   EDEADLOCK    : constant := 36; -- Same as EDEADLK for compatibility with older Microsoft C versions
   ENAMETOOLONG : constant := 38; -- Filename too long
   ENOLCK       : constant := 39; -- No locks available
   ENOSYS       : constant := 40; -- Function not supported
   ENOTEMPTY    : constant := 41; -- Directory not empty
   EILSEQ       : constant := 42; -- Illegal byte sequence
   STRUNCATE    : constant := 80; -- String was truncated

end System_Errno;
