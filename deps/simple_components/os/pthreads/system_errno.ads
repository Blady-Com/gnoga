--                                                                    --
--  package System_Errno            Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2018       --
--                                                                    --
--                                Last revision :  01:09 01 Jul 2018  --
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
--  Free BSD and OS X error codes
--
   EPERM           : constant :=   1;     -- Operation not permitted
   ENOENT          : constant :=   2;     -- No such file or directory
   ESRCH           : constant :=   3;     -- No such process
   EINTR           : constant :=   4;     -- Interrupted system call
   EIO             : constant :=   5;     -- Input/output error
   ENXIO           : constant :=   6;     -- Device not configured
   E2BIG           : constant :=   7;     -- Argument list too long
   ENOEXEC         : constant :=   8;     -- Exec format error
   EBADF           : constant :=   9;     -- Bad file descriptor
   ECHILD          : constant :=  10;     -- No child processes
   EDEADLK         : constant :=  11;     -- Resource deadlock avoided
   ENOMEM          : constant :=  12;     -- Cannot allocate memory
   EACCES          : constant :=  13;     -- Permission denied
   EFAULT          : constant :=  14;     -- Bad address
   ENOTBLK         : constant :=  15;     -- Block device required
   EBUSY           : constant :=  16;     -- Device / Resource busy
   EEXIST          : constant :=  17;     -- File exists
   EXDEV           : constant :=  18;     -- Cross-device link
   ENODEV          : constant :=  19;     -- Operation not supported by device
   ENOTDIR         : constant :=  20;     -- Not a directory
   EISDIR          : constant :=  21;     -- Is a directory
   EINVAL          : constant :=  22;     -- Invalid argument
   ENFILE          : constant :=  23;     -- Too many open files in system
   EMFILE          : constant :=  24;     -- Too many open files
   ENOTTY          : constant :=  25;     -- Inappropriate ioctl for device
   ETXTBSY         : constant :=  26;     -- Text file busy
   EFBIG           : constant :=  27;     -- File too large
   ENOSPC          : constant :=  28;     -- No space left on device
   ESPIPE          : constant :=  29;     -- Illegal seek
   EROFS           : constant :=  30;     -- Read-only file system
   EMLINK          : constant :=  31;     -- Too many links
   EPIPE           : constant :=  32;     -- Broken pipe
   EDOM            : constant :=  33;     -- Numerical argument out of domain
   ERANGE          : constant :=  34;     -- Result too large
   EAGAIN          : constant :=  35;     -- Resource temporarily unavailable
   EWOULDBLOCK     : constant :=  EAGAIN; -- Operation would block
   EINPROGRESS     : constant :=  36;     -- Operation now in progress
   EALREADY        : constant :=  37;     -- Operation already in progress
   ENOTSOCK        : constant :=  38;     -- Socket operation on non-socket
   EDESTADDRREQ    : constant :=  39;     -- Destination address required
   EMSGSIZE        : constant :=  40;     -- Message too long
   EPROTOTYPE      : constant :=  41;     -- Protocol wrong type for socket
   ENOPROTOOPT     : constant :=  42;     -- Protocol not available
   EPROTONOSUPPORT : constant :=  43;     -- Protocol not supported
   ESOCKTNOSUPPORT : constant :=  44;     -- Socket type not supported
   ENOTSUP         : constant :=  45;     -- Operation not supported
   EPFNOSUPPORT    : constant :=  46;     -- Protocol family not supported
   EAFNOSUPPORT    : constant :=  47;     -- Address family not supported by protocol family
   EADDRINUSE      : constant :=  48;     -- Address already in use
   EADDRNOTAVAIL   : constant :=  49;     -- Can't assign requested address
   ENETDOWN        : constant :=  50;     -- Network is down
   ENETUNREACH     : constant :=  51;     -- Network is unreachable
   ENETRESET       : constant :=  52;     -- Network dropped connection on reset
   ECONNABORTED    : constant :=  53;     -- Software caused connection abort
   ECONNRESET      : constant :=  54;     -- Connection reset by peer
   ENOBUFS         : constant :=  55;     -- No buffer space available
   EISCONN         : constant :=  56;     -- Socket is already connected
   ENOTCONN        : constant :=  57;     -- Socket is not connected
   ESHUTDOWN       : constant :=  58;     -- Can't send after socket shutdown
   ETOOMANYREFS    : constant :=  59;     -- Too many references: can't splice
   ETIMEDOUT       : constant :=  60;     -- Operation timed out
   ECONNREFUSED    : constant :=  61;     -- Connection refused
   ELOOP           : constant :=  62;     -- Too many levels of symbolic links
   ENAMETOOLONG    : constant :=  63;     -- File name too long
   EHOSTDOWN       : constant :=  64;     -- Host is down
   EHOSTUNREACH    : constant :=  65;     -- No route to host
   ENOTEMPTY       : constant :=  66;     -- Directory not empty
   EPROCLIM        : constant :=  67;     -- Too many processes
   EUSERS          : constant :=  68;     -- Too many users
   EDQUOT          : constant :=  69;     -- Disc quota exceeded
   ESTALE          : constant :=  70;     -- Stale NFS file handle
   EREMOTE         : constant :=  71;     -- Too many levels of remote in path
   EBADRPC         : constant :=  72;     -- RPC struct is bad
   ERPCMISMATCH    : constant :=  73;     -- RPC version wrong
   EPROGUNAVAIL    : constant :=  74;     -- RPC prog. not avail
   EPROGMISMATCH   : constant :=  75;     -- Program version wrong
   EPROCUNAVAIL    : constant :=  76;     -- Bad procedure for program
   ENOLCK          : constant :=  77;     -- No locks available
   ENOSYS          : constant :=  78;     -- Function not implemented
   EFTYPE          : constant :=  79;     -- Inappropriate file type or format
   EAUTH           : constant :=  80;     -- Authentication error
   ENEEDAUTH       : constant :=  81;     -- Need authenticator
   EPWROFF         : constant :=  82;     -- Device power is off
   EDEVERR         : constant :=  83;     -- Device error, e.g. paper out
   EOVERFLOW       : constant :=  84;     -- Value too large to be stored in data type
   EBADEXEC        : constant :=  85;     -- Bad executable
   EBADARCH        : constant :=  86;     -- Bad CPU type in executable
   ESHLIBVERS      : constant :=  87;     -- Shared library version mismatch
   EBADMACHO       : constant :=  88;     -- Malformed Macho file
   ECANCELED       : constant :=  89;     -- Operation canceled
   EIDRM           : constant :=  90;     -- Identifier removed
   ENOMSG          : constant :=  91;     -- No message of desired type
   EILSEQ          : constant :=  92;     -- Illegal byte sequence
   ENOATTR         : constant :=  93;     -- Attribute not found
   EBADMSG         : constant :=  94;     -- Bad message
   EMULTIHOP       : constant :=  95;     -- Reserved
   ENODATA         : constant :=  96;     -- No message available on STREAM
   ENOLINK         : constant :=  97;     -- Reserved
   ENOSR           : constant :=  98;     -- No STREAM resources
   ENOSTR          : constant :=  99;     -- Not a STREAM
   EPROTO          : constant := 100;     -- Protocol error
   ETIME           : constant := 101;     -- STREAM ioctl timeout
   EOPNOTSUPP      : constant := 102;     -- Operation not supported on socket
   ENOPOLICY       : constant := 103;     -- No such policy registered
   ENOTRECOVERABLE : constant := 104;     -- State not recoverable
   EOWNERDEAD      : constant := 105;     -- Previous owner died
   EQFULL          : constant := 106;     -- Interface output queue is full
   ELAST           : constant := 106;     -- Must be equal largest errno

end System_Errno;
