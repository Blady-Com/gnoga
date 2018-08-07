--                                                                    --
--  package System_Errno            Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2018       --
--                                                                    --
--                                Last revision :  00:11 22 Jul 2018  --
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
-- Linux error codes
--
   EPERM           : constant :=  1;      -- Operation not permitted
   ENOENT          : constant :=  2;      -- No such file or directory
   ESRCH           : constant :=  3;      -- No such process
   EINTR           : constant :=  4;      -- Interrupted system call
   EIO             : constant :=  5;      -- I/O error
   ENXIO           : constant :=  6;      -- No such device or address
   E2BIG           : constant :=  7;      -- Arg list too long
   ENOEXEC         : constant :=  8;      -- Exec format error
   EBADF           : constant :=  9;      -- Bad file number
   ECHILD          : constant := 10;      -- No child processes
   EAGAIN          : constant := 11;      -- Try again
   ENOMEM          : constant := 12;      -- Out of memory
   EACCES          : constant := 13;      -- Permission denied
   EFAULT          : constant := 14;      -- Bad address
   ENOTBLK         : constant := 15;      -- Block device required
   EBUSY           : constant := 16;      -- Device or resource busy
   EEXIST          : constant := 17;      -- File exists
   EXDEV           : constant := 18;      -- Cross-device link
   ENODEV          : constant := 19;      -- No such device
   ENOTDIR         : constant := 20;      -- Not a directory
   EISDIR          : constant := 21;      -- Is a directory
   EINVAL          : constant := 22;      -- Invalid argument
   ENFILE          : constant := 23;      -- File table overflow
   EMFILE          : constant := 24;      -- Too many open files
   ENOTTY          : constant := 25;      -- Not a typewriter
   ETXTBSY         : constant := 26;      -- Text file busy
   EFBIG           : constant := 27;      -- File too large
   ENOSPC          : constant := 28;      -- No space left on device
   ESPIPE          : constant := 29;      -- Illegal seek
   EROFS           : constant := 30;      -- Read-only file system
   EMLINK          : constant := 31;      -- Too many links
   EPIPE           : constant := 32;      -- Broken pipe
   EDOM            : constant := 33;      -- Math argument out of domain of func
   ERANGE          : constant := 34;      -- Math result not representable
   EDEADLK         : constant := 35;      -- Resource deadlock would occur
   ENAMETOOLONG    : constant := 36;      -- File name too long
   ENOLCK          : constant := 37;      -- No record locks available
   ENOSYS          : constant := 38;      -- Function not implemented
   ENOTEMPTY       : constant := 39;      -- Directory not empty
   ELOOP           : constant := 40;      -- Too many symbolic links encountered
   EWOULDBLOCK     : constant := EAGAIN;  -- Operation would block
   ENOMSG          : constant := 42;      -- No message of desired type
   EIDRM           : constant := 43;      -- Identifier removed
   ECHRNG          : constant := 44;      -- Channel number out of range
   EL2NSYNC        : constant := 45;      -- Level 2 not synchronized
   EL3HLT          : constant := 46;      -- Level 3 halted
   EL3RST          : constant := 47;      -- Level 3 reset
   ELNRNG          : constant := 48;      -- Link number out of range
   EUNATCH         : constant := 49;      -- Protocol driver not attached
   ENOCSI          : constant := 50;      -- No CSI structure available
   EL2HLT          : constant := 51;      -- Level 2 halted
   EBADE           : constant := 52;      -- Invalid exchange
   EBADR           : constant := 53;      -- Invalid request descriptor
   EXFULL          : constant := 54;      -- Exchange full
   ENOANO          : constant := 55;      -- No anode
   EBADRQC         : constant := 56;      -- Invalid request code
   EBADSLT         : constant := 57;      -- Invalid slot
   EDEADLOCK       : constant := EDEADLK;
   EBFONT          : constant := 59;      -- Bad font file format
   ENOSTR          : constant := 60;      -- Device not a stream
   ENODATA         : constant := 61;      -- No data available
   ETIME           : constant := 62;      -- Timer expired
   ENOSR           : constant := 63;      -- Out of streams resources
   ENONET          : constant := 64;      -- Machine is not on the network
   ENOPKG          : constant := 65;      -- Package not installed
   EREMOTE         : constant := 66;      -- Object is remote
   ENOLINK         : constant := 67;      -- Link has been severed
   EADV            : constant := 68;      -- Advertise error
   ESRMNT          : constant := 69;      -- Srmount error
   ECOMM           : constant := 70;      -- Communication error on send
   EPROTO          : constant := 71;      -- Protocol error
   EMULTIHOP       : constant := 72;      -- Multihop attempted
   EDOTDOT         : constant := 73;      -- RFS specific error
   EBADMSG         : constant := 74;      -- Not a data message
   EOVERFLOW       : constant := 75;      -- Value too large for defined data type
   ENOTUNIQ        : constant := 76;      -- Name not unique on network
   EBADFD          : constant := 77;      -- File descriptor in bad state
   EREMCHG         : constant := 78;      -- Remote address changed
   ELIBACC         : constant := 79;      -- Can not access a needed shared library
   ELIBBAD         : constant := 80;      -- Accessing a corrupted shared library
   ELIBSCN         : constant := 81;      -- .lib section in a.out corrupted
   ELIBMAX         : constant := 82;      -- Attempting to link in too many shared libraries
   ELIBEXEC        : constant := 83;      -- Cannot exec a shared library directly
   EILSEQ          : constant := 84;      -- Illegal byte sequence
   ERESTART        : constant := 85;      -- Interrupted system call should be restarted
   ESTRPIPE        : constant := 86;      -- Streams pipe error
   EUSERS          : constant := 87;      -- Too many users
   ENOTSOCK        : constant := 88;      -- Socket operation on non-socket
   EDESTADDRREQ    : constant := 89;      -- Destination address required
   EMSGSIZE        : constant := 90;      -- Message too long
   EPROTOTYPE      : constant := 91;      -- Protocol wrong type for socket
   ENOPROTOOPT     : constant := 92;      -- Protocol not available
   EPROTONOSUPPORT : constant := 93;      -- Protocol not supported
   ESOCKTNOSUPPORT : constant := 94;      -- Socket type not supported
   EOPNOTSUPP      : constant := 95;      -- Operation not supported on transport endpoint
   EPFNOSUPPORT    : constant := 96;      -- Protocol family not supported
   EAFNOSUPPORT    : constant := 97;      -- Address family not supported by protocol
   EADDRINUSE      : constant := 98;      -- Address already in use
   EADDRNOTAVAIL   : constant := 99;      -- Cannot assign requested address
   ENETDOWN        : constant := 100;     -- Network is down
   ENETUNREACH     : constant := 101;     -- Network is unreachable
   ENETRESET       : constant := 102;     -- Network dropped connection because of reset
   ECONNABORTED    : constant := 103;     -- Software caused connection abort
   ECONNRESET      : constant := 104;     -- Connection reset by peer
   ENOBUFS         : constant := 105;     -- No buffer space available
   EISCONN         : constant := 106;     -- Transport endpoint is already connected
   ENOTCONN        : constant := 107;     -- Transport endpoint is not connected
   ESHUTDOWN       : constant := 108;     -- Cannot send after transport endpoint shutdown
   ETOOMANYREFS    : constant := 109;     -- Too many references: cannot splice
   ETIMEDOUT       : constant := 110;     -- Connection timed out
   ECONNREFUSED    : constant := 111;     -- Connection refused
   EHOSTDOWN       : constant := 112;     -- Host is down
   EHOSTUNREACH    : constant := 113;     -- No route to host
   EALREADY        : constant := 114;     -- Operation already in progress
   EINPROGRESS     : constant := 115;     -- Operation now in progress
   ESTALE          : constant := 116;     -- Stale NFS file handle
   EUCLEAN         : constant := 117;     -- Structure needs cleaning
   ENOTNAM         : constant := 118;     -- Not a XENIX named type file
   ENAVAIL         : constant := 119;     -- No XENIX semaphores available
   EISNAM          : constant := 120;     -- Is a named type file
   EREMOTEIO       : constant := 121;     -- Remote I/O error
   EDQUOT          : constant := 122;     -- Quota exceeded
   ENOMEDIUM       : constant := 123;     -- No medium found
   EMEDIUMTYPE     : constant := 124;     -- Wrong medium type

end System_Errno;
