/*                                                                    *\
\*  Linux primitives                Copyright (c)  Dmitry A. Kazakov  */
/*                                                 Luebeck            *\
\*                                                 Spring, 2018       */
/*                                                                    *\
\*                                Last revision :  01:05 01 May 2018  */
/*                                                                    *\
\*  This  library  is  free software; you can redistribute it and/or  */
/*  modify it under the terms of the GNU General Public  License  as  *\
\*  published by the Free Software Foundation; either version  2  of  */
/*  the License, or (at your option) any later version. This library  *\
\*  is distributed in the hope that it will be useful,  but  WITHOUT  */
/*  ANY   WARRANTY;   without   even   the   implied   warranty   of  *\
\*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  */
/*  General  Public  License  for  more  details.  You  should  have  *\
\*  received  a  copy  of  the GNU General Public License along with  */
/*  this library; if not, write to  the  Free  Software  Foundation,  *\
\*  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    */
/*                                                                    *\
\*  As a special exception, if other files instantiate generics from  */
/*  this unit, or you link this unit with other files to produce  an  *\
\*  executable, this unit does not by  itself  cause  the  resulting  */
/*  executable to be covered by the GNU General Public License. This  *\
\*  exception  does not however invalidate any other reasons why the  */
/*  executable file might be covered by the GNU Public License.       */

#define _GNU_SOURCE
#include <unistd.h>
#include <linux/futex.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/syscall.h>

#pragma ASSERT (sizeof (pid_t) == sizeof (int))
#pragma ASSERT (sizeof (long) == sizeof (off_t))
#pragma ASSERT (sizeof (timespec) == sizeof (long) * 2)

int futex_wake (int * data, int val)
{
   return syscall
          (  SYS_futex,
             data,
             FUTEX_WAKE,
             val,
             0,
             0,
             0
          );
}

int futex_wait
    (  int                   * data,
       int                     val,
       const struct timespec * timeout
    )
{
   return syscall
          (  SYS_futex,
             data,
             FUTEX_WAIT,
             val,
             timeout,
             0,
             0
          );
}

pid_t gettid ()
{
   return syscall (SYS_gettid);
}

