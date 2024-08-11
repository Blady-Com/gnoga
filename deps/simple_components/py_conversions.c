/*
   package Py                           Copyright (c)  Dmitry A. Kazakov
   Interface                                           Luebeck
                                                       Summer, 2022

                                      Last revision :  13:34 16 Dec 2022

This  library is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as  published  by  the
Free Software Foundation; either version 2 of the License, or  (at  your
option)  any later version. This library is distributed in the hope that
it  will  be  useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU  General Public License for more details. You should have received a
copy of the GNU General Public License along with this library; if  not,
write to the Free Software Foundation, Inc., 59  Temple  Place  -  Suite
330, Boston, MA 02111-1307, USA.

As a special exception, if other files instantiate  generics  from  this
unit,  or  you link this unit with other files to produce an executable,
this  unit  does  not  by  itself  cause  the resulting executable to be
covered  by  the  GNU  General  Public  License. This exception does not
however invalidate any other reasons why the executable  file  might  be
covered by the GNU Public License.

*/
#include <stdint.h>

typedef long long (*tolonglong) (void *);
typedef unsigned long long (*tounsignedlonglong) (void *);

typedef void * (*fromlonglong) (long long);
typedef void * (*fromunsignedlonglong) (unsigned long long);

int64_t long_toint64 (tolonglong func, void * object, int * overflow)
{  /* long long is at least as large as int64_t */
   const long long Value  = (*func) (object);
   const int64_t   Result = (int64_t) Value;

   *overflow = (Value != (long long) Result);
   return Result;
}
uint64_t long_touint64
(
   tounsignedlonglong    func,
   void               *  object,
   int                *  overflow
)
{  /* unsigned long long is at least as large as uint64_t */
   const unsigned long long Value  = (*func) (object);
   const uint64_t           Result = (uint64_t) Value;

   *overflow = (Value != (unsigned long long) Result);
   return Result;
}
void * long_fromint64 (fromlonglong func, int64_t value, int * overflow)
{
   return (*func) ((long long) (value)); /* This must be safe */
}
void * long_fromuint64 (fromunsignedlonglong func, uint64_t value)
{
   return (*func) ((unsigned long long) value); /* This must be safe */
}
