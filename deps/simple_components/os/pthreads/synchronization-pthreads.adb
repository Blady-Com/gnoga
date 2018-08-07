--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Pthreads                    Luebeck            --
--  Implementation                                 Summer, 2018       --
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

with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with System_Errno;           use System_Errno;

package body Synchronization.Pthreads is

   Busy_Wait_Interval : constant Duration := 0.000_1; -- 0.1ms

   function "<" (Left, Right : timespec) return Boolean is
   begin
      return (  Left.tv_sec < Right.tv_sec
             or else
                (  Left.tv_sec = Right.tv_sec
                and then
                   Left.tv_nsec < Right.tv_nsec
             )  );
   end "<";

   function "<=" (Left, Right : timespec) return Boolean is
   begin
      return Left = Right or else Left < Right;
   end "<=";

   function errno return int is
      function errno_location return access int;
      pragma Import (C, errno_location, "__error");
   begin
      return errno_location.all;
   end errno;

   function pthread_mutexattr_setrobust_fallback
            (  Attr       : access pthread_mutexattr_t;
               Robustness : int
            )  return int;
   pragma External
          (  C,
             pthread_mutexattr_setrobust_fallback,
             "pthread_mutexattr_setrobust"
          );
   pragma Weak_External (pthread_mutexattr_setrobust_fallback);

   function pthread_mutexattr_setrobust_fallback
            (  Attr       : access pthread_mutexattr_t;
               Robustness : int
            )  return int is
   begin
      return 0;
   end pthread_mutexattr_setrobust_fallback;

   function pthread_mutex_timedlock_fallback
            (  Mutex   : access pthread_mutex_t;
               Timeout : timespec
            )  return int;
   pragma External
          (  C,
             pthread_mutex_timedlock_fallback,
             "pthread_mutex_timedlock"
          );
   pragma Weak_External (pthread_mutex_timedlock_fallback);

   function pthread_mutex_timedlock_fallback
            (  Mutex   : access pthread_mutex_t;
               Timeout : timespec
            )  return int is
      Result : int;
      Now    : aliased timespec;
   begin
      loop -- Busy waiting
         Result := pthread_mutex_trylock (Mutex);
         case Result is
            when EBUSY =>
               Result := timespec_get (Now'Access);
               if Result = 0 then
                  if Timeout < Now then
                     return ETIMEDOUT;
                  end if;
               else
                  return Result;
               end if;
            when others =>
               return Result;
         end case;
         delay Busy_Wait_Interval;
      end loop;
   end pthread_mutex_timedlock_fallback;

   function timespec_get_fallback
            (  TS   : access timespec;
               Base : int := TIME_UTC
            )  return int;
   pragma External
          (  C,
             timespec_get_fallback,
             "timespec_get"
          );
   pragma Weak_External (timespec_get_fallback);

   function timespec_get_fallback
            (  TS   : access timespec;
               Base : int := TIME_UTC
            )  return int is
      Result : int;
      Stamp  : aliased timeval;
   begin
      Result := gettimeofday (Stamp'Access);
      if Result = 0 then
         TS.tv_sec  := Stamp.tv_sec;
         TS.tv_nsec := long (Stamp.tv_usec) * 1000;
      end if;
      return Result;
   exception
      when others =>
         Raise_Exception
         (  Program_Error'Identity,
            (  "Invalid gettimeofday:"
            &  long'Image (Stamp.tv_sec)
            &  "s,"
            &  int'Image (Stamp.tv_usec)
            &  "us"
         )  );
   end timespec_get_fallback;

   procedure Finalize (Lock : in out Mutex_Holder) is
      Result : int;
   begin
      Result := pthread_mutex_unlock (Lock.Mutex);
   end Finalize;

   function From_Duration
            (  Value    : Duration;
               Absolute : Boolean
            )  return timespec is
      Error  : int;
      Result : aliased timespec := (0, 0);
   begin
      if Absolute then
         Error := timespec_get (Result'Access);
      end if;
      if Value > 0.0 then
         declare
            Total   : constant Long_Float :=
                      (  Long_Float (Value)
                      +  Long_Float (Result.tv_sec)
                      +  Long_Float (Result.tv_nsec) / 1_000_000_000.0
                      );
            Seconds : constant Long_Float := Long_Float'Floor (Total);
         begin
            Result.tv_sec  := long (Seconds);
            Result.tv_nsec := long ((Total - Seconds) * 1_000_000_000.0);
         exception
            when Constraint_Error =>
               Result.tv_sec  := long'Last;
               Result.tv_nsec := 999_999_999;
         end;
      end if;
      return Result;
   end From_Duration;

   procedure Initialize (Lock : in out Mutex_Holder) is
      Result : int;
   begin
      Result := pthread_mutex_lock (Lock.Mutex);
   end Initialize;

   procedure Initialize (Cond : access pthread_cond_t) is
      Attribute : aliased pthread_condattr_t;
      Result    : int;
   begin
      Result := pthread_condattr_init (Attribute'Access);
      if Result /= 0 then
         Raise_From_Errno
         (  Data_Error'Identity,
            "pthread_condattr_init fault: ",
            Result
         );
      end if;
      Result := pthread_condattr_setpshared
                (  Attribute'Access,
                   PTHREAD_PROCESS_SHARED
                );
      if Result /= 0 then
         Raise_From_Errno
         (  Data_Error'Identity,
            "pthread_condattr_setpshared fault: ",
            Result
         );
      end if;
      Result := pthread_cond_init (Cond, Attribute);
      if Result /= 0 then
         Raise_From_Errno
         (  Data_Error'Identity,
            "pthread_cond_init fault: ",
            Result
         );
      end if;
   end Initialize;

   procedure Initialize (Mutex : access pthread_mutex_t) is
      Attribute : aliased pthread_mutexattr_t;
      Result    : int;
   begin
      Result := pthread_mutexattr_init (Attribute'Access);
      if Result /= 0 then
         Raise_From_Errno
         (  Data_Error'Identity,
            "pthread_mutexattr_init fault: ",
            Result
         );
      end if;
      Result := pthread_mutexattr_setpshared
                (  Attribute'Access,
                   PTHREAD_PROCESS_SHARED
                );
      if Result /= 0 then
         Raise_From_Errno
         (  Data_Error'Identity,
            "pthread_mutexattr_setpshared fault: ",
            Result
         );
      end if;
      Result := pthread_mutexattr_setrobust
                (  Attribute'Access,
                   PTHREAD_MUTEX_ROBUST
                );
      if Result /= 0 then
         Raise_From_Errno
         (  Data_Error'Identity,
            "pthread_mutexattr_setrobust fault: ",
            Result
         );
      end if;
      Result := pthread_mutex_init (Mutex, Attribute);
      if Result /= 0 then
         Raise_From_Errno
         (  Data_Error'Identity,
            "pthread_mutex_init fault: ",
            Result
         );
      end if;
   end Initialize;

   procedure Raise_From_Errno
             (  ID     : Exception_ID;
                Prefix : String := "";
                Error  : int := errno
             )  is
      function strerror_r
               (  Error  : int;
                  Buffer : System.Address;
                  Length : size_t
               )  return int;
      pragma Import (C, strerror_r);

      Buffer  : String (1..2048);
      Result  : int;
   begin
      Result :=
         strerror_r (Error, Buffer (1)'Address, Buffer'Length);
      if Result = 0 then
         for Index in Buffer'Range loop
            if Buffer (Index) = Character'Val (0) then
               if Prefix'Length = 0 then
                  Raise_Exception
                  (  ID,
                     (  Buffer (1..Index - 1)
                     &  " ["
                     &  Image (Integer (Error))
                     &  "]"
                  )  );
               else
                  Raise_Exception
                  (  ID,
                     (  Prefix
                     &  Buffer (1..Index - 1)
                     &  " ["
                     &  Image (Integer (Error))
                     &  "]"
                  )  );
               end if;
            end if;
         end loop;
         if Prefix'Length = 0 then
            Raise_Exception
            (  ID,
               Buffer & " [" &  Image (Integer (Error)) & "]"
            );
         else
            Raise_Exception
            (  ID,
               Prefix & Buffer & " [" &  Image (Integer (Error)) & "]"
            );
         end if;
      end if;
      if Prefix'Length = 0 then
         Raise_Exception
         (  ID,
            "[" &  Image (Integer (Error)) & "]"
         );
      else
         Raise_Exception
         (  ID,
            Prefix & " [" &  Image (Integer (Error)) & "]"
         );
      end if;
   end Raise_From_Errno;

end Synchronization.Pthreads;
