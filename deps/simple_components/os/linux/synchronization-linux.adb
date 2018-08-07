--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Linux                       Luebeck            --
--  Implementation                                 Spring, 2018       --
--                                                                    --
--                                Last revision :  11:33 15 Jul 2018  --
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

with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body Synchronization.Linux is

   function errno return int is
      function errno_location return access int;
      pragma Import (C, errno_location, "__errno_location");
   begin
      return errno_location.all;
   end errno;

   function From_Duration (Value : Duration) return timespec is
   begin
      if Value > 0.0 then
         declare
            Total   : constant Long_Float := Long_Float (Value);
            Seconds : constant Long_Float := Long_Float'Floor (Total);
         begin
            return
            (  tv_sec  => long (Seconds),
               tv_nsec => long ((Total - Seconds) * 1_000_000_000.0)
            );
         end;
      else
         return (0, 0);
      end if;
   end From_Duration;

   procedure Raise_From_Errno
             (  ID     : Exception_ID;
                Prefix : String := "";
                Error  : int := errno
             )  is
      function strerror_r
               (  Error  : int;
                  Buffer : access char;
                  Length : size_t
               )  return chars_ptr;
      pragma Import (C, strerror_r);

      Buffer : char_array (1..2048);
      Result : chars_ptr;
   begin
      Result :=
         strerror_r (Error, Buffer (1)'Access, Buffer'Length);
      if Prefix'Length = 0 then
         Raise_Exception
         (  ID,
            (  Value (Result)
            &  " ["
            &  Image (Integer (Error))
            &  "]"
         )  );
      else
         Raise_Exception
         (  ID,
            (  Prefix
            &  Value (Result)
            &  " ["
            &  Image (Integer (Error))
            &  "]"
         )  );
      end if;
   end Raise_From_Errno;

end Synchronization.Linux;
