--                                                                    --
--  package Persistent.Data_Bank    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2002       --
--                                                                    --
--                                Last revision :  10:05 22 Nov 2014  --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Persistent.Data_Bank is

   procedure Commit (Mutex : in out Access_Mutex) is
   begin
      if Mutex.Committed then
         Raise_Exception
         (  Use_Error'Identity,
            "Transaction already committed"
         );
      else
         Mutex.Committed := True;
         Commit (Mutex.Storage.all);
      end if;
   end Commit;

   procedure Finalize (Mutex : in out Access_Mutex) is
   begin
      if not Mutex.Committed then
         Mutex.Committed := True;
         Roll_Back (Mutex.Storage.all);
      end if;
   end Finalize;

   procedure Initialize (Mutex : in out Read_Mutex) is
   begin
      Seize_Read (Mutex.Storage.all);
   end Initialize;

   procedure Initialize (Mutex : in out Write_Mutex) is
   begin
      Seize_Write (Mutex.Storage.all);
   end Initialize;

end Persistent.Data_Bank;
