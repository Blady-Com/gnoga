--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Memory_Pools.Dump                Luebeck            --
--  Interface                                      Winter, 2014       --
--                                                                    --
--                                Last revision :  22:06 23 Jul 2014  --
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
--  This package  provides  procedures  to dump the contents of a memory
--  pools into a text file.
--
with Ada.Text_IO;  use Ada.Text_IO;

package Persistent.Memory_Pools.Dump is

   type Dump_Flags is mod 2**5;
   Dump_General_Information : constant Dump_Flags := 2**0;
   Dump_Free_List           : constant Dump_Flags := 2**1;
   Dump_Block_Margins       : constant Dump_Flags := 2**2;
   Dump_Block_Contents      : constant Dump_Flags := 2**3;
   Dump_Memory_Contents     : constant Dump_Flags := 2**4;
   Dump_All                 : constant Dump_Flags := Dump_Flags'Last;
--
-- Put -- Dump contents of a memory pool into the file
--
-- [ File ] - To dump into, when omitted Standard_Output is used
--   Pool   - The memory pool
--   Flags  - The flags controlling the output
--
-- Exceptions :
--
--    Use_Error - No file open
--
   procedure Put
             (  File  : File_Type;
                Pool  : Persistent_Pool'Class;
                Flags : Dump_Flags := Dump_All
             );
   procedure Put
             (  Pool  : Persistent_Pool'Class;
                Flags : Dump_Flags := Dump_All
             );

end Persistent.Memory_Pools.Dump;
