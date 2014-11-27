--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Single_File.Text_IO              Luebeck            --
--  Interface                                      Autumn, 2014       --
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

with Ada.Text_IO;                   use Ada.Text_IO;
with Persistent.Memory_Pools.Dump;  use Persistent.Memory_Pools.Dump;

with Persistent.Data_Bank.Indexed.Text_IO;

package Persistent.Single_File.Text_IO is
   type Output_Flags is mod 2**(5+5);
   Output_Pool_General_Information : constant Output_Flags;
   Output_Pool_Free_List           : constant Output_Flags;
   Output_Pool_Block_Margins       : constant Output_Flags;
   Output_Pool_Block_Contents      : constant Output_Flags;
   Output_Pool_Memory_Contents     : constant Output_Flags;
   Output_Catalogue_Index          : constant Output_Flags;
   Output_Objects_Map              : constant Output_Flags;
   Output_Names_Map                : constant Output_Flags;
   Output_Objects_Links            : constant Output_Flags;
   Output_File_General_Information : constant Output_Flags;
   Output_Pool                     : constant Output_Flags;
   Output_Single_File              : constant Output_Flags;
   Output_All                      : constant Output_Flags;
--
-- Put - Dump the storage catalogue content
--
--  [ File ]  - The output file
--    Storage - The storage
--  [ Flags ] - The flags controlling persistent memory pool output
--
-- Exceptions :
--
--   Constraint_Error - Storage is not an single file storage
--
   procedure Put
             (  File    : File_Type;
                Storage : Storage_Handle;
                Flags   : Output_Flags := Output_Catalogue_Index
             );
   procedure Put
             (  File    : File_Type;
                Storage : in out Data_Bank_Object'Class;
                Flags   : Output_Flags := Output_Catalogue_Index
             );
   procedure Put
             (  Storage : Storage_Handle;
                Flags   : Output_Flags := Output_Catalogue_Index
             );
   procedure Put
             (  Storage : in out Data_Bank_Object'Class;
                Flags   : Output_Flags := Output_Catalogue_Index
             );
--
-- Put - Dump object
--
--  [ File ]  - The output file
--    Storage - The storage
--    ID      - The object's ID
--    Prefix  - The prefix of each output line
--
-- Exceptions :
--
--   Constraint_Error - Storage is not an single file storage
--
   procedure Put
             (  File    : File_Type;
                Storage : Storage_Handle;
                ID      : Object_ID;
                Prefix  : String := ""
             );
   procedure Put
             (  File    : File_Type;
                Storage : Data_Bank_Object'Class;
                ID      : Object_ID;
                Prefix  : String := ""
             );
   procedure Put
             (  Storage : Storage_Handle;
                ID      : Object_ID;
                Prefix  : String := ""
             );
   procedure Put
             (  Storage : Data_Bank_Object'Class;
                ID      : Object_ID;
                Prefix  : String := ""
             );
private
   Output_Pool_General_Information : constant Output_Flags :=
          Output_Flags (Dump_General_Information);
   Output_Pool_Free_List : constant Output_Flags :=
          Output_Flags (Dump_Free_List);
   Output_Pool_Block_Margins : constant Output_Flags :=
          Output_Flags (Dump_Block_Margins);
   Output_Pool_Block_Contents : constant Output_Flags :=
          Output_Flags (Dump_Block_Contents);
   Output_Pool_Memory_Contents : constant Output_Flags :=
          Output_Flags (Dump_Memory_Contents);
   Output_Catalogue_Index : constant Output_Flags :=
          Output_Pool_Memory_Contents * 2;
   Output_Objects_Map : constant Output_Flags :=
          Output_Catalogue_Index * 2;
   Output_Names_Map : constant Output_Flags :=
          Output_Objects_Map * 2;
   Output_Objects_Links : constant Output_Flags :=
          Output_Names_Map * 2;
   Output_File_General_Information : constant Output_Flags :=
          Output_Objects_Links * 2;
   Output_All : constant Output_Flags := Output_Flags'Last;
   Output_Pool : constant Output_Flags := Output_Flags (Dump_All);
   Output_Single_File : constant Output_Flags :=
          Output_Objects_Map or Output_Names_Map or
          Output_Objects_Links or Output_File_General_Information;

   package Indexed_IO is new Indexed.Text_IO;

   procedure Put
             (  File    : File_Type;
                Storage : Data_Base_Object'Class;
                Index   : Byte_Index;
                Prefix  : String := ""
             );

end Persistent.Single_File.Text_IO;
