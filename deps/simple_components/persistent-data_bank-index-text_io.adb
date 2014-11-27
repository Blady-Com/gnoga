--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Data_Bank.Index.                 Luebeck            --
--        Text_IO                                  Summer, 2009       --
--  Implementation                                                    --
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

with Ada.Tags;                 use Ada.Tags;
with System.Storage_Elements;  use System.Storage_Elements;

package body Persistent.Data_Bank.Index.Text_IO is
   use Ptr_Map;
   use Key_Map;
   use Name_Map;

   Title : constant String :=
   "--record--key-object---name--type-tag-------------------------";

   procedure Put (File : File_Type; List : Catalogue) is
      procedure Put (Item : Catalogue_Record) is
      begin
         Put_Line
         (  File,
            (  Integer_Address'Image (To_Integer (Item'Address))
            &  "  "
            &  Image (List.Storage.all, Item.External)
            &  "  "
            &  Integer_Address'Image
               (  To_Integer (This (Item).all'Address)
               )
            &  "  '"
            &  Item.Name
            &  "'  "
            &  Expanded_Name (This (Item).all'Tag)
         )  );
      end Put;
   begin
      Put_Line (File, "By name table");
      Put_Line (File, Title);
      for Index in 1..Get_Size (List.By_Name) loop
         Put (Get (List.By_Name, Index).all);
      end loop;
      Put_Line (File, "By address table");
      Put_Line (File, Title);
      for Index in 1..Ptr_Map.Get_Size (List.By_Ptr) loop
         Put (Catalogue_Record (Get (List.By_Ptr, Index).all));
      end loop;
      Put_Line (File, "By key table");
      Put_Line (File, Title);
      for Index in 1..Get_Size (List.By_Key) loop
         Put (Get (List.By_Key, Index).all);
      end loop;
   end Put;

   procedure Put (List : Catalogue) is
   begin
      Put (Standard_Output, List);
   end Put;

end Persistent.Data_Bank.Index.Text_IO;
