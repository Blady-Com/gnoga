--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.SQLite.Text_IO                   Luebeck            --
--  Implementation                                 Winter, 2010       --
--                                                                    --
--                                Last revision :  09:24 09 Apr 2010  --
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

package body Persistent.SQLite.Text_IO is

   procedure Put (File : File_Type; Storage : Storage_Handle) is
   begin
      Indexed_IO.Put (File, Data_Base_Object'Class (Ptr (Storage).all));
   end Put;

   procedure Put
             (  File    : File_Type;
                Storage : in out Data_Bank_Object'Class
             )  is
   begin
      Indexed_IO.Put (File, Data_Base_Object'Class (Storage));
   end Put;

   procedure Put (Storage : Storage_Handle) is
   begin
      Put (Standard_Output, Storage);
   end Put;

   procedure Put (Storage : in out Data_Bank_Object'Class) is
   begin
      Put (Standard_Output, Storage);
   end Put;

end Persistent.SQLite.Text_IO;
