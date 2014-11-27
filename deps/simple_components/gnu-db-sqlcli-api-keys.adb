--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNU.DB.SQLCLI.API.Keys                      Luebeck            --
--  Implementation                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  11:37 13 Oct 2007  --
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

with GNU.DB.SQLCLI.API.Keys.Edit;  use GNU.DB.SQLCLI.API.Keys.Edit;

package body GNU.DB.SQLCLI.API.Keys is

   function Image
            (  Storage : Data_Bank_Object'Class;
               Key     : Object_Key
            )  return String is
      ID : Object_ID renames Key.ID;
   begin
      return Image (ID);
   end Image;

   function Null_Key return Object_Key is
   begin
      return (Persistent_Key with No_ID);
   end Null_Key;
   
   function "=" (Left, Right : Object_Key) return Boolean is
   begin
      return Left.ID = Right.ID;
   end "=";

   function "<" (Left, Right : Object_Key) return Boolean is
   begin
      return Left.ID < Right.ID;
   end "<";

end GNU.DB.SQLCLI.API.Keys;
