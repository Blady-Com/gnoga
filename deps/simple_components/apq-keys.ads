--                                                                    --
--  package APQ.Keys                Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  19:53 12 Jan 2008  --
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

with Persistent.Data_Bank;  use Persistent.Data_Bank;

package APQ.Keys is
   pragma Elaborate_Body (APQ.Keys);
--
-- Object_ID -- Object identifier
--
   subtype Object_ID is Row_ID_Type;
   No_ID : constant Object_ID := 0;
--
-- Object_Key -- Persistent key is an integer
--
   type Object_Key is new Persistent_Key with record
      ID : Object_ID;
   end record;
--
-- Comparisons -- Override Persistent.Data_Bank...
--
   function "<" (Left, Right : Object_Key) return Boolean;
   function "=" (Left, Right : Object_Key) return Boolean;
   pragma Inline ("<", "=");
--
-- Image -- Implements Persistent.Data_Bank...
--
   function Image
            (  Storage : Data_Bank_Object'Class;
               Key     : Object_Key
            )  return String;
--
-- Null_Key -- Override Persistent.Data_Bank...
--
   function Null_Key return Object_Key;
   pragma Inline (Null_Key);

   type Object_ID_Array is array (Integer range <>) of Object_ID;

end APQ.Keys;
