--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Single_File_Keys                 Luebeck            --
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

with Ada.Streams;           use Ada.Streams;
with Persistent.Data_Bank;  use Persistent.Data_Bank;

package Persistent.Single_File_Keys is
--
-- Object_ID -- Object identifier
--
   type Object_ID is range 0..2**31 - 1;
   No_ID : constant Object_ID := 0;
--
-- Object_Key -- Persistent key is an integer
--
   type Object_Key is new Persistent_Key with record
      ID : aliased Object_ID;
   end record;
--
-- Null_Key, Comparisons -- Override Persistent.Data_Bank...
--
   function Null_Key return Object_Key;
   function "<" (Left, Right : Object_Key) return Boolean;
   function "=" (Left, Right : Object_Key) return Boolean;
   pragma Inline (Null_Key, "<", "=");
--
-- Image -- Implements Persistent.Data_Bank...
--
   function Image
            (  Storage : Data_Bank_Object'Class;
               Key     : Object_Key
            )  return String;
   function Image (ID : Object_ID) return String;
   function Value (Text : String) return Object_ID;

   type Object_ID_Array is array (Integer range <>) of Object_ID;
--
-- Name_Token -- Name token
--
   type Name_Token (Length : Natural) is record
      Parent : Object_ID;
      Name   : String (1..Length);
   end record;
   function "<" (Left, Right : Name_Token) return Boolean;

   function Input
            (  Stream : access Root_Stream_Type'Class
            )  return Object_ID;
   function Input
            (  Stream : access Root_Stream_Type'Class
            )  return Name_Token;
   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : Object_ID
             );
   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : Name_Token
             );

private
   pragma Inline (Image);

end Persistent.Single_File_Keys;
