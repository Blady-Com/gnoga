--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Single_File_Keys                 Luebeck            --
--  Implementation                                 Autumn, 2014       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.IO_Exceptions;              use Ada.IO_Exceptions;
with Strings_Edit.Streams.Naturals;  use Strings_Edit.Streams.Naturals;

with Strings_Edit.Integers;

package body Persistent.Single_File_Keys is

   function Image
            (  Storage : Data_Bank_Object'Class;
               Key     : Object_Key
            )  return String is
   begin
      return Image (Key.ID);
   end Image;

   function Image (ID : Object_ID) return String is
   begin
      return Strings_Edit.Integers.Image (Integer (ID));
   end Image;

   function Input
            (  Stream : access Root_Stream_Type'Class
            )  return Object_ID is
      ID : constant Natural := Input (Stream);
   begin
      return Object_ID (ID);
   end Input;

   function Input
            (  Stream : access Root_Stream_Type'Class
            )  return Name_Token is
      Result : Name_Token (Input (Stream));
   begin
      Result.Parent := Input (Stream);
      if Result.Length > 0 then
         String'Read (Stream, Result.Name);
      end if;
      return Result;
   exception
      when Constraint_Error =>
         Raise_Exception (Data_Error'Identity, "String is too long");
         raise;
      when others =>
         Raise_Exception (Data_Error'Identity, "Wrong string");
   end Input;

   function Null_Key return Object_Key is
   begin
      return (Persistent_Key with 0);
   end Null_Key;

   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : Object_ID
             )  is
   begin
      Output (Stream, Natural (Value));
   end Output;

   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : Name_Token
             )  is
   begin
      Output (Stream, Value.Length);
      Output (Stream, Natural (Value.Parent));
      if Value.Length > 0 then
         String'Write (Stream, Value.Name);
      end if;
   end Output;

   function Value (Text : String) return Object_ID is
   begin
      return Object_ID (Strings_Edit.Integers.Value (Text));
   end Value;

   function "=" (Left, Right : Object_Key) return Boolean is
   begin
      return Left.ID = Right.ID;
   end "=";

   function "<" (Left, Right : Object_Key) return Boolean is
   begin
      return Left.ID < Right.ID;
   end "<";

   function "<" (Left, Right : Name_Token) return Boolean is
   begin
      return
      (  Left.Parent < Right.Parent
      or else
         (  Left.Parent = Right.Parent
         and then
            Left.Name < Right.Name
      )  );
   end "<";

end Persistent.Single_File_Keys;
