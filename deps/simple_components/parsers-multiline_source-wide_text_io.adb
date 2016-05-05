--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Multiline_Source.Wide_Text_IO       Luebeck            --
--  Implementation                                 Winter, 2009       --
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

with Ada.Unchecked_Deallocation;

with Strings_Edit.UTF8.Handling;  use Strings_Edit.UTF8.Handling;

package body Parsers.Multiline_Source.Wide_Text_IO is

   Increment : constant Integer := 512;

   procedure Free is
      new Ada.Unchecked_Deallocation (Wide_String, Wide_String_Ptr);

   procedure Finalize (Code : in out Source) is
   begin
      Free (Code.Wide_Buffer);
      Finalize (Multiline_Source.Source (Code));
   end Finalize;

   procedure Get_Line (Code : in out Source) is
      Size : Natural;
   begin
      if Code.Wide_Buffer = null then
         raise End_Error;
      end if;
      Get_Line (Code.File.all, Code.Wide_Buffer.all, Size);
      Code.Length := Size;
      while Code.Length = Code.Wide_Buffer'Last loop
         declare
            Old_Line : Wide_String_Ptr := Code.Wide_Buffer;
         begin
            Code.Wide_Buffer :=
               new Wide_String (1..Old_Line'Length + Increment);
            Code.Wide_Buffer (1..Old_Line'Length) := Old_Line.all;
            Free (Old_Line);
         end;
         Get_Line
         (  Code.File.all,
            Code.Wide_Buffer (Code.Length + 1..Code.Wide_Buffer'Last),
            Size
         );
         Code.Length := Size;
      end loop;
      declare
         Line : constant String :=
                To_UTF8 (Code.Wide_Buffer (1..Code.Length));
      begin
         if Line'Length > Code.Length then
            Free (Code.Buffer);
            Code.Buffer := new String'(Line);
         else
            Code.Buffer (Line'Range) := Line;
         end if;
         Code.Length := Line'Length;
      end;
   exception
      when others =>
         Free (Code.Wide_Buffer);
         raise;
   end Get_Line;

   procedure Initialize (Code : in out Source) is
   begin
      Initialize (Multiline_Source.Source (Code));
      Code.Wide_Buffer := new Wide_String (Code.Buffer'Range);
   end Initialize;

end Parsers.Multiline_Source.Wide_Text_IO;
