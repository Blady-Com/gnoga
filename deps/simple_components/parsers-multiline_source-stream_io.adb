--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Multiline_Source.Stream_IO          Luebeck            --
--  Implementation                                 Spring, 2010       --
--                                                                    --
--                                Last revision :  13:13 14 Sep 2019  --
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

package body Parsers.Multiline_Source.Stream_IO is

   Increment : constant Integer := 512;

   function Default_Delimiters return Map is
      Result : Map;
   begin
      Add (Result, Character'Val (4),  Text_End);
      Add (Result, Character'Val (10), Line_End);
      Add (Result, Character'Val (13), Line_Trailer);
      return Result;
   end Default_Delimiters;

   procedure Get_Line (Code : in out Source) is
      Item  : Character;
      Index : Integer;
   begin
      if Code.Terminated or else Code.Buffer = null then
         raise End_Error;
      end if;
      Code.Length := 0;
      begin
         loop
            Character'Read (Code.Stream, Item);
            Index := Find (Code.Delimiters, Item);
            if Index > 0 then
               case Get (Code.Delimiters, Index) is
                  when Line_End =>
                     exit;
                  when Text_End =>
                     raise End_Error;
                  when Line_Trailer =>
                     null;
               end case;
            end if;
            if Code.Length = Code.Buffer'Length then
               declare
                  Old_Line : String_Ptr := Code.Buffer;
               begin
                  Code.Buffer :=
                     new String (1..Old_Line'Length + Increment);
                  Code.Buffer (1..Old_Line'Length) := Old_Line.all;
                  Free (Old_Line);
               end;
            end if;
            Code.Length := Code.Length + 1;
            Code.Buffer (Code.Length) := Item;
         end loop;
      exception
         when End_Error =>
            Code.Terminated := True;
            if Code.Length = 0 then
               raise;
            end if;
      end;
      while (  Code.Length > 0
            and then
               Find (Code.Delimiters, Code.Buffer (Code.Length)) > 0
            )
      loop
         Code.Length := Code.Length - 1;
      end loop;
   exception
      when others =>
         Free (Code.Buffer);
         raise;
   end Get_Line;

end Parsers.Multiline_Source.Stream_IO;
