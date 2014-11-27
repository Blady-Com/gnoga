--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Multiline_Source.Stream_IO          Luebeck            --
--  Interface                                      Spring, 2010       --
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
--
--  This package provides an implementation of  code  sources  based  on
--  top of a stream.
--
with Ada.Streams;  use Ada.Streams;

with Generic_Map;

package Parsers.Multiline_Source.Stream_IO is
--
-- Delimiter -- Types of delimiters
--
--   Line_End     - Characters of this category terminate a source line
--   Line_Trailer - These charaters are removed when at the line end
--   Text_End     - These characters terminate the source
--
-- Source lines are terminated by a Line_End character.  The  last  line
-- may  also  be  terminated  by  Text_End.  So the sequences Line_End +
-- Text_End and Text_End are equivalent. Any Line_Trailer characters are
-- removed when appear at the line end.
--
   type Delimiter is (Line_End, Line_Trailer, Text_End);
--
-- Delimiter_Maps -- Character to Delimiter map
--
   package Delimiter_Maps is new Generic_Map (Character, Delimiter);
   use Delimiter_Maps;
--
-- Default_Delimiters -- The  default  delimiter  map  is  that  LF is a
--                       Line_End  delimiter,  CR is a Line_Trailer, EOT
-- is a Text_End. Additionally  when  stream  propagates  the  End_Error
-- exeption that is treated as the end of the source.
--
   function Default_Delimiters return Map;
--
-- Source -- The source contained by a stream
--
--    Stream     - To read from
--    Delimiters - Used to match lines in the stream
--    Terminated - True if the text end has been reached
--
   type Source (Stream : access Root_Stream_Type'Class) is
      new Multiline_Source.Source with
   record
      Delimiters : Map := Default_Delimiters;
      Terminated : Boolean := False;
   end record;
--
-- Get_Line -- Overrides Parsers.Multiline_Source...
--
   procedure Get_Line (Code : in out Source);

end Parsers.Multiline_Source.Stream_IO;
