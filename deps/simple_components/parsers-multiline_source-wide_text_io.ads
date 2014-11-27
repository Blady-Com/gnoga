--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Multiline_Source.Wide_Text_IO       Luebeck            --
--  Interface                                      Winter, 2009       --
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
--  This package provides an implementation of code sources based on the
--  standard  text  I/O package for UCS-2 files. The package recodes the
--  source into UTF-8 as the file is read.
--
with Ada.Wide_Text_IO;  use Ada.Wide_Text_IO;

package Parsers.Multiline_Source.Wide_Text_IO is
--
-- Source -- The source containing a file
--
   type Source (File : access File_Type) is
      new Multiline_Source.Source with private;
--
-- Finalize -- Overrides Parsers.Multiline_Source...
--
   procedure Finalize (Code : in out Source);
--
-- Get_Line -- Overrides Parsers.Multiline_Source...
--
   procedure Get_Line (Code : in out Source);
--
-- Initialize -- Overrides Parsers.Multiline_Source...
--
   procedure Initialize (Code : in out Source);

private
   type Wide_String_Ptr is access Wide_String;
   type Source (File : access File_Type) is
      new Multiline_Source.Source with
   record
      Wide_Buffer : Wide_String_Ptr;
   end record;

end Parsers.Multiline_Source.Wide_Text_IO;
