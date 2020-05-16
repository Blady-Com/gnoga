--                                                                    --
--  package Tables.UTF8_Names       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2008       --
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
--
--  This package defines type Dictionary derived from Table. It has same
--  functionality as the base type, but intended for keeping only  valid
--  case-insensitive  UTF-8  encoded  names.  The  case  is ignored when
--  matched  (Find,  Get),  but  kept  by  the  table. Additionally, any
--  non-empty  chain  of  characters  from  the set Blanks is considered
--  equivalent to space when matched. Further any character from Ignored
--  is ignored. Ignored characters  are  not  kept  in  the  table.  The
--  procedure Check_Spelling is used to check spelling of a name  before
--  placing it into the table. It may raise Constraint_Error to indicate
--  a wrong spelling. The  procedure  Check_Matched  is  used  to  check
--  whether the matched keyword is a proper name. It is called from  Get
--  with Pointer set to the first character following the matched  name.
--  It  returns  True if the name fits. Usually an implementation checks
--  whether   Source   (Pointer)  is  neither  a  letter  nor  a  digit.
--  Check_Matched is never called with Pointer outside Source'Range.
--
with Strings_Edit.UTF8;       use Strings_Edit.UTF8;
with Strings_Edit.UTF8.Maps;  use Strings_Edit.UTF8.Maps;

with Strings_Edit.UTF8.Maps.Constants;

generic
   with procedure Check_Spelling (Name : String) is <>;
   with function Check_Matched (Source : String; Pointer : Integer)
      return Boolean is <>;
   Blanks  : Unicode_Set := -- Tabulator and spaces
      Strings_Edit.UTF8.Maps.Constants.Blanks_Set;
   Ignored : Unicode_Set := -- Hyphen and others
      Strings_Edit.UTF8.Maps.Constants.Other_Format_Set;
package Tables.UTF8_Names is
   type Dictionary is new Table with private;
--
-- Add -- Overrides Tables...
--
-- Constraint_Error   is   propagated  if  Name  is  spelt  incorrectly.
-- Data_Error is propagated when Name is an invalid, yet properly  spelt
-- UTF-8 string.
--
   procedure Add
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag
             );
   procedure Add
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag;
                Offset : out Positive
             );
--
-- Canonize -- Get canonic form of a name
--
--    Name - To canonize
--
-- This function returns Name with any code points from Ignored removed.
--
-- Returns :
--
--    The canonic form of
--
-- Exceptions :
--
--    Data_Error - Name is not a valid UTF-8 string
--
   function Canonize (Name : String) return String;
--
-- Delete -- Overrides Tables...
--
   procedure Delete (Folder : in out Dictionary; Name : String);
--
-- Find -- Overrides Tables...
--
-- End_Error is propagated when Name is an invalid UTF-8 string
--
   function Find (Folder : Dictionary; Name : String) return Tag;
   pragma Inline (Find);
--
-- Get -- Overrides Tables...
--
-- When Source is invalid, only its valid prefix is matched.
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Dictionary;
                Data    : out Tag
             );
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Dictionary;
                Data    : out Tag;
                Got_It  : out Boolean
             );
--
-- IsIn -- Overrides Tables...
--
   function IsIn (Folder : Dictionary; Name : String) return Boolean;
   pragma Inline (IsIn);
--
-- Locate -- Overrides Tables...
--
   function Locate (Folder : Dictionary; Name : String) return Natural;
   procedure Locate
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Dictionary;
                Offset  : out Natural
             );
--
-- Replace -- Overrides Tables...
--
-- Constraint_Error  is  propagated  if  Name  is  spelled  incorrectly.
-- Data_Error is propagated when Name is an invalid, yet properly  spelt
-- UTF-8 string.
--
   procedure Replace
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag
             );
   procedure Replace
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag;
                Offset : out Positive
             );
private
   type Dictionary is new Table with null record;
--
-- Search -- Overrides Tables...
--
   function Search (Folder : Dictionary; Name : String)
      return Integer;

end Tables.UTF8_Names;
