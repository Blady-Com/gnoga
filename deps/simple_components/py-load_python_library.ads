--                                                                    --
--  package Py.Load_Python_Library  Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2018       --
--                                                                    --
--                                Last revision :  08:30 04 Aug 2022  --
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

package Py.Load_Python_Library is
--
-- Get_Default_Name -- The default Python relocatable library name
--
--    Major - The major version to look after
--
-- This function under  Windows returns the expected name of Python DLL.
-- Under Linux  where the library name is not fixed  it tries  to find a
-- the library libpython3.*.so.*  under /usr/lib  or its subfolders with
-- the most major version greater than or equal to Major.
--
-- Returns :
--
--    The name of the library
--
   function Get_Default_Name (Major : Natural := 7) return String;
--
-- Get_Extension -- The default Python relocatable library extension
--
-- Returns :
--
--    The extension pattern, e.g. "*.dll"
--
   function Get_Extension return String;
--
-- Get_Python_Path -- The Python installation path
--
--    Major - The major version to look after
--
-- This function  returns  empty  string  under Linux  where  Python  is
-- installed under a standard location.  Under Windows it determines the
-- Python  installation  directory  of  at  least  the  specified  major
-- version.
--
-- Returns :
--
--    The path
--
   function Get_Python_Path (Major : Natural := 7) return String;
--
-- Is_Loaded -- Python load check
--
-- Returns :
--
--    True if the Python library is loaded
--
   function Is_Loaded return Boolean;
--
-- Load -- The Python library
--
--    Name - Of the library or empty if the default should be used
--
-- This procedure does nothing if the library is already loaded.
--
   procedure Load (Name : String := "");

end Py.Load_Python_Library;
