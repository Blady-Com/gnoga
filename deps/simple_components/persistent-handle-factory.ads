--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Handle.Factory                   Luebeck            --
--  Interface                                      Winter, 2004       --
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

with APQ;  use APQ;

with Persistent.APQ;
with Persistent.ODBC;

package Persistent.Handle.Factory is
   pragma Elaborate_Body (Persistent.Handle.Factory);
--
-- Create_APQ -- A connection to a data base server using APQ
--
--    Server_Type    - The data base family (Engine_PostgreSQL etc)
--    Data_Base_Name - To connect to (see APQ package)
--    User_Name      - To connect as
--    Password       - Of the user
--    Host_Name      - Of the server running the data base engine
--    Port_Number    - The TCP/IP port listened by the server
--    Erase          - Erase the data base upon connecting
--
-- This  function  establishes a connection to some data base through an
-- APQ bindings. A connection is established to the server specified  by
-- the parameter Host_Name. The  parameter  Server_Type  identifies  the
-- data  base engine. It can be Engine_PostgreSQL, Engine_MySQL etc, one
-- of the supported by APQ engines. User_Name and Password identify  the
-- data base user. Data_Base_Name is the name of a data base managed  by
-- the server. Port_Number specifies the TCP/IP  port  listened  by  the
-- server. When specified as  0,  a  reasonable  default  is  used.  The
-- parameter  Erase  when True erases the data base contents by dropping
-- all  the tables used for storing persistent objects. If the data base
-- contains any additional tables, they remain untouched.
--
-- Returns :
--
--    Handle to the APQ storage
--
-- Exceptions :
--
--    Use_Error  - Password or other data might be wrong
--    Data_Error - Data base error
--
   function Create_APQ
            (  Server_Type    : Database_Type;
               Data_Base_Name : String;
               User_Name      : String;
               Password       : String;
               Host_Name      : String  := "localhost";
               Port_Number    : Natural := 0;
               Erase          : Boolean := False
            )  return Storage_Handle
      renames Persistent.APQ.Create;
--
-- Create_ODBC -- A connection to an ODBC data base server
--
--    Server_Name - The data base server name
--    User_Name   - The user name
--    Password    - The user password
--    Erase       - The data base
--
-- This  function  establishes a connection to some data base through an
-- ODBC driver. The parameter Erase when True removes  all  tables  from
-- the data base.
--
-- Returns :
--
--    Handle to the ODBC data base
--
-- Exceptions :
--
--    Use_Error  - Password or other data might be wrong
--    Data_Error - Data base error
--
   function Create_ODBC
            (  Server_Name : Wide_String;
               User_Name   : Wide_String;
               Password    : Wide_String;
               Erase       : Boolean := False
            )  return Storage_Handle;
   function Create_ODBC
            (  Server_Name : String;
               User_Name   : String;
               Password    : String;
               Erase       : Boolean := False
            )  return Storage_Handle
      renames Persistent.ODBC.Create;
   pragma Inline (Create_ODBC);

end Persistent.Handle.Factory;
