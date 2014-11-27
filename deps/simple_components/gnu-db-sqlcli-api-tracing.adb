--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNU.DB.SQLCLI.API.Tracing                   Luebeck            --
--  Interface                                      Winter, 2002       --
--                                                                    --
--                                Last revision :  11:37 13 Oct 2007  --
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

with Ada.Characters.Latin_1;
with GNU.DB.SQLCLI.Connection_Attribute;
use  GNU.DB.SQLCLI.Connection_Attribute;

package body GNU.DB.SQLCLI.API.Tracing is
   procedure Disable_Tracing
             (  Connection : in out ODBC_Connection'Class
             )  is
   begin
      SQLSetConnectAttr
      (  Connection.Handle,
         Connection_Attribute_Trace'
         (  Attribute => SQL_ATTR_TRACE,
            Value     => SQL_OPT_TRACE_OFF
      )  );
   end Disable_Tracing;
             
   procedure Enable_Tracing
             (  Connection : in out ODBC_Connection'Class;
                Name       : String
             )  is
   begin
      SQLSetConnectAttr
      (  Connection.Handle,
         Connection_Attribute_Trace'
         (  Attribute => SQL_ATTR_TRACE,
            Value     => SQL_OPT_TRACE_OFF
      )  );
      SQLSetConnectAttr
      (  Connection.Handle,
         Connection_Attribute_String'
         (  Attribute => SQL_ATTR_TRACEFILE,
            Value     => Name & Ada.Characters.Latin_1.NUL,
            Len       => Name'Length + 1
      )  );
      SQLSetConnectAttr
      (  Connection.Handle,
         Connection_Attribute_Trace'
         (  Attribute => SQL_ATTR_TRACE,
            Value     => SQL_OPT_TRACE_ON
      )  );
   end Enable_Tracing;
   
end GNU.DB.SQLCLI.API.Tracing;
