--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Server.Secure.                 Luebeck            --
--     Anonymous                                   Winter, 2015       --
--  Interface                                                         --
--                                Last revision :  20:28 27 May 2018  --
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

package GNAT.Sockets.Server.Secure.Anonymous is
--
-- The priorities applied if not set explicitly
--
   Default_Priorities : constant String := "NORMAL";
--
-- Anonymous_Authentication_Factory -- Abstract  factory  for  anonymous
--                                     authentication
--
   type Anonymous_Authentication_Factory is
      new Abstract_GNUTLS_Factory with private;
--
-- Set_Priorities -- Set priorities for the anonymous authentication
--
--    Factory    - The anonymous credentials connection factory
--    Priorities - The priorities to set
--
   procedure Set_Priorities
             (  Factory    : in out Anonymous_Authentication_Factory;
                Priorities : String
             );
--
-- Overriddes ...HTTP_Server.Secure...
--
   procedure Prepare
             (  Factory : in out Anonymous_Authentication_Factory;
                Client  : in out Connection'Class;
                Session : in out Session_Type
             );
--
-- Overriddes ...HTTP_Server.Secure...
--
   procedure Finalize
             (  Factory : in out Anonymous_Authentication_Factory
             );
--
-- Overriddes ...HTTP_Server.Secure...
--
   procedure Initialize
             (  Factory : in out Anonymous_Authentication_Factory
             );

private
   type Priorities_Ptr is access String;
   type Anonymous_Authentication_Factory is
      new Abstract_GNUTLS_Factory with
   record
      Credentials : Anon_Server_Credentials;
      Parameters  : DH_Params;
      Priorities  : Priorities_Ptr;
   end record;

end GNAT.Sockets.Server.Secure.Anonymous;
