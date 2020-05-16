--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Server.Secure.Anonymous        Luebeck            --
--  Implementation                                 Winter, 2015       --
--                                                                    --
--                                Last revision :  08:25 05 May 2020  --
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

package body GNAT.Sockets.Server.Secure.Anonymous is

   procedure Free is
      new Ada.Unchecked_Deallocation (String, Priorities_Ptr);

   procedure Finalize
             (  Factory : in out Anonymous_Authentication_Factory
             )  is
   begin
      Finalize (Abstract_GNUTLS_Factory (Factory));
      Free (Factory.Priorities);
   end Finalize;

   procedure Initialize
             (  Factory : in out Anonymous_Authentication_Factory
             )  is
      Bits : Natural;
   begin
      Initialize (Abstract_GNUTLS_Factory (Factory));
      Bits := Sec_Param_To_PK_Bits (PK_DH, SEC_PARAM_LEGACY);
      if Bits > 0 then
         DH_Params_Generate2
         (  Factory.Parameters,
            Bits
         );
      else
         DH_Params_Generate2
         (  Factory.Parameters,
            1024
         );
      end if;
   end Initialize;

   procedure Prepare
             (  Factory : in out Anonymous_Authentication_Factory;
                Client  : in out Connection'Class;
                Session : in out Session_Type
             )  is
   begin
      if Factory.Priorities = null then
         Priority_Set_Direct (Session, Default_Priorities);
      else
         Priority_Set_Direct (Session, Factory.Priorities.all);
      end if;
      Credentials_Set (Session, Factory.Credentials);
      Certificate_Server_Set_Request (Session, Cert_Ignore);
   end Prepare;

   procedure Set_Priorities
             (  Factory    : in out Anonymous_Authentication_Factory;
                Priorities : String
             )  is
   begin
      if Factory.Priorities /= null then
         Free (Factory.Priorities);
      end if;
      Factory.Priorities := new String'(Priorities);
   end Set_Priorities;

end GNAT.Sockets.Server.Secure.Anonymous;
