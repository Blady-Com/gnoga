--                                                                    --
--  package GNAT.Sockets.SMTP       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2016       --
--                                                                    --
--                                Last revision :  12:47 19 Jun 2016  --
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

package GNAT.Sockets.SMTP is
--
-- SMTP_Port -- The default SMTP port
--
   SMTP_Port : constant := 25;
--
-- SMTP_Command -- SMTP commands
--
   type SMTP_Command is
        (  SMTP_Greeting, -- Initial greeting from the server
           SMTP_Sent,     -- Confirmation of mail sent or rejected
           SMTP_HELO,
           SMTP_EHLO,
           SMTP_AUTH, -- AUTH command
           SMTP_MAIL,
           SMTP_RCPT,
           SMTP_DATA,
           SMTP_RSET,
           SMTP_STARTTLS,
--         SMTP_VRFY, -- Superfluous, not really supported by servers
--         SMTP_EXPN, -- Not recommended to be supported anyway
           SMTP_HELP,
           SMTP_NOOP,
           SMTP_QUIT
        );
--
-- SMTP_Extension -- SMTP extensions returned by the server
--
   type SMTP_Extension is
        (  SMTP_8BITMIME,
           SMTP_ATRN,
           SMTP_AUTH,
           SMTP_BINARYMIME,
           SMTP_CHECKPOINT,
           SMTP_CHUNKING,
           SMTP_DSN,
           SMTP_ENHANCEDSTATUSCODES,
           SMTP_ETRN,
           SMTP_EXPN,
           SMTP_HELP,
           SMTP_ONEX,
           SMTP_PIPELINING,
           SMTP_RSET,
           SMTP_SAML,
           SMTP_SEND,
           SMTP_SIZE,
           SMTP_SMTPUTF8,
           SMTP_SOML,
           SMTP_STARTTLS,
           SMTP_TIME,
           SMTP_TLS,
           SMTP_TURN,
           SMTP_UTF8SMTP,
           SMTP_VERB,
           SMTP_VRFY,
           SMTP_X_EXPS,
           SMTP_X_EXPS_LOGIN,
           SMTP_X_LINK2STATE,
           SMTP_X_RCPTLIMIT,
           SMTP_X_TURNME,
           SMTP_XADR,
           SMTP_XAUD,
           SMTP_XDSN,
           SMTP_XEXCH50,
           SMTP_XGEN,
           SMTP_XONE,
           SMTP_XQUE,
           SMTP_XREMOTEQUEUE,
           SMTP_XSTA,
           SMTP_XTRN,
           SMTP_XUSR,
           SMTP_XVRB
        );
--
-- SMTP_AUTH_Mechanism
--
   type SMTP_AUTH_Mechanism is mod 2**3;
   SMTP_ANONYMOUS : constant SMTP_AUTH_Mechanism := 0;
   SMTP_PLAIN     : constant SMTP_AUTH_Mechanism := 2**0;
   SMTP_LOGIN     : constant SMTP_AUTH_Mechanism := 2**1;
   SMTP_CRAM_MD5  : constant SMTP_AUTH_Mechanism := 2**2;
--
-- Reply_Code -- The code introducing SMTP server response
--
   type Reply_Code is range 200..554;
   function Image (Code : Reply_Code) return String;
--
-- Text -- Mnemonic of a reply code
--
--    Code - The reply code
--
-- Returns :
--
--    Human-readable text corresponding to the code
--
   function Text (Code : Reply_Code) return String;
--
-- Enhanced_Error_Code -- The enhanced code RFC 1893
--
   type Code_Class is
        (  Success,
           Persistent_Transient_Failure,
           Permanent_Failure
        );
   type Code_Subject is range 0..999;
   type Code_Detail  is range 0..999;

   type Enhanced_Status_Code is record
      Class   : Code_Class;
      Subject : Code_Subject;
      Detail  : Code_Detail;
   end record;
   function Image (Code : Enhanced_Status_Code) return String;
--
-- Text -- Mnemonic of an enhanced error code
--
--    Code - The error code / class / subject
--
-- Returns :
--
--    Human-readable text corresponding to the code
--
   function Text (Code    : Enhanced_Status_Code) return String;
   function Text (Class   : Code_Class)   return String;
   function Text (Subject : Code_Subject) return String;
--
-- Error_Code -- The composite error code
--
   type Error_Code (Enhanced : Boolean := False) is record
      Reply : Reply_Code;
      case Enhanced is
         when True =>
            Error : Enhanced_Status_Code;
         when False =>
            null;
      end case;
   end record;
   function Image (Code : Error_Code) return String;

end GNAT.Sockets.SMTP;
