--                                                                    --
--  package GNAT.Sockets.SMTP       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2016       --
--                                                                    --
--                                Last revision :  09:54 04 Feb 2017  --
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

with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body GNAT.Sockets.SMTP is

   function Image (Code : Reply_Code) return String is
   begin
      return Image (Integer (Code));
   end Image;

   function Image (Code : Enhanced_Status_Code) return String is
   begin
      case Code.Class is
         when Success =>
            return
            (  "2."
            &  Image (Integer (Code.Subject))
            &  "."
            &  Image (Integer (Code.Detail))
            );
         when Persistent_Transient_Failure =>
            return
            (  "4."
            &  Image (Integer (Code.Subject))
            &  "."
            &  Image (Integer (Code.Detail))
            );
         when Permanent_Failure =>
            return
            (  "5."
            &  Image (Integer (Code.Subject))
            &  "."
            &  Image (Integer (Code.Detail))
            );
      end case;
   end Image;

   function Image (Code : Error_Code) return String is
   begin
      if Code.Enhanced then
         declare
            Message : constant String := Text (Code.Error);
         begin
            if Message = "Unknown" then -- Use reply instead
               return
               (  Text (Code.Reply)
               &  " ["
               &  Image (Code.Reply)
               &  " "
               &  Image (Code.Error)
               &  "]"
               );
            else
               return
               (  Message
               &  " ["
               &  Image (Code.Reply)
               &  " "
               &  Image (Code.Error)
               &  "]"
               );
            end if;
         end;
      else
         return Text (Code.Reply) & " [" & Image (Code.Reply) & "]";
      end if;
   end Image;

   function Text (Code : Reply_Code) return String is
   begin
      case Code is
         when 211 =>
            return "System status, or system help reply";
         when 214 =>
            return "Help message";
         when 220 =>
            return "Service ready";
         when 221 =>
            return "Service closing transmission channel";
         when 250 =>
            return "Requested mail action okay, completed";
         when 251 =>
            return "User not local; will forward";
         when 252 =>
            return "Cannot VRFY user, but will accept message and " &
                   "attempt delivery";
         when 354 =>
            return "Start mail input; end with <CRLF>.<CRLF>";
         when 421 =>
            return "Service not available, closing " &
                   "transmission channel";
         when 450 =>
            return "Requested mail action not taken: " &
                   "mailbox unavailable";
         when 451 =>
            return "Requested action aborted: " &
                   "local error in processing";
         when 452 =>
            return "Requested action not taken: " &
                   "insufficient system storage";
         when 500 =>
            return "Syntax error, command unrecognised";
         when 501 =>
            return "Syntax error in parameters or arguments";
         when 502 =>
            return "Command not implemented";
         when 503 =>
            return "Bad sequence of commands";
         when 504 =>
            return "Command parameter not implemented";
         when 521 =>
            return "Domain does not accept mail";
         when 530 =>
            return "Access denied";
         when 550 =>
            return "Requested action not taken: mailbox unavailable";
         when 551 =>
            return "User not local";
         when 552 =>
            return "Requested mail action aborted: " &
                   "exceeded storage allocation";
         when 553 =>
            return "Requested action not taken: " &
                   "mailbox name not allowed";
         when 554 =>
            return "Transaction failed";
         when others =>
            if Code <= 299 then
               return "The requested action has been " &
                      "successfully completed";
            elsif Code <= 399 then
               return "Receipt of further information is pending";
            elsif Code <= 499 then
               return "The command was not accepted, " &
                      "the action may be requested again";
            else
               return "The command was not accepted";
            end if;
      end case;
   end Text;

   function Text (Class : Code_Class) return String is
   begin
      case Class is
         when Success =>
            return "Success";
         when Persistent_Transient_Failure =>
            return "Persistent transient failure";
         when Permanent_Failure =>
            return "Permanent failure";
      end case;
   end Text;

   function Text (Subject : Code_Subject) return String is
   begin
      case Subject is
         when 0 => return "Other or undefined status";
         when 1 => return "Addressing status";
         when 2 => return "Mailbox status";
         when 3 => return "Mail system status";
         when 4 => return "Network and routing";
         when 5 => return "Mail delivery protocol status";
         when 6 => return "Message content or media status ";
         when 7 => return "Security or policy status";
         when others => return "Unknown";
      end case;
   end Text;

   function Text (Code : Enhanced_Status_Code) return String is
   begin
      case Code.Subject is
         when 0 =>
            case Code.Detail is
               when 1 =>  return "Other undefined Status";
               when others => null;
            end case;
         when 1 =>
            case Code.Detail is
               when 0 =>  return "Other address status";
               when 1 =>  return "Bad destination mailbox address";
               when 2 =>  return "Bad destination system address";
               when 3 =>  return "Bad destination mailbox " &
                                 "address syntax";
               when 4 =>  return "Destination mailbox " &
                                 "address ambiguous";
               when 5 =>  return "Destination address valid";
               when 6 =>  return "Destination mailbox has moved, " &
                                 "no forwarding address";
               when 7 =>  return "Bad sender's mailbox address syntax";
               when 8 =>  return "Bad sender's system address";
               when 9 =>  return "Message relayed " &
                                 "to non-compliant mailer";
               when 10 => return "Recipient address has null MX";
               when others => null;
            end case;
         when 2 =>
            case Code.Detail is
               when 0 => return "Other or undefined mailbox status";
               when 1 => return "Mailbox disabled, " &
                                "not accepting messages";
               when 2 => return "Mailbox full";
               when 3 => return "Message length exceeds " &
                                "administrative limit";
               when 4 => return "Mailing list expansion problem";
               when others => null;
            end case;
         when 3 =>
            case Code.Detail is
               when 0 => return "Other or undefined mail system status";
               when 1 => return "Mail system full";
               when 2 => return "System not accepting network messages";
               when 3 => return "System not capable " &
                                "of selected features";
               when 4 => return "Message too big for system";
               when 5 => return "System incorrectly configured";
               when 6 => return "Requested priority was changed";
               when others => null;
            end case;
         when 4 =>
            case Code.Detail is
               when 0 => return "Other or undefined network " &
                                "or routing status";
               when 1 => return "No answer from host";
               when 2 => return "Bad connection";
               when 3 => return "Directory server failure";
               when 4 => return "Unable to route";
               when 5 => return "Mail system congestion";
               when 6 => return "Routing loop detected";
               when 7 => return "Delivery time expired";
               when others => null;
            end case;
         when 5 =>
            case Code.Detail is
               when 0 => return "Other or undefined protocol status";
               when 1 => return "Invalid command";
               when 2 => return "Syntax error";
               when 3 => return "Too many recipients";
               when 4 => return "Invalid command arguments";
               when 5 => return "Wrong protocol version";
               when 6 => return "Authentication exchange line " &
                                "is too long";
               when others => null;
            end case;
         when 6 =>
            case Code.Detail is
               when 0 => return "Other or undefined media error";
               when 1 => return "Media not supported";
               when 2 => return "Conversion required and prohibited";
               when 3 => return "Conversion required but not supported";
               when 4 => return "Conversion with loss performed";
               when 5 => return "Conversion Failed";
               when 6 => return "Message content not available";
               when 7 => return "Non-ASCII addresses not permitted " &
                                "for that sender/recipient";
               when 8 | 10 => return "UTF-8 string reply is " &
                                     "required, but not permitted " &
                                     "by the SMTP client";
               when 9 => return "UTF-8 header message cannot be " &
                                "transferred to one or more " &
                                "recipients, so the message " &
                                "must be rejected";
               when others => null;
            end case;
         when 7 =>
            case Code.Detail is
               when  0 => return "Other or undefined security status";
               when  1 => return "Delivery not authorized, " &
                                 "message refused";
               when  2 => return "Mailing list expansion prohibited";
               when  3 => return "Security conversion required " &
                                "but not possible";
               when  4 => return "Security features not supported";
               when  5 => return "Cryptographic failure";
               when  6 => return "Cryptographic algorithm " &
                                 "not supported";
               when  7 => return "Message integrity failure";
               when  8 => return "Authentication credentials invalid";
               when  9 => return "Authentication mechanism is too weak";
               when 10 => return "Encryption Needed";
               when 11 => return "Encryption required for requested " &
                                 "authentication mechanism";
               when 12 => return "A password transition is needed";
               when 13 => return "User Account Disabled";
               when 14 => return "Trust relationship required";
               when 15 => return "Priority Level is too low";
               when 16 => return "Message is too big for " &
                                 "the specified priority";
               when 17 => return "Mailbox owner has changed";
               when 18 => return "Domain owner has changed";
               when 19 => return "RRVS test cannot be completed";
               when 20 => return "No passing DKIM signature found";
               when 21 => return "No acceptable DKIM signature found";
               when 22 => return "No valid author-matched DKIM " &
                                 "signature found";
               when 23 => return "SPF validation failed";
               when 24 => return "SPF validation error";
               when 25 => return "Reverse DNS validation failed";
               when 26 => return "Multiple authentication " &
                                 "checks failed";
               when 27 => return "Sender address has null MX";
               when others => null;
            end case;
         when others => null;
      end case;
      return "Unknown";
   end Text;

end GNAT.Sockets.SMTP;
