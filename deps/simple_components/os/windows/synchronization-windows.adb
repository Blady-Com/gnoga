--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Windows                     Luebeck            --
--  Implementation                                 Spring, 2018       --
--                                                           --
--                                Last revision :  11:33 15 Jul 2018  --
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

package body Synchronization.Windows is

   Last : constant Duration := Duration (16#FFFF_FFFF# / 1000);

   function Milliseconds (Value : Duration) return DWORD is
   begin
      if Value <= 0.0 then
         return 0;
      elsif Value >= Last then
         return INFINITE;
      else
         return DWORD (Value) * 1000;
      end if;
   end Milliseconds;

   FORMAT_MESSAGE_ALLOCATE_BUFFER : constant := 16#0100#;
   FORMAT_MESSAGE_IGNORE_INSERTS  : constant := 16#0200#;
   FORMAT_MESSAGE_FROM_STRING     : constant := 16#0400#;
   FORMAT_MESSAGE_FROM_HMODULE    : constant := 16#0800#;
   FORMAT_MESSAGE_FROM_SYSTEM     : constant := 16#1000#;
   FORMAT_MESSAGE_ARGUMENT_ARRAY  : constant := 16#2000#;
   FORMAT_MESSAGE_MAX_WIDTH_MASK  : constant := 16#00FF#;

   function FormatMessage
            (  Flags      : DWORD := FORMAT_MESSAGE_FROM_SYSTEM or
                                     FORMAT_MESSAGE_IGNORE_INSERTS or
                                     FORMAT_MESSAGE_ARGUMENT_ARRAY;
               Source     : chars_ptr := Null_Ptr;
               MessageId  : DWORD;
               LanguageId : DWORD := 0;
               Buffer     : access char;
               Size       : DWORD;
               Arguments  : System.Address := Null_Address
            )  return DWORD;
   pragma Import (Stdcall, FormatMessage, "FormatMessageA");

   function GetErrorText (Error : DWORD) return String is
      Buffer : char_array (1..2048);
      Length : DWORD;
   begin
      Length := FormatMessage
                (  MessageId => Error,
                   Buffer    => Buffer (1)'Access,
                   Size      => Buffer'Length
                );
      declare
         Message : String (1..Natural (Length));
      begin
         for Index in 1..size_t (Length) loop
            Message (Positive (Index)) :=
               Character'Val (char'Pos (Buffer (Index)));
         end loop;
         for Index in reverse Message'Range loop
            case Message (Index) is
               when Character'Val (0)..' ' =>
                  null;
               when others =>
                  return Message (1..Index) &
                         " [" & Image (Integer (Error)) & "]";
            end case;
         end loop;
         return "[" & Image (Integer (Error)) & "]";
      end;
   end GetErrorText;

   procedure Raise_From_LastError
             (  ID     : Exception_ID;
                Prefix : String := "";
                Error  : DWORD
             )  is
   begin
      if Prefix'Length > 0 then
         Raise_Exception (ID, Prefix & GetErrorText (Error));
      else
         Raise_Exception (ID, GetErrorText (Error));
      end if;
   end Raise_From_LastError;

end Synchronization.Windows;
