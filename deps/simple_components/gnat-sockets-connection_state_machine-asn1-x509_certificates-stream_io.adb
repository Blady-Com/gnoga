--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.X509_Certificate.Stream_IO             Summer, 2019       --
--  Implementation                                                    --
--                                Last revision :  18:41 01 Aug 2019  --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.IO_Exceptions;    use Ada.IO_Exceptions;
with Ada.Streams;          use Ada.Streams;
with Strings_Edit.Base64;  use Strings_Edit.Base64;

package body GNAT.Sockets.Connection_State_Machine.ASN1.
             X509_Certificates.Stream_IO is

   Prefix : constant String := "-----BEGIN CERTIFICATE-----";
   Suffix : constant String := "-----END CERTIFICATE-----";
   LF     : constant Stream_Element := 10;

   procedure Process_Packet
             (  Certificate : in out X509_Certificate_Data
             )  is
   begin
      Certificate.Completed := True;
   end Process_Packet;

   procedure Read
             (  Stream      : in out Root_Stream_Type'Class;
                Certificate : in out X509_Certificate_Data
             )  is
      Pointer : Integer := Prefix'First;
      This    : Stream_Element_Array (1..1);
      Last    : Stream_Element_Offset;
      Decoder : aliased Base64_Decoder (80);

      procedure Flush is
      begin
         if not Is_Empty (Decoder) then
            declare
               Data  : Stream_Element_Array (1..Used (Decoder));
               Last  : Stream_Element_Offset;
               Index : Stream_Element_Offset := 1;
            begin
               Read (Decoder, Data, Last);
               Index := 1;
               Received (Certificate, Data (Index..Last), Index);
            end;
         end if;
      end Flush;
   begin
      loop
         Read (Stream, This (1..1), Last);
         if Last /= 1 then
            raise End_Error;
         end if;
         if Character'Pos (Prefix (Pointer)) = This (1) then
            exit when Pointer = Prefix'Last;
            Pointer := Pointer + 1;
         else
            Pointer := Prefix'First;
         end if;
      end loop;
      Connected (Certificate);
      loop
         Read (Stream, This (1..1), Last);
         if Last /= 1 then
            raise End_Error;
         end if;
         case This (1) is
            when Character'Pos ('-') =>
               Flush;
               Pointer := Suffix'First + 1;
               exit;
            when 9..13 | Character'Pos (' ') =>
               null;
            when others =>
               if Is_Full (Decoder) then
                  Flush;
               end if;
               Write (Decoder, This);
         end case;
      end loop;
      loop
         Read (Stream, This (1..1), Last);
         if Last /= 1 then
            raise End_Error;
         end if;
         if Character'Pos (Suffix (Pointer)) = This (1) then
            exit when Pointer = Suffix'Last;
            Pointer := Pointer + 1;
         else
            Raise_Exception
            (  Data_Error'Identity,
               "Missing -----END CERTIFICATE----- at the end"
            );
         end if;
      end loop;
   end Read;

   procedure Write
             (  Stream      : in out Root_Stream_Type'Class;
                Certificate : X509_Certificate_Data
             )  is
      Encoder : Base64_Encoder ((65 * 4 + 2) / 3);
      Size    : Stream_Element_Offset := 1024 * 2;
      Line    : Stream_Element_Array (1..65);
   begin
      Line (65) := LF;
      loop
         declare
            Buffer   : Stream_Element_Array (1..Size);
            From, To : Stream_Element_Offset := 1;
            Count    : Stream_Element_Count;
            Encoded  : Boolean := False;
         begin
            begin
               Encode (Certificate.Certificate, Buffer, To);
               Encoded := True;
            exception
               when End_Error => -- Increase buffer
                  Size := Size * 2;
            end;
            if Encoded then
               To := To - 1;
               String'Write (Stream'Unchecked_Access, Prefix);
               Stream_Element'Write (Stream'Unchecked_Access, LF);
               while From <= To loop
                  if Used (Encoder) >= 64 then
                     declare
                        Last : Stream_Element_Offset;
                     begin
                        Read (Encoder, Line (1..64), Last);
                     end;
                     Write (Stream, Line);
                  end if;
                  Count := Stream_Element_Offset'Min
                           (  Free (Encoder),
                              To - From + 1
                           );
                  Write (Encoder, Buffer (From..From + Count - 1));
                  From := From + Count;
               end loop;
               Flush (Encoder);
               declare
                  Last : Stream_Element_Offset;
               begin
                  Read (Encoder, Line (1..Used (Encoder)), Last);
                  Line (Last + 1) := LF;
                  Write (Stream, Line (1..Last + 1));
               end;
               String'Write (Stream'Unchecked_Access, Suffix);
               Stream_Element'Write (Stream'Unchecked_Access, LF);
               exit;
            end if;
         end;
      end loop;
   end Write;

end GNAT.Sockets.Connection_State_Machine.ASN1.X509_Certificates.
    Stream_IO;
