--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.X509_Certificate.Stream_IO             Summer, 2019       --
--  Interface                                                         --
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

package GNAT.Sockets.Connection_State_Machine.ASN1.X509_Certificates.
        Stream_IO is
--
-- X509_Certificate_Data -- The streaming object
--
--    Size - Of the buffer to keep certificate data
--
-- The data type encapsulates the X.509 certificate ASN.1 representation
-- object  and the buffer  that keeps the dynamic certificate data.  The
-- object  can read from and written into stream in the format customary
-- for keeping certificates.
--
   type X509_Certificate_Data (Size : Positive) is
      new State_Machine (1, 1) with
   record
      Buffer      : External_String_Buffer (Size);
      Certificate : X509_Certificate;
      Completed   : Boolean := False;
   end record;
--
-- Read -- Certificate from input stream
--
--    Stream      - The stream
--    Certificate - The certificate data to read
--
-- The stream is  read until -----BEGIN CERTIFICATE-----  appears.  Then
-- the certificate  is input until  -----END CERTIFICATE-----,  which is
-- also consumed.
--
-- Exceptions :
--
--    Data_Error    - Errors in the certificate
--    End_Error     - Premature stream end
--    Storage_Error - The buffer is too small to hold the certificate
--    I/O exceptions
--
   procedure Read
             (  Stream      : in out Root_Stream_Type'Class;
                Certificate : in out X509_Certificate_Data
             );
--
-- Write -- Certificate into output stream
--
--    Stream      - The stream
--    Certificate - The certificate data to read
--
-- First -----BEGIN CERTIFICATE-----  is written.  Then  the certificate
-- encoded in ASN.1 and then Base64. After that the certificate the line
-- -----END CERTIFICATE----- is written.
--
-- Exceptions :
--
--    Data_Error - Errors in the certificate
--    I/O exceptions
--
   procedure Write
             (  Stream      : in out Root_Stream_Type'Class;
                Certificate : X509_Certificate_Data
             );
private
   procedure Process_Packet
             (  Certificate : in out X509_Certificate_Data
             );

end GNAT.Sockets.Connection_State_Machine.ASN1.X509_Certificates.
    Stream_IO;
