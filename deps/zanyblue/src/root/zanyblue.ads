--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, Michael Rohan <mrohan@zanyblue.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--    * Neither the name of ZanyBlue nor the names of its contributors may
--      be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--
--  Root package for the ZanyBlue library.  This is pure package containing
--  function definitions giving version information.
--

package ZanyBlue is

   pragma Pure;

   type Version_Status_Type is (Alpha, Beta, Production);
   --  There are three supported releases statuses:
   --    * Alpha for development code
   --    * Beta for beta releases
   --    * Production for production releases

   function Version_Major return Natural;
   --  The major version number associated with the ZanyBlue release.

   function Version_Minor return Natural;
   --  The minor version number associated with the ZanyBlue release.

   function Version_Patch return Natural;
   --  The patch version number associated with the ZanyBlue release.
   --  This should normally be 0 unless a serious issue was encountered
   --  with a release.

   function Version_Status return Version_Status_Type;
   --  The release status, see Version_Status_Type for the possible
   --  enumeration values.

   function Revision return Wide_String;
   --  The Subversion revision number for the build.

   function Copyright_Year return Positive;
   --  The copyright year for the build.

   procedure Debug;
   pragma Convention (C, Debug);
   --  Utiltiy routine to aid with setting breakpoint within GDB.  The
   --  C convention making naming from GDB easier.

   Max_Float_Precision : constant := 5000;
   --  Maximum precision supported for floating point formatting (number of
   --  digits printed after the decimal point.  Requests to format floating
   --  point numbers with a precision greater than this will be truncated to
   --  this value, e.g., "10.10000e" => "10.5000e".

end ZanyBlue;
