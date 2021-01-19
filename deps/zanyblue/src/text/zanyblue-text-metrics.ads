--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2016, Michael Rohan <mrohan@zanyblue.com>
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

with UXStrings.Text_IO;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Formatting;

package ZanyBlue.Text.Metrics is

   use UXStrings.Text_IO;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Formatting;

   procedure Write_Usage
     (Destination : UXStrings.Text_IO.File_Type;
      Catalog     : Catalog_Type := Standard_Catalog);
   --  Write a summary of the usage of each message in the catalog to the
   --  given file as an XML document.  This should be call after the
   --  application has executed, e.g.,
   --
   --    <?xml version="1.0" encoding="utf-8"?>
   --    <zanyblue-message-usage>
   --       <message facility="myfac"
   --                key="mykey"
   --                locale="mylocale"
   --                count="10" />
   --    </zanyblue-message-usage>
   --

   procedure Write_Usage
     (File_Name : String;
      Catalog   : Catalog_Type := Standard_Catalog);
   --  Write the message usage summary to the named file.

--     procedure Write_Usage
--       (File_Name : String;
--        Catalog   : Catalog_Type := Standard_Catalog);
   --  Write the message usage summary to the named file.

end ZanyBlue.Text.Metrics;
