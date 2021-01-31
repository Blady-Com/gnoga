--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2016, Michael Rohan <mrohan@zanyblue.com>
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

with Ada.Containers;
with ZanyBlue.Text.Codecs;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Generic_Modulars;
with ZanyBlue.Text.Generic_Enumerations;

with UXStrings;

package ZBInfo is

   use Ada.Containers;
   use ZanyBlue.Text.Codecs;
   use ZanyBlue.Text.Locales;
   use UXStrings;

   subtype String is UXString;

   package Hash_Type_Arguments is new ZanyBlue.Text.Generic_Modulars
     (Hash_Type);
   package Date_Time_Style_Arguments is new ZanyBlue.Text.Generic_Enumerations
     (Date_Time_Style_Type);
   package Day_Period_Arguments is new ZanyBlue.Text.Generic_Enumerations
     (Day_Period_Type);
   package Day_Arguments is new ZanyBlue.Text.Generic_Enumerations (Day_Type);
   package Era_Arguments is new ZanyBlue.Text.Generic_Enumerations (Era_Type);
   package Month_Arguments is new ZanyBlue.Text.Generic_Enumerations
     (Month_Type);
   package Numeric_Style_Arguments is new ZanyBlue.Text.Generic_Enumerations
     (Numeric_Style_Type);
   package Numeric_Item_Arguments is new ZanyBlue.Text.Generic_Enumerations
     (Numeric_Item_Type);
   package Text_Layout_Arguments is new ZanyBlue.Text.Generic_Enumerations
     (Text_Layout_Type);
   package Implementation_Arguments is new ZanyBlue.Text.Generic_Enumerations
     (Implementation_Type);

end ZBInfo;
