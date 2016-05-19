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

with Ada.Strings.Wide_Maps;

package ZanyBlue.Text.Pseudo is

   use Ada.Strings.Wide_Maps;

   type Pseudo_Map_Type is tagged private;
   type Pseudo_Map_Access is access Pseudo_Map_Type;

   type Pseudo_Character_Map is
      record
         Source : Wide_Character;
         Target : Wide_Character;
      end record;

   type Pseudo_Map_Vector is array (Positive range <>) of Pseudo_Character_Map;

   procedure Add_Mapping (Pseudo_Map : in out Pseudo_Map_Type;
                          Mapping    : Pseudo_Map_Vector);
   --  Add vector of character mappings to the pseudo map.

   function Map (Pseudo_Map : Pseudo_Map_Type;
                 Ch         : Wide_Character) return Wide_Character;
   --  Return the mapping for a character wrt a pseudo map.

   function Pseudo_Start return Wide_Character;
   function Pseudo_End return Wide_Character;
   --  Characters used to mark the start and end of a formatted message.

   function Format_Start return Wide_Character;
   function Format_End return Wide_Character;
   --  Characters used to mark the start and end of a formatted argument.

   function Null_Map return Pseudo_Map_Vector;
   function Uppercase_Map return Pseudo_Map_Vector;
   function Lowercase_Map return Pseudo_Map_Vector;
   function Halfwidth_Forms_Map return Pseudo_Map_Vector;
   function Enclosed_Alphanumeric_Map return Pseudo_Map_Vector;
   --  Standard pseudo maps.

private

   type Pseudo_Map_Type is tagged
      record
         Mapping : Wide_Character_Mapping;
      end record;

end ZanyBlue.Text.Pseudo;
