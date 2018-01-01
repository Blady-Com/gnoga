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

with Ada.Exceptions;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;

private package ZanyBlue.Text.Indexed_Strings is

   use Ada.Containers;
   use Ada.Exceptions;

   No_Such_Item : exception;

   type Indexed_Strings_Type is tagged private;

   function Length (Indexed_Strings : Indexed_Strings_Type) return Natural;
   --  Return the number of items currently stored in the indexed map.

   function Get (Indexed_Strings : Indexed_Strings_Type;
                 Index           : Positive) return Wide_String;
   --  Get the name associated with a particular index.  Raises the
   --  exception No_Such_Item if the index does not exist.

   function Get (Indexed_Strings : Indexed_Strings_Type;
                 Name            : Wide_String;
                 Id              : Exception_Id) return Positive;
   --  Find index associated with a particular name.  If not present,
   --  the argument exception is raised.

   procedure Add (Indexed_Strings : in out Indexed_Strings_Type;
                  Name            : Wide_String;
                  Index           : out Positive);
   --  Add a new name to the set.  The name is associated with the next
   --  index value, i.e., Length + 1.  This is a noop if the name already
   --  exists in the set.

private

   package Id_To_Name_Vectors is
      new Indefinite_Vectors (Index_Type   => Positive,
                              Element_Type => Wide_String);

   package Name_To_Id_Maps is
      new Indefinite_Hashed_Maps (Key_Type        => Wide_String,
                                  Element_Type    => Positive,
                                  Hash            => ZanyBlue.Text.Wide_Hash,
                                  Equivalent_Keys => "=");

   type Indexed_Strings_Type is tagged
      record
         Name_To_Id : Name_To_Id_Maps.Map;
         Id_To_Name : Id_To_Name_Vectors.Vector;
      end record;

end ZanyBlue.Text.Indexed_Strings;
