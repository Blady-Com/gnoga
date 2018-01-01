--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2017, Michael Rohan <mrohan@zanyblue.com>
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

package body ZanyBlue.Text.Indexed_Strings is

   ---------
   -- Add --
   ---------

   procedure Add (Indexed_Strings : in out Indexed_Strings_Type;
                  Name            : Wide_String;
                  Index           : out Positive) is
      use type Name_To_Id_Maps.Cursor;
      Position : constant Name_To_Id_Maps.Cursor :=
                             Indexed_Strings.Name_To_Id.Find (Name);
   begin
      if Position = Name_To_Id_Maps.No_Element then
         Indexed_Strings.Id_To_Name.Append (Name);
         Index := Positive (Indexed_Strings.Id_To_Name.Length);
         Indexed_Strings.Name_To_Id.Insert (Name, Index);
      else
         Index := Name_To_Id_Maps.Element (Position);
      end if;
   end Add;

   ---------
   -- Get --
   ---------

   function Get (Indexed_Strings : Indexed_Strings_Type;
                 Index           : Positive) return Wide_String is
   begin
      if Index <= Indexed_Strings.Length then
         return Indexed_Strings.Id_To_Name.Element (Index);
      else
         raise No_Such_Item;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Indexed_Strings : Indexed_Strings_Type;
                 Name            : Wide_String;
                 Id              : Exception_Id) return Positive is

      use type Name_To_Id_Maps.Cursor;
      Position : constant Name_To_Id_Maps.Cursor :=
                             Indexed_Strings.Name_To_Id.Find (Name);

   begin
      if Position /= Name_To_Id_Maps.No_Element then
         return Name_To_Id_Maps.Element (Position);
      else
         Raise_Exception (Id, Message => Wide_To_UTF8 (Name));
      end if;
   end Get;

   ------------
   -- Length --
   ------------

   function Length (Indexed_Strings : Indexed_Strings_Type)
      return Natural is
   begin
      --  ASSERT: Id_To_Name.Length = Name_To_Id.Length
      return Natural (Indexed_Strings.Id_To_Name.Length);
   end Length;

end ZanyBlue.Text.Indexed_Strings;
