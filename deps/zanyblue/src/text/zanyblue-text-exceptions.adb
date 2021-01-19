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

package body ZanyBlue.Text.Exceptions is

   function Create
     (Name        : Wide_String;
      Message     : Wide_String;
      Information : Wide_String)
      return Exception_Argument_Type;
   --  Create argument type based on the exception information converted
   --  from Strings assumed to UTF-8 strings to Wide_Strings.

   ------------
   -- Create --
   ------------

   function Create
     (Value : Exception_Occurrence)
      return Exception_Argument_Type
   is
   begin
      return
        Create
          (Wide_From_UTF8 (Exception_Name (Value)),
           Wide_From_UTF8 (Exception_Message (Value)),
           Wide_From_UTF8 (Exception_Information (Value)));
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Name        : Wide_String;
      Message     : Wide_String;
      Information : Wide_String)
      return Exception_Argument_Type
   is
   begin
      return
        Exception_Argument_Type'
          (N_Length    => Name'Length, M_Length => Message'Length,
           I_Length    => Information'Length, Name => Name, Message => Message,
           Information => Information);
   end Create;

   ------------
   -- Format --
   ------------

   overriding function Format
     (Value     : Exception_Argument_Type;
      Type_Name : Wide_String;
      Template  : Wide_String;
      Locale    : Locale_Type)
      return Wide_String
   is
      pragma Unreferenced (Type_Name);
      pragma Unreferenced (Locale);
   begin
      if Template = "name" then
         return Value.Name;
      elsif Template = "info" then
         return Value.Information;
      else
         return Value.Message;
      end if;
   end Format;

end ZanyBlue.Text.Exceptions;
