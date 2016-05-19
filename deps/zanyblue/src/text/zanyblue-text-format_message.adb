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

with Ada.Strings.Wide_Unbounded;
with Ada.Wide_Characters.Unicode;
with Ada.Containers.Indefinite_Vectors;

----------------------------------
-- ZanyBlue.Text.Format_Message --
----------------------------------

function ZanyBlue.Text.Format_Message
            (Message        : Wide_String;
             Arguments      : ZanyBlue.Text.Arguments.Argument_List;
             Mapping        : ZanyBlue.Text.Pseudo.Pseudo_Map_Access;
             Locale         : ZanyBlue.Text.Locales.Locale_Type;
             Raise_Errors   : Boolean := True;
             Mark_Messages  : Boolean := True;
             Mark_Arguments : Boolean := True;
             Error_Handler  : access Error_Handler_Type'Class
                                 := Standard_Error_Handler'Access)
   return Wide_String
is

   use Ada.Strings.Wide_Unbounded;
   use Ada.Wide_Characters.Unicode;
   use ZanyBlue.Text.Pseudo;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Arguments;

   Done     : exception;
   --  End of input is signaled by raising the Done exception.

   type Source_Buffer (Length : Natural) is
      record
         Buffer : Wide_String (1 .. Length);
         Position : Positive := 1;
      end record;
   --  The "stream of characters" being formatted is simply the input
   --  format string, Message, but is augmented in the case of nested
   --  arguments, e.g, "X: {0,{1}}" to format a value within a field
   --  width which is, itself, an argument.  Such nested arguments are
   --  handled by maintaining a stack of string sources.  When expanding
   --  an argument, if another reference is encountered, the value of
   --  the reference is then pushed onto a stack of strings being
   --  processed.  The Source_Buffer is the record type used to store
   --  these nested references.  The Position is used to track the
   --  current character being consumed.
   package Source_Stacks is
      new Ada.Containers.Indefinite_Vectors (Index_Type => Positive,
                                             Element_Type => Source_Buffer);
   --  The stack of source strings is managed by a simple vector.
   use Ada.Containers;
   use Source_Stacks;

   Zero : constant Natural := Wide_Character'Pos ('0');
   --  Offset value when converting a string of decimal digits to an integer.

   procedure Add_Argument (Buffer : in out Unbounded_Wide_String;
                           Value  : Wide_String);
   --  Add a formatted argument value to the output buffer.

   function Buffered_Next (Last_Buffer : Natural) return Wide_Character;
   --  Get the next character.  There are recursive references to
   --  formatted values so a stack is in use to manage them.  This routine
   --  accesses the stack to get the character.

   function Character_Mapping (Ch : Wide_Character) return Wide_Character;
   --  Return the pseudo translation mapping for a given character.  The
   --  same character is returned if pseudo translation is not enabled.

   function Next return Wide_Character;
   --  Return the next character from the format string.  Calls the
   --  Buffered_Next procedure if the stack of sources is in use, i.e.,
   --  recursive references to arguments, e.g., "{0:{1}}"

   function Parse_Argument (Level : Natural := 0) return Wide_String;
   --  Parse the an argument reference: argument number and format
   --  template.

   procedure Pseudo_Append (Buffer  : in out Unbounded_Wide_String;
                            Ch      : Wide_Character;
                            Enabled : Boolean);
   --  Append a character to the output buffer if pseudo translation
   --  is enabled, otherwise do nothing.

   procedure Push_Source (Data : Wide_String);
   --  Add a new format character source used to handle recursive format
   --  references, e.g., "{0:{1}}"

   Source_Stack : Source_Stacks.Vector;
   Buffer       : Unbounded_Wide_String;
   Ch           : Wide_Character;
   I            : Positive := Message'First;

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument (Buffer : in out Unbounded_Wide_String;
                           Value  : Wide_String)
   is
   begin
      Pseudo_Append (Buffer, Format_Start, Mark_Arguments);
      Append (Buffer, Value);
      Pseudo_Append (Buffer, Format_End, Mark_Arguments);
   end Add_Argument;

   -------------------
   -- Buffered_Next --
   -------------------

   function Buffered_Next (Last_Buffer : Natural) return Wide_Character
   is

      Found  : Boolean := False;
      Result : Wide_Character;

      procedure Get_Character (Buffer : in out Source_Buffer);
      --  Get a character for a buffer source.

      -------------------
      -- Get_Character --
      -------------------

      procedure Get_Character (Buffer : in out Source_Buffer)
      is
      begin
         if Buffer.Position <= Buffer.Buffer'Last then
            Result := Buffer.Buffer (Buffer.Position);
            Buffer.Position := Buffer.Position + 1;
            Found := True;
         end if;
      end Get_Character;

   begin
      Update_Element (Source_Stack, Last_Buffer, Get_Character'Access);
      if not Found then
         Delete_Last (Source_Stack);
         Result := Next;
      end if;
      return Result;
   end Buffered_Next;

   -----------------------
   -- Character_Mapping --
   -----------------------

   function Character_Mapping (Ch : Wide_Character) return Wide_Character
   is
   begin
      if Mapping /= null then
         return Mapping.Map (Ch);
      else
         return Ch;
      end if;
   end Character_Mapping;

   ----------
   -- Next --
   ----------

   function Next return Wide_Character
   is
      Last_Buffer : constant Natural := Natural (Length (Source_Stack));
      Result : Wide_Character;
   begin
      if Last_Buffer = 0 then
         if I > Message'Last then
            raise Done;
         end if;
         Result := Message (I);
         I := I + 1;
      else
         Result := Buffered_Next (Last_Buffer);
      end if;
      return Result;
   end Next;

   --------------------
   -- Parse_Argument --
   --------------------

   function Parse_Argument (Level : Natural := 0) return Wide_String
   is

      function Next_Character return Wide_Character;
      --  Return the next format character.  If the character is '{' then
      --  it's a recursive format reference: format the argument value and
      --  add to the stack, then return the next chararacter.

      --------------------
      -- Next_Character --
      --------------------

      function Next_Character return Wide_Character
      is
         Result : Wide_Character := Next;
      begin
         while Result = '{' loop
            Push_Source (Parse_Argument (Level => Level + 1));
            Result := Next;
         end loop;
         return Result;
      end Next_Character;

      Template : Unbounded_Wide_String;
      Index    : Natural := 0;
      Ch       : Wide_Character;

   begin
      Template := Null_Unbounded_Wide_String;
      Ch := Next_Character;
      if not Is_Digit (Ch) then
         Error_Handler.Illegal_Character (Message, I - Message'First + 1, Ch,
                                          Natural (Length (Source_Stack)),
                                          Raise_Errors);
         --  If an exception was not raised, skip to next closing brace
         while Ch /= '}' loop
            Ch := Next_Character;
         end loop;
      end if;
      while Is_Digit (Ch) loop
         Index := Index * 10 + Wide_Character'Pos (Ch) - Zero;
         Ch := Next_Character;
      end loop;
      if Ch = ',' or else Ch = ':' then
         Ch := Next_Character;
         while Ch /= '}' loop
            Append (Template, Ch);
            Ch := Next_Character;
         end loop;
      else
         if Ch /= '}' then
            Error_Handler.Format_Not_Closed (Message, I - Message'First + 1,
                                             Natural (Length (Source_Stack)),
                                             Raise_Errors);
         end if;
      end if;
      return Arguments.Format (Index, Message, To_Wide_String (Template),
                               Locale, Raise_Errors,
                               Error_Handler => Error_Handler);
   exception
   when Done =>
      Error_Handler.Format_Not_Closed (Message, I - Message'First + 1,
                                       Natural (Length (Source_Stack)),
                                       Raise_Errors);
      --  If the handler decided not to raise an exception, re-raise the Done
      --  exception
      raise Done;
   end Parse_Argument;

   -------------------
   -- Pseudo_Append --
   -------------------

   procedure Pseudo_Append (Buffer  : in out Unbounded_Wide_String;
                            Ch      : Wide_Character;
                            Enabled : Boolean)
   is
   begin
      if Enabled and then Mapping /= null then
         Append (Buffer, Ch);
      end if;
   end Pseudo_Append;

   -----------------
   -- Push_Source --
   -----------------

   procedure Push_Source (Data : Wide_String)
   is
      New_Buffer : Source_Buffer (Data'Length);
   begin
      New_Buffer.Buffer := Data;
      Append (Source_Stack, New_Buffer);
   end Push_Source;

begin
   Pseudo_Append (Buffer, Pseudo_Start, Mark_Messages);
<<String>>
   Ch := Next;
   case Ch is
   when ''' =>
      goto Quote;
   when '{' =>
      goto FormatElement;
   when others =>
      Append (Buffer, Character_Mapping (Ch));
      goto String;
   end case;
<<Quote>>
   Ch := Next;
   case Ch is
   when ''' =>
      Append (Buffer, ''');
      goto String;
   when others =>
      Append (Buffer, Character_Mapping (Ch));
      goto QuotedString;
   end case;
<<QuotedString>>
   Ch := Next;
   case Ch is
   when ''' =>
      goto String;
   when others =>
      Append (Buffer, Character_Mapping (Ch));
      goto QuotedString;
   end case;
<<FormatElement>>
   Add_Argument (Buffer, Parse_Argument);
   goto String;
exception
when Done =>
   Pseudo_Append (Buffer, Pseudo_End, Mark_Messages);
   return To_Wide_String (Buffer);
end ZanyBlue.Text.Format_Message;
