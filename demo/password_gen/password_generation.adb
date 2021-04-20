-- Password_Generation: A package to contain the common logic used by Password_Gen and pwdgen
-- Copyright (C) 2017 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Generation of secure passwords from a domain, master password, and symbol
--
-- V1.0  2017 Nov 15     Move password-generation logic into a package
--
with Ada.Characters.Handling;
with GNAT.SHA512;
with PragmARC.Unbounded_Integers;

package body Password_Generation is
   function Generate
     (Domain      : String;
      Master      : String;
      Length      : Length_Value;
      Symbol      : String;
      Hash_Symbol : Boolean := True)
      return String
   is
      subtype Digit is Character range '0' .. '9';
      subtype Lower is Character range 'a' .. 'z';
      subtype Upper is Character range 'A' .. 'Z';

      function Digest
        (Source : String)
         return String;
      -- Perform an SHA512 digest on Source and convert the result to base 36

      function Digest
        (Source : String)
         return String
      is
         -- Empty
      begin -- Digest
         return
           PragmARC.Unbounded_Integers.Image
             (PragmARC.Unbounded_Integers.Value ("16#" & GNAT.SHA512.Digest (Source) & '#'), Base => 36);
      end Digest;

      Hash_Domain : constant String := Ada.Characters.Handling.To_Lower (Domain);

      Salted_Input : constant String := ':' & Hash_Domain & ':' & Master & ':';

      Digit_1_Index : Natural  := 0;
      Digit_1       : Natural  := 0;
      Digit_1_Rem   : Natural;
      Hash_Length   : Positive := Length; -- The length of the hash (Result) to use in the password
      Letter_Count  : Natural  := 0;
      Has_Lower     : Boolean  := False;
      Has_Upper     : Boolean  := False;
      Result        : String   :=
        Digest (Salted_Input & Integer'Image (Length) & ':' & (if Hash_Symbol then Symbol & ':' else ""));
   begin -- Generate
      if Symbol /= No_Symbol then
         Hash_Length := Hash_Length - 1;
      end if;

      Find_Digit_1 :
      for I in 1 .. Hash_Length loop
         if Result (I) in Digit then
            Digit_1_Index := I;
            Digit_1       := Integer'Value (Result (I .. I));

            exit Find_Digit_1;
         end if;
      end loop Find_Digit_1;

      if Digit_1_Index = 0 then -- Needs a digit
         Digit_1_Index := 1;
         Digit_1       := 0;
         Result (1)    := '0';
      end if;

      Digit_1_Rem := Digit_1 rem 2;

      Lower_Some :
      for I in 1 .. Hash_Length loop
         if Result (I) in Upper then
            Letter_Count := Letter_Count + 1;

            if Letter_Count rem 2 = Digit_1_Rem then
               Result (I) := Ada.Characters.Handling.To_Lower (Result (I));
            end if;
         end if;
      end loop Lower_Some;

      Check_Letters :
      for I in 1 .. Hash_Length loop
         if Result (I) in Lower then
            Has_Lower := True;
         end if;

         if Result (I) in Upper then
            Has_Upper := True;
         end if;
      end loop Check_Letters;

      -- Hash_Length >= 7, so if not Has_Lower, then Result (1 .. Hash_Length) has at most 1 letter, which was not lowered by
      -- Lower_Some
      -- if not Has_Upper, then Result (1 .. Hash_Length) has at most 1 letter, which was lowered by Lower_Some
      -- if both not Has_Lower and not Has_Upper, then Result (1 .. Hash_Length) is all digits
      -- In all of these cases, there are digits afer Digit_1_Index that can be replaced by the desired kind of letter

      if not Has_Lower then -- Needs a lower-case letter
         Find_Digit_Lower :
         for I in Digit_1_Index + 1 .. Hash_Length loop
            if Result (I) in Digit then
               Result (I) := 'a';

               exit Find_Digit_Lower;
            end if;
         end loop Find_Digit_Lower;
      end if;

      if not Has_Upper then -- Needs an upper-case letter
         Find_Digit_Upper :
         for I in Digit_1_Index + 1 .. Hash_Length loop
            if Result (I) in Digit then
               Result (I) := 'A';

               exit Find_Digit_Upper;
            end if;
         end loop Find_Digit_Upper;
      end if;

      if Symbol = No_Symbol then
         return Result (1 .. Hash_Length);
      end if;

      return
        Result (1 .. Integer'Min (Digit_1, Hash_Length)) &
        (if Symbol = Auto_Symbol then (1 => Symbol_Set (Digit_1)) else Symbol) & Result (Digit_1 + 1 .. Hash_Length);
   end Generate;
end Password_Generation;
