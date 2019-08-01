-- Password_Generation: A package to contain the common logic used by Password_Gen and pwdgen
-- Copyright (C) 2018 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Generation of secure passwords from a domain, master password, and symbol
-- With many accounts on many web sites, it is important for security to have a unique, secure
-- password for each
-- A good way to do this is to have a single, secure, master password which can be used to
-- generate a unique but reproducible password for each site; being reproducible, it need not be
-- remembered, but can be regenerated at will as needed
-- The concepts are:
-- 1. The domain: this is the domain of the web site that the password is for (example.com,
--    example.co.uk, example.fr, ...)
-- 2. The master password: the only password the user must remember. It should be secure, as
--    brute-forcing the master password from a generated site password has the same difficulty
--    as brute-forcing the master password directly
-- 3. The symbol: secure passwords should have upper-case letters, lower-case letters, digits,
--    and symbols. Whether the generated password should have a symbol (some sites don't allow
--    them) and what it should be if it does must be specified. Generate will usually be used
--    with Symbol => Auto_Symbol, but for sites that allow only a restricted set of symbols, it
--    is necessary to be able to specify the symbol
--
-- V1.1  2018 Nov 01     Improved documentation
-- V1.0  2017 Nov 15     Move password-generation logic into a package
--
package Password_Generation is
   subtype Length_Value is Integer range 8 .. 16;

   subtype Symbol_Range is Natural range 0 .. 9;

   type Symbol_List is array (Symbol_Range) of Character;

   Symbol_Set : constant Symbol_List := "!@#$%^&*/?";

   No_Symbol   : constant String := "None";
   Auto_Symbol : constant String := "Auto";

   function Generate (Domain : String; Master : String; Length : Length_Value; Symbol : String; Hash_Symbol : Boolean := True)
   return String;
   -- Generates a password of length Length from Domain, Master, and Symbol
   -- See the discussion above
   -- Domain is the domain for which the password is being generated
   -- Master is the user's master password
   -- Length is the desired length of the generated password; longer is usually better
   -- Symbol:
   --   If Symbol = No_Symbol, the password will have no symbol in it
   --   If Symbol = Auto_Symbol, the password will have a symbol selected from Symbol_Set
   --   If Symbol is anything else, it is used as the symbol
   -- Hash_Symbol should be True for any new password generation. If True, Symbol is included in the string that is hashed as
   -- part of generating the password. Earlier versions did not do this, so using False allows reproducing the password generated
   -- by those earlier versions
end Password_Generation;
