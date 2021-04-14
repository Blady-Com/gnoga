-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings-hash_case_insensitive.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : UXString case insensitive hash implementation.
-- NOTES                        : Ada 202x
--
-- COPYRIGHT                    : (c) Pascal Pignard 2021
-- LICENCE                      : CeCILL V2.1 (https://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Strings.Hash_Case_Insensitive;

function UXStrings.Hash_Case_Insensitive (Key : UXString) return Ada.Containers.Hash_Type is
begin
   return Ada.Strings.Hash_Case_Insensitive (String (Key.Chars.all));
end UXStrings.Hash_Case_Insensitive;
