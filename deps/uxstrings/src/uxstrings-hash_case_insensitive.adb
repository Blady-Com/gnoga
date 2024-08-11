-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings-hash_case_insensitive.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : UXString case insensitive hash implementation.
-- NOTES                        : Ada 202x
--
-- COPYRIGHT                    : (c) Pascal Pignard 2023
-- LICENCE                      : CeCILL-C (https://cecill.info)
-- CONTACT                      : http://blady.chez.com
-------------------------------------------------------------------------------

with Ada.Strings.Wide_Wide_Hash;

function UXStrings.Hash_Case_Insensitive (Key : UXString) return Ada.Containers.Hash_Type is
begin
   return Ada.Strings.Wide_Wide_Hash (To_Unicode (To_Lower (Key)));
end UXStrings.Hash_Case_Insensitive;
