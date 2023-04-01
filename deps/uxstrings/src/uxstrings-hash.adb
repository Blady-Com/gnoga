-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings-hash.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : UXString hash implementation.
-- NOTES                        : Ada 202x
--
-- COPYRIGHT                    : (c) Pascal Pignard 2023
-- LICENCE                      : CeCILL V2.1 (https://cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Strings.Wide_Wide_Hash;

function UXStrings.Hash (Key : UXString) return Ada.Containers.Hash_Type is
begin
   return Ada.Strings.Wide_Wide_Hash (To_Unicode (Key));
end UXStrings.Hash;
