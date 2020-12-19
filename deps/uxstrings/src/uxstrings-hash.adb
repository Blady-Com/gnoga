-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings-hash.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : UXString hash implementation.
-- NOTES                        : Ada 202x
--
-- COPYRIGHT                    : (c) Pascal Pignard 2020
-- LICENCE                      : CeCILL V2.1 (https://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Strings.Hash;

function UXStrings.Hash (Key : UXString) return Ada.Containers.Hash_Type is
begin
   return Ada.Strings.Hash (String (Key.Chars.all));
end UXStrings.Hash;
