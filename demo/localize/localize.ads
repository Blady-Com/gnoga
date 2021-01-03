-------------------------------------------------------------------------------
-- NAME (specification)         : Localize.ads
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : Root unit.
-- NOTES                        : Ada 2012, GNOGA 2.1 alpha
--
-- COPYRIGHT                    : (c) Pascal Pignard 2021
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with UXStrings;

package Localize is
   use UXStrings;
   subtype String is UXString;
end Localize;
