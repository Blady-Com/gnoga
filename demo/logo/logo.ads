-------------------------------------------------------------------------------
-- NAME (specification)         : logo.ads
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : Unit root.
-- NOTES                        : Ada 2012, GNOGA 1.4 beta
--
-- COPYRIGHT                    : (c) Pascal Pignard 2018
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Gnoga;

package Logo is
   use Gnoga;
   use all type Gnoga.String;
   subtype String is Gnoga.String;
end Logo;
