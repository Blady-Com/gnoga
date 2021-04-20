-------------------------------------------------------------------------------
-- NAME (specification)         : logo-engine.ads
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : Logo engine unit.
-- NOTES                        : Ada 2012, GNOGA 1.4 beta
--
-- COPYRIGHT                    : (c) Pascal Pignard 2018
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Logo.View;
with Logo.Parser;

package Logo.Engine is

   type Instruction is record
      Command : Logo.Parser.Primitive := Logo.Parser.nop;
      Arg1    : Natural               := 0;
   end record;

   function Decode
     (Line       : String;
      Primitives : Logo.Parser.Primitive_Tables.Dictionary)
      return Instruction;

   procedure Action
     (Instr : Instruction;
      View  : Logo.View.Default_View_Access);

end Logo.Engine;
