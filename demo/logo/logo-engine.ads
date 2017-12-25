with Logo.View;

package Logo.Engine is

--     type Primitive is (avance, recule, tournedroite, tournegauche);
   type Primitive is (nop, av, re, td, tg);

   type Instruction is record
      Command : Primitive := nop;
      Arg1    : Natural   := 0;
   end record;

   function Decode (Line : String) return Instruction;

   procedure Action
     (Instr : Instruction;
      View  : Logo.View.Default_View_Access);

end Logo.Engine;
