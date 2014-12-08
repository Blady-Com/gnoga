package body Talk.Double is
   procedure Say_More (S : in String) is
   begin
      Talk.Say (S);
      Say (S);
      --  Since Talk.Double is a child of Talk, there is no need to qualify the
      --  parent package Talk and there was no need to "with" the parent
      --  package either.
   end Say_More;
end Talk.Double;
