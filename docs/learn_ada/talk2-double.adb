package body Talk2.Double is
   procedure Say_More (S : in String) is
   begin
      Talk2.Say (S);

      Talk_Count := Talk_Count - 1;
      --  Talk_Count is "private" in the Talk2 package and so accessible
      --  to child packages. Say_Count is in the body of Talk2 and so
      --  private even to child packages.

      Say (S);
   end Say_More;
end Talk2.Double;
