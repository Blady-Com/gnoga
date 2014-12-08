with Talk.Double;

procedure Say_Hello2 is
begin
   Talk.Say ("Hello World!");
   --  Since the child of Talk, Talk.Double was "with"ed the parent package
   --  is automatically "with"ed.

   Talk.Double.Say_More ("Hello again.");
end Say_Hello2;
