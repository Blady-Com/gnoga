with Gnoga.Application.Singleton;

with Essai2_Ext_Decl;

procedure Essai2 is
begin
   Gnoga.Application.Singleton.Initialize
     (Main_Window => Essai2_Ext_Decl.ExtW);
   Gnoga.Application.Singleton.Message_Loop;
end Essai2;
