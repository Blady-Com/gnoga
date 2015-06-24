with Gnoga.Gui.Window;
with Gnoga.Application.Singleton;

procedure Essai4 is
   LocalW : Gnoga.Gui.Window.Window_Access;
begin
   --  LocalW := new Main_Window_Record;
   Gnoga.Application.Singleton.Initialize (LocalW.all);
   --  no CE displayed in console
end Essai4;
