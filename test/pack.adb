with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Grid;
with Gnoga.Types.Colors;

procedure Pack is
   use Gnoga.Gui.Element;
   use Gnoga.Gui.View.Grid;
   use Gnoga.Types.Colors;

   Main_Window : Gnoga.Gui.Window.Window_Type;
   Main_View   : Gnoga.Gui.View.Grid.Grid_View_Type;
   Split_View  : Gnoga.Gui.View.Grid.Grid_View_Type;

begin
   Gnoga.Application.Open_URL;

   Gnoga.Application.Singleton.Initialize (Main_Window);

   Main_View.Create (Main_Window, ((COL, SPN),
                             (COL, COL),
                             (COL, SPN)));

   Main_View.Panel (1, 1).Height (50);
   Main_View.Panel (1, 1).Background_Color (Pink);

   Main_View.Panel (2, 1).Width (100);
   Main_View.Panel (2, 1).Background_Color (Orange);
   Main_View.Panel (2, 1).Overflow (Auto);
   Main_View.Panel (2, 1).Resizable (Horizontal);

   Main_View.Panel (3, 1).Height (50);
   Main_View.Panel (3, 1).Background_Color (Blue);

   Split_View.Create (Main_View.Panel (2, 2).all, Vertical_Split);

   Split_View.Panel (1, 1).Background_Color (Purple);

   Split_View.Panel (2, 1).Background_Color (Brown);

   Gnoga.Application.Singleton.Message_Loop;
end Pack;
