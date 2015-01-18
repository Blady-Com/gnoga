with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Types.Colors;

procedure Pack is
   use Gnoga.Gui.Element;
   use Gnoga.Types.Colors;

   Main_Window : Gnoga.Gui.Window.Window_Type;
   Main_View   : Gnoga.Gui.View.View_Type;

   Display_Table : Gnoga.Gui.Element.Element_Type;
   Top_Panel     : Gnoga.Gui.View.View_Type;
   Left_Panel    : Gnoga.Gui.View.View_Type;
   Fill_Panel    : Gnoga.Gui.View.View_Type;
   Status_Panel  : Gnoga.Gui.View.View_Type;

begin
   Gnoga.Application.Open_URL;

   Gnoga.Application.Singleton.Initialize (Main_Window);

   Main_View.Put_HTML ("<table>" &
                         "<tr><td id='top_panel' colspan=2 />" &
                         "<tr>" &
                         "<td id='left_panel' />" &
                         "<td id='fill_panel' />" &
                         "</tr>" &
                         "<tr>" &
                         "<td id='left_panel2'>x</td>" &
                         "<td id='fill_panel2'>y</td>" &
                         "</tr>" &
                         "<tr><td id='status_panel' colspan=2 />" &
                         "</table>",
                       ID => "display_table");

   Display_Table.Attach_Using_Parent (Main_View, "display_table");
   Top_Panel.Attach_Using_Parent (Main_View, "top_panel");
   Left_Panel.Attach_Using_Parent (Main_View, "left_panel");
   Fill_Panel.Attach_Using_Parent (Main_View, "fill_panel");
   Status_Panel.Attach_Using_Parent (Main_View, "status_panel");

   Display_Table.Minimum_Width ("100%");
   Display_Table.Minimum_Height ("100%");

   Top_Panel.Height (50);
   Top_Panel.Background_Color (Orange);

   Left_Panel.Width (100);
   Left_Panel.Background_Color (Pink);
   Left_Panel.Overflow (Auto);
   Left_Panel.Resizable (Horizontal);

   Fill_Panel.Background_Color (Yellow_Green);
   Fill_Panel.Overflow (Auto);

   Status_Panel.Height (50);
   Status_Panel.Background_Color (Green);
   Status_Panel.Overflow (Auto);

   Gnoga.Application.Singleton.Message_Loop;
end Pack;
