with Ada.Numerics.Elementary_Functions;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Canvas.Context_2D.Plotting;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.View;
with Gnoga.Gui.Window;

procedure Plot_Test is
   package Math renames Ada.Numerics.Elementary_Functions;

   procedure On_Quit (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   X_Limit : constant := 8.0;
   Y_Limit : constant := 2.0;

   Win  : Gnoga.Gui.Window.Window_Type;
   View : Gnoga.Gui.View.View_Type;
   Plot : Gnoga.Gui.Element.Canvas.Context_2D.Plotting.Plot_Info;
   Quit : Gnoga.Gui.Element.Common.Button_Type;
   List : Gnoga.Gui.Element.Canvas.Context_2D.Plotting.Point_List
     (1 .. Integer (20 * X_Limit) + 5);

   procedure On_Quit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      --  Empty
   begin -- On_Quit
      View.Remove;
      Gnoga.Application.Singleton.End_Application;
   end On_Quit;
begin -- Plot_Test
   Gnoga.Application.Open_URL;
   Gnoga.Application.Title (Name => "Plot Test");
   Gnoga.Application.HTML_On_Close (HTML => "Plot Test Finished");
   Gnoga.Application.Singleton.Initialize (Main_Window => Win);
   View.Create (Parent => Win);
   View.Text_Alignment (Value => Gnoga.Gui.Element.Center);
   Plot.Create
     (Parent => View, Width => 500, Height => 500, X_Min => -X_Limit,
      X_Max => X_Limit, Y_Min => -Y_Limit, Y_Max => Y_Limit);
   Plot.Border;
   View.New_Line;
   Plot.Axes (Interval => 1.0, Length => 10);
   Plot.Point (Position => (X => -1.0, Y => 1.5), Color => "green");
   Plot.Point (Position => (X =>  0.0, Y => 1.5), Color => "green");
   Plot.Point (Position => (X =>  1.0, Y => 1.5), Color => "green");
   Plot.Line (From  => (X => -X_Limit, Y => Y_Limit),
              To    => (X => X_Limit, Y => -Y_Limit),
              Color => "blue");
   List (List'First) := (X => -X_Limit,
                         Y => Math.Sin (-X_Limit) / (-X_Limit));

   All_Points : for I in List'First + 1 .. List'Last loop
      List (I).X := List (I - 1).X + 0.1;

      exit All_Points when List (I).X > X_Limit;

      if List (I).X = 0.0 then
         List (I).Y := 1.0;
      else
         List (I).Y := Math.Sin (List (I).X) / List (I).X;
      end if;
   end loop All_Points;

   Plot.Graph (List => List, Color => "red");
   Quit.Create (Parent => View, Content => "Quit");
   Quit.On_Click_Handler (Handler => On_Quit'Unrestricted_Access);

   Gnoga.Application.Singleton.Message_Loop;
end Plot_Test;
