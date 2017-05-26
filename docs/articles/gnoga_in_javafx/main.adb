with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;

with GNAT.OS_Lib;

procedure Main is
   Main_Window  : Gnoga.Gui.Window.Window_Type;
   Main_View    : Gnoga.Gui.View.View_Type;

   procedure Open_URL_Java (url    : String  := "http://127.0.0.1:8080";
                            title  : String  := "Gnoga App";
                            width  : Natural := 750;
                            height : Natural := 500) is
      Args : GNAT.OS_Lib.Argument_List_Access;
      PID  : GNAT.OS_Lib.Process_Id;
   begin
      Args := GNAT.OS_Lib.Argument_String_To_List ("java -cp . JavaBrowser " &
                                                     url & " " &
                                                     Integer'Image(width) & " " &
                                                     Integer'Image(height)& " " &
                                                     title);
      PID := GNAT.OS_Lib.Non_Blocking_Spawn
        (Program_Name => Args (Args'First).all,
         Args         => Args (Args'First + 1 .. Args'Last));
   end Open_URL_Java;

begin
   --Gnoga.Application.Open_URL_Windows;
   Open_URL_Java(url    => "http://localhost:8080",
                 width  => 1000,
                 height => 500,
                 title  => "Gnoga in JavaFX");
   Gnoga.Application.Singleton.Initialize(Main_Window => Main_Window);

   Main_View.Create(Main_Window);
   Main_View.Put_Line("Hello World");

   Gnoga.Application.Singleton.Message_Loop;

end Main;
