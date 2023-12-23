with Ada.Text_IO;
with Gnoga.Application.Singleton;
with Gnoga.Gui.Element.Canvas.Context_2D;
with Gnoga.Gui.View;
with Gnoga.Gui.Window;
with Gnoga.Types.Colors;

procedure Image_Data is
--  Test the correctness of image data subprograms.
--  (https://developer.mozilla.org/en-US/docs/Web/API/ImageData)
--  Especially the order of row / column or x / y of image data.
--  Write image data file in ppm format (https://en.wikipedia.org/wiki/Netpbm#File_formats).

   procedure Write_P3
     (File_Name : in String;
      Image     : in Gnoga.Types.Pixel_Data_Type) with
     Pre => Image'First (1) = 1 and Image'First (2) = 1;

   procedure Write_P3
     (File_Name : in String;
      Image     : in Gnoga.Types.Pixel_Data_Type)
   is
      Output : Ada.Text_IO.File_Type;
   begin -- Write_P3
      Ada.Text_IO.Create (File => Output, Name => File_Name);
      Ada.Text_IO.Put_Line (File => Output, Item => "P3");
      Ada.Text_IO.Put_Line
        (File => Output, Item => Integer'Image (Image'Length (1)) & Integer'Image (Image'Length (2)) & " 255");

      All_Rows :
      for Y in Image'Range (2) loop
         All_Columns :
         for X in Image'Range (1) loop
            Ada.Text_IO.Put
              (File => Output, Item => Image (X, Y).Red'Image & Image (X, Y).Green'Image & Image (X, Y).Blue'Image);
         end loop All_Columns;

         Ada.Text_IO.New_Line (File => Output);
      end loop All_Rows;

      Ada.Text_IO.Close (File => Output);
   end Write_P3;

   Width  : constant := 4;
   Height : constant := 3;

   Window  : Gnoga.Gui.Window.Window_Type;
   View    : Gnoga.Gui.View.View_Type;
   Canvas  : Gnoga.Gui.Element.Canvas.Canvas_Type;
   Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   Image   : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
begin -- Image_Data
   Gnoga.Application.Title (Name => "Demonstrate error in Data");
   Gnoga.Application.HTML_On_Close (HTML => "Image_Data ended.");
   --     Gnoga.Application.Open_URL;
   Gnoga.Application.Singleton.Initialize (Main_Window => Window);
   View.Create (Parent => Window);
   View.Text_Alignment (Value => Gnoga.Gui.Element.Center);
   Canvas.Create (Parent => View, Width => Width, Height => Height);
   Context.Get_Drawing_Context_2D (Canvas => Canvas);

   Red :
   for X in 0 .. Width - 1 loop
      Context.Pixel (X => X, Y => 0, Color => Gnoga.Types.Colors.Red);
   end loop Red;

   Green :
   for X in 0 .. Width - 1 loop
      Context.Pixel (X => X, Y => 1, Color => Gnoga.Types.Colors.Green);
   end loop Green;

   Blue :
   for X in 0 .. Width - 1 loop
      Context.Pixel (X => X, Y => 2, Color => Gnoga.Types.Colors.Blue);
   end loop Blue;

   delay 0.5;

   Context.Get_Image_Data (Image_Data => Image, Left => 0, Top => 0, Width => 4, Height => 3);

   Get_Data :
   declare
      Pixel : constant Gnoga.Types.Pixel_Data_Type := Image.Data;
      use type Gnoga.String;
   begin -- Get_Data
      Gnoga.Log
        (Message => "Width: " & Gnoga.Image (Pixel'Length (1)) & " - Height: " & Gnoga.Image (Pixel'Length (2)));
      Write_P3 (File_Name => "image_data.ppm", Image => Pixel);
   end Get_Data;

   --  Gnoga.Application.Singleton.Message_Loop;

   delay 4.5;

   Gnoga.Application.Singleton.End_Application;
end Image_Data;
