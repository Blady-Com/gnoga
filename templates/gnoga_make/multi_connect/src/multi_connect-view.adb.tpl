package body @@data.App_Name@@.View is

   ------------
   -- Create --
   ------------

   overriding
   procedure Create
     (View    : in out Default_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach  : in     Boolean := True;
      ID      : in     String  := "")
   is
   begin
      Gnoga.Gui.View.View_Type (View).Create (Parent, Attach, ID);
      
      View.Label_Text.Create (View);
      View.Click_Button.Create (View, "Click Me");
   end Create;

end @@data.App_Name@@.View;

