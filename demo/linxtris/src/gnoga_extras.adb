package body Gnoga_Extras is

   ------------
   -- Hidden --
   ------------

   overriding procedure Hidden
     (Element : in out View_Type;
      Value   : in     Boolean := True)
   is
      Already_Hidden : constant Boolean :=
        Gnoga.Gui.View.View_Type (Element).Hidden;
      use type Gnoga.Gui.Base.Action_Event;
   begin
      if Already_Hidden and not Value and Element.Show_Handler /= null then
         Element.Show_Handler.all (Element);
      end if;
      Gnoga.Gui.View.View_Type (Element).Hidden (Value);
      if not Already_Hidden and Value and Element.Hide_Handler /= null then
         Element.Hide_Handler.all (Element);
      end if;
   end Hidden;

   ---------------------
   -- On_Show_Handler --
   ---------------------

   procedure On_Show_Handler
     (Object  : in out View_Type;
      Handler : in     Gnoga.Gui.Base.Action_Event)
   is
   begin
      Object.Show_Handler := Handler;
   end On_Show_Handler;

   ---------------------
   -- On_Hide_Handler --
   ---------------------

   procedure On_Hide_Handler
     (Object  : in out View_Type;
      Handler : in     Gnoga.Gui.Base.Action_Event)
   is
   begin
      Object.Hide_Handler := Handler;
   end On_Hide_Handler;

end Gnoga_Extras;
