with Gnoga.Gui.Base;

with GnogaBoard.View;

package body GnogaBoard.Controller is

   Users : Gnoga.Gui.Base.Base_Type_Array;

   procedure On_Change (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Called by a view when a new line segment is drawn by the user. It will
   --  update every user's view with the new segment.

   procedure On_Destroy (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Set to the On_Destroy event in each new event to remove it from that
   --  view from the Users collection.

   procedure On_Change (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      use GnogaBoard.View;

      View : Default_View_Type renames Default_View_Type (Object);
   begin
      for i in Users.First_Index .. Users.Last_Index loop
         declare
            User_View : constant GnogaBoard.View.Default_View_Access :=
              GnogaBoard.View.Default_View_Access (Users.Element (i));
         begin
            User_View.Draw (View.X1, View.Y1, View.X2, View.Y2);
         end;
      end loop;
   end On_Change;

   procedure On_Destroy (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      use Gnoga.Gui.Base.Base_Type_Arrays;

      N : constant Integer := Users.Find_Index (Object'Unchecked_Access);
   begin
      if N /= No_Index then
         Users.Delete (N);
      end if;
   end On_Destroy;

   procedure Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      View : constant GnogaBoard.View.Default_View_Access :=
               new GnogaBoard.View.Default_View_Type;
   begin
      View.Dynamic;
      View.Create (Main_Window);
      Users.Append (Gnoga.Gui.Base.Pointer_To_Base_Class (View));
      View.On_Change := On_Change'Access;
      View.On_Destroy_Handler (On_Destroy'Access);
   end Default;

begin
   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Default'Access, "default");
end GnogaBoard.Controller;
