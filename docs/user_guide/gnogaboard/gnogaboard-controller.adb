with Gnoga.Gui.Base;

with GnogaBoard.View;

package body GnogaBoard.Controller is

   protected Users is
      procedure Add (View : in GnogaBoard.View.Default_View_Access);
      procedure Remove (N : in Positive);
      function Find_User (P : in out Gnoga.Gui.Base.Base_Type'Class) return Natural;
      entry Update_User_Views (X1, Y1, X2, Y2 : in Integer);
   private
      U : Gnoga.Gui.Base.Base_Type_Array;
   end Users;

   protected body Users is

      procedure Add (View : in GnogaBoard.View.Default_View_Access) is
      begin
         U.Append (Gnoga.Gui.Base.Pointer_To_Base_Class (View));
      end Add;

      procedure Remove (N : Positive) is
      begin
         U.Delete (N);
      end Remove;

      function Find_User (P : in out Gnoga.Gui.Base.Base_Type'Class) return Natural is
      begin
         return U.Find_Index (P'Unchecked_Access);
      end Find_User;

      entry Update_User_Views (X1, Y1, X2, Y2 : in Integer) when not U.Is_Empty is
      begin
         for I in U.First_Index .. U.Last_Index loop
            declare
               User_View : constant GnogaBoard.View.Default_View_Access :=
                             GnogaBoard.View.Default_View_Access (U.Element (I));
            begin
               User_View.Draw (X1, Y1, X2, Y2);
            exception
               -- Ignore error on attempting to draw to already-invalidated view.
               when others =>
                  null;
            end;
         end loop;
      end Update_User_Views;

   end Users;


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
      Users.Update_User_Views (View.X1, View.Y1, View.X2, View.Y2);
   end On_Change;

   procedure On_Destroy (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      use Gnoga.Gui.Base.Base_Type_Arrays;

      N : constant Natural := Users.Find_User (Object);
   begin
      if N /= No_Index then
         Users.Remove (N);
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
      Users.Add (View);
      View.On_Change := On_Change'Access;
      View.On_Destroy_Handler (On_Destroy'Access);
   end Default;

begin
   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Default'Access, "default");
end GnogaBoard.Controller;
