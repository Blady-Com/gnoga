with GNAT.OS_Lib;
with GNAT.Expect;

with Gnoga.Gui.Base;

package body GnogaCMD.View is

   
   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Handle submit of command line from either hitting the submit button
   --  or pressing enter with in the command line.
      
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
      Gnoga.Gui.View.Console.Console_View_Type
        (View).Create (Parent, Attach, ID);
      
      View.Entry_Form.Create (Parent => View);
      
      View.Cmd_Line.Create (Form  => View.Entry_Form,
                            Size  => 40);
      
      View.Prompt.Create (Form       => View.Entry_Form,
                          Label_For  => View.Cmd_Line,
                          Contents   => "Command >",
                          Auto_Place => True);
      --  Labels can automatically place themselves before the associated
      --  control they are labeling. This is the default if Auto_Place not
      --  specified.
      
      View.Go_Button.Create (Form  => View.Entry_Form,
                             Value => "Go");
      --  The "value" of a button is the button's text. Go_Button is a submit
      --  button and will cause it's Form to submit.
      
      View.On_Submit_Handler (On_Submit'Access);
      --  The submit event will bubble up if not handled at the form itself.
      --  In our case instead of placing On_Submit on the View.Entry_Form
      --  we have placed it on the parent view.
   end Create;
   
   
   ---------------
   -- On_Submit --
   ---------------
   
   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : Default_View_Type renames Default_View_Type (Object);
      --  Renaming is a convenient way to "upcast"
      
      --  Our tutorial is focusing on Gnoga not the GNAT packages in gcc/ada
      --  so will will not go in to length about using GNAT.Expect, it is
      --  left as an exercise to the reader to look at the specs.
      
      Args   : GNAT.OS_Lib.Argument_List_Access;
      Status : aliased Integer;
   begin
      Args := GNAT.OS_Lib.Argument_String_To_List (View.Cmd_Line.Value);
      
      declare
         Result : String := Gnat.Expect.Get_Command_Output
           (Command    => Args (Args'First).all,
            Arguments  => Args (Args'First + 1 .. Args'Last),
            Input      => "",
            Status     => Status'Access,
            Err_To_Out => True);
      begin
         View.Put_HTML ("<pre>" & Result & "</pre>");
         --  Put_HTML is a convenient way to dump pure html in to a view at
         --  the bottom of the view.
         
         View.New_Line;
      end;
      
      View.Entry_Form.Place_Inside_Bottom_Of (View);
      --  This will move entire Entry_Form (all of its children as well) to
      --  the bottom of the View.
      
      View.Cmd_Line.Focus;
      --  Place the entry focus on Cmd_Line
      
      View.Cmd_Line.Select_Text;
      --  Select the entire contents of Cmd_Line
   end On_Submit;   

end GnogaCMD.View;
