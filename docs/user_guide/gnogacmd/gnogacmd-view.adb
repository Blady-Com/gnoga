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
                            Size  => 40,
                            Value => "");
      View.Prompt.Create (Form       => View.Entry_Form,
                          Label_For  => View.Cmd_Line,
                          Contents   => "Command >");
      View.Go_Button.Create (Form  => View.Entry_Form,
                             Value => "Go");
      
      View.On_Submit_Handler (On_Submit'Access);
   end Create;
   
   
   ---------------
   -- On_Submit --
   ---------------
   
   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : Default_View_Type renames Default_View_Type (Object);
      
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
         View.New_Line;
      end;
      
      View.Entry_Form.Place_Inside_Bottom_Of (View);
      
      View.Cmd_Line.Focus;
      View.Cmd_Line.Select_Text;
   end On_Submit;   

end GnogaCMD.View;
