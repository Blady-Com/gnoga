with Gnoga.Types;
with Gnoga.Window;
with Gnoga.Element.Common;

package AdaBlog.View is

   procedure Display_Blog_Entry
     (Parent : in out Gnoga.Element.Common.DIV_Type'Class;
      Data   : in     Gnoga.Types.Data_Map_Type);
   --  Adds blog entries to content

   procedure New_Entry_Form
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Content     : in out Gnoga.Element.Common.DIV_Type'Class);
   --  Cretes new entry form for  content

   procedure User_Panel
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Panel       : in out Gnoga.Element.Common.DIV_Type'Class;
      User_Record : in     Gnoga.Types.Data_Map_Type);
   --  Setup Panel with User_Record information or Login if User_Record empty

   procedure Template
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Content     : in out Gnoga.Element.Common.DIV_Type'Class;
      Left_Panel  : in out Gnoga.Element.Common.DIV_Type'Class);
   --  Content will be placed in to Content area of Template
end AdaBlog.View;
