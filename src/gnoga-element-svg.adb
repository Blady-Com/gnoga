package body Gnoga.Element.SVG is

   ------------
   -- Create --
   ------------

   procedure Create
     (SVG     : in out SVG_Type;
      Parent  : in out Gnoga.Base.Base_Type'Class;
      Content : in     String := "";
      ID      : in     String := "")
   is
   begin
      SVG.Create_From_HTML (Parent,
                            "<svg xmlns=""http://www.w3.org/2000/svg"" " &
                              "xmlns:xlink=""http://www.w3.org/1999/xlink"">" &
                              Escape_Quotes (Content) &
                              "</svg>", ID);
   end Create;

end Gnoga.Element.SVG;
