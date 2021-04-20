-- A generic for creating simple DBs (one table in an RDBMS) with PragmARC.Persistent_Skip_List_Unbounded and a Gnoga UI.
--
-- Copyright (C) 2017 by Jeffrey R. Carter
--
with Ada.Numerics.Discrete_Random;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Window;

with PragmARC.Persistent_Skip_List_Unbounded;

package body DB_Maker is
   use all type Gnoga.String;

   subtype String is Gnoga.String;

   Full_Name : constant String := File_Name & ".psl";

   package Lists is new PragmARC.Persistent_Skip_List_Unbounded (Element => Element);

   Window  : Gnoga.Gui.Window.Window_Type;
   View    : Gnoga.Gui.View.View_Type;
   Form    : Gnoga.Gui.Element.Form.Form_Type;
   Sel     : Gnoga.Gui.Element.Form.Selection_Type;
   Count   : Gnoga.Gui.Element.Form.Number_Type;
   Cnt_Lbl : Gnoga.Gui.Element.Form.Label_Type;
   Rand    : Gnoga.Gui.Element.Common.Button_Type;
   Quit    : Gnoga.Gui.Element.Common.Button_Type;
   Grid    : Gnoga.Gui.View.Grid.Grid_View_Type;
   L_Form  : Gnoga.Gui.Element.Form.Form_Type;
   R_View  : Gnoga.Gui.View.View_Type;

   type Field_Display_Info is record
      Text  : Gnoga.Gui.Element.Form.Text_Type;
      Label : Gnoga.Gui.Element.Form.Label_Type;
   end record;

   type Field_Display_List is array (Field_Number) of Field_Display_Info;

   Field   : Field_Display_List;
   Add     : Gnoga.Gui.Element.Common.Button_Type;
   Modif   : Gnoga.Gui.Element.Common.Button_Type;
   Delete  : Gnoga.Gui.Element.Common.Button_Type;
   S_Form  : Gnoga.Gui.Element.Form.Form_Type;
   Search  : Gnoga.Gui.Element.Common.Button_Type;
   Or_Rad  : Gnoga.Gui.Element.Form.Radio_Button_Type;
   Or_Lbl  : Gnoga.Gui.Element.Form.Label_Type;
   And_Rad : Gnoga.Gui.Element.Form.Radio_Button_Type;
   And_Lbl : Gnoga.Gui.Element.Form.Label_Type;
   Srch_Mr : Gnoga.Gui.Element.Common.Button_Type;
   Clear   : Gnoga.Gui.Element.Common.Button_Type;
   List    : Lists.Persistent_Skip_List := Lists.Open_List (To_UTF_8 (Full_Name));

   procedure Quit_Now (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   function Get_By_Index
     (Index : in Positive)
      return Element;

   procedure Transfer_Selected;

   procedure Random (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure Click_Selection (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure Key_Selection
     (Object         : in out Gnoga.Gui.Base.Base_Type'Class;
      Keyboard_Event : in     Gnoga.Gui.Base.Keyboard_Event_Record);

   function Get_From_Fields return Element;

   procedure Refresh;

   procedure Add_Item (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure Modify (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure Delete_Item (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure Search_From
     (Search_Item : in Element;
      Prev_Index  : in Natural);
   -- Performs a search starting with at Prev_Index + 1

   procedure Search_Item (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure Search_More (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure Reset (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure Add_One
     (Item     : in     Element;
      Continue :    out Boolean);
   -- Add Item to Sel

   procedure Add_All is new Lists.Iterate (Action => Add_One);

   function Get_By_Index
     (Index : in Positive)
      return Element
   is
      procedure Check_One
        (Item     : in     Element;
         Continue :    out Boolean);
      -- Increments Item_Num. If Item_Num = Index, sets Result to Item and Continue to False

      procedure Check_All is new Lists.Iterate (Action => Check_One);

      Item_Num : Natural := 0;
      Result   : Element;

      procedure Check_One
        (Item     : in     Element;
         Continue :    out Boolean)
      is
         -- Empty
      begin -- Check_One
         Continue := True;
         Item_Num := Item_Num + 1;

         if Item_Num = Index then
            Result   := Item;
            Continue := False;
         end if;
      end Check_One;
   begin -- Get_By_Index
      Check_All (List => List);

      return Result;
   end Get_By_Index;

   procedure Transfer_Selected is
      Item : constant Element := Get_By_Index (Sel.Selected_Index);
   begin -- Transfer_Selected
      All_Fields :
      for I in Field'Range loop
         Field (I).Text.Value (Value => Value (Item, I));
      end loop All_Fields;
   end Transfer_Selected;

   procedure Quit_Now (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      -- Empty;
   begin -- Quit_Now
      Gnoga.Application.Singleton.End_Application;
   exception -- Quit_Now
      when E : others =>
         Gnoga.Log (Message => "Quit_Now: ", Occurrence => E);
   end Quit_Now;

   procedure Random (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      subtype Item_Number is Integer range 1 .. Sel.Length;

      package Random_Item is new Ada.Numerics.Discrete_Random (Result_Subtype => Item_Number);

      Gen : Random_Item.Generator;
   begin -- Random
      Random_Item.Reset (Gen => Gen);
      Sel.Selected (Index => Random_Item.Random (Gen));
      Transfer_Selected;
   exception -- Random
      when E : others =>
         Gnoga.Log (Message => "Random: ", Occurrence => E);
   end Random;

   procedure Click_Selection (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      -- Empty
   begin -- Click_Selection
      Transfer_Selected;
   exception -- Click_Selection
      when E : others =>
         Gnoga.Log (Message => "Click_Selection: ", Occurrence => E);
   end Click_Selection;

   procedure Key_Selection
     (Object         : in out Gnoga.Gui.Base.Base_Type'Class;
      Keyboard_Event : in     Gnoga.Gui.Base.Keyboard_Event_Record)
   is
      -- Empty
   begin -- Key_Selection
      Transfer_Selected;
   exception -- Key_Selection
      when E : others =>
         Gnoga.Log (Message => "Key_Selection: ", Occurrence => E);
   end Key_Selection;

   function Get_From_Fields return Element is
      Item : Element;
   begin -- Get_From_Fields
      All_Fields :
      for I in Field'Range loop
         Put (Item => Item, Field => I, Value => Field (I).Text.Value);
      end loop All_Fields;

      return Item;
   end Get_From_Fields;

   procedure Refresh is
   begin -- Refresh
      Remove :
      for I in reverse 1 .. Sel.Length loop
         Sel.Remove_Option (Index => I);
      end loop Remove;

      Add_All (List => List);
      Count.Value (Value => Sel.Length);
   end Refresh;

   procedure Add_Item (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Item : constant Element := Get_From_Fields;

      Current : constant Lists.Result := List.Search (Item);
   begin -- Add_Item
      if Current.Found then
         Window.Alert (Message => "Item already exists. Use Modify to change.");

         return;
      end if;

      List.Insert (Item => Item);
      Refresh;
      And_Rad.Checked;
      Search_From (Search_Item => Item, Prev_Index => 0);
   exception -- Add_Item
      when E : others =>
         Gnoga.Log (Message => "Add_Item: ", Occurrence => E);
   end Add_Item;

   procedure Modify (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Item : constant Element := Get_From_Fields;

      Current : constant Lists.Result := List.Search (Item);
   begin -- Modify
      if not Current.Found then
         if Sel.Selected_Index = 0 then
            Window.Alert (Message => "Item doesn't exist. Use Add to insert.");

            return;
         end if;

         List.Delete (Item => Get_By_Index (Sel.Selected_Index));
      end if;

      List.Insert (Item => Item);
      Refresh;
      And_Rad.Checked;
      Search_From (Search_Item => Item, Prev_Index => 0);
   exception -- Modify
      when E : others =>
         Gnoga.Log (Message => "Modify: ", Occurrence => E);
   end Modify;

   procedure Delete_Item (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Item : Element;
   begin -- Delete_Item
      if Sel.Selected_Index = 0 then
         Window.Alert (Message => "Select an item to delete.");

         return;
      end if;

      Item := Get_By_Index (Sel.Selected_Index);
      List.Delete (Item => Item);
      Refresh;
   exception -- Delete_Item
      when E : others =>
         Gnoga.Log (Message => "Delete_Item: ", Occurrence => E);
   end Delete_Item;

   Search_Index : Natural := 0;

   procedure Search_From
     (Search_Item : in Element;
      Prev_Index  : in Natural)
   is
      procedure Check_One
        (Item     : in     Element;
         Continue :    out Boolean);
      -- Increments Index. If Index > Prev_Index and Item matches Search_Item, sets Found to True and Continue to False

      procedure Check_All is new Lists.Iterate (Action => Check_One);

      type Lowered_List is array (Field_Number) of String;

      Lowered : Lowered_List; -- To_Lower applied to the fields of Search_Item
      Found   : Boolean := False;
      Index   : Natural := 0;

      Or_Checked : constant Boolean := Or_Rad.Checked;

      procedure Check_One
        (Item     : in     Element;
         Continue :    out Boolean)
      is
         Local : Boolean := not Or_Checked;
      begin -- Check_One
         Index := Index + 1;

         if Index <= Prev_Index then
            Continue := True;

            return;
         end if;

         All_Fields :
         for I in Field'Range loop
            Field_Value :
            declare
               Text : constant String := Value (Item, I);
            begin -- Field_Value
               if Length (Lowered (I)) > 0 then
                  if Or_Checked then
                     Local := Local or Text.To_Lower.Index (Lowered (I)) > 0;
                  else
                     Local := Local and Text.To_Lower.Index (Lowered (I)) > 0;
                  end if;
               end if;
            end Field_Value;
         end loop All_Fields;

         Found    := Local;
         Continue := not Local;
      end Check_One;
   begin -- Search_From
      Fill_Lowered :
      for I in Lowered'Range loop
         Lowered (I) := To_Lower (Value (Search_Item, I));
      end loop Fill_Lowered;

      Check_All (List => List);

      if not Found then
         Window.Alert (Message => "No matching item.");

         return;
      end if;

      Sel.Selected (Index => Index);
      Search_Index := Index;
   end Search_From;

   procedure Search_Item (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Item : constant Element := Get_From_Fields;
   begin -- Search_Item
      Search_Index := 0;
      Search_From (Search_Item => Item, Prev_Index => 0);
   exception -- Search_Item
      when E : others =>
         Gnoga.Log (Message => "Search_Item: ", Occurrence => E);
   end Search_Item;

   procedure Search_More (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Item : constant Element := Get_From_Fields;
   begin -- Search_More
      Search_From (Search_Item => Item, Prev_Index => Search_Index);
   exception -- Search_More
      when E : others =>
         Gnoga.Log (Message => "Search_More: ", Occurrence => E);
   end Search_More;

   procedure Reset (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      -- Empty
   begin -- Reset
      All_Fields :
      for I in Field'Range loop
         Field (I).Text.Value (Value => "");
      end loop All_Fields;
   exception -- Reset
      when E : others =>
         Gnoga.Log (Message => "Reset: ", Occurrence => E);
   end Reset;

   procedure Add_One
     (Item     : in     Element;
      Continue :    out Boolean)
   is
      Image : String;
   begin -- Add_One
      All_Fields :
      for I in Field'Range loop
         if I > Field'First then
            Append (Source => Image, New_Item => " | ");
         end if;

         Append (Source => Image, New_Item => Value (Item, I));
      end loop All_Fields;

      Continue := True;
      Sel.Add_Option (Value => Image, Text => Image);
   end Add_One;

   Header : String;
begin -- DB_Maker
   Gnoga.Application.Title (File_Name);
   Gnoga.Application.HTML_On_Close (File_Name & " ended.");
   Gnoga.Application.Open_URL;
   Gnoga.Application.Singleton.Initialize (Main_Window => Window);

   View.Create (Parent => Window);
   Form.Create (Parent => View);
   Form.Text_Alignment (Value => Gnoga.Gui.Element.Center);

   Build_Header :
   for I in Field_Number loop
      if I > Field_Number'First then
         Append (Source => Header, New_Item => " | ");
      end if;

      Append (Source => Header, New_Item => Field_Name (I));
   end loop Build_Header;

   Form.Put_Line (Message => Header);
   Sel.Create (Form => Form, Visible_Lines => 20);
   Sel.On_Click_Handler (Handler => Click_Selection'Unrestricted_Access);
   Sel.On_Key_Press_Handler (Handler => Key_Selection'Unrestricted_Access);
   Form.New_Line;

   Count.Create (Form => Form);
   Count.Editable (Value => False);
   Count.Read_Only;
   Cnt_Lbl.Create (Form => Form, Label_For => Count, Content => "Number of items:");
   Rand.Create (Parent => Form, Content => "Random");
   Rand.On_Click_Handler (Handler => Random'Unrestricted_Access);
   Quit.Create (Parent => Form, Content => "Quit");
   Quit.On_Click_Handler (Handler => Quit_Now'Unrestricted_Access);

   Grid.Create
     (Parent => View, Layout => Gnoga.Gui.View.Grid.Horizontal_Split, Fill_Parent => False, Set_Sizes => False);
   L_Form.Create (Parent => Grid.Panel (1, 1).all);
   L_Form.Text_Alignment (Value => Gnoga.Gui.Element.Right);

   Create_Fields :
   for I in Field'Range loop
      Field (I).Text.Create (Form => L_Form, Size => 50);
      Field (I).Label.Create (Form => L_Form, Label_For => Field (I).Text, Content => Field_Name (I));

      if I < Field'Last then
         L_Form.New_Line;
      end if;
   end loop Create_Fields;

   R_View.Create (Parent => Grid.Panel (1, 2).all);
   Add.Create (Parent => R_View, Content => "Add");
   Add.On_Click_Handler (Handler => Add_Item'Unrestricted_Access);
   R_View.New_Line;
   Modif.Create (Parent => R_View, Content => "Modify");
   Modif.On_Click_Handler (Handler => Modify'Unrestricted_Access);
   R_View.New_Line;
   Delete.Create (Parent => R_View, Content => "Delete");
   Delete.On_Click_Handler (Handler => Delete_Item'Unrestricted_Access);
   R_View.New_Line;
   S_Form.Create (Parent => R_View);
   Search.Create (Parent => S_Form, Content => "Search");
   Search.On_Click_Handler (Handler => Search_Item'Unrestricted_Access);
   Or_Rad.Create (Form => S_Form, Checked => True, Name => "search");
   Or_Lbl.Create (Form => S_Form, Label_For => Or_Rad, Content => "or", Auto_Place => False);
   And_Rad.Create (Form => S_Form, Name => "search");
   And_Lbl.Create (Form => S_Form, Label_For => And_Rad, Content => "and", Auto_Place => False);
   S_Form.New_Line;
   Srch_Mr.Create (Parent => S_Form, Content => "Search Again");
   Srch_Mr.On_Click_Handler (Handler => Search_More'Unrestricted_Access);
   S_Form.New_Line;
   Clear.Create (Parent => S_Form, Content => "Clear");
   Clear.On_Click_Handler (Handler => Reset'Unrestricted_Access);

   Add_All (List => List);
   Count.Value (Value => Sel.Length);
   Gnoga.Application.Singleton.Message_Loop;
exception -- DB_Maker
   when E : others =>
      Gnoga.Log (E);
end DB_Maker;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
