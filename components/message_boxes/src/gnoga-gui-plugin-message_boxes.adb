--  Inspired by GWindows and jdemo

with Ada.Exceptions;

with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Plugin.jQueryUI.Widget;

package body Gnoga.Gui.Plugin.Message_Boxes is

   use Gnoga.Gui.Element;

   function Message_Box
     (Parent      : in out Gnoga.Gui.Base.Base_Type'Class;
      Title, Text : in     String;
      Style       : in     Message_Box_Type := OK_Box)
      return Message_Box_Result
   is
      Dialog : aliased jQueryUI.Widget.Dialog_Type;
      Result : Message_Box_Result;
      pragma Volatile (Result);
      --
      procedure OK_Close_Dialog (Object : in out Gnoga.Gui.Base.Base_Type'Class);
      procedure No_Close_Dialog (Object : in out Gnoga.Gui.Base.Base_Type'Class);
      procedure Cancel_Close_Dialog (Object : in out Gnoga.Gui.Base.Base_Type'Class);
      procedure Cancel_Window_Dialog (Object : in out Gnoga.Gui.Base.Base_Type'Class);

      procedure OK_Close_Dialog (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         pragma Unreferenced (Object);
      begin
         case Style is
            when OK_Box =>
               Result := OK;
            when others =>
               Result := Yes;
         end case;
         Dialog.Close;
      end OK_Close_Dialog;
      --
      procedure No_Close_Dialog (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         pragma Unreferenced (Object);
      begin
         Result := No;
         Dialog.Close;
      end No_Close_Dialog;
      --
      procedure Cancel_Close_Dialog (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         pragma Unreferenced (Object);
      begin
         Result := Cancel;
         Dialog.Close;
      end Cancel_Close_Dialog;
      --
      procedure Cancel_Window_Dialog (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         pragma Unreferenced (Object);
      begin
         if Result = None then
            Gnoga.Log ("fermeture 'croix'");
            Result := Cancel;
         end if;
      end Cancel_Window_Dialog;
      --
      ok_btn     : Common.Button_Access;  --  OK, Yes are the same button
      no_btn     : Common.Button_Access;
      cancel_btn : Common.Button_Access;
   begin
      Result := None;
      Dialog.Create
        (Parent      => Parent, Title => Title, Content => Text, Height => 300, Width => 300, Position_My => "top",
         Position_At => "center top+5%");
      if Style in Yes_No_Box .. Yes_No_Def_Box then
         --  Disable the [x] box: no Cancel choice there
         null; -- !!
      end if;
      Dialog.Open;
      Dialog.On_Close_Handler (Cancel_Window_Dialog'Unrestricted_Access);
      ok_btn := Common.Button_Access (Dialog.New_Element ("ok", new Common.Button_Type));
      case Style is
         when OK_Box =>
            ok_btn.Create (Dialog, "OK");
            jQueryUI.Position (ok_btn.all, Target => Dialog, Using_My => "center bottom", At_Target => "center bottom");
         when others =>
            ok_btn.Create (Dialog, "Yes");
            jQueryUI.Position (ok_btn.all, Target => Dialog, Using_My => "left bottom", At_Target => "left bottom");
      end case;
      ok_btn.On_Click_Handler (OK_Close_Dialog'Unrestricted_Access);
      case Style is
         when OK_Box =>
            null;
         when others =>
            no_btn := Common.Button_Access (Dialog.New_Element ("no", new Common.Button_Type));
            no_btn.Create (Dialog, "No");
            no_btn.On_Click_Handler (No_Close_Dialog'Unrestricted_Access);
            jQueryUI.Position (no_btn.all, Target => Dialog, Using_My => "center bottom", At_Target => "center bottom");
      end case;
      case Style is
         when Yes_No_Cancel_Box | Yes_No_Def_Cancel_Box | Yes_No_Cancel_Def_Box =>
            cancel_btn := Common.Button_Access (Dialog.New_Element ("cancel", new Common.Button_Type));
            cancel_btn.Create (Dialog, "Cancel");
            cancel_btn.On_Click_Handler (Cancel_Close_Dialog'Unrestricted_Access);
            jQueryUI.Position
              (cancel_btn.all, Target => Dialog, Using_My => "right bottom", At_Target => "right bottom");
         when others =>
            null;  --  No cancel button
      end case;

      case Style is
         when OK_Box | Yes_No_Box | Yes_No_Cancel_Box =>
            ok_btn.Focus;
         when Yes_No_Def_Cancel_Box | Yes_No_Def_Box =>
            no_btn.Focus;
         when Yes_No_Cancel_Def_Box =>
            cancel_btn.Focus;
      end case;
      Gnoga.Log ("entrée de la boucle");
      loop
         delay 0.10;
         exit when Result /= None;
      end loop;  -- Waiting for a click into OK
      Gnoga.Log ("sortie de la boucle");
      return Result;
   exception
      when E : others =>
         Log ("Error Message_Box.");
         Log (From_UTF_8 (Ada.Exceptions.Exception_Information (E)));
         return Cancel;
   end Message_Box;

   procedure Message_Box
     (Parent      : in out Gnoga.Gui.Base.Base_Type'Class;
      Title, Text : in     String;
      Style       : in     Message_Box_Type := OK_Box)
   is
      Dummy_Result : Message_Box_Result;
   begin
      Dummy_Result := Message_Box (Parent, Title, Text, Style);
   end Message_Box;

end Gnoga.Gui.Plugin.Message_Boxes;
