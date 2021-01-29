-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings-text_io.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : Text input / output implementation for UXString.
-- NOTES                        : Ada 202x
--
-- COPYRIGHT                    : (c) Pascal Pignard 2021
-- LICENCE                      : CeCILL V2.1 (https://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

package body UXStrings.Text_IO is

   use GNAT.OS_Lib;

   Std_In  : aliased File_Type := (Standin, In_File, "sdtin", Latin_1, CRLF_Ending, others => <>);
   Std_Out : aliased File_Type := (Standout, Out_File, "sdtout", Latin_1, CRLF_Ending, others => <>);
   Std_Err : aliased File_Type := (Standerr, Out_File, "sdterr", Latin_1, CRLF_Ending, others => <>);

   Cur_In  : aliased File_Type := Std_In;
   Cur_Out : aliased File_Type := Std_Out;
   Cur_Err : aliased File_Type := Std_Err;

   LM : UXString := From_Latin_1 (Character'Val (13) & Character'Val (10)); -- Default is CRLF for Line_Mark function

   procedure Read_More (File : in out File_Type) is
      Buffer_Size : constant := 200;
      subtype Buffer_Type is String (1 .. Buffer_Size);
      Buffer : Buffer_Type;
      Last   : constant Integer := Read (File.FD, Buffer'Address, Buffer'Length);
   begin
      case File.Scheme is
         when Latin_1 =>
            File.Buffer.Append (From_Latin_1 (Buffer (1 .. Last)));
         when UTF_8 =>
            File.Buffer.Append (From_UTF_8 (Buffer (1 .. Last)));
         when UTF_16BE | UTF_16LE =>
            File.Buffer.Append (From_UTF_16 (Buffer (1 .. Last), File.Scheme));
      end case;
      if Last < Buffer_Size then
         File.EOF := True;
      end if;
   end Read_More;

   ------------
   -- Create --
   ------------

   procedure Create
     (File   : in out File_Type; Mode : in File_Mode := Out_File; Name : in UXString := Null_UXString;
      Scheme : in     Encoding_Scheme := Latin_1; Ending : Line_Ending := CRLF_Ending)
   is
      FD : File_Descriptor;
   begin
      case Mode is
         when In_File =>
            FD := Open_Read (To_Latin_1 (Name), Binary);
         when Out_File =>
            FD := Create_File (To_Latin_1 (Name), Binary);
         when Append_File =>
            FD := Open_Append (To_Latin_1 (Name), Binary);
      end case;
      File := (FD, Mode, Name, Scheme, Ending, others => <>);
   end Create;

   ----------
   -- Open --
   ----------

   procedure Open
     (File   : in out File_Type; Mode : in File_Mode; Name : in UXString; Scheme : in Encoding_Scheme := Latin_1;
      Ending :        Line_Ending := CRLF_Ending)
   is
      FD : File_Descriptor;
   begin
      case Mode is
         when In_File =>
            FD := Open_Read (To_Latin_1 (Name), Binary);
         when Out_File =>
            FD := Create_File (To_Latin_1 (Name), Binary);
         when Append_File =>
            FD := Open_Append (To_Latin_1 (Name), Binary);
      end case;
      File := (FD, Mode, Name, Scheme, Ending, others => <>);
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      Close (File.FD);
   end Close;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : in out File_Type) is
      Dummy_Result : Boolean;
   begin
      Delete_File (To_Latin_1 (File.Name), Dummy_Result);
   end Delete;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : in out File_Type; Mode : in File_Mode) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Reset unimplemented");
      raise Program_Error with "Unimplemented procedure Reset";
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : in out File_Type) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Reset unimplemented");
      raise Program_Error with "Unimplemented procedure Reset";
   end Reset;

   ----------
   -- Mode --
   ----------

   function Mode (File : in File_Type) return File_Mode is
   begin
      return File.Mode;
   end Mode;

   ----------
   -- Name --
   ----------

   function Name (File : in File_Type) return UXString is
   begin
      return File.Name;
   end Name;

   ------------
   -- Scheme --
   ------------

   function Scheme (File : in File_Type) return Encoding_Scheme is
   begin
      return File.Scheme;
   end Scheme;

   procedure Scheme (File : in File_Access; Value : in Encoding_Scheme) is
   begin
      File.Scheme := Value;
   end Scheme;

   ------------
   -- Ending --
   ------------

   function Ending (File : in File_Type) return Line_Ending is
   begin
      return File.Ending;
   end Ending;

   procedure Ending (File : in File_Access; Value : Line_Ending) is
   begin
      File.Ending := Value;
   end Ending;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (File : in File_Type) return Boolean is
   begin
      return File.FD /= Invalid_FD;
   end Is_Open;

   ---------------
   -- Set_Input --
   ---------------

   procedure Set_Input (File : in File_Type) is
   begin
      Cur_In := File;
   end Set_Input;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output (File : in File_Type) is
   begin
      Cur_Out := File;
   end Set_Output;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error (File : in File_Type) is
   begin
      Cur_Err := File;
   end Set_Error;

   --------------------
   -- Standard_Input --
   --------------------

   function Standard_Input return File_Type is
   begin
      return Std_In;
   end Standard_Input;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return File_Type is
   begin
      return Std_Out;
   end Standard_Output;

   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return File_Type is
   begin
      return Std_Err;
   end Standard_Error;

   -------------------
   -- Current_Input --
   -------------------

   function Current_Input return File_Type is
   begin
      return Cur_In;
   end Current_Input;

   --------------------
   -- Current_Output --
   --------------------

   function Current_Output return File_Type is
   begin
      return Cur_Out;
   end Current_Output;

   -------------------
   -- Current_Error --
   -------------------

   function Current_Error return File_Type is
   begin
      return Cur_Err;
   end Current_Error;

   --------------------
   -- Standard_Input --
   --------------------

   function Standard_Input return File_Access is
   begin
      return Std_In'Access;
   end Standard_Input;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return File_Access is
   begin
      return Std_Out'Access;
   end Standard_Output;

   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return File_Access is
   begin
      return Std_Err'Access;
   end Standard_Error;

   -------------------
   -- Current_Input --
   -------------------

   function Current_Input return File_Access is
   begin
      return Cur_In'Access;
   end Current_Input;

   --------------------
   -- Current_Output --
   --------------------

   function Current_Output return File_Access is
   begin
      return Cur_Out'Access;
   end Current_Output;

   -------------------
   -- Current_Error --
   -------------------

   function Current_Error return File_Access is
   begin
      return Cur_Err'Access;
   end Current_Error;

   -----------
   -- Flush --
   -----------

   procedure Flush (File : in File_Type) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Flush unimplemented");
      raise Program_Error with "Unimplemented procedure Flush";
   end Flush;

   -----------
   -- Flush --
   -----------

   procedure Flush is
   begin
      pragma Compile_Time_Warning (Standard.True, "Flush unimplemented");
      raise Program_Error with "Unimplemented procedure Flush";
   end Flush;

   ---------------------
   -- Set_Line_Length --
   ---------------------

   procedure Set_Line_Length (File : in File_Type; To : in Count) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Set_Line_Length unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Line_Length";
   end Set_Line_Length;

   ---------------------
   -- Set_Line_Length --
   ---------------------

   procedure Set_Line_Length (To : in Count) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Set_Line_Length unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Line_Length";
   end Set_Line_Length;

   ---------------------
   -- Set_Page_Length --
   ---------------------

   procedure Set_Page_Length (File : in File_Type; To : in Count) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Set_Page_Length unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Page_Length";
   end Set_Page_Length;

   ---------------------
   -- Set_Page_Length --
   ---------------------

   procedure Set_Page_Length (To : in Count) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Set_Page_Length unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Page_Length";
   end Set_Page_Length;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length (File : in File_Type) return Count is
   begin
      pragma Compile_Time_Warning (Standard.True, "Line_Length unimplemented");
      return raise Program_Error with "Unimplemented function Line_Length";
   end Line_Length;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length return Count is
   begin
      pragma Compile_Time_Warning (Standard.True, "Line_Length unimplemented");
      return raise Program_Error with "Unimplemented function Line_Length";
   end Line_Length;

   -----------------
   -- Page_Length --
   -----------------

   function Page_Length (File : in File_Type) return Count is
   begin
      pragma Compile_Time_Warning (Standard.True, "Page_Length unimplemented");
      return raise Program_Error with "Unimplemented function Page_Length";
   end Page_Length;

   -----------------
   -- Page_Length --
   -----------------

   function Page_Length return Count is
   begin
      pragma Compile_Time_Warning (Standard.True, "Page_Length unimplemented");
      return raise Program_Error with "Unimplemented function Page_Length";
   end Page_Length;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (File : in File_Type; Spacing : in Positive_Count := 1) is
      NL : constant String :=
        (case File.Ending is when CR_Ending => (1 => Character'val (13)), when LF_Ending => (1 => Character'val (10)),
           when CRLF_Ending                 => Character'Val (13) & Character'Val (10));
      Dummy_Result : Integer;
   begin
      case File.Scheme is
         when Latin_1 | UTF_8 =>
            for Count in 1 .. Spacing loop
               Dummy_Result := Write (File.FD, NL'Address, NL'Length);
            end loop;
         when UTF_16BE | UTF_16LE =>
            declare
               Buffer : UTF_16_Character_Array := To_UTF_16 (From_Latin_1 (NL), File.Scheme);
            begin
               for Count in 1 .. Spacing loop
                  Dummy_Result := Write (File.FD, Buffer'Address, Buffer'Length);
               end loop;
            end;
      end case;
   end New_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Spacing : in Positive_Count := 1) is
   begin
      New_Line (Cur_Out, Spacing);
   end New_Line;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line (File : in File_Type; Spacing : in Positive_Count := 1) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Skip_Line unimplemented");
      raise Program_Error with "Unimplemented procedure Skip_Line";
   end Skip_Line;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line (Spacing : in Positive_Count := 1) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Skip_Line unimplemented");
      raise Program_Error with "Unimplemented procedure Skip_Line";
   end Skip_Line;

   -----------------
   -- End_Of_Line --
   -----------------

   function End_Of_Line (File : in File_Type) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "End_Of_Line unimplemented");
      return raise Program_Error with "Unimplemented function End_Of_Line";
   end End_Of_Line;

   -----------------
   -- End_Of_Line --
   -----------------

   function End_Of_Line return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "End_Of_Line unimplemented");
      return raise Program_Error with "Unimplemented function End_Of_Line";
   end End_Of_Line;

   ---------------
   -- Line_Mark --
   ---------------

   function Line_Mark return UXString is
   begin
      return LM;
   end Line_Mark;

   procedure Line_Mark (Ending : Line_Ending) is
   begin
      LM :=
        (case Ending is when CR_Ending => From_Latin_1 (Character'val (13)), when LF_Ending => From_Latin_1 (Character'val (10)),
           when CRLF_Ending            => From_Latin_1 (Character'Val (13) & Character'Val (10)));
   end Line_Mark;

   --------------
   -- New_Page --
   --------------

   procedure New_Page (File : in File_Type) is
   begin
      pragma Compile_Time_Warning (Standard.True, "New_Page unimplemented");
      raise Program_Error with "Unimplemented procedure New_Page";
   end New_Page;

   --------------
   -- New_Page --
   --------------

   procedure New_Page is
   begin
      pragma Compile_Time_Warning (Standard.True, "New_Page unimplemented");
      raise Program_Error with "Unimplemented procedure New_Page";
   end New_Page;

   ---------------
   -- Skip_Page --
   ---------------

   procedure Skip_Page (File : in File_Type) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Skip_Page unimplemented");
      raise Program_Error with "Unimplemented procedure Skip_Page";
   end Skip_Page;

   ---------------
   -- Skip_Page --
   ---------------

   procedure Skip_Page is
   begin
      pragma Compile_Time_Warning (Standard.True, "Skip_Page unimplemented");
      raise Program_Error with "Unimplemented procedure Skip_Page";
   end Skip_Page;

   -----------------
   -- End_Of_Page --
   -----------------

   function End_Of_Page (File : in File_Type) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "End_Of_Page unimplemented");
      return raise Program_Error with "Unimplemented function End_Of_Page";
   end End_Of_Page;

   -----------------
   -- End_Of_Page --
   -----------------

   function End_Of_Page return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "End_Of_Page unimplemented");
      return raise Program_Error with "Unimplemented function End_Of_Page";
   end End_Of_Page;

   ---------------
   -- Page_Mark --
   ---------------

   function Page_Mark return UXString is
   begin
      pragma Compile_Time_Warning (Standard.True, "Page_Mark unimplemented");
      return raise Program_Error with "Unimplemented function Page_Mark";
   end Page_Mark;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : in File_Type) return Boolean is
   begin
      return File.EOF and File.Buffer.Length = 0;
   end End_Of_File;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File return Boolean is
   begin
      return End_Of_File (Current_Input);
   end End_Of_File;

   -------------
   -- Set_Col --
   -------------

   procedure Set_Col (File : in File_Type; To : in Positive_Count) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Set_Col unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Col";
   end Set_Col;

   -------------
   -- Set_Col --
   -------------

   procedure Set_Col (To : in Positive_Count) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Set_Col unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Col";
   end Set_Col;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line (File : in File_Type; To : in Positive_Count) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Set_Line unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Line";
   end Set_Line;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line (To : in Positive_Count) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Set_Line unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Line";
   end Set_Line;

   ---------
   -- Col --
   ---------

   function Col (File : in File_Type) return Positive_Count is
   begin
      pragma Compile_Time_Warning (Standard.True, "Col unimplemented");
      return raise Program_Error with "Unimplemented function Col";
   end Col;

   ---------
   -- Col --
   ---------

   function Col return Positive_Count is
   begin
      pragma Compile_Time_Warning (Standard.True, "Col unimplemented");
      return raise Program_Error with "Unimplemented function Col";
   end Col;

   ----------
   -- Line --
   ----------

   function Line (File : in File_Type) return Positive_Count is
   begin
      pragma Compile_Time_Warning (Standard.True, "Line unimplemented");
      return raise Program_Error with "Unimplemented function Line";
   end Line;

   ----------
   -- Line --
   ----------

   function Line return Positive_Count is
   begin
      pragma Compile_Time_Warning (Standard.True, "Line unimplemented");
      return raise Program_Error with "Unimplemented function Line";
   end Line;

   ----------
   -- Page --
   ----------

   function Page (File : in File_Type) return Positive_Count is
   begin
      pragma Compile_Time_Warning (Standard.True, "Page unimplemented");
      return raise Program_Error with "Unimplemented function Page";
   end Page;

   ----------
   -- Page --
   ----------

   function Page return Positive_Count is
   begin
      pragma Compile_Time_Warning (Standard.True, "Page unimplemented");
      return raise Program_Error with "Unimplemented function Page";
   end Page;

   -------------
   -- Put_BOM --
   -------------

   procedure Put_BOM (File : in File_Type) is
      Dummy_Result : Integer;
      use Ada.Strings.UTF_Encoding;
   begin
      case File.Scheme is
         when Latin_1 =>
            null;
         when UTF_8 =>
            Dummy_Result := Write (File.FD, BOM_8'Address, BOM_8'Length);
         when UTF_16BE =>
            Dummy_Result := Write (File.FD, BOM_16BE'Address, BOM_16BE'Length);
         when UTF_16LE =>
            Dummy_Result := Write (File.FD, BOM_16LE'Address, BOM_16LE'Length);
      end case;
   end Put_BOM;

   -------------
   -- Put_BOM --
   -------------

   procedure Put_BOM is
   begin
      Put_BOM (Cur_Out);
   end Put_BOM;

   ---------
   -- Get --
   ---------

   procedure Get (File : in out File_Type; Item : out Unicode_Character) is
   begin
      if File.Buffer.Length = 0 then
         Read_More (File);
      end if;
      Item := Get_Unicode (File.Buffer, 1);
      Delete (File.Buffer, 1, 1);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get (Item : out Unicode_Character) is
   begin
      Get (Cur_In, Item);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (File : in File_Type; Item : in Unicode_Character) is
      Dummy_Result : Integer;
   begin
      case File.Scheme is
         when Latin_1 =>
            declare
               Buffer : constant String := To_Latin_1 (From_Unicode (Item));
            begin
               Dummy_Result := Write (File.FD, Buffer'Address, Buffer'Length);
            end;
         when UTF_8 =>
            declare
               Buffer : constant String := String (To_UTF_8 (From_Unicode (Item)));
            begin
               Dummy_Result := Write (File.FD, Buffer'Address, Buffer'Length);
            end;
         when UTF_16BE | UTF_16LE =>
            declare
               Buffer : UTF_16_Character_Array := To_UTF_16 (From_Unicode (Item), File.Scheme);
            begin
               Dummy_Result := Write (File.FD, Buffer'Address, Buffer'Length);
            end;
      end case;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : in Unicode_Character) is
   begin
      Put (Cur_Out, Item);
   end Put;

   ----------------
   -- Look_Ahead --
   ----------------

   procedure Look_Ahead (File : in File_Type; Item : out Unicode_Character; End_Of_Line : out Boolean) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Look_Ahead unimplemented");
      raise Program_Error with "Unimplemented procedure Look_Ahead";
   end Look_Ahead;

   ----------------
   -- Look_Ahead --
   ----------------

   procedure Look_Ahead (Item : out Unicode_Character; End_Of_Line : out Boolean) is
   begin
      Look_Ahead (Cur_In, Item, End_Of_Line);
   end Look_Ahead;

   -------------------
   -- Get_Immediate --
   -------------------

   procedure Get_Immediate (File : in File_Type; Item : out Unicode_Character) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Immediate unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Immediate";
   end Get_Immediate;

   -------------------
   -- Get_Immediate --
   -------------------

   procedure Get_Immediate (Item : out Unicode_Character) is
   begin
      Get_Immediate (Cur_In, Item);
   end Get_Immediate;

   -------------------
   -- Get_Immediate --
   -------------------

   procedure Get_Immediate (File : in File_Type; Item : out Unicode_Character; Available : out Boolean) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Immediate unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Immediate";
   end Get_Immediate;

   -------------------
   -- Get_Immediate --
   -------------------

   procedure Get_Immediate (Item : out Unicode_Character; Available : out Boolean) is
   begin
      Get_Immediate (Cur_In, Item, Available);
   end Get_Immediate;

   ---------
   -- Get --
   ---------

   procedure Get (File : in out File_Type; Item : out UXString; Length : in Count) is
      Last : Natural;
   begin
      if File.Buffer.Length < Length then
         Read_More (File);
      end if;
      Last := Natural'Min (File.Buffer.Length, Length);
      Item := Slice (File.Buffer, 1, Last);
      Delete (File.Buffer, 1, Last);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get (Item : out UXString; Length : in Count) is
   begin
      Get (Cur_In, Item, Length);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (File : in File_Type; Item : in UXString) is
      Dummy_Result : Integer;
   begin
      case File.Scheme is
         when Latin_1 =>
            declare
               Buffer : constant String := To_Latin_1 (Item);
            begin
               Dummy_Result := Write (File.FD, Buffer'Address, Buffer'Length);
            end;
         when UTF_8 =>
            declare
               Buffer : constant String := String (To_UTF_8 (Item));
            begin
               Dummy_Result := Write (File.FD, Buffer'Address, Buffer'Length);
            end;
         when UTF_16BE | UTF_16LE =>
            declare
               Buffer : UTF_16_Character_Array := To_UTF_16 (Item, File.Scheme);
            begin
               Dummy_Result := Write (File.FD, Buffer'Address, Buffer'Length);
            end;
      end case;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : in UXString) is
   begin
      Put (Cur_Out, Item);
   end Put;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (File : in out File_Type; Item : out UXString) is
      LM : constant UXString :=
        (case File.Ending is when CR_Ending => From_Latin_1 (Character'val (13)), when LF_Ending => From_Latin_1 (Character'val (10)),
           when CRLF_Ending                 => From_Latin_1 (Character'Val (13) & Character'Val (10)));
      EOL : Natural := Index (File.Buffer, LM);
   begin
      while EOL = 0 loop
         -- Read one more time even if EOF is set, in case of standard input
         Read_More (File);
         EOL := Index (File.Buffer, LM);
         exit when File.EOF;
      end loop;
      if EOL /= 0 then
         Slice (File.Buffer, Item, 1, EOL - 1);
         Delete (File.Buffer, 1, EOL - 1 + LM.Length);
      elsif File.Buffer.Length /= 0 then
         Item        := File.Buffer;
         File.Buffer := Null_UXString;
      end if;
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (Item : out UXString) is
   begin
      Get_Line (Cur_In, Item);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (File : in out File_Type) return UXString is
   begin
      return Line : UXString do
         Get_Line (File, Line);
      end return;
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line return UXString is
   begin
      return Get_Line (Cur_In);
   end Get_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (File : in File_Type; Item : in UXString) is
   begin
      Put (File, Item);
      New_Line (File, 1);
   end Put_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Item : in UXString) is
   begin
      Put_Line (Cur_Out, Item);
   end Put_Line;

end UXStrings.Text_IO;