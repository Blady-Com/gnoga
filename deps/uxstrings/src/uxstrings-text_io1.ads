private with GNAT.OS_Lib;

package UXStrings.Text_IO is

   type File_Type is limited private;
   type File_Access is access all File_Type;

   type File_Mode is (In_File, Out_File, Append_File);

   subtype Count is Natural range 0 .. Natural'Last;
   subtype Positive_Count is Count range 1 .. Count'Last;

   type Line_Ending is (CR_Ending, LF_Ending, CRLF_Ending);

   -- File Management

   procedure Create
     (File   : in out File_Type; Mode : in File_Mode := Out_File; Name : in UXString := Null_UXString;
      Scheme : in     Encoding_Scheme := Latin_1; Ending : Line_Ending := CRLF_Ending);
   procedure Open
     (File   : in out File_Type; Mode : in File_Mode; Name : in UXString; Scheme : in Encoding_Scheme := Latin_1;
      Ending :        Line_Ending := CRLF_Ending);

   procedure Close (File : in out File_Type);
   procedure Delete (File : in out File_Type);
   procedure Reset (File : in out File_Type; Mode : in File_Mode);
   procedure Reset (File : in out File_Type);

   function Mode (File : in File_Type) return File_Mode;
   function Name (File : in File_Type) return UXString;
   function Scheme (File : in File_Type) return Encoding_Scheme;
   procedure Scheme (File : in File_Access; Value : in Encoding_Scheme);
   function Ending (File : in File_Type) return Line_Ending;
   procedure Ending (File : in File_Access; Value : Line_Ending);

   function Is_Open (File : in File_Type) return Boolean;

   -- Control of default input and output files

   procedure Set_Input (File : in File_Type);
   procedure Set_Output (File : in File_Type);
   procedure Set_Error (File : in File_Type);

   function Standard_Input return File_Type;
   function Standard_Output return File_Type;
   function Standard_Error return File_Type;

   function Current_Input return File_Type;
   function Current_Output return File_Type;
   function Current_Error return File_Type;

   function Standard_Input return File_Access;
   function Standard_Output return File_Access;
   function Standard_Error return File_Access;

   function Current_Input return File_Access;
   function Current_Output return File_Access;
   function Current_Error return File_Access;

   -- Buffer control

   procedure Flush (File : in File_Type);
   procedure Flush;

   -- Specification of line and page lengths

   procedure Set_Line_Length (File : in File_Type; To : in Count);
   procedure Set_Line_Length (To : in Count);

   procedure Set_Page_Length (File : in File_Type; To : in Count);
   procedure Set_Page_Length (To : in Count);

   function Line_Length (File : in File_Type) return Count;
   function Line_Length return Count;

   function Page_Length (File : in File_Type) return Count;
   function Page_Length return Count;

   -- Column, Line, and Page Control

   procedure New_Line (File : in File_Type; Spacing : in Positive_Count := 1);
   procedure New_Line (Spacing : in Positive_Count := 1);

   procedure Skip_Line (File : in File_Type; Spacing : in Positive_Count := 1);
   procedure Skip_Line (Spacing : in Positive_Count := 1);

   function End_Of_Line (File : in File_Type) return Boolean;
   function End_Of_Line return Boolean;

   function Line_Mark return UXString;
   procedure Line_Mark (Ending : Line_Ending);

   procedure New_Page (File : in File_Type);
   procedure New_Page;

   procedure Skip_Page (File : in File_Type);
   procedure Skip_Page;

   function End_Of_Page (File : in File_Type) return Boolean;
   function End_Of_Page return Boolean;

   function Page_Mark return UXString;

   function End_Of_File (File : in File_Type) return Boolean;
   function End_Of_File return Boolean;

   procedure Set_Col (File : in File_Type; To : in Positive_Count);
   procedure Set_Col (To : in Positive_Count);

   procedure Set_Line (File : in File_Type; To : in Positive_Count);
   procedure Set_Line (To : in Positive_Count);

   function Col (File : in File_Type) return Positive_Count;
   function Col return Positive_Count;

   function Line (File : in File_Type) return Positive_Count;
   function Line return Positive_Count;

   function Page (File : in File_Type) return Positive_Count;
   function Page return Positive_Count;

   -- Byte Order Mark Output (with respect of Encoding Scheme)

   procedure Put_BOM (File : in File_Type);
   procedure Put_BOM;

   -- Unicode Character Input-Output

   procedure Get (File : in out File_Type; Item : out Unicode_Character);
   procedure Get (Item : out Unicode_Character);

   procedure Put (File : in File_Type; Item : in Unicode_Character);
   procedure Put (Item : in Unicode_Character);

   procedure Look_Ahead (File : in out File_Type; Item : out Unicode_Character; End_Of_Line : out Boolean);
   procedure Look_Ahead (Item : out Unicode_Character; End_Of_Line : out Boolean);

   procedure Get_Immediate (File : in out File_Type; Item : out Unicode_Character);
   procedure Get_Immediate (Item : out Unicode_Character);

   procedure Get_Immediate (File : in out File_Type; Item : out Unicode_Character; Available : out Boolean);
   procedure Get_Immediate (Item : out Unicode_Character; Available : out Boolean);

   -- Unicode String Input-Output

   procedure Get (File : in out File_Type; Item : out UXString; Length : in Count);
   procedure Get (Item : out UXString; Length : in Count);

   procedure Put (File : in File_Type; Item : in UXString);
   procedure Put (Item : in UXString);

   procedure Get_Line (File : in out File_Type; Item : out UXString);
   procedure Get_Line (Item : out UXString);

   function Get_Line (File : in out File_Type) return UXString;
   function Get_Line return UXString;

   procedure Put_Line (File : in File_Type; Item : in UXString);
   procedure Put_Line (Item : in UXString);

   ----------------
   -- Exceptions --
   ----------------

   Status_Error : exception;
   Mode_Error   : exception;
   Name_Error   : exception;
   Use_Error    : exception;
   Device_Error : exception;
   End_Error    : exception;
   Data_Error   : exception;
   Layout_Error : exception;

private

   type String_Access is access String;
   type File_Type is record
      FD     : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Mode   : File_Mode;
      Name   : UXString;
      Scheme : Encoding_Scheme;
      Ending : Line_Ending;
      Buffer : String_Access               := new String'("");
      EOF    : Boolean                     := False;
   end record;

   procedure Read_Stream (File : in out File_Type; Item : out UTF_8_Character_Array; Last : out Natural);
   procedure Write_Stream (File : in out File_Type; Item : UTF_8_Character_Array);

end UXStrings.Text_IO;
