with Ada.Streams;
package UXStrings.Text_IO.Text_Streams is

   type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

   function Stream (File : File_Type) return Stream_Access;
   -- Return an stream access to File

private

   type Stream_File is new Ada.Streams.Root_Stream_Type with record
      File : File_Access;
   end record;

   use Ada.Streams;

   overriding procedure Read
     (Stream : in out Stream_File; Item : out Stream_Element_Array; Last : out Stream_Element_Offset);

   overriding procedure Write (Stream : in out Stream_File; Item : Stream_Element_Array);

end UXStrings.Text_IO.Text_Streams;
