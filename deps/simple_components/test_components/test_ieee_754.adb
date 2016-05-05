--                                                                    --
--  procedure Test_IEEE_754         Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Summer, 2008       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Text_IO;            use Ada.Text_IO;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with IEEE_754.Floats;
with IEEE_754.Long_Floats;

procedure Test_IEEE_754 is
begin
   declare
      use IEEE_754.Floats;
      function Image (X : Float_32) return String is
         Text    : String (1..80);
         Pointer : Integer := 1;
      begin
         for I in X'Range loop
            Put
            (  Text,
               Pointer,
               Integer (X (I)),
               Field => 2,
               Fill  => '0',
               Base  => 16
            );
            Put (Text, Pointer, ' ');
         end loop;
         return Text (1..Pointer - 2);
      end Image;
   begin
      Put_Line ("Float'First " & Image (To_IEEE (Float'First)));
      Put_Line ("Float'Lasst " & Image (To_IEEE (Float'Last)));
      Put_Line ("Float'Small " & Image (To_IEEE (Float'Small)));
      for Power in -10..10 loop
         declare
            N : constant Float := Float'Scaling (1.0, Power);
         begin
            if From_IEEE (To_IEEE (N)) /= N then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "N=" & Image (Power)
                  &  " IEEE=" & Image (To_IEEE (N))
                  &  " Back=" & Float'Image (N)
               )  );
            end if;
         end;
      end loop;
      if Float_32'((0, 0, 0, 1)) /= To_IEEE (1.401298464324817e-45) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in "
            &  Image (To_IEEE (1.401298464324817e-45))
            &  " /= "
            &  Image (Float_32'((0, 0, 0, 1)))
         )  );
      end if;
      if From_IEEE ((0, 0, 0, 1)) /= 1.401298464324817e-45 then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in 1.401298464324817e-45 /="
            &  Float'Image (From_IEEE ((0, 0, 0, 1)))
         )  );
      end if;
      if (  From_IEEE ((16#00#, 16#7F#, 16#FF#, 16#FF#))
         /= 1.1754942106924411e-38
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in 1.1754942106924411e-38 /="
            &  Float'Image
               (  From_IEEE ((16#00#, 16#7F#, 16#FF#, 16#FF#))
         )  )  );
      end if;
      if (  Float_32'(16#00#, 16#7F#, 16#FF#, 16#FF#)
         /= To_IEEE (1.1754942106924411e-38)
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in "
            &  Image (To_IEEE (1.1754942106924411e-38))
            &  " /= "
            &  Image (Float_32'(16#00#, 16#7F#, 16#FF#, 16#FF#))
         )  );
      end if;
      if From_IEEE ((16#49#, 16#96#, 16#BB#, 16#38#)) /= 1234791.0 then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in 1234791.0 /="
            &  Float'Image
               (  From_IEEE ((16#49#, 16#96#, 16#BB#, 16#38#))
         )  )  );
      end if;
      if From_IEEE ((16#3F#, 16#80#, 0, 0)) /= 1.0 then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in 1.0 /="
            &  Float'Image (From_IEEE ((16#3F#, 16#80#, 0, 0)))
         )  );
      end if;
      if To_IEEE (1.0) /= (16#3F#, 16#80#, 0, 0) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in "
            &  Image (To_IEEE (1.0))
            &  " /= "
            &  Image ((16#3F#, 16#80#, 0, 0))
         )  );
      end if;
      if From_IEEE (Positive_Zero) /= 0.0 then
         Raise_Exception (Constraint_Error'Identity, "Error in +0");
      end if;
      if From_IEEE (Negative_Zero) /= 0.0 then
         Raise_Exception (Constraint_Error'Identity, "Error in -0");
      end if;
   exception
      when Error : Constraint_Error =>
         Put_Line
         (  "Error (note some tests may fail on some machines "
         &  "depending on the range of the standard type Float): "
         &  Exception_Message (Error)
         );
   end;

   declare
      use IEEE_754.Long_Floats;
      function Image (X : Float_64) return String is
         Text    : String (1..80);
         Pointer : Integer := 1;
      begin
         for I in X'Range loop
            Put
            (  Text,
               Pointer,
               Integer (X (I)),
               Field => 2,
               Fill  => '0',
               Base  => 16
            );
            Put (Text, Pointer, ' ');
         end loop;
         return Text (1..Pointer - 2);
      end Image;
   begin
      Put_Line
      (  "Long_Float'First "
      &  Image (To_IEEE (Long_Float'First))
      );
      Put_Line
      (  "Long_Float'Lasst "
      &  Image (To_IEEE (Long_Float'Last))
      );
      Put_Line
      (  "Long_Float'Small "
      &  Image (To_IEEE (Long_Float'Small))
      );
      if To_IEEE (1.0) /= Float_64'(16#3F#, 16#F0#, others => 0) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in "
            &  Image (To_IEEE (1.0))
            &  " /= "
            &  Image ((16#3F#, 16#F0#, others => 0))
         )  );
      end if;
      if From_IEEE ((16#3F#, 16#F0#, others => 0)) /= 1.0 then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in 1.0 /="
            &  Long_Float'Image
               (  From_IEEE ((16#3F#, 16#F0#, others => 0))
         )  )  );
      end if;
      for Power in -20..20 loop
         declare
            N : constant Long_Float := Long_Float'Scaling (1.0, Power);
         begin
            if From_IEEE (To_IEEE (N)) /= N then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "N=" & Image (Power)
                  &  " IEEE=" & Image (To_IEEE (N))
                  &  " Back=" & Long_Float'Image (N)
               )  );
            end if;
         end;
      end loop;
      if (  From_IEEE
            (( 16#42#, 16#DC#, 16#13#, 16#6E#,
               16#FB#, 16#C2#, 16#DA#, 16#40#
            ))
         /= 123479167798121.0
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in 123479167798121.0 /="
            &  Long_Float'Image
               (  From_IEEE
                  (( 16#42#, 16#DC#, 16#13#, 16#6E#,
                     16#FB#, 16#C2#, 16#DA#, 16#40#
         )  )  )  ));
      end if;
      if From_IEEE (Positive_Zero) /= 0.0 then
         Raise_Exception (Constraint_Error'Identity, "Error in +0");
      end if;
      if From_IEEE (Negative_Zero) /= 0.0 then
         Raise_Exception (Constraint_Error'Identity, "Error in -0");
      end if;
   exception
      when Error : Constraint_Error =>
         Put_Line
         (  "Error (note some tests may fail on some machines "
         &  "depending on the range of the standard type Long_Float): "
         &  Exception_Message (Error)
         );
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_IEEE_754;
