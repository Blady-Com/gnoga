--                                                                    --
--  procedure Test_Data_Client      Copyright (c)  Dmitry A. Kazakov  --
--  Test data client                               Luebeck            --
--                                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  13:09 10 Mar 2013  --
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
with Ada.Streams;            use Ada.Streams;
with Ada.Text_IO;            use Ada.Text_IO;
with GNAT.Sockets;           use GNAT.Sockets;
with Interfaces;             use Interfaces;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Floats;    use Strings_Edit.Floats;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

with Ada.Numerics.Discrete_Random;

with GNAT.Sockets.Connection_State_Machine.Big_Endian.IEEE_754_Floats;
with GNAT.Sockets.Connection_State_Machine.Big_Endian.
                                           IEEE_754_Long_Floats;
with GNAT.Sockets.Connection_State_Machine.Big_Endian.Integers;
with GNAT.Sockets.Connection_State_Machine.Big_Endian.Unsigneds;
with GNAT.Sockets.Connection_State_Machine.Chain_Code.Naturals;
with GNAT.Sockets.Connection_State_Machine.Chain_Code.Integers;
with GNAT.Sockets.Connection_State_Machine.Little_Endian.Integers;
with GNAT.Sockets.Connection_State_Machine.Little_Endian.
                                           IEEE_754_Floats;
with GNAT.Sockets.Connection_State_Machine.Little_Endian.
                                           IEEE_754_Long_Floats;
with GNAT.Sockets.Connection_State_Machine.Little_Endian.Unsigneds;
with GNAT.Sockets.Connection_State_Machine.Terminated_Strings;
with GNAT.Sockets.Connection_State_Machine.Variable_Length_Strings;
with GNAT.Sockets.Server;

procedure Test_Data_Client is
   use Connection_State_Machine;

   package Random_Integers is
     new Ada.Numerics.Discrete_Random (Integer);
   use Random_Integers;

   Port           : constant := 5876;
   Session_Length : constant := 200;

   Dice    : Generator;
   Socket  : Socket_Type;
   Address : Sock_Addr_Type;
   Packet  : Stream_Element_Array (1..200);
   Size    : Stream_Element_Offset;

   N1  : Unsigned_8  := 0;
   N2  : Unsigned_16 := 0;
   N3  : Unsigned_32 := 0;
   N4  : Unsigned_64 := 0;
   N5  : Unsigned_8  := 0;
   N6  : Unsigned_16 := 0;
   N7  : Unsigned_32 := 0;
   N8  : Unsigned_64 := 0;
   N10 : Integer;
   F1  : Float       := 0.0;
   D1  : Long_Float  := 0.0;
begin
   Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
   Address.Port := Port;
   Create_Socket (Socket);
   Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
   Connect_Socket (Socket, Address);
   for No in 1..Session_Length loop
      declare
         Pointer : Stream_Element_Offset := Packet'First;
         Last    : Stream_Element_Offset;
      begin
         N1 := N1 + 1;
         N2 := N2 + 21;
         N3 := N3 + 337;
         N4 := N4 + 47777;
         N5 := N5 + 5;
         N6 := N6 + 6561;
         N7 := N7 + 71;
         N8 := N8 + 7773;
         F1 := F1 + 58191.0543211;
         D1 := D1 - 94591.0111;

         N10 := Random (Dice);
         Little_Endian.Unsigneds.Put (Packet, Pointer, N1);
         Little_Endian.Unsigneds.Put (Packet, Pointer, N2);
         Little_Endian.Unsigneds.Put (Packet, Pointer, N3);
         Little_Endian.Unsigneds.Put (Packet, Pointer, N4);
         Big_Endian.Unsigneds.Put (Packet, Pointer, N5);
         Big_Endian.Unsigneds.Put (Packet, Pointer, N6);
         Big_Endian.Unsigneds.Put (Packet, Pointer, N7);
         Big_Endian.Unsigneds.Put (Packet, Pointer, N8);
         Big_Endian.IEEE_754_Floats.Put (Packet, Pointer, F1);
         Big_Endian.IEEE_754_Long_Floats.Put (Packet, Pointer, D1);
         Little_Endian.IEEE_754_Floats.Put (Packet, Pointer, F1);
         Little_Endian.IEEE_754_Long_Floats.Put (Packet, Pointer, D1);

         Terminated_Strings.Put
         (  Packet,
            Pointer,
            Image (No),
            Character'Val (0)
         );

         Put
         (  "Packet:"
         &  Unsigned_8'Image  (N1)
         &  Unsigned_16'Image (N2)
         &  Unsigned_32'Image (N3)
         &  Unsigned_64'Image (N4)
         &  Unsigned_8'Image  (N5)
         &  Unsigned_16'Image (N6)
         &  Unsigned_32'Image (N7)
         &  Unsigned_64'Image  (N8)
         &  " " & Image (F1)
         &  " " & Image (Float (D1))
         &  " " & Image (F1)
         &  " " & Image (Float (D1))
         &  " " & Quote (Image (No))
         );

         declare
            S2 : String := Long_Float'Image (D1);
         begin
            Put (" " & Quote (S2));
            Big_Endian.Integers.Put
            (  Packet,
               Pointer,
               Integer_16 (S2'Length)
            );
            Variable_Length_Strings.Put (Packet, Pointer, S2);
         end;
         Chain_Code.Naturals.Put (Packet, Pointer, Natural (N8));
         Chain_Code.Integers.Put (Packet, Pointer, N10);

         Put (" " & Image (Natural (N8)) & " " & Image (N10));
         case N8 mod 4 is
            when 0 =>
               Little_Endian.Integers.Put
               (  Packet,
                  Pointer,
                  Integer_16 (No)
               );
               Put (" SELECTED=1 " & Image (No));
            when 1 =>
               Chain_Code.Naturals.Put
               (  Packet,
                  Pointer,
                  Natural (No)
               );
               Chain_Code.Naturals.Put
               (  Packet,
                  Pointer,
                  Natural (No)
               );
               Put (" SELECTED=2 " & Image (No) & " " & Image (No));
            when 2 =>
               Terminated_Strings.Put
               (  Packet,
                  Pointer,
                  Image (No),
                  Character'Val (0)
               );
               Put (" SELECTED=3 " & Quote (Image (No)));
            when others =>
               Put (" SELECTED=4");
         end case;
         New_Line;
         Size := Pointer - 1;
         Send_Socket (Socket, Packet (1..Size), Last);
         if Last /= Size then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Send error:"
               &  Stream_Element_Offset'Image (Last)
               &  " sent less than expected"
               &  Stream_Element_Offset'Image (Size)
            )  );
         end if;
      end;
      declare
         Pointer : aliased Stream_Element_Offset := Packet'First;
         Last    : Stream_Element_Offset;
         L1      : Unsigned_8  := 0;
         L2      : Unsigned_16 := 0;
         L3      : Unsigned_32 := 0;
         L4      : Unsigned_64 := 0;
         L5      : Float       := 0.0;
         L6      : Long_Float  := 0.0;
         L8      : Natural     := 0;
         L10     : Integer     := 0;
      begin
         Receive_Socket (Socket, Packet (1..Size), Last);
         Big_Endian.Unsigneds.Get (Packet, Pointer, L1);
         if N1 /= L1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse 8-bit big endian: got"
               &  Unsigned_8'Image (L1)
               &  ", expected"
               &  Unsigned_8'Image (N1)
            )  );
         end if;
         Big_Endian.Unsigneds.Get (Packet, Pointer, L2);
         if N2 /= L2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse 16-bit big endian: got"
               &  Unsigned_16'Image (L2)
               &  ", expected"
               &  Unsigned_16'Image (N2)
            )  );
         end if;
         Big_Endian.Unsigneds.Get (Packet, Pointer, L3);
         if N3 /= L3 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse 32-bit big endian: got"
               &  Unsigned_32'Image (L3)
               &  ", expected"
               &  Unsigned_32'Image (N3)
            )  );
         end if;
         Big_Endian.Unsigneds.Get (Packet, Pointer, L4);
         if N4 /= L4 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse 64-bit big endian: got"
               &  Unsigned_64'Image (L4)
               &  ", expected"
               &  Unsigned_64'Image (N4)
            )  );
         end if;
         Little_Endian.Unsigneds.Get (Packet, Pointer, L1);
         if N5 /= L1 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse 8-bit little endian: got"
               &  Unsigned_8'Image (L1)
               &  ", expected"
               &  Unsigned_8'Image (N5)
            )  );
         end if;
         Little_Endian.Unsigneds.Get (Packet, Pointer, L2);
         if N6 /= L2 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse 16-bit little endian: got"
               &  Unsigned_16'Image (L2)
               &  ", expected"
               &  Unsigned_16'Image (N6)
            )  );
         end if;
         Little_Endian.Unsigneds.Get (Packet, Pointer, L3);
         if N7 /= L3 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse 32-bit little endian: got"
               &  Unsigned_32'Image (L3)
               &  ", expected"
               &  Unsigned_32'Image (N7)
            )  );
         end if;
         Little_Endian.Unsigneds.Get (Packet, Pointer, L4);
         if N8 /= L4 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse 64-bit little endian: got"
               &  Unsigned_64'Image (L4)
               &  ", expected"
               &  Unsigned_64'Image (N8)
            )  );
         end if;
         Big_Endian.IEEE_754_Floats.Get (Packet, Pointer, L5);
         if abs (F1 - L5) > 0.0001 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse 32-bit big endian IEEE: got"
               &  Float'Image (L5)
               &  ", expected"
               &  Float'Image (F1)
            )  );
         end if;
         Big_Endian.IEEE_754_Long_Floats.Get (Packet, Pointer, L6);
         if abs (D1 - L6) > 0.0000001 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse 64-bit big endian IEEE: got"
               &  Long_Float'Image (L6)
               &  ", expected"
               &  Long_Float'Image (D1)
            )  );
         end if;
         Little_Endian.IEEE_754_Floats.Get (Packet, Pointer, L5);
         if abs (F1 - L5) > 0.0001 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse 32-bit big endian IEEE: got"
               &  Float'Image (L5)
               &  ", expected"
               &  Float'Image (F1)
            )  );
         end if;
         Little_Endian.IEEE_754_Long_Floats.Get (Packet, Pointer, L6);
         if abs (D1 - L6) > 0.0000001 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse 64-bit big endian IEEE: got"
               &  Long_Float'Image (L6)
               &  ", expected"
               &  Long_Float'Image (D1)
            )  );
         end if;
         if (  No
            /= Value
               (  Terminated_Strings.Get
                  (  Packet,
                     Pointer'Access,
                     Character'Val (0)
            )  )  )
         then
            Raise_Exception
            (  Data_Error'Identity,
               "Error nul-terminated string reponse"
            );
         end if;
         declare
            S : String := Terminated_Strings.Get
                          (  Packet,
                             Pointer'Access,
                             Character'Val (0)
                          );
         begin
            if Long_Float'Image (D1) /= S then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Error string reponse, expect "
                  &  Long_Float'Image (D1)
                  &  ", got '"
                  &  S
                  &  "' Pointer"
                  &  Stream_Element_Offset'Image (Pointer)
               )  );
            end if;
         end;
         Chain_Code.Naturals.Get (Packet, Pointer, L8);
         if Natural (N8) /= L8 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse chain code natural: got"
               &  Integer'Image (L8)
               &  ", expected"
               &  Unsigned_64'Image (N8)
            )  );
         end if;
         Chain_Code.Integers.Get (Packet, Pointer, L10);
         if N10 /= L10 then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error reponse chain code integer: got"
               &  Integer'Image (L10)
               &  ", expected"
               &  Integer'Image (N10)
            )  );
         end if;
         case N8 mod 4 is
            when 0 =>
               declare
                  L : Integer_16;
               begin
                  Little_Endian.Integers.Get (Packet, Pointer, L);
                  if Integer_16 (No) /= L then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Error variant (1) record: got"
                        &  Integer_16'Image (L)
                        &  ", expected"
                        &  Integer'Image (No)
                     )  );
                  end if;
               end;
            when 1 =>
               declare
                  L : Natural;
               begin
                  Chain_Code.Naturals.Get (Packet, Pointer, L);
                  if No /= L then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Error variant (2.1) record: got"
                        &  Integer'Image (L)
                        &  ", expected"
                        &  Integer'Image (No)
                     )  );
                  end if;
                  Chain_Code.Naturals.Get (Packet, Pointer, L);
                  if No /= L then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Error variant (2.2) record: got"
                        &  Integer'Image (L)
                        &  ", expected"
                        &  Integer'Image (No)
                     )  );
                  end if;
               end;
            when 2 =>
               if (  No
                  /= Value
                     (  Terminated_Strings.Get
                        (  Packet,
                           Pointer'Access,
                           Character'Val (0)
                  )  )  )
               then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Error variant (3) reponse"
                  );
               end if;
            when others =>
               null;
         end case;
      end;
   end loop;
   Shutdown_Socket (Socket);
   Close_Socket (Socket);
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Data_Client;
