--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_DN                                     Luebeck            --
--  Test                                           Summer, 2019       --
--                                                                    --
--                                Last revision :  18:40 01 Aug 2019  --
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
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Strings_Edit.Distinguished_Names;
use  Strings_Edit.Distinguished_Names;

with Strings_Edit.Object_Identifiers;
use  Strings_Edit.Object_Identifiers;

procedure Test_DN is
   procedure Dump (Name : Distinguished_Name) is
      function Image (Key : Attribute_Key) return String is
      begin
         case Key.Mode is
            when OID_Keyed =>
               return Image (Key.Identifier);
            when Text_Keyed =>
               return Key.Text;
         end case;
      end Image;
   begin
      Put_Line ("Name length " & Image (Get_Length (Name)) & " -------");
      for Component in 1..Get_Length (Name) loop
         Put (Image (Component) & " ");
         for Attribute in 1..Get_Component_Length (Name, Component) loop
            Put (Image (Get_Key (Name, Component, Attribute)));
            Put ("=");
            Put (Get_Value (Name, Component, Attribute));
            New_Line;
            if Attribute /= Get_Component_Length (Name, Component) then
               Put ("  ");
            end if;
         end loop;
      end loop;
   end Dump;

   procedure Check_1 (Text : String) is
      Buffer  : String (1..200);
      Pointer : aliased Integer := Text'First;
      Name    : constant Distinguished_Name :=
                         Get (Text, Pointer'Access);
   begin
      if Pointer /= Text'Last + 1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Unrecognized tail '"
            &  Text (Pointer..Text'Last)
            &  "'"
         )  );
      end if;
      Dump (Name);
      Pointer := Buffer'First;
      Put (Buffer, Pointer, Name);
      Put_Line ("Parsed: " & Buffer (1..Pointer - 1));
      if Name /= Value (Buffer (1..Pointer - 1)) then
         Raise_Exception
         (  Data_Error'Identity,
            "Name does not match "
         );
      end if;
   end Check_1;

   procedure Check_2 (Name : Distinguished_Name; Text : String) is
   begin
      if Name /= Value (Text) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Name '"
            &  Image (Name)
            &  "' /= '"
            &  Text
            &  "'"
         )  );
      end if;
   end Check_2;

   procedure Check_3
             (  Text : String;
                Last : Integer;
                Name : Distinguished_Name
             )  is
      Pointer : aliased Integer := Text'First;
      Value   : constant Distinguished_Name :=
                         Get (Text, Pointer'Access);
   begin
      if Name /= Value then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Name '"
            &  Image (Name)
            &  "' /= '"
            &  Text
            &  "'"
         )  );
      elsif Pointer /= Last + 1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Pointer "
            &  Image (Pointer)
            &  " /= "
            &  Image (Last + 1)
            &  " (expected)"
         )  );
      end if;
   end Check_3;
begin
   Check_1 ("c=US+description=USA,ou=USER,dc=example,dc=com");
   Check_2
   (  Subname
      (  (  ("A"="1" or "B"="2")
         &  ("C"="3" or Value ("1.2.3") ="4" or "D"="5")
         &  ("E"="6" or "F"="7")
         ),
         2,
         3
      ),
      "C=3+1.2.3=4+D=5,E=6+F=7"
   );
   Check_2
   (  ("A"="1" or "B"="2") & ("C"="3" or Value("1.2.3")="4" or "D"="5"),
      "A=1+B=2,C=3+1.2.3=4+D=5"
   );
   Check_1 ("1.2.234=jsmith,4.56.9=example,1.1.1.2.777.0=net");
   Check_1 ("CN=#41636566");
   Check_1 ("CN=Lu\C4\8Di\C4\87");
   Check_1 ("OU=Sales+CN=J.  Smith,DC=example,DC=net");
   Check_1 ("CN=James \""Jim\"" Smith\, III,DC=example,DC=net");
   Check_1 ("UID=jsmith,DC=example,DC=net");
   Check_2
   (  ("OU"="Sales" or "CN"="J.  Smith") and
      "DC"="example" and
      "DC"="net",
      "CN=J.  Smith+OU=Sales,DC=example,DC=net"
   );
   Check_2
   (  "UID"="jsmith" and "DC"="example" and "DC"="net",
      "UID=jsmith,DC=example,DC=net"
   );
   Check_2 ("CN"="James", "cn=James");
   Check_2
   (  "ou"="People" and "dc"="example" and "dc"="com",
      "ou=People, dc=example, dc=com"
   );
   Check_2
   (  "ou"="People" and "dc"="example" and "dc"="com",
      "ou = People , dc = example , dc = com  "
   );
   Check_2
   (  "ou"="Peopl  e" and "dc"="example" and "dc"="com",
      "ou = Peopl  e , dc = example , dc = com  "
   );
   Check_3
   (  "ou = Peopl  e , dc = example , dc = com  ",
      39,
   --  12345678901234567890123456789012345678901234567890
   --           1         2         3         4
      "ou"="Peopl  e" and "dc"="example" and "dc"="com"
   );
end Test_DN;
