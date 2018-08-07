--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess                Luebeck            --
--  Implementation                                 Summer, 2018       --
--                                                                    --
--                                Last revision :  01:09 01 May 2018  --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.IO_Exceptions;    use Ada.IO_Exceptions;
with Ada.Tags;             use Ada.Tags;
with Strings_Edit.Quoted;  use Strings_Edit.Quoted;
with System_Errno;         use System_Errno;

with System.Address_To_Access_Conversions;

package body Synchronization.Interprocess is

   package Conversions is
      new System.Address_To_Access_Conversions (Head);

   package body Generic_Memory_Mapper is
      package Conversions is
         new System.Address_To_Access_Conversions (Object_Type);

      function Map
               (  Location : System.Address;
                  Owner    : Boolean
               )  return Object_Type_Ptr is
      begin
         if Owner then
            declare
               Mapping : Memory_Mapper;
               type Type_Ptr is access all Object_Type;
               for Type_Ptr'Storage_Pool use Mapping;
               Ptr : Type_Ptr;
            begin
               Mapping.Location := Location;
               Ptr := new Object_Type;
               return Ptr.all'Unchecked_Access;
            end;
         else
            declare
               use Conversions;
            begin
               return To_Pointer (Location).all'Unchecked_Access;
            end;
         end if;
      end Map;
   end Generic_Memory_Mapper;

   procedure Allocate
             (  Pool            : in out Memory_Mapper;
                Storage_Address : out Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             )  is
   begin
      Storage_Address := Pool.Location;
   end Allocate;

   procedure Clean_Up
             (  Shared : in out Abstract_Shared_Environment'Class
             )  is
      Result : int;
      Owner  : Boolean;
   begin
      if Shared.Map /= null then
         Owner := Sub_And_Fetch (Shared.Map.Open_Count'Access) = 0;
         declare
            This : Abstract_Shared_Object_Ptr := Shared.First;
            Prev : Abstract_Shared_Object_Ptr;
            Next : Abstract_Shared_Object_Ptr;
         begin
            while This /= null loop
               Next := This.Next;
               This.Next := Prev;
               Prev := This;
               This := Next;
            end loop;
            This := Prev;
            while This /= null loop
               Prev := This.Next;
               Unmap (This.all, Shared, Owner);
               This := Prev;
            end loop;
         end;
         Result := munmap
                   (  Shared.Map.all'Address,
                      size_t (Shared.Map.Size)
                   );
         Shared.Map := null;
      end if;
      if Shared.File /= -1 then
         if Shared.Owner then
            Result := unlink (Shared.Name);
            Shared.Owner := False;
         end if;
         Result := close (Shared.File);
         Shared.File := -1;
      end if;
      Free (Shared.Name);
   end Clean_Up;

   procedure Close (Shared : in out Abstract_Shared_Environment) is
      Result : int;
   begin
      if Shared.Map = null then
         Raise_Exception
         (  Status_Error'Identity,
            "Shared environment is not open"
         );
      end if;
      Result := unlink (Shared.Name);
   end Close;

   procedure Deallocate
             (  Pool            : in out Memory_Mapper;
                Storage_Address : Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             )  is
   begin
      raise Program_Error;
   end Deallocate;

  procedure Start
             (  Shared : in out Abstract_Shared_Environment'Class;
                Stream : Walker
             )  is
      This     : Abstract_Shared_Object_Ptr := Stream.First;
      Location : Address :=
                    (  Shared.Map.all'Address
                    +  Round (Head'Max_Size_In_Storage_Elements)
                    );
   begin
      while This /= null loop
         Map
         (  Object   => This.all,
            Shared   => Shared,
            Location => Location,
            Size     => This.Shared_Size,
            Owner    => Shared.Owner
         );
         Location := Location + This.Shared_Size;
         This     := This.Next;
      end loop;
      This := Stream.First;
      while This /= null loop
         Start
         (  Object => This.all,
            Shared => Shared,
            Owner  => Shared.Owner
         );
         This := This.Next;
      end loop;
   end Start;

   procedure Do_Create
             (  Shared : in out Abstract_Shared_Environment;
                Stream : Walker
             )  is
   begin
      Shared.Owner := True;
      if 0 /= ftruncate (Shared.File, off_t (Stream.Size)) then
         Raise_From_Errno
         (  Data_Error'Identity,
            (  "File name "
            &  Quote (Value (Shared.Name))
            &  " resize fault: "
         )  );
      end if;
      declare
         Location : constant Address :=
                             mmap
                             (  Length => size_t (Stream.Size),
                                FD     => Shared.File
                             );
      begin
         if Location = MAP_FAILED then
            Raise_From_Errno
            (  Data_Error'Identity,
               (  "Mapping "
               &  Quote (Value (Shared.Name))
               &  " view fault: "
            )  );
         end if;
         Shared.Map :=
            Conversions.To_Pointer (Location).all'Unchecked_Access;
         Shared.Map.Checksum    := Stream.Sum_1 * 2**16 + Stream.Sum_2;
         Shared.Map.Size        := Stream.Size;
         Shared.Map.Initialized := False;
         Shared.Map.Open_Count  := 1;
      end;
      Shared.Map.Owner := getpid;
      Start (Shared, Stream);
      Shared.Map.Initialized := True;
   exception
      when others =>
         Clean_Up (Shared);
         raise;
   end Do_Create;

   procedure Do_Open
             (  Shared : in out Abstract_Shared_Environment'Class;
                Stream : Walker
             )  is
      Open_Count : Unsigned_32;
   begin
      Shared.Owner := False;
      declare
         Location : constant Address :=
                             mmap
                             (  Length => size_t (Stream.Size),
                                FD     => Shared.File
                             );
      begin
         if Location = MAP_FAILED then
            Raise_From_Errno
            (  Data_Error'Identity,
               (  "Mapping "
               &  Quote (Value (Shared.Name))
               &  " view fault: "
            )  );
         end if;
         Shared.Map :=
            Conversions.To_Pointer (Location).all'Unchecked_Access;
      end;
      while not Shared.Map.Initialized loop
         delay 0.1;
      end loop;
      Open_Count := Add_And_Fetch (Shared.Map.Open_Count'Access);
      if Shared.Map.Size /= Stream.Size then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Invalid memory map size"
            &  Storage_Count'Image (Shared.Map.Size)
            &  ", expected"
            &  Storage_Count'Image (Stream.Size)
         )  );
      elsif (  Shared.Map.Checksum
            /= Stream.Sum_1 * 2**16 + Stream.Sum_2
            )  then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Invalid memory map checksum"
            &  Unsigned_32'Image (Stream.Sum_1 * 2**16 + Stream.Sum_2)
            &  ", expected"
            &  Unsigned_32'Image (Shared.Map.Checksum)
         )  );
      end if;
      Start (Shared, Stream);
   exception
      when others =>
         Clean_Up (Shared);
         raise;
   end Do_Open;

   procedure Create
             (  Shared : in out Abstract_Shared_Environment;
                Name   : String
             )  is
      Stream : aliased Walker;
   begin
      if Shared.Map /= null then
         Raise_Exception
         (  Status_Error'Identity,
            "Shared environment is already open"
         );
      end if;
      Stream.Size := Round (Head'Max_Size_In_Storage_Elements);
      Abstract_Shared_Environment'Class'Write
      (  Stream'Access,
         Abstract_Shared_Environment'Class (Shared)
      );
      Shared.First := Stream.First;
      Shared.Name  := New_String (Shared_Directory & Name);
      if unlink (Shared.Name) = -1 and then errno /= ENOENT then
         Raise_From_Errno
         (  Data_Error'Identity,
            (  "Unlinking file mapping "
            &  Quote (Value (Shared.Name))
            &  " fault: "
         )  );
      end if;
      Shared.File := open (Shared.Name, O_RDWR + O_CREAT + O_EXCL);
      if Shared.File = -1 then
         case errno is
            when EEXIST =>
               Raise_Exception
               (  Use_Error'Identity,
                  (  "File "
                  &  Quote (Value (Shared.Name))
                  &  " alreay exists"
               )  );
            when EINVAL | ENAMETOOLONG | ENODEV | ENOENT | ENOTDIR |
                 ELOOP  | EFAULT =>
               Raise_From_Errno
               (  Name_Error'Identity,
                  (  "File name "
                  &  Quote (Value (Shared.Name))
                  &  " is invalid: "
               )  );
            when others =>
               Raise_From_Errno
               (  Data_Error'Identity,
                  (  "File "
                  &  Quote (Value (Shared.Name))
                  &  " creation fault: "
               )  );
         end case;
      end if;
      Do_Create (Shared, Stream);
   end Create;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Object : Abstract_Shared_Object
             )  is
      Self : Walker'Class renames Walker'Class (Stream.all);
      This : Abstract_Shared_Object'Class renames Object.Self.all;
   begin
      This.Offset      := Self.Size;
      This.Shared_Size := Round (Get_Size (This));
      Self.Size        := Self.Size + This.Shared_Size;
      if Self.First = null then
         Self.First := This.Self;
         Self.Last  := This.Self;
      else
         Self.Last.Next := This.Self;
         Self.Last      := This.Self;
      end if;
      declare
         Sum_1    : Unsigned_32 renames Self.Sum_1;
         Sum_2    : Unsigned_32 renames Self.Sum_2;
         Position : Unsigned_32 renames Self.Position;
      begin
         Sum_1 := (  Sum_1
                  +  Unsigned_32 (Get_Signature (This))
                  *  Position
                  );
         Sum_2 := (Sum_2 + Sum_1) mod 16#FFFF#;
         Sum_1 := Sum_1 mod 16#FFFF#;
         Position := Position + 1;
      end;
   end Enumerate;

   procedure Finalize (Shared : in out Abstract_Shared_Environment) is
   begin
      Clean_Up (Shared);
   end Finalize;

   function Get_Offset
            (  Object : Abstract_Shared_Object
            )  return Storage_Offset is
   begin
      return Object.Offset;
   end Get_Offset;

   function Get_Signature (Data : String) return Unsigned_16 is
      Sum_1 : Unsigned_16 := 16#FF#;
      Sum_2 : Unsigned_16 := 16#FF#;
   begin
      for Major in 1..Data'Length / 21 loop
         for Index in Major * 21 - 20..Major * 21 loop
            Sum_1 := Sum_1 + Unsigned_16 (Character'Pos (Data (Index)));
            Sum_2 := Sum_2 + Sum_1;
         end loop;
         Sum_1 := (Sum_1 and 16#FF#) + Sum_1 / 2**8;
         Sum_2 := (Sum_2 and 16#FF#) + Sum_2 / 2**8;
      end loop;
      Sum_1 := (Sum_1 and 16#FF#) + Sum_1 / 2**8;
      Sum_2 := (Sum_2 and 16#FF#) + Sum_2 / 2**8;
      return (Sum_1 and 16#FF#) + (Sum_2 and 16#FF#) * 2**16;
   end Get_Signature;

   function Get_Signature
            (  Object : Abstract_Shared_Object
            )  return Unsigned_16 is
   begin
      return
      (  Get_Signature
         (  External_Tag
            (  Abstract_Shared_Object'Class (Object)'Tag
      )  )  );
   end Get_Signature;

   function Get_Size
            (  Shared : Abstract_Shared_Environment
            )  return Storage_Count is
   begin
      if Shared.Map = null then
         Raise_Exception
         (  Status_Error'Identity,
            "Shared environment is not open"
         );
      end if;
      return Shared.Map.Size;
   end Get_Size;

   procedure Initialize (Shared : in out Abstract_Shared_Environment) is
   begin
      null;
   end Initialize;

   procedure Open
             (  Shared : in out Abstract_Shared_Environment;
                Name   : String;
                Create : Boolean := False
             )  is
      Stream : aliased Walker;
   begin
      if Shared.Map /= null then
         Raise_Exception
         (  Status_Error'Identity,
            "Shared environment is already open"
         );
      end if;
      Stream.Size := Round (Head'Max_Size_In_Storage_Elements);
      Abstract_Shared_Environment'Class'Write
      (  Stream'Access,
         Abstract_Shared_Environment'Class (Shared)
      );
      Shared.First := Stream.First;
      Shared.Name  := New_String (Shared_Directory & Name);
      loop
         Shared.File := open (Shared.Name, O_RDWR);
         if Shared.File = -1 then
            case errno is
               when EINVAL | ENAMETOOLONG | ENODEV | ENOTDIR |
                    ELOOP  | EFAULT =>
                  Raise_From_Errno
                  (  Name_Error'Identity,
                     (  "File name "
                     &  Quote (Value (Shared.Name))
                     &  " is invalid: "
                  )  );
               when ENOENT  =>
                  if not Create then
                     Raise_Exception
                     (  Name_Error'Identity,
                        (  "File "
                        &  Quote (Value (Shared.Name))
                        &  " does not exist"
                     )  );
                  end if;
               when others =>
                  Raise_From_Errno
                  (  Data_Error'Identity,
                     (  "File "
                     &  Quote (Value (Shared.Name))
                     &  " open fault: "
                  )  );
            end case;
         else
            Do_Open (Shared, Stream);
            exit;
         end if;
         Shared.File := open (Shared.Name, O_RDWR + O_CREAT + O_EXCL);
         if Shared.File = -1 then
            case errno is
               when EEXIST => -- Exists, try to open it instead
                  null;
               when EINVAL | ENAMETOOLONG | ENODEV | ENOENT | ENOTDIR |
                    ELOOP  | EFAULT =>
                  Raise_From_Errno
                  (  Name_Error'Identity,
                     (  "File name "
                     &  Quote (Value (Shared.Name))
                     &  " is invalid: "
                  )  );
               when others =>
                  Raise_From_Errno
                  (  Data_Error'Identity,
                     (  "File "
                     &  Quote (Value (Shared.Name))
                     &  " creation fault: "
                  )  );
            end case;
         else
            Do_Create (Shared, Stream);
            exit;
         end if;
      end loop;
   end Open;

   procedure Read
             (  Stream : in out Walker;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      null;
   end Read;

   function Round (Offset : Storage_Count) return Storage_Count is
   begin
      return ((Offset + 7) / 8) * 8;
   end;

   procedure Start
             (  Object : in out Abstract_Shared_Object;
                Shared : in out Abstract_Shared_Environment'Class;
                Owner  : Boolean
             )  is
   begin
      null;
   end Start;

   function Storage_Size (Pool : Memory_Mapper) return Storage_Count is
   begin
      return Storage_Count'Last;
   end Storage_Size;

   procedure Unmap
             (  Object : in out Abstract_Shared_Object;
                Shared : in out Abstract_Shared_Environment'Class;
                Owner  : Boolean
             )  is
   begin
      null;
   end Unmap;

   procedure Write
             (  Stream      : access Root_Stream_Type'Class;
                Environment : Abstract_Shared_Environment
             )  is
   begin
      null;
   end Write;

   procedure Write
             (  Stream : in out Walker;
                Item   : in Stream_Element_Array
             )  is
   begin
      null;
   end Write;

end Synchronization.Interprocess;
