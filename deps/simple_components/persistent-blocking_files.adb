--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Blocking_Files                   Luebeck            --
--  Implementation                                 Winter, 2014       --
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

with Ada.Unchecked_Deallocation;

package body Persistent.Blocking_Files is

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Blocks_Array,
             Blocks_Array_Ptr
          );

   procedure Close (Container : in out Persistent_Array) is
   begin
      if Container.Is_Open then
         if Mode (Container.File) /= In_File then
            Flush (Container);
         end if;
         declare
            Buffer : Blocks_Array renames Container.Buffer.all;
         begin
            for Index in Buffer'Range loop
               Buffer (Index).Used := False;
            end loop;
         end;
         Close (Container.File);
         Container.Is_Open := False;
         Container.Size    := 0;
      end if;
   end Close;

   procedure Commit (Container : in out Persistent_Array) is
   begin
      null;
   end Commit;

   function Compose
            (  Block  : Block_Index;
               Offset : Block_Offset
            )  return Byte_Index is
   begin
      return
      (  (Byte_Index (Block) - 1) * 2 ** Byte_Offset_Bits
      +  Byte_Index (Offset)
      );
   end Compose;

   procedure Finalize (Container : in out Persistent_Array) is
   begin
      Close (Container);
      Free (Container.Buffer);
   end Finalize;

   procedure Flush (Container : in out Persistent_Array) is
   begin
      if (  Container.Is_Open
         and then
            Container.Buffer /= null
         and then
            Mode (Container.File) /= In_File
         )
      then
         declare
            Buffer : Blocks_Array renames Container.Buffer.all;
         begin
            for Index in Buffer'Range loop
               declare
                  Block : Cashed_Block renames Buffer (Index);
               begin
                  if Block.Used and then Block.Updated then
                     Write (Container.File, Block.Data, Block.Index);
                     Block.Updated := False;
                  end if;
               end;
            end loop;
         end;
      end if;
   end Flush;

   function Get
            (  Container : access Persistent_Array;
               Index     : Byte_Index
            )  return Block_Type_Ptr is
   begin
      if not Container.Is_Open then
         Raise_Exception (Use_Error'Identity, "No file open");
      elsif Mode (Container.File) = In_File then
         Raise_Exception (Use_Error'Identity, "File is read-only");
      end if;
      declare
         Item : Count := Count (Index / Block_Byte_Size);
         This : Cashed_Block renames
                   Container.Buffer
                   (  Count (Item mod Container.Buffer'Length)
                   );
      begin
         Item := Item + 1;
         if This.Used then
            if This.Index = Item then
               This.Updated := True;
               return This.Data'Unchecked_Access;
            end if;
            if This.Updated then
               Write (Container.File, This.Data, This.Index);
            end if;
         end if;
         if Item <= Container.Size then
            Read (Container.File, This.Data, Item);
         else
            Set_Index (Container.File, Container.Size + 1);
            loop
               Write (Container.File, This.Data);
               Container.Size := Container.Size + 1;
               exit when Item <= Container.Size;
            end loop;
         end if;
         This.Index   := Item;
         This.Updated := True;
         This.Used    := True;
         return This.Data'Unchecked_Access;
      end;
   end Get;

   function Get_Block_Size
            (  Container : Persistent_Array
            )  return Block_Count is
   begin
      if Container.Is_Open then
         return Block_Count (Container.Size);
      else
         Raise_Exception (Use_Error'Identity, "No file open");
      end if;
   end Get_Block_Size;

   function Get_First (Index : Byte_Index) return Byte_Index is
   begin
      return Index and not (2 ** Byte_Offset_Bits - 1);
   end Get_First;

   function Get_Index (Index : Byte_Index) return Block_Index is
   begin
      return Block_Index (Index / Block_Byte_Size + 1);
   end Get_Index;

   function Get_Name (Container : Persistent_Array) return String is
   begin
      if Container.Is_Open then
         return Name (Container.File);
      else
         Raise_Exception (Use_Error'Identity, "No file open");
      end if;
   end Get_Name;

   function Get_Offset (Index : Byte_Index) return Block_Offset is
   begin
      return Block_Offset (Index mod Block_Byte_Size);
   end Get_Offset;

   function Get_Size (Container : Persistent_Array) return Byte_Index is
   begin
      if Container.Is_Open then
         return Byte_Index (Container.Size) * Block_Byte_Size;
      else
         Raise_Exception (Use_Error'Identity, "No file open");
      end if;
   end Get_Size;

   function Is_Open (Container : Persistent_Array) return Boolean is
   begin
      return Container.Is_Open;
   end Is_Open;

   function Is_Resident
            (  Container : Persistent_Array;
               Index     : Block_Index
            )  return Boolean is
   begin
      if not Container.Is_Open then
         return False;
      end if;
      declare
         Item : constant Count := Count (Index);
         This : Cashed_Block renames
                   Container.Buffer
                   (  Count ((Index - 1) mod Container.Buffer'Length)
                   );
      begin
         return This.Used and then This.Index = Item;
      end;
   end Is_Resident;

   function Is_Resident
            (  Container : Persistent_Array;
               Index     : Byte_Index
            )  return Boolean is
   begin
      return Is_Resident (Container, Get_Index (Index));
   end Is_Resident;

   function Is_Writable (Container : Persistent_Array) return Boolean is
   begin
      return Container.Is_Open and then
             Mode (Container.File) /= In_File;
   end Is_Writable;

   procedure Load
             (  Container : in out Persistent_Array;
                No        : Count;
                Block     : out Cashed_Block_Ptr
             )  is
   begin
      if not Container.Is_Open then
         Raise_Exception (Use_Error'Identity, "No file open");
      elsif No >= Container.Size then
         Block := null;
      else
         Block := Container.Buffer
                  (  No mod Container.Buffer'Length
                  ) 'Unchecked_Access;
         declare
            This : Cashed_Block renames Block.all;
            Item : constant Count := No + 1;
         begin
            if This.Used then
               if This.Index = Item then
                  return;
               end if;
               if This.Updated then
                  Write (Container.File, This.Data, This.Index);
               end if;
            end if;
            Read (Container.File, This.Data, Item);
            This.Index   := Item;
            This.Updated := False;
            This.Used    := True;
         end;
      end if;
   end Load;

   procedure Load
             (  Container : in out Persistent_Array;
                Index     : Byte_Index;
                Block     : out Cashed_Block_Ptr
             )  is
   begin
      Load (Container, Count (Index / Block_Byte_Size), Block);
   end Load;

   function Load
            (  Container : access Persistent_Array;
               Index     : Byte_Index
            )  return Block_Type_Ref is
      This : Cashed_Block_Ptr;
   begin
      Load (Container.all, Count (Index / Block_Byte_Size), This);
      if This = null then
         return null;
      else
         return This.Data'Unchecked_Access;
      end if;
   end Load;

   procedure Open
             (  Container : in out Persistent_Array;
                Hash_Size : Positive := 256;
                Form      : String   := ""
             )  is
   begin
      Open (Container, "", Create_Mode, Hash_Size, Form);
   end Open;

   procedure Open
             (  Container : in out Persistent_Array;
                Name      : String;
                Mode      : Access_Mode := Read_Mode;
                Hash_Size : Positive    := 256;
                Form      : String      := ""
             )  is
   begin
      Close (Container);
      case Mode is
         when Read_Mode =>
           Open (Container.File, In_File, Name, Form);
         when Read_Write_Mode =>
           Open (Container.File, Inout_File, Name, Form);
         when Create_Mode =>
           Create (Container.File, Inout_File, Name, Form);
      end case;
      begin
         if Container.Buffer = null then
            Container.Buffer :=
               new Blocks_Array (0..Count (Hash_Size) - 1);
         elsif Hash_Size > Container.Buffer'Length then
            Free (Container.Buffer);
            Container.Buffer :=
               new Blocks_Array (0..Count (Hash_Size) - 1);
         end if;
      exception
         when others =>
            begin
               if Mode = Create_Mode then
                  Delete (Container.File);
               else
                  Close (Container.File);
               end if;
            exception
               when others =>
                  null;
            end;
            raise;
      end;
      Container.Is_Open := True;
      Container.Size    := Size (Container.File);
   end Open;

   procedure Read
             (  Container : in out Persistent_Array;
                Index     : Byte_Index;
                Block     : out Block_Type
             )  is
   begin
      if not Container.Is_Open then
         Raise_Exception (Use_Error'Identity, "No file open");
      end if;
      declare
         Item : Count := Count (Index / Block_Byte_Size);
      begin
         Item := Item + 1;
         if Item > Container.Size then
            Raise_Exception (End_Error'Identity, "Out of file");
         end if;
         declare
            This : Cashed_Block renames
                      Container.Buffer
                      (  Item mod Container.Buffer'Length
                      );
         begin
            if This.Used and then This.Index = Item then
               Block        := This.Data;
               This.Used    := False;
               This.Updated := False;
            else
               Read (Container.File, Block, Item);
            end if;
         end;
      end;
   end Read;

   procedure Rollback (Container : in out Persistent_Array) is
   begin
      null;
   end Rollback;

   function Update
            (  Container : access Persistent_Array;
               Index     : Byte_Index
            )  return Block_Type_Ptr is
      This : Cashed_Block_Ptr;
   begin
      Load (Container.all, Count (Index / Block_Byte_Size), This);
      if Mode (Container.File) = In_File then
         Raise_Exception (Use_Error'Identity, "File is read-only");
      elsif This = null then
         return null;
      else
         This.Updated := True;
         return This.Data'Unchecked_Access;
      end if;
   end Update;

   procedure Write
             (  Container : in out Persistent_Array;
                Index     : Byte_Index;
                Block     : Block_Type
             )  is
   begin
      if not Container.Is_Open then
         Raise_Exception (Use_Error'Identity, "No file open");
      elsif Mode (Container.File) = In_File then
         Raise_Exception (Use_Error'Identity, "File is read-only");
      end if;
      declare
         Item : Count := Count (Index / Block_Byte_Size);
         This : Cashed_Block renames
                   Container.Buffer
                   (  Count (Item mod Container.Buffer'Length)
                   );
      begin
         Item := Item + 1;
         if This.Used and then This.Index = Item then
            This.Used    := False;
            This.Updated := False;
         end if;
         if Item <= Container.Size then
            Write (Container.File, Block, Item);
         else
            Set_Index (Container.File, Container.Size + 1);
            loop
               Write (Container.File, This.Data);
               Container.Size := Container.Size + 1;
               exit when Item <= Container.Size;
            end loop;
         end if;
      end;
   end Write;

end Persistent.Blocking_Files;
