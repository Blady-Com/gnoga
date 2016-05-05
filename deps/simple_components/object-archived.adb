--                                                                    --
--  package Object.Archived         Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2003       --
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

with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Object.Archived.Sets;  use Object.Archived.Sets;
with System;                use System;

package body Object.Archived is
   use Dispatch_Tables;

   package Link_Handles is
      new Object.Handle (Backward_Link, Backward_Link_Ptr);
   use Link_Handles;

--   procedure Dump_List (Object : Deposit'Class) is
--      use Ada.Text_IO, Ada.Tags, System.Storage_Elements;
--      function "+" (Value : Address) return Integer_Address
--         renames To_Integer;
--      function Status (Link : Boolean) return String is
--      begin
--         if Link then
--            return "";
--         else
--            return "broken";
--         end if;
--      end Status;
--      This : Backward_Link_Ptr;
--   begin
--      Put_Line
--      (  "Object: "
--      &  Expanded_Name (Object'Tag)
--      &  " at"
--      &  Integer_Address'Image (+Object'Address)
--      );
--      if Object.Delivery = null then
--         Put_Line ("        no backward links");
--      else
--         This := Object.Delivery;
--         loop
--            Put_Line
--            (  "        "
--            &  Expanded_Name (This.all'Tag)
--            &  Integer_Address'Image (+This.Prev.all'Address)
--            &  " <-"
--            &  Status (This.Prev.Next = This)
--            &  Integer_Address'Image (+This.all'Address)
--            &  " ->"
--            &  Status (This.Next.Prev = This)
--            &  Integer_Address'Image (+This.Next.all'Address)
--            );
--            This := This.Next;
--            exit when This = Object.Delivery;
--         end loop;
--      end if;
--   end Dump_List;

   procedure Attach
             (  Link   : Backward_Link_Ptr;
                Object : Deposit_Ptr
             )  is
   begin
      Detach (Link.all);
      if Object /= null then
         Link.Object := Object;
         if Object.Delivery = null then
            Object.Delivery := Link;
            Link.Next := Link;
            Link.Prev := Link;
         else
            Link.Next      := Object.Delivery; -- This -> List head
            Link.Prev      := Link.Next.Prev;  -- List tail <- This
            Link.Prev.Next := Link;
            Link.Next.Prev := Link;
         end if;
      end if;
   end Attach;

   procedure Close (Object : in out Deposit'Class) is
      This : Backward_Link_Ptr := Object.Delivery;
      Lock : Link_Handles.Handle;
   begin
      while This /= null loop
         Set (Lock, This);
         Destroyed (This.all);
         Detach (This.all);
         This := Object.Delivery;
      end loop;
   end Close;

   procedure Create
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Object  : out Deposit_Ptr
             )  is
      Constructor : Restore;
   begin
      begin
         Constructor := Find (Dispatch_Table, Class);
      exception
         when End_Error =>
            raise Name_Error;
      end;
      Constructor (Source, Pointer, Class, List, Object);
   end Create;

   procedure Delete (Object : in out Deposit'Class) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Backward_Link'Class,
                Backward_Link_Ptr
             );
   begin
      if Object.Delivery /= null then
         declare
            Set    : Deposit_Set;
            Handle : Link_Handles.Handle;
            This   : Backward_Link_Ptr := new Walker_Stub;
            Next   : Backward_Link_Ptr;
         begin
            --
            -- The  list  of backward links can be modified while we are
            -- walking  down it. A stub backward link object is inserted
            -- in the list. The object is pointed by This. This.Prev  is
            -- notified. Then This is moved forward in the list until it
            -- becomes next the list head. This prevents list corruption
            -- when some list elements are removed from or inserted into
            -- the list upon notification.
            --
            This.Prev := Object.Delivery;
            This.Next := Object.Delivery.Next;
            This.Next.Prev := This;
            This.Prev.Next := This;
            This.Object := Object.Delivery.Object;
            loop
               Link_Handles.Set (Handle, This.Prev);
               Deleted (Ptr (Handle).all, Set);
               Next := This.Next;
               exit when Next = Object.Delivery;
               --
               -- Moving This forward in the list
               --  .______.   .______.   .______.   .______.
               --  |This. |-->|      |-->|      |-->|Next. |
               --  |  Prev|   | This |   | Next |   |  Next|
               --  |______|<--|______|<--|______|<--|______|
               --
               --  .______.   .______.   .______.   .______.
               --  |      |-->|      |-->|      |-->|      |
               --  |      |   | Next |   | This |   |      |
               --  |______|<--|______|<--|______|<--|______|
               --
               Next.Prev := This.Prev;
               This.Prev.Next := Next;

               This.Prev := Next;
               This.Next := Next.Next;

               Next.Next.Prev := This;
               Next.Next := This;
            end loop;
            Free (This);
         end;
      end if;
   end Delete;

   procedure Deleted
             (  Link  : in out Walker_Stub;
                Temps : in out Deposit_Container'Class
             )  is
   begin
      null;
   end Deleted;

   procedure Destroyed (Link : in out Walker_Stub) is
   begin
      null;
   end Destroyed;

   procedure Detach (Link : in out Backward_Link)  is
   begin
      if Link.Object /= null then
         if Link.Next = Link.Next.Next then
            Link.Object.Delivery := null;           -- The only item
         else
            if Link.Next.Prev = Link.Object.Delivery then
               Link.Object.Delivery := Link.Next;   -- The first item
            end if;
            Link.Next.Prev := Link.Prev;
            Link.Prev.Next := Link.Next;
         end if;
         Link.Object := null;
      end if;
   end Detach;

   procedure Finalize (Object : in out Deposit) is
   begin
      Close (Object);
      Finalize (Entity (Object));
   end Finalize;

   procedure Finalize (Link : in out Backward_Link) is
   begin
      Detach (Link);
      Finalize (Entity (Link));
   end Finalize;

   procedure Get_Referents
             (  Object    : Deposit;
                Container : in out Deposit_Container'Class
             )  is
   begin
      null;
   end Get_Referents;

   function Is_Empty (Container : Deposit_Container'Class)
      return Boolean is
   begin
      return Get_Size (Container) = 0;
   end Is_Empty;

   function Is_Registered (Class : String) return Boolean is
   begin
      return IsIn (Dispatch_Table, Class);
   end Is_Registered;

   procedure Register_Class
             (  Class       : String;
                Constructor : Restore
             )  is
   begin
      Add (Dispatch_Table, Class, Constructor);
   exception
      when Name_Error =>
         if Find (Dispatch_Table, Class) /= Constructor then
            raise;
         end if;
   end Register_Class;

   function Self (Link : Backward_Link) return Backward_Link_Ptr is
   begin
      if Link.Object = null then
         raise Constraint_Error;
      else
         return Link.Next.Prev;
      end if;
   end Self;

   function This (Link : Backward_Link) return Deposit_Ptr is
   begin
      if Link.Object = null then
         raise Constraint_Error;
      else
         return Link.Object;
      end if;
   end This;

   function To_Object_Ptr (Link : Backward_Link_Ptr)
      return Backward_Link_Ptr is
   begin
      return Link;
   end To_Object_Ptr;

   function "<" (Left, Right : Backward_Link_Ptr) return Boolean is
   begin
      return
      (  Right /= null
      and then
         (  Left = null
         or else
            Left.all'Address < Right.all'Address
      )  );
   end "<";

end Object.Archived;
