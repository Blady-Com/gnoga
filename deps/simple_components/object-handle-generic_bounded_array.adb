--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Object.Handle.Bounded_Array                 Luebeck            --
--  Implementation                                 Spring, 2003       --
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

with System;  use System;

package body Object.Handle.Generic_Bounded_Array is
--
-- Reset -- Decrement references in an array
--
   procedure Reset (Vector : in out Object_Ptr_Array_Type) is
      pragma Inline (Reset);
   begin
      for Index in Vector'Range loop
         Release (Vector (Index));
         Vector (Index) := null;
      end loop;
   end Reset;
--
-- Inc -- Increment references in an array
--
   procedure Inc (Vector : in out Object_Ptr_Array_Type) is
      pragma Inline (Inc);
      Item : Object_Type_Ptr;
   begin
      for Index in Vector'Range loop
         Item := Vector (Index);
         if Item /= null then
            Increment_Count (Item.all);
         end if;
      end loop;
   end Inc;
--
-- Copy -- References from a source to an empty target
--
   procedure Copy
             (  Target : in out Object_Ptr_Array_Type;
                Index  : Index_Type;
                Source : Object_Ptr_Array_Type
             )  is
      pragma Inline (Copy);
      To : constant Index_Type :=
              Index_Type'Val
              (  Index_Type'Pos (Index)
              +  (Source'Length - 1)
              );
   begin
      Target (Index..To) := Source;
      Inc (Target (Index..To));
   end Copy;
--
-- Replace -- References in the target by ones from the source
--
   procedure Replace
             (  Target : in out Object_Ptr_Array_Type;
                Index  : Index_Type;
                Source : Object_Ptr_Array_Type
             )  is
   begin
      if Target'Address <= Source'Address then
         for From in Source'Range loop
            declare
               Item : Object_Type_Ptr renames
                         Target
                         (  Index_Type'Val
                            (  Index_Type'Pos (Index)
                            +  (  Index_Type'Pos (From)
                               -  Index_Type'Pos (Source'First)
                         )  )  );
            begin
               if Source (From) = null then
                  Release (Item);
                  Item := null;
               elsif Item = null then
                  Item := Source (From);
                  Increment_Count (Item.all);
               elsif not Equal (Item.all, Source (From).all) then
                  Release (Item);
                  Item := Source (From);
                  Increment_Count (Item.all);
               end if;
            end;
         end loop;
      else
         for From in reverse Source'Range loop
            declare
               Item : Object_Type_Ptr renames
                         Target
                         (  Index_Type'Val
                            (  Index_Type'Pos (Index)
                            +  (  Index_Type'Pos (From)
                               -  Index_Type'Pos (Source'First)
                         )  )  );
            begin
               if Source (From) = null then
                  Release (Item);
                  Item := null;
               elsif Item = null then
                  Item := Source (From);
                  Increment_Count (Item.all);
               elsif not Equal (Item.all, Source (From).all) then
                  Release (Item);
                  Item := Source (From);
                  Increment_Count (Item.all);
               end if;
            end;
         end loop;
      end if;
   end Replace;

   procedure Adjust (Container : in out Bounded_Array) is
   begin
      Inc (Container.Vector);
   end Adjust;

   function Append
            (  Container : Bounded_Array;
               Element   : Object_Type_Ptr := null;
               Count     : Natural         := 1
            )  return Bounded_Array is
   begin
      if Count = 0 then
         return Container;
      else
         declare
            Last   : constant Index_Type :=
                        Index_Type'Val
                        (  Index_Type'Pos (Container.Last)
                        +  Count
                        );
            Result : Bounded_Array :=
                     (  (  Ada.Finalization.Controlled
                        with
                           Container.First,
                           Last,
                           (  Container.Vector
                           &  (  Index_Type'Succ (Container.Last)
                              .. Last =>
                                    Element
                     )  )  )  );
         begin
            Inc (Result.Vector);
            return Result;
         end;
      end if;
   end Append;

   function Append
            (  Container : Bounded_Array;
               Element   : Handle_Type;
               Count     : Natural := 1
            )  return Bounded_Array is
   begin
      return Append (Container, Ptr (Element), Count);
   end Append;

   function Delete
            (  Container : Bounded_Array;
               From      : Index_Type;
               Count     : Natural := 1
            )  return Bounded_Array is
   begin
      if From not in Container.First..Container.Last then
         raise Constraint_Error;
      elsif Count = 0 then
         return Container;
      elsif From = Container.First then
         --
         -- Removing a prefix
         --
         if Container.Vector'Length <= Count then
            return  -- Empty result
            (  (  Ada.Finalization.Controlled
               with
                  Index_Type'Succ (Index_Type'First),
                  Index_Type'First,
                  (others => null)
            )  );
         else
            declare
               Last : constant Index_Type :=
                         Index_Type'Val
                         (  Index_Type'Pos (Container.Last)
                         -  Count
                         );
               Result : Bounded_Array :=
                        (  (  Ada.Finalization.Controlled
                           with
                              Container.First,
                              Last,
                              Container.Vector
                              (  Index_Type'Val
                                    (Index_Type'Pos (From) + Count)
                              .. Container.Last
                        )  )  );
            begin
               Inc (Result.Vector);
               return Result;
            end;
         end if;
      else
         --
         -- Removing from the middle or a suffix
         --
         if (  (  Index_Type'Pos (Container.Last)
               -  Index_Type'Pos (From)
               +  1
               )
            <= Count
            )
         then -- Removing all suffix
            declare
               Last   : constant Index_Type := Index_Type'Pred (From);
               Result : Bounded_Array :=
                        (  (  Ada.Finalization.Controlled
                           with
                              Container.First,
                              Last,
                              Container.Vector (Container.First..Last)
                        )  );
            begin
               Inc (Result.Vector);
               return Result;
            end;
         else -- Removing a slice
            declare
               Last : constant Index_Type :=
                  Index_Type'Val
                  (  Index_Type'Pos (Container.Last)
                  -  Count
                  );
               Result : Bounded_Array :=
                        (  (  Ada.Finalization.Controlled
                           with
                              Container.First,
                              Last,
                              (  Container.Vector
                                 (  Container.First
                                 .. Index_Type'Pred (From)
                                 )
                              &  Container.Vector
                                 (  Index_Type'Val
                                    (  Index_Type'Pos (From)
                                    +  Count
                                    )
                                 .. Container.Last
                        )  )  )  );
            begin
               Inc (Result.Vector);
               return Result;
            end;
         end if;
      end if;
   end Delete;

   procedure Finalize (Container : in out Bounded_Array) is
   begin
      Reset (Container.Vector);
   end Finalize;

   procedure Fill
             (  Container : in out Bounded_Array;
                From      : Index_Type;
                To        : Index_Type;
                Element   : Object_Type_Ptr := null
             )  is
      subtype Index_Range is Index_Type range Container.Vector'Range;
   begin
      if From > To then
         return;
      else
         if Element = null then
            Reset (Container.Vector (From..To));
         else
            declare
               Object : Object_Type'Class renames Element.all;
            begin
               for Index in Index_Range (From)..Index_Range (To) loop
                  if (  Container.Vector (Index) = null
                     or else
                        not Equal
                            (  Container.Vector (Index).all,
                               Object
                     )      )
                  then
                     Release (Container.Vector (Index));
                     Container.Vector (Index) := Element;
                     Increment_Count (Object);
                  end if;
               end loop;
            end;
         end if;
      end if;
   end Fill;

   procedure Fill
             (  Container : in out Bounded_Array;
                From      : Index_Type;
                To        : Index_Type;
                Element   : Handle_Type
             )  is
   begin
      Fill (Container, From, To, Ptr (Element));
   end Fill;

   function Get
            (  Container : Bounded_Array;
               Index     : Index_Type
            )  return Object_Type_Ptr is
   begin
      if Index not in Container.Vector'Range then
         return null;
      else
         return Container.Vector (Index);
      end if;
   end Get;

   function Get
            (  Container : Bounded_Array;
               From      : Index_Type;
               To        : Index_Type
            )  return Bounded_Array is
   begin
      if From > To then
         declare
            Result : Bounded_Array (From, To);
         begin
            return Result;
         end;
      else
         if From < Container.First or else To > Container.Last then
            raise Constraint_Error;
         else
            declare
               Result : Bounded_Array (From, To);
            begin
               Result.Vector := Container.Vector (From..To);
               Inc (Result.Vector);
               return Result;
            end;
         end if;
      end if;
   end Get;

   function Prepend
            (  Container : Bounded_Array;
               Element   : Object_Type_Ptr := null;
               Count     : Natural         := 1
            )  return Bounded_Array is
   begin
      if Count = 0 then
         return Container;
      else
         declare
            First  : constant Index_Type :=
                        Index_Type'Val
                        (  Index_Type'Pos (Container.First)
                        -  Count
                        );
            Result : Bounded_Array :=
                     (  (  Ada.Finalization.Controlled
                        with
                           First,
                           Container.Last,
                           (  (  First
                              .. Index_Type'Pred (Container.First) =>
                                    Element
                              )
                           &  Container.Vector
                     )  )  );
         begin
            Inc (Result.Vector);
            return Result;
         end;
      end if;
   end Prepend;

   function Prepend
            (  Container : Bounded_Array;
               Element   : Handle_Type;
               Count     : Natural := 1
            )  return Bounded_Array is
   begin
      return Prepend (Container, Ptr (Element), Count);
   end Prepend;

   procedure Put
             (  Container : in out Bounded_Array;
                Index     : Index_Type;
                Element   : Object_Type_Ptr
             )  is
      Item : Object_Type_Ptr renames Container.Vector (Index);
   begin
      if Element = null then
         Release (Item);
         Item := null;
      else
         declare
            Object : Object_Type'Class renames Element.all;
         begin
            if Item /= null and then Equal (Item.all, Object) then
               return;
            end if;
            Release (Item);
            Item := Element;
            Increment_Count (Object);
         end;
      end if;
   end Put;

   procedure Put
             (  Container : in out Bounded_Array;
                Index     : Index_Type;
                Element   : Handle_Type
             )  is
   begin
      Put (Container, Index, Ptr (Element));
   end Put;

   procedure Put
             (  Container : in out Bounded_Array;
                From      : Index_Type;
                To        : Index_Type;
                Elements  : Bounded_Array
             )  is
   begin
      if From > To then
         return;
      elsif From < Container.First or else To > Container.Last then
         raise Constraint_Error;
      else
         if (  Elements.Vector'Length
            >= Index_Type'Pos (To) - Index_Type'Pos (From) + 1
            )
         then
            Replace
            (  Container.Vector,
               From,
               Elements.Vector
               (  Elements.Vector'First
               .. Index_Type'Val
                  (  Index_Type'Pos (Elements.Vector'First)
                  +  (  Index_Type'Pos (To)
                     -  Index_Type'Pos (From)
            )  )  )  );
         else
            Replace (Container.Vector, From, Elements.Vector);
            Reset
            (  Container.Vector
               (  Index_Type'Val
                  (  Index_Type'Pos (From)
                  +  (Elements.Vector'Length - 1)
                  )
               .. To
            )  );
         end if;
      end if;
   end Put;

   function Ref
            (  Container : Bounded_Array;
               Index     : Index_Type
            )  return Handle_Type is
      Ptr : constant Object_Type_Ptr := Get (Container, Index);
   begin
      if Ptr = null then
         raise Constraint_Error;
      else
         return Ref (Ptr);
      end if;
   end Ref;

   function "&" (Left, Right : Bounded_Array) return Bounded_Array is
   begin
      if Right.Last < Right.First then
         return Left;
      elsif Left.Last < Left.First then
         return Right;
      else
         declare
            Result : Bounded_Array
                     (  Index_Type'First,
                        Index_Type'Val
                        (  Index_Type'Pos (Index_Type'First)
                        +  (Left.Vector'Length)
                        +  (Right.Vector'Length)
                        -  1
                     )  );
         begin
            Copy (Result.Vector, Result.First, Left.Vector);
            Copy
            (  Result.Vector,
               Index_Type'Val
               (  Index_Type'Pos (Index_Type'First)
               +  (Left.Vector'Length)
               -  1
               ),
               Right.Vector
            );
            return Result;
         end;
      end if;
   end "&";

end Object.Handle.Generic_Bounded_Array;
