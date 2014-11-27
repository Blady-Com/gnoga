--                                                                    --
--  package Generic_Stack           Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2003       --
--                                                                    --
--                                Last revision :  11:37 13 Oct 2007  --
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

package body Generic_Stack is

   procedure Erase (Container : in out Stack) is
   begin
      while Container.Top > Index_Type'First loop
         Put (Container.Data, Container.Top, Null_Element);
         Container.Top := Index_Type'Pred (Container.Top);
      end loop;
   end Erase;

   function Get
            (  Container : Stack;
               Index     : Index_Type
            )  return Object_Type is
   begin
      if Index = Index_Type'First or else Index > Container.Top then
         raise Constraint_Error;
      end if;
      return Get (Container.Data, Index);
   end Get;

   function Is_Empty (Container : Stack) return Boolean is
   begin
      return Container.Top = Index_Type'First;
   end Is_Empty;

   function Mark (Container : Stack) return Index_Type is
   begin
      return Container.Top;
   end Mark;

   procedure Pop (Container : in out Stack; Count : Natural := 1) is
   begin
      for No in 1..Count loop
         exit when Container.Top = Index_Type'First;
         Put (Container.Data, Container.Top, Null_Element);
         Container.Top := Index_Type'Pred (Container.Top);
      end loop;
   end Pop;

   procedure Push (Container : in out Stack; Item : Object_Type) is
   begin
      Put (Container.Data, Index_Type'Succ (Container.Top), Item);
      Container.Top := Index_Type'Succ (Container.Top);
   end Push;

   procedure Put
             (  Container : in out Stack;
                Index     : Index_Type;
                Element   : Object_Type
             )  is
   begin
      if Index = Index_Type'First or else Index > Container.Top then
         raise Constraint_Error;
      end if;
      Put (Container.Data, Index, Element);
   end Put;

   procedure Release (Container : in out Stack; Mark : Index_Type) is
      Count : constant Integer :=
         Index_Type'Pos (Container.Top) - Index_Type'Pos (Mark);
   begin
      if Count > 0 then
         Pop (Container, Count);
      end if;
   end Release;

   function Top (Container : Stack) return Object_Type is
   begin
      return Get (Container.Data, Container.Top);
   end Top;

end Generic_Stack;
