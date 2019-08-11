--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     LDAP                                        Summer, 2019       --
--  Implementation                                                    --
--                                Last revision :  21:13 03 Aug 2019  --
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
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Ada.Tags;               use Ada.Tags;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values;
with ada.Text_IO;
package body GNAT.Sockets.Connection_State_Machine.LDAP is
   use GNAT.Sockets.Connection_State_Machine.ASN1.Booleans;
   use GNAT.Sockets.Connection_State_Machine.ASN1.Choices;
   use GNAT.Sockets.Connection_State_Machine.ASN1.Distinguished_Names;
   use GNAT.Sockets.Connection_State_Machine.ASN1.Integers_8;
   use GNAT.Sockets.Connection_State_Machine.ASN1.Integers_32;
   use GNAT.Sockets.Connection_State_Machine.ASN1.Nulls;
   use GNAT.Sockets.Connection_State_Machine.ASN1.Sequences;
   use GNAT.Sockets.Connection_State_Machine.ASN1.Sets;
   use GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
   use GNAT.Sockets.Server.Stream_Element_Offset_Edit;
   use Node_Handles;

   function Create_And (Children : Node_Array)
      return Node_Handles.Handle is
      Result : Node_Handles.Handle;
   begin
      Node_Handles.Set (Result, new And_Node (Children'Length));
      And_Node (Ptr (Result).all).Children := Children;
      return Result;
   end Create_And;

   function Create_Attribute_Data
            (  Description : String;
               List        : Values_Array
            )  return Attribute_Data_Handles.Handle is
      Result : Attribute_Data_Handles.Handle;
   begin
      Attribute_Data_Handles.Set
      (  Result,
         new Attribute_Data (Description'Length, List'Length)
      );
      declare
         This : Attribute_Data renames
                Attribute_Data
                (  Attribute_Data_Handles.Ptr (Result).all
                );
      begin
         This.Description := Description;
         This.List        := List;
      end;
      return Result;
   end Create_Attribute_Data;

   function Create_Comparison
            (  Description : String;
               Mode        : Comparison_Type;
               Value       : String
            )  return Node_Handles.Handle is
      Result : Node_Handles.Handle;
   begin
      Node_Handles.Set
      (  Result,
         new Comparison_Node (Mode, Description'Length, Value'Length)
      );
      declare
         This : Comparison_Node renames
                Comparison_Node (Ptr (Result).all);
      begin
         This.Description := Description;
         This.Value       := Value;
      end;
      return Result;
   end Create_Comparison;

   function Create_Not (Child : Node_Handles.Handle)
      return Node_Handles.Handle is
      Result : Node_Handles.Handle;
   begin
      Node_Handles.Set (Result, new Not_Node);
      Not_Node (Ptr (Result).all).Child := Child;
      return Result;
   end Create_Not;

   function Create_Or (Children : Node_Array)
      return Node_Handles.Handle is
      Result : Node_Handles.Handle;
   begin
      Node_Handles.Set (Result, new Or_Node (Children'Length));
      Or_Node (Ptr (Result).all).Children := Children;
      return Result;
   end Create_Or;

   function Create_Present (Description : String)
      return Node_Handles.Handle is
      Result : Node_Handles.Handle;
   begin
      Node_Handles.Set (Result, new Present_Node (Description'Length));
      Present_Node (Ptr (Result).all).Description := Description;
      return Result;
   end Create_Present;

   function Create_Rule
            (  Value       : String;
               Rule        : String;
               Description : String;
               Attributes  : Boolean
            )  return Node_Handles.Handle is
      Result : Node_Handles.Handle;
   begin
      Node_Handles.Set
      (  Result,
         new Rule_Node (Rule'Length, Description'Length, Value'Length)
      );
      declare
         This : Rule_Node renames Rule_Node (Ptr (Result).all);
      begin
         This.Description := Description;
         This.Rule        := Rule;
         This.Attributes  := Attributes;
         This.Value       := Value;
      end;
      return Result;
   end Create_Rule;

   function Create_Substring
            (  Component : Substring_Component_Type;
               Substring : String
            )  return Node_Handles.Handle is
      Result : Node_Handles.Handle;
   begin
      Node_Handles.Set
      (  Result,
         new Substring_Node (Component, Substring'Length)
      );
      Substring_Node (Ptr (Result).all).Substring := Substring;
      return Result;
   end Create_Substring;

   function Create_Substring_List
            (  Description : String;
               List        : Node_Array
            )  return Node_Handles.Handle is
      Result : Node_Handles.Handle;
   begin
      Node_Handles.Set
      (  Result,
         new Substring_List_Node (Description'Length, List'Length)
      );
      declare
         This : Substring_List_Node renames
                Substring_List_Node (Ptr (Result).all);
      begin
         This.Description := Description;
         This.Children    := List;
      end;
      return Result;
   end Create_Substring_List;

   function Create_Substring_Sequence (Children : Node_Array)
      return Node_Handles.Handle is
      Result : Node_Handles.Handle;
   begin
      Node_Handles.Set
      (  Result,
         new Substring_Sequence_Node (Children'Length)
      );
      Substring_Sequence_Node (Ptr (Result).all).Children := Children;
      return Result;
   end Create_Substring_Sequence;

   function "=" (Description, Value : String) return Search_Filter is
   begin
      return
      (  Reference => Create_Comparison (Description, Equal, Value)
      );
   end "=";

   function "="
            (  Description : String;
               Substrings  : Substring_Filter
            )  return Search_Filter is
   begin
      if Ptr (Substrings.Reference).all in Substring_Node then
         return
         (  Reference =>
               Create_Substring_List
               (  Description,
                  (1 => Substrings.Reference)
         )     );
      else
         return
         (  Reference =>
               Create_Substring_List
               (  Description,
                  Substring_Sequence_Node
                  (  Ptr (Substrings.Reference).all
                  ) .Children
         )     );
      end if;
   end "=";

   function "-"
            (  Description : String;
               Value       : String
            )  return Attribute_Definition is
   begin
      return
      (  Reference =>
            Create_Attribute_Data (Description, (1 => Create (Value)))
      );
   end "-";

   function "-"
            (  Description : String;
               Values      : Values_List
            )  return Attribute_Definition is
   begin
      return
      (  Reference =>
            Create_Attribute_Data (Description, Values.List)
      );
   end "-";

   function "-"
            (  Description : String;
               Value       : String
            )  return Attributes_List is
   begin
      return
      (  1,
         (  1 => Create_Attribute_Data
                 (  Description,
                    (  1 => Create (Value)
      )  )       )  );
   end "-";

   function "-"
            (  Description : String;
               Values      : Values_List
            )  return Attributes_List is
   begin
      return
      (  1,
         (1 => Create_Attribute_Data (Description, Values.List))
      );
   end "-";

   function "or" (Left, Right : Attributes_List)
      return Attributes_List is
   begin
      return
      (  Left.Size + Right.Size,
         Left.List & Right.List
      );
   end "or";

   function "or" (Left, Right : Updates_List) return Updates_List is
   begin
      return (Left.Size + Right.Size, Left.List & Right.List);
   end "or";

   function "+" (Value : String) return Values_List is
   begin
      return (1, (1 => Create (Value)));
   end "+";

   function "/" (Left : String; Right : String) return Values_List is
   begin
      return (2, (Create (Left), Create (Right)));
   end "/";

   function "/" (Left : Values_List; Right : String)
      return Values_List is
   begin
      return
      (  Left.Size + 1,
         Left.List & Create (Right)
      );
   end "/";

   function "/" (Left, Right : Substring_Filter)
      return Substring_Filter is
      This : Abstract_Node'Class renames Ptr (Left.Reference).all;
      That : Abstract_Node'Class renames Ptr (Right.Reference).all;
   begin
      if This in Substring_Sequence_Node'Class then
         if That in Substring_Sequence_Node'Class then
            return
            (  Reference =>
                  Create_Substring_Sequence
                  (  Substring_Sequence_Node (This).Children
                  &  Substring_Sequence_Node (That).Children
            )     );
         else
            return
            (  Reference =>
                  Create_Substring_Sequence
                  (  Substring_Sequence_Node (This).Children
                  &  Right.Reference
            )     );
         end if;
      else
         if That in Substring_Sequence_Node'Class then
            return
            (  Reference =>
                  Create_Substring_Sequence
                  (  Left.Reference
                  &  Substring_Sequence_Node (That).Children
            )     );
         else
            return
            (  Reference =>
                  Create_Substring_Sequence
                  (  (Left.Reference, Right.Reference)
            )     );
         end if;
      end if;
   end "/";

   function "<=" (Description, Value : String) return Search_Filter is
   begin
      return
      (  Reference =>
            Create_Comparison (Description, Less_Or_Equal, Value)
      );
   end "<=";

   function ">=" (Description, Value : String) return Search_Filter is
   begin
      return
      (  Reference =>
            Create_Comparison (Description, Greater_Or_Equal, Value)
      );
   end ">=";

   function "and" (Left, Right : Search_Filter) return Search_Filter is
      This : Abstract_Node'Class renames Ptr (Left.Reference).all;
      That : Abstract_Node'Class renames Ptr (Right.Reference).all;
   begin
      if This in And_Node'Class then
         if That in And_Node'Class then
            return
            (  Reference =>
                  Create_And
                  (  And_Node (This).Children & And_Node (That).Children
            )     );
         else
            return
            (  Reference =>
                  Create_And
                  (  And_Node (This).Children & Right.Reference
            )     );
         end if;
      else
         if That in And_Node'Class then
            return
            (  Reference =>
                  Create_And (Left.Reference & And_Node (That).Children)
            );
         else
            return
            (  Reference =>
                  Create_And ((Left.Reference, Right.Reference))
            );
         end if;
      end if;
   end "and";

   function "not" (Left : Search_Filter) return Search_Filter is
   begin
      return (Reference => Create_Not (Left.Reference));
   end "not";

   function "or" (Left, Right : Search_Filter) return Search_Filter is
      This : Abstract_Node'Class renames Ptr (Left.Reference).all;
      That : Abstract_Node'Class renames Ptr (Right.Reference).all;
   begin
      if This in Or_Node'Class then
         if That in Or_Node'Class then
            return
            (  Reference =>
                  Create_Or
                  (  Or_Node (This).Children
                  &  Or_Node (That).Children
            )     );
         else
            return
            (  Reference =>
                  Create_Or (Or_Node (This).Children & Right.Reference)
            );
         end if;
      else
         if That in Or_Node'Class then
            return
            (  Reference =>
                  Create_Or (Left.Reference & Or_Node (That).Children)
            );
         else
            return
            (  Reference =>
                  Create_Or ((Left.Reference, Right.Reference))
            );
         end if;
      end if;
   end "or";

   function Add
            (  Attribute : Attribute_Definition
            )  return Updates_List is
   begin
      return (1, (1 => (Add_Operation, Attribute)));
   end Add;

   procedure Append
             (  List  : in out LDAP_Implicit_String_Sequence;
                Value : String
             )  is
   begin
      Append (List);
      Set_Value (Get (List, Get_Length (List)).all, Value);
   end Append;

   procedure Append
             (  List  : in out LDAP_String_Sequence;
                Value : String
             )  is
   begin
      Append (List);
      Set_Value (Get (List, Get_Length (List)).all, Value);
   end Append;

   procedure Append
             (  List  : in out LDAP_String_Set;
                Value : String
             )  is
   begin
      Append (List);
      Set_Value (Get (List, Get_Length (List)).all, Value);
   end Append;

   procedure Append
             (  List      : in out LDAP_Substring_Sequence;
                Component : Substring_Component_Type;
                Value     : String
             )  is
   begin
      Append (List);
      declare
         Last : LDAP_Substring renames
                Get (List, Get_Length (List)).all;
      begin
         Set_Selected
         (  Last,
            Substring_Component_Type'Pos (Component) + 1
         );
         Set_Value (Last, Value);
      end;
   end Append;

   procedure Append
             (  Attribute : in out LDAP_Attribute;
                Value     : String
             )  is
   begin
      Append (Attribute.Values, Value);
   end Append;

   procedure Append
             (  List        : in out LDAP_Attribute_List;
                Description : String
             )  is
   begin
      Append (List);
      declare
         Last : LDAP_Attribute renames
                Get (List, Get_Length (List)).all;
      begin
         Set_Value (Last.Attribute_Description, Description);
      end;
   end Append;

   procedure Append
             (  List        : in out LDAP_Attribute_List;
                Description : String;
                Value       : String
             )  is
   begin
      Append (List, Description);
      Append_Value (List, Value);
   end Append;

   procedure Append
             (  Filter    : in out LDAP_Filter;
                Component : Substring_Component_Type;
                Value     : String
             )  is
   begin
      if Get_Selected (Filter) = Substrings_Choice then
         Append (Filter.Substrings.Values, Component, Value);
      else
         Raise_Exception
         (  Use_Error'Identity,
            "Substrings is not the current mode selected in the filter"
         );
      end if;
   end Append;

   procedure Append
             (  List        : in out LDAP_Operations_List;
                Operation   : Operation_Type;
                Description : String
             )  is
   begin
      Append (List);
      declare
         Last : LDAP_Modification renames
                Get (List, Get_Length (List)).all;
      begin
         Last.Operation.Value := Operation;
         Set_Value
         (  Last.Modification.Attribute_Description,
            Description
         );
      end;
   end Append;

   procedure Append
             (  List        : in out LDAP_Operations_List;
                Operation   : Operation_Type;
                Description : String;
                Value       : String
             )  is
   begin
      Append (List);
      declare
         Last : LDAP_Modification renames
                Get (List, Get_Length (List)).all;
      begin
         Last.Operation.Value := Operation;
         Set_Value
         (  Last.Modification.Attribute_Description,
            Description
         );
         Append (Last.Modification, Value);
      end;
   end Append;

   function Append_Term
            (  Filter : access LDAP_Filter
            )  return LDAP_Filter_Ptr is
   begin
      case Get_Selected (Filter.all) is
         when And_Filter_Choice =>
            Append (Filter.And_Filter);
            return LDAP_Filter
                   (  Get
                      (  Filter.And_Filter,
                         Get_Length (Filter.And_Filter)
                      ) .all
                   ) 'Unchecked_Access;
         when Or_Filter_Choice =>
            Append (Filter.Or_Filter);
            return LDAP_Filter
                   (  Get
                      (  Filter.Or_Filter,
                         Get_Length (Filter.Or_Filter)
                      ) .all
                   ) 'Unchecked_Access;
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               "Conjunction or disjunction is not the current filter"
            );
      end case;
   end Append_Term;

   procedure Append_Value
             (  List  : in out LDAP_Attribute_List;
                Value : String
             )  is
      Length : constant Natural := Get_Length (List);
   begin
      if Length = 0 then
         Raise_Exception
         (  Use_Error'Identity,
            "Attributes list is empty when appending attribute value"
            );
      end if;
      Append (Get (List,Length).Values, Value);
   end Append_Value;

   procedure Append_Value
             (  List  : in out LDAP_Operations_List;
                Value : String
             )  is
      Length : constant Natural := Get_Length (List);
   begin
      if Length = 0 then
         Raise_Exception
         (  Use_Error'Identity,
            "Operations list is empty when appending operation value"
            );
      end if;
      Append (Get (List, Length).Modification, Value);
   end Append_Value;

   procedure Check_Add (Message : LDAP_Message) is
   begin
      case Get_Selected (Message.Protocol_Op) is
         when Add_Request_Choice | Search_Result_Entry_Choice =>
            null;
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               (  "Add or search entry is not the current operation "
               &  "in the message"
            )  );
      end case;
   end Check_Add;

   procedure Check_Conjunction (Filter : LDAP_Filter) is
   begin
      if Get_Selected (Filter) = And_Filter_Choice then
         return;
      end if;
      Raise_Exception
      (  Use_Error'Identity,
         "Conjunction is not the current operation in the filter"
      );
   end Check_Conjunction;

   procedure Check_Disjunction (Filter : LDAP_Filter) is
   begin
      if Get_Selected (Filter) = Or_Filter_Choice then
         return;
      end if;
      Raise_Exception
      (  Use_Error'Identity,
         "Disjunction is not the current operation in the filter"
      );
   end Check_Disjunction;

   procedure Check_Modify (Message : LDAP_Message) is
   begin
      if Get_Selected (Message.Protocol_Op) = Modify_Request_Choice then
         return;
      end if;
      Raise_Exception
      (  Use_Error'Identity,
         "Modify is not the current operation in the message"
      );
   end Check_Modify;

   procedure Check_Negation (Filter : LDAP_Filter) is
   begin
      if Get_Selected (Filter) = Not_Filter_Choice then
         return;
      end if;
      Raise_Exception
      (  Use_Error'Identity,
         "Negation is not the current operation in the filter"
      );
   end Check_Negation;

   procedure Check_Search (Message : LDAP_Message) is
   begin
      if Get_Selected (Message.Protocol_Op) = Search_Request_Choice then
         return;
      end if;
      Raise_Exception
      (  Use_Error'Identity,
         "Search is not the current operation in the message"
      );
   end Check_Search;

   procedure Check_Search_Result_Reference (Message : LDAP_Message) is
   begin
      if (  Get_Selected (Message.Protocol_Op)
         =  Search_Result_Reference_Choice
         )  then
         return;
      end if;
      Raise_Exception
      (  Use_Error'Identity,
         (  "Search result reference is not the current operation "
         &  "in the message"
      )  );
   end Check_Search_Result_Reference;

   function Conjunction_Size (Filter : LDAP_Filter) return Natural is
   begin
      Check_Conjunction (Filter);
      return Get_Length (Filter.And_Filter);
   end Conjunction_Size;

   function Create (Value : String) return Value_Handles.Handle is
      Result : Value_Handles.Handle;
   begin
      Value_Handles.Set (Result, new Attribute_Value (Value'Length));
      Value_Handles.Ptr (Result).Value := Value;
      return Result;
   end Create;

   function Create
            (  Item : access LDAP_Filter_Reference
            )  return ASN1.Abstract_ASN1_Data_Item_Ptr is
      type Pool_Ptr is access LDAP_Filter;
      for Pool_Ptr'Storage_Pool use Get_Container (Item.all).Pool;
      This : constant Pool_Ptr := new LDAP_Filter;
   begin
      return This.all'Unchecked_Access;
   end Create;

   function Create
            (  Item : access LDAP_Filter_Set
            )  return ASN1.Abstract_ASN1_Data_Item_Ptr is
      type Pool_Ptr is access LDAP_Filter;
      for Pool_Ptr'Storage_Pool use Get_Container (Item.all).Pool;
      This : constant Pool_Ptr := new LDAP_Filter;
   begin
      return This.all'Unchecked_Access;
   end Create;

   function Delete
            (  Attribute : Attribute_Definition
            )  return Updates_List is
   begin
      return (1, (1 => (Delete_Operation, Attribute)));
   end Delete;

   function Delete_All (Description : String) return Updates_List is
   begin
      return
      (  1,
         (  1 => (  Delete_Operation,
                    (  Reference =>
                          Create_Attribute_Data
                          (  Description,
                             (1..0 => Value_Handles.Null_Handle)
      )  )       )  )     );
   end Delete_All;

   function Disjunction_Size (Filter : LDAP_Filter) return Natural is
   begin
      Check_Disjunction (Filter);
      return Get_Length (Filter.Or_Filter);
   end Disjunction_Size;

   function Empty return Values_List is
   begin
      return (0, List => (1..0 => Value_Handles.Null_Handle));
   end Empty;

   function Extended
            (  Value       : String;
               Rule        : String;
               Description : String  := "";
               Attributes  : Boolean := False
            )  return Search_Filter is
   begin
      return
      (  Reference =>
            Create_Rule
            (  Description => Description,
               Value       => Value,
               Rule        => Rule,
               Attributes  => Attributes
      )     );
   end Extended;

   function Failure return Search_Filter is
   begin
      return
      (  Reference =>
            Create_Or ((1..0 => Node_Handles.Null_Handle))
      );
   end Failure;

   function Get (List : Values_List; Index : Positive) return String is
   begin
      if Index > List.Size then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong values list index"
         );
      end if;
      return Value_Handles.Ptr (List.List (Index)).Value;
   end Get;

   function Get (Name : LDAP_DN) return Distinguished_Name is
   begin
      if Get_Length (Name) = 0 then
         return Null_Name;
      else
         return Get_Name (Name);
      end if;
   end Get;

   function Get (Name : LDAP_Implicit_DN) return Distinguished_Name is
   begin
      if Get_Length (Name) = 0 then
         return Null_Name;
      else
         return Get_Name (Name);
      end if;
   end Get;

   function Get
            (  List : LDAP_Attribute_List
            )  return Attributes_List is
      Result : Attributes_List (Get_Length (List));
   begin
      for Index in Result.List'Range loop
         declare
            This : LDAP_Attribute renames Get (List, Index).all;
         begin
            Result.List (Index) :=
               Create_Attribute_Data
               (  Get_Value (This.Attribute_Description),
                  Get (This.Values)
               );
         end;
      end loop;
      return Result;
   end Get;

   function Get
            (  Sequence : LDAP_Implicit_String_Sequence
            )  return Values_List is
      Result : Values_List (Get_Length (Sequence));
   begin
      for Index in Result.List'Range loop
         Result.List (Index) := Create (Get (Sequence, Index));
      end loop;
      return Result;
   end Get;

   function Get (Item : LDAP_Modification) return Operation_Type is
   begin
      return Item.Operation.Value;
   end Get;

   function Get (OID : LDAP_OID) return Object_Identifier is
   begin
      if Get_Length (OID) > 0 then
         return Value (Get_Value (OID));
      else
         return (1..0 => 0);
      end if;
   end Get;

   function Get (List : LDAP_String_Set'Class) return Values_Array is
      Result : Values_Array (1..Get_Length (List));
   begin
      for Index in Result'Range loop
         Result (Index) := Create (Get_Value (Get (List, Index).all));
      end loop;
      return Result;
   end Get;

   function Get_ASN1_Type (Item : LDAP_DN) return ASN1.ASN1_Type is
   begin
      return ASN1.Octet_String_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : LDAP_Implicit_DN
            )  return ASN1.ASN1_Type is
   begin
      return ASN1.Octet_String_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : LDAP_Implicit_String
            )  return ASN1.ASN1_Type is
   begin
      return ASN1.Octet_String_Tag;
   end Get_ASN1_Type;

   function Get_ASN1_Type
            (  Item : LDAP_String
            )  return ASN1.ASN1_Type is
   begin
      return ASN1.Octet_String_Tag;
   end Get_ASN1_Type;

   function Get_Code (Result : LDAP_Result) return Result_Code is
   begin
      return Result.Result_Code.Value;
   end Get_Code;

   function Get_Conjunction_Term
            (  Filter : LDAP_Filter;
               Index  : Positive
            )  return LDAP_Filter_Ptr is
   begin
      Check_Conjunction (Filter);
      return Get_Filter (Filter.And_Filter, Index);
   end Get_Conjunction_Term;

   function Get_Description (List : Attributes_List; Index : Positive)
      return String is
   begin
      if Index > List.Size then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong attributes list index"
         );
      end if;
      return Attribute_Data_Handles.Ptr (List.List (Index)).Description;
   end Get_Description;

   function Get_Description
            (  Attribute : LDAP_Attribute
            )  return String is
   begin
      return Get_Value (Attribute.Attribute_Description);
   end Get_Description;

   function Get_Disjunction_Term
            (  Filter : LDAP_Filter;
               Index  : Positive
            )  return LDAP_Filter_Ptr is
   begin
      Check_Disjunction (Filter);
      return Get_Filter (Filter.Or_Filter, Index);
   end Get_Disjunction_Term;

   function Get_Filter
            (  Item : LDAP_Filter_Item
            )  return LDAP_Filter_Ptr is
   begin
      return Get_Filter (Item.Filter);
   end Get_Filter;

   function Get_Filter
            (  Item : LDAP_Filter_Reference
            )  return LDAP_Filter_Ptr is
   begin
      return LDAP_Filter (Get (Item).all)'Unchecked_Access;
   end Get_Filter;

   function Get_Filter
            (  Item  : LDAP_Filter_Set;
               Index : Positive
            )  return LDAP_Filter_Ptr is
   begin
      return LDAP_Filter (Get (Item, Index).all)'Unchecked_Access;
   end Get_Filter;

   function Get_Length (List : Values_List) return Natural is
   begin
      return List.Size;
   end Get_Length;

   function Get_Length (List : Attributes_List) return Natural is
   begin
      return List.Size;
   end Get_Length;

   function Get_Matched (Result : LDAP_Result)
      return Distinguished_Name is
   begin
      return Get (Result.Matched_DN);
   end Get_Matched;

   function Get_Message (Result : LDAP_Result) return String is
   begin
      return Get_Value (Result.Diagnostic_Message);
   end Get_Message;

   function Get_Negation_Term
            (  Filter : LDAP_Filter
            )  return LDAP_Filter_Ptr is
      use GNAT.Sockets.Connection_State_Machine.ASN1;
      This : constant Abstract_ASN1_Data_Item_Ptr :=
                      Get (Filter.Not_Filter.Filter, True);
   begin
      Check_Negation (Filter);
      if This = null then
         return null;
      else
         return LDAP_Filter (This.all)'Unchecked_Access;
      end if;
   end Get_Negation_Term;

   function Get_Referral (Result : LDAP_Result) return Values_List is
   begin
      if Is_Set (Result, 4) then
         return Get (Result.Referral);
      else
         return Empty;
      end if;
   end Get_Referral;

   function Get_Search_Request_Filter
            (  Message : LDAP_Message
            )  return LDAP_Filter_Ptr is
   begin
      Check_Search (Message);
      return LDAP_Filter
             (  Self (Message.Protocol_Op.Search_Request.Filter).all
             ) 'Unchecked_Access;
   end Get_Search_Request_Filter;

   function Get_Size (Node : Abstract_Node) return Natural is
   begin
      return 0;
   end Get_Size;

   function Get_Size (Node : Abstract_Parent_Node) return Natural is
   begin
      return Node.Size;
   end Get_Size;

   function Get_Size (Node : Not_Node) return Natural is
   begin
      return 1;
   end Get_Size;

   function Get_Value (Item : LDAP_Substring) return String is
   begin
      return Get_Value (LDAP_Assertion_Value (Get_Selected (Item).all));
   end Get_Value;

   function Get_Values (List : Attributes_List; Index : Positive)
      return Values_List is
   begin
      if Index > List.Size then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong attributes list index"
         );
      end if;
      declare
         Data : Attribute_Data'Class renames
                Attribute_Data_Handles.Ptr (List.List (Index)).all;
      begin
         return (Data.Size, Data.List);
      end;
   end Get_Values;

   function Image (Code : Result_Code) return String is
      function Tail return String is
      begin
         return " [" & Image (Integer (Code)) & "]";
      end Tail;
   begin
      case Code is
         when Success_Code =>
            return "Success" & Tail;
         when Operations_Error_Code =>
            return "Operations error" & Tail;
         when Protocol_Error_Code =>
            return "Protocol error" & Tail;
         when Time_Limit_Exceeded_Code =>
            return "Time limit is exceeded" & Tail;
         when Size_Limit_Exceeded_Code =>
            return "Size limit is exceeded" & Tail;
         when Compare_False_Code =>
            return "Compare is false" & Tail;
         when Compare_True_Code =>
            return "Compare is true" & Tail;
         when Auth_Method_Not_Supported_Code =>
            return "Authentication method is not supported" & Tail;
         when Stronger_Auth_Required_Code =>
            return "Stronger authentication is required" & Tail;
         when Referral_Code =>
            return "Referral" & Tail;
         when Admin_Limit_Exceeded_Code =>
            return "Administration limit is exceeded" & Tail;
         when Unavailable_Critical_Extension_Code =>
            return "Critical extension is unavailable" & Tail;
         when Confidentiality_Required_Code =>
            return "Confidentiality is required" & Tail;
         when SASL_Bind_In_Progress_Code =>
            return "SASL bind is in progress" & Tail;
         when No_Such_Attribute_Code =>
            return "No such attribute" & Tail;
         when Undefined_Attribute_Type_Code =>
            return "Undefined attribute type" & Tail;
         when Inappropriate_Matching_Code =>
            return "Inappropriate matching code" & Tail;
         when Constraint_Violation_Code =>
            return "Constraint violation" & Tail;
         when Attribute_Or_Value_Exists_Code =>
            return "Attribute or value exists" & Tail;
         when Invalid_Attribute_Syntax_Code =>
            return "Invalid attribute syntax" & Tail;
         when No_Such_Object_Code =>
            return "No such object" & Tail;
         when Alias_Problem_Code =>
            return "Alias problem" & Tail;
         when Invalid_DN_Syntax_Code =>
            return "Invalid distinguished name syntax" & Tail;
         when Alias_Dereferencing_Problem_Code =>
            return "Alias dereferencing problem" & Tail;
         when Inappropriate_Authentication_Code =>
            return "Inappropriate authentication" & Tail;
         when Invalid_Credentials_Code =>
            return "Invalid credentials" & Tail;
         when Insufficient_Access_Rights_Code =>
            return "Insufficient access rights" & Tail;
         when Busy_Code =>
            return "Busy" & Tail;
         when Unavailable_Code =>
            return "Unavailable" & Tail;
         when Unwilling_To_Perform_Code =>
            return "Unwilling to perform" & Tail;
         when Loop_Detect_Code =>
            return "Loop is detected" & Tail;
         when Naming_Violation_Code =>
            return "Naming violation" & Tail;
         when Object_Class_Violation_Code =>
            return "Object class violation" & Tail;
         when Not_Allowed_On_Non_Leaf_Code =>
            return "Not allowed on a non-leaf" & Tail;
         when Not_Allowed_On_RDN_Code =>
            return "Not allowed on a relative distinguished name" &
                   Tail;
         when Entry_Already_Exists_Code =>
            return "Entry already exists" & Tail;
         when Object_Class_Mods_Prohibited_Code =>
            return "Object class mods are prohibited" & Tail;
         when Affects_Multiple_DSAs_Code =>
            return "Multiple DSAs are affected" & Tail;
         when others =>
            return "Unknown" & Tail;
      end case;
   end Image;

   procedure Initialize (Item : in out LDAP_Output) is
   begin
      External_Initialize (Item.Message, Item.Buffer'Unchecked_Access);
      Item.Message.Message_ID.Value := 1;
   end Initialize;

   procedure Initialized
             (  Sequence : in out LDAP_Attribute_Value_Assertion
             )  is
   begin
      Set_Tag (Sequence, 1, (ASN1.Universal_Tag, 0, False));
      Set_Tag (Sequence, 2, (ASN1.Universal_Tag, 0, False));
   end Initialized;

   procedure Initialized (Response : in out LDAP_Bind_Response) is
   begin
      Initialized (LDAP_Result (Response));
      Set_Tag
      (  Response,
         Get_Length (Response),
         (ASN1.Context_Specific_Tag, 7, True)
      );
   end Initialized;

   procedure Initialized
             (  Sequence : in out
                           LDAP_Implicit_Attribute_Value_Assertion
             )  is
   begin
      Set_Tag (Sequence, 1, (ASN1.Universal_Tag, 0, False));
      Set_Tag (Sequence, 2, (ASN1.Universal_Tag, 0, False));
   end Initialized;

   procedure Initialized (Result : in out LDAP_Result) is
   begin
      Set_Tag (Result, 1, (ASN1.Universal_Tag, 0, False));
      Set_Tag (Result, 2, (ASN1.Universal_Tag, 0, False));
      Set_Tag (Result, 3, (ASN1.Universal_Tag, 0, False));
      Set_Tag (Result, 4, (ASN1.Context_Specific_Tag, 3, True));
   end Initialized;

   procedure Initialized (Credentials : in out LDAP_SASL_Credentials) is
   begin
      Set_Tag (Credentials, 1, (ASN1.Universal_Tag, 0, False));
      Set_Tag (Credentials, 2, (ASN1.Universal_Tag, 0, True));
   end Initialized;

   procedure Initialized (Choice : in out LDAP_Authentication_Choice) is
   begin
      Enable_Unsolicited (Choice, True);
      Set_Tag (Choice, 1, (ASN1.Context_Specific_Tag, 0, True));
      Set_Tag (Choice, 2, (ASN1.Context_Specific_Tag, 3, True));
   end Initialized;

   procedure Initialized (Control : in out LDAP_Control) is
   begin
      Set_Tag (Control, 1, (ASN1.Universal_Tag, 0, False));
      Set_Tag (Control, 2, (ASN1.Universal_Tag, 0, False));
      Set_Tag (Control, 3, (ASN1.Universal_Tag, 0, True));
   end Initialized;

   procedure Initialized (Request : in out LDAP_Extended_Request) is
   begin
      Set_Tag (Request, 1, (ASN1.Context_Specific_Tag, 0, False));
      Set_Tag (Request, 2, (ASN1.Context_Specific_Tag, 1, True));
   end Initialized;

   procedure Initialized (Response : in out LDAP_Extended_Response) is
   begin
      Initialized (LDAP_Result (Response));
      Set_Tag
      (  Response,
         Get_Length (Response) - 1,
         (ASN1.Context_Specific_Tag, 10, True)
      );
      Set_Tag
      (  Response,
         Get_Length (Response),
         (ASN1.Context_Specific_Tag, 11, True)
      );
   end Initialized;

   procedure Initialized (Choice : in out LDAP_Filter) is
   begin
      Enable_Unsolicited (Choice, True);
      Set_Tag (Choice,  1, (ASN1.Context_Specific_Tag, 0, True));
      Set_Tag (Choice,  2, (ASN1.Context_Specific_Tag, 1, True));
      Set_Tag (Choice,  3, (ASN1.Context_Specific_Tag, 2, True));
      Set_Tag (Choice,  4, (ASN1.Context_Specific_Tag, 3, True));
      Set_Tag (Choice,  5, (ASN1.Context_Specific_Tag, 4, True));
      Set_Tag (Choice,  6, (ASN1.Context_Specific_Tag, 5, True));
      Set_Tag (Choice,  7, (ASN1.Context_Specific_Tag, 6, True));
      Set_Tag (Choice,  8, (ASN1.Context_Specific_Tag, 7, True));
      Set_Tag (Choice,  9, (ASN1.Context_Specific_Tag, 8, True));
      Set_Tag (Choice, 10, (ASN1.Context_Specific_Tag, 9, True));
   end Initialized;

   procedure Initialized
             (  Response : in out LDAP_Intermediate_Response
             )  is
   begin
      Set_Tag (Response, 1, (ASN1.Context_Specific_Tag, 0, True));
      Set_Tag (Response, 2, (ASN1.Context_Specific_Tag, 1, True));
   end Initialized;

   procedure Initialized (Request : in out LDAP_Modify_DN_Request) is
   begin
      Set_Tag (Request, 1, (ASN1.Universal_Tag, 0, False));
      Set_Tag (Request, 2, (ASN1.Universal_Tag, 0, False));
      Set_Tag (Request, 3, (ASN1.Universal_Tag, 0, False));
      Set_Tag (Request, 4, (ASN1.Context_Specific_Tag, 0, True));
   end Initialized;

   procedure Initialized (Operation : in out LDAP_Operation) is
   begin
      Enable_Unsolicited (Operation, True);
      Set_Tag (Operation,  1, (ASN1.Application_Tag,  0, True));
      Set_Tag (Operation,  2, (ASN1.Application_Tag,  1, True));
      Set_Tag (Operation,  3, (ASN1.Application_Tag,  2, True));
      Set_Tag (Operation,  4, (ASN1.Application_Tag,  3, True));
      Set_Tag (Operation,  5, (ASN1.Application_Tag,  4, True));
      Set_Tag (Operation,  6, (ASN1.Application_Tag,  5, True));
      Set_Tag (Operation,  7, (ASN1.Application_Tag, 19, True));
      Set_Tag (Operation,  8, (ASN1.Application_Tag,  6, True));
      Set_Tag (Operation,  9, (ASN1.Application_Tag,  7, True));
      Set_Tag (Operation, 10, (ASN1.Application_Tag,  8, True));
      Set_Tag (Operation, 11, (ASN1.Application_Tag,  9, True));
      Set_Tag (Operation, 12, (ASN1.Application_Tag, 10, True));
      Set_Tag (Operation, 13, (ASN1.Application_Tag, 11, True));
      Set_Tag (Operation, 14, (ASN1.Application_Tag, 12, True));
      Set_Tag (Operation, 15, (ASN1.Application_Tag, 13, True));
      Set_Tag (Operation, 16, (ASN1.Application_Tag, 14, True));
      Set_Tag (Operation, 17, (ASN1.Application_Tag, 15, True));
      Set_Tag (Operation, 18, (ASN1.Application_Tag, 16, True));
      Set_Tag (Operation, 19, (ASN1.Application_Tag, 23, True));
      Set_Tag (Operation, 20, (ASN1.Application_Tag, 24, True));
      Set_Tag (Operation, 21, (ASN1.Application_Tag, 25, True));
   end Initialized;

   procedure Initialized (Rule : in out LDAP_Matching_Rule_Assertion) is
   begin
      Set_Tag (Rule, 1, (ASN1.Context_Specific_Tag, 1,  True));
      Set_Tag (Rule, 2, (ASN1.Context_Specific_Tag, 2,  True));
      Set_Tag (Rule, 3, (ASN1.Context_Specific_Tag, 3, False));
      Set_Tag (Rule, 4, (ASN1.Context_Specific_Tag, 4,  True));
   end Initialized;

   procedure Initialized (Message : in out LDAP_Message) is
   begin
      Set_Tag (Message, 1, (ASN1.Universal_Tag, 0, False));
      Set_Untagged (Message, 2);
--    Set_Tag (Message, 2, (ASN1.Universal_Tag, 0, False));
      Set_Tag (Message, 3, (ASN1.Context_Specific_Tag, 0, True));
   end Initialized;

   procedure Initialized (Choice : in out LDAP_Substring) is
   begin
      Set_Tag (Choice, 1, (ASN1.Context_Specific_Tag, 0, True));
      Set_Tag (Choice, 2, (ASN1.Context_Specific_Tag, 1, True));
      Set_Tag (Choice, 3, (ASN1.Context_Specific_Tag, 2, True));
   end Initialized;

   function Like (Description, Value : String) return Search_Filter is
   begin
      return
      (  Reference =>
            Create_Comparison
            (  Description,
               Approximately_Equal,
               Value
      )     );
   end Like;

   function Prefix (Value : String) return Substring_Filter is
   begin
      return (Reference => Create_Substring (Initial_Component, Value));
   end Prefix;

   function Present (Description : String) return Search_Filter is
   begin
      return (Reference => Create_Present (Description));
   end Present;

   function Replace
            (  Attribute : Attribute_Definition
            )  return Updates_List is
   begin
      return (1, (1 => (Replace_Operation, Attribute)));
   end Replace;

   function Replace_All (Description : String) return Updates_List is
   begin
      return
      (  1,
         (  1 => (  Replace_Operation,
                    (  Reference =>
                       Create_Attribute_Data
                       (  Description,
                          (1..0 => Value_Handles.Null_Handle)
      )  )       )  )  );
   end Replace_All;

   procedure Reply_Response (Peer : in out LDAP_Peer) is
      Pointer : Stream_Element_Offset := 1;
      Sent    : Stream_Element_Offset := 1;
   begin
      Peer.Output.Message.Message_ID.Value :=
         Peer.Message.Message_ID.Value;
      Encode (Peer.Output.Message, Peer.Output.Request, Pointer);
      if Available_To_Send (Peer) >= Pointer - 1 then
         Send (Peer, Peer.Output.Request (1..Pointer - 1), Sent);
         if Pointer = Sent then
            return;
         end if;
      end if;
      Raise_Exception
      (  Data_Error'Identity,
         (  "Output buffer overrun, "
         &  Image (Queued_To_Send (Peer))
         &  " elements queued, space for at least more "
         &  Image (Pointer - Sent)
         &  " requred (available "
         &  Image (Available_To_Send (Peer))
         &  ")"
      )  );
   end Reply_Response;

   procedure Send_Request
             (  Peer : in out LDAP_Peer;
                ID   : Integer_32
             )  is
      Pointer : Stream_Element_Offset := 1;
      Sent    : Stream_Element_Offset := 1;
   begin
      Peer.Output.Message.Message_ID.Value := ID;
      Encode (Peer.Output.Message, Peer.Output.Request, Pointer);
      if Available_To_Send (Peer) >= Pointer - 1 then
         Send (Peer, Peer.Output.Request (1..Pointer - 1), Sent);
         if Pointer = Sent then
            return;
         end if;
      end if;
      Raise_Exception
      (  Data_Error'Identity,
         (  "Output buffer overrun, "
         &  Image (Queued_To_Send (Peer))
         &  " elements queued, space for at least more "
         &  Image (Pointer - Sent)
         &  " requred (available "
         &  Image (Available_To_Send (Peer))
         &  ")"
      )  );
   end Send_Request;

   procedure Set
             (  Sequence    : in out LDAP_Attribute_Value_Assertion;
                Description : String;
                Value       : String
             )  is
   begin
      Set_Value (Sequence.Attribute_Description, Description);
      Set_Value (Sequence.Value, Value);
   end Set;

   procedure Set
             (  Filter  : in out LDAP_Filter;
                Present : String
             )  is
   begin
      Set_Selected (Filter, Present_Choice);
      Set_Value (Filter.Present, Present);
   end Set;

   procedure Set
             (  Filter      : in out LDAP_Filter;
                Description : String;
                Comparison  : Comparison_Type;
                Value       : String
             )  is
   begin
      case Comparison is
         when Approximately_Equal =>
            Set_Selected (Filter, Approx_Match_Choice);
            Set (Filter.Approx_Match, Description, Value);
         when Less_Or_Equal =>
            Set_Selected (Filter, Less_Or_Equal_Choice);
            Set (Filter.Less_Or_Equal, Description, Value);
         when Equal =>
            Set_Selected (Filter, Equality_Match_Choice);
            Set (Filter.Equality_Match, Description, Value);
         when Greater_Or_Equal =>
            Set_Selected (Filter, Greater_Or_Equal_Choice);
            Set (Filter.Greater_Or_Equal, Description, Value);
      end case;
   end Set;

   procedure Set
             (  Filter      : in out LDAP_Filter;
                Description : String;
                Component   : Substring_Component_Type;
                Value       : String
             )  is
   begin
      Set_Selected (Filter, Substrings_Choice);
      Set_Value (Filter.Substrings.Description, Description);
      Append (Filter.Substrings.Values, Component, Value);
   end Set;

   procedure Set
             (  Filter      : in out LDAP_Filter;
                Value       : String;
                Rule        : String;
                Description : String  := "";
                Attributes  : Boolean := False
             )  is
      Match : LDAP_Matching_Rule_Assertion renames
              Filter.Extensible_Match;
   begin
      if Rule'Length = 0 and then Description'Length = 0 then
         Raise_Exception
         (  Use_Error'Identity,
            "Either the rule or description must be specified"
         );
      end if;
      Set_Selected (Filter, Extensible_Match_Choice);
      if Rule'Length > 0 then
         Reset (Match, 1, False);
         Set_Value (Match.Rule, Rule);
      else
         Reset (Match, 1, True);
      end if;
      if Description'Length > 0 then
         Reset (Match, 2, False);
         Set_Value (Match.Description, Description);
      else
         Reset (Match, 2, True);
      end if;
      Set_Value (Match.Value, Value);
      if Attributes then
         Reset (Match, 4, False);
         Match.Attributes.Value := True;
      else
         Reset (Match, 4, True);
      end if;
   end Set;

   procedure Set
             (  Sequence    : in out
                              LDAP_Implicit_Attribute_Value_Assertion;
                Description : String;
                Value       : String
             )  is
   begin
      Set_Value (Sequence.Attribute_Description, Description);
      Set_Value (Sequence.Value, Value);
   end Set;

   procedure Set
             (  Item      : in out LDAP_Modification;
                Operation : Operation_Type
             )  is
   begin
      Item.Operation.Value := Operation;
   end Set;

   procedure Set_Add_Attribute
             (  Message     : in out LDAP_Message;
                Descrpition : String;
                Value       : String := ""
             )  is
   begin
      case Get_Selected (Message.Protocol_Op) is
         when Add_Request_Choice =>
            declare
               Request : LDAP_Add_Request renames
                         Message.Protocol_Op.Add_Request;
            begin
               if Value'Length > 0 then
                  Append (Request.Attributes, Descrpition, Value);
               else
                  Append (Request.Attributes, Descrpition);
               end if;
            end;
         when Search_Result_Entry_Choice =>
            declare
               Result : LDAP_Search_Result_Entry renames
                        Message.Protocol_Op.Search_Result_Entry;
            begin
               if Value'Length > 0 then
                  Append (Result.Attributes, Descrpition, Value);
               else
                  Append (Result.Attributes, Descrpition);
               end if;
            end;
         when others =>
            Check_Add (Message);
      end case;
   end Set_Add_Attribute;

   procedure Set_Abandon_Request
             (  Message : in out LDAP_Message;
                ID      : Integer_32
             )  is
      Request : LDAP_Abandon_Request renames
                Message.Protocol_Op.Abandon_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Abandon_Request_Choice);
      Request.Value := ID;
      Set_Optional (Message, 3, True);
   end Set_Abandon_Request;

   procedure Set_Add_Request
             (  Message     : in out LDAP_Message;
                Name        : Distinguished_Name;
                Descrpition : String;
                Value       : String := ""
             )  is
      Request : LDAP_Add_Request renames
                Message.Protocol_Op.Add_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Add_Request_Choice);
      Set_Name (Request.Entry_Item, Name);
      if Value'Length > 0 then
         Append (Request.Attributes, Descrpition, Value);
      else
         Append (Request.Attributes, Descrpition);
      end if;
      Set_Optional (Message, 3, True);
   end Set_Add_Request;

   procedure Set_Add_Request
             (  Message    : in out LDAP_Message;
                Name       : Distinguished_Name;
                Attributes : Attributes_List
             )  is
      Request : LDAP_Add_Request renames
                Message.Protocol_Op.Add_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Add_Request_Choice);
      Set_Name (Request.Entry_Item, Name);
      for Attribute in Attributes.List'Range loop
         declare
            This : Attribute_Data renames
                   Attribute_Data
                   (  Attribute_Data_Handles.Ptr
                      (  Attributes.List (Attribute)
                      ) .all
                   );
         begin
            Append (Request.Attributes, This.Description);
            for Index in This.List'Range loop
               Append_Value
               (  Request.Attributes,
                  Value_Handles.Ptr (This.List (Index)).Value
               );
            end loop;
         end;
      end loop;
      Set_Optional (Message, 3, True);
   end Set_Add_Request;

   procedure Set_Add_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Referral    : Values_List        := Empty
             )  is
      Response : LDAP_Result renames Message.Protocol_Op.Add_Response;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Add_Response_Choice);
      Response.Result_Code.Value := Code;
      Set_Name (Response.Matched_DN, Matched);
      Set_Value (Response.Diagnostic_Message, Diagnostics);
      Set_Optional (Response, 4, True);
      Set_Optional (Message,  3, True);
      Set_Response_Referral (Message, Referral);
   end Set_Add_Response;

   procedure Set_Attribute_Value
             (  Message : in out LDAP_Message;
                Value   : String
             )  is
   begin
      case Get_Selected (Message.Protocol_Op) is
         when Add_Request_Choice =>
            declare
               Request : LDAP_Add_Request renames
                         Message.Protocol_Op.Add_Request;
            begin
               if Get_Length (Request.Attributes) > 0 then
                  Append
                  (  Get
                     (  Request.Attributes,
                        Get_Length (Request.Attributes)
                     ) .all,
                     Value
                  );
               else
                  Raise_Exception
                  (  Use_Error'Identity,
                     "Add request is empty"
                  );
               end if;
            end;
         when Search_Result_Entry_Choice =>
            declare
               Result : LDAP_Search_Result_Entry renames
                        Message.Protocol_Op.Search_Result_Entry;
            begin
               if Get_Length (Result.Attributes) > 0 then
                  Append
                  (  Get
                     (  Result.Attributes,
                        Get_Length (Result.Attributes)
                     ) .all,
                     Value
                  );
               else
                  Raise_Exception
                  (  Use_Error'Identity,
                     "Search result entry is empty"
                  );
               end if;
            end;
         when others =>
            Check_Add (Message);
      end case;
   end Set_Attribute_Value;

   function Set_Conjunction
            (  Filter : access LDAP_Filter
            )  return LDAP_Filter_Ptr is
   begin
      Set_Selected (Filter.all, And_Filter_Choice);
      return Append_Term (Filter);
   end Set_Conjunction;

   procedure Set_Conjunction (Filter : in out LDAP_Filter) is
   begin
      Set_Selected (Filter, And_Filter_Choice);
   end Set_Conjunction;

   function Set_Disjunction
            (  Filter : access LDAP_Filter
            )  return LDAP_Filter_Ptr is
   begin
      Set_Selected (Filter.all, Or_Filter_Choice);
      return Append_Term (Filter);
   end Set_Disjunction;

   procedure Set_Disjunction (Filter : in out LDAP_Filter) is
   begin
      Set_Selected (Filter, Or_Filter_Choice);
   end Set_Disjunction;

   function Set_Negation
            (  Filter : access LDAP_Filter
            )  return LDAP_Filter_Ptr is
   begin
      Set_Selected (Filter.all, Not_Filter_Choice);
      return Get_Negation_Term (Filter.all);
   end Set_Negation;

   procedure Set_Bind_Request
             (  Message  : in out LDAP_Message;
                Name     : Distinguished_Name;
                Password : String
             )  is
      Request : LDAP_Bind_Request renames
                Message.Protocol_Op.Bind_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Bind_Request_Choice);
      Request.Version.Value := 3;
      Set_Name (Request.Name, Name);
      Set_Selected (Request.Authentication, 1);
      Set_Value (Request.Authentication.Simple, Password);
      Set_Optional (Message, 3, True);
   end Set_Bind_Request;

   procedure Set_Bind_Request
             (  Message     : in out LDAP_Message;
                Name        : Distinguished_Name;
                Mechanism   : String;
                Credentials : String
             )  is
      Request : LDAP_Bind_Request renames
                Message.Protocol_Op.Bind_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Bind_Request_Choice);
      Request.Version.Value := 3;
      Set_Name (Request.Name, Name);
      Set_Selected (Request.Authentication, 2);
      Set_Value (Request.Authentication.SASL.Mechanism, Mechanism);
      if Credentials'Length > 0 then
         Reset (Request.Authentication.SASL, 2, False);
         Set_Value
         (  Request.Authentication.SASL.Credentials,
            Credentials
         );
      else
         Reset (Request.Authentication.SASL, 2, True);
      end if;
      Set_Optional (Message, 3, True);
   end Set_Bind_Request;

   procedure Set_Bind_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Credentials : String             := "";
                Referral    : Values_List        := Empty
             )  is
      Response : LDAP_Bind_Response renames
                 Message.Protocol_Op.Bind_Response;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Bind_Response_Choice);
      Response.Result_Code.Value := Code;
      Set_Name (Response.Matched_DN, Matched);
      Set_Value (Response.Diagnostic_Message, Diagnostics);
      Reset (Response, 4, True);
      if Credentials'Length > 0 then
         Reset (Response, 5, False);
         Set_Value (Response.Credentials, Credentials);
      else
         Reset (Response, 5, True);
      end if;
      Set_Optional (Message, 3, True);
      Set_Response_Referral (Message, Referral);
   end Set_Bind_Response;

   procedure Set_Compare_Request
             (  Message     : in out LDAP_Message;
                Name        : Distinguished_Name;
                Description : String;
                Value       : String
             )  is
      Request : LDAP_Compare_Request renames
                Message.Protocol_Op.Compare_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Compare_Request_Choice);
      Set_Name (Request.Entry_Item, Name);
      Set_Value
      (  Request.Attribute_Value_Assertion.Attribute_Description,
         Description
      );
      Set_Value
      (  Request.Attribute_Value_Assertion.Value,
         Value
      );
      Set_Optional (Message, 3, True);
   end Set_Compare_Request;

   procedure Set_Compare_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Referral    : Values_List        := Empty
             )  is
      Response : LDAP_Result renames
                 Message.Protocol_Op.Compare_Response;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Compare_Response_Choice);
      Response.Result_Code.Value := Code;
      Set_Name (Response.Matched_DN, Matched);
      Set_Value (Response.Diagnostic_Message, Diagnostics);
      Set_Optional (Response, 4, True);
      Set_Optional (Message,  3, True);
      Set_Response_Referral (Message, Referral);
   end Set_Compare_Response;

   procedure Set_Delete_Request
             (  Message : in out LDAP_Message;
                Name    : Distinguished_Name
             )  is
      Request : LDAP_Implicit_DN renames
                Message.Protocol_Op.Delete_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Delete_Request_Choice);
      Set_Name (Request, Name);
      Set_Optional (Message, 3, True);
   end Set_Delete_Request;

   procedure Set_Delete_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Referral    : Values_List        := Empty
             )  is
      Response : LDAP_Result renames
                 Message.Protocol_Op.Delete_Response;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Delete_Response_Choice);
      Response.Result_Code.Value := Code;
      Set_Name (Response.Matched_DN, Matched);
      Set_Value (Response.Diagnostic_Message, Diagnostics);
      Set_Optional (Response, 4, True);
      Set_Optional (Message,  3, True);
      Set_Response_Referral (Message, Referral);
   end Set_Delete_Response;

   procedure Set_Description
             (  Attribute : in out LDAP_Attribute;
                Value     : String
             )  is
   begin
      Set_Value (Attribute.Attribute_Description, Value);
   end Set_Description;

   procedure Set_Extended_Request
             (  Message : in out LDAP_Message;
                Name    : Object_Identifier;
                Value   : String := ""
             )  is
      Request : LDAP_Extended_Request renames
                Message.Protocol_Op.Extended_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Extended_Request_Choice);
      Set_Value (Request.Name, Image (Name));
      if Value'Length = 0 then
         Reset (Request, 2, True);
      else
         Reset (Request, 2, False);
         Set_Value (Request.Value, Value);
      end if;
      Set_Optional (Message, 3, True);
   end Set_Extended_Request;

   procedure Set_Extended_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Name        : Object_Identifier  := (1..0 => 0);
                Value       : String             := "";
                Referral    : Values_List        := Empty
             )  is
      Response : LDAP_Extended_Response renames
                 Message.Protocol_Op.Extended_Response;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Extended_Response_Choice);
      Response.Result_Code.Value := Code;
      Set_Name (Response.Matched_DN, Matched);
      Set_Value (Response.Diagnostic_Message, Diagnostics);
      Set_Optional (Response, 4, True);
      if Name'Length > 0 then
         Reset (Response, 5, False);
         Set_Value (Response.Name, Image (Name));
      else
         Reset (Response, 5, True);
      end if;
      if Value'Length > 0 then
         Reset (Response, 6, False);
         Set_Value (Response.Value, Value);
      else
         Reset (Response, 6, True);
      end if;
      Set_Optional (Message, 3, True);
      Set_Response_Referral (Message, Referral);
   end Set_Extended_Response;

   procedure Set_Intermediate_Response
             (  Message : in out LDAP_Message;
                Name    : Object_Identifier := (1..0 => 0);
                Value   : String            := ""
             )  is
      Request : LDAP_Intermediate_Response renames
                Message.Protocol_Op.Intermediate_Response;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Intermediate_Response_Choice);
      if Name'Length = 0 then
         Reset (Request, 1, True);
      else
         Reset (Request, 1, False);
         Set_Value (Request.Name, Image (Name));
      end if;
      if Value'Length = 0 then
         Reset (Request, 2, True);
      else
         Reset (Request, 2, False);
         Set_Value (Request.Value, Value);
      end if;
      Set_Optional (Message, 3, True);
   end Set_Intermediate_Response;

   procedure Set_Modify_Request
             (  Message     : in out LDAP_Message;
                Name        : Distinguished_Name;
                Operation   : Operation_Type;
                Descrpition : String;
                Value       : String := ""
             )  is
      Request : LDAP_Modify_Request renames
                Message.Protocol_Op.Modify_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Modify_Request_Choice);
      Set_Name (Request.Object, Name);
      if Value'Length > 0 then
         Append (Request.Changes, Operation, Descrpition, Value);
      else
         Append (Request.Changes, Operation, Descrpition);
      end if;
      Set_Optional (Message, 3, True);
   end Set_Modify_Request;

   procedure Set_Modify_Request
             (  Message : in out LDAP_Message;
                Name    : Distinguished_Name;
                Update  : Updates_List
             )  is
      Request : LDAP_Modify_Request renames
                Message.Protocol_Op.Modify_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Modify_Request_Choice);
      Set_Name (Request.Object, Name);
      for Index in Update.List'Range loop
         declare
            Attribute : Attribute_Data'Class renames
                        Attribute_Data_Handles.Ptr
                        (  Update.List (Index).Attribute.Reference
                        ) .all;
         begin
            Append
            (  Request.Changes,
               Update.List (Index).Operation,
               Attribute.Description
            );
            for Item in Attribute.List'Range loop
               Append_Value
               (  Request.Changes,
                  Value_Handles.Ptr (Attribute.List (Item)).Value
               );
            end loop;
         end;
      end loop;
      Set_Optional (Message, 3, True);
   end Set_Modify_Request;

   procedure Set_Modify_Request_Add_Operation
             (  Message     : in out LDAP_Message;
                Operation   : Operation_Type;
                Descrpition : String;
                Value       : String := ""
             )  is
      Request : LDAP_Modify_Request renames
                Message.Protocol_Op.Modify_Request;
   begin
      Check_Modify (Message);
      if Value'Length > 0 then
         Append (Request.Changes, Operation, Descrpition, Value);
      else
         Append (Request.Changes, Operation, Descrpition);
      end if;
   end Set_Modify_Request_Add_Operation;

   procedure Set_Modify_Request_Add_Value
             (  Message : in out LDAP_Message;
                Value   : String
             )  is
      Request : LDAP_Modify_Request renames
                Message.Protocol_Op.Modify_Request;
   begin
      Check_Modify (Message);
      if Get_Length (Request.Changes) = 0 then
         Raise_Exception
         (  Use_Error'Identity,
            "Modify request is empty"
         );
      end if;
      Append
      (  Get
         (  Request.Changes,
            Get_Length (Request.Changes)
         ) .Modification,
         Value
      );
   end Set_Modify_Request_Add_Value;

   procedure Set_Modify_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Referral    : Values_List        := Empty
             )  is
      Response : LDAP_Result renames
                 Message.Protocol_Op.Modify_Response;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Modify_Response_Choice);
      Response.Result_Code.Value := Code;
      Set_Name (Response.Matched_DN, Matched);
      Set_Value (Response.Diagnostic_Message, Diagnostics);
      Set_Optional (Response, 4, True);
      Set_Optional (Message,  3, True);
      Set_Response_Referral (Message, Referral);
   end Set_Modify_Response;

   procedure Set_Modify_DN_Request
             (  Message      : in out LDAP_Message;
                Name         : Distinguished_Name;
                New_RDN      : Distinguished_Name;
                Delete_RDN   : Boolean;
                New_Superior : Distinguished_Name
             )  is
      Request : LDAP_Modify_DN_Request renames
                Message.Protocol_Op.Modify_DN_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Modify_DN_Request_Choice);
      Set_Name (Request.Entry_Item, Name);
      Set_Name (Request.New_RDN, New_RDN);
      Request.Delete_Old_RDN.Value := Delete_RDN;
      Set_Name (Request.New_Superior, New_Superior);
      Reset (Request, 4, False);
      Set_Optional (Message, 3, True);
   end Set_Modify_DN_Request;

   procedure Set_Modify_DN_Request
             (  Message    : in out LDAP_Message;
                Name       : Distinguished_Name;
                New_RDN    : Distinguished_Name;
                Delete_RDN : Boolean
             )  is
      Request : LDAP_Modify_DN_Request renames
                Message.Protocol_Op.Modify_DN_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Modify_DN_Request_Choice);
      Set_Name (Request.Entry_Item, Name);
      Set_Name (Request.New_RDN, New_RDN);
      Request.Delete_Old_RDN.Value := Delete_RDN;
      Reset (Request, 4, True);
      Set_Optional (Message, 3, True);
   end Set_Modify_DN_Request;

   procedure Set_Modify_DN_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Referral    : Values_List        := Empty
             )  is
      Response : LDAP_Result renames
                 Message.Protocol_Op.Modify_DN_Response;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Modify_DN_Response_Choice);
      Response.Result_Code.Value := Code;
      Set_Name (Response.Matched_DN, Matched);
      Set_Value (Response.Diagnostic_Message, Diagnostics);
      Set_Optional (Response, 4, True);
      Set_Optional (Message,  3, True);
      Set_Response_Referral (Message, Referral);
   end Set_Modify_DN_Response;

   procedure Set_Response_Referral
             (  Message : in out LDAP_Message;
                URL     : String
             )  is
      procedure Set (Result : in out LDAP_Result'Class) is
      begin
         Reset (Result, 4, False);
         Append (Result.Referral, URL);
      end Set;
   begin
      case Get_Selected (Message.Protocol_Op) is
         when Bind_Response_Choice =>
            Set (Message.Protocol_Op.Bind_Response);
         when Search_Result_Done_Choice =>
            Set (Message.Protocol_Op.Search_Result_Done);
         when Modify_Response_Choice =>
            Set (Message.Protocol_Op.Modify_Response);
         when Add_Response_Choice =>
            Set (Message.Protocol_Op.Add_Response);
         when Delete_Response_Choice =>
            Set (Message.Protocol_Op.Delete_Response);
         when Modify_DN_Response_Choice =>
            Set (Message.Protocol_Op.Modify_DN_Response);
         when Compare_Response_Choice =>
            Set (Message.Protocol_Op.Compare_Response);
         when Extended_Response_Choice =>
            Set (Message.Protocol_Op.Extended_Response);
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               (  "A response or result is not the current operation "
               &  "in the message"
            )  );
      end case;
   end Set_Response_Referral;

   procedure Set_Response_Referral
             (  Message : in out LDAP_Message;
                URLs    : Values_List
             )  is
      procedure Set (Result : in out LDAP_Result'Class) is
      begin
         if URLs.Size > 0 then
            Reset (Result, 4, False);
            for Index in URLs.List'Range loop
               Append
               (  Result.Referral,
                  Value_Handles.Ptr (URLs.List (Index)).Value
               );
            end loop;
         end if;
      end Set;
   begin
      case Get_Selected (Message.Protocol_Op) is
         when Bind_Response_Choice =>
            Set (Message.Protocol_Op.Bind_Response);
         when Search_Result_Done_Choice =>
            Set (Message.Protocol_Op.Search_Result_Done);
         when Modify_Response_Choice =>
            Set (Message.Protocol_Op.Modify_Response);
         when Add_Response_Choice =>
            Set (Message.Protocol_Op.Add_Response);
         when Delete_Response_Choice =>
            Set (Message.Protocol_Op.Delete_Response);
         when Modify_DN_Response_Choice =>
            Set (Message.Protocol_Op.Modify_DN_Response);
         when Compare_Response_Choice =>
            Set (Message.Protocol_Op.Compare_Response);
         when Extended_Response_Choice =>
            Set (Message.Protocol_Op.Extended_Response);
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               (  "A response or result is not the current operation "
               &  "in the message"
            )  );
      end case;
   end Set_Response_Referral;

   procedure Set_Search_Result_Done
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Referral    : Values_List        := Empty
             )  is
      Response : LDAP_Result renames
                 Message.Protocol_Op.Search_Result_Done;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Search_Result_Done_Choice);
      Response.Result_Code.Value := Code;
      Set_Name (Response.Matched_DN, Matched);
      Set_Value (Response.Diagnostic_Message, Diagnostics);
      Set_Optional (Response, 4, True);
      Set_Optional (Message,  3, True);
      Set_Response_Referral (Message, Referral);
   end Set_Search_Result_Done;

   procedure Set_Search_Result_Entry
             (  Message     : in out LDAP_Message;
                Name        : Distinguished_Name;
                Descrpition : String;
                Value       : String := ""
             )  is
      Request : LDAP_Search_Result_Entry renames
                Message.Protocol_Op.Search_Result_Entry;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Search_Result_Entry_Choice);
      Set_Name (Request.Object_Name, Name);
      if Value'Length > 0 then
         Append (Request.Attributes, Descrpition, Value);
      else
         Append (Request.Attributes, Descrpition);
      end if;
      Set_Optional (Message, 3, True);
   end Set_Search_Result_Entry;

   procedure Set_Search_Result_Entry
             (  Message    : in out LDAP_Message;
                Name       : Distinguished_Name;
                Attributes : Attributes_List
             )  is
      Request : LDAP_Add_Request renames
                Message.Protocol_Op.Add_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Search_Result_Entry_Choice);
      Set_Name (Request.Entry_Item, Name);
      for Attribute in Attributes.List'Range loop
         declare
            This : Attribute_Data renames
                   Attribute_Data
                   (  Attribute_Data_Handles.Ptr
                      (  Attributes.List (Attribute)
                      ) .all
                   );
         begin
            Append (Request.Attributes, This.Description);
            for Index in This.List'Range loop
               Append_Value
               (  Request.Attributes,
                  Value_Handles.Ptr (This.List (Index)).Value
               );
            end loop;
         end;
      end loop;
      Set_Optional (Message, 3, True);
   end Set_Search_Result_Entry;

   procedure Set_Search_Result_Reference
             (  Message : in out LDAP_Message;
                URI     : String
             )  is
      Request : LDAP_Implicit_String_Sequence renames
                Message.Protocol_Op.Search_Result_Reference;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected
      (  Message.Protocol_Op,
         Search_Result_Reference_Choice
      );
      Append (Request, URI);
      Set_Optional (Message, 3, True);
   end Set_Search_Result_Reference;

   procedure Set_Search_Result_Reference
             (  Message : in out LDAP_Message;
                URIs    : Values_List
             )  is
      Request : LDAP_Implicit_String_Sequence renames
                Message.Protocol_Op.Search_Result_Reference;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected
      (  Message.Protocol_Op,
         Search_Result_Reference_Choice
      );
      for Index in URIs.List'Range loop
         Append
         (  Request,
            Value_Handles.Ptr (URIs.List (Index)).Value
         );
      end loop;
      Set_Optional (Message, 3, True);
   end Set_Search_Result_Reference;

   procedure Set_Search_Result_Reference_URI
             (  Message : in out LDAP_Message;
                URI     : String
             )  is
      Request : LDAP_Implicit_String_Sequence renames
                Message.Protocol_Op.Search_Result_Reference;
   begin
      Check_Search_Result_Reference (Message);
      Append (Request, URI);
   end Set_Search_Result_Reference_URI;

   procedure Set_Search_Request_Common
             (  Message       : in out LDAP_Message'Class;
                Name          : Distinguished_Name;
                Scope         : Scope_Type;
                Aliasing_Mode : Dereference_Type;
                Size_Limit    : Integer_32;
                Time_Limit    : Duration;
                Types_Only    : Boolean
             )  is
      Request : LDAP_Search_Request renames
                Message.Protocol_Op.Search_Request;
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Search_Request_Choice);
      Set_Name (Request.Base_Object, Name);
      Request.Scope.Value := Scope;
      Request.Deref_Aliases.Value := Aliasing_Mode;
      if Size_Limit < 0 then
         raise Constraint_Error;
      end if;
      Request.Size_Limit.Value := Size_Limit;
      if Time_Limit < 0.0 then
         raise Constraint_Error;
      elsif Time_Limit = Duration'Last then
         Request.Time_Limit.Value := Integer_32'Last;
      else
         begin
            Request.Time_Limit.Value := Integer_32 (Time_Limit);
         exception
            when others =>
               Request.Time_Limit.Value := Integer_32'Last;
         end;
      end if;
      Request.Types_Only.Value := Types_Only;
      Set_Optional (Message, 3, True);
   end Set_Search_Request_Common;

   procedure Set_Search_Request
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Filter        : Search_Filter;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             )  is
      Request : LDAP_Search_Request renames
                Message.Protocol_Op.Search_Request;
      procedure Add
                (  To   : access LDAP_Filter;
                   From : Abstract_Node'Class
                )  is
      begin
         if From in And_Node then
            declare
               Terms : Node_Array renames And_Node (From).Children;
            begin
               Set_Conjunction (To.all);
               for Index in Terms'Range loop
                  Add (Append_Term (To), Ptr (Terms (Index)).all);
               end loop;
            end;
         elsif From in Or_Node then
            declare
               Terms : Node_Array renames Or_Node (From).Children;
            begin
               Set_Disjunction (To.all);
               for Index in Terms'Range loop
                  Add (Append_Term (To), Ptr (Terms (Index)).all);
               end loop;
            end;
         elsif From in Not_Node then
            Add
            (  Set_Negation (To),
               Ptr (Not_Node (From).Child).all
            );
         elsif From in Comparison_Node then
            declare
               This : Comparison_Node renames
                      Comparison_Node (From);
            begin
               Set (To.all, This.Description, This.Mode, This.Value);
            end;
         elsif From in Present_Node then
            Set (To.all, Present_Node (From).Description);
         elsif From in Substring_List_Node then
            declare
               List : Substring_List_Node renames
                      Substring_List_Node (From);
            begin
               declare
                  First : Substring_Node renames
                          Substring_Node (Ptr (List.Children (1)).all);
               begin
                  Set
                  (  Filter      => To.all,
                     Description => List.Description,
                     Component   => First.Component,
                     Value       => First.Substring
                  );
               end;
               for Index in 2..List.Children'Last loop
                  declare
                     Next : Substring_Node renames
                            Substring_Node
                            (  Ptr (List.Children (Index)).all
                            );
                  begin
                     Append (To.all, Next.Component, Next.Substring);
                  end;
               end loop;
            end;
         elsif From in Rule_Node then
            declare
               This : Rule_Node renames Rule_Node (From);
            begin
               Set
               (  Filter      => To.all,
                  Value       => This.Value,
                  Attributes  => This.Attributes,
                  Rule        => This.Rule,
                  Description => This.Description
               );
            end;
         end if;
      end Add;
   begin
      Set_Search_Request_Common
      (  Message       => Message,
         Name          => Name,
         Scope         => Scope,
         Aliasing_Mode => Aliasing_Mode,
         Size_Limit    => Size_Limit,
         Time_Limit    => Time_Limit,
         Types_Only    => Types_Only
      );
      Add (Request.Filter'Access, Ptr (Filter.Reference).all);
   end Set_Search_Request;

   procedure Set_Search_Request
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Present       : String;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             )  is
      Request : LDAP_Search_Request renames
                Message.Protocol_Op.Search_Request;
   begin
      Set_Search_Request_Common
      (  Message       => Message,
         Name          => Name,
         Scope         => Scope,
         Aliasing_Mode => Aliasing_Mode,
         Size_Limit    => Size_Limit,
         Time_Limit    => Time_Limit,
         Types_Only    => Types_Only
      );
      Set (Request.Filter, Present);
   end Set_Search_Request;

   procedure Set_Search_Request
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Description   : String;
                Comparison    : Comparison_Type;
                Value         : String;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             )  is
      Request : LDAP_Search_Request renames
                Message.Protocol_Op.Search_Request;
   begin
      Set_Search_Request_Common
      (  Message       => Message,
         Name          => Name,
         Scope         => Scope,
         Aliasing_Mode => Aliasing_Mode,
         Size_Limit    => Size_Limit,
         Time_Limit    => Time_Limit,
         Types_Only    => Types_Only
      );
      Set (Request.Filter, Description, Comparison, Value);
   end Set_Search_Request;

   procedure Set_Search_Request
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Description   : String;
                Component     : Substring_Component_Type;
                Value         : String;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             )  is
      Request : LDAP_Search_Request renames
                Message.Protocol_Op.Search_Request;
   begin
      Set_Search_Request_Common
      (  Message       => Message,
         Name          => Name,
         Scope         => Scope,
         Aliasing_Mode => Aliasing_Mode,
         Size_Limit    => Size_Limit,
         Time_Limit    => Time_Limit,
         Types_Only    => Types_Only
      );
      Set (Request.Filter, Description, Component, Value);
   end Set_Search_Request;

   procedure Set_Search_Request_Substring
             (  Message   : in out LDAP_Message;
                Component : Substring_Component_Type;
                Value     : String
             )  is
      Request : LDAP_Search_Request renames
                Message.Protocol_Op.Search_Request;
   begin
      Check_Search (Message);
      Append (Request.Filter, Component, Value);
   end Set_Search_Request_Substring;

   procedure Set_Search_Request
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Value         : String;
                Rule          : String;
                Description   : String           := "";
                Attributes    : Boolean          := False;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             )  is
      Request : LDAP_Search_Request renames
                Message.Protocol_Op.Search_Request;
   begin
      Set_Search_Request_Common
      (  Message       => Message,
         Name          => Name,
         Scope         => Scope,
         Aliasing_Mode => Aliasing_Mode,
         Size_Limit    => Size_Limit,
         Time_Limit    => Time_Limit,
         Types_Only    => Types_Only
      );
      Set
      (  Filter      => Request.Filter,
         Value       => Value,
         Rule        => Rule,
         Description => Description,
         Attributes  => Attributes
      );
   end Set_Search_Request;

   procedure Set_Search_Request_Attribute
             (  Message   : in out LDAP_Message;
                Attribute : String
             )  is
      Request : LDAP_Search_Request renames
                Message.Protocol_Op.Search_Request;
   begin
      Check_Search (Message);
      Append (Request.Attributes, Attribute);
   end Set_Search_Request_Attribute;

   function Set_Search_Request_Conjunction
            (  Message       : LDAP_Message;
               Name          : Distinguished_Name;
               Scope         : Scope_Type       := Whole_Subtree_Scope;
               Aliasing_Mode : Dereference_Type := Deref_Always;
               Size_Limit    : Integer_32       := 0;
               Time_Limit    : Duration         := 0.0;
               Types_Only    : Boolean          := False
            )  return LDAP_Filter_Ptr is
      This    : LDAP_Message renames LDAP_Message (Self (Message).all);
      Request : LDAP_Search_Request renames
                This.Protocol_Op.Search_Request;
   begin
      Set_Search_Request_Common
      (  Message       => This,
         Name          => Name,
         Scope         => Scope,
         Aliasing_Mode => Aliasing_Mode,
         Size_Limit    => Size_Limit,
         Time_Limit    => Time_Limit,
         Types_Only    => Types_Only
      );
      return Set_Conjunction (Request.Filter'Access);
   end Set_Search_Request_Conjunction;

   procedure Set_Search_Request_Conjunction
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             )  is
      Request : LDAP_Search_Request renames
                Message.Protocol_Op.Search_Request;
   begin
      Set_Search_Request_Common
      (  Message       => Message,
         Name          => Name,
         Scope         => Scope,
         Aliasing_Mode => Aliasing_Mode,
         Size_Limit    => Size_Limit,
         Time_Limit    => Time_Limit,
         Types_Only    => Types_Only
      );
      Set_Selected (Request.Filter, And_Filter_Choice);
   end Set_Search_Request_Conjunction;

   function Set_Search_Request_Disjunction
            (  Message       : LDAP_Message;
               Name          : Distinguished_Name;
               Scope         : Scope_Type       := Whole_Subtree_Scope;
               Aliasing_Mode : Dereference_Type := Deref_Always;
               Size_Limit    : Integer_32       := 0;
               Time_Limit    : Duration         := 0.0;
               Types_Only    : Boolean          := False
            )  return LDAP_Filter_Ptr is
      This    : LDAP_Message renames LDAP_Message (Self (Message).all);
      Request : LDAP_Search_Request renames
                This.Protocol_Op.Search_Request;
   begin
      Set_Search_Request_Common
      (  Message       => This,
         Name          => Name,
         Scope         => Scope,
         Aliasing_Mode => Aliasing_Mode,
         Size_Limit    => Size_Limit,
         Time_Limit    => Time_Limit,
         Types_Only    => Types_Only
      );
      return Set_Disjunction (Request.Filter'Access);
   end Set_Search_Request_Disjunction;

   procedure Set_Search_Request_Disjunction
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             )  is
      Request : LDAP_Search_Request renames
                Message.Protocol_Op.Search_Request;
   begin
      Set_Search_Request_Common
      (  Message       => Message,
         Name          => Name,
         Scope         => Scope,
         Aliasing_Mode => Aliasing_Mode,
         Size_Limit    => Size_Limit,
         Time_Limit    => Time_Limit,
         Types_Only    => Types_Only
      );
      Set_Selected (Request.Filter, Or_Filter_Choice);
   end Set_Search_Request_Disjunction;

   function Set_Search_Request_Negation
            (  Message       : LDAP_Message;
               Name          : Distinguished_Name;
               Scope         : Scope_Type       := Whole_Subtree_Scope;
               Aliasing_Mode : Dereference_Type := Deref_Always;
               Size_Limit    : Integer_32       := 0;
               Time_Limit    : Duration         := 0.0;
               Types_Only    : Boolean          := False
            )  return LDAP_Filter_Ptr is
      This    : LDAP_Message renames LDAP_Message (Self (Message).all);
      Request : LDAP_Search_Request renames
                This.Protocol_Op.Search_Request;
   begin
      Set_Search_Request_Common
      (  Message       => This,
         Name          => Name,
         Scope         => Scope,
         Aliasing_Mode => Aliasing_Mode,
         Size_Limit    => Size_Limit,
         Time_Limit    => Time_Limit,
         Types_Only    => Types_Only
      );
      return Set_Negation (Request.Filter'Access);
   end Set_Search_Request_Negation;

   procedure Set_Unbind_Request (Message : in out LDAP_Message) is
   begin
      Set_Optional (Message.Control, 1, True);
      Set_Optional (Message.Control, 2, True);
      Set_Optional (Message.Control, 3, True);
      Set_Selected (Message.Protocol_Op, Unbind_Request_Choice);
      Set_Optional (Message, 3, True);
   end Set_Unbind_Request;

   procedure Set_Value (Item : in out LDAP_Substring; Value : String) is
   begin
      Set_Value (LDAP_Assertion_Value (Get_Selected (Item).all), Value);
   end Set_Value;

   function Success return Search_Filter is
   begin
      return
      (  Reference =>
            Create_And ((1..0 => Node_Handles.Null_Handle))
      );
   end Success;

   function Substring (Value : String) return Substring_Filter is
   begin
      return (Reference => Create_Substring (Any_Component, Value));
   end Substring;

   function Suffix (Value : String) return Substring_Filter is
   begin
      return (Reference => Create_Substring (Final_Component, Value));
   end Suffix;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : LDAP_Output
             )  is
   begin
      null;
   end Write;

end GNAT.Sockets.Connection_State_Machine.LDAP;
