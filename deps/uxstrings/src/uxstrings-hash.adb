with Ada.Strings.Hash;

function UXStrings.Hash (Key : UXString) return Ada.Containers.Hash_Type is
begin
      return Ada.Strings.Hash (String(Key.Chars.all));
end UXStrings.Hash;
