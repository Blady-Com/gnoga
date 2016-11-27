#Proposal for Gnoga coding guidelines:

The following guidelines apply on Gnoga framework (e.g. not on tests, demos nor tutorials).


###1. Version numbering

m.n with m major version number , n minor version number.

* -alpha is added on m.n when this version is created, to be used without any warranty.
* -beta is added on m.n when this version is enough mature to be tested.
* Then a letter is added on m.n when this version becomes a stable reference release (fully tested).
* The letter is incremented when fixes come.

For instance : 1.2-alpha, 1.2-beta, 1.2a, 1.2b, 1.2c…

Version number is set in gnoga.ads and with a GIT label.

Example:

``` ada
version      : constant String := "1.3-alpha";
```

>Discussion: (JRC) I still feel there should be a role for m.n
without any suffix.

###2. Header
Several lines of fixed comments customized with the name and type of the unit, the creation date and the name of the author.

Example:

``` ada
------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                         G N O G A . T Y P E S                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------
```

###3. Purpose of the unit
Several line of comments indicating the purpose of the unit after header and before first declaration.

Example:

``` ada
--  This package allows for the creation of GUI applications with multiple
--  connections to the same app.
```

###4. Coding style
Line length is limited to 120 characters.
Ada keyword are in lower case.
Ada entities are in mixed case with an underscore to separate words.

Example:

``` ada
procedure On_Connect_Handler (Event : in Application_Connect_Event;
                              Path  : in String := "default");
```

###5. Getter and setter
Getter and setter names are identical as Ada provides function for getters and procedure for setters.

Example:

``` ada
procedure Resizable (Element : in out Element_Type;
                     Value   : in     Resizable_Type);
function Resizable (Element : Element_Type) return Resizable_Type;
```
>Discussion: (JPR) This is current practice in Gnoga, however I'm not so much in favour of it, because it prevents the usual usage that functions names are names or adjectives, while procedure names are verbs. Presumably too late to change for Gnoga.
>(JRC) This is current Gnoga style, but I find it awkward. Standard usage is for procedure names to be verbs or verb phrases. Non-Boolean functions should be nouns or noun phrases; Boolean functions should be adjectives, adjective
phrases, or predicates.


###6. Entity description
Entity description is lines of comments below the entity declaration.

Example:

``` ada
function Client_Width (Element : Element_Type) return Natural;
--  Inner width of an element in pixels.
--  CSS width + CSS padding - width of vertical scrollbar (if present)
--  Does not include the border or margin.
```
Gnatdoc annotations are recommended, see http://docs.adacore.com/gnatdoc-docs/users_guide/_build/html/gnatdoc.html#annotating-source-files.

Example:

``` ada
-- TODO
```

###7. Ada contracts
Ada contracts Pre, Post and type invariant are recommended.

Example:

``` ada
-- TODO
```

###8. Ada type
Ada type name is ending with "\_Type" for a common type, "\_Access" for an simple access type, "\_Class" for an access to class type.

Example:

``` ada
type Element_Type is new Gnoga.Gui.Base.Base_Type with private;
type Element_Access is access all Element_Type;
type Pointer_To_Element_Class is access all Element_Type'Class;
```
>Discussion: (M) I think that this kind of "style" rule was really only necessarily helpful in languages like C that did not have a strong type system. (JCR) I think it fair to point out that the guidelines have to fit with the existing Gnoga code. (JPR) We are not starting a new project here, just making sure that contributors keep a standard style. IMHO, I do not fully agree with all these rules, however, whatever the rules, the important issue is to keep a uniform style.

###9. Protype declaration
Functions and procedures must be declared before coding the body. Comments describing a subprogram should come after its declaration.

Example:

``` ada
function Split (P : String) return String;
--  Split string and extract values

function Split (P : String) return String is
begin
   S := F + 1;
   F := Index (Source  => Value,
               Pattern => P,
               From    => S);
   return Value (S .. (F - 1));
end Split;
```

###10. Use clause
Use clause is restricted to minimum use, preferably local use to functions or procedures. Use type clause is recommended.

Example:

``` ada
function To_RGBA_From_RGB_or_RGBA (Value : String) return RGBA_Type is
   use Ada.Strings.Fixed;
   S    : Integer   := Value'First;
...   
```
``` ada
function Tab_Selected (Item : Tab_Item_Type) return Boolean
is
   use type Gnoga.Types.RGBA_Type;
   Link : Gnoga.Gui.Element.Element_Type;
begin
   Link.Attach_Using_Parent (Item, ID => Item.ID & "_a");
   return Link.Background_Color = Tab_Access (Item.Parent).Select_Color;
end Tab_Selected;
```

###11. Code reformat
Code reformat is recommended before commit to Gnoga repository.

Example:

```
$ gnatpp -rnb -P src/gnoga.gpr
```



###12. Style check
GNAT style check is mandatory before commit to Gnoga repository.

Example:

```
-gnaty3abcefhiklmnOprsStu
```
AdaControl check is recommended before commit to Gnoga repository.

Example:

```
TODO
```


###13. Warnings
For debug builds, most warnings, runtime verifications and exception tracebacks are recommended.

Example:

```
Debug_Options := ("-g", "-gnata", "-gnatq", "-gnatQ", "-gnatw.eH.YD", "-gnatVa", "-gnato", "-fstack-check", "-gnatf", "-gnateE", "-gnateF");
```


For release builds, runtime verifications disabled and optimisations enabled are recommended.

Example:

```
Release_Options := ("-O2");
```
>Discussion: (JPR) I'm OK for disabling assertions, but not other checks (constraint_error...). I meant all forms of assertions (including Pre/Post), not just pragma Assert. The trouble is that sometimes, the post-condition needs more
computations than the subprogram being described. (AV) But it should be shorter that unit test :-). Post conditions replacing unit test is my most favorite feature. (JRC) I'm for leaving them all on.


###14. Platform
Implementation must target Linux, macOS and Windows platform. Underlying HTML, CSS, SVG and Javascript code must be conformant to https://www.w3.org/TR/ and executable at least with Firefox.

###15. Configuration management
GIT is currently used on repository https://sourceforge.net/projects/gnoga.

Before commit, source code shall compile.

Example:

```
$ make gnoga
```

Before label set, tests, demos and tutos shall compile in debug mode and should execute without exceptions on at least one platform.

Example:

```
$ make tests demo tutorials
```

###16. Compiler
Interface shall not be compiler dependent and implementation should not be compiler dependent.
Both shall compile at least with GNAT FSF.

>Discussion: (WP) I find it odd to ask for compiler independence at this point. When last I looked at Gnoga it depends on GNAT specific libraries and attributes (eg Unrestricted_Access). (DB) Well if someone is inspired that upgrade is doable. Most unrestricted access is just used in creating simple examples not in the framework. 

###17. Language
The Ada language standard used is Ada 2012 without restrictions.
