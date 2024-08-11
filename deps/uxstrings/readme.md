# Unicode Extended Strings (UXStrings)

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/uxstrings.json)](https://alire.ada.dev/crates/uxstrings.html)

## Motivation

My first motivation was to avoid the user of the Ada language from having to make a choice in the representation of character strings.
With the current Ada 2012 standard, the choice must be made according to the nature of the characters handled (Character, Wide\_Character or Wide\_Wide\_Character) and the adaptation of the string size according to the operations carried out.
Moreover, depending on the libraries used, making a single choice is generally not possible, which leads to continuous conversions.

Ada GUI library [Gnoga](https://sourceforge.net/projects/gnoga) internal character strings implementation is based on both Ada types String and Unbounded\_String.
The native Ada String encoding is Latin-1 whereas transactions with the Javascript part are in UTF-8 encoding.

Some drawbacks come up, for instance, with internationalization of programs (see [Localize Gnoga demo](https://sourceforge.net/p/gnoga/code/ci/dev_1.6/tree/demo/localize)):

* several conversions between String and Unbounded\_String objects
* it isn't usable out of Latin-1 character set, characters out of Latin-1 set are blanked
* continuous conversions between Latin-1 and UTF-8, each sent and received transaction between Ada and Javascript parts

Two ways of possible improvement for native Ada String: dynamic length handling and Unicode support.

## Workarounds

First possibility is using UTF-8 as internal implementation in Unbounded\_String objects.
The simplest way but Gnoga uses many times character indexes to parse Javascript messages that is not easy to achieved with UTF-8 which may have several lengths to represent one character. String parsing will be time consuming. Some combinations may lead to incorrect UTF-8 representation.

Second possibility is to use Unbounded\_Wide\_String or Unbounded\_Wide\_Wide\_String.
Using Unbounded\_Wide\_String is quite being in the middle of the river might as well use Unbounded\_Wide\_Wide\_String. In this latter case the memory penalty is heavy for only few accentuated character occurrences. So back to Unbounded\_Wide\_String but you'll miss the so essential emojis ;-)

Third possibility is to make no choice between Latin-1, Wide and Wide\_Wide characters. The object shall adapt its inner implementation to the actual content.
For instance with English language the most often use case will be Latin-1 inner implementation, for French language the most often will be Latin-1 with some exceptions with BMP (Unicode Basic Multilingual Plane) implementation such as in "cœur", for Greek language the most often will be BMP implementation.
The programmer won't make any representation choice when for example receiving UTF-8 messages:

``` ada
   S2 : UXString;
   ...
   S2 := "Received: " & From_UTF_8 (Message);
```

Automatically S2 will adapt its inner representation to the received characters.

## UXStrings packages

Package named [UXStrings](https://github.com/Blady-Com/UXStrings/blob/master/src/uxstrings1.ads) (Unicode Extended Strings) and its [Text_IO](https://github.com/Blady-Com/UXStrings/blob/master/src/uxstrings-text_io1.ads) child package are proposed to bring String enhancements using some Ada 2022 features.

The first part of UXStrings package contains renaming statements of current Ada types.
Ada current String type is structurally an array of Latin-1 characters thus is renamed as Latin\_1\_Character\_Array.
And so on.

The second part defines the USXString type as a tagged private type which has got aspects such as Constant\_Indexing, Variable\_Indexing, Iterable and String_Literal, so we can write:

``` ada
   S1, S2, S3 : UXString;
   C          : Character;
   WC         : Wide_Character;
   WWC        : Wide_Wide_Character;
   ...
   S1 := "était blah blah";
   C   := S1 (3);
   WC  := S1 (2);
   WWC := S1 (1);
   S1 (3) := WWC;
   S1 (2) := WC;
   S1 (1) := C;
   S3  := "une soirée passée à étudier les mathématiques ℕ⊂𝕂...";
   for I in S3 loop
      C   := S3 (I);
      WC  := S3 (I);
      WWC := S3 (I);
      Put_Line (Character'pos (C)'img & Wide_Character'pos (WC)'img & Wide_Wide_Character'pos (WWC)'img);
   end loop;
```

The third part defines conversion functions between UXString and various encoding such as Latin-1, BMP (USC-2), Unicode (USC-4), UTF-8 or UTF-16, so we can write:

``` ada
   S1  := From_Latin_1 ("blah blah");
   S2  := From_BMP ("une soirée passée à étudier la physique ω=Δθ/Δt...");
   S3  := From_Unicode ("une soirée passée à étudier les mathématiques ℕ⊂𝕂...");
   Send (To_UTF_8 (S1) & To_UTF_8 (S3));
```

The fourth part defines various API coming from Unbounded\_String such as Append, "&", Slice, "=", Index and so on.

Note: Iterable is a GNAT specific aspect.

With string lists we can write:

``` ada
   UXSL1 : constant UXStrings.Lists.UXString_List := ["Ada", "Strings", "Wide_Wide_Maps", "Wide_Wide_Constants", "Lower_Case_Map"];
...
   UXS1 : constant UXStrings.UXString := UXSL1.Join ('-').To_Lower;
```

## UXStrings implementations

### UXStrings 1

A first proof of concept implementation is provided. The source code files are ending with the number 1 as for instance "uxstrings1.ads". A GNAT project file "uxstrings.gpr" is provided with variant choice UXS_VARIANT set to UXS1 and some naming conventions for both packages UXStrings and UXStrings.Text\_IO.

#### Implementation choices

UTF-8 encoding is chosen for internal representation. The [Strings_Edit library](http://www.dmitry-kazakov.de/ada/strings_edit.htm) is used for UTF-8 encoding management.
[GNAT.OS_Lib](https://docs.adacore.com/gnat_rm-docs/html/gnat_rm/gnat_rm/the_gnat_library.html#gnat-os-lib-g-os-lib-ads) is chosen for input / output management.

### UXStrings 2

A second proof of concept implementation is provided. The source code files are ending with the number 2 as for instance "uxstrings2.ads". A GNAT project file "uxstrings.gpr" is provided with variant choice UXS_VARIANT set to UXS2 and some naming conventions for both packages UXStrings and UXStrings.Text\_IO.

#### Implementation choices

In addition to implementation UXStrings 1, some API have been added to support ASCII 7 bits encoding. ASCII is a subset of UTF-8 thus no change with the internal representation.
However, the API are now aware if content is full ASCII. On one hand, this permits to access directly to the position of one character without iterating on UTF-8 characters. Thus this is a time improvement when content is full ASCII. On the other hand, when content is changing the API check if the content is full ASCII. Thus this is a time penalty when changes are not full ASCII.

### UXStrings 3

A third proof of concept implementation is provided. The source code files are ending with the number 3 as for instance "uxstrings3.ads". A GNAT project file "uxstrings.gpr" is provided with variant choice UXS_VARIANT set to UXS3 and some naming conventions for both packages UXStrings and UXStrings.Text\_IO.

#### Implementation choices

In addition to implementation UXStrings 1, Unbounded\_Wide\_Wide\_Strings Ada standard package is chosen for internal representation. Characters are stored as Wide\_Wide\_Characters equivalent to Unicode. Memory management is done with the Unbounded capacity.

### UXStrings 4 - **NEW** -

A fourth proof of concept implementation is provided. The source code files are ending with the number 4 as for instance "uxstrings4.ads". A GNAT project file "uxstrings.gpr" is provided with variant choice UXS_VARIANT set to UXS4 and some naming conventions for both packages UXStrings and UXStrings.Text\_IO.

#### Implementation choices

In addition to implementation UXStrings 1, Ada.Containers.Vectors standard generic package is chosen for internal representation. Characters are stored as Wide\_Wide\_Characters equivalent to Unicode. Memory management is done with the container capacity.

### Children units

- UXStrings.Conversions: convenient subprograms to convert String into basic types and vice versa
- UXStrings.Formatting: subprogram formatting integers
- UXStrings.Hash_case_insensitive: compute hash not case sensitive
- UXStrings.Hash: compute hash
- UXStrings.Lists: convenient subprograms to manage string lists - **NEW** -
- UXStrings.Text_IO.Text_Streams: subprogram giving access to text streams
- UXStrings.Text_IO: standard Text_IO subprograms adapted to UXStrings

### Limitations

These implementations which are only for demonstrate the possible usages of UXString have many limitations.

- not thread safe
- single character assignment is not implemented in UXS1, UXS2 and UXS3 but is implemented in UXS4.
- only few API are implemented

### Future implementations

Here are some ideas:

- Use memory management as implemented in XStrings from [GNATColl](https://github.com/AdaCore/gnatcoll-core/blob/master/src/gnatcoll-strings_impl.ads).
- Adapt the inner implementation to the actual content with 8 bits character encodings, 16 bits or 32 bits.

## Tests

One test program [test\_uxstrings1.adb](https://github.com/Blady-Com/UXStrings/blob/master/tests/test_uxstrings1.adb) is provided for UXStrings tests and an other test program [test\_uxstrings\_text\_io.adb](https://github.com/Blady-Com/UXStrings/blob/master/tests/test_uxstrings_text_io.adb) is provided for UXStrings.Text\_IO tests.

## Dependencies

UXStrings library depends on [Strings Edit](http://www.dmitry-kazakov.de/ada/strings_edit.htm) library.
This latter is also part of [Simple Components](http://www.dmitry-kazakov.de/ada/components.htm) framework available on Alire.
Get one of these and add the path of strings\_edit.gpr in your GPR\_PROJECT_PATH before building your program with UXStrings.

## Using Alire

In your own [Alire](https://alire.ada.dev) project, add UXStrings dependency:

`% alr with uxstrings`

UXStrings with variant choice UXS_VARIANT set to UXS4 is default chosen implementation.

You might choose an other implementation 1, 2, 3 or 4 by setting variant choice UXS_VARIANT.

Then you can import the Ada UXStrings packages in your programs.

Note: Alire will take care of dependencies.

## Licence

The provided UXStrings specifications are intend to be public.
Constructive criticism and alternative implementations of these specifications are expected.
The actual proposed implementation is under [CeCILL-C](https://cecill.info) licence.

## Feedbacks

Feel free to send feedback about UXStrings specification source code on [Github](https://github.com/Blady-Com/UXStrings/issues).

Pascal Pignard, April 2021, August 2022, March-October 2023, April 2024.
