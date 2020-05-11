# Unicode Extended Strings (UXStrings)

[Gnoga](https://sourceforge.net/projects/gnoga) internal character strings implementation is based on both Ada types String and Unbounded\_String.
The native Ada String encoding is Latin-1 whereas transactions with the Javascript part are in UTF-8 encoding.

Some drawbacks come up, for instance,  with internationalization of programs (see [Localize Gnoga demo](https://sourceforge.net/p/gnoga/code/ci/dev_1.6/tree/demo/localize)):

* several conversions between String and Unbounded\_String objects
* it isn't usable out of Latin-1 character set, characters out of Latin-1 set are blanked
* continuous conversions between Latin-1 and UTF-8, each sent and received transaction between Ada and Javascript parts

Two ways of improvement: native dynamic length handling and Unicode support.

First possibility is using UTF-8 as internal implementation in Unbounded\_String objects.
The simplest way but Gnoga uses many times character indexes to parse Javascript messages that is not easy to achieved with UTF-8 which may have several lengths to represent one character. String parsing will be time consuming. Some combinations may lead to incorrect UTF-8 representation.

Second possibility is to use Unbounded\_Wide\_String or Unbounded\_Wide\_Wide\_String.
Using Unbounded\_Wide\_String is quite being in the middle of the river might as well use Unbounded\_Wide\_Wide\_String. In this latter case the memory penalty is heavy for only few accentuated character occurrences. So back to Unbounded\_Wide\_String but you'll miss the so essential emojis ;-)

Third possibility is to make no choice between Latin-1, Wide and Wide\_Wide characters. The object shall adapt its inner implementation to the actual content.
For instance with English language the most often use case will be Latin-1 inner implementation, for French language the most often will be Latin-1 with some exceptions with BMP (Unicode Basic Multilingual Plane) implementation such as in "cœur", for Greek language the most often will be BMP implementation.
The programer won't make any representation choice when for example receiving UTF-8 messages:

``` ada
   S2 : UXString;
   ...
   S2 := "Received: " & From_UTF8 (Message);
```

Automatically S2 will adapt its inner representation to the received characters.

Package named UXStrings (Unicode Extended String) is containing :

The first part contains renaming statements of Ada types.
Ada String type is structurally an array of Latin-1 characters thus is renamed as Latin\_1\_Character\_Array.
And so on.

The second part defines the USXString type as a tagged private type which has got aspects such as Constant\_Indexing, Variable\_Indexing and Iterable, so we can write:

``` ada
   S1, S2, S3 : UXString;
   C          : Character;
   WC         : Wide_Character;
   WWC        : Wide_Wide_Character;
   ...
   C   := S1 (3);
   WC  := S1 (2);
   WWC := S1 (1);
   S1 (3) := WWC;
   S1 (2) := WC;
   S1 (1) := C;
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

The private and implementation parts are not yet defined.
One idea is to use the XStrings from [GNATColl](https://github.com/AdaCore/gnatcoll-core/blob/master/src/gnatcoll-strings_impl.ads).

Feel free to send feedback about UXStrings specification source code on [Gnoga mailing list](https://sourceforge.net/p/gnoga/mailman/gnoga-list).

Pascal Pignard, May 2020.
