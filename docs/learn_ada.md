# Getting Started with Ada
## An Ada quick start guide for programmers

The goal of this guide is not to teach programming, nor to teach everything about the Ada language, but to be a quick and simple guide to those familiar with programming concepts and other languages to quickly start writing or modifying Ada applications. Ada is so simple at its root that most can get rolling in hours and become even be working with the higher level concurrency facilities within days.

## Table of Contents
   * Introduction
      - Reliability Readability Efficiency
      - ISO Standard
      - Benefits
      - Features
   * Getting a Compiler
   * Get an Editor
   * Get a Book
   * Get Started
      - The Environment
      - Hello World
      - Blocks
      - Control Flow
   * Packages and Sub-Programs
      - Packages
      - Child packages
      - Procedures
      - Functions
   * Exceptions
   * Types the Heart of Ada
   * Object Oriented Programing
   * Concurrency
   * The Ada Standard Library
   
   
## Introduction
### Reliability Readability Efficiency

Ada is and easy language to learn yet extremely reliable, readable and efficient. Ada is written in a human readable user friendly way making it exceptional for well maintained and reusable code. It is particular adept at handling massive software projects and for more than 30 years has been proven in real world use to be one of the most robust solutions to mission, security and safety critical software solutions. If you have flown on a Boeing 777, your life was in Ada's hands.

### ISO Standard

The Ada language has an international standard as defined by ISO/IEC 8652:2012 which is freely available to read and is an excellent resource, unlike many standards, for daily use as a reference manual. [On-line Ada Reference Manual](http://www.ada-auth.org/standards/ada12.html)

### Benefits
* Development cost savings
* Maturity of language and implementations
* Freely available international standard
* Interoperability with other languages
* Ease of training programmers
* Successful usage in practice
* Software portability
* Professional grade Open Source compilers

### Features
* Support for “programming in the large”
* Flexibility
* Data Abstraction and Information Hiding
* Reusability
* Concurrency support
* Methodology neutrality
* Real-time support
* Mission and Safety-critical support

## Get a Compiler

Getting setup with an Ada compiler is easy. Go to [GetAdaNow.com](http://GetAdaNow.com) and follow the directions that apply to your operating system or hardware.

## Get an Editor

Some distributions of gcc/ada will include an editor called gps (or gnat-gps). It currently has the best Ada language support with syntactical code completion.

Emacs has exceptional support for Ada as well. Install both Emacs and the Emacs lisp source scripts (most distributions have them separate). Then from with in Emacs use M-x list-packages and choose Ada-Mode. This will install the latest version of Ada mode for Emacs. (Most version of Emacs include some version of emacs).

Any editor that supports TextMate bundles and most code editors support at least code highlighting for Ada.

## Get a Book

While this guide may be enough for some to get started and then use the [On-line Ada Reference Manual](http://www.ada-auth.org/standards/ada12.html) to fill in the missing details. There is an excellent free e-book available for those wanting a more detailed learning experience [Ada Distilled](http://getadanow.com/Ada-Distilled-24-January-2011-Ada-2005-Version.pdf)

If you are new to software development and would like a full text book [Programming in Ada 2012 by John Barnes](http://www.amazon.com/Programming-Ada-2012-John-Barnes/dp/110742481X/) is a good choice.

## Get Started

### The Environment

Make sure that you have gcc/ada on your path:

Type:
```bash
gnatmake --version
```

This will return the version of gcc/ada. If gnatmake is not on your path
then you will need to add it.

### Hello World

gcc/ada uses a familiar file based scheme of organizing your source code.

In Ada code usually has two parts, a specification (a.k.a a spec) and a body.

For a simple Hello World program we will not be creating a spec file, but when used the default extension is .ads and the body file extension is .adb. In Ada specs are part of the program not just forward headers for code as in some languages.

An Ada program (unless a library) will have a main procedure and a number of "package"s that are libraries of code "with"ed in.

Comments in Ada are "inline" only and start with a double dash "--".

Copy the following code in to a file hello.adb:

``` ada
with Ada.Text_IO;
--  "with" loads the package (in this case a standard library) called
--  Ada.Text_IO and its parent package Ada. Note that there is no way
-- to "with" in all sub-packages as in some languages.

procedure Hello is
begin
   Ada.Text_IO.Put_Line ("Hello World");
   --  Statements in Ada are terminated with semicolons.
   --  Note that the procedure Put_Line is called from the Text_IO package
   --  and to do so required fully qualifying where the procedure was located.
end Hello;
```

Then run
```
gnatmake hello
```

That will create an executable "hello" (or hello.exe on Windows).

After trying out your new program (run ./hello or hello on Windows), you
can clean up the build files and delete the executable by running:

```
gnatclean hello
```

### Blocks

Ada code is built by nesting blocks of code, for example you could nest a procedure inside another procedure inside a package inside another package. Ada was designed with "programing in the large" from the start to handle even millions of lines of code.

The basic structure of blocks are a declarative section and a sequence of statements:

``` ada
declare
   --  declarations of variables or additional "declarative blocks"
begin
   --  sequence of simple statements or "block statements"
end;
```

Other types of blocks include "procedure"s:

``` ada
procedure Name (param : in out Type) is
   --  declarations of variables or additional "declarative blocks"
begin
   --  sequence of simple statements or "block statements"
end Name;
```

and "function"s:

``` ada
function Name (param : in Type) return Type is
   --  declarations of variables or additional "declarative blocks"
begin
   --  sequence of simple statements or "block statements"
end Name;
```

The highest level block is called a "package":

``` ada
package Name is
   --  declarations of variables or additional "declarative blocks"
end Name;
package body Name is
   --  declarations of variables or additional "declarative blocks"
begin
   --  sequence of simple statements or "block statements"
end Name;
```

"declare" above defines a "block statement", other examples of block statements include loops, case statements and if then blocks. The other blocks described are "declarative blocks".

Here is an example of a main procedure with lots of nested blocks. Most often packages are at the "library level", the most outer blocks, but they too can be nested.

``` ada
with Ada.Text_IO;

procedure Blocks is
   package Test is
      procedure My_Test;
   end Test;

   package body Test is
      procedure My_Test is
         use Ada.Text_IO;
         --  A use clause removes the need to fully qualify the location
         --  of package members.
      begin
         Put_Line ("Hello from Blocks.My_Test.Test");
      end My_Test;
   end Test;
begin
   declare
      X : Integer := 10;
      --  Declarations in Ada of variables/objects are Name followed by :
      --  followed by the Type. Optionally as here an initial value is set
      --  using the assignment operator :=
   begin
      declare
         X : Integer := 11;
         --  Note that since this X is in an inner block it hides the X
         --  X of the outer block.
      begin
         Ada.Text_IO.Put_Line (Integer'Image (X));
         --  Types in Ada have attributes that can be access using the type
         --  name ' attribute. In this case it has an attribute that can be
         --  used to return a string representation of a numeric type called
         --  Image. Variables and Objects also have some attributes as well.
      end;

      Ada.Text_IO.Put_Line (Integer'Image (X));
   end;

   Test.My_Test;
end Blocks;
```

### Control Flow

Ada has a full collection of control flow block statements similar in name and use as other languages.

**If .. then**

``` ada
if expression then
  statement;
elsif expression then
  statement;
else
  statement;
end if;
```

The common operators for building an expression in Ada are:

|    Operator Category    |      Operators      |
| ----------------------- | ------------------- |
| logical                 | and, or, xor        |
| relational              | =, /=, <, >, <=, >= |
| binary                  | +, -, &             |
| unary                   | +, -                |
| multiplying             | *, /, mod, rem      |
| high precedence         | **, abs, not        |

/= is not equal
&  is concatenate 

**Switch cases**

``` ada
case expression is
  when value =>
     statement;
  when others =>
     statement;
end case;
```

Note that in Ada for case statements there must be full coverage for the expression result. So unless every member of the result set of expression has a "when value => statement" there must be a "when others => statement" or the compiler will return an error.

for case expression can also be ranges:

``` ada
case expression is
  when 1 .. 4    =>
     statement;
  when 5 | 6 | 7 => null;
end case;
```

**Loops**

``` ada
loop
  statement;
  exit when expression;
end loop;
```

**While loops**

``` ada
while expression loop
  exit when expression2;
end loop;
```

**For loops**

``` ada
for some_variable in 1 .. 10 loop
  statement;
end loop;
```

The count of a for loop can be reversed using:

``` ada
for some_variable in reverse 1 .. 10 loop
   statement;
end loop;
```

It is also possible to use an expression that evaluates to a range or attribute that evaluates to a range for the range.

``` ada
for some_variable in Some_Object'Range loop
   statement;
end loop;
```

**Labeled loops**

Labels can be give to loops of any type to allow exiting from nested loops.

``` ada
Top_Loop:
   while true loop
      for c in Buffer'Range loop
         exit when Buffer (C) = some_value;
         exit Top_Loop when Buffer (c) = another_value;
      end loop;
   end loop;
```

**Goto**

Labels also exist to allow for "goto"s:

```ada
some_label:
    goto some_labe;
```
