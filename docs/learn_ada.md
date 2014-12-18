# Getting Started with Ada
## An Ada quick start guide for programmers

(c) 2014 David Botton
    Permission is granted to copy, distribute and/or modify this document
    under the terms of the GNU Free Documentation License, Version 1.3
    or any later version published by the Free Software Foundation;
    with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.
    A copy of the license is included in the section entitled "GNU
    Free Documentation License".

The goal of this guide is _not_ to teach programming, _nor_ to teach everything about the Ada language, but to be a **quick and simple guide** to those familiar with programming concepts and other languages to quickly start writing or modifying Ada applications. Ada is so simple at its root that most can get rolling in hours and even be working with the higher level concurrency facilities. After this guide check the learning resources on [GetAdaNow.com](http://getadanow.com/#step2) to go to the next level.

* * *

### Table of Contents
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
      - Packages and files
      - Package privacy
      - Sub-Programs (procedures and functions)
   * Types the Heart and Soul of Ada
   * Exceptions
   * Object Oriented Programing
   * Generics
      - How to use generics
   * Concurrency
      - Protected Objects
      - Tasks
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
```
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

Copy the following code in to a file called hello.adb:

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

Copy the following code in to a file called blocks.adb:

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

``` ada
some_label:
    goto some_label;
```

## Packages and Sub-Programs

### Packages and Files

Outside of the main procedure, in Ada code, the most outer block is a Package. Packages are divided into two sections as described before. When packages are the outer most blocks in gcc/ada they are contained in files, the specification portion is contained in a file with the .ads extension and a body portion which is contained in a file with the .adb extension.

The following spec should be saved in a file called talk.ads

``` ada
package Talk is
   procedure Say (S : in String);
end Talk;
```

This body then goes in talk.adb

``` ada
with Ada.Text_IO;

package body Talk is
   procedure Say (S : in String) is
      use Ada.Text_IO;
   begin
      Put_Line (S);
   end Say;
end Talk;
```

To use the Talk package we will need a main procedure, place the following in say_sello.adb

``` ada
with Talk;

procedure Say_Hello is
begin
   Talk.Say ("Hello World!");
end Say_Hello;
```

It is also possible to have child packages. For gcc-ada child package file names are the parent "-" the child name dot extension. So for Talk.Double, use talk-double.ads for the spec.

``` ada
package Talk.Double is
   procedure Say_More (S : in String);
end Talk.Double;
```

For the body use talk-double.adb

``` ada
package body Talk.Double is
   procedure Say_More (S : in String) is
   begin
      Talk.Say (S);
      Say (S);
      --  Since Talk.Double is a child of Talk, there is no need to specify the
      --  parent package Talk.
   end Say_More;
end Talk.Double;
```

He is a main procedure using the parent and child packages.

``` ada
with Talk.Double;

procedure Say_Hello2 is
begin
   Talk.Say ("Hello World!");
   --  Since the child of Talk, Talk.Double was "with"ed the parent package
   --  is automatically "with"ed.

   Talk.Double.Say_More ("Hello again.");
end Say_Hello2;
```

### Package privacy

Packages can contain a private section that is accessible only to the body of the package or it's child packages. However anything declared in the body of a
parent package is not visible to child packages. For example:

talk2.ads

``` ada
package Talk2 is
   procedure Say (S : in String);
private
   Talk_Count : Natural := 0;
end Talk2;
```

talk2.adb

``` ada
with Ada.Text_IO;

package body Talk2 is
   Say_Count : Natural := 0;

   procedure Say (S : in String) is
      use Ada.Text_IO;
   begin
      Say_Count  := Say_Count + 1;
      Talk_Count := Talk_Count + 1;

      Put_Line (Natural'Image (Talk_Count) & " : " & S);
      Put_Line ("Say was called" & Natural'Image (Say_Count) & " times.");
      --  In Ada '&' is used to concatinate strings
   end Say;
```

talk2-double.ads

``` ada
package Talk2.Double is
   procedure Say_More (S : in String);
end Talk2.Double;
```

talk2-double.adb

``` ada
package body Talk2.Double is
   procedure Say_More (S : in String) is
   begin
      Talk2.Say (S);

      Talk_Count := Talk_Count - 1;
      --  Talk_Count is "private" in the Talk2 package and so accessible
      --  to child packages. Say_Count is in the body of Talk2 and so
      --  private even to child packages.

      Say (S);
   end Say_More;
end Talk2.Double;
```

say_hello3.adb

``` ada
with Talk2.Double;

procedure Say_Hello3 is
begin
   Talk2.Say ("Hello World!");

   Talk2.Double.Say_More ("Hello again.");
end Say_Hello3;
```

### Sub-Programs (procedures and functions)

In Ada there are two types of subprograms, procedures and functions. Procedures have not return value.

The syntax for declaring a procedure is:

``` ada
procedure My_Procedure (Param_Name : in out Param_Type);
```

The "in out" portion of the declaration is the "mode" and declares if the parameter contains data coming in to the procedure, out of the procedure or both.

An "in" mode parameter cant be a constant, but out and in out need to be variables since they will contain any changes assigned to them in the procedure.

If no "mode" is given the parameter is an "in" parameter.

The implementation of the procedure is:

``` ada
procedure My_Procedure (Param_Name : in out Param_Type) is
begin
end My_Procedure;
```

Functions are declared in the same way but contain a return type:

``` ada
function My_Function (Param_Name : in Param_Type) return Return_Type;
```

The implementation of that function would be:


``` ada
function My_function (Param_Name : in Param_Type) return Return_Type is
begin
   return Some_Value_of_Return_Type;
end My_Function
```

Invoking the above would be:

``` ada
declare
   X : Param_Type;
   Y : Param_Type;
begin
   My_Procecure (X);
   Y := My_Function (X);
end;
```

In Ada you can also use invoke  using named parameters:

``` ada
declare
   X : Param_Type;
   Y : Param_Type;
begin
   My_Procecure (Param_Name => X);
   Y := My_Function (Param_Name => X);
end;
```

## Types the Heart and Soul of Ada

### Introduction

One of the most important features of Ada is type safety through both strict static and dynamic typing. Strict typing insures that values are in bound and helps insure that the intet of the programer is fulfilled in the way it was intended.
