# Fourmilab Unum

![Fourmilab Unum](figures/unum.png)

Fourmilab Unum a stand-alone utility program written in portable Perl 
which allows you to look up Unicode and HTML characters by name or 
number, and interconvert numbers in decimal, hexadecimal, and octal 
bases.  It contains a complete Unicode character database and allows
query by code point, character block, or name.  A separate database
of all HTML named character entities, including alternative names and
entities composed of multiple Unicode code points, allows lookup by
name or code.  For complete documentation see the
[Unum home page](https://www.fourmilab.ch/webtools/unum/) at
[Fourmilab](https://www.fourmilab.ch/),
a copy of which is included in this repository.

All of this software is licensed under the Creative Commons 
Attribution-ShareAlike license.  Please see
[LICENSE.md](LICENSE.md) in this repository for details.

<hr />

# NAME

unum - Interconvert numbers, Unicode, and HTML/XHTML characters

# SYNOPSIS

**unum** _argument..._

# DESCRIPTION

The **unum** program is a command line utility which allows you to
convert decimal, octal, hexadecimal, and binary numbers; Unicode
character and block names; and HTML/XHTML character reference names and
numbers into one another.  It can be used as an on-line special
character reference for Web authors.

## Arguments

The command line may contain any number of the following
forms of _argument_.

- 123

    Decimal number.

- 0371

    Octal number preceded by a zero.

- 0x1D351

    Hexadecimal number preceded by `0x`.  Letters may be upper or
    lower case, but the `x` must be lower case.

- 0b110101

    Binary number.

- b=_block_

    Unicode character blocks matching _block_ are listed.
    The _block_ specification may be a regular expression.
    For example, `b=greek` lists all Greek character blocks
    in Unicode.

- c=_char_...

    The Unicode character codes for the characters _char_... are listed.
    If the first character is not a decimal digit and the second not an
    equal sign, the `c=` may be omitted.

- h=_entity_

    List all HTML/XHTML character references matching _entity_, which may
    be a regular expression.  Matching is case-insensitive, so `h=alpha`
    finds both `&Alpha;` and `&alpha;`.  If the reference is composed of
    multiple Unicode code points, the components are printed after the name
    of the composed character reference.

- '&amp;#_number_;&amp;#x_hexnum_;...'

    List the characters corresponding to the specified HTML/XHTML
    character entities, which may be given in either decimal or
    hexadecimal.  Note that the "x" in XHTML entities must be lower case.
    On most Unix-like operating systems, you'll need to quote the argument
    so the ampersand, octothorpe, and semicolon aren't interpreted by the
    shell.

- l=_block_

    List all Unicode blocks matching _block_ and all characters
    within each block; `l=goth` lists the `Gothic` block
    and the 32 characters it contains.

- n=_name_

    List all Unicode character whose names match _name_, which may be
    a regular expression.  For example, `n=telephone` finds the twelve
    Unicode characters for telephone symbols.

- utf8=_number_

    Treating the number (which may be specified as either decimal,
    octal, hexadecimal, or binary, as for numeric arguments) as a
    stream of from one to four bytes, decode the bytes as the
    UTF-8 representation of a character.  For example, the
    specification "utf8=0xE298A2" decodes to Unicode code
    point 0x2622, the radioactive sign.

## Options

- --nent

    When showing an HTML character reference, always show its numerical 
    form (for example, &amp;#8212;), even if it has a named character 
    reference.

- --utf8

    Show UTF-8 encoding of characters as a byte sequence in a
    hexadecimal number.  This is the same format as is accepted
    by the utf8= argument.  The option applies to the display of
    all arguments which follow on the command line.

## Output

For number or character arguments, the value(s) are listed in
all of the input formats, save binary.

```
Octal  Decimal      Hex        HTML    Character   Unicode
  056       46     0x2E    &period;    "."         FULL STOP
```

If the terminal font cannot display the character being listed,
the "Character" field will contain whatever default is shown in
such circumstances.  Control characters are shown as a Perl
hexadecimal escape.  If multiple HTML named character references
map to the same Unicode code point, all are shown separated by
commas.

Unicode blocks are listed as follows:

```
  Start        End  Unicode Block
 U+2460 -   U+24FF  Enclosed Alphanumerics
U+1D400 -  U+1D7FF  Mathematical Alphanumeric Symbols
```
