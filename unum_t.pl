#! /usr/bin/perl -CA
#                ^^^   This allows Unicode command-line arguments to be
#                      accepted if the underlying system supports it.
#                      If it causes an error, your version of Perl does
#                      not support this feature.  You can remove the option
#                      and continue to use the program with all other forms
#                      of arguments.

        use utf8;

=head1 NAME

unum - Interconvert numbers, Unicode, and HTML/XHTML characters

=head1 SYNOPSIS

B<unum> I<argument...>

=head1 DESCRIPTION

The B<unum> program is a command line utility which allows you to
convert decimal, octal, hexadecimal, and binary numbers; Unicode
character and block names; and HTML/XHTML character reference names and
numbers into one another.  It can be used as an on-line special
character reference for Web authors.

=head2 Arguments

The command line may contain any number of the following
forms of I<argument>.

=over 10

=item S<>123

Decimal number.

=item S<>0371

Octal number preceded by a zero.

=item S<>0x1D351

Hexadecimal number preceded by C<0x>.  Letters may be upper or
lower case, but the C<x> must be lower case.

=item S<>0b110101

Binary number.

=item b=I<block>

Unicode character blocks matching I<block> are listed.
The I<block> specification may be a regular expression.
For example, C<b=greek> lists all Greek character blocks
in Unicode.

=item c=I<char>...

The Unicode character codes for the characters I<char>... are listed.
If the first character is not a decimal digit and the second not an
equal sign, the C<c=> may be omitted.

=item h=I<entity>

List all HTML/XHTML character references matching I<entity>, which may
be a regular expression.  Matching is case-insensitive, so C<h=alpha>
finds both C<&Alpha;> and C<&alpha;>.  If the reference is composed of
multiple Unicode code points, the components are printed after the name
of the composed character reference.

=item '&#I<number>;&#xI<hexnum>;...'

List the characters corresponding to the specified HTML/XHTML
character entities, which may be given in either decimal or
hexadecimal.  Note that the "x" in XHTML entities must be lower case.
On most Unix-like operating systems, you'll need to quote the argument
so the ampersand, octothorpe, and semicolon aren't interpreted by the
shell.

=item l=I<block>

List all Unicode blocks matching I<block> and all characters
within each block; C<l=goth> lists the C<Gothic> block
and the 32 characters it contains.

=item n=I<name>

List all Unicode character whose names match I<name>, which may be
a regular expression.  For example, C<n=telephone> finds the twelve
Unicode characters for telephone symbols.

=item utf8=I<number>

Treating the number (which may be specified as either decimal,
octal, hexadecimal, or binary, as for numeric arguments) as a
stream of from one to four bytes, decode the bytes as the
UTF-8 representation of a character.  For example, the
specification "utf8=0xE298A2" decodes to Unicode code
point 0x2622, the radioactive sign.

=back

=head2 Options

=over 10

=item --nent

When showing an HTML character reference, always show its numerical 
form (for example, &#8212;), even if it has a named character 
reference.

=item --utf8

Show UTF-8 encoding of characters as a byte sequence in a
hexadecimal number.  This is the same format as is accepted
by the utf8= argument.  The option applies to the display of
all arguments which follow on the command line.

=back

=head2 Output

For number or character arguments, the value(s) are listed in
all of the input formats, save binary.

   Octal  Decimal      Hex        HTML    Character   Unicode
     056       46     0x2E    &period;    "."         FULL STOP

If the terminal font cannot display the character being listed,
the "Character" field will contain whatever default is shown in
such circumstances.  Control characters are shown as a Perl
hexadecimal escape.  If multiple HTML named character references
map to the same Unicode code point, all are shown separated by
commas.

Unicode blocks are listed as follows:

    Start        End  Unicode Block
   U+2460 -   U+24FF  Enclosed Alphanumerics
  U+1D400 -  U+1D7FF  Mathematical Alphanumeric Symbols


=head1 VERSION

This is B<unum> version 3.4-14.0.0, released on September 20th, 2021.
The current version of this program is always posted at
http://www.fourmilab.ch/webtools/unum/.

=head1 AUTHOR

John Walker

http://www.fourmilab.ch/

=head1 BUGS

Specification of Unicode characters on the command line requires
an operating system and shell which support that feature and a
version of Perl with the B<-CA> command line option
(v5.8.5 has it, but v5.8.0 does not; I don't know in which
intermediate release it was introduced).  If your version of
Perl does not implement this switch, you'll have to remove it
from the C<#!> statement at the top of the program, and Unicode
characters on the command line will not be interpreted correctly.

If you specify a regular expression, be sure to quote the argument
if it contains any characters the shell would otherwise interpret.

If you run B<perldoc> on the compressed version of the program,
a large amount of gibberish will be displayed after the end of the
embedded documentation.  B<perldoc> gets confused by sequences in
the compressed data table and tries to interpret it as documentation.
This doesn't happen with the uncompressed version.

Please report any bugs to bugs@fourmilab.ch.

=head1 COPYRIGHT

This is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut

    use strict;
    use warnings;

    sub usage {
        print << "EOD";
usage: unum arg...
    Arguments:
        147               Decimal number
        0371              Octal number
        0xfa75            Hexadecimal number (letters may be A-F or a-f)
        0b11010011        Binary number
        '&#8747;&#x3c0;'  One or more XHTML numeric entities (hex or decimal)
        utf8=0xc397       Character encoded as UTF-8 byte stream (any number format)
        xyz               The characters xyz (non-digit)
        c=7Y              The characters 7Y (any Unicode characters)
        b=cherokee        List Unicode blocks containing "CHEROKEE" (b=. to list all)
        h=alpha           List XHTML entities containing "alpha" (h=. to list all)
        n=aggravation     Unicode characters with "AGGRAVATION" in the name
        n=^greek.*rho     Unicode characters beginning with "GREEK" and containing "RHO"
        l=gothic          List all characters in matching Unicode blocks

    Options:
        --nent            Always show HTML character entities as numeric
        --utf8            Show UTF-8 encoding of characters

        All name queries are case-insensitive and accept regular
        expressions.  Be sure to quote regular expressions if they
        contain characters with meaning to the shell.

        Run perldoc on this program or visit:
            http://www.fourmilab.ch/webtools/unum/
        for additional information.
    Version 3.4-14.0.0, September 2021
EOD
    }

    my $debug_decompress = 0;           # Debug code point table decompression?

    my (@HTML_CHARACTER_REFERENCES, @HTML_COMPOSED_CHARACTER_REFERENCES,
        %UNICODE_NAMES, @UNICODE_BLOCKS);

    binmode(STDOUT, ":utf8");

    if ($#ARGV < 0) {
        usage();
        exit(0);
    }

    init_names();

    my $utf8o = 0;                      # Show UTF-8 encoding ?
    my $numHTMLent = 0;                 # Always generate numeric HTML entities ?

    my ($chartitle, $blocktitle) = (0, 0);
    my $arg = 0;
    while ($#ARGV >= 0) {
        my $n = shift();

        $arg++;

        #   Process options
        if ($n eq "--utf8") {           # --utf8  Show UTF-8 encoding
            $utf8o = 1;
            next;
        } elsif ($n eq "--nent") {      # --nent  Always generate numeric HTML entities
            $numHTMLent = 1;
            next;
        }
=begin test_UTF8
        elsif ($n eq "--test8") {
            test_UTF8();
            next;
        }
=end test_UTF8
=cut

        if ($n =~ m/^\d/) {

            #   Number                  List numeric and character representations

            #   Argument is a number: use oct() to convert to binary
            $n = oct($n) if ($n =~ m/^0/);

        } elsif ($n =~ m/^(b|l)=(.+)/) {

            #   b=<block name>          List Unicode blocks matching name

            my $bl = $1;
            my $cpat = qr/$2/i;
            my $listall = $bl =~ m/l/i;

            my $k;
            for $k (@UNICODE_BLOCKS) {
                if ($k->[2] =~ m/$cpat/) {
                    if (!$blocktitle) {
                        $chartitle = 0;
                        $blocktitle = 1;
                        print("   Start        End  Unicode Block\n");
                    }
                    printf("%8s - %8s  %s\n",
                        sprintf("U+%04X", $k->[0]),
                        sprintf("U+%04X", $k->[1]),
                        $k->[2]);

                    if ($listall) {
                        for (my $i = $k->[0]; $i <= $k->[1]; $i++) {
                            showchar($i);
                        }
                    }
                }
            }
            next;

        } elsif ($n =~ m/^h=(.+)/) {

            #   h=<character name>      List HTML character entities matching name

            my $cpat = qr/$1/i;

            #   Scan through the table of names and build a hash of all
            #   the code points that matches map to.  Then sort those
            #   code points in ascending order and display them,
            #   counting on showchar() to show all of the character
            #   reference names which mapped from the code points
            #   displayed.

            my %htmlCodePoints;
            for (my $i = 0; $i < scalar(@HTML_CHARACTER_REFERENCES); $i += 2) {
                if ($HTML_CHARACTER_REFERENCES[$i] =~ m/$cpat/) {
                    $htmlCodePoints{$HTML_CHARACTER_REFERENCES[$i + 1]} = 1;
                }
            }

            my $k;
            for $k (sort {$a <=> $b} keys(%htmlCodePoints)) {
                showchar($k);
            }

            #   Now we must scan through the table of composed character
            #   references.  These are logical characters which are made
            #   up by combining multiple code points.

            for (my $i = 0; $i < scalar(@HTML_COMPOSED_CHARACTER_REFERENCES); $i += 2) {
                if ($HTML_COMPOSED_CHARACTER_REFERENCES[$i] =~ m/$cpat/) {
                    my $hcp = $HTML_COMPOSED_CHARACTER_REFERENCES[$i + 1];
                    print("                                &$HTML_COMPOSED_CHARACTER_REFERENCES[$i]; =\n");
                    $chartitle = 0;
                    while ($hcp =~ s/\s*(\d+)(?:,\s*)?//) {
                        $k = $1;
                        showchar($k);
                    }
                    $chartitle = 0;
                }
            }
            next;

        } elsif ($n =~ m/^n=(.+)/) {

            #   n=<character name>      List Unicode characters matching name

            my $cpat = qr/$1/i;

            #   The following would be faster if we selected matching
            #   characters into an auxiliary array and then sorted
            #   the selected ones before printing.  In fact, the time it
            #   takes to sort the entire list is less than that consumed
            #   in init_names() loading it, so there's little point bothering
            #   with this refinement.
            my $k;
            for $k (sort {oct("0x$a") <=> oct("0x$b")} keys(%UNICODE_NAMES)) {
                if ($UNICODE_NAMES{$k} =~ m/$cpat/) {
                    showchar(oct("0x$k"));
                }
            }
            next;

        } elsif ($n =~ m/^utf8=(.+)/) {

            #   utf8=<number>           UTF-8 character encoded as number

            my $u = $1;
            #   Argument is a number: use oct() to convert to binary if leading 0
            $u = oct($u) if ($u =~ m/^0/);

            $n = decode_UTF8($u);

        } elsif ($n =~ m/^&#/) {

            #   '&#NNN;&#xNNNN;...'     One or more XHTML numeric entities

            my @htmlent;
            while ($n =~ s/&#(x?[0-9A-Fa-f]+);//) {
                my $hch = $1;
                $hch =~ s/^x/0x/;
                push(@htmlent, $hch);
            }
            unshift(@ARGV, @htmlent);
            next;

        } else {

            #   =<char>... or c=<char>...   List code for one or more characters

            #   If argument is an equal sign followed by a single
            #   character, take the second character as the argument.
            #   This allows treating digits as characters to be looked
            #   up.
            $n =~ s/^c?=(.+)$/$1/i;

            while ($n =~ s/^(.)//) {
                showchar(ord($1));
            }
            next;
        }

        showchar($n);
    }

    #   Show a numeric code in all its manifestations

    sub showchar {
        my ($n) = @_;


        my $ch = ((($n >= 32) && ($n < 128)) || ($n > 160)) ?
            chr($n) :
            sprintf("\\x{%X}", $n);

        #   Determine the Unicode character code as best we can

        my $u = uname($n);
        if (!defined($u)) {
            $u = ublock($n);
            if (defined($u)) {
                $u = sprintf("%s U+%05X", $u, $n);
            } else {
                $u = sprintf("Undefined U+%05X", $n);
            }
        }

        my $ut8l = "";
        if ($utf8o) {
            $ut8l = "       UTF-8  ";
        }

        if (!$chartitle) {
            $blocktitle = 0;
            $chartitle = 1;
            print("   Octal  Decimal      Hex        HTML$ut8l    Character   Unicode\n");
        }

        #   With the advent of HTML5, (aka, W3C meets crap sandwich), the mapping
        #   of named character references to Unicode code points is many-to-many.
        #   If there is more than one character reference name for the given
        #   code point, list all of them separated by commas.  They are listed
        #   in the vaguely alphabetical order given in the W3C table.  We only
        #   display direct mappings of code points to named character references,
        #   not composed character references of which the code point is a
        #   part.

        my $htmlcr;
        if (!$numHTMLent) {
            for (my $i = 0; $i < scalar(@HTML_CHARACTER_REFERENCES); $i += 2) {
                if ($HTML_CHARACTER_REFERENCES[$i + 1] == $n) {
                    if ($htmlcr) {
                        $htmlcr .= ",";
                    }
                    $htmlcr .= "&" . $HTML_CHARACTER_REFERENCES[$i] . ";";
                }
            }
        }
        if (!$htmlcr) {
            $htmlcr = sprintf("&#%d;", $n);
        }

        my $u8e = "";
        if ($utf8o) {
            $u8e = sprintf("  %10s  ", sprintf("0x%X", encode_UTF8($n)));
        }

        printf("%8s %8d %8s %11s%s    %-8s    %s\n",
            sprintf("0%lo", $n),
            $n,
            sprintf("0x%X", $n),
            $htmlcr,
            $u8e,
            sprintf("\"%s\"", $ch),
            $u);
    }

    #   Decode a stream of bytes, stored in an integer, into a
    #   single UTF-8 character.  Leading zero bytes are
    #   ignored.  The following validations are performed
    #   and warning messages issued in case of violations of
    #   the UTF-8 standard.
    #
    #       1.  No extraneous bytes following UTF-8 character
    #       2.  No continuation code in first byte
    #       3.  All continuation bytes have 0b10 as the two
    #           highest bits
    #       4.  No bytes forbidden or undefined in UTF-8
    #           (0xC0, 0xC1, 0xF5-0xFF)
    #       5.  No "overlong" encoding of code points into
    #           more bytes than necessary.
    #
    #   The code point of the UTF-8 character is returned as
    #   an integer.
    #
    #   Test cases:
    #           0x0                 NULL
    #           0x4B                LATIN CAPITAL LETTER K
    #           0xC397              MULTIPLICATION SIGN
    #           0xE298A2            RADIOACTIVE SIGN
    #           0xF09F918C          OK HAND SIGN

    sub decode_UTF8 {

        my ($u) = @_;

        #   Now we run the gauntlet of our very exacting UTF-8
        #   decoder.  Many UTF-8 decoders are tolerant of
        #   sloppiness, but we are not.  That would compromise
        #   our mission of accepting only well-formed input and
        #   diagnosing errors.

        my $err = 0;
        my $n;
        my @bytes;
        my $m = 0xFF000000;
        for (my $i = 0; $i < 4; $i++) {
            my $b = ($u & $m) >> (8 * (3 - $i));
            if (($b != 0) || ($i == 3)) {
                push(@bytes, ($u & $m) >> (8 * (3 - $i)));
            }
            $m = $m >> 8;
        }

        #   Verify that the first byte is not a continuation
        #   code.
        if (($bytes[0] & 0b1100_0000) == 0b1000_0000) {
            printf("First byte is a continuation code " .
                   "in UTF-8 code 0x%X\n", $u);
            $err++;
        }

        #   If there is more than a single byte of UTF-8
        #   code, validate that all continuation bytes
        #   have the correct 0b10xx_xxxx high bits.
        if (scalar(@bytes) > 1) {
            for (my $i = 1; $i < scalar(@bytes); $i++) {
                if (($bytes[$i] & 0b1100_0000) != 0b1000_0000) {
                    printf("Incorrect continuation code in byte $i " .
                           "of UTF-8 code 0x%X\n", $u);
                    $err++;
                }
            }
        }

        #   Verify that no byte contains a value forbidden in
        #   a valid UTF-8 stream.
        for (my $i = 0; $i < scalar(@bytes); $i++) {
            my $b = $bytes[$i];
            if (($b == 0xC0) || ($b == 0xC1) ||
                ($b >= 0xF5)) {
                printf("Byte $i contains invalid UTF-8 code 0x%X\n", $b);
                $err++;
            }
        }

        #   Bail out on gross encoding errors.  This avoids blundering
        #   into undefined variable references and other horrors in
        #   the following decoder.
        if ($err > 0) {
            printf("Erroneous UTF-8 encoding: returning code point 0\n");
            @bytes = ( 0 );
        }

        #   Decode the bytes according to the length specified
        #   by the high-order bits in the first byte.

        if (($bytes[0] & 0b1000_0000) == 0) {                   # Code points 0000 - 007F
            $n = $bytes[0];
            if (scalar(@bytes) > 1) {
                printf("Excess byte(s) in UTF-8 code 0x%X: 1 byte expected, %d specified\n",
                       $u, scalar(@bytes));
            }

        } elsif (($bytes[0] & 0b1110_0000) == 0b1100_0000) {    # Code points 0080 - 07FF
            $n = (($bytes[0] & 0b1_1111) << 6) | ($bytes[1] & 0b11_1111);
            if (($bytes[0] & 0b1_1111) == 0) {
                printf("Overlong 2 byte UTF-8 code 0x%X for code point 0x%X\n", $u, $n);
            }
            if (scalar(@bytes) > 2) {
                printf("Excess byte(s) in UTF-8 code 0x%X: 2 bytes expected, %d specified\n",
                       $u, scalar(@bytes));
            }

        } elsif (($bytes[0] & 0b1111_0000) == 0b1110_0000) {    # Code points 0800 - 0FFF
            $n = (($bytes[0] & 0b1111) << 12) |
                 (($bytes[1] & 0b11_1111) << 6) |
                  ($bytes[2] & 0b11_1111);
            if ((($bytes[0] & 0b1111) == 0) &&
                (($bytes[1] & 0b1000_0000) == 0)) {
                printf("Overlong 3 byte UTF-8 code 0x%X for code point 0x%X\n", $u, $n);
            }
            if (scalar(@bytes) > 3) {
                printf("Excess byte(s) in UTF-8 code 0x%X: 3 bytes expected, %d specified\n",
                       $u, scalar(@bytes));
            }

        } elsif (($bytes[0] & 0b1111_1000) == 0b1111_0000) {    # Code points 10000 - 10FFFF
            $n = (($bytes[0] & 0b0111) << 18) |
                 (($bytes[1] & 0b11_1111) << 12) |
                 (($bytes[2] & 0b11_1111) << 6) |
                  ($bytes[3] & 0b11_1111);
            if ((($bytes[0] & 0b0111) == 0) &&
                (($bytes[1] & 0b0011_0000) == 0)) {
                printf("Overlong 4 byte UTF-8 code 0x%X for code point 0x%X\n", $u, $n);
            }
        }

        return $n;
    }

    #   Encode a single UTF-8 character code point as a byte
    #   stream in an integer.

    sub encode_UTF8 {
        my ($n) = @_;

        my $u;

        if ($n < 0x80) {
            $u = $n;
        } elsif ($n < 0x800) {
            $u = ((0b1100_0000 | ($n >> 6)) << 8) |
                  (0b1000_0000 | ($n & 0b0011_1111));
        } elsif ($n < 0x10000) {
            $u = ((0b1110_0000 | ($n >> 12)) << 16) |
                 ((0b1000_0000 | (($n >> 6) & 0b0011_1111)) << 8) |
                  (0b1000_0000 | ($n & 0b0011_1111));
        } else {
            $u = ((0b1111_0000 | ($n >> 18)) << 24) |
                 ((0b1000_0000 | (($n >> 12) & 0b0011_1111)) << 16) |
                 ((0b1000_0000 | (($n >> 6) & 0b0011_1111)) << 8) |
                  (0b1000_0000 | ($n & 0b0011_1111));
        }

        return $u;
    }

=begin test_UTF8
    #   Test UTF-8 encoding and decoding

    sub test_UTF8 {
        for (my $c = 0; $c <= 0x10FFFF; $c++) {
            my $n = encode_UTF8($c);
            my $u = decode_UTF8($n);
            if ($c != $u) {
                printf("UTF-8 encode/decode failure for code point 0x%X: encoded 0x%X  decoded 0x%X\n",
                    $c, $n, $u);
            }
        }
    }
=end test-UTF8
=cut

=pod

The Unicode character tables are based upon the Unicode 14.0.0
(2021) standard.

The control characters in this B<unum> version have been annotated
with their Unicode abbreviations, names, and for U+0000 to U+001F,
the Ctrl-key code which generates them.

The HTML named character references are from the World Wide Web
Consortium HTML standard.  Some browsers may not support all of
these references.

=cut

    sub uname {
        my $code = shift;
        if ($code >= 0x4E00) {
            if ($code >= 0xD800 && $code <= 0xF8FF) {
                # Surrogate and private
                if ($code <= 0xDFFF) {
                    return "<surrogate>";
                } else {
                    return "<private>";
                }
            }

        }
        $UNICODE_NAMES{sprintf("%04X", $code)}
    }

    sub ublock {
        my $code = shift;
        # XXX: could use a binary search, but I am too lazy today...
        my $block;
        for $block (@UNICODE_BLOCKS) {
            return $block->[2] if $block->[0] <= $code && $block->[1] >= $code;
        }
        undef;
    }

    sub init_names {
        #   Pre-dimension array and hash bucket sizes to reduce overhead
        #   in dynamic allocation as they are built below.
        $#UNICODE_BLOCKS = 320;
        $#HTML_CHARACTER_REFERENCES = 2032;
        $#HTML_COMPOSED_CHARACTER_REFERENCES = 93;
        keys %UNICODE_NAMES = 144762;

        #   The following code allows us to build two versions of the program
        #   from the same template file.  The table of Unicode code points
        #   is enormous (7.9 Mb as of Unicode 14.0.0), and we'd prefer not
        #   to carry it around within this program.  We read the table from
        #   a __DATA__ block appended to the program.  Following this can
        #   either be the table itself, appended from a separate file when
        #   the program is built, or the table compressed with bzip2,
        #   preceded by a single text line that says "COMPRESSED".  If
        #   that sentinel is read, we switch the data stream to binary and
        #   feed it through bunzip, creating a temporary file.  Then we
        #   read the temporary file to load the data.  Otherwise, we
        #   continue to read and process the embedded uncompressed table.

        my $l;
        while ($l = <DATA>) {
            if ($l =~ m/^COMPRESSED/) {
                #       The code point table is compressed.  There are two ways
                #       we can approach uncompressing and loading it.  The first
                #       is to use the system's bunzip2 utility, decompressing to
                #       a temporary file which we then read.  The second is to use
                #       Perl's IO::Uncompress::Bunzip2, which is a core module in
                #       recent releases of Perl.  The first approach will only work
                #       on Unix-like systems, while the second should work on any
                #       implementation of Perl which supports all of the core
                #       modules.  The choice should be simple: use the module if
                #       it's present and otherwise fall back to the utility if
                #       we're on a system which provides it.
                #
                #       As with most things, what should be simple is actually more
                #       complicated.  The Perl module is very slow compared to the
                #       utility: almost four times slower.  This results in a
                #       noticeable pause on each invocation of unum, due solely to
                #       the decompression of the table.  There is no clean solution
                #       to this, so here's what I'm going to do.  If the file is
                #       compressed, we test for the existence of an executable of
                #       bunzip2 in the library locations where it's likely to be
                #       found on Unix-like systems.  If it's not found (which will
                #       be the case on legacy systems) and the IO::Uncompress::Bunzip2
                #       module exists, we use it, slow as it may be.  Otherwise,
                #       we try using bunzip2, whether or not we found it.  This
                #       will fail only if the system doesn't support the module
                #       and doesn't have an executable bunzip2.  In all other cases,
                #       the most efficient available alternative will be used.

                my $decomp_start = times() if $debug_decompress;
                                my $cmd_bunzip2 = (-x "/bin/bunzip2") || (-x "/usr/bin/bunzip2") ||
                                        (-x "/usr/local/bin/bunzip2");

                if ((!$cmd_bunzip2) && eval { require IO::Uncompress::Bunzip2; }) {
                    print(STDERR "Using IO::Uncompress::Bunzip2 module\n") if $debug_decompress;
                    my $bz = IO::Uncompress::Bunzip2->new(\*DATA);
                    while ($l = <$bz>) {
                        chop($l);
                        my ($code, $name) = split(' ', $l, 2);
                        $UNICODE_NAMES{$code} = $name;
                    }
                } else {
                    print(STDERR "Using system bunzip2\n") if $debug_decompress;
                    use File::Temp qw(tempfile);
                    my ($fh, $filename) = tempfile("unumXXXXXX", DIR => "/tmp",
                                                   SUFFIX => ".tmp", UNLINK => 1);
                    {
                        local $/ = undef;       # Set to read entire file at once
                        binmode(DATA);
                        open(UCHARS, "| bunzip2 -c >$filename") ||
                            die("Unable to open pipe to bunzip2 code point database");
                        print(UCHARS <DATA>);
                        close(UCHARS);
                    }

                    while ($l = <$fh>) {
                        chop($l);
                        my ($code, $name) = split(' ', $l, 2);
                        $UNICODE_NAMES{$code} = $name;
                    }
                    close($fh);
                }
                printf(STDERR "Decompression time: %.4f seconds.\n",
                    times() - $decomp_start) if $debug_decompress;
                last;
            } else {
                #   Code point table is uncompressed: read text directly
                do {
                    chop($l);
                    my ($code, $name) = split(' ', $l, 2);
                    $UNICODE_NAMES{$code} = $name;
                } while ($l = <DATA>);
                last;
            }
        }
        close(DATA);

        #   Remember to update $#UNICODE_BLOCKS pre-dimension above!
        @UNICODE_BLOCKS = (
        #  start   end        block name
          [0x0000, 0x007F => 'Basic Latin'],
          [0x0080, 0x00FF => 'Latin-1 Supplement'],
          [0x0100, 0x017F => 'Latin Extended-A'],
          [0x0180, 0x024F => 'Latin Extended-B'],
          [0x0250, 0x02AF => 'IPA Extensions'],
          [0x02B0, 0x02FF => 'Spacing Modifier Letters'],
          [0x0300, 0x036F => 'Combining Diacritical Marks'],
          [0x0370, 0x03FF => 'Greek and Coptic'],
          [0x0400, 0x04FF => 'Cyrillic'],
          [0x0500, 0x052F => 'Cyrillic Supplement'],
          [0x0530, 0x058F => 'Armenian'],
          [0x0590, 0x05FF => 'Hebrew'],
          [0x0600, 0x06FF => 'Arabic'],
          [0x0700, 0x074F => 'Syriac'],
          [0x0750, 0x077F => 'Arabic Supplement'],
          [0x0780, 0x07BF => 'Thaana'],
          [0x07C0, 0x07FF => 'NKo'],
          [0x0800, 0x083F => 'Samaritan'],
          [0x0840, 0x085F => 'Mandaic'],
          [0x0860, 0x086F => 'Syriac Supplement'],
          [0x0870, 0x089F => 'Arabic Extended-B'],
          [0x08A0, 0x08FF => 'Arabic Extended-A'],
          [0x0900, 0x097F => 'Devanagari'],
          [0x0980, 0x09FF => 'Bengali'],
          [0x0A00, 0x0A7F => 'Gurmukhi'],
          [0x0A80, 0x0AFF => 'Gujarati'],
          [0x0B00, 0x0B7F => 'Oriya'],
          [0x0B80, 0x0BFF => 'Tamil'],
          [0x0C00, 0x0C7F => 'Telugu'],
          [0x0C80, 0x0CFF => 'Kannada'],
          [0x0D00, 0x0D7F => 'Malayalam'],
          [0x0D80, 0x0DFF => 'Sinhala'],
          [0x0E00, 0x0E7F => 'Thai'],
          [0x0E80, 0x0EFF => 'Lao'],
          [0x0F00, 0x0FFF => 'Tibetan'],
          [0x1000, 0x109F => 'Myanmar'],
          [0x10A0, 0x10FF => 'Georgian'],
          [0x1100, 0x11FF => 'Hangul Jamo'],
          [0x1200, 0x137F => 'Ethiopic'],
          [0x1380, 0x139F => 'Ethiopic Supplement'],
          [0x13A0, 0x13FF => 'Cherokee'],
          [0x1400, 0x167F => 'Unified Canadian Aboriginal Syllabics'],
          [0x1680, 0x169F => 'Ogham'],
          [0x16A0, 0x16FF => 'Runic'],
          [0x1700, 0x171F => 'Tagalog'],
          [0x1720, 0x173F => 'Hanunoo'],
          [0x1740, 0x175F => 'Buhid'],
          [0x1760, 0x177F => 'Tagbanwa'],
          [0x1780, 0x17FF => 'Khmer'],
          [0x1800, 0x18AF => 'Mongolian'],
          [0x18B0, 0x18FF => 'Unified Canadian Aboriginal Syllabics Extended'],
          [0x1900, 0x194F => 'Limbu'],
          [0x1950, 0x197F => 'Tai Le'],
          [0x1980, 0x19DF => 'New Tai Lue'],
          [0x19E0, 0x19FF => 'Khmer Symbols'],
          [0x1A00, 0x1A1F => 'Buginese'],
          [0x1A20, 0x1AAF => 'Tai Tham'],
          [0x1AB0, 0x1AFF => 'Combining Diacritical Marks Extended'],
          [0x1B00, 0x1B7F => 'Balinese'],
          [0x1B80, 0x1BBF => 'Sundanese'],
          [0x1BC0, 0x1BFF => 'Batak'],
          [0x1C00, 0x1C4F => 'Lepcha'],
          [0x1C50, 0x1C7F => 'Ol Chiki'],
          [0x1C80, 0x1C8F => 'Cyrillic Extended-C'],
          [0x1C90, 0x1CBF => 'Georgian Extended'],
          [0x1CC0, 0x1CCF => 'Sundanese Supplement'],
          [0x1CD0, 0x1CFF => 'Vedic Extensions'],
          [0x1D00, 0x1D7F => 'Phonetic Extensions'],
          [0x1D80, 0x1DBF => 'Phonetic Extensions Supplement'],
          [0x1DC0, 0x1DFF => 'Combining Diacritical Marks Supplement'],
          [0x1E00, 0x1EFF => 'Latin Extended Additional'],
          [0x1F00, 0x1FFF => 'Greek Extended'],
          [0x2000, 0x206F => 'General Punctuation'],
          [0x2070, 0x209F => 'Superscripts and Subscripts'],
          [0x20A0, 0x20CF => 'Currency Symbols'],
          [0x20D0, 0x20FF => 'Combining Diacritical Marks for Symbols'],
          [0x2100, 0x214F => 'Letterlike Symbols'],
          [0x2150, 0x218F => 'Number Forms'],
          [0x2190, 0x21FF => 'Arrows'],
          [0x2200, 0x22FF => 'Mathematical Operators'],
          [0x2300, 0x23FF => 'Miscellaneous Technical'],
          [0x2400, 0x243F => 'Control Pictures'],
          [0x2440, 0x245F => 'Optical Character Recognition'],
          [0x2460, 0x24FF => 'Enclosed Alphanumerics'],
          [0x2500, 0x257F => 'Box Drawing'],
          [0x2580, 0x259F => 'Block Elements'],
          [0x25A0, 0x25FF => 'Geometric Shapes'],
          [0x2600, 0x26FF => 'Miscellaneous Symbols'],
          [0x2700, 0x27BF => 'Dingbats'],
          [0x27C0, 0x27EF => 'Miscellaneous Mathematical Symbols-A'],
          [0x27F0, 0x27FF => 'Supplemental Arrows-A'],
          [0x2800, 0x28FF => 'Braille Patterns'],
          [0x2900, 0x297F => 'Supplemental Arrows-B'],
          [0x2980, 0x29FF => 'Miscellaneous Mathematical Symbols-B'],
          [0x2A00, 0x2AFF => 'Supplemental Mathematical Operators'],
          [0x2B00, 0x2BFF => 'Miscellaneous Symbols and Arrows'],
          [0x2C00, 0x2C5F => 'Glagolitic'],
          [0x2C60, 0x2C7F => 'Latin Extended-C'],
          [0x2C80, 0x2CFF => 'Coptic'],
          [0x2D00, 0x2D2F => 'Georgian Supplement'],
          [0x2D30, 0x2D7F => 'Tifinagh'],
          [0x2D80, 0x2DDF => 'Ethiopic Extended'],
          [0x2DE0, 0x2DFF => 'Cyrillic Extended-A'],
          [0x2E00, 0x2E7F => 'Supplemental Punctuation'],
          [0x2E80, 0x2EFF => 'CJK Radicals Supplement'],
          [0x2F00, 0x2FDF => 'Kangxi Radicals'],
          [0x2FF0, 0x2FFF => 'Ideographic Description Characters'],
          [0x3000, 0x303F => 'CJK Symbols and Punctuation'],
          [0x3040, 0x309F => 'Hiragana'],
          [0x30A0, 0x30FF => 'Katakana'],
          [0x3100, 0x312F => 'Bopomofo'],
          [0x3130, 0x318F => 'Hangul Compatibility Jamo'],
          [0x3190, 0x319F => 'Kanbun'],
          [0x31A0, 0x31BF => 'Bopomofo Extended'],
          [0x31C0, 0x31EF => 'CJK Strokes'],
          [0x31F0, 0x31FF => 'Katakana Phonetic Extensions'],
          [0x3200, 0x32FF => 'Enclosed CJK Letters and Months'],
          [0x3300, 0x33FF => 'CJK Compatibility'],
          [0x3400, 0x4DBF => 'CJK Unified Ideographs Extension A'],
          [0x4DC0, 0x4DFF => 'Yijing Hexagram Symbols'],
          [0x4E00, 0x9FFF => 'CJK Unified Ideographs'],
          [0xA000, 0xA48F => 'Yi Syllables'],
          [0xA490, 0xA4CF => 'Yi Radicals'],
          [0xA4D0, 0xA4FF => 'Lisu'],
          [0xA500, 0xA63F => 'Vai'],
          [0xA640, 0xA69F => 'Cyrillic Extended-B'],
          [0xA6A0, 0xA6FF => 'Bamum'],
          [0xA700, 0xA71F => 'Modifier Tone Letters'],
          [0xA720, 0xA7FF => 'Latin Extended-D'],
          [0xA800, 0xA82F => 'Syloti Nagri'],
          [0xA830, 0xA83F => 'Common Indic Number Forms'],
          [0xA840, 0xA87F => 'Phags-pa'],
          [0xA880, 0xA8DF => 'Saurashtra'],
          [0xA8E0, 0xA8FF => 'Devanagari Extended'],
          [0xA900, 0xA92F => 'Kayah Li'],
          [0xA930, 0xA95F => 'Rejang'],
          [0xA960, 0xA97F => 'Hangul Jamo Extended-A'],
          [0xA980, 0xA9DF => 'Javanese'],
          [0xA9E0, 0xA9FF => 'Myanmar Extended-B'],
          [0xAA00, 0xAA5F => 'Cham'],
          [0xAA60, 0xAA7F => 'Myanmar Extended-A'],
          [0xAA80, 0xAADF => 'Tai Viet'],
          [0xAAE0, 0xAAFF => 'Meetei Mayek Extensions'],
          [0xAB00, 0xAB2F => 'Ethiopic Extended-A'],
          [0xAB30, 0xAB6F => 'Latin Extended-E'],
          [0xAB70, 0xABBF => 'Cherokee Supplement'],
          [0xABC0, 0xABFF => 'Meetei Mayek'],
          [0xAC00, 0xD7AF => 'Hangul Syllables'],
          [0xD7B0, 0xD7FF => 'Hangul Jamo Extended-B'],
          [0xD800, 0xDB7F => 'High Surrogates'],
          [0xDB80, 0xDBFF => 'High Private Use Surrogates'],
          [0xDC00, 0xDFFF => 'Low Surrogates'],
          [0xE000, 0xF8FF => 'Private Use Area'],
          [0xF900, 0xFAFF => 'CJK Compatibility Ideographs'],
          [0xFB00, 0xFB4F => 'Alphabetic Presentation Forms'],
          [0xFB50, 0xFDFF => 'Arabic Presentation Forms-A'],
          [0xFE00, 0xFE0F => 'Variation Selectors'],
          [0xFE10, 0xFE1F => 'Vertical Forms'],
          [0xFE20, 0xFE2F => 'Combining Half Marks'],
          [0xFE30, 0xFE4F => 'CJK Compatibility Forms'],
          [0xFE50, 0xFE6F => 'Small Form Variants'],
          [0xFE70, 0xFEFF => 'Arabic Presentation Forms-B'],
          [0xFF00, 0xFFEF => 'Halfwidth and Fullwidth Forms'],
          [0xFFF0, 0xFFFF => 'Specials'],
          [0x10000, 0x1007F => 'Linear B Syllabary'],
          [0x10080, 0x100FF => 'Linear B Ideograms'],
          [0x10100, 0x1013F => 'Aegean Numbers'],
          [0x10140, 0x1018F => 'Ancient Greek Numbers'],
          [0x10190, 0x101CF => 'Ancient Symbols'],
          [0x101D0, 0x101FF => 'Phaistos Disc'],
          [0x10280, 0x1029F => 'Lycian'],
          [0x102A0, 0x102DF => 'Carian'],
          [0x102E0, 0x102FF => 'Coptic Epact Numbers'],
          [0x10300, 0x1032F => 'Old Italic'],
          [0x10330, 0x1034F => 'Gothic'],
          [0x10350, 0x1037F => 'Old Permic'],
          [0x10380, 0x1039F => 'Ugaritic'],
          [0x103A0, 0x103DF => 'Old Persian'],
          [0x10400, 0x1044F => 'Deseret'],
          [0x10450, 0x1047F => 'Shavian'],
          [0x10480, 0x104AF => 'Osmanya'],
          [0x104B0, 0x104FF => 'Osage'],
          [0x10500, 0x1052F => 'Elbasan'],
          [0x10530, 0x1056F => 'Caucasian Albanian'],
          [0x10570, 0x105BF => 'Vithkuqi'],
          [0x10600, 0x1077F => 'Linear A'],
          [0x10780, 0x107BF => 'Latin Extended-F'],
          [0x10800, 0x1083F => 'Cypriot Syllabary'],
          [0x10840, 0x1085F => 'Imperial Aramaic'],
          [0x10860, 0x1087F => 'Palmyrene'],
          [0x10880, 0x108AF => 'Nabataean'],
          [0x108E0, 0x108FF => 'Hatran'],
          [0x10900, 0x1091F => 'Phoenician'],
          [0x10920, 0x1093F => 'Lydian'],
          [0x10980, 0x1099F => 'Meroitic Hieroglyphs'],
          [0x109A0, 0x109FF => 'Meroitic Cursive'],
          [0x10A00, 0x10A5F => 'Kharoshthi'],
          [0x10A60, 0x10A7F => 'Old South Arabian'],
          [0x10A80, 0x10A9F => 'Old North Arabian'],
          [0x10AC0, 0x10AFF => 'Manichaean'],
          [0x10B00, 0x10B3F => 'Avestan'],
          [0x10B40, 0x10B5F => 'Inscriptional Parthian'],
          [0x10B60, 0x10B7F => 'Inscriptional Pahlavi'],
          [0x10B80, 0x10BAF => 'Psalter Pahlavi'],
          [0x10C00, 0x10C4F => 'Old Turkic'],
          [0x10C80, 0x10CFF => 'Old Hungarian'],
          [0x10D00, 0x10D3F => 'Hanifi Rohingya'],
          [0x10E60, 0x10E7F => 'Rumi Numeral Symbols'],
          [0x10E80, 0x10EBF => 'Yezidi'],
          [0x10F00, 0x10F2F => 'Old Sogdian'],
          [0x10F30, 0x10F6F => 'Sogdian'],
          [0x10F70, 0x10FAF => 'Old Uyghur'],
          [0x10FB0, 0x10FDF => 'Chorasmian'],
          [0x10FE0, 0x10FFF => 'Elymaic'],
          [0x11000, 0x1107F => 'Brahmi'],
          [0x11080, 0x110CF => 'Kaithi'],
          [0x110D0, 0x110FF => 'Sora Sompeng'],
          [0x11100, 0x1114F => 'Chakma'],
          [0x11150, 0x1117F => 'Mahajani'],
          [0x11180, 0x111DF => 'Sharada'],
          [0x111E0, 0x111FF => 'Sinhala Archaic Numbers'],
          [0x11200, 0x1124F => 'Khojki'],
          [0x11280, 0x112AF => 'Multani'],
          [0x112B0, 0x112FF => 'Khudawadi'],
          [0x11300, 0x1137F => 'Grantha'],
          [0x11400, 0x1147F => 'Newa'],
          [0x11480, 0x114DF => 'Tirhuta'],
          [0x11580, 0x115FF => 'Siddham'],
          [0x11600, 0x1165F => 'Modi'],
          [0x11660, 0x1167F => 'Mongolian Supplement'],
          [0x11680, 0x116CF => 'Takri'],
          [0x11700, 0x1174F => 'Ahom'],
          [0x11800, 0x1184F => 'Dogra'],
          [0x118A0, 0x118FF => 'Warang Citi'],
          [0x11900, 0x1195F => 'Dives Akuru'],
          [0x119A0, 0x119FF => 'Nandinagari'],
          [0x11A00, 0x11A4F => 'Zanabazar Square'],
          [0x11A50, 0x11AAF => 'Soyombo'],
          [0x11AB0, 0x11ABF => 'Unified Canadian Aboriginal Syllabics Extended-A'],
          [0x11AC0, 0x11AFF => 'Pau Cin Hau'],
          [0x11C00, 0x11C6F => 'Bhaiksuki'],
          [0x11C70, 0x11CBF => 'Marchen'],
          [0x11D00, 0x11D5F => 'Masaram Gondi'],
          [0x11D60, 0x11DAF => 'Gunjala Gondi'],
          [0x11EE0, 0x11EFF => 'Makasar'],
          [0x11FB0, 0x11FBF => 'Lisu Supplement'],
          [0x11FC0, 0x11FFF => 'Tamil Supplement'],
          [0x12000, 0x123FF => 'Cuneiform'],
          [0x12400, 0x1247F => 'Cuneiform Numbers and Punctuation'],
          [0x12480, 0x1254F => 'Early Dynastic Cuneiform'],
          [0x12F90, 0x12FFF => 'Cypro-Minoan'],
          [0x13000, 0x1342F => 'Egyptian Hieroglyphs'],
          [0x13430, 0x1343F => 'Egyptian Hieroglyph Format Controls'],
          [0x14400, 0x1467F => 'Anatolian Hieroglyphs'],
          [0x16800, 0x16A3F => 'Bamum Supplement'],
          [0x16A40, 0x16A6F => 'Mro'],
          [0x16A70, 0x16ACF => 'Tangsa'],
          [0x16AD0, 0x16AFF => 'Bassa Vah'],
          [0x16B00, 0x16B8F => 'Pahawh Hmong'],
          [0x16E40, 0x16E9F => 'Medefaidrin'],
          [0x16F00, 0x16F9F => 'Miao'],
          [0x16FE0, 0x16FFF => 'Ideographic Symbols and Punctuation'],
          [0x17000, 0x187FF => 'Tangut'],
          [0x18800, 0x18AFF => 'Tangut Components'],
          [0x18B00, 0x18CFF => 'Khitan Small Script'],
          [0x18D00, 0x18D7F => 'Tangut Supplement'],
          [0x1AFF0, 0x1AFFF => 'Kana Extended-B'],
          [0x1B000, 0x1B0FF => 'Kana Supplement'],
          [0x1B100, 0x1B12F => 'Kana Extended-A'],
          [0x1B130, 0x1B16F => 'Small Kana Extension'],
          [0x1B170, 0x1B2FF => 'Nushu'],
          [0x1BC00, 0x1BC9F => 'Duployan'],
          [0x1BCA0, 0x1BCAF => 'Shorthand Format Controls'],
          [0x1CF00, 0x1CFCF => 'Znamenny Musical Notation'],
          [0x1D000, 0x1D0FF => 'Byzantine Musical Symbols'],
          [0x1D100, 0x1D1FF => 'Musical Symbols'],
          [0x1D200, 0x1D24F => 'Ancient Greek Musical Notation'],
          [0x1D2E0, 0x1D2FF => 'Mayan Numerals'],
          [0x1D300, 0x1D35F => 'Tai Xuan Jing Symbols'],
          [0x1D360, 0x1D37F => 'Counting Rod Numerals'],
          [0x1D400, 0x1D7FF => 'Mathematical Alphanumeric Symbols'],
          [0x1D800, 0x1DAAF => 'Sutton SignWriting'],
          [0x1DF00, 0x1DFFF => 'Latin Extended-G'],
          [0x1E000, 0x1E02F => 'Glagolitic Supplement'],
          [0x1E100, 0x1E14F => 'Nyiakeng Puachue Hmong'],
          [0x1E290, 0x1E2BF => 'Toto'],
          [0x1E2C0, 0x1E2FF => 'Wancho'],
          [0x1E7E0, 0x1E7FF => 'Ethiopic Extended-B'],
          [0x1E800, 0x1E8DF => 'Mende Kikakui'],
          [0x1E900, 0x1E95F => 'Adlam'],
          [0x1EC70, 0x1ECBF => 'Indic Siyaq Numbers'],
          [0x1ED00, 0x1ED4F => 'Ottoman Siyaq Numbers'],
          [0x1EE00, 0x1EEFF => 'Arabic Mathematical Alphabetic Symbols'],
          [0x1F000, 0x1F02F => 'Mahjong Tiles'],
          [0x1F030, 0x1F09F => 'Domino Tiles'],
          [0x1F0A0, 0x1F0FF => 'Playing Cards'],
          [0x1F100, 0x1F1FF => 'Enclosed Alphanumeric Supplement'],
          [0x1F200, 0x1F2FF => 'Enclosed Ideographic Supplement'],
          [0x1F300, 0x1F5FF => 'Miscellaneous Symbols and Pictographs'],
          [0x1F600, 0x1F64F => 'Emoticons'],
          [0x1F650, 0x1F67F => 'Ornamental Dingbats'],
          [0x1F680, 0x1F6FF => 'Transport and Map Symbols'],
          [0x1F700, 0x1F77F => 'Alchemical Symbols'],
          [0x1F780, 0x1F7FF => 'Geometric Shapes Extended'],
          [0x1F800, 0x1F8FF => 'Supplemental Arrows-C'],
          [0x1F900, 0x1F9FF => 'Supplemental Symbols and Pictographs'],
          [0x1FA00, 0x1FA6F => 'Chess Symbols'],
          [0x1FA70, 0x1FAFF => 'Symbols and Pictographs Extended-A'],
          [0x1FB00, 0x1FBFF => 'Symbols for Legacy Computing'],
          [0x20000, 0x2A6DF => 'CJK Unified Ideographs Extension B'],
          [0x2A700, 0x2B73F => 'CJK Unified Ideographs Extension C'],
          [0x2B740, 0x2B81F => 'CJK Unified Ideographs Extension D'],
          [0x2B820, 0x2CEAF => 'CJK Unified Ideographs Extension E'],
          [0x2CEB0, 0x2EBEF => 'CJK Unified Ideographs Extension F'],
          [0x2F800, 0x2FA1F => 'CJK Compatibility Ideographs Supplement'],
          [0x30000, 0x3134F => 'CJK Unified Ideographs Extension G'],
          [0xE0000, 0xE007F => 'Tags'],
          [0xE0100, 0xE01EF => 'Variation Selectors Supplement'],
          [0xF0000, 0xFFFFF => 'Supplementary Private Use Area-A'],
          [0x100000, 0x10FFFF => 'Supplementary Private Use Area-B'],
        );

        #   HTML5 Named Character References

        #   Remember to update $#HTML_CHARACTER_REFERENCES pre-dimension above!
        @HTML_CHARACTER_REFERENCES = (
            #   From https://www.w3.org/TR/html5/entities.json
            'Aacute', 193,
            'aacute', 225,
            'Abreve', 258,
            'abreve', 259,
            'ac', 8766,
            'acd', 8767,
            'Acirc', 194,
            'acirc', 226,
            'acute', 180,
            'Acy', 1040,
            'acy', 1072,
            'AElig', 198,
            'aelig', 230,
            'af', 8289,
            'Afr', 120068,
            'afr', 120094,
            'Agrave', 192,
            'agrave', 224,
            'alefsym', 8501,
            'aleph', 8501,
            'Alpha', 913,
            'alpha', 945,
            'Amacr', 256,
            'amacr', 257,
            'amalg', 10815,
            'AMP', 38,
            'amp', 38,
            'And', 10835,
            'and', 8743,
            'andand', 10837,
            'andd', 10844,
            'andslope', 10840,
            'andv', 10842,
            'ang', 8736,
            'ange', 10660,
            'angle', 8736,
            'angmsd', 8737,
            'angmsdaa', 10664,
            'angmsdab', 10665,
            'angmsdac', 10666,
            'angmsdad', 10667,
            'angmsdae', 10668,
            'angmsdaf', 10669,
            'angmsdag', 10670,
            'angmsdah', 10671,
            'angrt', 8735,
            'angrtvb', 8894,
            'angrtvbd', 10653,
            'angsph', 8738,
            'angst', 197,
            'angzarr', 9084,
            'Aogon', 260,
            'aogon', 261,
            'Aopf', 120120,
            'aopf', 120146,
            'ap', 8776,
            'apacir', 10863,
            'apE', 10864,
            'ape', 8778,
            'apid', 8779,
            'apos', 39,
            'ApplyFunction', 8289,
            'approx', 8776,
            'approxeq', 8778,
            'Aring', 197,
            'aring', 229,
            'Ascr', 119964,
            'ascr', 119990,
            'Assign', 8788,
            'ast', 42,
            'asymp', 8776,
            'asympeq', 8781,
            'Atilde', 195,
            'atilde', 227,
            'Auml', 196,
            'auml', 228,
            'awconint', 8755,
            'awint', 10769,
            'backcong', 8780,
            'backepsilon', 1014,
            'backprime', 8245,
            'backsim', 8765,
            'backsimeq', 8909,
            'Backslash', 8726,
            'Barv', 10983,
            'barvee', 8893,
            'Barwed', 8966,
            'barwed', 8965,
            'barwedge', 8965,
            'bbrk', 9141,
            'bbrktbrk', 9142,
            'bcong', 8780,
            'Bcy', 1041,
            'bcy', 1073,
            'bdquo', 8222,
            'becaus', 8757,
            'Because', 8757,
            'because', 8757,
            'bemptyv', 10672,
            'bepsi', 1014,
            'bernou', 8492,
            'Bernoullis', 8492,
            'Beta', 914,
            'beta', 946,
            'beth', 8502,
            'between', 8812,
            'Bfr', 120069,
            'bfr', 120095,
            'bigcap', 8898,
            'bigcirc', 9711,
            'bigcup', 8899,
            'bigodot', 10752,
            'bigoplus', 10753,
            'bigotimes', 10754,
            'bigsqcup', 10758,
            'bigstar', 9733,
            'bigtriangledown', 9661,
            'bigtriangleup', 9651,
            'biguplus', 10756,
            'bigvee', 8897,
            'bigwedge', 8896,
            'bkarow', 10509,
            'blacklozenge', 10731,
            'blacksquare', 9642,
            'blacktriangle', 9652,
            'blacktriangledown', 9662,
            'blacktriangleleft', 9666,
            'blacktriangleright', 9656,
            'blank', 9251,
            'blk12', 9618,
            'blk14', 9617,
            'blk34', 9619,
            'block', 9608,
            'bNot', 10989,
            'bnot', 8976,
            'Bopf', 120121,
            'bopf', 120147,
            'bot', 8869,
            'bottom', 8869,
            'bowtie', 8904,
            'boxbox', 10697,
            'boxDL', 9559,
            'boxDl', 9558,
            'boxdL', 9557,
            'boxdl', 9488,
            'boxDR', 9556,
            'boxDr', 9555,
            'boxdR', 9554,
            'boxdr', 9484,
            'boxH', 9552,
            'boxh', 9472,
            'boxHD', 9574,
            'boxHd', 9572,
            'boxhD', 9573,
            'boxhd', 9516,
            'boxHU', 9577,
            'boxHu', 9575,
            'boxhU', 9576,
            'boxhu', 9524,
            'boxminus', 8863,
            'boxplus', 8862,
            'boxtimes', 8864,
            'boxUL', 9565,
            'boxUl', 9564,
            'boxuL', 9563,
            'boxul', 9496,
            'boxUR', 9562,
            'boxUr', 9561,
            'boxuR', 9560,
            'boxur', 9492,
            'boxV', 9553,
            'boxv', 9474,
            'boxVH', 9580,
            'boxVh', 9579,
            'boxvH', 9578,
            'boxvh', 9532,
            'boxVL', 9571,
            'boxVl', 9570,
            'boxvL', 9569,
            'boxvl', 9508,
            'boxVR', 9568,
            'boxVr', 9567,
            'boxvR', 9566,
            'boxvr', 9500,
            'bprime', 8245,
            'Breve', 728,
            'breve', 728,
            'brvbar', 166,
            'Bscr', 8492,
            'bscr', 119991,
            'bsemi', 8271,
            'bsim', 8765,
            'bsime', 8909,
            'bsol', 92,
            'bsolb', 10693,
            'bsolhsub', 10184,
            'bull', 8226,
            'bullet', 8226,
            'bump', 8782,
            'bumpE', 10926,
            'bumpe', 8783,
            'Bumpeq', 8782,
            'bumpeq', 8783,
            'Cacute', 262,
            'cacute', 263,
            'Cap', 8914,
            'cap', 8745,
            'capand', 10820,
            'capbrcup', 10825,
            'capcap', 10827,
            'capcup', 10823,
            'capdot', 10816,
            'CapitalDifferentialD', 8517,
            'caret', 8257,
            'caron', 711,
            'Cayleys', 8493,
            'ccaps', 10829,
            'Ccaron', 268,
            'ccaron', 269,
            'Ccedil', 199,
            'ccedil', 231,
            'Ccirc', 264,
            'ccirc', 265,
            'Cconint', 8752,
            'ccups', 10828,
            'ccupssm', 10832,
            'Cdot', 266,
            'cdot', 267,
            'cedil', 184,
            'Cedilla', 184,
            'cemptyv', 10674,
            'cent', 162,
            'CenterDot', 183,
            'centerdot', 183,
            'Cfr', 8493,
            'cfr', 120096,
            'CHcy', 1063,
            'chcy', 1095,
            'check', 10003,
            'checkmark', 10003,
            'Chi', 935,
            'chi', 967,
            'cir', 9675,
            'circ', 710,
            'circeq', 8791,
            'circlearrowleft', 8634,
            'circlearrowright', 8635,
            'circledast', 8859,
            'circledcirc', 8858,
            'circleddash', 8861,
            'CircleDot', 8857,
            'circledR', 174,
            'circledS', 9416,
            'CircleMinus', 8854,
            'CirclePlus', 8853,
            'CircleTimes', 8855,
            'cirE', 10691,
            'cire', 8791,
            'cirfnint', 10768,
            'cirmid', 10991,
            'cirscir', 10690,
            'ClockwiseContourIntegral', 8754,
            'CloseCurlyDoubleQuote', 8221,
            'CloseCurlyQuote', 8217,
            'clubs', 9827,
            'clubsuit', 9827,
            'Colon', 8759,
            'colon', 58,
            'Colone', 10868,
            'colone', 8788,
            'coloneq', 8788,
            'comma', 44,
            'commat', 64,
            'comp', 8705,
            'compfn', 8728,
            'complement', 8705,
            'complexes', 8450,
            'cong', 8773,
            'congdot', 10861,
            'Congruent', 8801,
            'Conint', 8751,
            'conint', 8750,
            'ContourIntegral', 8750,
            'Copf', 8450,
            'copf', 120148,
            'coprod', 8720,
            'Coproduct', 8720,
            'COPY', 169,
            'copy', 169,
            'copysr', 8471,
            'CounterClockwiseContourIntegral', 8755,
            'crarr', 8629,
            'Cross', 10799,
            'cross', 10007,
            'Cscr', 119966,
            'cscr', 119992,
            'csub', 10959,
            'csube', 10961,
            'csup', 10960,
            'csupe', 10962,
            'ctdot', 8943,
            'cudarrl', 10552,
            'cudarrr', 10549,
            'cuepr', 8926,
            'cuesc', 8927,
            'cularr', 8630,
            'cularrp', 10557,
            'Cup', 8915,
            'cup', 8746,
            'cupbrcap', 10824,
            'CupCap', 8781,
            'cupcap', 10822,
            'cupcup', 10826,
            'cupdot', 8845,
            'cupor', 10821,
            'curarr', 8631,
            'curarrm', 10556,
            'curlyeqprec', 8926,
            'curlyeqsucc', 8927,
            'curlyvee', 8910,
            'curlywedge', 8911,
            'curren', 164,
            'curvearrowleft', 8630,
            'curvearrowright', 8631,
            'cuvee', 8910,
            'cuwed', 8911,
            'cwconint', 8754,
            'cwint', 8753,
            'cylcty', 9005,
            'Dagger', 8225,
            'dagger', 8224,
            'daleth', 8504,
            'Darr', 8609,
            'dArr', 8659,
            'darr', 8595,
            'dash', 8208,
            'Dashv', 10980,
            'dashv', 8867,
            'dbkarow', 10511,
            'dblac', 733,
            'Dcaron', 270,
            'dcaron', 271,
            'Dcy', 1044,
            'dcy', 1076,
            'DD', 8517,
            'dd', 8518,
            'ddagger', 8225,
            'ddarr', 8650,
            'DDotrahd', 10513,
            'ddotseq', 10871,
            'deg', 176,
            'Del', 8711,
            'Delta', 916,
            'delta', 948,
            'demptyv', 10673,
            'dfisht', 10623,
            'Dfr', 120071,
            'dfr', 120097,
            'dHar', 10597,
            'dharl', 8643,
            'dharr', 8642,
            'DiacriticalAcute', 180,
            'DiacriticalDot', 729,
            'DiacriticalDoubleAcute', 733,
            'DiacriticalGrave', 96,
            'DiacriticalTilde', 732,
            'diam', 8900,
            'Diamond', 8900,
            'diamond', 8900,
            'diamondsuit', 9830,
            'diams', 9830,
            'die', 168,
            'DifferentialD', 8518,
            'digamma', 989,
            'disin', 8946,
            'div', 247,
            'divide', 247,
            'divideontimes', 8903,
            'divonx', 8903,
            'DJcy', 1026,
            'djcy', 1106,
            'dlcorn', 8990,
            'dlcrop', 8973,
            'dollar', 36,
            'Dopf', 120123,
            'dopf', 120149,
            'Dot', 168,
            'dot', 729,
            'DotDot', 8412,
            'doteq', 8784,
            'doteqdot', 8785,
            'DotEqual', 8784,
            'dotminus', 8760,
            'dotplus', 8724,
            'dotsquare', 8865,
            'doublebarwedge', 8966,
            'DoubleContourIntegral', 8751,
            'DoubleDot', 168,
            'DoubleDownArrow', 8659,
            'DoubleLeftArrow', 8656,
            'DoubleLeftRightArrow', 8660,
            'DoubleLeftTee', 10980,
            'DoubleLongLeftArrow', 10232,
            'DoubleLongLeftRightArrow', 10234,
            'DoubleLongRightArrow', 10233,
            'DoubleRightArrow', 8658,
            'DoubleRightTee', 8872,
            'DoubleUpArrow', 8657,
            'DoubleUpDownArrow', 8661,
            'DoubleVerticalBar', 8741,
            'DownArrow', 8595,
            'Downarrow', 8659,
            'downarrow', 8595,
            'DownArrowBar', 10515,
            'DownArrowUpArrow', 8693,
            'DownBreve', 785,
            'downdownarrows', 8650,
            'downharpoonleft', 8643,
            'downharpoonright', 8642,
            'DownLeftRightVector', 10576,
            'DownLeftTeeVector', 10590,
            'DownLeftVector', 8637,
            'DownLeftVectorBar', 10582,
            'DownRightTeeVector', 10591,
            'DownRightVector', 8641,
            'DownRightVectorBar', 10583,
            'DownTee', 8868,
            'DownTeeArrow', 8615,
            'drbkarow', 10512,
            'drcorn', 8991,
            'drcrop', 8972,
            'Dscr', 119967,
            'dscr', 119993,
            'DScy', 1029,
            'dscy', 1109,
            'dsol', 10742,
            'Dstrok', 272,
            'dstrok', 273,
            'dtdot', 8945,
            'dtri', 9663,
            'dtrif', 9662,
            'duarr', 8693,
            'duhar', 10607,
            'dwangle', 10662,
            'DZcy', 1039,
            'dzcy', 1119,
            'dzigrarr', 10239,
            'Eacute', 201,
            'eacute', 233,
            'easter', 10862,
            'Ecaron', 282,
            'ecaron', 283,
            'ecir', 8790,
            'Ecirc', 202,
            'ecirc', 234,
            'ecolon', 8789,
            'Ecy', 1069,
            'ecy', 1101,
            'eDDot', 10871,
            'Edot', 278,
            'eDot', 8785,
            'edot', 279,
            'ee', 8519,
            'efDot', 8786,
            'Efr', 120072,
            'efr', 120098,
            'eg', 10906,
            'Egrave', 200,
            'egrave', 232,
            'egs', 10902,
            'egsdot', 10904,
            'el', 10905,
            'Element', 8712,
            'elinters', 9191,
            'ell', 8467,
            'els', 10901,
            'elsdot', 10903,
            'Emacr', 274,
            'emacr', 275,
            'empty', 8709,
            'emptyset', 8709,
            'EmptySmallSquare', 9723,
            'emptyv', 8709,
            'EmptyVerySmallSquare', 9643,
            'emsp', 8195,
            'emsp13', 8196,
            'emsp14', 8197,
            'ENG', 330,
            'eng', 331,
            'ensp', 8194,
            'Eogon', 280,
            'eogon', 281,
            'Eopf', 120124,
            'eopf', 120150,
            'epar', 8917,
            'eparsl', 10723,
            'eplus', 10865,
            'epsi', 949,
            'Epsilon', 917,
            'epsilon', 949,
            'epsiv', 1013,
            'eqcirc', 8790,
            'eqcolon', 8789,
            'eqsim', 8770,
            'eqslantgtr', 10902,
            'eqslantless', 10901,
            'Equal', 10869,
            'equals', 61,
            'EqualTilde', 8770,
            'equest', 8799,
            'Equilibrium', 8652,
            'equiv', 8801,
            'equivDD', 10872,
            'eqvparsl', 10725,
            'erarr', 10609,
            'erDot', 8787,
            'Escr', 8496,
            'escr', 8495,
            'esdot', 8784,
            'Esim', 10867,
            'esim', 8770,
            'Eta', 919,
            'eta', 951,
            'ETH', 208,
            'eth', 240,
            'Euml', 203,
            'euml', 235,
            'euro', 8364,
            'excl', 33,
            'exist', 8707,
            'Exists', 8707,
            'expectation', 8496,
            'ExponentialE', 8519,
            'exponentiale', 8519,
            'fallingdotseq', 8786,
            'Fcy', 1060,
            'fcy', 1092,
            'female', 9792,
            'ffilig', 64259,
            'fflig', 64256,
            'ffllig', 64260,
            'Ffr', 120073,
            'ffr', 120099,
            'filig', 64257,
            'FilledSmallSquare', 9724,
            'FilledVerySmallSquare', 9642,
            'flat', 9837,
            'fllig', 64258,
            'fltns', 9649,
            'fnof', 402,
            'Fopf', 120125,
            'fopf', 120151,
            'ForAll', 8704,
            'forall', 8704,
            'fork', 8916,
            'forkv', 10969,
            'Fouriertrf', 8497,
            'fpartint', 10765,
            'frac12', 189,
            'frac13', 8531,
            'frac14', 188,
            'frac15', 8533,
            'frac16', 8537,
            'frac18', 8539,
            'frac23', 8532,
            'frac25', 8534,
            'frac34', 190,
            'frac35', 8535,
            'frac38', 8540,
            'frac45', 8536,
            'frac56', 8538,
            'frac58', 8541,
            'frac78', 8542,
            'frasl', 8260,
            'frown', 8994,
            'Fscr', 8497,
            'fscr', 119995,
            'gacute', 501,
            'Gamma', 915,
            'gamma', 947,
            'Gammad', 988,
            'gammad', 989,
            'gap', 10886,
            'Gbreve', 286,
            'gbreve', 287,
            'Gcedil', 290,
            'Gcirc', 284,
            'gcirc', 285,
            'Gcy', 1043,
            'gcy', 1075,
            'Gdot', 288,
            'gdot', 289,
            'gE', 8807,
            'ge', 8805,
            'gEl', 10892,
            'gel', 8923,
            'geq', 8805,
            'geqq', 8807,
            'geqslant', 10878,
            'ges', 10878,
            'gescc', 10921,
            'gesdot', 10880,
            'gesdoto', 10882,
            'gesdotol', 10884,
            'gesles', 10900,
            'Gfr', 120074,
            'gfr', 120100,
            'Gg', 8921,
            'gg', 8811,
            'ggg', 8921,
            'gimel', 8503,
            'GJcy', 1027,
            'gjcy', 1107,
            'gl', 8823,
            'gla', 10917,
            'glE', 10898,
            'glj', 10916,
            'gnap', 10890,
            'gnapprox', 10890,
            'gnE', 8809,
            'gne', 10888,
            'gneq', 10888,
            'gneqq', 8809,
            'gnsim', 8935,
            'Gopf', 120126,
            'gopf', 120152,
            'grave', 96,
            'GreaterEqual', 8805,
            'GreaterEqualLess', 8923,
            'GreaterFullEqual', 8807,
            'GreaterGreater', 10914,
            'GreaterLess', 8823,
            'GreaterSlantEqual', 10878,
            'GreaterTilde', 8819,
            'Gscr', 119970,
            'gscr', 8458,
            'gsim', 8819,
            'gsime', 10894,
            'gsiml', 10896,
            'GT', 62,
            'Gt', 8811,
            'gt', 62,
            'gtcc', 10919,
            'gtcir', 10874,
            'gtdot', 8919,
            'gtlPar', 10645,
            'gtquest', 10876,
            'gtrapprox', 10886,
            'gtrarr', 10616,
            'gtrdot', 8919,
            'gtreqless', 8923,
            'gtreqqless', 10892,
            'gtrless', 8823,
            'gtrsim', 8819,
            'Hacek', 711,
            'hairsp', 8202,
            'half', 189,
            'hamilt', 8459,
            'HARDcy', 1066,
            'hardcy', 1098,
            'hArr', 8660,
            'harr', 8596,
            'harrcir', 10568,
            'harrw', 8621,
            'Hat', 94,
            'hbar', 8463,
            'Hcirc', 292,
            'hcirc', 293,
            'hearts', 9829,
            'heartsuit', 9829,
            'hellip', 8230,
            'hercon', 8889,
            'Hfr', 8460,
            'hfr', 120101,
            'HilbertSpace', 8459,
            'hksearow', 10533,
            'hkswarow', 10534,
            'hoarr', 8703,
            'homtht', 8763,
            'hookleftarrow', 8617,
            'hookrightarrow', 8618,
            'Hopf', 8461,
            'hopf', 120153,
            'horbar', 8213,
            'HorizontalLine', 9472,
            'Hscr', 8459,
            'hscr', 119997,
            'hslash', 8463,
            'Hstrok', 294,
            'hstrok', 295,
            'HumpDownHump', 8782,
            'HumpEqual', 8783,
            'hybull', 8259,
            'hyphen', 8208,
            'Iacute', 205,
            'iacute', 237,
            'ic', 8291,
            'Icirc', 206,
            'icirc', 238,
            'Icy', 1048,
            'icy', 1080,
            'Idot', 304,
            'IEcy', 1045,
            'iecy', 1077,
            'iexcl', 161,
            'iff', 8660,
            'Ifr', 8465,
            'ifr', 120102,
            'Igrave', 204,
            'igrave', 236,
            'ii', 8520,
            'iiiint', 10764,
            'iiint', 8749,
            'iinfin', 10716,
            'iiota', 8489,
            'IJlig', 306,
            'ijlig', 307,
            'Im', 8465,
            'Imacr', 298,
            'imacr', 299,
            'image', 8465,
            'ImaginaryI', 8520,
            'imagline', 8464,
            'imagpart', 8465,
            'imath', 305,
            'imof', 8887,
            'imped', 437,
            'Implies', 8658,
            'in', 8712,
            'incare', 8453,
            'infin', 8734,
            'infintie', 10717,
            'inodot', 305,
            'Int', 8748,
            'int', 8747,
            'intcal', 8890,
            'integers', 8484,
            'Integral', 8747,
            'intercal', 8890,
            'Intersection', 8898,
            'intlarhk', 10775,
            'intprod', 10812,
            'InvisibleComma', 8291,
            'InvisibleTimes', 8290,
            'IOcy', 1025,
            'iocy', 1105,
            'Iogon', 302,
            'iogon', 303,
            'Iopf', 120128,
            'iopf', 120154,
            'Iota', 921,
            'iota', 953,
            'iprod', 10812,
            'iquest', 191,
            'Iscr', 8464,
            'iscr', 119998,
            'isin', 8712,
            'isindot', 8949,
            'isinE', 8953,
            'isins', 8948,
            'isinsv', 8947,
            'isinv', 8712,
            'it', 8290,
            'Itilde', 296,
            'itilde', 297,
            'Iukcy', 1030,
            'iukcy', 1110,
            'Iuml', 207,
            'iuml', 239,
            'Jcirc', 308,
            'jcirc', 309,
            'Jcy', 1049,
            'jcy', 1081,
            'Jfr', 120077,
            'jfr', 120103,
            'jmath', 567,
            'Jopf', 120129,
            'jopf', 120155,
            'Jscr', 119973,
            'jscr', 119999,
            'Jsercy', 1032,
            'jsercy', 1112,
            'Jukcy', 1028,
            'jukcy', 1108,
            'Kappa', 922,
            'kappa', 954,
            'kappav', 1008,
            'Kcedil', 310,
            'kcedil', 311,
            'Kcy', 1050,
            'kcy', 1082,
            'Kfr', 120078,
            'kfr', 120104,
            'kgreen', 312,
            'KHcy', 1061,
            'khcy', 1093,
            'KJcy', 1036,
            'kjcy', 1116,
            'Kopf', 120130,
            'kopf', 120156,
            'Kscr', 119974,
            'kscr', 120000,
            'lAarr', 8666,
            'Lacute', 313,
            'lacute', 314,
            'laemptyv', 10676,
            'lagran', 8466,
            'Lambda', 923,
            'lambda', 955,
            'Lang', 10218,
            'lang', 10216,
            'langd', 10641,
            'langle', 10216,
            'lap', 10885,
            'Laplacetrf', 8466,
            'laquo', 171,
            'Larr', 8606,
            'lArr', 8656,
            'larr', 8592,
            'larrb', 8676,
            'larrbfs', 10527,
            'larrfs', 10525,
            'larrhk', 8617,
            'larrlp', 8619,
            'larrpl', 10553,
            'larrsim', 10611,
            'larrtl', 8610,
            'lat', 10923,
            'lAtail', 10523,
            'latail', 10521,
            'late', 10925,
            'lBarr', 10510,
            'lbarr', 10508,
            'lbbrk', 10098,
            'lbrace', 123,
            'lbrack', 91,
            'lbrke', 10635,
            'lbrksld', 10639,
            'lbrkslu', 10637,
            'Lcaron', 317,
            'lcaron', 318,
            'Lcedil', 315,
            'lcedil', 316,
            'lceil', 8968,
            'lcub', 123,
            'Lcy', 1051,
            'lcy', 1083,
            'ldca', 10550,
            'ldquo', 8220,
            'ldquor', 8222,
            'ldrdhar', 10599,
            'ldrushar', 10571,
            'ldsh', 8626,
            'lE', 8806,
            'le', 8804,
            'LeftAngleBracket', 10216,
            'LeftArrow', 8592,
            'Leftarrow', 8656,
            'leftarrow', 8592,
            'LeftArrowBar', 8676,
            'LeftArrowRightArrow', 8646,
            'leftarrowtail', 8610,
            'LeftCeiling', 8968,
            'LeftDoubleBracket', 10214,
            'LeftDownTeeVector', 10593,
            'LeftDownVector', 8643,
            'LeftDownVectorBar', 10585,
            'LeftFloor', 8970,
            'leftharpoondown', 8637,
            'leftharpoonup', 8636,
            'leftleftarrows', 8647,
            'LeftRightArrow', 8596,
            'Leftrightarrow', 8660,
            'leftrightarrow', 8596,
            'leftrightarrows', 8646,
            'leftrightharpoons', 8651,
            'leftrightsquigarrow', 8621,
            'LeftRightVector', 10574,
            'LeftTee', 8867,
            'LeftTeeArrow', 8612,
            'LeftTeeVector', 10586,
            'leftthreetimes', 8907,
            'LeftTriangle', 8882,
            'LeftTriangleBar', 10703,
            'LeftTriangleEqual', 8884,
            'LeftUpDownVector', 10577,
            'LeftUpTeeVector', 10592,
            'LeftUpVector', 8639,
            'LeftUpVectorBar', 10584,
            'LeftVector', 8636,
            'LeftVectorBar', 10578,
            'lEg', 10891,
            'leg', 8922,
            'leq', 8804,
            'leqq', 8806,
            'leqslant', 10877,
            'les', 10877,
            'lescc', 10920,
            'lesdot', 10879,
            'lesdoto', 10881,
            'lesdotor', 10883,
            'lesges', 10899,
            'lessapprox', 10885,
            'lessdot', 8918,
            'lesseqgtr', 8922,
            'lesseqqgtr', 10891,
            'LessEqualGreater', 8922,
            'LessFullEqual', 8806,
            'LessGreater', 8822,
            'lessgtr', 8822,
            'LessLess', 10913,
            'lesssim', 8818,
            'LessSlantEqual', 10877,
            'LessTilde', 8818,
            'lfisht', 10620,
            'lfloor', 8970,
            'Lfr', 120079,
            'lfr', 120105,
            'lg', 8822,
            'lgE', 10897,
            'lHar', 10594,
            'lhard', 8637,
            'lharu', 8636,
            'lharul', 10602,
            'lhblk', 9604,
            'LJcy', 1033,
            'ljcy', 1113,
            'Ll', 8920,
            'll', 8810,
            'llarr', 8647,
            'llcorner', 8990,
            'Lleftarrow', 8666,
            'llhard', 10603,
            'lltri', 9722,
            'Lmidot', 319,
            'lmidot', 320,
            'lmoust', 9136,
            'lmoustache', 9136,
            'lnap', 10889,
            'lnapprox', 10889,
            'lnE', 8808,
            'lne', 10887,
            'lneq', 10887,
            'lneqq', 8808,
            'lnsim', 8934,
            'loang', 10220,
            'loarr', 8701,
            'lobrk', 10214,
            'LongLeftArrow', 10229,
            'Longleftarrow', 10232,
            'longleftarrow', 10229,
            'LongLeftRightArrow', 10231,
            'Longleftrightarrow', 10234,
            'longleftrightarrow', 10231,
            'longmapsto', 10236,
            'LongRightArrow', 10230,
            'Longrightarrow', 10233,
            'longrightarrow', 10230,
            'looparrowleft', 8619,
            'looparrowright', 8620,
            'lopar', 10629,
            'Lopf', 120131,
            'lopf', 120157,
            'loplus', 10797,
            'lotimes', 10804,
            'lowast', 8727,
            'lowbar', 95,
            'LowerLeftArrow', 8601,
            'LowerRightArrow', 8600,
            'loz', 9674,
            'lozenge', 9674,
            'lozf', 10731,
            'lpar', 40,
            'lparlt', 10643,
            'lrarr', 8646,
            'lrcorner', 8991,
            'lrhar', 8651,
            'lrhard', 10605,
            'lrm', 8206,
            'lrtri', 8895,
            'lsaquo', 8249,
            'Lscr', 8466,
            'lscr', 120001,
            'Lsh', 8624,
            'lsh', 8624,
            'lsim', 8818,
            'lsime', 10893,
            'lsimg', 10895,
            'lsqb', 91,
            'lsquo', 8216,
            'lsquor', 8218,
            'Lstrok', 321,
            'lstrok', 322,
            'LT', 60,
            'Lt', 8810,
            'lt', 60,
            'ltcc', 10918,
            'ltcir', 10873,
            'ltdot', 8918,
            'lthree', 8907,
            'ltimes', 8905,
            'ltlarr', 10614,
            'ltquest', 10875,
            'ltri', 9667,
            'ltrie', 8884,
            'ltrif', 9666,
            'ltrPar', 10646,
            'lurdshar', 10570,
            'luruhar', 10598,
            'macr', 175,
            'male', 9794,
            'malt', 10016,
            'maltese', 10016,
            'Map', 10501,
            'map', 8614,
            'mapsto', 8614,
            'mapstodown', 8615,
            'mapstoleft', 8612,
            'mapstoup', 8613,
            'marker', 9646,
            'mcomma', 10793,
            'Mcy', 1052,
            'mcy', 1084,
            'mdash', 8212,
            'mDDot', 8762,
            'measuredangle', 8737,
            'MediumSpace', 8287,
            'Mellintrf', 8499,
            'Mfr', 120080,
            'mfr', 120106,
            'mho', 8487,
            'micro', 181,
            'mid', 8739,
            'midast', 42,
            'midcir', 10992,
            'middot', 183,
            'minus', 8722,
            'minusb', 8863,
            'minusd', 8760,
            'minusdu', 10794,
            'MinusPlus', 8723,
            'mlcp', 10971,
            'mldr', 8230,
            'mnplus', 8723,
            'models', 8871,
            'Mopf', 120132,
            'mopf', 120158,
            'mp', 8723,
            'Mscr', 8499,
            'mscr', 120002,
            'mstpos', 8766,
            'Mu', 924,
            'mu', 956,
            'multimap', 8888,
            'mumap', 8888,
            'nabla', 8711,
            'Nacute', 323,
            'nacute', 324,
            'nap', 8777,
            'napos', 329,
            'napprox', 8777,
            'natur', 9838,
            'natural', 9838,
            'naturals', 8469,
            'nbsp', 160,
            'ncap', 10819,
            'Ncaron', 327,
            'ncaron', 328,
            'Ncedil', 325,
            'ncedil', 326,
            'ncong', 8775,
            'ncup', 10818,
            'Ncy', 1053,
            'ncy', 1085,
            'ndash', 8211,
            'ne', 8800,
            'nearhk', 10532,
            'neArr', 8663,
            'nearr', 8599,
            'nearrow', 8599,
            'NegativeMediumSpace', 8203,
            'NegativeThickSpace', 8203,
            'NegativeThinSpace', 8203,
            'NegativeVeryThinSpace', 8203,
            'nequiv', 8802,
            'nesear', 10536,
            'NestedGreaterGreater', 8811,
            'NestedLessLess', 8810,
            'NewLine', 10,
            'nexist', 8708,
            'nexists', 8708,
            'Nfr', 120081,
            'nfr', 120107,
            'nge', 8817,
            'ngeq', 8817,
            'ngsim', 8821,
            'ngt', 8815,
            'ngtr', 8815,
            'nhArr', 8654,
            'nharr', 8622,
            'nhpar', 10994,
            'ni', 8715,
            'nis', 8956,
            'nisd', 8954,
            'niv', 8715,
            'NJcy', 1034,
            'njcy', 1114,
            'nlArr', 8653,
            'nlarr', 8602,
            'nldr', 8229,
            'nle', 8816,
            'nLeftarrow', 8653,
            'nleftarrow', 8602,
            'nLeftrightarrow', 8654,
            'nleftrightarrow', 8622,
            'nleq', 8816,
            'nless', 8814,
            'nlsim', 8820,
            'nlt', 8814,
            'nltri', 8938,
            'nltrie', 8940,
            'nmid', 8740,
            'NoBreak', 8288,
            'NonBreakingSpace', 160,
            'Nopf', 8469,
            'nopf', 120159,
            'Not', 10988,
            'not', 172,
            'NotCongruent', 8802,
            'NotCupCap', 8813,
            'NotDoubleVerticalBar', 8742,
            'NotElement', 8713,
            'NotEqual', 8800,
            'NotExists', 8708,
            'NotGreater', 8815,
            'NotGreaterEqual', 8817,
            'NotGreaterLess', 8825,
            'NotGreaterTilde', 8821,
            'notin', 8713,
            'notinva', 8713,
            'notinvb', 8951,
            'notinvc', 8950,
            'NotLeftTriangle', 8938,
            'NotLeftTriangleEqual', 8940,
            'NotLess', 8814,
            'NotLessEqual', 8816,
            'NotLessGreater', 8824,
            'NotLessTilde', 8820,
            'notni', 8716,
            'notniva', 8716,
            'notnivb', 8958,
            'notnivc', 8957,
            'NotPrecedes', 8832,
            'NotPrecedesSlantEqual', 8928,
            'NotReverseElement', 8716,
            'NotRightTriangle', 8939,
            'NotRightTriangleEqual', 8941,
            'NotSquareSubsetEqual', 8930,
            'NotSquareSupersetEqual', 8931,
            'NotSubsetEqual', 8840,
            'NotSucceeds', 8833,
            'NotSucceedsSlantEqual', 8929,
            'NotSupersetEqual', 8841,
            'NotTilde', 8769,
            'NotTildeEqual', 8772,
            'NotTildeFullEqual', 8775,
            'NotTildeTilde', 8777,
            'NotVerticalBar', 8740,
            'npar', 8742,
            'nparallel', 8742,
            'npolint', 10772,
            'npr', 8832,
            'nprcue', 8928,
            'nprec', 8832,
            'nrArr', 8655,
            'nrarr', 8603,
            'nRightarrow', 8655,
            'nrightarrow', 8603,
            'nrtri', 8939,
            'nrtrie', 8941,
            'nsc', 8833,
            'nsccue', 8929,
            'Nscr', 119977,
            'nscr', 120003,
            'nshortmid', 8740,
            'nshortparallel', 8742,
            'nsim', 8769,
            'nsime', 8772,
            'nsimeq', 8772,
            'nsmid', 8740,
            'nspar', 8742,
            'nsqsube', 8930,
            'nsqsupe', 8931,
            'nsub', 8836,
            'nsube', 8840,
            'nsubseteq', 8840,
            'nsucc', 8833,
            'nsup', 8837,
            'nsupe', 8841,
            'nsupseteq', 8841,
            'ntgl', 8825,
            'Ntilde', 209,
            'ntilde', 241,
            'ntlg', 8824,
            'ntriangleleft', 8938,
            'ntrianglelefteq', 8940,
            'ntriangleright', 8939,
            'ntrianglerighteq', 8941,
            'Nu', 925,
            'nu', 957,
            'num', 35,
            'numero', 8470,
            'numsp', 8199,
            'nVDash', 8879,
            'nVdash', 8878,
            'nvDash', 8877,
            'nvdash', 8876,
            'nvHarr', 10500,
            'nvinfin', 10718,
            'nvlArr', 10498,
            'nvrArr', 10499,
            'nwarhk', 10531,
            'nwArr', 8662,
            'nwarr', 8598,
            'nwarrow', 8598,
            'nwnear', 10535,
            'Oacute', 211,
            'oacute', 243,
            'oast', 8859,
            'ocir', 8858,
            'Ocirc', 212,
            'ocirc', 244,
            'Ocy', 1054,
            'ocy', 1086,
            'odash', 8861,
            'Odblac', 336,
            'odblac', 337,
            'odiv', 10808,
            'odot', 8857,
            'odsold', 10684,
            'OElig', 338,
            'oelig', 339,
            'ofcir', 10687,
            'Ofr', 120082,
            'ofr', 120108,
            'ogon', 731,
            'Ograve', 210,
            'ograve', 242,
            'ogt', 10689,
            'ohbar', 10677,
            'ohm', 937,
            'oint', 8750,
            'olarr', 8634,
            'olcir', 10686,
            'olcross', 10683,
            'oline', 8254,
            'olt', 10688,
            'Omacr', 332,
            'omacr', 333,
            'Omega', 937,
            'omega', 969,
            'Omicron', 927,
            'omicron', 959,
            'omid', 10678,
            'ominus', 8854,
            'Oopf', 120134,
            'oopf', 120160,
            'opar', 10679,
            'OpenCurlyDoubleQuote', 8220,
            'OpenCurlyQuote', 8216,
            'operp', 10681,
            'oplus', 8853,
            'Or', 10836,
            'or', 8744,
            'orarr', 8635,
            'ord', 10845,
            'order', 8500,
            'orderof', 8500,
            'ordf', 170,
            'ordm', 186,
            'origof', 8886,
            'oror', 10838,
            'orslope', 10839,
            'orv', 10843,
            'oS', 9416,
            'Oscr', 119978,
            'oscr', 8500,
            'Oslash', 216,
            'oslash', 248,
            'osol', 8856,
            'Otilde', 213,
            'otilde', 245,
            'Otimes', 10807,
            'otimes', 8855,
            'otimesas', 10806,
            'Ouml', 214,
            'ouml', 246,
            'ovbar', 9021,
            'OverBar', 8254,
            'OverBrace', 9182,
            'OverBracket', 9140,
            'OverParenthesis', 9180,
            'par', 8741,
            'para', 182,
            'parallel', 8741,
            'parsim', 10995,
            'parsl', 11005,
            'part', 8706,
            'PartialD', 8706,
            'Pcy', 1055,
            'pcy', 1087,
            'percnt', 37,
            'period', 46,
            'permil', 8240,
            'perp', 8869,
            'pertenk', 8241,
            'Pfr', 120083,
            'pfr', 120109,
            'Phi', 934,
            'phi', 966,
            'phiv', 981,
            'phmmat', 8499,
            'phone', 9742,
            'Pi', 928,
            'pi', 960,
            'pitchfork', 8916,
            'piv', 982,
            'planck', 8463,
            'planckh', 8462,
            'plankv', 8463,
            'plus', 43,
            'plusacir', 10787,
            'plusb', 8862,
            'pluscir', 10786,
            'plusdo', 8724,
            'plusdu', 10789,
            'pluse', 10866,
            'PlusMinus', 177,
            'plusmn', 177,
            'plussim', 10790,
            'plustwo', 10791,
            'pm', 177,
            'Poincareplane', 8460,
            'pointint', 10773,
            'Popf', 8473,
            'popf', 120161,
            'pound', 163,
            'Pr', 10939,
            'pr', 8826,
            'prap', 10935,
            'prcue', 8828,
            'prE', 10931,
            'pre', 10927,
            'prec', 8826,
            'precapprox', 10935,
            'preccurlyeq', 8828,
            'Precedes', 8826,
            'PrecedesEqual', 10927,
            'PrecedesSlantEqual', 8828,
            'PrecedesTilde', 8830,
            'preceq', 10927,
            'precnapprox', 10937,
            'precneqq', 10933,
            'precnsim', 8936,
            'precsim', 8830,
            'Prime', 8243,
            'prime', 8242,
            'primes', 8473,
            'prnap', 10937,
            'prnE', 10933,
            'prnsim', 8936,
            'prod', 8719,
            'Product', 8719,
            'profalar', 9006,
            'profline', 8978,
            'profsurf', 8979,
            'prop', 8733,
            'Proportion', 8759,
            'Proportional', 8733,
            'propto', 8733,
            'prsim', 8830,
            'prurel', 8880,
            'Pscr', 119979,
            'pscr', 120005,
            'Psi', 936,
            'psi', 968,
            'puncsp', 8200,
            'Qfr', 120084,
            'qfr', 120110,
            'qint', 10764,
            'Qopf', 8474,
            'qopf', 120162,
            'qprime', 8279,
            'Qscr', 119980,
            'qscr', 120006,
            'quaternions', 8461,
            'quatint', 10774,
            'quest', 63,
            'questeq', 8799,
            'QUOT', 34,
            'quot', 34,
            'rAarr', 8667,
            'Racute', 340,
            'racute', 341,
            'radic', 8730,
            'raemptyv', 10675,
            'Rang', 10219,
            'rang', 10217,
            'rangd', 10642,
            'range', 10661,
            'rangle', 10217,
            'raquo', 187,
            'Rarr', 8608,
            'rArr', 8658,
            'rarr', 8594,
            'rarrap', 10613,
            'rarrb', 8677,
            'rarrbfs', 10528,
            'rarrc', 10547,
            'rarrfs', 10526,
            'rarrhk', 8618,
            'rarrlp', 8620,
            'rarrpl', 10565,
            'rarrsim', 10612,
            'Rarrtl', 10518,
            'rarrtl', 8611,
            'rarrw', 8605,
            'rAtail', 10524,
            'ratail', 10522,
            'ratio', 8758,
            'rationals', 8474,
            'RBarr', 10512,
            'rBarr', 10511,
            'rbarr', 10509,
            'rbbrk', 10099,
            'rbrace', 125,
            'rbrack', 93,
            'rbrke', 10636,
            'rbrksld', 10638,
            'rbrkslu', 10640,
            'Rcaron', 344,
            'rcaron', 345,
            'Rcedil', 342,
            'rcedil', 343,
            'rceil', 8969,
            'rcub', 125,
            'Rcy', 1056,
            'rcy', 1088,
            'rdca', 10551,
            'rdldhar', 10601,
            'rdquo', 8221,
            'rdquor', 8221,
            'rdsh', 8627,
            'Re', 8476,
            'real', 8476,
            'realine', 8475,
            'realpart', 8476,
            'reals', 8477,
            'rect', 9645,
            'REG', 174,
            'reg', 174,
            'ReverseElement', 8715,
            'ReverseEquilibrium', 8651,
            'ReverseUpEquilibrium', 10607,
            'rfisht', 10621,
            'rfloor', 8971,
            'Rfr', 8476,
            'rfr', 120111,
            'rHar', 10596,
            'rhard', 8641,
            'rharu', 8640,
            'rharul', 10604,
            'Rho', 929,
            'rho', 961,
            'rhov', 1009,
            'RightAngleBracket', 10217,
            'RightArrow', 8594,
            'Rightarrow', 8658,
            'rightarrow', 8594,
            'RightArrowBar', 8677,
            'RightArrowLeftArrow', 8644,
            'rightarrowtail', 8611,
            'RightCeiling', 8969,
            'RightDoubleBracket', 10215,
            'RightDownTeeVector', 10589,
            'RightDownVector', 8642,
            'RightDownVectorBar', 10581,
            'RightFloor', 8971,
            'rightharpoondown', 8641,
            'rightharpoonup', 8640,
            'rightleftarrows', 8644,
            'rightleftharpoons', 8652,
            'rightrightarrows', 8649,
            'rightsquigarrow', 8605,
            'RightTee', 8866,
            'RightTeeArrow', 8614,
            'RightTeeVector', 10587,
            'rightthreetimes', 8908,
            'RightTriangle', 8883,
            'RightTriangleBar', 10704,
            'RightTriangleEqual', 8885,
            'RightUpDownVector', 10575,
            'RightUpTeeVector', 10588,
            'RightUpVector', 8638,
            'RightUpVectorBar', 10580,
            'RightVector', 8640,
            'RightVectorBar', 10579,
            'ring', 730,
            'risingdotseq', 8787,
            'rlarr', 8644,
            'rlhar', 8652,
            'rlm', 8207,
            'rmoust', 9137,
            'rmoustache', 9137,
            'rnmid', 10990,
            'roang', 10221,
            'roarr', 8702,
            'robrk', 10215,
            'ropar', 10630,
            'Ropf', 8477,
            'ropf', 120163,
            'roplus', 10798,
            'rotimes', 10805,
            'RoundImplies', 10608,
            'rpar', 41,
            'rpargt', 10644,
            'rppolint', 10770,
            'rrarr', 8649,
            'Rrightarrow', 8667,
            'rsaquo', 8250,
            'Rscr', 8475,
            'rscr', 120007,
            'Rsh', 8625,
            'rsh', 8625,
            'rsqb', 93,
            'rsquo', 8217,
            'rsquor', 8217,
            'rthree', 8908,
            'rtimes', 8906,
            'rtri', 9657,
            'rtrie', 8885,
            'rtrif', 9656,
            'rtriltri', 10702,
            'RuleDelayed', 10740,
            'ruluhar', 10600,
            'rx', 8478,
            'Sacute', 346,
            'sacute', 347,
            'sbquo', 8218,
            'Sc', 10940,
            'sc', 8827,
            'scap', 10936,
            'Scaron', 352,
            'scaron', 353,
            'sccue', 8829,
            'scE', 10932,
            'sce', 10928,
            'Scedil', 350,
            'scedil', 351,
            'Scirc', 348,
            'scirc', 349,
            'scnap', 10938,
            'scnE', 10934,
            'scnsim', 8937,
            'scpolint', 10771,
            'scsim', 8831,
            'Scy', 1057,
            'scy', 1089,
            'sdot', 8901,
            'sdotb', 8865,
            'sdote', 10854,
            'searhk', 10533,
            'seArr', 8664,
            'searr', 8600,
            'searrow', 8600,
            'sect', 167,
            'semi', 59,
            'seswar', 10537,
            'setminus', 8726,
            'setmn', 8726,
            'sext', 10038,
            'Sfr', 120086,
            'sfr', 120112,
            'sfrown', 8994,
            'sharp', 9839,
            'SHCHcy', 1065,
            'shchcy', 1097,
            'SHcy', 1064,
            'shcy', 1096,
            'ShortDownArrow', 8595,
            'ShortLeftArrow', 8592,
            'shortmid', 8739,
            'shortparallel', 8741,
            'ShortRightArrow', 8594,
            'ShortUpArrow', 8593,
            'shy', 173,
            'Sigma', 931,
            'sigma', 963,
            'sigmaf', 962,
            'sigmav', 962,
            'sim', 8764,
            'simdot', 10858,
            'sime', 8771,
            'simeq', 8771,
            'simg', 10910,
            'simgE', 10912,
            'siml', 10909,
            'simlE', 10911,
            'simne', 8774,
            'simplus', 10788,
            'simrarr', 10610,
            'slarr', 8592,
            'SmallCircle', 8728,
            'smallsetminus', 8726,
            'smashp', 10803,
            'smeparsl', 10724,
            'smid', 8739,
            'smile', 8995,
            'smt', 10922,
            'smte', 10924,
            'SOFTcy', 1068,
            'softcy', 1100,
            'sol', 47,
            'solb', 10692,
            'solbar', 9023,
            'Sopf', 120138,
            'sopf', 120164,
            'spades', 9824,
            'spadesuit', 9824,
            'spar', 8741,
            'sqcap', 8851,
            'sqcup', 8852,
            'Sqrt', 8730,
            'sqsub', 8847,
            'sqsube', 8849,
            'sqsubset', 8847,
            'sqsubseteq', 8849,
            'sqsup', 8848,
            'sqsupe', 8850,
            'sqsupset', 8848,
            'sqsupseteq', 8850,
            'squ', 9633,
            'Square', 9633,
            'square', 9633,
            'SquareIntersection', 8851,
            'SquareSubset', 8847,
            'SquareSubsetEqual', 8849,
            'SquareSuperset', 8848,
            'SquareSupersetEqual', 8850,
            'SquareUnion', 8852,
            'squarf', 9642,
            'squf', 9642,
            'srarr', 8594,
            'Sscr', 119982,
            'sscr', 120008,
            'ssetmn', 8726,
            'ssmile', 8995,
            'sstarf', 8902,
            'Star', 8902,
            'star', 9734,
            'starf', 9733,
            'straightepsilon', 1013,
            'straightphi', 981,
            'strns', 175,
            'Sub', 8912,
            'sub', 8834,
            'subdot', 10941,
            'subE', 10949,
            'sube', 8838,
            'subedot', 10947,
            'submult', 10945,
            'subnE', 10955,
            'subne', 8842,
            'subplus', 10943,
            'subrarr', 10617,
            'Subset', 8912,
            'subset', 8834,
            'subseteq', 8838,
            'subseteqq', 10949,
            'SubsetEqual', 8838,
            'subsetneq', 8842,
            'subsetneqq', 10955,
            'subsim', 10951,
            'subsub', 10965,
            'subsup', 10963,
            'succ', 8827,
            'succapprox', 10936,
            'succcurlyeq', 8829,
            'Succeeds', 8827,
            'SucceedsEqual', 10928,
            'SucceedsSlantEqual', 8829,
            'SucceedsTilde', 8831,
            'succeq', 10928,
            'succnapprox', 10938,
            'succneqq', 10934,
            'succnsim', 8937,
            'succsim', 8831,
            'SuchThat', 8715,
            'Sum', 8721,
            'sum', 8721,
            'sung', 9834,
            'Sup', 8913,
            'sup', 8835,
            'sup1', 185,
            'sup2', 178,
            'sup3', 179,
            'supdot', 10942,
            'supdsub', 10968,
            'supE', 10950,
            'supe', 8839,
            'supedot', 10948,
            'Superset', 8835,
            'SupersetEqual', 8839,
            'suphsol', 10185,
            'suphsub', 10967,
            'suplarr', 10619,
            'supmult', 10946,
            'supnE', 10956,
            'supne', 8843,
            'supplus', 10944,
            'Supset', 8913,
            'supset', 8835,
            'supseteq', 8839,
            'supseteqq', 10950,
            'supsetneq', 8843,
            'supsetneqq', 10956,
            'supsim', 10952,
            'supsub', 10964,
            'supsup', 10966,
            'swarhk', 10534,
            'swArr', 8665,
            'swarr', 8601,
            'swarrow', 8601,
            'swnwar', 10538,
            'szlig', 223,
            'Tab', 9,
            'target', 8982,
            'Tau', 932,
            'tau', 964,
            'tbrk', 9140,
            'Tcaron', 356,
            'tcaron', 357,
            'Tcedil', 354,
            'tcedil', 355,
            'Tcy', 1058,
            'tcy', 1090,
            'tdot', 8411,
            'telrec', 8981,
            'Tfr', 120087,
            'tfr', 120113,
            'there4', 8756,
            'Therefore', 8756,
            'therefore', 8756,
            'Theta', 920,
            'theta', 952,
            'thetasym', 977,
            'thetav', 977,
            'thickapprox', 8776,
            'thicksim', 8764,
            'thinsp', 8201,
            'ThinSpace', 8201,
            'thkap', 8776,
            'thksim', 8764,
            'THORN', 222,
            'thorn', 254,
            'Tilde', 8764,
            'tilde', 732,
            'TildeEqual', 8771,
            'TildeFullEqual', 8773,
            'TildeTilde', 8776,
            'times', 215,
            'timesb', 8864,
            'timesbar', 10801,
            'timesd', 10800,
            'tint', 8749,
            'toea', 10536,
            'top', 8868,
            'topbot', 9014,
            'topcir', 10993,
            'Topf', 120139,
            'topf', 120165,
            'topfork', 10970,
            'tosa', 10537,
            'tprime', 8244,
            'TRADE', 8482,
            'trade', 8482,
            'triangle', 9653,
            'triangledown', 9663,
            'triangleleft', 9667,
            'trianglelefteq', 8884,
            'triangleq', 8796,
            'triangleright', 9657,
            'trianglerighteq', 8885,
            'tridot', 9708,
            'trie', 8796,
            'triminus', 10810,
            'TripleDot', 8411,
            'triplus', 10809,
            'trisb', 10701,
            'tritime', 10811,
            'trpezium', 9186,
            'Tscr', 119983,
            'tscr', 120009,
            'TScy', 1062,
            'tscy', 1094,
            'TSHcy', 1035,
            'tshcy', 1115,
            'Tstrok', 358,
            'tstrok', 359,
            'twixt', 8812,
            'twoheadleftarrow', 8606,
            'twoheadrightarrow', 8608,
            'Uacute', 218,
            'uacute', 250,
            'Uarr', 8607,
            'uArr', 8657,
            'uarr', 8593,
            'Uarrocir', 10569,
            'Ubrcy', 1038,
            'ubrcy', 1118,
            'Ubreve', 364,
            'ubreve', 365,
            'Ucirc', 219,
            'ucirc', 251,
            'Ucy', 1059,
            'ucy', 1091,
            'udarr', 8645,
            'Udblac', 368,
            'udblac', 369,
            'udhar', 10606,
            'ufisht', 10622,
            'Ufr', 120088,
            'ufr', 120114,
            'Ugrave', 217,
            'ugrave', 249,
            'uHar', 10595,
            'uharl', 8639,
            'uharr', 8638,
            'uhblk', 9600,
            'ulcorn', 8988,
            'ulcorner', 8988,
            'ulcrop', 8975,
            'ultri', 9720,
            'Umacr', 362,
            'umacr', 363,
            'uml', 168,
            'UnderBar', 95,
            'UnderBrace', 9183,
            'UnderBracket', 9141,
            'UnderParenthesis', 9181,
            'Union', 8899,
            'UnionPlus', 8846,
            'Uogon', 370,
            'uogon', 371,
            'Uopf', 120140,
            'uopf', 120166,
            'UpArrow', 8593,
            'Uparrow', 8657,
            'uparrow', 8593,
            'UpArrowBar', 10514,
            'UpArrowDownArrow', 8645,
            'UpDownArrow', 8597,
            'Updownarrow', 8661,
            'updownarrow', 8597,
            'UpEquilibrium', 10606,
            'upharpoonleft', 8639,
            'upharpoonright', 8638,
            'uplus', 8846,
            'UpperLeftArrow', 8598,
            'UpperRightArrow', 8599,
            'Upsi', 978,
            'upsi', 965,
            'upsih', 978,
            'Upsilon', 933,
            'upsilon', 965,
            'UpTee', 8869,
            'UpTeeArrow', 8613,
            'upuparrows', 8648,
            'urcorn', 8989,
            'urcorner', 8989,
            'urcrop', 8974,
            'Uring', 366,
            'uring', 367,
            'urtri', 9721,
            'Uscr', 119984,
            'uscr', 120010,
            'utdot', 8944,
            'Utilde', 360,
            'utilde', 361,
            'utri', 9653,
            'utrif', 9652,
            'uuarr', 8648,
            'Uuml', 220,
            'uuml', 252,
            'uwangle', 10663,
            'vangrt', 10652,
            'varepsilon', 1013,
            'varkappa', 1008,
            'varnothing', 8709,
            'varphi', 981,
            'varpi', 982,
            'varpropto', 8733,
            'vArr', 8661,
            'varr', 8597,
            'varrho', 1009,
            'varsigma', 962,
            'vartheta', 977,
            'vartriangleleft', 8882,
            'vartriangleright', 8883,
            'Vbar', 10987,
            'vBar', 10984,
            'vBarv', 10985,
            'Vcy', 1042,
            'vcy', 1074,
            'VDash', 8875,
            'Vdash', 8873,
            'vDash', 8872,
            'vdash', 8866,
            'Vdashl', 10982,
            'Vee', 8897,
            'vee', 8744,
            'veebar', 8891,
            'veeeq', 8794,
            'vellip', 8942,
            'Verbar', 8214,
            'verbar', 124,
            'Vert', 8214,
            'vert', 124,
            'VerticalBar', 8739,
            'VerticalLine', 124,
            'VerticalSeparator', 10072,
            'VerticalTilde', 8768,
            'VeryThinSpace', 8202,
            'Vfr', 120089,
            'vfr', 120115,
            'vltri', 8882,
            'Vopf', 120141,
            'vopf', 120167,
            'vprop', 8733,
            'vrtri', 8883,
            'Vscr', 119985,
            'vscr', 120011,
            'Vvdash', 8874,
            'vzigzag', 10650,
            'Wcirc', 372,
            'wcirc', 373,
            'wedbar', 10847,
            'Wedge', 8896,
            'wedge', 8743,
            'wedgeq', 8793,
            'weierp', 8472,
            'Wfr', 120090,
            'wfr', 120116,
            'Wopf', 120142,
            'wopf', 120168,
            'wp', 8472,
            'wr', 8768,
            'wreath', 8768,
            'Wscr', 119986,
            'wscr', 120012,
            'xcap', 8898,
            'xcirc', 9711,
            'xcup', 8899,
            'xdtri', 9661,
            'Xfr', 120091,
            'xfr', 120117,
            'xhArr', 10234,
            'xharr', 10231,
            'Xi', 926,
            'xi', 958,
            'xlArr', 10232,
            'xlarr', 10229,
            'xmap', 10236,
            'xnis', 8955,
            'xodot', 10752,
            'Xopf', 120143,
            'xopf', 120169,
            'xoplus', 10753,
            'xotime', 10754,
            'xrArr', 10233,
            'xrarr', 10230,
            'Xscr', 119987,
            'xscr', 120013,
            'xsqcup', 10758,
            'xuplus', 10756,
            'xutri', 9651,
            'xvee', 8897,
            'xwedge', 8896,
            'Yacute', 221,
            'yacute', 253,
            'YAcy', 1071,
            'yacy', 1103,
            'Ycirc', 374,
            'ycirc', 375,
            'Ycy', 1067,
            'ycy', 1099,
            'yen', 165,
            'Yfr', 120092,
            'yfr', 120118,
            'YIcy', 1031,
            'yicy', 1111,
            'Yopf', 120144,
            'yopf', 120170,
            'Yscr', 119988,
            'yscr', 120014,
            'YUcy', 1070,
            'yucy', 1102,
            'Yuml', 376,
            'yuml', 255,
            'Zacute', 377,
            'zacute', 378,
            'Zcaron', 381,
            'zcaron', 382,
            'Zcy', 1047,
            'zcy', 1079,
            'Zdot', 379,
            'zdot', 380,
            'zeetrf', 8488,
            'ZeroWidthSpace', 8203,
            'Zeta', 918,
            'zeta', 950,
            'Zfr', 8488,
            'zfr', 120119,
            'ZHcy', 1046,
            'zhcy', 1078,
            'zigrarr', 8669,
            'Zopf', 8484,
            'zopf', 120171,
            'Zscr', 119989,
            'zscr', 120015,
            'zwj', 8205,
            'zwnj', 8204,
        );

        #   HTML5 Composed Character References

        #   These are logical characters formed by combining multiple
        #   Unicode code points.  The code points which make up the
        #   character are given in a comma-separated string.

        #   Remember to update $#HTML_COMPOSED_CHARACTER_REFERENCES pre-dimension above!
        @HTML_COMPOSED_CHARACTER_REFERENCES = (
            #   From https://www.w3.org/TR/html5/entities.json
            'acE', '8766, 819',
            'bne', '61, 8421',
            'bnequiv', '8801, 8421',
            'caps', '8745, 65024',
            'cups', '8746, 65024',
            'fjlig', '102, 106',
            'gesl', '8923, 65024',
            'gvertneqq', '8809, 65024',
            'gvnE', '8809, 65024',
            'lates', '10925, 65024',
            'lesg', '8922, 65024',
            'lvertneqq', '8808, 65024',
            'lvnE', '8808, 65024',
            'nang', '8736, 8402',
            'napE', '10864, 824',
            'napid', '8779, 824',
            'nbump', '8782, 824',
            'nbumpe', '8783, 824',
            'ncongdot', '10861, 824',
            'nedot', '8784, 824',
            'nesim', '8770, 824',
            'ngE', '8807, 824',
            'ngeqq', '8807, 824',
            'ngeqslant', '10878, 824',
            'nges', '10878, 824',
            'nGg', '8921, 824',
            'nGt', '8811, 8402',
            'nGtv', '8811, 824',
            'nlE', '8806, 824',
            'nleqq', '8806, 824',
            'nleqslant', '10877, 824',
            'nles', '10877, 824',
            'nLl', '8920, 824',
            'nLt', '8810, 8402',
            'nLtv', '8810, 824',
            'NotEqualTilde', '8770, 824',
            'NotGreaterFullEqual', '8807, 824',
            'NotGreaterGreater', '8811, 824',
            'NotGreaterSlantEqual', '10878, 824',
            'NotHumpDownHump', '8782, 824',
            'NotHumpEqual', '8783, 824',
            'notindot', '8949, 824',
            'notinE', '8953, 824',
            'NotLeftTriangleBar', '10703, 824',
            'NotLessLess', '8810, 824',
            'NotLessSlantEqual', '10877, 824',
            'NotNestedGreaterGreater', '10914, 824',
            'NotNestedLessLess', '10913, 824',
            'NotPrecedesEqual', '10927, 824',
            'NotRightTriangleBar', '10704, 824',
            'NotSquareSubset', '8847, 824',
            'NotSquareSuperset', '8848, 824',
            'NotSubset', '8834, 8402',
            'NotSucceedsEqual', '10928, 824',
            'NotSucceedsTilde', '8831, 824',
            'NotSuperset', '8835, 8402',
            'nparsl', '11005, 8421',
            'npart', '8706, 824',
            'npre', '10927, 824',
            'npreceq', '10927, 824',
            'nrarrc', '10547, 824',
            'nrarrw', '8605, 824',
            'nsce', '10928, 824',
            'nsubE', '10949, 824',
            'nsubset', '8834, 8402',
            'nsubseteqq', '10949, 824',
            'nsucceq', '10928, 824',
            'nsupE', '10950, 824',
            'nsupset', '8835, 8402',
            'nsupseteqq', '10950, 824',
            'nvap', '8781, 8402',
            'nvge', '8805, 8402',
            'nvgt', '62, 8402',
            'nvle', '8804, 8402',
            'nvlt', '60, 8402',
            'nvltrie', '8884, 8402',
            'nvrtrie', '8885, 8402',
            'nvsim', '8764, 8402',
            'race', '8765, 817',
            'smtes', '10924, 65024',
            'sqcaps', '8851, 65024',
            'sqcups', '8852, 65024',
            'ThickSpace', '8287, 8202',
            'varsubsetneq', '8842, 65024',
            'varsubsetneqq', '10955, 65024',
            'varsupsetneq', '8843, 65024',
            'varsupsetneqq', '10956, 65024',
            'vnsub', '8834, 8402',
            'vnsup', '8835, 8402',
            'vsubnE', '10955, 65024',
            'vsubne', '8842, 65024',
            'vsupnE', '10956, 65024',
            'vsupne', '8843, 65024',
        );
    }

__DATA__
