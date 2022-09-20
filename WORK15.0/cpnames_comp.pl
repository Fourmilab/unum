
    #	Compress cpnames.txt by applying macro compressions
    
    use strict;
    use warnings;
    
    binmode(STDIN, ":utf8");
    binmode(STDOUT, ":utf8");
    
    my @Macros = (
    	'CJK UNIFIED IDEOGRAPH-#', '1',
	'CJK COMPATIBILITY IDEOGRAPH-#', '2',
	'TANGUT IDEOGRAPH-#', '3',
	'SMALL LETTER', '5',
	'CAPITAL LETTER', '6',
	'LETTER', '7',
	'SYLLABICS', '8',
	'IRGKangXi=', '9',
	'RSKangXi=', 'A',
	'SYLLABLE', 'B',
	'CHARACTER', 'C',
	'TGT_MergedSrc=', 'D',
	
    );

    while (my $l = <>) {
    	for (my $i = 0; $i < scalar(@Macros); $i += 2) {
	    my $re = qr/$Macros[$i]/;
	    $l =~ s/$re/&$Macros[$i + 1]/g;
	}
	print($l);
    }
