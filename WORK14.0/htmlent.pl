
    #	Convert database of HTML5 named character references to our format
    
    #	As with everything associated with HTML5, character references
    #	have degenerated to Microsoft-grade crap.  There are now
    #	both multiple names mapped to the same Unicode code point and
    #	names mapped to multiple code points.  Why?  Because some
    #	pointy-head thought it would be cool to be sloppy.  So this
    #	means we can't use a hash in either direction, but must
    #	search a table of mappings.  Note that since Unicode code points
    #	deemed "equivalent" are not guaranteed to be rendered the same
    #	in all fonts on all platforms, there is no way to know when you
    #	use an HTML named character reference which one you're going to
    #	get.
    
    #	Further, the mapping file supplied by the W3C includes identical
    #	text entities with and without the terminating semicolon, for
    #	example:
    #       "&nbsp;": { "codepoints": [160], "characters": "\u00A0" },
    #       "&nbsp": { "codepoints": [160], "characters": "\u00A0" },
    #	I have no idea what this's about other than to demonstrate
    #	that "The stupid is strong in this one".  I ignore the entries
    #	without the closing semicolon.

    
    open(FI, "<entities.json") || die("Cannot open entities.json");
    
    while (my $l = <FI>) {
    	if ($l =~ m/^\s*"&([^;]+);":.*\[(\d+(?:,\s+\d+)*)\]/) {
	    my ($name, $value) = ($1, $2);
#print("$name: ($value)\n");
    	    while ($value =~ s/\s*(\d+)(?:,\s*)?//) {
	    	my $n = $1;
		print("            $n, '$name',\n");
	    }
	}
    }
    
    close(FI);
    
