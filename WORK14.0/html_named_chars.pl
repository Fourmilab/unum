
    #	Convert database of HTML5 named character references to our format
    
    #	The mapping file supplied by the W3C includes identical
    #	text entities with and without the terminating semicolon, for
    #	example:
    #       "&nbsp;": { "codepoints": [160], "characters": "\u00A0" },
    #       "&nbsp": { "codepoints": [160], "characters": "\u00A0" },
    #	I have no idea what this's about other than to demonstrate
    #	that "The stupid is strong in this one".  I ignore the entries
    #	without the closing semicolon.

    
    open(FI, "<entities.json") || die("Cannot open entities.json");
    
    #	First build the table uniquely-mapped named character
    #	references.  These names map to a single code point.
    #	Note that there may be multiple names (synonyms) which
    #	map to a single code point.
    
    print("        \@HTML_CHARACTER_REFERENCES = (\n");
    print("            #   From https://www.w3.org/TR/html5/entities.json\n");
    while (my $l = <FI>) {
    	if ($l =~ m/^\s*"&([^;]+);":.*\[(\d+)\]/) {
	    my ($name, $value) = ($1, $2);
	    print("            '$name', $value,\n");
	}
    }
    print("        );\n\n");
    
    seek(FI, 0, 0);
    
    #	Now we deal with the composed character definitions.
    #	These specify multiple (at this writing, never more
    #	than two) code points which are combined to produce
    #	the logical character.  We express the individual code
    #	points as a comma-separated string.
    
    print("        \@HTML_COMPOSED_CHARACTER_REFERENCES = (\n");
    print("            #   From https://www.w3.org/TR/html5/entities.json\n");
    while (my $l = <FI>) {
    	if ($l =~ m/^\s*"&([^;]+);":.*\[(\d+(?:,\s+\d+)+)\]/) {
	    my ($name, $value) = ($1, $2);
	    print("            '$name', '$value',\n");
	}
    }
    print("        );\n");
    
    close(FI);
    
