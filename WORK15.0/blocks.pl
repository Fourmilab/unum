
    #	Extract block definitions from Unicode XML database
    
    use strict;
    use warnings;
    
    
    use XML::Parser;
#    use Data::Dumper;
    
    binmode(STDOUT, ":utf8");
    
    print << "EOD";
	\@UNICODE_BLOCKS = (
	#  start   end        block name
EOD
    
    my $parser = new XML::Parser(
    	    Handlers => {
	    	Start => \&p_start,
		End => \&p_end
		},
	    ProtocolEncoding => 'UTF-8'
    	);
	
    $parser->parsefile('ucd.all.flat.xml');
    
    print << "EOD";
	);
EOD
    
    my @e_stack;
    
    sub p_start {
    	my ($expat, $element, %attrs) = @_;
	
	my $line = $expat->current_line;
#	print("Start $element on line $line\n");
	push(@e_stack, { element=>$element, line=>$line });
	
	if ($element eq 'block') {
	    my ($first, $last, $name) =
	    	($attrs{"first-cp"}, $attrs{"last-cp"}, $attrs{name});
		
#print("$name: $first - $last\n");
    	    print << "EOD";
	  [0x$first, 0x$last => '$name'],
EOD
    	}
    }
    
    sub p_end {
    	my ($expat, $element) = @_;
	
	my $element_record = pop(@e_stack);
	
#	print("End $element which started on line ", $$element_record{line},
#	    "\n");
    }
    
#    print(Dumper($chars));
