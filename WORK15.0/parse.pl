
    use strict;
    use warnings;
    
    
    use XML::Parser;
    use Data::Dumper;
    
    binmode(STDOUT, ":utf8");
    
    my $parser = new XML::Parser(
    	    Handlers => {
	    	Start => \&p_start,
		End => \&p_end
		},
	    ProtocolEncoding => 'UTF-8'
    	);
	
    $parser->parsefile('ucd.all.flat.xml');
    
    my @e_stack;
    
    sub p_start {
    	my ($expat, $element, %attrs) = @_;
	
	my $line = $expat->current_line;
	print("Start $element on line $line\n");
	push(@e_stack, { element=>$element, line=>$line });
	
	if (%attrs) {
	    print("Attributes: \n");
	    while (my($key, $value) = each(%attrs)) {
	    	print("    $key => $value\n");
	    }
    	}
    }
    
    sub p_end {
    	my ($expat, $element) = @_;
	
	my $element_record = pop(@e_stack);
	
	print("End $element which started on line ", $$element_record{line},
	    "\n");
    }
    
#    print(Dumper($chars));
