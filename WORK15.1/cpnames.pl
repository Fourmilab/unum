
    #	Build code point table from Unicode XML database

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
    
    my ($cp, $name, @ctrl, @abbr, @figment, @corr, @alter, $control);
    my ($kIRGKangXi, $kRSKangXi, $kTGT_MergedSrc, $kPrimaryNumeric,
    	$kAccountingNumeric, $kDefinition);
    
    #	Process start of element
    
    sub p_start {
    	my ($expat, $element, %attrs) = @_;
	
	my $line = $expat->current_line;
	push(@e_stack, { element=>$element, line=>$line });
	
#	print("Start $element on line $line\n");
    	if ($element eq "char") {
	    undef $cp;
	    undef $name;
	    undef @ctrl;
	    undef @abbr;
	    undef @figment;
	    undef @corr;
	    undef @alter;
	    undef $control;
	    undef $kIRGKangXi;
	    undef $kRSKangXi;
	    undef $kTGT_MergedSrc;
	    undef $kPrimaryNumeric;
	    undef $kAccountingNumeric;
	    undef $kDefinition;
	    
    	    if ($attrs{cp}) {
	    	$cp = $attrs{cp};
		if ($attrs{na}) {
		    $name = $attrs{na};
		} elsif ($attrs{na1}) {
		    $name = $attrs{na1};
		} else {
		    #die("No name for CP $cp");
		    $name = "XXX";
		}
		
		#   Process any CJK character identifications
		if ($attrs{kIRGKangXi} && ($attrs{kIRGKangXi} ne "")) {
		    $kIRGKangXi = $attrs{kIRGKangXi};
		}
		if ($attrs{kRSKangXi} && ($attrs{kRSKangXi} ne "")) {
		    $kRSKangXi = $attrs{kRSKangXi};
		}
		if ($attrs{kRSKangXi} && ($attrs{kRSKangXi} ne "")) {
		    $kRSKangXi = $attrs{kRSKangXi};
		}
		if ($attrs{kTGT_MergedSrc} && ($attrs{kTGT_MergedSrc} ne "")) {
		    $kTGT_MergedSrc = $attrs{kTGT_MergedSrc};
		}
		if ($attrs{kPrimaryNumeric} && ($attrs{kPrimaryNumeric} ne "")) {
		    $kPrimaryNumeric = $attrs{kPrimaryNumeric};
		}
		if ($attrs{kAccountingNumeric} && ($attrs{kAccountingNumeric} ne "")) {
		    $kAccountingNumeric = $attrs{kAccountingNumeric};
		}
		if ($attrs{kDefinition} && ($attrs{kDefinition} ne "")) {
		    $kDefinition = $attrs{kDefinition};
		}
		
#		print("$cp $n\n");
	    }
	}
	
	#   Save name aliases embedded within the <char> definition.
	#   Note that multiple aliases of the same time may be
	#   present.
	
	if ($element eq "name-alias") {
	    if ($attrs{type} eq "control") {
		$control = 1;
	    }
	    if ($attrs{alias}) {
	    	if ($attrs{type} eq "control") {
		    if ($attrs{alias} ne $name) {
		    	push(@ctrl, $attrs{alias});
		    }
#print("Ctrl:\n", Dumper(@ctrl));
		} elsif ($attrs{type} eq "abbreviation") {
		    push(@abbr, $attrs{alias});
#print("Abbr:\n", Dumper(@abbr));
		} elsif ($attrs{type} eq "figment") {
		    push(@figment, $attrs{alias});
		} elsif ($attrs{type} eq "correction") {
		    push(@corr, $attrs{alias});
		} elsif ($attrs{type} eq "alternate") {
		    push(@alter, $attrs{alias});
		} else {
		    die("Unknown name-alias type $attrs{alias}");
		}
	    }
    	}
    }
    
    #	Process end of element
    
    sub p_end {
    	my ($expat, $element) = @_;
	
	my $element_record = pop(@e_stack);
	
	#   If this is end of a <char> element, emit the character
	#   item, composed of the information from the <char> element
	#   and any nested elements of interest.
	
	if (($element eq "char") && ($cp)) {
	    my $n = dname();
	    print("$cp $n\n");
	}
	
#	print("End $element which started on line ", $$element_record{line},
#	    "\n");
    }
    
    #	Append aliases if any exist
    
    sub aliases {
    	my (@a) = @_;
	
#print(Dumper(\@a));
	if (@a) {
	    return ", " . join(" ", @a);
	}
	return "";
    }
    
    #	Compose displayed name from names, aliases, etc.
    
    sub dname {
    	my $dn = "";
	
	if ($control) {
	    $dn = "<control> ";
	}
	
	$dn .= $name;
	
	#   If name ends with '#', append the code point to make the
	#   name unique.
	if ($dn =~ m/#$/) {
	    $dn .= $cp;
	}
	
	if (@corr) {
	    if (scalar(@corr) > 1) {
		die("More than one correction for CP $cp: $name");
	    }
	    $name = $corr[0];
	}
	
#print("Ctrl:\n", Dumper(@ctrl));
	$dn .= aliases(@ctrl);
#print("Abbr:\n", Dumper(@abbr));
	$dn .= aliases(@abbr);
	$dn .= aliases(@figment);
#	$dn .= aliases(@corr);
	$dn .= aliases(@alter);
	$dn .= ", IRGKangXi=$kIRGKangXi" if $kIRGKangXi;
	$dn .= ", RSKangXi=$kRSKangXi" if $kRSKangXi;
	$dn .= ", TGT_MergedSrc=$kTGT_MergedSrc" if $kTGT_MergedSrc;
	$dn .= ", PrimaryNumeric=$kPrimaryNumeric" if $kPrimaryNumeric;
	$dn .= ", AccountingNumeric=$kAccountingNumeric" if $kAccountingNumeric;
	$dn .= ", Def\{$kDefinition\}" if $kDefinition;
	
	if (hex($cp) < 0x20) {
	    $dn .= ", Ctrl-" . chr(0x40 + hex($cp));
	}
	
	return $dn;
    }
