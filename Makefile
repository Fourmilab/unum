
#	Note: this file contains UTF-8 characters and *must* be
#	edited with a tool which supports that character set.

duh:
	@echo So, what will it be\?

all:	unum unum_comp

dist:
	( cd unum; tar cfvz ../unum.tar.gz unum.pl )
	( cd unum_comp; tar cfvz ../unum_comp.tar.gz unum.pl )

#	Build uncompressed program in unum directory

unum:	FORCE
	rm -f unum/unum.pl
	cat unum_t.pl cpnames.txt >unum/unum.pl
	chmod 755 unum/unum.pl

#	Build compressed program in unum_comp directory

unum_comp: FORCE
	rm -f unum_comp/unum.pl
	bzip2 -c cpnames.txt >cpnames$$PPID.tmp
	echo COMPRESSED >compressed$$PPID.tmp
	cat unum_t.pl compressed$$PPID.tmp cpnames$$PPID.tmp >unum_comp/unum.pl
	rm cpnames$$PPID.tmp compressed$$PPID.tmp
	chmod 755 unum_comp/unum.pl

clean:	FORCE
	rm -f *.tmp unum.html unum*.tar.gz

check:	FORCE
	echo "     -- unum/unum.pl --" >check_rcv.tmp
	unum/unum.pl 65261 >>check_rcv.tmp
	unum/unum.pl 0x2622 >>check_rcv.tmp
	unum/unum.pl 0377 >>check_rcv.tmp
	unum/unum.pl 0b11010011 >>check_rcv.tmp
	unum/unum.pl n=ctrl-q >>check_rcv.tmp
	unum/unum.pl n=integral >>check_rcv.tmp
	unum/unum.pl h=quo >>check_rcv.tmp
	unum/unum.pl n=^greek.*rho >>check_rcv.tmp
	unum/unum.pl b=greek >>check_rcv.tmp
	unum/unum.pl b=. >>check_rcv.tmp
	unum/unum.pl l=thai >>check_rcv.tmp
	unum/unum.pl c=Правда >>check_rcv.tmp
	unum/unum.pl --nent c=Известия >>check_rcv.tmp
	unum/unum.pl '&#1575;&#1604;&#1593;&#1585;&#1576;&#1610;&#1577;' >>check_rcv.tmp
	unum/unum.pl 炮 >>check_rcv.tmp
	unum/unum.pl h=nbump >>check_rcv.tmp
	unum/unum.pl --utf8 utf8=0x0 utf8=0x4B utf8=0xC397 utf8=0xE298A2 utf8=0xF09F918C >>check_rcv.tmp
	echo "     -- unum_comp/unum.pl --" >>check_rcv.tmp
	unum_comp/unum.pl 65261 >>check_rcv.tmp
	unum_comp/unum.pl 0x2622 >>check_rcv.tmp
	unum_comp/unum.pl 0377 >>check_rcv.tmp
	unum_comp/unum.pl 0b11010011 >>check_rcv.tmp
	unum_comp/unum.pl n=ctrl-q >>check_rcv.tmp
	unum_comp/unum.pl n=integral >>check_rcv.tmp
	unum_comp/unum.pl h=quo >>check_rcv.tmp
	unum_comp/unum.pl n=^greek.*rho >>check_rcv.tmp
	unum_comp/unum.pl b=greek >>check_rcv.tmp
	unum_comp/unum.pl b=. >>check_rcv.tmp
	unum_comp/unum.pl l=thai >>check_rcv.tmp
	unum_comp/unum.pl c=Правда >>check_rcv.tmp
	unum_comp/unum.pl --nent c=Известия >>check_rcv.tmp
	unum_comp/unum.pl '&#1575;&#1604;&#1593;&#1585;&#1576;&#1610;&#1577;' >>check_rcv.tmp
	unum_comp/unum.pl 炮 >>check_rcv.tmp
	unum_comp/unum.pl h=nbump >>check_rcv.tmp
	unum_comp/unum.pl --utf8 utf8=0x0 utf8=0x4B utf8=0xC397 utf8=0xE298A2 utf8=0xF09F918C >>check_rcv.tmp
	@cmp -s check_exp.txt check_rcv.tmp ; if test $$? -ne 0  ; then \
		echo '** unum:  check failed.  Results differ from check_exp.txt. **' ; \
		diff check_exp.txt check_rcv.tmp ; else \
		echo 'All tests passed.' ; fi ; fi
	@rm check_rcv.tmp

html:	FORCE
	pod2html unum/unum.pl >unum.html

view:	FORCE
	pod2man unum/unum.pl >/tmp/unum.1
	vman /tmp/unum.1
	rm /tmp/unum.1

FORCE:
