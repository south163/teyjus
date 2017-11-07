use lib '../lib';
use strict;
use Test::More tests => 2;

my $TJTWELF = "../../tjtwelf";
my $MODULE = "append/append.elf";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
append (cons (s (s z)) nil) (cons (s (s (s z))) nil) L.
CODE
$ans = <<'ANS';

The answer substitution:
L = cons (s (s z)) (cons (s (s (s z))) nil)

ANS
same_answers( `$TJTWELF -b --query "$code" $MODULE\n`, $ans,"append");
############################################
############################################
$code = <<'CODE';
append L1 L2 (cons z (cons z nil)).
CODE
$ans = <<'ANS';

The answer substitution:
L1 = nil
L2 = cons z (cons z nil)



L1 = cons z nil
L2 = cons z nil



L1 = cons z (cons z nil)
L2 = nil

ANS
same_answers( `$TJTWELF -b --query "$code" $MODULE\n`, $ans,"append");