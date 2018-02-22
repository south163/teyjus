use lib '../lib';
use strict;
use Test::More tests => 4;

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
same_answers_twelf( `$TJTWELF -b --query "$code" $MODULE\n`, $ans,"append");
############################################
############################################
$code = <<'CODE';
append L1 L2 (cons z (cons z nil)).
CODE
$ans = <<'ANS';

The answer substitution:
L1 = nil
L2 = cons z (cons z nil)


The answer substitution:
L1 = cons z nil
L2 = cons z nil


The answer substitution:
L1 = cons z (cons z nil)
L2 = nil

ANS
same_answers_twelf( `$TJTWELF -b --query "$code" $MODULE\n`, $ans,"append");
############################################
############################################
$code = <<'CODE';
M : append nil L2 L3.
CODE
$ans = <<'ANS';

The answer substitution:
M = appNil
L2 = nil
L3 = nil

The answer substitution:
M = appNil
L2 = cons z nil
L3 = cons z nil

The answer substitution:
M = appNil
L2 = cons (s z) nil
L3 = cons (s z) nil

ANS
same_answers_twelf( `$TJTWELF -t naive -m 3 -b --query "$code" $MODULE\n`, $ans, "append-naive");
############################################
############################################
$code = <<'CODE';
M : append nil L2 L3.
CODE
$ans = <<'ANS';

The answer substitution:
L2 = L3
L3 = L3
M = appNil

ANS
same_answers_twelf( `$TJTWELF -b --query "$code" $MODULE\n`, $ans, "append-optimized");