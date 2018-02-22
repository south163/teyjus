use lib '../lib';
use strict;
use Test::More tests => 8;

my $TJTWELF = "../../tjtwelf";
my $MODULE = "ccc/sources.cfg";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
abse empty (llam [x] x) M.
CODE
$ans = <<'ANS';

The answer substitution:
M = cur (snd)

ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"ccc1");

############################################
############################################
## removed because query not a base type
# $code = <<'CODE';
# {x}abse (addv empty x) (llam [y] lpair y x) M.
# CODE
# $ans = <<'ANS';

# The answer substitution:
# M = cur (pair snd (snd @ fst))

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"ccc2");

############################################
############################################
$code = <<'CODE';
abse empty (llam [x] llam [y] lapp x y) M.
CODE
$ans = <<'ANS';

The answer substitution:
M = cur (cur ((app @ pair ((snd @ fst)) (snd))))

ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"ccc3");

############################################
############################################
$code = <<'CODE';
conc (id @ snd) E.
CODE
$ans = <<'ANS';

The answer substitution:
E = [A2:term (_ * _)] lsnd A2

ANS
same_answers_twelf( `$TJTWELF -b --query "$code" $MODULE\n`, $ans,"ccc4");

############################################
############################################
$code = <<'CODE';
conc (app @ pair snd fst) E.
CODE
$ans = <<'ANS';

The answer substitution:
E =
    [A2:term ((_ * (_ => _)))]
      lapp (lfst (lpair (lsnd A2) (lfst A2)))
        (lsnd (lpair (lsnd A2) (lfst A2)))

ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"ccc5");

############################################
############################################
$code = <<'CODE';
conc (cur app @ snd) E.
CODE
$ans = <<'ANS';

The answer substitution:

ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"ccc6");

############################################
############################################
$code = <<'CODE';
etoc exp_e (ccomp capp (cpair (ccomp (ccur cfst) cfst) csnd)) (cfst) EP.
CODE
$ans = <<'ANS';

The answer substitution:

ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"ccc7");

############################################
############################################
$code = <<'CODE';
invca
(alam ([x:term _A1] alam ([x1:term _A2] apair (avar (av_y av_x)) (avar av_x))))
(ccur (ccur (cpair (ccomp csnd cfst) csnd)))
(exp_empty)
EP.
CODE
$ans = <<'ANS';

The answer substitution:

ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"ccc8");
