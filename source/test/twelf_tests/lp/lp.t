use lib '../lib';
use strict;
use Test::More tests => 9; # 19;

my $TJTWELF = "../../tjtwelf";
my $MODULE = "lp/sources.cfg";
my $code;
my $ans;
# ############################################
# ############################################
# $code = <<'CODE';
# {P:p} {Q:p} solve (atom P imp atom Q imp (atom P and atom Q)).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"lp1");
# ############################################
# ############################################
# $code = <<'CODE';
# {P:p} {Q:p} solve (atom P imp atom Q imp atom P).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"lp2");
# ############################################
# ############################################
# $code = <<'CODE';
# {P:p} {Q:p}
# s_sound
#   (s_imp ([H1:assume (atom P)] s_imp ([H2:assume (atom Q)] s_atom i_atom H1)))
#   (D P Q).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"lp3");
# ############################################
# ############################################
# $code = <<'CODE';
# {P} {Q} can _ (impi [u:pf (atom P)] impi [u1:pf (atom Q)] u).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"lp4");
# ############################################
# ############################################
# $code = <<'CODE';
# {P:p} {Q:p}
# ss_can
#   (ss_imp
#      ([d:assume (atom P)] [u:pf (atom P)] [HS1:h_sound d u]
# 	ss_imp
# 	([d1:assume (atom Q)] [u1:pf (atom Q)] [HS2:h_sound d1 u1]
# 	   ss_atom is_atom HS1)))
#   (CN P Q).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"lp5");
############################################
############################################
$code = <<'CODE';
solve ((atom q0 imp atom r0 imp atom s0) imp (atom q0 imp atom r0)
	imp (atom q0 imp atom s0)).
CODE
$ans = <<'ANS';

The answer substitution:
yes

ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"lp6");
############################################
############################################
# MKS: removed becuase causes heap overflow...
# $code = <<'CODE';
# s_sound
#   (s_imp
#       ([H1:assume (atom q0 imp atom r0 imp atom s0)]
#           s_imp
#              ([H2:assume (atom q0 imp atom r0)]
#                  s_imp
#                     ([H3:assume (atom q0)]
#                         s_atom
#                            (i_imp (s_atom i_atom H3)
#                                (i_imp
#                                    (s_atom (i_imp (s_atom i_atom H3) i_atom) H2)
#                                    i_atom))
#                            H1))))
# D.
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"lp7");
############################################
############################################
# MKS: another heap overflow
# $code = <<'CODE';
# ss_can
#   (ss_imp
#       ([d:assume (atom q0 imp atom r0 imp atom s0)]
#           [u:pf (atom q0 imp atom r0 imp atom s0)] [HS1:h_sound d u]
#           ss_imp
#              ([d1:assume (atom q0 imp atom r0)] [u1:pf (atom q0 imp atom r0)]
#                  [HS2:h_sound d1 u1]
#                  ss_imp
#                     ([d2:assume (atom q0)] [u2:pf (atom q0)] [HS3:h_sound d2 u2]
#                         ss_atom
#                            (is_imp (ss_atom is_atom HS3)
#                                (is_imp
#                                    (ss_atom
#                                        (is_imp (ss_atom is_atom HS3) is_atom)
#                                        HS2)
#                                    is_atom))
#                            HS1))))
#   CN.
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"lp8");
############################################
############################################
$code = <<'CODE';
solve (((atom q0 imp atom r0) imp atom q0) imp atom q0).
CODE
$ans = <<'ANS';

The answer substitution:

ANS
same_answers_twelf( `$TJTWELF -e 0 -b --query "$code" $MODULE\n`, $ans,"lp9");
############################################
############################################
$code = <<'CODE';
solve ((atom q0 imp atom q0) imp atom q0 imp atom q0).
CODE
$ans = <<'ANS';

The answer substitution:
yes

ANS
same_answers_twelf( `$TJTWELF -m 15 -b --query "$code" $MODULE\n`, $ans,"lp10");
############################################
############################################
$code = <<'CODE';
solve (atom (plus (s (s 0)) (s (s (s 0))) Z)).
CODE
$ans = <<'ANS';

The answer substitution:
Z = s (s (s (s (s 0))))
ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"lp11");
############################################
############################################
$code = <<'CODE';
solve (atom (plus X Y (s (s (s (s 0)))))).
CODE
$ans = <<'ANS';

The answer substitution:
X = 0
Y = s (s (s (s 0)))


The answer substitution:
X = s 0
Y = s (s (s 0))


The answer substitution:
X = s (s 0)
Y = s (s 0)


The answer substitution:
X = s (s (s 0))
Y = s 0


The answer substitution:
X = s (s (s (s 0)))
Y = 0

ANS
same_answers_twelf( `$TJTWELF -e 5 -b --query "$code" $MODULE\n`, $ans,"lp12");
# ############################################
# ############################################
# $code = <<'CODE';
# {q:p} {r:p} solve (((atom q imp atom r) imp atom q) imp atom q).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 0 -b --query "$code" $MODULE\n`, $ans,"lp13");
# ############################################
# ############################################
# $code = <<'CODE';
# {X:i} {Y:i}
# resolve (atom^ (double 0 0) and^
# 	   (forall^ [X] forall^ [Y]
# 	      atom' (double X Y) imp^ atom^ (double (s X) (s (s Y)))))
#  (double X Y) (G X Y).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"lp14");
# ############################################
# ############################################
# $code = <<'CODE';
# {a:i -> p} {b:p}
# solve' (((forall' [x] atom'(a(x))) imp^ atom^(b))
# 	  imp' (exists' [x] (atom^(a(x)) imp' atom'(b)))).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 0 -b --query "$code" $MODULE\n`, $ans,"lp15");
############################################
############################################
$code = <<'CODE';
top_solve (atom^ (double 0 0) and^
	   (forall^ [X] forall^ [Y]
	      atom' (double X Y) imp^ atom^ (double (s X) (s (s Y)))))
(atom' (double 0 Y)).
CODE
$ans = <<'ANS';

The answer substitution:
Y = 0

ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"lp16");
############################################
############################################
$code = <<'CODE';
top_solve (atom^ (double 0 0) and^
	   (forall^ [X] forall^ [Y]
	      atom' (double X Y) imp^ atom^ (double (s X) (s (s Y)))))
(atom' (double X (s (s 0)))).
CODE
$ans = <<'ANS';

The answer substitution:
X = s 0

ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"lp17");
############################################
############################################
# MKS: the solutions we find differ from those of twelf b/c we
#      will be required to construct an explicit term for the argument
#      `T1' of the constructor `cs_exists' unlike in twelf where it is
#      left as a hole.
$code = <<'CODE';
top_solve ((forall^ [Y] atom^ (plus 0 Y Y))
	     and^
	     (forall^ [X] forall^ [Y] forall^ [Z]
		atom' (plus X Y Z) imp^ atom^ (plus (s X) Y (s Z))))
(atom' (plus X Y (s (s 0)))).
CODE
$ans = <<'ANS';

The answer substitution:
X = 0
Y = s (s 0)


The answer substitution:
X = s 0
Y = s 0


The answer substitution:
X = s 0
Y = s 0


ANS
same_answers_twelf( `$TJTWELF -e 3 -m 3 -b --query "$code" $MODULE\n`, $ans,"lp18");
############################################
############################################
$code = <<'CODE';
top_solve true^ (true' or' true').
CODE
$ans = <<'ANS';

The answer substitution:
yes

ANS
same_answers_twelf( `$TJTWELF -e 2 -b --query "$code" $MODULE\n`, $ans,"lp19");
