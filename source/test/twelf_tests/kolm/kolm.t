use lib '../lib';
use strict;
use Test::More tests => 11;

my $TJTWELF = "../../tjtwelf";
my $MODULE = "kolm/sources.cfg";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
D:sound (nk_noti [p:o] [u:nk false] (nk_falsee u)) (kolm_not kolm_false) NJ.
CODE
$ans = <<'ANS';

The answer substitution:
NJ =
    nj_noti
      ([p:o]
         ([A0:nj ((not (not (not (not false)))))]
            nj_note A0 p
              (nj_noti
                 ([p:o]
                    ([A0:nj ((not (not false)))]
                       nj_note
                         (nj_note A0 ((not (not false)))
                            (nj_noti ([p:o] ([A0:nj false] nj_falsee A0)))) p
                         (nj_noti ([p:o] ([A0:nj false] nj_falsee A0))))))))
D =
    sound_noti
      ([p:o]
         ([u:nk false]
            ([v:nj ((not (not false)))]
               ([kp:kolm p ((not (not p)))]
                  ([A0:existskolm p ((not (not p))) kp]
                     ([A1:sound u kolm_false v] sound_falsee A1))))))

ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm1");
############################################
############################################
$code = <<'CODE';
D:complete (kolm_not kolm_false) 
   (nj_noti
      ([p:o] [u:nj (not not not not false)]
          nj_note u p
             (nj_noti
                 ([p1:o] [X1:nj (not not false)]
                     nj_note
                        (nj_note X1 (not not false)
                            (nj_noti ([p2:o] [u1:nj false] nj_falsee u1))) p1
                        (nj_noti ([p3:o] [u2:nj false] nj_falsee u2))))))
    NK.
CODE
$ans = <<'ANS';

The answer substitution:
NK =
    nk_noti
      ([p:o]
         ([A0:nk false]
            nk_note
              (nk_dnotr
                 (nk_noti
                    ([p:o]
                       ([A0:nk ((not (not (not (not false)))))]
                          nk_note A0 p
                            (nk_noti
                               ([p:o]
                                  ([A0:nk ((not (not false)))]
                                     nk_note
                                       (nk_note A0 ((not (not false)))
                                          (nk_noti
                                             ([p:o]
                                                ([A0:nk false] nk_falsee A0))))
                                       p
                                       (nk_noti
                                          ([p:o] ([A0:nk false] nk_falsee A0))))))))))
              p (nk_noti ([p:o] ([A0:nk ((not false))] nk_note A0 p A0)))))
D =
    complete1 (equiv_not equiv_false)
      (nj_nk_noti
         ([p:o]
            ([u:nj ((not (not (not (not false)))))]
               ([v:nk ((not (not (not (not false)))))]
                  ([A:nj_nk u v]
                     nj_nk_note
                       (nj_nk_noti
                          ([p:o]
                             ([u:nj ((not (not false)))]
                                ([v:nk ((not (not false)))]
                                   ([A:nj_nk u v]
                                      nj_nk_note
                                        (nj_nk_noti
                                           ([p:o]
                                              ([u:nj false]
                                                 ([v:nk false]
                                                    ([A:nj_nk u v]
                                                       nj_nk_falsee A)))))
                                        (nj_nk_note
                                           (nj_nk_noti
                                              ([p:o]
                                                 ([u:nj false]
                                                    ([v:nk false]
                                                       ([A:nj_nk u v]
                                                          nj_nk_falsee A)))))
                                           A)))))) A)))))
                                           
ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm2");
############################################
############################################
$code = <<'CODE';
D:sound (nk_impi ([u:nk false] nk_falsee u)) (kolm_imp kolm_false kolm_false) NJ.
CODE
$ans = <<'ANS';

The answer substitution:
NJ =
    nj_noti
      ([p:o]
         ([A0:nj ((not (not (not false) imp not (not false))))]
            nj_note A0 p
              (nj_impi
                 ([A0:nj ((not (not false)))]
                    nj_note A0 ((not (not false)))
                      (nj_noti ([p:o] ([A0:nj false] nj_falsee A0)))))))
D =
    sound_impi
      ([u:nk false]
         ([v:nj ((not (not false)))]
            ([A0:sound u kolm_false v] sound_falsee A0)))
               
ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm3");
############################################
############################################
$code = <<'CODE';
D:complete (kolm_imp kolm_false kolm_false)
   (nj_noti   
      ([p:o] [u:nj (not (not not false imp not not false))]
          nj_note u p
             (nj_impi
                 ([X1:nj (not not false)]
                     nj_note X1 (not not false)
                        (nj_noti ([p1:o] [u1:nj false] nj_falsee u1)))))) 
   NK.
CODE
$ans = <<'ANS';

The answer substitution:
NK =
    nk_impi
      ([A0:nk false]
         nk_dnotr
           (nk_impe
              (nk_dnotr
                 (nk_noti
                    ([p:o]
                       ([A0:nk ((not (not (not false) imp not (not false))))]
                          nk_note A0 p
                            (nk_impi
                               ([A0:nk ((not (not false)))]
                                  nk_note A0 ((not (not false)))
                                    (nk_noti
                                       ([p:o] ([A0:nk false] nk_falsee A0)))))))))
              (nk_noti ([p:o] ([A0:nk ((not false))] nk_note A0 p A0)))))
D =
    complete1 (equiv_imp equiv_false equiv_false)
      (nj_nk_noti
         ([p:o]
            ([u:nj ((not (not (not false) imp not (not false))))]
               ([v:nk ((not (not (not false) imp not (not false))))]
                  ([A:nj_nk u v]
                     nj_nk_note
                       (nj_nk_impi
                          ([u:nj ((not (not false)))]
                             ([v:nk ((not (not false)))]
                                ([A:nj_nk u v]
                                   nj_nk_note
                                     (nj_nk_noti
                                        ([p:o]
                                           ([u:nj false]
                                              ([v:nk false]
                                                 ([A:nj_nk u v]
                                                    nj_nk_falsee A))))) A))))
                       A)))))
                       
ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm4");
############################################
############################################
$code = <<'CODE';
D:sound (nk_impi [u:nk false] nk_truei) (kolm_imp kolm_true kolm_false) NJ.
CODE
$ans = <<'ANS';

The answer substitution:
NJ =
    nj_noti
      ([p:o]
         ([A0:nj ((not (not (not false) imp not (not true))))]
            nj_note A0 p
              (nj_impi
                 ([A0:nj ((not (not false)))]
                    nj_noti
                      ([p:o] ([A0:nj ((not true))] nj_note A0 p nj_truei))))))
D =
    sound_impi
      ([u:nk false]
         ([v:nj ((not (not false)))] ([A0:sound u kolm_false v] sound_truei)))
            
ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm5");
############################################
############################################
$code = <<'CODE';
D:complete (kolm_imp kolm_true kolm_false)
   (nj_noti
      ([p:o] [u:nj (not (not not false imp not not true))]
          nj_note u p
             (nj_impi
                 ([X1:nj (not not false)]
                     nj_noti ([p1:o] [u1:nj (not true)] nj_note u1 p1 nj_truei)))))
   NK.
CODE
$ans = <<'ANS';

The answer substitution:
NK =
    nk_impi
      ([A0:nk false]
         nk_dnotr
           (nk_impe
              (nk_dnotr
                 (nk_noti
                    ([p:o]
                       ([A0:nk ((not (not (not false) imp not (not true))))]
                          nk_note A0 p
                            (nk_impi
                               ([A0:nk ((not (not false)))]
                                  nk_noti
                                    ([p:o]
                                       ([A0:nk ((not true))]
                                          nk_note A0 p nk_truei))))))))
              (nk_noti ([p:o] ([A0:nk ((not false))] nk_note A0 p A0)))))
D =
    complete1 (equiv_imp equiv_true equiv_false)
      (nj_nk_noti
         ([p:o]
            ([u:nj ((not (not (not false) imp not (not true))))]
               ([v:nk ((not (not (not false) imp not (not true))))]
                  ([A:nj_nk u v]
                     nj_nk_note
                       (nj_nk_impi
                          ([u:nj ((not (not false)))]
                             ([v:nk ((not (not false)))]
                                ([A:nj_nk u v]
                                   nj_nk_noti
                                     ([p:o]
                                        ([u:nj ((not true))]
                                           ([v:nk ((not true))]
                                              ([A:nj_nk u v]
                                                 nj_nk_note nj_nk_truei A))))))))
                       A)))))
                       
ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm6");
############################################
############################################
$code = <<'CODE';
D:sound (nk_impi [u:nk false] (nk_falsee u)) (kolm_imp kolm_true kolm_false) NJ.
CODE
$ans = <<'ANS';

The answer substitution:
NJ =
    nj_noti
      ([p:o]
         ([A0:nj ((not (not (not false) imp not (not true))))]
            nj_note A0 p
              (nj_impi
                 ([A0:nj ((not (not false)))]
                    nj_note A0 ((not (not true)))
                      (nj_noti ([p:o] ([A0:nj false] nj_falsee A0)))))))
D =
    sound_impi
      ([u:nk false]
         ([v:nj ((not (not false)))]
            ([A0:sound u kolm_false v] sound_falsee A0)))

ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm7");
############################################
############################################
$code = <<'CODE';
D:complete (kolm_imp kolm_true kolm_false)
   (nj_noti
      ([p:o] [u:nj (not (not not false imp not not true))]
          nj_note u p
             (nj_impi
                 ([X1:nj (not not false)]
                     nj_noti ([p1:o] [u1:nj (not true)] nj_note u1 p1 nj_truei)))))
   NK.
CODE
$ans = <<'ANS';
The answer substitution:
NK =
    nk_impi
      ([A0:nk false]
         nk_dnotr
           (nk_impe
              (nk_dnotr
                 (nk_noti
                    ([p:o]
                       ([A0:nk ((not (not (not false) imp not (not true))))]
                          nk_note A0 p
                            (nk_impi
                               ([A0:nk ((not (not false)))]
                                  nk_noti
                                    ([p:o]
                                       ([A0:nk ((not true))]
                                          nk_note A0 p nk_truei))))))))
              (nk_noti ([p:o] ([A0:nk ((not false))] nk_note A0 p A0)))))
D =
    complete1 (equiv_imp equiv_true equiv_false)
      (nj_nk_noti
         ([p:o]
            ([u:nj ((not (not (not false) imp not (not true))))]
               ([v:nk ((not (not (not false) imp not (not true))))]
                  ([A:nj_nk u v]
                     nj_nk_note
                       (nj_nk_impi
                          ([u:nj ((not (not false)))]
                             ([v:nk ((not (not false)))]
                                ([A:nj_nk u v]
                                   nj_nk_noti
                                     ([p:o]
                                        ([u:nj ((not true))]
                                           ([v:nk ((not true))]
                                              ([A:nj_nk u v]
                                                 nj_nk_note nj_nk_truei A))))))))
                       A)))))
                       
ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm8");
############################################
############################################
$code = <<'CODE';
D:sound (nk_dnotr (nk_noti ([p:o] [u:nk (not true)] nk_note u p nk_truei))) kolm_true NJ.
CODE
$ans = <<'ANS';

The answer substitution:
NJ =
    nj_noti
      ([p:o]
         ([A0:nj ((not true))]
            nj_note
              (nj_noti
                 ([p:o]
                    ([A0:nj ((not (not (not true))))]
                       nj_note
                         (nj_noti
                            ([p:o]
                               ([A0:nj ((not (not (not (not (not true))))))]
                                  nj_note
                                    (nj_noti
                                       ([p:o]
                                          ([A0:
                                             nj ((not (not (not (not (not
                                               (not (not true))))))))
                                             ]
                                             nj_note A0 p
                                               (nj_noti
                                                  ([p:o]
                                                     ([A0:
                                                        nj ((not (not (not
                                                          (not (not
                                                          true))))))
                                                        ]
                                                        nj_note
                                                          (nj_note
                                                             (nj_noti
                                                                ([p:o]
                                                                   ([A0:
                                                                    nj ((not
                                                                    (not
                                                                    true)))]
                                                                    nj_note
                                                                    A0 p
                                                                    (nj_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    (not (not
                                                                    true))))]
                                                                    nj_note
                                                                    A0 p A0))))))
                                                             ((not (not
                                                             false)))
                                                             (nj_noti
                                                                ([p:o]
                                                                   ([A0:
                                                                    nj ((not
                                                                    true))]
                                                                    nj_note
                                                                    A0 p
                                                                    nj_truei))))
                                                          p
                                                          (nj_noti
                                                             ([p:o]
                                                                ([A0:
                                                                   nj false]
                                                                   nj_falsee
                                                                    A0)))))))))
                                    p
                                    (nj_noti
                                       ([p:o]
                                          ([A0:
                                             nj ((not (not (not (not (not
                                               (not true)))))))
                                             ] nj_note A0 p A0)))))) p
                         (nj_noti
                            ([p:o]
                               ([A0:nj ((not (not (not (not true)))))]
                                  nj_note A0 p A0)))))) p
              (nj_noti ([p:o] ([A0:nj ((not (not true)))] nj_note A0 p A0)))))
D =
    sound_dnotr
      (sound_noti
         ([p:o]
            ([u:nk ((not true))]
               ([v:nj ((not (not (not (not (not true))))))]
                  ([kp:kolm p ((not (not p)))]
                     ([A0:existskolm p ((not (not p))) kp]
                        ([A1:sound u (kolm_not kolm_true) v]
                           sound_note sound_truei A1 existskolm_true)))))))
 
ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm9");
############################################
############################################
$code = <<'CODE';
D:complete kolm_true
   (nj_noti
      ([q:o] [u:nj (not true)]
          nj_note
             (nj_noti
                 ([q1:o] [u1:nj (not not not true)]
                     nj_note
                        (nj_noti
                            ([q2:o] [u2:nj (not not not not not true)]
                                nj_note
                                   (nj_noti
                                       (
[p:o] [u3:nj (not not not not not not not true)]
   nj_note u3 p
      (nj_noti
          ([p1:o] [X1:nj (not not not not not true)]
              nj_note
                 (nj_note
                     (nj_noti
                         ([q3:o] [u4:nj (not not true)]
                             nj_note X1 q3
                                (nj_noti
                                    ([p2:o] [v:nj (not not not true)]
                                        nj_note v p2 u4))))
                     (not not false)
                     (nj_noti ([p3:o] [u5:nj (not true)] nj_note u5 p3 nj_truei)))
                 p1 (nj_noti ([p4:o] [u6:nj false] nj_falsee u6))))))
                                   q2
                                   (nj_noti
                                       ([p:o]
                                           [v:nj (not not not not not not true)]
                                           nj_note v p u2))))
                        q1
                        (nj_noti
                            ([p:o] [v:nj (not not not not true)] nj_note v p u1))))
             q (nj_noti ([p:o] [v:nj (not not true)] nj_note v p u))))
   NK.
CODE
$ans = <<'ANS';

The answer substitution:
NK =
    nk_dnotr
      (nk_noti
         ([p:o]
            ([A0:nk ((not true))]
               nk_note
                 (nk_noti
                    ([p:o]
                       ([A0:nk ((not (not (not true))))]
                          nk_note
                            (nk_noti
                               ([p:o]
                                  ([A0:
                                     nk ((not (not (not (not (not true))))))]
                                     nk_note
                                       (nk_noti
                                          ([p:o]
                                             ([A0:
                                                nk ((not (not (not (not (not
                                                  (not (not true))))))))
                                                ]
                                                nk_note A0 p
                                                  (nk_noti
                                                     ([p:o]
                                                        ([A0:
                                                           nk ((not (not (not
                                                             (not (not
                                                             true))))))
                                                           ]
                                                           nk_note
                                                             (nk_note
                                                                (nk_noti
                                                                   ([p:o]
                                                                    ([A0:
                                                                    nk ((not
                                                                    (not
                                                                    true)))]
                                                                    nk_note
                                                                    A0 p
                                                                    (nk_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nk ((not
                                                                    (not (not
                                                                    true))))]
                                                                    nk_note
                                                                    A0 p A0))))))
                                                                ((not (not
                                                                false)))
                                                                (nk_noti
                                                                   ([p:o]
                                                                    ([A0:
                                                                    nk ((not
                                                                    true))]
                                                                    nk_note
                                                                    A0 p
                                                                    nk_truei))))
                                                             p
                                                             (nk_noti
                                                                ([p:o]
                                                                   ([A0:
                                                                    nk false]
                                                                    nk_falsee
                                                                    A0)))))))))
                                       p
                                       (nk_noti
                                          ([p:o]
                                             ([A0:
                                                nk ((not (not (not (not (not
                                                  (not true)))))))
                                                ] nk_note A0 p A0)))))) p
                            (nk_noti
                               ([p:o]
                                  ([A0:nk ((not (not (not (not true)))))]
                                     nk_note A0 p A0)))))) p
                 (nk_noti
                    ([p:o] ([A0:nk ((not (not true)))] nk_note A0 p A0))))))
D =
    complete1 equiv_true
      (nj_nk_noti
         ([p:o]
            ([u:nj ((not true))]
               ([v:nk ((not true))]
                  ([A:nj_nk u v]
                     nj_nk_note
                       (nj_nk_noti
                          ([p:o]
                             ([u:nj ((not (not true)))]
                                ([v:nk ((not (not true)))]
                                   ([A:nj_nk u v] nj_nk_note A A)))))
                       (nj_nk_noti
                          ([p:o]
                             ([u:nj ((not (not (not true))))]
                                ([v:nk ((not (not (not true))))]
                                   ([A:nj_nk u v]
                                      nj_nk_note
                                        (nj_nk_noti
                                           ([p:o]
                                              ([u:
                                                 nj ((not (not (not (not
                                                   true)))))
                                                 ]
                                                 ([v:
                                                    nk ((not (not (not (not
                                                      true)))))
                                                    ]
                                                    ([A:nj_nk u v]
                                                       nj_nk_note A A)))))
                                        (nj_nk_noti
                                           ([p:o]
                                              ([u:
                                                 nj ((not (not (not (not (not
                                                   true))))))
                                                 ]
                                                 ([v:
                                                    nk ((not (not (not (not
                                                      (not true))))))
                                                    ]
                                                    ([A:nj_nk u v]
                                                       nj_nk_note
                                                         (nj_nk_noti
                                                            ([p:o]
                                                               ([u:
                                                                  nj ((not
                                                                    (not (not
                                                                    (not (not
                                                                    (not
                                                                    true)))))))
                                                                  ]
                                                                  ([v:
                                                                    nk ((not
                                                                    (not (not
                                                                    (not (not
                                                                    (not
                                                                    true)))))))
                                                                    ]
                                                                    ([A:
                                                                    nj_nk u v
                                                                    ]
                                                                    nj_nk_note
                                                                    A A)))))
                                                         (nj_nk_noti
                                                            ([p:o]
                                                               ([u:
                                                                  nj ((not
                                                                    (not (not
                                                                    (not (not
                                                                    (not (not
                                                                    true))))))))
                                                                  ]
                                                                  ([v:
                                                                    nk ((not
                                                                    (not (not
                                                                    (not (not
                                                                    (not (not
                                                                    true))))))))
                                                                    ]
                                                                    ([A:
                                                                    nj_nk u v
                                                                    ]
                                                                    nj_nk_note
                                                                    (nj_nk_noti
                                                                    ([p:o]
                                                                    ([u:
                                                                    nj ((not
                                                                    (not (not
                                                                    (not (not
                                                                    true))))))
                                                                    ]
                                                                    ([v:
                                                                    nk ((not
                                                                    (not (not
                                                                    (not (not
                                                                    true))))))
                                                                    ]
                                                                    ([A:
                                                                    nj_nk u v
                                                                    ]
                                                                    nj_nk_note
                                                                    (nj_nk_noti
                                                                    ([p:o]
                                                                    ([u:
                                                                    nj false]
                                                                    ([v:
                                                                    nk false]
                                                                    ([A:
                                                                    nj_nk u v
                                                                    ]
                                                                    nj_nk_falsee
                                                                    A)))))
                                                                    (nj_nk_note
                                                                    (nj_nk_noti
                                                                    ([p:o]
                                                                    ([u:
                                                                    nj ((not
                                                                    true))]
                                                                    ([v:
                                                                    nk ((not
                                                                    true))]
                                                                    ([A:
                                                                    nj_nk u v
                                                                    ]
                                                                    nj_nk_note
                                                                    nj_nk_truei
                                                                    A)))))
                                                                    (nj_nk_noti
                                                                    ([p:o]
                                                                    ([u:
                                                                    nj ((not
                                                                    (not
                                                                    true)))]
                                                                    ([v:
                                                                    nk ((not
                                                                    (not
                                                                    true)))]
                                                                    ([A:
                                                                    nj_nk u v
                                                                    ]
                                                                    nj_nk_note
                                                                    (nj_nk_noti
                                                                    ([p:o]
                                                                    ([u:
                                                                    nj ((not
                                                                    (not (not
                                                                    true))))]
                                                                    ([v:
                                                                    nk ((not
                                                                    (not (not
                                                                    true))))]
                                                                    ([A:
                                                                    nj_nk u v
                                                                    ]
                                                                    nj_nk_note
                                                                    A A)))))
                                                                    A)))))))))))
                                                                    A))))))))))))))))))))

ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm10");
############################################
############################################
$code = <<'CODE';
D:sound
  (nk_andi 
    (nk_ore 
      (nk_impe (nk_impi [v2:nk true] (nk_orir (nk_andi nk_truei v2)))
               (nk_dnotr (nk_noti [p] [u2:nk (not true)]
                           (nk_note u2 p nk_truei))))
      ([u:nk ((true or false) and false)]
             (nk_andel u))
      ([v:nk (true and true)] (nk_oril (nk_ander v))))
    (nk_noti [p] [w:nk false] (nk_falsee w)))
  (kolm_and (kolm_not kolm_false) (kolm_or kolm_false kolm_true))
  NJ.
CODE
$ans = <<'ANS';

The answer substitution:
NJ =
    nj_noti
      ([p:o]
         ([A0:
            nj ((not (not (not (not (not true) or not (not false))) and not
              (not (not (not (not false)))))))
            ]
            nj_note A0 p
              (nj_andi
                 (nj_noti
                    ([p:o]
                       ([A0:nj ((not (not (not true) or not (not false))))]
                          nj_note
                            (nj_noti
                               ([p:o]
                                  ([A0:
                                     nj ((not (not (not (not (not (not (not
                                       true) or not (not false))) and not
                                       (not false))) or not (not (not (not
                                       true) and not (not true))))))
                                     ]
                                     nj_note
                                       (nj_noti
                                          ([p:o]
                                             ([A0:
                                                nj ((not (not (not true) imp
                                                  not (not (not (not (not
                                                  (not (not (not true) or not
                                                  (not false))) and not (not
                                                  false))) or not (not (not
                                                  (not true) and not (not
                                                  true))))))))
                                                ]
                                                nj_note A0 p
                                                  (nj_impi
                                                     ([A0:
                                                        nj ((not (not true)))
                                                        ]
                                                        nj_noti
                                                          ([p:o]
                                                             ([A0:
                                                                nj ((not (not
                                                                  (not (not
                                                                  (not (not
                                                                  (not true)
                                                                  or not (not
                                                                  false)))
                                                                  and not
                                                                  (not
                                                                  false))) or
                                                                  not (not
                                                                  (not (not
                                                                  true) and
                                                                  not (not
                                                                  true))))))
                                                                ]
                                                                nj_note A0 p
                                                                  (nj_orir
                                                                    (nj_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    (not (not
                                                                    true) and
                                                                    not (not
                                                                    true))))]
                                                                    nj_note
                                                                    A0 p
                                                                    (nj_andi
                                                                    (nj_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    true))]
                                                                    nj_note
                                                                    A0 p
                                                                    nj_truei)))
                                                                    A0))))))))))))
                                       p
                                       (nj_noti
                                          ([p:o]
                                             ([A0:
                                                nj ((not (not true) imp not
                                                  (not (not (not (not (not
                                                  (not (not true) or not (not
                                                  false))) and not (not
                                                  false))) or not (not (not
                                                  (not true) and not (not
                                                  true)))))))
                                                ]
                                                nj_note
                                                  (nj_impe A0
                                                     (nj_noti
                                                        ([p:o]
                                                           ([A0:
                                                              nj ((not true))
                                                              ]
                                                              nj_note
                                                                (nj_noti
                                                                   ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    (not (not
                                                                    true))))]
                                                                    nj_note
                                                                    (nj_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    (not (not
                                                                    (not (not
                                                                    true))))))
                                                                    ]
                                                                    nj_note
                                                                    (nj_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    (not (not
                                                                    (not (not
                                                                    (not (not
                                                                    true))))))))
                                                                    ]
                                                                    nj_note
                                                                    A0 p
                                                                    (nj_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    (not (not
                                                                    (not (not
                                                                    true))))))
                                                                    ]
                                                                    nj_note
                                                                    (nj_note
                                                                    (nj_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    (not
                                                                    true)))]
                                                                    nj_note
                                                                    A0 p
                                                                    (nj_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    (not (not
                                                                    true))))]
                                                                    nj_note
                                                                    A0 p A0))))))
                                                                    ((not
                                                                    (not
                                                                    false)))
                                                                    (nj_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    true))]
                                                                    nj_note
                                                                    A0 p
                                                                    nj_truei))))
                                                                    p
                                                                    (nj_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nj false]
                                                                    nj_falsee
                                                                    A0)))))))))
                                                                    p
                                                                    (nj_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    (not (not
                                                                    (not (not
                                                                    (not
                                                                    true)))))))
                                                                    ]
                                                                    nj_note
                                                                    A0 p A0))))))
                                                                    p
                                                                    (nj_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    (not (not
                                                                    (not
                                                                    true)))))
                                                                    ]
                                                                    nj_note
                                                                    A0 p A0))))))
                                                                p
                                                                (nj_noti
                                                                   ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    (not
                                                                    true)))]
                                                                    nj_note
                                                                    A0 p A0)))))))
                                                  p A0)))))) p
                            (nj_noti
                               ([p:o]
                                  ([A0:
                                     nj ((not (not (not (not (not (not true)
                                       or not (not false))) and not (not
                                       false))) or not (not (not (not true)
                                       and not (not true)))))
                                     ]
                                     nj_note
                                       (nj_ore A0
                                          ([A1:
                                             nj ((not (not (not (not (not
                                               (not true) or not (not
                                               false))) and not (not
                                               false)))))
                                             ]
                                             nj_noti
                                               ([p:o]
                                                  ([A0:
                                                     nj ((not (not (not true)
                                                       or not (not false))))
                                                     ]
                                                     nj_note A1 p
                                                       (nj_noti
                                                          ([p:o]
                                                             ([A0:
                                                                nj ((not (not
                                                                  (not (not
                                                                  true) or
                                                                  not (not
                                                                  false)))
                                                                  and not
                                                                  (not
                                                                  false)))
                                                                ]
                                                                nj_note
                                                                  (nj_andel
                                                                    A0) p A0))))))
                                          ([A2:
                                             nj ((not (not (not (not true)
                                               and not (not true)))))
                                             ]
                                             nj_noti
                                               ([p:o]
                                                  ([A0:
                                                     nj ((not (not (not true)
                                                       or not (not false))))
                                                     ]
                                                     nj_note A0 p
                                                       (nj_oril
                                                          (nj_noti
                                                             ([p:o]
                                                                ([A0:
                                                                   nj ((not
                                                                    true))
                                                                   ]
                                                                   nj_note A2
                                                                    p
                                                                    (nj_noti
                                                                    ([p:o]
                                                                    ([A0:
                                                                    nj ((not
                                                                    (not
                                                                    true) and
                                                                    not (not
                                                                    true)))]
                                                                    nj_note
                                                                    (nj_ander
                                                                    A0) p A0)))))))))))
                                       p A0))))))
                 (nj_noti
                    ([p:o]
                       ([A0:nj ((not (not (not (not false)))))]
                          nj_note A0 p
                            (nj_noti
                               ([p:o]
                                  ([A0:nj ((not (not false)))]
                                     nj_note
                                       (nj_note A0 ((not (not false)))
                                          (nj_noti
                                             ([p:o]
                                                ([A0:nj false] nj_falsee A0))))
                                       p
                                       (nj_noti
                                          ([p:o] ([A0:nj false] nj_falsee A0))))))))))))
D =
    sound_andi
      (sound_noti
         ([p:o]
            ([u:nk false]
               ([v:nj ((not (not false)))]
                  ([kp:kolm p ((not (not p)))]
                     ([A0:existskolm p ((not (not p))) kp]
                        ([A1:sound u kolm_false v] sound_falsee A1)))))))
      (sound_ore
         ([u:nk ((true and true))]
            ([v:nj ((not (not (not (not true) and not (not true)))))]
               ([A0:sound u (kolm_and kolm_true kolm_true) v]
                  sound_oril (sound_ander A0 existskolm_true))))
         ([u:nk (((true or false) and false))]
            ([v:
               nj ((not (not (not (not (not (not true) or not (not false)))
                 and not (not false)))))
               ]
               ([A1:
                  sound u
                    (kolm_and kolm_false (kolm_or kolm_false kolm_true)) v
                  ] sound_andel A1 existskolm_false)))
         (sound_impe
            (sound_impi
               ([u:nk true]
                  ([v:nj ((not (not true)))]
                     ([A0:sound u kolm_true v]
                        sound_orir (sound_andi A0 sound_truei)))))
            (sound_dnotr
               (sound_noti
                  ([p:o]
                     ([u:nk ((not true))]
                        ([v:nj ((not (not (not (not (not true))))))]
                           ([kp:kolm p ((not (not p)))]
                              ([A0:existskolm p ((not (not p))) kp]
                                 ([A1:sound u (kolm_not kolm_true) v]
                                    sound_note sound_truei A1 existskolm_true))))))))
            existskolm_true) (existskolm_and existskolm_true existskolm_true)
         (existskolm_and existskolm_false
            (existskolm_or existskolm_false existskolm_true)))

ANS
same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm11");
############################################
############################################
# MKS: Should have one solution but has none
# $code = <<'CODE';
# D:complete (kolm_and (kolm_not kolm_false) (kolm_or kolm_false kolm_true))
#    (nj_noti
#       ([p:o]
#           [u:nj
#                 (not
#                     (not not (not not true or not not false)
#                         and not not not not not false))]
#           nj_note u p
#              (nj_andi
#                  (nj_noti
#                      ([q:o] [v:nj (not (not not true or not not false))]
#                          nj_note
#                             (nj_noti
#                                 ([q1:o]
#                                     [v1:nj
#                                            (not
#                                                (not not
#                                                    (not not
#                                                        (not not true
#                                                            or not not false)
#                                                        and not not false)
#                                                    or not not
#                                                          (not not true
#                                                              and not not true)))]
#                                     nj_note
#                                        (
# nj_noti
#    ([p1:o]
#        [u1:nj
#               (not
#                   (not not true
#                       imp not not
#                              (not not
#                                  (not not (not not true or not not false)
#                                      and not not false)
#                                  or not not (not not true and not not true))))]
#        nj_note u1 p1
#           (nj_impi
#               ([X1:nj (not not true)]
#                   nj_noti
#                      ([p2:o]
#                          [u2:nj
#                                 (not
#                                     (not not
#                                         (not not (not not true or not not false)
#                                             and not not false)
#                                         or not not
#                                               (not not true and not not true)))]
#                          nj_note u2 p2
#                             (nj_orir
#                                 (nj_noti
#                                     ([p3:o]
#                                         [u3:nj
#                                                (not
#                                                    (not not true
#                                                        and not not true))]
#                                         nj_note u3 p3
#                                            (nj_andi
#                                                (nj_noti
#                                                    ([p4:o] [u4:nj (not true)]
#                                                        nj_note u4 p4 nj_truei))
#                                                X1)))))))))
#                                        q1
#                                        (
# nj_noti
#    ([p5:o]
#        [u5:nj
#               (not not true
#                   imp not not
#                          (not not
#                              (not not (not not true or not not false)
#                                  and not not false)
#                              or not not (not not true and not not true)))]
#        nj_note
#           (nj_impe u5
#               (nj_noti
#                   ([q2:o] [u6:nj (not true)]
#                       nj_note
#                          (nj_noti
#                              ([q3:o] [u7:nj (not not not true)]
#                                  nj_note
#                                     (nj_noti
                                        
# ([q4:o] [u8:nj (not not not not not true)]
#     nj_note
#        (nj_noti
#            ([p6:o] [u9:nj (not not not not not not not true)]
#                nj_note u9 p6
#                   (nj_noti
#                       ([p7:o] [X2:nj (not not not not not true)]
#                           nj_note
#                              (nj_note
#                                  (nj_noti
#                                      ([q5:o] [u10:nj (not not true)]
#                                          nj_note X2 q5
#                                             (nj_noti
#                                                 ([p8:o]
#                                                     [v2:nj (not not not true)]
#                                                     nj_note v2 p8 u10))))
#                                  (not not false)
#                                  (nj_noti
#                                      ([p9:o] [u11:nj (not true)]
#                                          nj_note u11 p9 nj_truei)))
#                              p7 (nj_noti ([p10:o] [u12:nj false] nj_falsee u12))))))
#        q4
#        (nj_noti
#            ([p11:o] [v3:nj (not not not not not not true)] nj_note v3 p11 u8))))
#                                     q3
#                                     (nj_noti
#                                         ([p12:o] [v4:nj (not not not not true)]
#                                             nj_note v4 p12 u7))))
#                          q2
#                          (nj_noti
#                              ([p13:o] [v5:nj (not not true)] nj_note v5 p13 u6)))))
#           p5 v1))))
#                             q
#                             (nj_noti
#                                 ([p14:o]
#                                     [u13:nj
#                                             (not not
#                                                 (not not
#                                                     (not not true
#                                                         or not not false)
#                                                     and not not false)
#                                                 or not not
#                                                       (not not true
#                                                           and not not true))]
#                                     nj_note
#                                        (
# nj_ore u13
#    ([X3:nj (not not (not not (not not true or not not false) and not not false))]
#        nj_noti
#           ([q6:o] [u14:nj (not (not not true or not not false))]
#               nj_note X3 q6
#                  (nj_noti
#                      ([p15:o]
#                          [v6:nj
#                                 (not not (not not true or not not false)
#                                     and not not false)]
#                          nj_note (nj_andel v6) p15 u14))))
#    ([X4:nj (not not (not not true and not not true))]
#        nj_noti
#           ([p16:o] [u15:nj (not (not not true or not not false))]
#               nj_note u15 p16
#                  (nj_oril
#                      (nj_noti
#                          ([q7:o] [u16:nj (not true)]
#                              nj_note X4 q7
#                                 (nj_noti
#                                     ([p17:o]
#                                         [v7:nj (not not true and not not true)]
#                                         nj_note (nj_ander v7) p17 u16))))))))
#                                        p14 v))))
#                  (nj_noti
#                      ([p18:o] [u17:nj (not not not not false)]
#                          nj_note u17 p18
#                             (nj_noti
#                                 ([p19:o] [X5:nj (not not false)]
#                                     nj_note
#                                        (nj_note X5 (not not false)
#                                            (nj_noti
#                                                ([p20:o] [u18:nj false]
#                                                    nj_falsee u18))) p19
#                                        (nj_noti
#                                            ([p21:o] [u19:nj false] nj_falsee u19)))))))))
#    NK.
# CODE
# $ans = <<'ANS';
# 
# The answer substitution:
# 
# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm12");
############################################
############################################
# $code = <<'CODE';
# D:({a:o} {ka:kolm a (not not a)} existskolm a (not not a) ka ->
#                 sound 
#                       (nk_dnotr
#                         (nk_noti 
#                           ([p] [u:nk (not (a or (not a)))]
#                             nk_note u p
#                               (nk_orir 
#                                 (nk_noti 
#                                   ([q] [v:nk a]
#                                     nk_note u q (nk_oril v)))))))
#                       (kolm_or (kolm_not ka) ka)
#                       (NJ a)).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm13");
############################################
############################################
# $code = <<'CODE';
# D:({a:o} {ka:kolm a (not not a)} 
#    equiv ka ([u:nk a] (nk_dnotx u)) ([u:nk (not not a)] (nk_dnotr u)) ->
#    complete (kolm_or (kolm_not ka) ka) 
#       (nj_triple_neg_red
#          (nj_triple_neg_red
#              (nj_triple_neg_red
#                  (nj_dnotx
#                      (nj_noti
#                          ([q:o]
#                              [v:nj
#                                    (not not not not not
#                                        (not not a or not not not not not a))]
#                              nj_dneg_falser
#                                 (nj_note (nj_triple_neg_red v) (not not false)
#                                     (nj_dnotx
                                        
# (nj_orir
#     (nj_dnotx
#         (nj_noti
#             ([q1:o] [v1:nj (not not a)]
#                 nj_dneg_falser
#                    (nj_note (nj_triple_neg_red v) (not not false)
#                        (nj_dnotx (nj_oril v1))) q1))))))
#                                 q))))))
#        (NK a)).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm14");
############################################
############################################
# $code = <<'CODE';
# D:({a:o} {ka:kolm a (not not a)} 
#    equiv ka ([u:nk a] (nk_dnotx u)) ([u:nk (not not a)] (nk_dnotr u)) ->
#    complete (kolm_or (kolm_not ka) ka)
#             (nj_noti [p] [u:nj (not ((not not a) or (not not not not not a)))]
#               (nj_note u p
#                 (nj_orir 
#                   (nj_dnotx
#                     (nj_noti [q] [v:nj (not not a)]
#                       (nj_note u q (nj_oril v)))))))
#             (NK a)).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm15");
############################################
############################################
# $code = <<'CODE';
# D:({a:o} {b:o} {ka:kolm a (n a)} {kb:kolm b (n b)}
#    existskolm a (n a) ka ->
#    existskolm b (n b) kb ->
#    sound (nk_impi [u:nk (not (a and b))]
#           (nk_dnotr
#            (nk_noti ([p] [v:nk (not ((not a) or (not b)))]
#             nk_note v p
#              (nk_orir 
#               (nk_noti ([q] [w:nk b]
#                nk_note v q
#                 (nk_oril 
#                  (nk_noti [r] [z:nk a]
#                   (nk_note u r
#                    (nk_andi z w)))))))))))
#          (kolm_imp (kolm_or (kolm_not kb) (kolm_not ka))
#                    (kolm_not (kolm_and kb ka)))
#          (NJ a b)).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm16");
############################################
############################################
# $code = <<'CODE';
# D:({a:o} {b:o} {ka:kolm a (n a)} {kb:kolm b (n b)}
#    equiv ka ([u:nk a] (nk_dnotx u)) ([u:nk (not not a)] (nk_dnotr u)) ->
#    equiv kb ([u:nk b] (nk_dnotx u)) ([u:nk (not not b)] (nk_dnotr u)) ->
#    complete
#      (kolm_imp (kolm_or (kolm_not kb) (kolm_not ka))
#                (kolm_not (kolm_and kb ka)))
#       (nj_dnotx
#          (nj_impi
#              ([X2:nj (n (not n (n a and n b)))]
#                  nj_triple_neg_red
#                     (nj_triple_neg_red
#                         (nj_triple_neg_red
#                             (nj_dnotx
#                                 (nj_noti
#                                     ([q:o]
#                                         [v:nj
#                                               (n
#                                                   (not not not
#                                                       (n (not n a)
#                                                           or n (not n b))))]
                                        
# nj_dneg_falser
#    (nj_note (nj_triple_neg_red v) (n false)
#        (nj_dnotx
#            (nj_orir
#                (nj_dnotx
#                    (nj_noti
#                        ([q1:o] [v1:nj (n b)]
#                            nj_dneg_falser
#                               (nj_note (nj_triple_neg_red v) (n false)
#                                   (nj_dnotx
#                                       (nj_oril
                                          
# (nj_dnotx
#     (nj_noti
#         ([q2:o] [v2:nj (n a)]
#             nj_dneg_falser
#                (nj_note (nj_triple_neg_red X2) (n false)
#                    (nj_dnotx (nj_andi v2 v1))) q2))))))
#                               q1))))))
#    q))))))))
#  (NK a b)).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm17");
############################################
############################################
# $code = <<'CODE';
# D:({R:i -> i -> o} {kr:{x} {y} kolm (R x y) (not not (R x y))} 
#     ({x} {y} existskolm (R x y) (not not (R x y)) (kr x y)) ->
#   sound
#    (nk_impi [u:nk (forall [x] (R x x))]
#        (nk_foralli [a]
#           (nk_existsi a
#              (nk_foralle u a))))
#    (kolm_imp (kolm_forall [x] (kolm_exists [y] (kr x y)))
#              (kolm_forall [x] (kr x x)))
#    (NJ R)).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm18");
############################################
############################################
# $code = <<'CODE';
# D:({R:i -> i -> o} {kr:{x} {y} kolm (R x y) (not not (R x y))} 
#    ({x} {y} equiv (kr x y) ([u:nk (R x y)] (nk_dnotx u)) 
#                            ([u:nk (not not (R x y))] (nk_dnotr u))) ->
#    complete (kolm_imp (kolm_forall [x] (kolm_exists [y] (kr x y)))
#              (kolm_forall [x] (kr x x)))
#    (nj_noti
#          ([p:o]
#              [u:nj
#                    (not
#                        (not not forall ([x:i] not not R x x)
#                            imp not not
#                                   forall
#                                      ([x:i]
#                                          not not exists ([y:i] not not R x y))))]
#              nj_note u p
#                 (nj_impi
#                     ([X2:nj (not not forall ([x:i] not not R x x))]
#                         nj_noti
#                            ([p1:o]
#                                [u1:nj
#                                       (not
#                                           forall
#                                              ([T1:i]
#                                                  not not
#                                                     exists
#                                                        ([y:i] not not R T1 y)))]
#                                nj_note u1 p1
#                                   (nj_foralli
#                                       ([a:i]
                                          
# nj_noti
#    ([p2:o] [u2:nj (not exists ([T2:i] not not R a T2))]
#        nj_note u2 p2
#           (nj_existsi a
#               (nj_noti
#                   ([q:o] [v:nj (not R a a)]
#                       nj_note X2 q
#                          (nj_noti
#                              ([p3:o] [u3:nj (forall ([x:i] not not R x x))]
#                                  nj_note (nj_foralle u3 a) p3 v)))))))))))))
#     (NK R)).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm19");
############################################
############################################
# $code = <<'CODE';
# D:({A:i -> o} {kA:{x:i} kolm (A x) (n (A x))}
#    ({x:i} existskolm (A x) (n (A x)) (kA x)) ->
#    sound (nk_impi [u:nk (not (forall A))]
#           (nk_dnotr
#            (nk_noti ([p] [v:nk (not (exists [x] (not (A x))))]
#              nk_note u p
#              (nk_foralli ([a:i]
#                nk_dnotr 
#                (nk_noti ([q] [w:nk (not (A a))]
#                  nk_note v q 
#                  (nk_existsi a w)))))))))
#          (kolm_imp (kolm_exists [x] (kolm_not (kA x)))
#                    (kolm_not (kolm_forall kA)))
#          (NJ A)).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm20");
############################################
############################################
# $code = <<'CODE';
# D:({A:i -> o} {kA:{x:i} kolm (A x) (n (A x))}
#    ({x} equiv (kA x) ([u:nk (A x)] (nk_dnotx u)) ([u:nk (not not (A x))] (nk_dnotr u))) ->
#    complete
#       (kolm_imp (kolm_exists [x] (kolm_not (kA x)))
#                    (kolm_not (kolm_forall kA)))
#       (nj_dnotx
#          (nj_impi
#              ([X2:nj (n (not n (forall ([T1:i] n (A T1)))))]
#                  nj_triple_neg_red
#                     (nj_triple_neg_red
#                         (nj_triple_neg_red
#                             (nj_dnotx
#                                 (nj_noti
#                                     ([q:o]
#                                         [v:nj
#                                               (n
#                                                   (not not not
#                                                       exists
#                                                          ([T2:i]
#                                                              n (not n (A T2)))))]
                                        
# nj_dneg_falser
#    (nj_note (nj_triple_neg_red X2) (n false)
#        (nj_dnotx
#            (nj_foralli
#                ([T3:i]
#                    nj_triple_neg_red
#                       (nj_triple_neg_red
#                           (nj_triple_neg_red
#                               (nj_dnotx
#                                   (nj_noti
#                                       ([q1:o] [v1:nj (n (not not not A T3))]
#                                           nj_dneg_falser
#                                              (nj_note (nj_triple_neg_red v)
#                                                  (n false)
#                                                  (nj_dnotx (nj_existsi T3 v1)))
#                                              q1)))))))))
#    q))))))))
#    (NK A)).
# CODE
# $ans = <<'ANS';

# The answer substitution:

# ANS
# same_answers_twelf( `$TJTWELF -e 1 -b --query "$code" $MODULE\n`, $ans,"kolm21"); 