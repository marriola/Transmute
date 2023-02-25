﻿; Based on https://en.wikipedia.org/wiki/Proto-Germanic_language#Phonological_stages_from_Proto-Indo-European_to_end_of_Proto-Germanic.
; For the purposes of this example we take the values of h₁, h₂ and h₃ to be ʔ, χ and χʷ, respectively, on the basis that I think they're neat.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Rules                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO implement NOT nodes
;   $SONORANT/u$SONORANT/$V_!$V
;   $SONORANT/u$SONORANT/!$V_$V

;;;;;;;;;;;;
; Pre-PGmc ;
;;;;;;;;;;;;

; You can omit the environment segment for unconditional rules.
[+$palatalized]→[-$palatalized]

; Insertion rules transform ∅ into something. You can omit the ∅ symbol if you want.
∅→u/(#|[$C-$LARYNGEAL])(ˈ)_(m|n|l|r)(#|$C)

[-$overlong]→[+$overlong]/_#

$LARYNGEAL→∅/#_$C

; e-coloring and dropping of laryngeals in onset
e→o/χʷ(ˈ)_
e→a/χ(ˈ)_
$LARYNGEAL→∅/#_
$LARYNGEAL→∅/_(ˈ)$V

; Homorganic vowels in hiatus → long vowel
aa→aːː
ee→eːː
ii→iːː
oo→oːː
uu→uːː

; e-coloring
e→o/_(w|j)χʷ
e→a/_(w|j)χ
eχʷ→oː
eχ→ɑː

; Compensatory lengthening with loss of laryngeals after sonorants
[-$long]→[+$long]/_$LARYNGEAL
$LARYNGEAL→∅/($V|$SONORANT)_

; Cowgill's law
χʷ→g/($SONORANT)_w

; Vocalization of remaining laryngeals
$LARYNGEAL→ə


;;;;;;;;;;;;;;
; Early PGmc ;
;;;;;;;;;;;;;;

(j|w)→∅/_(e|a|o)#
(e|a|o)→∅/_#

; Grimm's law: voiceless stops become fricatives, except after an obstruent
[$STOP-$voiced]→[+$fricative]/(#|$V|$SONORANT)_
;[$STOP-$voiced]→[+$fricative]/!($OBSTRUENT)_

; Undo Grimm's law after s-
[+$fricative-$voiced]→[-$fricative]/s_

; Germanic spirant law
[$STOP+$LABIAL]→ɸ/_(t|s) ;($C|$V)
[$STOP+$DENTAL]→ts/_(t|s) ;($C|$V)
tst→ss
;tss→ss
ss→s/_#
[$STOP+$VELAR]→x/_(t|s)($C|$V)

; Grimm's law: voiced unaspirated stops become voiceless stops
; TODO: Do not match a sound if it is actually a prefix for a longer sound that should not be matched
; Fix: identify features that have the sound as a prefix and transition on error
; e.g. don't match b for [$STOP+$voiced-$aspirated] when it is followed by ʰ
[$STOP+$voiced-$aspirated]→[-$voiced]/_(#|(ˈ)$V|$C)

; Grimm's law: aspirated stops become unaspirated
[$STOP+$aspirated]→[-$aspirated]
g→ɣ/(#|$LIQUID|[+$fricative])_

; Lenition of intervocalic voiced stops
[$STOP+$voiced]→[+$fricative]/($V|$GLIDE)_(#|$V|$GLIDE)

; Verner's law
[+$fricative-$voiced]→[+$voiced]/[-$stressed-$SONORANT]($SONORANT)_($SONORANT)[+$stressed] ; Why does this one result in environment and input states being merged?

; Undo Verner's law after voiceless consonant
[+$fricative+$voiced]→[-$voiced]/[-$voiced]_

; Undo Verner's law before voiceless stops
[+$fricative+$voiced]→[-$voiced]/_[$STOP-$voiced]

; Voiced fricatives resulting from Verner's law to stops after nasal
[+$fricative+$voiced]→[-$fricative]/$NASAL_

; Stress moves to initial syllable. Let's just stop marking it.
[+$stressed]→[-$stressed]

; Word-final /s/ previously unaffected by Verner's law becomes voiced by analogy with those that were
s→z/$V(($NASAL|$LIQUID))_#

gʷ→b/#_

nw→nn
ln→ll
zm→mm

; TODO: make this be smart and match short vowels without having to manually specify what could come after
e→i/$V($GLIDE)($C)($C)_(#|$C)
e→i/$V($GLIDE)($C)($C)_(#|$C)
(ei|ej)→iː
i(j(i)|ːi)→iː
; TODO: on error, allow the machine to jump to another branch (e.g. from $C to _) if that one is capable of matching the input
ji→i/$V_(#|$C)
ji→i/$V$C_(#|$C)
ji→i/$V$C$C_(#|$C)

(o|a)→ɑ

;;;;;;;;;;;;;
; Late PGmc ;
;;;;;;;;;;;;;

m→n/_(#|$DENTAL)
[-$nasalized]n→[+$nasalized]/_#

ẽː→ɑ̃ː

; Stressed schwa becomes /a/
ə→ɑ/#(s)($C)($C)_

; Unstressed schwa disappears between consonants
ə→∅/$C_$C
ə→ɑ

;t→∅/$V$C($C)($C)$V($C)($C)_#
t→∅/$V($C)($C)$V($C)_#
ɣʷ→w

ɑː→ɔː
ɑ̃ː→ɔ̃ː

; Not really sure where this should go, so I'll just stick it at the end
sr→str

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Sets and features                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


$V {
    ə
    ɑ  a  e  i  o     u
    ɑː aː eː iː oː ɔː uː
    ɑ̃  ẽ  ĩ  õ     ũ
    ɑ̃ː ẽː ĩː õː ɔ̃ː ũː
}

[$mid] { e o }
[$round] { o u }
[$high] { i u }

[$long] {
    ɑ → ɑː
    a → aː
    e → eː
    i → iː
    o → oː
    u → uː
    ɔː 
    ɑ̃ → ɑ̃ː
    ẽ → ẽː
    ĩ → ĩː
    õ → õː
    ũ → ũː
    ɔ̃ː
}

[$overlong] {
    aː → aːː
    oː → oːː    
}

[$nasalized] {
    ɑ → ɑ̃
    e → ẽ
    i → ĩ
    o → õ
    u → ũ
    ɑː → ɑ̃ː
    eː → ẽː
    iː → ĩː
    oː → õː
    uː → ũː
    ɑːː → ɑ̃ːː
    oːː → õːː
}

[$stressed] {
    a → ˈa
    aː → ˈaː
    ɑ → ˈɑ
    ɑː → ˈɑː
    e → ˈe
    eː → ˈeː
    i → ˈi
    iː → ˈiː
    o → ˈo
    oː → ˈoː
    ɔ → ˈɔ
    ɔː → ˈɔː
    u → ˈu
    uː → ˈuː
    ə → ˈə
    m → ˈm
    n → ˈn
    l → ˈl
    r → ˈr
}

; TODO map sets
; $V → &ː
;   produces aː eː iː oː uː

$STOP {
    k  kʲ  kʷ  p  t
    g  gʲ  gʷ  b  d
    gʰ gʲʰ gʷʰ bʰ dʰ
}

$DENTAL { t d dʰ θ ð s z }
$LABIAL { m p b bʰ ɸ β }
$LABIOVELAR { kʷ gʷ gʷʰ xʷ ɣʷ }
$VELAR { k g gʰ gʲʰ x ɣ $LABIOVELAR }
$SONORANT { m n l r w j }
$LIQUID { l r }
$GLIDE { w j }
$NASAL { m n }
$LARYNGEAL { ʔ χ χʷ }
$SIBILANT { s }

$OBSTRUENT { $STOP $fricative }
$C { $DENTAL $LABIAL $VELAR $SONORANT $LIQUID $GLIDE $NASAL $LARYNGEAL $SIBILANT }

[$voiced] {
    k → g
    gʰ
    x → ɣ
    xʷ → ɣʷ
    kʲ → gʲ
    gʲʰ
    kʷ → gʷ
    gʷʰ
    p → b
    bʰ
    ɸ → β
    t → d
    s → z
    dʰ
    θ → ð
}

[$palatalized] {
    k → kʲ
    g → gʲ
    gʰ → gʲʰ
}

[$labialized] {
    k → kʷ
    g → gʷ
    gʰ → gʷʰ
}

[$aspirated] {
    g → gʰ
    gʲ → gʲʰ
    gʷ → gʷʰ
    b → bʰ
    d → dʰ
}

[$fricative] {
    k → x
    kʷ → xʷ
    p → ɸ
    t → θ
    g → ɣ
    gʷ → ɣʷ
    b → β
    d → ð
    s
    z
}
