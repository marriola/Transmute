; From https://en.wikipedia.org/wiki/Proto-Germanic_language#Phonological_stages_from_Proto-Indo-European_to_end_of_Proto-Germanic
; For the purposes of this example we take the values of h₁, h₂ and h₃ to be ʔ, χ and χʷ, respectively, on the basis that I think
; they're neat.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Rules                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO implement NOT nodes
;   $SONORANT/u$SONORANT/$V_!$V
;   $SONORANT/u$SONORANT/!$V_$V

; Pre-PGmc

; TODO implement transformation of Ø, i.e. insertion
; /u/#_$SONORANT$C

[+$palatalized]/[-$palatalized]/_

m/um/(#|[$C-$LARYNGEAL])(ˈ)_(#|$C)
n/un/(#|[$C-$LARYNGEAL])(ˈ)_(#|$C)
l/ul/(#|[$C-$LARYNGEAL])(ˈ)_(#|$C)
r/ur/(#|[$C-$LARYNGEAL])(ˈ)_(#|$C)

$LARYNGEAL//#_$C

ː/ːː/_#

; e-coloring and dropping of laryngeals in onset
e/o/χʷ(ˈ)_
e/a/χ(ˈ)_
$LARYNGEAL//#_
$LARYNGEAL//_$V

; Homorganic vowels in hiatus -> long vowel
aa/aː/_
ee/eː/_
ii/iː/_
oo/oː/_
uu/uː/_

; e-coloring
e/o/_(w|j)χʷ
e/a/_(w|j)χ
eχʷ/oː/_
eχ/ɑː/_

; Compensatory lengthening with loss of laryngeals after sonorants
[-$long]/[+$long]/_$LARYNGEAL
$LARYNGEAL//($V|$SONORANT)_

; Cowgill's law
χʷ/g/($SONORANT)_w

; Vocalization of remaining laryngeals
$LARYNGEAL/ə/_

; Early PGmc

j//_(e|a|o)#
w//_(e|a|o)#
e//_#
a//_#
o//_#

; Grimm's law: voiceless stops become fricatives, except after an obstruent
; TODO: fix optional node moving to end inside branch of disjunct node
[$STOP-$voiced]/[+$fricative]/(#|$V|$SONORANT)_
;[$STOP-$voiced]/[+$fricative]/!($OBSTRUENT)_

; Undo Grimm's law after s-
[+$fricative-$voiced]/[-$fricative]/s_

; Germanic spirant law
[$STOP+$LABIAL]/ɸ/_(t|s) ;($C|$V)
[$STOP+$DENTAL]/ts/_(t|s) ;($C|$V)
;tst/ss/_
;tss/ss/_
ss/s/_
[$STOP+$VELAR]/x/_(t|s)($C|$V)

; Grimm's law: voiced unaspirated stops become voiceless stops
[$STOP+$voiced-$aspirated]/[-$voiced]/_(#|$V|ˈ$V|$C)

; Grimm's law: aspirated stops become unaspirated
[$STOP+$aspirated]/[-$aspirated]/_
g/ɣ/(#|$LIQUID|[+$fricative])_

; Lenition of intervocalic voiced stops
[$STOP+$voiced]/[+$fricative]/($V|$GLIDE)_(#|$V|$GLIDE)

; Verner's law
[+$fricative-$voiced]/[+$voiced]/(#|$C)[-$stressed-$SONORANT]($C)_

; Undo Verner's law before voiceless stops
[+$fricative+$voiced]/[-$voiced]/_[$STOP-$voiced]

; Voiced fricatives resulting from Verner's law to stops after nasal
[+$fricative+$voiced]/[-$fricative]/$NASAL_

; Stress moves to initial syllable. Let's just stop marking it.
[+$stressed]/[-$stressed]/_
s/z/$V_# ; Analogy

gʷ/b/#_

nw/nn/_
ln/ll/_
zm/mm/_

; TODO: make this be smart and match short vowels without having to manually specify what could come after
e/i/$V($GLIDE)($C)($C)_(#|$C)
; TODO: on error, allow the machine to jump to another branch (e.g. from $C to _) if that one is capable of matching the input
ji/i/$V($C)($C)_(#|$C)
ei/iː/_
ej/iː/_
ij/iː/_

o/ɑ/_
a/ɑ/_

; Late PGmc

m/n/_#
m/n/_$DENTAL
[-$nasalized]n/[+$nasalized]/_#

ẽː/ɑ̃ː/_

; Stressed schwa becomes /a/
ə/ɑ/#($C)($C)_

; Unstressed schwa disappears between consonants
ə//$C_$C
ə/ɑ/_

;t//$V$C($C)($C)$V($C)($C)_#
t//$V($C)($C)$V($C)_#
ɣʷ/w/_

ɑː/ɔː/_
ɑ̃ː/ɔ̃ː/_

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
    ɑ => ɑː
    a => aː
    e => eː
    i => iː
    o => oː
    u => uː
    ɔː 
    ɑ̃ => ɑ̃ː
    ẽ => ẽː
    ĩ => ĩː
    õ => õː
    ũ => ũː
    ɔ̃ː
}

[$nasalized] {
    ɑ => ɑ̃
    e => ẽ
    i => ĩ
    o => õ
    u => ũ
    ɑː => ɑ̃ː
    eː => ẽː
    iː => ĩː
    oː => õː
    uː => ũː
}

[$stressed] {
    a => ˈa
    aː => ˈaː
    ɑ => ˈɑ
    ɑː => ˈɑː
    e => ˈe
    eː => ˈeː
    i => ˈi
    iː => ˈiː
    o => ˈo
    oː => ˈoː
    ɔ => ˈɔ
    ɔː => ˈɔː
    u => ˈu
    uː => ˈuː
    ə => ˈə
    m => 'm
    n => 'n
    l => 'l
    r => 'r
}

; TODO map sets
; $V -> &ː
;   produces aː eː iː oː uː

$C {
    k  kʲ  kʷ  p  t
    g  gʲ  gʷ  b  d
    gʰ gʲʰ gʷʰ bʰ dʰ
    x      xʷ  ɸ  θ
    ɣ      ɣʷ  β  ð
    s  z
    ʔ  χ   χʷ
    m  n
    l  r
    w  j
}

$OBSTRUENT {
    k  kʲ  kʷ  p  t
    g  gʲ  gʷ  b  d
    gʰ gʲʰ gʷʰ bʰ dʰ
    x      xʷ  ɸ  θ
    ɣ      ɣʷ  β  ð
    s  z
    ʔ  χ   χʷ
}

$DENTAL { t d dʰ θ ð s z }
$LABIAL { m p b bʰ ɸ β }
$VELAR { k kʷ g gʷ x ɣ }
$SONORANT { m n l r w j }
$LIQUID { l r }
$GLIDE { w j }
$NASAL { m n }
$LARYNGEAL { ʔ χ χʷ }
$SIBILANT { s }

$STOP {
    k  kʲ  kʷ  p  t
    g  gʲ  gʷ  b  d
    gʰ gʲʰ gʷʰ bʰ dʰ
}

[$voiced] {
    k => g
    gʰ
    x => ɣ
    xʷ => ɣʷ
    kʲ => gʲ
    gʲʰ
    kʷ => gʷ
    gʷʰ
    p => b
    bʰ
    ɸ => β
    t => d
    s => z
    dʰ
    θ => ð
}

[$palatalized] {
    k => kʲ
    g => gʲ
    gʰ => gʲʰ
}

[$labialized] {
    k => kʷ
    g => gʷ
    gʰ => gʷʰ
}

[$aspirated] {
    g => gʰ
    gʲ => gʲʰ
    gʷ => gʷʰ
    b => bʰ
    d => dʰ
}

[$fricative] {
    k => x
    kʷ => xʷ
    p => ɸ
    t => θ
    g => ɣ
    gʷ => ɣʷ
    b => β
    d => ð
    s
}
