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

m/um/(#|$C)_$C
n/un/(#|$C)_$C
l/ul/(#|$C)_$C
r/ur/(#|$C)_$C

$LARYNGEAL//#_$C

; e-coloring and dropping of laryngeals in onset
χʷe/o/_
χe/a/_
$LARYNGEAL//_$V

; e-coloring, compensatory lengthening and dropping of laryngeals in coda
e/o/_(w|j)χʷ
e/a/_(w|j)χ
eχʷ/oː/_
eχ/ɑː/_
$LARYNGEAL/ː/$V_

; Cowgill's law
χʷ/g/($GLIDE|$SONORANT)_w

; Vocalization of remaining laryngeals
$LARYNGEAL/ə/_

; Early PGmc

j//_(e|a|o)#
w//_(e|a|o)#
e//_#
a//_#
o//_#

; Grimm's law: voiceless stops become fricatives
; TODO: fix optional node moving to end inside branch of disjunct node
[$STOP-$voiced]/[+$fricative]/_($V|ˈ$V|$C)
[+$fricative-$voiced-$SIBILANT]/[-$fricative]/[$C-$voiced]_

; Undo Grimm's law after s-
[+$fricative-$voiced]/[-$fricative]/s_

; Grimm's law: voiced unaspirated stops become voiceless stops
[$STOP+$voiced-$aspirated]/[-$voiced]/_(#|$V|ˈ$V|$C)

; Grimm's law: aspirated stops become unaspirated
[$STOP+$aspirated]/[-$aspirated]/_
g/ɣ/#_

; Lenition of intervocalic voiced stops
[$STOP+$voiced]/[+$fricative]/($V|$GLIDE)_($V|$GLIDE)

; Verner's law
[+$fricative]/[+$voiced]/(#|$C)[-$stressed]($C)_

; Undo Verner's law before voiceless stops
[+$fricative+$voiced]/[-$voiced]/_[$STOP-$voiced]

; Germanic spirant law
[$STOP+$LABIAL]/ɸ/_(t|s)($C|$V)
[$STOP+$DENTAL]/ts/_(t|s)($C|$V)
tst/ss/_
tss/ss/_
[$STOP+$VELAR]/x/_(t|s)($C|$V)

; Stress moves to initial syllable. Let's just stop marking it.
[+$stressed]/[-$stressed]/_
s/z/$V_# ; Analogy

gʷ/b/#_

; TODO: make this be smart and match short vowels without having to manually specify what could come after
e/i/$V($GLIDE)($C)($C)_(#|$C)
ji/i/$V($GLIDE)($C)($C)_(#|$C)
ei/iː/_
ej/iː/_

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


$VSHORT { ə ɑ a e i o u }

$VLONG { ɑː aː eː iː oː uː }

$GLIDE { w j }

$V {
    ə
    ɑ  a  e  i  o     u
    ɑː aː eː iː oː ɔː uː
    ɑ̃  ẽ  ĩ  õ     ũ
    ɑ̃ː ẽː ĩː õː ɔ̃ː ũː
}

[$stressed] {
    a => ˈa
    ɑ => ˈɑ
    e => ˈe
    i => ˈi
    o => ˈo
    ɔ => ˈɔ
    u => ˈu
    ə => ˈə
}

; TODO map sets
; $V -> &ː
;   produces aː eː iː oː uː

$C {
    s
    ʔ  χ   χʷ
    k  kʲ  kʷ  p  t
    g  gʲ  gʷ  b  d
    gʰ gʲʰ gʷʰ bʰ dʰ
    m  n   l   r  w  y
    x  xʷ  ɸ   θ  ɣ  β  ð
}

$DENTAL { t d dʰ θ ð }
$LABIAL { m p b bʰ ɸ β }
$VELAR { k kʷ g gʷ x ɣ }
$SONORANT { m n l r }
$NASAL { m n }
$LARYNGEAL { ʔ χ χʷ }
$SIBILANT { s }

$STOP {
    k  kʲ  kʷ  p  t
    g  gʲ  gʷ  b  d
    gʰ gʲʰ gʷʰ bʰ dʰ
}

[$mid] { e o }
[$round] { o u }
[$high] { i u }

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
