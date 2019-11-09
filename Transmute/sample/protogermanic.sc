; From https://en.wikipedia.org/wiki/Proto-Germanic_language#Phonological_stages_from_Proto-Indo-European_to_end_of_Proto-Germanic

; TODO implement NOT nodes
;   $SONORANT/u$SONORANT/$V_!$V
;   $SONORANT/u$SONORANT/!$V_$V

; Pre-PGmc

m/um/(#|$C)_(#|$C)
n/un/(#|$C)_(#|$C)
l/ul/(#|$C)_(#|$C)
r/ur/(#|$C)_(#|$C)

; n/m/_$LABIAL

; TODO implement transformation of Ø, i.e. insertion
; /u/#_$SONORANT$C

; kʲ/k/_
; gʲ/g/_
; gʲʰ/gʰ/_

; k/kʷ/_w
; g/gʷ/_w
; gʰ/gʷʰ/_w

[+$palatalized]/[-$palatalized]/_
[+$labialized]/[-$labialized]/_

$LARYNGEAL//#_[$C-$LARYNGEAL]
ʔ//_$V

χʷe/o/_
eʔ/eː/_
eχ/ɑː/_
eχʷ/oː/_

χ/a/(#|$C)_(#|$C)
χʷ/o/(#|$C)_(#|$C)
$LARYNGEAL/ə/(#|$C)_(#|$C)

χʷ/g/$SONORANT_w

; Early PGmc

y//_[$V-$high]#
w//_[$V-$high]#
[$V-$high]//_#

; Grimm's law

[$STOP+$LABIAL]/ɸ/_t
[$STOP+$VELAR]/x/_t

; Grimm's law

[$STOP-$voiced]/[+$fricative]/(#|$VGLIDE|$NASAL)_
[$STOP+$voiced]/[-$voiced]/(#|$VGLIDE|$NASAL)_
[$STOP+$aspirated]/[-$aspirated]/(#|$VGLIDE|$NASAL)_

;[$STOP-$voiced]/[+$fricative]/$VGLIDE_
;[$STOP+$voiced]/[-$voiced]/$VGLIDE_
;[$STOP+$aspirated]/[-$aspirated]/$VGLIDE_

; Verner's law

[+$fricative]/[+$voiced]/(#|$C)[-$stressed]($C)_

; Stress moves to initial syllable. Let's just stop marking it.
[+$stressed]/[-$stressed]/_

gʷ/b/#_

o/ɑ/_
a/ɑ/_

; Late PGmc
m/n/_#
m/n/_$DENTAL

ɑ$NASAL/ɑ̃/_

ˈə/ˈɑ/$C_$C
ə//$C_$C

g/w/$VGLIDE_$VGLIDE

;e/i/!ˈ_
yi/i/_
ii/iː/_

$VSHORT { ə ɑ a e i o u }

$VLONG { ɑː aː eː iː oː uː }

$V {
    ə
    ɑ  a  e  i  o  u
    ɑː aː eː iː oː uː
}

[$stressed] {
    ɑ => ˈɑ
    e => ˈe
    i => ˈi
    o => ˈo
    u => ˈu
    ə => ˈə
}

; TODO map sets
; $V -> &ː
;   produces aː eː iː oː uː

$VGLIDE {
    ə
    ɑ  a  e  i  o  u
    ɑː aː eː iː oː uː
    w  y
}

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
$SONORANT { m n l r w y }
$NASAL { m n }
$LARYNGEAL { ʔ χ χʷ }

$STOP {
    k  kʲ  kʷ  p  t
    g  gʲ  gʷ  b  d
    gʰ gʲʰ gʷʰ bʰ dʰ
}

[$mid] { e o }
[$round] { o u }
[$high] { i u }

[$nasalized] {
    a => ã
    e => ẽ
    i => ĩ
    o => õ
    u => ũ
}

[$voiced] {
    k => g
    gʰ
    x => ɣ
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
    b => β
    d => ð
}
