; Ingvaeonic and Anglo-Frisian

; Nasal spirant law
[-$long]→[+$long]/_n$FRICATIVE
[-$nasalized]n→[+$nasalized]/_$FRICATIVE
ɑ̃ː→ɔ̃ː

ɑ(i|j)→ɑː

; Anglo-Frisian brightening
ɑ→æ/_$C
æ̃→ɑ̃
æ→ɑ/_[+$geminate]
æ→ɑ/_$C($C)($C)$BACK

(æ|ɑ|ɑ̃)→∅/_#

; Old English

[+$nasalized]→[-$nasalized]
;∅→u/[+$front]_(x|w|(r|l)$C)

; ...

(i|u)→∅/(ː|$C)$C_#

ø→e
iu→eo
ɣ→g/#_

[$front] {
	ɑ → æ
	ɑː → æː
	e
	eː
	i
	iː
	ɔ → ø
	ɔː → øː
	u → y
	uː → yː
}

[$long] {
	ɑ → ɑː
	e → eː
	i → iː
	ɔ → ɔː
	u → uː
}

[$overlong] {
	ɑː → ɑːː
	ɑ̃ː → ɑ̃ːː
	ɔː → ɔːː
	ɔ̃ː → ɔ̃ːː
}

[$nasalized] {
	ɑ → ɑ̃
	ɑː → ɑ̃ː
	ɑːː → ɑ̃ːː
	e → ẽ
	i → ĩ
	ɔ → ɔ̃
	ɔː → ɔ̃ː
	ɔːː → ɔ̃ːː
	u → ũ
}

[$rounded] {
	k → kʷ
	x → xʷ
	ɣ → ɣʷ
}

[$geminate] {
	k → kk
	p → pp
	t → tt
	ɣ → gg
	β → bb
	d → dd
	x → xx
	ɸ → ɸɸ
	θ → θθ
	l → ll
}

$BACK { ɑ ɔ o u }

$V { $short $long $overlong $nasalized }
$STOP { k kʷ p t g b d }
$LIQUID { r l }
$NASAL { m n }
$FRICATIVE { x xʷ f θ ɣ v ð s z }
$SONORANT { $LIQUID $NASAL $V }
$C { $STOP $LIQUID $NASAL $FRICATIVE }
