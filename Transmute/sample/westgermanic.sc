; Northwest Gmc.

[-$front]→[+$front]/_$C($C)($C)(i|j)
u→o/_lθ[$V-$high]
[-$short]→[+$short]/_#
[+$overlong]→[-$overlong]

; West Gmc.

[+$rounded]/[-$rounded]/($C|$V)_
(zw|ðw)→ww
ð→d
β→v
ɸ→f
z→∅/_#
z→r/$SONORANT_
s→∅/$C_#
a→∅/$C($C)($C)_#
[-$geminate]→[+$geminate]/_j

[$high] {
	i iː y yː u uː
}

[$front] {
	ɑ → æ
	ɑː → æː
	ɔ → ø
	ɔː → øː
	u → y
	uː → yː
}

[$short] {
	eː → ɑ
	ɔːi → u
	ɔːj → u
	ɔː → u
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

$V { $short $long $overlong $nasalized }
$STOP { k kʷ p t g b d }
$LIQUID { r l }
$NASAL { m n }
$GLIDE { w j }
$FRICATIVE { x xʷ ɸ f θ ɣ β v ð s z }
$SONORANT { $LIQUID $NASAL $GLIDE $V }
$C { $STOP $LIQUID $NASAL $FRICATIVE }
