; Ingvaeonic and Anglo-Frisian

; Nasal spirant law
[-Long] → [+Long] / _nFRICATIVE
[-Nasalized] → [+Nasalized] / _nFRICATIVE
n → ∅ / [+Nasalized]_

ɑ̃ː→ɔ̃ː

ɑ(i|j) → ɑː

; Anglo-Frisian brightening
ɑ → æ / _C
æ̃ → ɑ̃
æ → ɑ / _[+Geminate]
æ → ɑ / _C(C)(C)[V-Front]

(æ|ɑ|ɑ̃) → ∅ / _#

; Old English

[+Nasalized] → [-Nasalized]
;∅→u/[+Front]_(x|w|(r|l)C)

; Front vowel breaking
[Broken] { ɑ → ɑu, e → eu, i → iu }
[-Broken] → [+Broken] / _(x(V|#)|w(V|#)|rC|lC)
(ɑu|ɑw) → æɑ
(eu|ew) → eo

ɑj → ɑː

; ...

(i|u) → ∅ / (ː|C)C_#

ø → e
iu → eo
ɣ → g / #_


[Front] {
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

[Long] {
	ɑ → ɑː
	e → eː
	i → iː
	ɔ → ɔː
	u → uː
}

[Overlong] {
	ɑː → ɑːː
	ɑ̃ː → ɑ̃ːː
	ɔː → ɔːː
	ɔ̃ː → ɔ̃ːː
}

[Nasalized] {
	ɑ → ɑ̃
	ɑː → ɑ̃ː
	ɑːː → ɑ̃ːː
	e → ẽ
	i → ĩ
	ɔ → ɔ̃
	ɔː → ɔ̃ː
	ɔːː → ɔ̃ːː
	u → ũ
	uː → ũː
}

[Rounded] {
	k → kʷ
	x → xʷ
	ɣ → ɣʷ
}

[Geminate] {
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

BACK { ɑ ɔ o u }

V { [+Long] [-Long] [+Front] [-Front] Overlong Nasalized }
STOP { k kʷ p t g b d }
LIQUID { r l }
NASAL { m n }
FRICATIVE { x xʷ f θ ɣ v ð s z }
SONORANT { LIQUID NASAL V }
C { STOP LIQUID NASAL FRICATIVE }
