;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Ingvaeonic and Anglo-Frisian   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Nasal spirant law

[+V]n → [+Nasalized +Long] / _FRICATIVE

(ɑ̃ː|æ̃ː) → ɔ̃ː

ɑ(i|j) → ɑː

; Anglo-Frisian brightening

ɑ → æ / _C
æ̃ → ɑ̃
æ → ɑ / _C(C)(C)[V-Front]
æ → ɑ / _C(C)(C)[V-Front]

(æ|ɑ|ɑ̃) → ∅ / _#

;;;;;;;;;;;;;;;;;;;
;   Old English   ;
;;;;;;;;;;;;;;;;;;;

[+Nasalized] → [-Nasalized]

; Front vowel breaking

∅ → u / [V+Front-/y/]_((x|w)(V|#) | rC | lC)

; Diphthong height harmonization

[+/ɑi ɑj/] → ɑː
[+/æu æw/] → æɑ
[+/eu ew/] → eo

; A-restoration

æ → ɑ / _[+Geminate]

; Palatalization of velars

[Palatalized] (k → tʃ, g → dʒ)
[-Palatalized] → [+Palatalized] / _(i(ː)|j)
[-Palatalized] → [+Palatalized] / iː_(C|#)
ɣ → ʝ / [V+Front]_
ɣ → ʝ / _[V+Front]
sk → ʃ

; ...

[-Front] → [+Front] / _C(C)(C)(i|j)

(i|u) → ∅ / (ː|C)C_#

ʝ → j
j → ∅ / C_#

; Final syllable vowel reductions

oː → ɑ / _(C)(C)(C)#
u ͏→ o / _C(C)(C)#
(æ|i) → e / _(C)(C)(C)#

ø → e
iu → eo
ɣ → g / #_


;;;;;;;;;;;;;;;;;;;;;;;;;
;   Sets and features   ;
;;;;;;;;;;;;;;;;;;;;;;;;;

V (Long [-Long] Front [-Front] Overlong Nasalized)
STOP (k kʷ p t g b d)
GLIDE (w j)
LIQUID (r l)
NASAL (m n)
FRICATIVE (x xʷ f θ ɣ v ð s z)
AFFRICATE (tʃ dʒ)
SONORANT (LIQUID NASAL V)
C (STOP GLIDE LIQUID NASAL FRICATIVE AFFRICATE)

[Front] (
	ɑ → æ
	ɑː → æː
	æɑ → yː
	æɑː → yː
	eo → y
	eoː → yː
	e
	eː
	i
	iː
	ɔ → ø
	ɔː → øː
	u → y
	uː → yː
)

[Long] (
	æ → æː
	ɑ → ɑː
	e → eː
	i → iː
	ɔ → ɔː
	ø → øː
	u → uː
	y → yː
)

[Overlong] (
	ɑː → ɑːː
	ɑ̃ː → ɑ̃ːː
	ɔː → ɔːː
	ɔ̃ː → ɔ̃ːː
)

[Nasalized] (
	ɑ → ɑ̃
	ɑː → ɑ̃ː
	ɑːː → ɑ̃ːː
	æ → æ̃
	æː → æ̃ː
	e → ẽ
	i → ĩ
	ɔ → ɔ̃
	ɔː → ɔ̃ː
	ɔːː → ɔ̃ːː
	ø → ø̃
	øː → ø̃ː
	u → ũ
	uː → ũː
	y → ỹ
	yː → ỹː
)

[Rounded] (
	k → kʷ
	x → xʷ
	ɣ → ɣʷ
)

[Geminate] (
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
)
