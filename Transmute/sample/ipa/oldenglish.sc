;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Ingvaeonic and Anglo-Frisian   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Nasal spirant law

V n → [+Long +Nasalized] / _FRICATIVE

(ɑ̃ː|æ̃ː) → ɔ̃ː

ɑ(i|j) → ɑː

; Anglo-Frisian brightening

ɑ → æ / _[C-/m n/](C)(C)([V+Front])
æ̃ → ɑ̃

; Undo before syllable with back vowel
;æ → ɑ / _C(C)(C)[V-Front]

; Redo before velar consonant
ɑ → æ / _(x|w|rC|lC)

[+/æ ɑ ɑ̃/] → ∅ / _#

;;;;;;;;;;;;;;;;;;;
;   Old English   ;
;;;;;;;;;;;;;;;;;;;

x → h / (#_ | _V)

[+Nasalized] → [-Nasalized]

; Front vowel breaking

∅ → u / [V+Front-/y/]_((x|w)(V|#) | rC | lC)

; Diphthong height harmonization

[+/ɑi ɑj/] → ɑː
[+/æu æw/] → æɑ
[+/eu ew/] → eo

; A-restoration

æ → ɑ / _[+Geminate] ( [C-/j/][V-Front] | # )

; Palatalization of velars

[Palatalized] (k → tʃ, g → dʒ, gg → dʒ)
[-Palatalized] → [+Palatalized] / _(i(ː)|j)
[-Palatalized] → [+Palatalized] / iː_(C|#)
ɣ → ʝ / [V+Front]_
ɣ → ʝ / _[V+Front]
sk → ʃ

; ...

[-Front] → [+Front] / _[C-/j/] ([C-/j/])([C-/j/])(i|j)

(i|u) → ∅ / (ː|C)C_#

(i)j → ∅ / [V+Long+/æɑ eo iy æɑː eoː iyː/]C(C)(C)_
(i)j → ∅ / [V-Long+/æɑ eo iy/]C.C(C)(C)_

ʝ → j
j → ∅ / C_#

; Final syllable vowel reductions

oː → ɑ / _(C)(C)(C)#
u ͏→ o / _C(C)(C)#
(æ|i) → e / _(C)(C)(C)#

ø → e
iu → eo
ɣ → g / #_

[FRICATIVE+Voiced] -> [-Voiced] / _#


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

[Voiced] (
	x → ɣ
	f → v
	θ → ð
	s → z
)

[Front] (
	ɑ → æ
	ɑː → æː
	æɑ → iy
	æɑː → iyː
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
