;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Ingvaeonic and Anglo-Frisian   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Ingvaeonic nasal spirant law

V n → [+Long +Nasalized] / _Fricative

(ɑ̃ː|æ̃ː) → ɔ̃ː

(ɑi|ɑj) → ɑː

; Anglo-Frisian brightening

ɑ → æ / _[C-/m n/](C)(C)([V+Front])
æ̃ → ɑ̃
æ → ɑ / _C(C)(C)[V-Front]
æ → ɑ / _C(C)(C)[V-Front]

(æ|ɑ|ɑ̃) → ∅ / _#

; Redo before velar consonant
ɑ → æ / _(x|w|rC|lC)

[+/æ ɑ ɑ̃/] → ∅ / _#

;;;;;;;;;;;;;;;;;;;
;   Old English   ;
;;;;;;;;;;;;;;;;;;;

[+Nasalized] → [-Nasalized]

; Front vowel breaking

∅ → u / [V+Front-/y/]_((x|h|w)(V|#) | rC | lC)

; Diphthong height harmonization

[+/ɑi ɑj/] → ɑː
[+/æu æw/] → æɑ
[+/eu ew/] → eo

; A-restoration

æ → ɑ / _[+Geminate] ( [C-/j/][V-Front] | # )

; Palatalization of velars

[Palatalized] (k → tʃ, g → dʒ)
[-Palatalized] → [+Palatalized] / _(i(ː)|j)
[-Palatalized] → [+Palatalized] / iː_(C|#)
ɣ → ʝ / [V+Front]_
ɣ → ʝ / _[V+Front]
sk → ʃ

; ... WIP ...

x → h / #_
x → h / _V
hʷ → hw

; Umlaut

[-Front] → [+Front] / _[C-/j ʝ/] ([C-/j ʝ/])([C-/j ʝ/])(i|j|ʝ)

; Loss of short, low vowels in medial syllables

(ɑ|æ|e) → ∅ / V.C(C)(C)_C.V

; High vowel loss

[-Front] → [+Front] / _C(C)(C)(i|j)

(i)j → ∅ / ([V+Long] | Diphthong)C(C)(C)_
(i)j → ∅ / ([V-Long] | [Diphthong-Long])C.C(C)(C)_

(i)j → ∅ / [V+Long+/æɑ eo iy æɑː eoː iyː/]C(C)(C)_
(i)j → ∅ / [V-Long+/æɑ eo iy/]C.C(C)(C)_

ʝ → j
j → ∅ / C_#

[Fricative-Voiced] -> [+Voiced] / V_V

; H-loss

(h|x) → ∅ / (V|l|r)_V

; Palatal umlaut

(e|eo) → i / _(xs|xt)

; Unstressed vowel reductions

ɔː → u / _#
oː → ɑ / _(C)(C)(C)#
[+Nasalized] → [-Nasalized]
[V+Long] → [-Long] / V.C(C)(C)_(C)(C)(C)(V|#)
u → o / V.C(C)(C)_C(C)(C)(V|#)
(æ|i) → e / _(C)(C)(C)#
ɔ → o

ø → e
iu → eo
ɣ → g / #_


xs → ks
h → x / _#

;;;;;;;;;;;;;;;;;;;;;;;;;
;   Sets and features   ;
;;;;;;;;;;;;;;;;;;;;;;;;;

V (Long [-Long] Front [-Front] Overlong Nasalized)
Stop (k kʷ p t g b d)
Glide (w j)
Liquid (r l)
Nasal (m n)
Fricative (h x xʷ f θ ɣ v ð s z)
Affricate (tʃ dʒ)
Sonorant (Liquid Nasal V)
C (Stop Glide Liquid Nasal Fricative Affricate)

Diphthong (æɑ, æɑː, eo, eoː, iy, iyː)

[Voiced] (
	x → ɣ
	f → v
	θ → ð
	s → z
)

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
	o → ø
	ɔː → øː
	ɔ̃ː → ø̃ː
	u → y
	uː → yː
)

[Long] (
	æ → æː
	ɑ → ɑː
	e → eː
	i → iː
	o → ɔː
	ø → øː
	ø̃ → ø̃ː
	u → uː
	y → yː
    æɑ → æɑː
    eo → eoː
    iy → iyː
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
