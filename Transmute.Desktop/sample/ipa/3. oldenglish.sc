Syllable = (
    Onset = (s)(C)(C)
    Nucleus = (ˈ)V
    Coda = (C)(C)(C)(C)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Ingvaeonic and Anglo-Frisian   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

nasal-spirant-law:
	V.n → [+Long +Nasalized] / _Fricative

(ɑ̃ː|æ̃ː) → ɔ̃ː

(ɑi|ɑj) → ɑː

a-brightening:
	ɑ!ː → æ / _!(m|n)

(æ|ɑ|ɑ̃) → ∅ / _#

;;;;;;;;;;;;;;;;;;;
;   Old English   ;
;;;;;;;;;;;;;;;;;;;

front-vowel-breaking:
	∅ → u / [V+Front-/y/]_((x|h|w)(V|#) | rC | lC)

teuxun → texun

height-harmonization: (
	(ɑi|ɑj) → ɑː
	(æu|æw) → æɑ
	(eu|ew) → eo
)

a-restoration:
	æ → ɑ / _[+Geminate] ( [C-/j/][V-Front] | # )

; Palatalization of velars

[Palatalized] = (k → tʃ, g → dʒ, gg → dʒ)

palatalization: (
	[-Palatalized] → [+Palatalized] / _(i(ː)|j)
	[-Palatalized] → [+Palatalized] / iː_(C|#)
)

ɣ → ʝ / [V+Front]_
ɣ → ʝ / _[V+Front]
sk → ʃ

; ... WIP ...

x → h / #_
x → h / _V
hʷ → hw

umlaut:
	[-Front] → [+Front] / _[C-/j ʝ/] ([C-/j ʝ/]) ([C-/j ʝ/]) (i|j|ʝ)

high-vowel-loss: (
	(i|u) → ∅ / ([V+Long] | Diphthong | C)C_#
	(i)j → ∅ / ([V+Long] | Diphthong)C_σσ
	(i)j → ∅ / ([V-Long] | [Diphthong-Long])C.C_σσ
)

ʝ → j
j → ∅ / C_#

intervocalic-voicing:
	[Fricative-Voiced] -> [+Voiced] / V_V

h-loss:
	V.h → [+Long] / ((r|l))_V

palatal-umlaut:
	(e|eo) → i / _(xs|xt)

unstressed-vowel-reduction: (
	ɔː → u / _#
	oː → ɑ / _σ#
	[+Nasalized] → [-Nasalized]
	[V+Long] → [-Long] / _(C C C|C C σσ)
	u → o / _Cσ#
	(æ|i) → e / _Cσ#
	ɔ → o
)

medial-syncopation:
	(ɑ|æ|e) → ∅ / σσ_σV

ø → e
iu → eo
ɣ → g / #_

word-final-devoicing:
	[Fricative+Voiced] → [-Voiced] / _#

xs → ks
h → x / _#
lθ → ld

;;;;;;;;;;;;;;;;;;;;;;;;;
;   Sets and features   ;
;;;;;;;;;;;;;;;;;;;;;;;;;

V = (Long [-Long] Front [-Front] Overlong Nasalized)
Stop = (k kʷ p t g b d)
Glide = (w j)
Liquid = (r l)
Nasal = (m n)
Fricative = (h x xʷ hʷ f θ ɣ ʝ v ð s z)
Affricate = (tʃ dʒ)
Sonorant = (Liquid Nasal V)
C = (Stop Glide Liquid Nasal Fricative Affricate)

Diphthong = (æɑ, æɑː, eo, eoː, iy, iyː)

[Voiced] = (
	x → ɣ
	f → v
	θ → ð
	s → z
)

[Voiced] = (
	x → ɣ
	f → v
	θ → ð
	s → z
)

High = (i, iː, u, uː, y, yː, iy, iyː)

[Front] = (
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

[Long] = (
	æ → æː
	ɑ → ɑː
	e → eː
	i → iː
	o → ɔː
	o → oː	
	ø → øː
	ø̃ → ø̃ː
	u → uː
	y → yː
    æɑ → æɑː
    eo → eoː
    iy → iyː
)

[Overlong] = (
	ɑː → ɑːː
	ɑ̃ː → ɑ̃ːː
	ɔː → ɔːː
	ɔ̃ː → ɔ̃ːː
)

[Nasalized] = (
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

[Rounded] = (
	k → kʷ
	x → xʷ
	ɣ → ɣʷ
)

[Geminate] = (
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
