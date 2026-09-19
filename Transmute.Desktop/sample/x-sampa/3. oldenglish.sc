$Syllable = (
    $Onset = (s)($C)($C)
    $Nucleus = (")$V
    $Coda = ($C)($C)($C)($C)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Ingvaeonic and Anglo-Frisian   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Ingvaeonic nasal spirant law

$V.n -> [+Long +Nasalized] / _$Fricative

(A~:|{~:) -> O~:

(Ai|Aj) -> A:

; Anglo-Frisian brightening

A!: -> { / _!(m|n)

[+/{ A A~/] // _#

;;;;;;;;;;;;;;;;;;;
;   Old English   ;
;;;;;;;;;;;;;;;;;;;

; Front vowel breaking

/ u / [V+Front-/y/]_((x|h|w)($V|#) | r$C | l$C)
teuxun -> texun

; Diphthong height harmonization

[+/Ai Aj/] -> A:
[+/{u {w/] -> {A
[+/eu ew/] -> eo

; A-restoration

{ -> A / _[+Geminate]( [C-/j/][V-Front] | # )

; Palatalization of velars

[Palatalized] = (k -> tS, g -> dZ, gg -> dZ)

[-Palatalized] -> [+Palatalized] / _(i(:)|j)
[-Palatalized] -> [+Palatalized] / i: _ ($C|#)
G -> j\ / [V+Front]_
G -> j\ / _[V+Front]
sk -> S

; ... WIP ...

x -> h / #_
x -> h / _$V
h_w -> hw

; Umlaut

[-Front] -> [+Front] / _[C-/j/]([C-/j/])([C-/j/])(i|j|j\)

; High vowel loss

(i|u) // ([V+Long] | $Diphthong | $C)$C_#

(i)j // ([V+Long] | $Diphthong)$C_$$
(i)j // ([V-Long] | [Diphthong-Long])$C$C_$$

j\ -> j
j // $C_#

[Fricative-Voiced] -> [+Voiced] / $V_$V

; H-loss

V(h|x) -> [+Long] / (r|l)_$V

; Palatal umlaut

(e|eo) -> i / _(xs|xt)

; Unstressed vowel reductions

O: -> u / _#
o: -> A / _$C$C$C#
[+Nasalized] -> [-Nasalized]
[V+Long] -> [-Long] / _($C$C$C|$C$C$$)
u -> o / _$C$#
({|i) -> e / _$C$#
O -> o

; Loss of short, low vowels in medial syllables

(A|{|e) // $$_$$V

2 -> e
iu -> eo
G -> g / #_

[Fricative+Voiced] -> [-Voiced] / _#

xs -> ks
h -> x / _#
lT -> ld

;;;;;;;;;;;;;;;;;;;;;;;;;
;   Sets and features   ;
;;;;;;;;;;;;;;;;;;;;;;;;;

$V = ($Long [-Long] $Front [-Front] $Overlong $Nasalized)
$Stop = (k k_w p t g b d)
$Glide = (w j)
$Liquid = (r l)
$Nasal = (m n)
$Fricative = (h x x_w f T G j\ v D s z)
$Affricate = (tS dZ)
$Sonorant = ($Liquid $Nasal $V)
$C = ($Stop $Glide $Liquid $Nasal $Fricative $Affricate)

$Diphthong = ({A, {A:, eo, eo:, iy, iy:)

[Voiced] = (
	x -> G
	f -> v
	T -> D
	s -> z
)

[Voiced] = (
	x -> G
	f -> v
	T -> D
	s -> z
)

$High = (i, i:, u, u:, y, y:, iy, iy:)

[Front] = (
	A -> {
	A: -> {:
	{A -> iy
	{A: -> iy:
	eo -> y
	eo: -> y:
	e
	e:
	i
	i:
	o -> 2
	O: -> 2:
	O~: -> 2~:
	u -> y
	u: -> y:
)

[Long] = (
	{ -> {:
	A -> A:
	e -> e:
	i -> i:
	o -> O:
	o -> o:
	2 -> 2:
	2~ -> 2~:
	u -> u:
	y -> y:
	{A -> {A:
	eo -> eo:
	iy -> iy:
)

[Overlong] = (
	A: -> A::
	A~: -> A~::
	O: -> O::
	O~: -> O~::
)

[Nasalized] = (
	A -> A~
	A: -> A~:
	A:: -> A~::
	{ -> {~
	{: -> {~:
	e -> e~
	i -> i~
	O -> O~
	O: -> O~:
	O:: -> O~::
	2 -> 2~
	2: -> 2~:
	u -> u~
	u: -> u~:
	y -> y~
	y: -> y~:
)

[Rounded] = (
	k -> k_w
	x -> x_w
	G -> G_w
)

[Geminate] = (
	k -> kk
	p -> pp
	t -> tt
	G -> gg
	B -> bb
	d -> dd
	x -> xx
	p\ -> p\p\
	T -> TT
	l -> ll
)
