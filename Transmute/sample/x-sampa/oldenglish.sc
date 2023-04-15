;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Ingvaeonic and Anglo-Frisian   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Ingvaeonic nasal spirant law

$V.n -> [+Long +Nasalized] / _$Fricative

(A~:|}~:) -> O~:

(Ai|Aj) -> A:

; Anglo-Frisian brightening

A -> } / _[C-/m n/]($C)($C)([V+Front])
}~ -> A~

; Undo before syllable with back vowel
;} -> A / _$C($C)($C)[V-Front]

; Re-undo before velar consonant
A -> } / _(x|w|r$C|l$C)

[+/} A A~/] // _#

;;;;;;;;;;;;;;;;;;;
;   Old English   ;
;;;;;;;;;;;;;;;;;;;

[+Nasalized] -> [-Nasalized]

; Front vowel breaking

/ u / [V+Front-/y/]_((x|h|w)($V|#) | r$C | l$C)

; Diphthong height harmonization

[+/Ai Aj/] -> A:
[+/}u }w/] -> }A
[+/eu ew/] -> eo

; A-restoration

} -> A / _[+Geminate]( [C-/j/][V-Front] | # )

; Palatalization of velars

[Palatalized] (k -> tS, g -> dZ, gg -> dZ)

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

[-Front] -> [+Front] / _[C-/j/]([C-/j/])([C-/j/])(i|j)

; Loss of short, low vowels in medial syllables

(A|}|e) // $V$C($C)($C)_$C$V

; High vowel loss

(i|u) // ([$V+Long] | $Diphthong | $C)$C_#

(i)j // ([V+Long] | $Diphthong)$C($C)($C)_
(i)j // ([V-Long] | [Diphthong-Long])$C$C($C)($C)_

j\ -> j
j // $C_#

; H-loss

(h|x) // ($V|l|r)_$V

; Palatal umlaut

(e|eo) -> i / _(xs|xt)

; Final syllable vowel reductions

o: -> A / _($C)($C)($C)#
u -> o / _$C($C)($C)#
(}|i) -> e / _($C)($C)($C)#

2 -> e
iu -> eo
G -> g / #_

[Fricative+Voiced] -> [-Voiced] / _#

xs -> ks
h -> x / _#


;;;;;;;;;;;;;;;;;;;;;;;;;
;   Sets and features   ;
;;;;;;;;;;;;;;;;;;;;;;;;;

$V ($Long [-Long] $Front [-Front] $Overlong $Nasalized)
$Stop (k k_w p t g b d)
$Glide (w j)
$Liquid (r l)
$Nasal (m n)
$Fricative (h x x_w f T G v D s z)
$Affricate (tS dZ)
$Sonorant ($Liquid $Nasal $V)
$C ($Stop $Glide $Liquid $Nasal $Fricative $Affricate)

$Diphthong (}A, }A:, eo, eo:, iy, iy:)

[Voiced] (
	x -> G
	f -> v
	T -> D
	s -> z
)

[Front] (
	A -> }
	A: -> }:
	}A -> iy
	}A: -> iy:
	eo -> y
	eo: -> y:
	e
	e:
	i
	i:
	O -> 2
	O: -> 2:
	u -> y
	u: -> y:
)

[Long] (
	} -> }:
	A -> A:
	e -> e:
	i -> i:
	O -> O:
	2 -> 2:
	u -> u:
	y -> y:
	}A -> }A:
	eo -> eo:
	iy -> iy:
)

[Overlong] (
	A: -> A::
	A~: -> A~::
	O: -> O::
	O~: -> O~::
)

[Nasalized] (
	A -> A~
	A: -> A~:
	A:: -> A~::
	} -> }~
	}: -> }~:
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

[Rounded] (
	k -> k_w
	x -> x_w
	G -> G_w
)

[Geminate] (
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
