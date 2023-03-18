;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Ingvaeonic and Anglo-Frisian   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Nasal spirant law

[+V]n -> [+Long +Nasalized] / _$FRICATIVE

(A~:|}~:) -> O~:

A(i|j) -> A:

; Anglo-Frisian brightening

A -> } / _$C
}~ -> A~
} -> A / _$C($C)($C)[V-Front]
} -> A / _$C($C)($C)[V-Front]

[+/} A A~/] // _#

;;;;;;;;;;;;;;;;;;;
;   Old English   ;
;;;;;;;;;;;;;;;;;;;

[+Nasalized] -> [-Nasalized]

; Front vowel breaking

/ u / [V+Front-/y/]_((x|w)($V|#) | r$C | l$C)

; Diphthong height harmonization

[+/Ai Aj/] -> A:
[+/}u }w/] -> }A
[+/eu ew/] -> eo

; A-restoration

} -> A / _[+Geminate]

; Palatalization of velars

[Palatalized] (k -> tS, g -> dZ)
[-Palatalized] -> [+Palatalized] / _(i(:)|j)
[-Palatalized] -> [+Palatalized] / i: _ ($C|#)
G -> j\ / [V+Front]_
G -> j\ / _[V+Front]
sk -> S

; ...

[-Front] -> [+Front] / _$C($C)($C)(i|j)

(i|u) // (:|$C)$C_#

j\ -> j
j // $C_#

; Final syllable vowel reductions

o: -> A / _($C)($C)($C)#
u ͏-> o / _$C($C)($C)#
(}|i) -> e / _($C)($C)($C)#

2 -> e
iu -> eo
G -> g / #_


;;;;;;;;;;;;;;;;;;;;;;;;;
;   Sets and features   ;
;;;;;;;;;;;;;;;;;;;;;;;;;

$V ($Long [-Long] $Front [-Front] $Overlong $Nasalized)
$STOP (k k_w p t g b d)
$GLIDE (w j)
$LIQUID (r l)
$NASAL (m n)
$FRICATIVE (x x_w f T G v D s z)
$AFFRICATE (tS dZ)
$SONORANT (LIQUID NASAL V)
$C ($STOP $GLIDE $LIQUID $NASAL $FRICATIVE $AFFRICATE)

[Front] (
	A -> }
	A: -> }:
	}A -> y:
	}A: -> y:
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
