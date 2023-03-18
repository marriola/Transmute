; Northwest Gmc.

[-Front] -> [+Front] / _$C($C)($C)(i|j)
u -> o / _[C-/n/]($C)($C)[V-HIGH]
[-Short] -> [+Short] / _#
[+Overlong] -> [-Overlong]

; West Gmc.

[+Rounded] -> [-Rounded] / ($C|$V)_
(zw|Dw) -> ww
D -> d
B -> v
p\ -> f
z // _#
z -> r / $SONORANT_
s // $C_#
[+/A A~/] // _#		; $V$C($C)($C)_#
[-Geminate] -> [+Geminate] / _j

$HIGH (i i: y y: u u:)

[Front] (
	A -> }
	A: -> }:
	O -> 2
	O: -> 2:
	u -> y
	u: -> y:
)

[Short] (
	e: -> A
	i: -> i
	O:i -> u
	O:j -> u
	O: -> u
)

[Long] (
	A -> A:
	e -> e:
	i -> i:
	o -> o:
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
	e -> e~
	i -> i~
	O -> O~
	O: -> O~:
	O:: -> O~::
	u -> u~
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

$V ($Long [-Long] $Overlong $Nasalized)
$STOP (k k_w p t g b d)
$LIQUID (r l)
$NASAL (m n)
$GLIDE (w j)
$FRICATIVE (x x_w p\ f T G B v D s z)
$SONORANT ($LIQUID $NASAL $GLIDE $V)
$C ($STOP $LIQUID $NASAL $FRICATIVE $GLIDE)
