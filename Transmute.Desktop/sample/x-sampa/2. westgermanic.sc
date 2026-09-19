$Syllable = (
    $Onset = (s)($C)($C)
    $Nucleus = (")$V
    $Coda = ($C)($C)($C)
)

[V-Long] // $SyllableEnd $SyllableEnd _#

; Northwest Gmc.

[-Front] -> [+Front] / _$(i|j)
u -> o / _[C-/n/]$$[V-High]
[-Short] -> [+Short] / _#
[+Overlong] -> [-Overlong]

; West Gmc.

[+Rounded] -> [-Rounded] / ($C|$V)_
(zw|Dw) -> ww
D -> d
B -> v
p\ -> f
lT -> ld / $$_$$
z // _#
z -> r / $Sonorant_
s // $C_#
(A|A~) // _#		; $V$C($C)($C)_#
[-Geminate] -> [+Geminate] / _j

$High = (i i: y y: u u:)

[Front] = (
	A -> {
	A: -> {:
	O -> 2
	O: -> 2:
	u -> y
	u: -> y:
)

[Short] = (
	e: -> A
	i: -> i
	O:i -> u
	O:j -> u
	O: -> u
)

[Long] = (
	A -> A:
	e -> e:
	i -> i:
	o -> o:
	O -> O:
	2 -> 2:
	u -> u:
	y -> y:
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
	e -> e~
	i -> i~
	O -> O~
	O: -> O~:
	O:: -> O~::
	u -> u~
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

$V = ($Long [-Long] $Overlong $Nasalized)
$Stop = (k k_w p t g b d)
$Liquid = (r l)
$Nasal = (m n)
$Glide = (w j)
$Fricative = (x x_w p\ f T G B v D s z)
$Sonorant = ($Liquid $Nasal $Glide $V)
$C = ($Stop $Liquid $Nasal $Fricative $Glide)
