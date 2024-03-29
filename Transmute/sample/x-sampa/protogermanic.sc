﻿; Based on https://en.wikipedia.org/wiki/Proto-Germanic_language#Phonological_stages_from_Proto-Indo-European_to_end_of_Proto-Germanic.
; For the purposes of this example we take the values of h₁, h₂ and h₃ to be ?, ?\ and ?\_w, respectively, on the basis that I think they're neat.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Rules                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO implement NOT nodes
;   Sonorant/uSONORANT/V_!V
;   Sonorant/uSONORANT/!V_V

;;;;;;;;;;;;
; Pre-PGmc ;
;;;;;;;;;;;;

; You can omit the environment section for unconditional rules.

[+Palatalized] -> [-Palatalized]

; Insertion rules transform ∅ into something. You can omit the ∅ symbol if you want.

/ u / (#|[C-Laryngeal])(")_(m|n|l|r)(#|$C)

[-Overlong] -> [+Overlong] / _#

$Laryngeal // #_$C

; e-coloring and dropping of laryngeals in onset

e -> o / ?\_w(")_
e -> a / ?\(")_
$Laryngeal // _(")$V

; TODO: implement syllable detection so we can match on syllable boundaries, then we could write the preceding rule like this:

; Laryngeal → ∅ / _$

; Homorganic vowels in hiatus -> long vowel

aa -> a::
ee -> e::
ii -> i::
oo -> o::
uu -> u::

; e-coloring

e -> o / _(w|j)?\_w
e -> a / _(w|j)?\
e?\_w -> o:
e?\ -> A:

; Compensatory lengthening with loss of laryngeals after sonorants

[-Long]$Laryngeal → [+Long] / _
$Laryngeal → ∅ / ($V|$Sonorant)_

; Cowgill's law

?\_w -> g / ($Sonorant)_w

; Vocalization of remaining laryngeals

$Laryngeal -> @


;;;;;;;;;;;;;;
; Early PGmc ;
;;;;;;;;;;;;;;

;[+Glide] -> [-Glide] / $V_$C

; Sievers' law

/ i / [V-Long][C-/j/][C-/j/] ([C-/j/])_j
/ i / [V+Long][C-/j/] ([C-/j/])([C-/j/])_j

(j|w) // _(e|a|o)#
(e|a|o) // $C_#

; Grimm's law: voiceless stops become fricatives, except after an obstruent

[Stop-Voiced] -> [+Fricative] / (#|$V|$Sonorant)_
;[Stop-Voiced] -> [+Fricative] / !($Obstruent)_

; Undo Grimm's law after s-

[+Fricative-Voiced] -> [-Fricative] / s _

; Germanic spirant law

[Stop+Labial] -> p\ / _(t|s) ;($C|$V)
[Stop+Dental] -> ts / _(t|s) ;($C|$V)
tst -> ss
;tss -> ss
ss -> s / _#
[Stop+Velar] -> x / _(t|s)($C|$V)

; Grimm's law: voiced unaspirated stops become voiceless stops
; TODO: Do not match a sound if it is actually a prefix for a longer sound that should not be matched
; Fix: identify features that have the sound as a prefix and transition on error
; e.g. don't match b for [Stop+Voiced-Aspirated] when it is followed by _h

[Stop +Voiced -Aspirated] -> [-Voiced] / _(#|(")$V|$C)

; Grimm's law: aspirated stops become unaspirated

[Stop+Aspirated] -> [-Aspirated]

; Lenition of /g/ and intervocalic voiced stops

g -> G / (#|$Liquid|[+Fricative])_

[Stop+Voiced] -> [+Fricative] / ($V|[+Glide])_(#|$V|[+Glide])

; Verner's law

[+Fricative-Voiced] -> [+Voiced] / ($C|#)[-Stressed-Sonorant]($Sonorant)_(#|$V|[+Overlong]|[+Stressed]|[+Voiced])

; Undo Verner's law after voiceless consonant

[+Fricative+Voiced] -> [-Voiced] / [-Voiced]_

; Undo Verner's law before voiceless stops

[+Fricative+Voiced] -> [-Voiced] / _[Stop-Voiced]

; Voiced fricatives resulting from Verner's law to stops after nasal

[+Fricative+Voiced] -> [-Fricative] / $Nasal_

; Stress moves to initial syllable. Let's just stop marking it, and from now on we'll treat the first syllable as the stressed one.

[+Stressed] -> [-Stressed]

; Word-final /s/ previously unaffected by Verner's law becomes voiced by analogy with those that were

s -> z / $V(($Nasal|$Liquid))_#

g_w -> b / #_

nw -> nn
ln -> ll
zm -> mm

; TODO: make this be smart and match short vowels without having to manually specify what could come after

e / i / $V([+Glide])($C)($C)_(#|$C)
e / i / $V([+Glide])($C)($C)_(#|$C)
(ei|ej) -> i:
iji -> i:
ij -> i: / _(C|#)
i:i -> i:

; TODO: on error, allow the machine to jump to another branch (e.g. from C to _) if that one is capable of matching the input

ji -> i / $V($C)($C)_(#|$C)

(o|a) -> A

;;;;;;;;;;;;;
; Late PGmc ;
;;;;;;;;;;;;;

m -> n / _(#|$Dental)
[-Nasalized]n -> [+Nasalized] / _#

e~: -> A~:

; Stressed schwa becomes /a/

@ -> A / #(s)($C)($C)_

; Unstressed schwa disappears between consonants

@ // $C_$C
@ -> A

;t // $V$C($C)($C)$V($C)($C)_#
t // $V($C)($C)$V($C)_#
G_w -> w

A: -> O:
A~: -> O~:

e -> i / _n$C

; Combined double transformation and deletion
; 1. Nasalization and compensatory lengthening of a vowel
; 2. Deletion of /n/

[-Nasalized]n -> [+Nasalized+Long] / _x

; Not really sure where this should go, so I'll just stick it at the end

sr -> str


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Sets and features                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


$V (
    @
    A   a   e   i   o       u
    A:  a:  e:  i:  o:  O:  u:
    A~  e~  i~  o~      u~
    A~: e~: i~: o~: O~: u~:
    Au eu
)

[Mid] (e, o)
[Round] (o, u)
[High] (i, u)

[Long] (
    A -> A:
    a -> a:
    e -> e:
    i -> i:
    o -> o:
    u -> u:
    O: 
    A~ -> A~:
    e~ -> e~:
    i~ -> i~:
    o~ -> o~:
    u~ -> u~:
    O~:
)

[Overlong] (
    a: -> a::
    o: -> o::    
)

[Nasalized] (
    A -> A~
    e -> e~
    i -> i~
    o -> o~
    u -> u~
    A: -> A~:
    e: -> e~:
    i: -> i~:
    o: -> o~:
    u: -> u~:
    A:: -> A~::
    o:: -> o~::
)

[Stressed] (
    a -> "a
    a: -> "a:
    A -> "A
    A: -> "A:
    Au -> "Au
    e -> "e
    e: -> "e:
    eu -> "eu
    i -> "i
    i: -> "i:
    o -> "o
    o: -> "o:
    O -> "O
    O: -> "O:
    u -> "u
    u: -> "u:
    @ -> "@
    m -> "m
    n -> "n
    l -> "l
    r -> "r
)

; TODO map sets
; V -> &:
;   produces a: e: i: o: u:

$Stop (
    k  k'  k_w  p  t
    g  g'  g_w  b  d
    g_h g'_h g_w_h b_h d_h
)

$Dental (t, d, d_h, T, D, s, z)
$Labial (m, p, b, b_h, p\, B)
$Labiovelar (k_w, g_w, g_w_h, x_w, G_w)
$Velar (k, g, g_h, g'_h, x, G, $Labiovelar)
$Sonorant (m, n, l, r, w, j)
$Liquid (l, r)
[Glide] (u -> w, i -> j)
$Nasal (m, n)
$Laryngeal (?, ?\, ?\_w, ?\_)
$Sibilant (s z)

$Obstruent ($Stop, Fricative)
$C ($Dental, $Labial, $Velar, $Sonorant, $Liquid, [+Glide] $Nasal, $Laryngeal, $Sibilant)

[Voiced] (
    k -> g
    g_h
    x -> G
    x_w -> G_w
    k' -> g'
    g'_h
    k_w -> g_w
    g_w_h
    p -> b
    b_h
    p\ -> B
    t -> d
    s -> z
    d_h
    T -> D
    $Sonorant
)

[Palatalized] (
    k -> k'
    g -> g'
    g_h -> g'_h
)

[Labialized] (
    k -> k_w
    g -> g_w
    g_h -> g_w_h
)

[Aspirated] (
    g -> g_h
    g' -> g'_h
    g_w -> g_w_h
    b -> b_h
    d -> d_h
)

[Fricative] (
    k -> x
    k_w -> x_w
    p -> p\
    t -> T
    g -> G
    g_w -> G_w
    b -> B
    d -> D
    s
    z
)

