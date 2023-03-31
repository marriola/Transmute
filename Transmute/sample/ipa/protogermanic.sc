Syllable = (
    Onset = ((s|Laryngeal)) (C) (C)
    Nucleus = (ˈ) ( V | Sonorant | Laryngeal)
    Coda = C ([C-Approximant])
)

; Based on https://en.wikipedia.org/wiki/Proto-Germanic_language#Phonological_stages_from_Proto-Indo-European_to_end_of_Proto-Germanic.
; For the purposes of this example we take the values of h₁, h₂ and h₃ to be ʔ, χ and χʷ, respectively, on the basis that I think they're neat.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Rules                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO implement NOT nodes
;   Sonorant/u Sonorant/V_!V
;   Sonorant/u Sonorant/!V_V

;;;;;;;;;;;;
; Pre-PGmc ;
;;;;;;;;;;;;

; You can omit the environment section for unconditional rules.

[+Palatalized] → [-Palatalized]

; Insertion rules transform ∅ into something. You can omit the ∅ symbol if you want.

∅ → u / (#|[C-Laryngeal])(ˈ)_(m|n|l|r)(#|C)

[-Overlong] → [+Overlong] / _#

Laryngeal → ∅ / #_C

; e-coloring and dropping of laryngeals in onset

e → o / χʷ(ˈ)_
e → a / χ(ˈ)_
Laryngeal → ∅ / _(ˈ)V

; TODO: implement syllable detection so we can match on syllable boundaries, then we could write the preceding rule like this:

; Laryngeal → ∅ / _$

; Homorganic vowels in hiatus → long vowel

aa → aːː
ee → eːː
ii → iːː
oo → oːː
uu → uːː

; e-coloring

e → o / _(w|j)χʷ
e → a / _(w|j)χ
eχʷ → oː
eχ → ɑː

; Compensatory lengthening with loss of laryngeals after sonorants

[-Long]Laryngeal → [+Long] / _
Laryngeal → ∅ / (V|Sonorant)_

; Cowgill's law

χʷ → g / (Sonorant)_w

; Vocalization of remaining laryngeals

Laryngeal → ə


;;;;;;;;;;;;;;
; Early PGmc ;
;;;;;;;;;;;;;;

;[+Glide] → [-Glide] / V_C

; Sievers' law

∅ → i / [V-Long][C-/j/][C-/j/] ([C-/j/])_j
∅ → i / [V+Long][C-/j/] ([C-/j/])([C-/j/])_j

(j|w) → ∅ / _(e|a|o)#
(e|a|o) → ∅ / C_#

; Grimm's law: voiceless stops become fricatives, except after an obstruent

[Stop-Voiced] → [+Fricative] / (#|V|Sonorant)_
;[Stop-Voiced] → [+Fricative] / !(Obstruent)_

; Undo Grimm's law after s-

[+Fricative-Voiced] → [-Fricative] / s_

; Germanic spirant law

[Stop+Labial] → ɸ / _(t|s) ;(C|V)
[Stop+Dental] → ts / _(t|s) ;(C|V)
tst → ss
;tss → ss
ss → s / _#
[Stop+Velar] → x / _(t|s)(C|V)

; Grimm's law: voiced unaspirated stops become voiceless stops
; TODO: Do not match a sound if it is actually a prefix for a longer sound that should not be matched
; Fix: identify features that have the sound as a prefix and transition on error
; e.g. don't match b for [Stop+Voiced-Aspirated] when it is followed by ʰ

[Stop +Voiced -Aspirated] → [-Voiced] / _(#|(ˈ)V|C)

; Grimm's law: aspirated stops become unaspirated

[Stop+Aspirated] → [-Aspirated]

; Lenition of /g/ and intervocalic voiced stops

g → ɣ / (#|Liquid|[+Fricative])_

[Stop+Voiced] → [+Fricative] / (V|[+Glide])_(#|V|[+Glide])

; Verner's law

[+Fricative-Voiced] → [+Voiced] / (C|#)[-Stressed-Sonorant](Sonorant)_(#|V|[+Overlong]|[+Stressed]|[+Voiced])

; Undo Verner's law after voiceless consonants and before voiceless stops

[+Fricative+Voiced] → [-Voiced] / [-Voiced]_

; Undo Verner's law before voiceless stops

[+Fricative+Voiced] → [-Voiced] / _[Stop-Voiced]

; Voiced fricatives resulting from Verner's law become stops after a nasal

[+Fricative+Voiced] → [-Fricative] / Nasal_

; Stress moves to initial syllable. Let's just stop marking it, and from now on we'll treat the first syllable as the stressed one.

[+Stressed] → [-Stressed]

; Word-final /s/ previously unaffected by Verner's law becomes voiced by analogy with those that were

s → z / V((Nasal|Liquid))_#

gʷ → b / #_

nw → nn
ln → ll
zm → mm

; TODO: make this be smart and match short vowels without having to manually specify what could come after

e → i / V([+Glide])(C)(C)_(#|C)
e → i / V([+Glide])(C)(C)_(#|C)
(ei|ej) → iː
iji → iː
ij → iː / _(C|#)
iːi → iː

; TODO: on error, allow the machine to jump to another branch (e.g. from C to _) if that one is capable of matching the input

ji → i / V(C)(C)_(#|C)

(o|a) → ɑ

;;;;;;;;;;;;;
; Late PGmc ;
;;;;;;;;;;;;;

m → n / _(#|Dental)
[-Nasalized]n → [+Nasalized] / _#

ẽː → ɑ̃ː

; Stressed schwa becomes /a/

ə → ɑ / #(s)(C)(C)_

; Unstressed schwa disappears between consonants

ə → ∅ / C_C
ə → ɑ

;t → ∅ / V.C(C)(C)V(C)(C)_#
t → ∅ / V(C)(C)V(C)_#
ɣʷ → w

ɑː → ɔː
ɑ̃ː → ɔ̃ː

e → i / _nC

; Combined double transformation and deletion
; 1. Nasalization and compensatory lengthening of a vowel
; 2. Deletion of /n/

[-Nasalized]n → [+Nasalized +Long] / _x

; Not really sure where this should go, so I'll just stick it at the end

sr → str


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Sets and features                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


V (
    ə
    ɑ  a  e  i  o     u
    ɑː aː eː iː oː ɔː uː
    ɑ̃  ẽ  ĩ  õ     ũ
    ɑ̃ː ẽː ĩː õː ɔ̃ː ũː
    ɑu eu
)

[Mid] (e, o)
[Round] (o, u)
[High] (i, u)

[Long] (
    ɑ → ɑː
    a → aː
    e → eː
    i → iː
    o → oː
    u → uː
    ɔː 
    ɑ̃ → ɑ̃ː
    ẽ → ẽː
    ĩ → ĩː
    õ → õː
    ũ → ũː
    ɔ̃ː
)

[Overlong] (
    aː → aːː
    ɑː → ɑːː
    oː → oːː
    ɔː → ɔːː
)

[Nasalized] (
    ɑ → ɑ̃
    e → ẽ
    i → ĩ
    o → õ
    u → ũ
    ɑː → ɑ̃ː
    eː → ẽː
    iː → ĩː
    oː → õː
    uː → ũː
    ɑːː → ɑ̃ːː
    oːː → õːː
)

[Stressed] (
    a → ˈa
    aː → ˈaː
    ɑ → ˈɑ
    ɑː → ˈɑː
    ɑu → ˈɑu
    e → ˈe
    eː → ˈeː
    eu → ˈeu
    i → ˈi
    iː → ˈiː
    o → ˈo
    oː → ˈoː
    ɔ → ˈɔ
    ɔː → ˈɔː
    u → ˈu
    uː → ˈuː
    ə → ˈə
    m → ˈm
    n → ˈn
    l → ˈl
    r → ˈr
)

; TODO map sets
; V → &ː
;   produces aː eː iː oː uː

Stop (
    k  kʲ  kʷ  p  t
    g  gʲ  gʷ  b  d
    gʰ gʲʰ gʷʰ bʰ dʰ
)

Dental (t, d, dʰ, θ, ð, s, z)
Labial (m, p, b, bʰ, ɸ, β)
Labiovelar (kʷ, gʷ, gʷʰ, xʷ, ɣʷ)
Velar (k, g, gʰ, gʲʰ, x, ɣ, Labiovelar)
Liquid (l, r)
Nasal (m, n)
[Glide] (u → w, i → j)
Approximant (w, j)
Sonorant (Nasal, Liquid, Approximant)
Laryngeal (ʔ, χ, χʷ)
Sibilant (s z)

Obstruent (Stop, Fricative)
C (Dental, Labial, Velar, Sonorant, Liquid, Glide, Nasal, Laryngeal, Sibilant, Fricative, [-Fricative] Palatalized)

[Voiced] (
    k → g
    gʰ
    x → ɣ
    xʷ → ɣʷ
    kʲ → gʲ
    gʲʰ
    kʷ → gʷ
    gʷʰ
    p → b
    bʰ
    ɸ → β
    t → d
    s → z
    dʰ
    θ → ð
    Sonorant
)

[Palatalized] (
    k → kʲ
    g → gʲ
    gʰ → gʲʰ
)

[Labialized] (
    k → kʷ
    g → gʷ
    gʰ → gʷʰ
)

[Aspirated] (
    g → gʰ
    gʲ → gʲʰ
    gʷ → gʷʰ
    b → bʰ
    d → dʰ
)

[Fricative] (
    k → x
    kʷ → xʷ
    p → ɸ
    t → θ
    g → ɣ
    gʷ → ɣʷ
    b → β
    d → ð
    s
    z
)
