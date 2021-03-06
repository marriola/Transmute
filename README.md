﻿# Transmute

A sound change applier for constructed languages. This application aims for flexibility, rather than speed, although it is reasonably fast on good hardware. Using the 66-rule protogermanic.sc file, most words take about 1-2 ms on an AMD Ryzen 7 4800H, or 100-200 ms on an Intel Core i5-6500T.

## Command line options

| Switch                 |                                                                                                                             |
| -----------------------|-----------------------------------------------------------------------------------------------------------------------------|
| --rules FILE           | Load rules from FILE (required)                                                                                             |
| --lexicon FILE         | Load lexicon from FILE (required)                                                                                           |
| --test-rules N1,N2,... | Run the specified list of rules, rather than all rules.                                                                     |
| --test-words N1,N2,... | Transform the specified list of words, rather than all words.                                                               |
| --verbose/-v N         | Set verbosity level (0 = silent, 1 = normal, 2 = show transformations, 3 = show DFA, 4 = show NFA and transformation state) |

## Rule files

A rule file consists of a list of sets, features and rules.

### Identifiers

Sets and features are identified by a name consisting of alphanumeric characters prefixed by a dollar sign, e.g. `$C` or `$voiced`.

### Defining sets

Sets define categories of sounds, e.g. consonants and vowels. Phonemes in a set are separated by whitespace and may be listed in any arrangement desired.

    $V { a e i o u }

    $C {
        p t k
        b d g
        m n ŋ
          s
          z
    }

### Defining features

Features have a similar syntax to sets. In a feature definition, the identifier is enclosed in brackets to reflect its usage in a phonological rule. A feature consists of a list of transformations from a sound that does not have the feature to a sound that does. As in a set, a feature can also contain sounds with no transformation, only membership. Transformations may be defined either using `->` or the Unicode U+2192 `→` character.

    [$fricative] {
        k → x
        kʷ → xʷ
        p → ɸ
        t → θ
        s
    }

Here, four phonemes are defined as having transformation from voiceless stops to fricatives. /s/ is just a fricative, and has no corresponding transformation.

### Defining rules

Languages are subject to many changes in their phonology as natural variations in pronunciation become entrenched over long periods of time, and these sound changes are usually regular, i.e. almost universally applied to every applicable word. Such regular sound changes can be described using phonological rules, a somewhat informal standard from the field of linguistics. Defining the sounds of a language in terms of distinctive features allows us to define phonological rules in terms of their presence, absence, removal and addition, rather than explicitly designing a rule multiple times for each phoneme it may apply to. This allows writing expressive and declarative rules that more closely resemble what one may find in an academic paper.

#### Rule types

##### Unconditional rules

A rule consists of at least two parts. An unconditional rule has only an **input** and an **output**, separated by either `->`, `→`, or `/`:

    a/ɑ                             ; a becomes ɑ
    o→ɔ                             ; o becomes ɔ

##### Conditional rules

A conditional rule has a third segment, the **environment** in which the rule applies, separated by a `/`. There are also two tokens that may appear only in the environment:

| Token | Purpose                                                    |
|-------|------------------------------------------------------------|
| `_`   | Matches what is specified in the input segment             |
| `#`   | Matches a word boundary, similar to `^` or `$` in a regex. |

For example, the rule

    $laryngeal→ə/$C_$C              ; Laryngeal consonant becomes a schwa between consonants

will first match any consonant `$C`, then a `$laryngeal`, and then another consonant, and upon matching the second consonant will replace the laryngeal with a schwa.

##### Deletion rules

A deletion rule is written with the output segment either empty or containing only `∅`, and can be either conditional or unconditional:

    ə→∅
    j//_(e|a|o)#

##### Insertion rules

Insertion rules are not yet supported, but when they are, will be written similarly with the input segment either empty or containing only `∅`. Insertion rules will be conditional only.

    ∅→s/[$stop+$dental]_[$stop+$dental]

#### Matching phonemes in a set

In the simplest case, one phoneme out of a set can be matched using only its identifier:

    $C→∅/_#                         ; Match any consonant and delete it

#### Intersection of sets and features

A compound set matches all phonemes that share all of the listed features. Whether to match the presence or absence of a feature is indicated by a `+` or a `-`, respectively. A few examples:

| Compound set                 | Process                                                                                 | Matches                |
|------------------------------|-----------------------------------------------------------------------------------------|------------------------|
| `[$sonorant-$C]`             | Starts with all sonorants (vowels, liquids and nasals) and removes all consonants       | Vowels                 |
| `[$stop-$voiced]`            | Starts with all stops and removes all voiced stops                                      | Voiceless stops        |
| `[$stop+$voiced+$aspirated]` | Starts with all stops, removes all voiceless stops, and removes all non-aspirated stops | Voiced aspirated stops |

More concretely, given the following sets and features

    $stop {
        p t k kʷ
        b d g gʷ
    }
    
    [$voiced] {
        p → b
        t → d
        k → g
        kʷ ͏→ gʷ
        m
        n
        ŋ
    }

    [$fricative] {
        p → ɸ
        t → θ
        k → x
        kʷ → xʷ
        s
    }

By starting with the set `$stop` and removing all phonemes that are `$voiced` (/b d g gʷ/), we can write a rule that affects only the voiceless stops /p t k kʷ/ː

    [$stop-$voiced]→[+$fricative]    ; Grimm's law for voiceless consonants

#### Transforming a sound by changing a feature

The same notation used to match either the presence or absence of a feature can also be used in the output segment of the rule. Currently only one feature may be changed. In the previous example, a voiceless stop was changed to a voiceless fricative using the transformations defined in the feature `[$fricative]`.

#### Optional matches

Phonemes contained in parentheses may be matched if present, but may also be skipped over if necessary to make the rule match. For example, in this rule a schwa becomes /ɑ/ when preceded by the word boundary, an optional /s/, and up to two other consonants:

    ə→ɑ/#(s)($C)($C)_

#### Disjunction matches

One of several different sequences of sounds can be matched by enclosing them in parentheses and separating them with `|`. For example, in the Germanic spirant law, stops followed by either a `t` or an `s` become fricatives:

    [$stop+$labial]→ɸ/_(t|s)        ; Affects p b bʰ
    [$stop+$dental]→ts/_(t|s)       ; Affects t d dʰ
    [$stop+$velar]→x/_(t|s)         ; Affects k g gʰ
