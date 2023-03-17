# Transmute

A sound change applier for constructed languages. Transmute aims for expressiveness and flexibility, rather than speed, although it is reasonably fast on good hardware. Using the 64 rules in `protogermanic.sc` and the `pie.txt` lexicon in the `samples/ipa` folder on an Intel Core i5-6500T, rules compile in ~20 ms on average, and words take a millisecond or less on average to process all the rules.



## Command line options

    Transmute [OPTION...] -r FILE [FILE...]
    Transmute [OPTION...] -l FILE [FILE...]
    Transmute [OPTION...] -r FILE -l FILE

Transmute processes the last rules file specified, and all lexicon files in the order specified.

Files can be specified using switches, or without switches based on their file extensions. Files ending in `.sc` are treated as rules files, and all other files are treated as lexicon files.

A filename of `-` stands for standard input.

| Switch                 | Short form | Description                                                                          |
| -----------------------|------------|--------------------------------------------------------------------------------------|
| --lexicon FILE         | -l         | Load lexicon from FILE.                                                              |
| --rules FILE           | -r         | Load rules from FILE.                                                                |
| --recompile            | -rc        | Recompile rules file instead of loading compiled rules.                              |
| --test-rules N1,N2,... |            | Run only the rules listed.                                                           |
| --test-words N1,N2,... |            | Transform only the words listed.                                                     |
| --verbose N            | -v         | Set verbosity level (0 = silent, 1 = normal, 2 = show transformations, 3 = show DFA) |
| --x-sampa              | -x         | Use X-SAMPA instead of IPA.                                                          |


## Rule files

A rule file consists of a list of sets, features and rules.

### IPA and X-SAMPA

By default, Transmute accepts rules written in IPA. You can write rules in X-SAMPA by using the `--x-sampa` or `-x` switch.

Because X-SAMPA clashes with Transmute identifiers, you need to use a `$` sigil to disambiguate identifiers when used outside of brackets:

    ; IPA rule

    [STOP-Voiced] / [+Fricative] / (#|V|SONORANT)_

    ; X-SAMPA rule

    [STOP-Voiced] / [+Fricative] / (#|$V|$SONORANT)_

### Identifiers

Sets and features are identified by a name consisting of alphanumeric characters beginning with a capital letter, e.g. `C` or `Voiced`.

### Defining sets

Sets define categories of sounds, e.g. consonants and vowels.

    V (a, e, i, o, u)

You can put phonemes of any length in a set.

    LABIOVELAR (kʷ, gʷ)
    OVERLONG (ɑːː, ɔːː)

Commas are optional. Whitespace is enough to separate phonemes, and you may list them in any arrangement desired.

    C (
        p t k
        b d g
        m n ŋ
          s
          z
    )

    LARYNGEAL (ʔ χ χʷ)

### Defining features

Features have a similar syntax to sets. In a feature definition, the identifier is enclosed in brackets to reflect its usage in a phonological rule. A feature consists of a list of transformations from a sound that does not have the feature to a sound that does. Transformations may be defined using either `->` or the Unicode U+2192 `→` character. Like a set, a feature can also contain sounds with no transformation, only membership.

    [Fricative] (
        k → x
        kʷ → xʷ
        p → ɸ
        t → θ
        s
    )

Here, four phonemes are defined as having transformation from voiceless stops to fricatives. /s/ is just a fricative, and has no corresponding transformation.

### Composing sets and features

Both sets and features allow you to include other sets or features in them:

    STOP (p t k)
    FRICATIVE (x f θ)
    NASAL (m n ŋ)
    C (STOP FRICATIVE NASAL) ; p t k x f θ m n ŋ

    V (Long [-Long] Front [-Front] Overlong Nasalized)


### Defining rules

Languages are subject to many changes in their phonology as natural variations in pronunciation become entrenched over long periods of time, and these sound changes are usually regular, i.e. almost universally applied to every applicable word. Such regular sound changes can be described using phonological rules, a convention from the field of linguistics. Defining the sounds of a language in terms of distinctive features allows us to define phonological rules in terms of their presence, absence, removal and addition, rather than explicitly designing a rule multiple times for each phoneme it may apply to. This allows writing expressive and declarative rules that more closely resemble what one may find in an academic paper.

#### Rule types

##### Unconditional rules

A rule consists of at least two parts. An unconditional rule has only an **input** and an **output**, separated by either `->`, `→`, or `/`:

    ; a becomes ɑ
    a/ɑ

    ; o becomes ɔ
    o → ɔ

##### Conditional rules

A conditional rule has a third section, the **environment** in which the rule applies, separated by a `/`. There are also two tokens that may appear only in the environment:

| Token | Purpose                                        |
|-------|----------------------------------------------- |
| `_`   | Matches what is specified in the input section |
| `#`   | Matches the beginning or end of the word       |

For example, the rule

    ; Laryngeal consonant becomes a schwa between consonants

    LARYNGEAL → ə / C_C

will first match any consonant `C`, then a `LARYNGEAL`, and then another consonant, and upon matching the second consonant will replace the laryngeal with a schwa.

##### Insertion rules

An insertion rule is written with the input section either empty or containing only `∅`. Insertion rules are conditional only.

    ; Insert /s/ between dental stops
    ∅ → s / [Stop+Dental]_[Stop+Dental]

##### Deletion rules

A deletion rule is written with the output section either empty or containing only `∅`, and can be either conditional or unconditional:

    ; Delete schwas
    ə → ∅

    ; Delete /j/ before /e a o/ at the end of a word
    j//_(e|a|o)#

#### Matching phonemes in a set

In the simplest case, one phoneme out of a set can be matched using only its identifier:

    ; Match any consonant at the end of a word and delete it

    C → ∅ / _#

If you need to use two identifiers in a row in a rule, you can separate them with spaces, as in `C C`.

#### Matching phonemes satisfying one or more characteristics

A compound set matches all phonemes that share all of the listed features. Whether to match the presence or absence of a feature is indicated by a `+` or a `-`, respectively. A few examples:

| Compound set              | Process                                                                                 | Matches                |
|---------------------------|-----------------------------------------------------------------------------------------|------------------------|
| `[Sonorant-C]`            | Starts with all sonorants (vowels, liquids and nasals) and removes all consonants       | Vowels                 |
| `[Stop-Voiced]`           | Starts with all stops and removes all voiced stops                                      | Voiceless stops        |
| `[Stop+Voiced+Aspirated]` | Starts with all stops, removes all voiceless stops, and removes all non-aspirated stops | Voiced aspirated stops |

More concretely, given the following sets and features

    Stop (
        p t k kʷ
        b d g gʷ
    )
    
    [Voiced] (
        p → b
        t → d
        k → g
        kʷ ͏→ gʷ
        m
        n
        ŋ
    )

    [Fricative] (
        p → ɸ
        t → θ
        k → x
        kʷ → xʷ
        s
    )

By starting with the set `Stop` and removing all phonemes that are `Voiced` (/b d g gʷ/), we can write a rule that affects only the voiceless stops /p t k kʷ/ː

    ; Grimm's law for voiceless consonants
    
    [Stop-Voiced] → [+Fricative]

#### Transforming a sound by changing features

The same notation used to match either the presence or absence of a feature can also be used in the output section of the rule. In the previous example, a voiceless stop was changed to a voiceless fricative using the transformations defined in the feature `[Fricative]`.

More than one feature can be changed. In the following rule, /n/ is deleted after a vowel undergoes nasalization and compensatory lengthening before /x/:

    [-Nasalized]n → [+Nasalized +Long] / _x

    ; brɑnxtɑz -> brãːxtɑz

#### Optional matches

Phonemes contained in parentheses may be matched if present, but may also be skipped over if necessary to make the rule match. For example, in this rule a schwa becomes /ɑ/ when preceded by the word boundary, an optional /s/, and up to two other consonants:

    ə → ɑ / #(s)(C)(C)_

#### Alternation matches

One of several different sequences of sounds can be matched by enclosing them in parentheses and separating them with `|`. For example, in the Germanic spirant law, stops followed by either a `t` or an `s` become fricatives:

    ; Affects p b bʰ
    [Stop+Labial] → ɸ / _(t|s)

    ; Affects t d dʰ
    [Stop+Dental] → ts / _(t|s)
    
    ; Affects k g gʰ
    [Stop+Velar] → x / _(t|s)

This type of match can also be used in the input section:

    (o|a) → ɑ
