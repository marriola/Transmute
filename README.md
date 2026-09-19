# Transmute

A sound change applier for constructed languages. Transmute uses a distinctive feature-based approach that allows you to write concise and expressive sound change rules.

## Example files

Under the `sample/ipa` and `sample/x-sampa` folders, there are IPA and X-SAMPA versions of:

| File name                                                                                | Description                           |
|------------------------------------------------------------------------------------------|---------------------------------------|
| [pie.txt](javascript:selectDemo('1. pie.txt', '1. protogermanic.sc'))                    | A sample Proto-Indo-European lexicon  |
| [protogermanic.txt](javascript:selectDemo('2. protogermanic.txt', '2. westgermanic.sc')) | A sample Proto-Germanic lexicon       |
| [westgermanic.txt](javascript:selectDemo('3. westgermanic.txt', '3. oldenglish.sc'))     | A sample West-Germanic lexicon        |
| [protogermanic.sc](javascript:selectDemo('1. pie.txt', '1. protogermanic.sc'))           | PIE to Proto-Germanic rules           |
| [westgermanic.sc](javascript:selectDemo('2. protogermanic.txt', '2. westgermanic.sc'))   | Proto-Germanic to West Germanic rules |
| [oldenglish.sc](javascript:selectDemo('3. westgermanic.txt', '3. oldenglish.sc'))        | West Germanic to Old English rules    |

Try out the last step in the [browser demo](https://marriola.github.io/transmute-demo). Click Examples and select West Germanic to Old English.

## Performance

Although written in a functional style, Transmute is reasonably fast on good hardware. Using the 67 rules in `protogermanic.sc` in the `samples/ipa` folder on a quad-core Intel Core i5-6500T, rules compile in ~5 ms on average with Native AOT. Using the sample `pie.txt` lexicon, each word takes ~1 millisecond on average to process all the rules with syllable detection.

Because X-SAMPA rules have to create and process about 50% more states for every diacritic and extended character, they don't perform as well as IPA rules. Depending on the target platform, the X-SAMPA version of `protogermanic.sc` compiles 40-50% slower than the IPA version. Once compiled, X-SAMPA rules perform nearly as well as IPA rules.

## To do

* α variables
* Replace the explicit transformation-based approach with a feature matrix-based approach and a built in standard matrix

## Rule files

A rule file consists of a list of sets, features and rules.

### IPA and X-SAMPA

By default, Transmute accepts rules written in IPA. You can write rules in X-SAMPA by selecting X-SAMPA in the toolbar.

### Identifiers

Sets and features are identified by a name consisting of alphanumeric characters beginning with a capital letter, e.g. `C` or `Voiced`.

Because all X-SAMPA symbols are also valid characters in an identifier, when using X-SAMPA you need to use a `$` sigil to disambiguate identifiers when used outside of brackets:

	; IPA rule

	[Stop-Voiced] / [+Fricative] / (#|V|Sonorant)_

	; X-SAMPA rule

	[Stop-Voiced] / [+Fricative] / (#|$V|$Sonorant)_


### Defining sound change rules

Languages are subject to many changes in their phonology as natural variations in pronunciation become entrenched over long periods of time, and these sound changes are usually regular, i.e. almost universally applied to every applicable word. Such regular sound changes can be described using phonological rules, a convention from the field of linguistics. Defining the sounds of a language in terms of distinctive features allows us to define phonological rules in terms of their presence, absence, removal and addition, rather than explicitly designing a rule multiple times for each phoneme it may apply to. This allows writing expressive and declarative rules that more closely resemble what one may find in an academic paper.

#### Rule types

##### Unconditional rules

A rule consists of at least two parts. An unconditional rule has only an **input** and an **output**, separated by either `->`, `/` or the Unicode character U+2192 `→`:

    ; a becomes ɑ
    a/ɑ

    ; o becomes ɔ
    o → ɔ

##### Conditional rules

A conditional rule has a third section, the **environment** in which the rule applies, separated by a `/`. In the environment, the placeholder symbol `_` stands for the input being transformed. For example:

    ; Laryngeal consonant becomes a schwa between consonants

    Laryngeal → ə / C_C

The rule will first match any consonant `C`, then a `LARYNGEAL`, and then another consonant, and upon matching the second consonant will replace the laryngeal with a schwa.

**Note:** `_` is a valid X-SAMPA character, so make sure your placeholder has spaces around it if you're using X-SAMPA mode.

##### Insertion rules

An insertion rule has an empty input section. You can use the empty set symbol U+2205 `∅` for clarity, but this is optional. Insertion rules are conditional only.

    ; Insert /s/ between dental stops
    ∅ → s / [Stop+Dental]_[Stop+Dental]
	
	; Insert /u/ before syllabic sonorants
	/ u / _Syllabic
	

##### Deletion rules

A deletion rule has an empty output section (or `∅`, as with insertion rules). It can be either conditional or unconditional.

    ; Delete all schwas
    ə → ∅

    ; Delete /j/ before /e a o/ at the end of a word
    j//_(e|a|o)#

#### Matching

##### Matching literals

The simplest type of match is literal IPA/X-SAMPA symbols.

    ; Match zm and change it to mm
    
    zm → mm

##### Matching phonemes in a set

In the simplest case, one phoneme out of a set can be matched using only its identifier:

    ; Match any consonant at the end of a word and delete it

    C → ∅ / _#

If you need to use two identifiers in a row in a rule, you can separate them with either ` ` or `.`. This isn't necessary in X-SAMPA mode since identifiers are already separated by `$`.

    ; These are equivalent

    t → ∅ / V.C(C)(C)V(C)(C)_#
    t → ∅ / V C(C)(C)V(C)(C)_#

    ; X-SAMPA version

    t // $V$C($C)($C)$V($C)($C)_#

##### Matching boundaries

In the environment section you can also match on word or syllable boundaries to limit where your rule applies.

| Symbol        | Boundary type                               |
| ------------- | ------------------------------------------- |
| \#            | The beginning or end of the word            |
| σ or $        | The beginning or end of the syllable        |
| SyllableStart | Start of the syllable                       |
| SyllableEnd   | End of the syllable                         |
| Onset         | Consonants at the beginning of the syllable |
| OnsetStart    | Start of the onset                          |
| OnsetEnd      | End of the onset                            |
| Nucleus       | The core of the syllable                    |
| NucleusStart  | Start of the nucleus                        |
| NucleusEnd    | End of the nucleus                          |
| Coda          | The consonants at the end of the syllable   |
| CodaStart     | Start of the coda                           |
| CodaEnd       | End of the coda                             |

The syllable boundary symbols are special because they are treated differently by the rule engine. While other symbols either match or fail, a syllable boundary symbol will consume any symbols until it finds a syllable boundary. If the syllable boundary type matches, you jump to that part of the word and resume matching from there. If the intended syllable segment isn't there, the match will fail.

The plain symbols (`σ`/`$`, `Onset`, `Nucleus`, `Coda`) either skip to the end of that segment if you're already inside it, or jump to the beginning of the next one if you're outside of it. There are also more explicit boundary types if you need more precision. For example, by matching on `CodaEnd`, you can check for the existence of a coda without needing to match any of it explicitly by jumping past it.

**Note:** To match on syllable boundaries, a syllable definition rule must be given first. See [Defining syllable rules](#defining-syllable-rules).

###### Example: Word boundaries

    ; Drop /z/ at the end of a word

    z → ∅ / _#

    ; Change gʷ to b, but only at the beginning of a word
    
    gʷ → b / #_

##### Optional matches

Phonemes contained in parentheses may be matched if present, but may also be skipped over if necessary to make the rule match. For example, in this rule a schwa becomes /ɑ/ when preceded by the word boundary, an optional /s/, and up to two other consonants:

    ə → ɑ / #(s)(C)(C)_

##### Alternation matches

One of several different sequences of sounds can be matched by enclosing them in parentheses and separating them with `|`. For example, laryngeals in Proto-Indo-European can cause nearby vowels to change, even when a /w/ or /j/ comes between them:

    e → a / _(w|j)ʕ

This type of match can also be used in the input section:

    (o|a) → ɑ

###### Example: Final short vowel syncope

This rule drops word final /e a o/ in a multisyllabic word. Note that we have two `σ` symbols here: the second matches the beginning of the syllable containing our vowel, and the first matches the end of the preceding syllable.

    (e|a|o) → ∅ / σσ_#

##### Matching phonemes satisfying one or more characteristics

A compound set matches all phonemes that share all of the listed features. Whether to match the presence or absence of a feature is indicated by a `+` or a `-`, respectively. A few examples:

| Compound set              | Process                                                                                 | Matches                |
|---------------------------|-----------------------------------------------------------------------------------------|------------------------|
| `[Sonorant-C]`            | Starts with all sonorants (vowels, liquids and nasals) and removes all consonants       | Vowels                 |
| `[Stop-Voiced]`           | Starts with all stops and removes all voiced stops                                      | Voiceless stops        |
| `[Stop+Voiced+Aspirated]` | Starts with all stops, removes all voiceless stops, and removes all non-aspirated stops | Voiced aspirated stops |

More concretely, given the following sets and features

    Stop = (
        p t k kʷ
        b d g gʷ
    )
    
    [Voiced] = (
        p → b
        t → d
        k → g
        kʷ ͏→ gʷ
        m
        n
        ŋ
    )

    [Fricative] = (
        p → ɸ
        t → θ
        k → x
        kʷ → xʷ
        s
    )

By starting with the set `Stop` and removing all phonemes that are `Voiced` (/b d g gʷ/), we can write a rule that affects only the voiceless stops /p t k kʷ/ː

    ; Grimm's law for voiceless consonants
    
    [Stop-Voiced] → [+Fricative]

You can also construct a set, and then remove specific segments from it:

    ; /u/ becomes /o/ before any consonant but /n/

    u → o / _[C-/n/]

You can also construct a set out of only segments:

    ; Delete final /ɑ ɑ̃/

    [+/ɑ ɑ̃/] → ∅ / _#

##### Transforming a sound by changing features

The same notation used to match the presence or absence of features can also be used in the output section of the rule. In the previous example, a voiceless stop was changed to a voiceless fricative using the transformations defined in the feature `[Fricative]`.

More than one feature can be changed. In the following rule, /n/ is deleted after a vowel undergoes nasalization and compensatory lengthening, all before /x/:

    [-Nasalized]n → [+Nasalized +Long] / _x

    ; brɑnxtɑz -> brɑ̃ːxtɑz

###### Example: Umlaut

Umlaut in the Germanic languages is a type of assimilation where the front quality of the vowel /i/ or glide /j/ in a following suffix spreads to the word stem. Here we match on a non-front vowel, then we skip the rest of the syllable by matching on a syllable boundary, and finally the /i j/.

    [-Front] → [+Front] / _σ(i|j)

###### Example: Verner's law

Verner's law is a Proto-Germanic sound change in which voiceless fricatives become voiced when they follow an unstressed syllable. By using a combination of syllable boundaries, we can ensure that this change only takes place in that environment while matching only what we need, keeping the compiled rule small and fast.

    [+Fricative-Voiced] → [+Voiced] / NucleusStart [-Stressed] CodaStart ![-Voiced] (Sonorant) _ ![-Voiced]

There are three parts to this rule. First, with `NucleusStart [-Stressed]`, we use NucleusStart to look for a vowel that isn't preceded by a stress marker. Since the rule engine will retry a rule from the next character whenever it fails, it's possible that we could accidentally match a stressed vowel if we allow the rule to fail on the stress marker and then match the rest of the vowel.

Then with `CodaStart ![-Voiced]`, we use CodaStart so we can verify that our target fricative doesn't follow a voiceless consonant, which should block the rule.

With `(Sonorant) _ ![-Voiced]` we finish the stem with an optional sonorant, and then we match our target voiceless fricative. To finish the whole thing off, we then check to make sure there isn't a voiceless consonant after it, which should block the rule.

### Defining sets and features

#### Defining sets

Sets define categories of sounds, such as consonants and vowels.

    V = (a, e, i, o, u)

You can put phonemes of any length in a set, including diacritics.

    Labiovelar = (kʷ, gʷ)
    Overlong = (ɑːː, ɔːː)
	NonSyllabic = (j, w, e̯, o̯)

Commas are optional. Whitespace is enough to separate phonemes, and you may list them in any arrangement desired.

    C = (
        p t k
        b d g
        m n ŋ
          s
          z
    )

    Laryngeal = (ʔ χ χʷ)

#### Defining features

Features have a similar syntax to sets. In a feature definition, the identifier is enclosed in brackets to reflect its usage in a phonological rule. A feature consists of a list of transformations from a sound that does not have the feature to a sound that does. Transformations may be defined using either `->` or `→` character. Like a set, a feature can also contain sounds with no transformation, only membership.

    [Fricative] = (
        k → x
        kʷ → xʷ
        p → ɸ
        t → θ
        s
    )

Here, four phonemes are defined as having transformation from voiceless stops to fricatives. /s/ is just a fricative, and has no corresponding transformation.

#### Composing sets and features

Both sets and features allow you to include other sets or features in them:

    Stop = (p t k)
    Fricative = (x f θ)
    Nasal = (m n ŋ)
    C = (Stop Fricative Nasal) ; p t k x f θ m n ŋ

    V = (Long [-Long] Front [-Front] Overlong Nasalized)


### Defining syllable rules

Use syllable rules to define your syllable structure so that you can match on syllable boundaries in your sound change rules. If your syllable structure changes, you can define a new syllable rule further down your rules file, and it will apply to all following rules until redefined again.

    ; Syllable structure is (C)V(C)
    
    Syllable = (
        Onset = (C)
        Nucleus = V
        Coda = (C)
    )
	
You can define more than one syllable structure in a single syllable rule. The first applicable one will be used on each word.

    ; Syllables can only end with a consonant when the nucleus is a short vowel

    Syllable = (
        Onset = (C)
        Nucleus = [V -Long]
        Coda = C
    ) or (
        Onset = (C)
        Nucleus = V
    )
