a/æa/_(h|(l|r)$C)

$V//$C_#

; /i/ is elided when preceded by a syllable and followed by a syllable with /i/ as the nucleus
i//$V$C_($C)$C.i

; Rounded consonant becomes unrounded preceding an unrounded vowel
[$C+$round]/[-$round]/[$V-$round]#

; Stress moves to initial syllable
[$V+$stressed]/[-$stressed]/_
[$V-$stressed]/[+$stressed]/#($C)($C)_

; /r/ becomes voiceless when preceding a voiceless consonant in coda position
r/[-$voiced]/[$C-$voiced]_#

; /r/ becomes voiceless when following a voiceless consonant in onset position
r/[-$voiced]/#[-$voiced]_

ea/ɛː/_
ai/eː/_
au/oː/_

$V {
    ɑ a e i o ø u y             ; Stressed
    ˈɑ ˈa ˈe ˈi ˈo ˈø ˈu ˈy     ; Unstressed
}

$C { k kʰ kʰʷ kʷ p t g gʷ b d x f θ s z m n w j ʔ r }

[$stressed] {
  ɑ => ˈɑ
  a => ˈa
  e => ˈe
  i => ˈi
  o => ˈo
  ø => ˈø
  u => ˈu
  y => ˈy
}

[$voiced] {
  k => g
  kʷ => gʷ
  kʰ => g
  kʰʷ => gʷ
  p => b
  t => d
  m̥ => m
  n̥ => n
  r̥ => r
  s => z
}

[$back] {
  ɑ
  ˈɑ
  o
  ˈo
  u
  ˈu
}

[$round] {
  o
  ˈo
  u
  ˈu
  ø
  ˈø
  y
  ˈy
  k => kʷ
  g => gʷ
  t => p
  d => b
}

[$front] {
  a
  ˈa
  e
  ˈe
  i
  ˈi
  ø
  ˈø
  y
  ˈy
}
