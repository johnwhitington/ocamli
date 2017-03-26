let isvowel c =
  c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u'

let isconsonant c =
  not (isvowel c)
