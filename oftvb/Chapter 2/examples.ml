let x = 200

let x = 200 in x * x * x

let a = 500 in (let b = a * a in a + b)

let cube x = x * x * x

let neg x = if x < 0 then true else false

let neg x = x < 0

let isvowel c =
  c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u'

let addtoten a b =
  a + b = 10

let rec factorial a =
  if a = 1 then 1 else a * factorial (a - 1)

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

let not x =
  if x then false else true
