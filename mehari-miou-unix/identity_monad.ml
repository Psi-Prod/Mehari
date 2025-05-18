type 'a t = 'a

let return x = x
let bind x f = f x
let map f x = f x
