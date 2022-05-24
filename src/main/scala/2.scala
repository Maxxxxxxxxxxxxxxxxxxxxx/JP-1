def pairwiseTest[A](l: List[A], acc: Boolean = true, index: Int = 0)(pred: (A, A) => Boolean): Boolean =
  if !acc 
  then false
  else {
    if index > l.length / 2 
    then acc
    else pairwiseTest(l, pred(l(index), l(l.length-(1+index))), index+1)(pred)
  }