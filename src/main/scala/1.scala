def countResults[A,B,C](l1: List[A], l2: List[B], acc: List[C] = List())(f: (A, B) => C): Set[(C, Int)] =
  (l1, l2) match {
    case (h1 :: t1, h2 :: t2) => countResults(t1, t2,  f(h1,h2) :: acc)(f)
    case _ => acc.groupBy(identity).mapValues(e => e.length).toSet
  }