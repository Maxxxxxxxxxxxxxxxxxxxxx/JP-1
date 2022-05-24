val d = io.Source
  .fromFile("world-happiness-report.csv")
  .getLines()
  .toList

type Datum = (String, Int, Double)

def happinessTest(d: List[String]): String =

  val d1 = (for (e <- d)
  yield e.split(",")
    .toList
    )
  .filter(e => !e.contains(""))
  .foldLeft(List[Datum]())
    ((acc, e) => acc :+ (e(0), e(1).toInt, e(2).toDouble))
  .sortBy(_._2)
  .groupBy(_._1)
  .toList

  (for (e <- d1)
  yield e._2
    .foldLeft(List[List[Datum]]())
      ((acc, i) => {
        (acc, i) match {
          case (Nil, i) => List(i) :: acc
          case _ => 
            if (acc.head.head._2 - i._2).abs == 1 && i._3 > acc.head.head._3
            then acc.tail :+ (i :: acc.head)
            else List(i) :: acc
        }
      })
      .foldLeft (("", 0)) ((acc, listaDatum) => {
        if listaDatum.size > acc._2 
        then (listaDatum.head.head, listaDatum.size)
        else acc
      })
    )
    .sortBy(_._2)
    .reverse
    .head._1