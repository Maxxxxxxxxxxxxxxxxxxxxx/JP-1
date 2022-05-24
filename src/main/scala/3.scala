val data = io.Source
  .fromFile("temperatury.txt")
  .getLines()
  .toList

def getMaxAvgTemp(data: List[String]): Set[(Int, Double)] =
  (for (e <- data) 
  yield e
    .split(" ")
    .tail
    .foldLeft(List[Double]()) 
      ((acc, num) => acc :+ num.toDouble)
    .zipWithIndex
    .foldLeft(List[(Int, Double)]()) 
      ((acc, e) => acc :+ (e(1)+1, e(0)))
    )
  .flatten
  .groupBy(e => e(0))
  .mapValues(e => e.foldLeft(-100.0)
    ((acc, e) => if e(1) > acc then e(1) else acc))
  .toSet