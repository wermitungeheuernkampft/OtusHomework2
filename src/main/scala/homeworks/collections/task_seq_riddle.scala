package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax


object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */
  task"Реализуйте функцию генерирующую след последовательность из текущей"

  def nextLine(currentLine: List[Int]): List[Int] = {
    def loop(pref:Int, count:Int, s: List[Int]): List[Int] =
      if (s.isEmpty) {
        List(count, pref)
      } else if (s.head == pref) {
        loop(pref, count+1, s.tail)
      } else {
        count :: pref :: loop(s.head, 1, s.tail)
      }
    loop(currentLine.head, 1, currentLine.tail)
  }


  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */
  task"Реализуйте ленивый список, который генерирует данную последовательность"()

  val funSeq: LazyList[List[Int]] = List(1) #:: funSeq.map(x => nextLine(x))
}