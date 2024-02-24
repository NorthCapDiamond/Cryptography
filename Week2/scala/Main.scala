import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

@main def hello(): Unit =
  println("Enter your open word")
  val inputList : List[Int] = readLine().toList.map(_.toString.toInt)
  val nInput : Int = inputList.length
  println("Enter amount of keys")
  val nIters : Int = readInt()

  val keyList: List[List[Int]] = fillListInt(nIters)
  val tmp = encryption(inputList, keyList, nIters)
  val answer = tmp(0) ++ tmp(1)

  printlnListInt(answer)
  


def fCipher(i : Int, j : Int) = i^j

def encryption_iter(left : List[Int], right : List[Int], key : List[Int]) : (List[Int], List[Int]) = 
  val leftAnswer : List[Int] = right
  val applyFCipher : List[Int] = (key zip right).map{(k, r) => fCipher(k, r)}
  val rightAnswer : List[Int] = (left zip applyFCipher).map{(l, r) => (l^r)}

  (leftAnswer, rightAnswer)


def encryption(word : List[Int], key : List[List[Int]], n: Int) : (List[Int], List[Int]) = { 
  @tailrec
  def go(leftik : List[Int], rightik : List[Int], i: Int = 0) : (List[Int], List[Int]) = {
    if (i >= n) (leftik, rightik) else{
      val resIter = encryption_iter(leftik, rightik, key.apply(i))
      go(resIter(0), resIter(1), i+1)
    }
  }
  go(word.slice(0, word.length/2 + 1), word.slice(word.length/2, word.length))
}

def printlnListInt(list: List[Int]) : Unit = { 
  @tailrec
  def go(i : Int = 0, answer : String = "") : String = if (i >= list.length) answer else go(i + 1, answer+ list.apply(i).toString)

  println(go())
}

def fillListInt(n: Int) : List[List[Int]] = {
  @tailrec
  def go(answer : List[List[Int]] = List(List()), i: Int = 0) : List[List[Int]] = if(i >= n) answer else {println("Enter your k"); go(answer:+((readLine().toList).map(_.toString.toInt)), i+1)}

  val listik = go()
  listik.slice(1, listik.length)
}
