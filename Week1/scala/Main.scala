import scala.annotation.tailrec
import scala.io.StdIn.readLine

@main def main(): Unit =
  println("Enter your Open Word")
  val myOpenWord : String = readLine().replaceAll("\\s", "")
  val n = myOpenWord.length
  println("Enter your Key Word")
  val myClosedWord : String = fixSize(readLine().replaceAll("\\s", ""), n)
  val myOpenList : List[Char] = myOpenWord.toList
  val myClosedList : List[Char] = myClosedWord.replaceAll("\\s", "").toList
  val lang = "ru"
  val langnumber = (if (lang=="ru") 33 else 26)

  println("Open Word : " ++ myOpenWord)
  println("New Closed Word : " ++ myClosedWord)

  println("\nIf you code this, it's gonna be : " ++ ((myOpenList zip myClosedList).map{(o, c) => code(o, c, langnumber)}).mkString)
  println("If you decode this, it's gonna be : " ++ ((myOpenList zip myClosedList).map{(o, c) => decode(o, c, langnumber)}).mkString ++ "\n")



def code(x: Char, y: Char, n: Int) : Char = {
  val xValue = (utfChar2IntAdapter(x, if(n==33) "ru" else "en") - aUpperOrLower(x).toInt)
  val yValue = (utfChar2IntAdapter(y, if(n==33) "ru" else "en") - aUpperOrLower(y).toInt)
  utfInt2CharAdapter(aUpperOrLower(x).toInt + (xValue + yValue) % n, if(n==33) "ru" else "en")
}

def decode(x: Char, y:Char, n: Int) : Char = { 
  val xValue = (utfChar2IntAdapter(x, if(n==33) "ru" else "en") - aUpperOrLower(x).toInt)
  val yValue = (utfChar2IntAdapter(y, if(n==33) "ru" else "en") - aUpperOrLower(y).toInt)
  val elseValue = n + xValue - yValue
  utfInt2CharAdapter(aUpperOrLower(x).toInt + (if (xValue >= yValue) (xValue - yValue) else elseValue), if(n==33) "ru" else "en")
}

def aUpperOrLower(x: Char) = {
  if ( x >= 'a' & x <= 'z' ) 'a' else {
    if(x >= 'A' & x <= 'Z') 'A' else {
      if(x >= 'а' & x <= 'я') 'а' else 'А'
    }
  }
}

def utfChar2IntAdapter(x: Char, lang: String) : Int = {
  if (lang != "ru") x.toInt else { 
    if (x == 'ё') 6 else { 
      if( x > 'е') (x.toInt + 1) else x
    }
  } 
}

def utfInt2CharAdapter(x: Int, lang: String) : Char = { 
  if (lang!= "ru") x.toChar else {
    if (x == 6) 'ё' else { 
      if (x <= 'е'.toInt) x.toChar else (x-1).toChar
    }
  }
}

def fixSize(myClosedWord : String, n: Int) : String = {
  @tailrec
  def go(acc: String = myClosedWord) : String = {
    if(acc.length >= n) acc.take(n) else go(acc+myClosedWord)
  }
  go(myClosedWord)
}



