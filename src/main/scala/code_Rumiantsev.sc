import scala.collection.immutable
import scala.io.Source

//test
// #1
// your code goes here
val alph="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
val num=alph.length
val alph2=(alph+alph).toList
val message="GHMABGZ VKXTMXL LNVVXLL EBDX GHG-LMHI, XGMANLBTLMBV XYYHKM".toList.filter(_.isLetter)

def encode(mes: List[Char], alph:List[Char], n:Int):List[Char]= mes match {
    case Nil => List()
    case (x::xs)=>alph(alph.indexOf(x)+n)::encode(xs, alph, n)}

def total_encode(mes:List[Char], alph:List[Char], num:Int):immutable.IndexedSeq[Char]={
  for {
    n <- 1 until num
    vars <- encode(mes, alph, n)
  } yield vars}
//total_encode(message, alph2, num)
val answer1 = encode(message, alph2, 7)

println(s"#1 - ${answer1}")

// #2

// your code goes here
val rawvalues=Source.fromURL("https://drive.google.com/uc?export=download&id=0Bw8apsd2PoTYRERMNWVGc044VW8").mkString
val values=rawvalues split "\\s+"
val x = values(0)
val y = values(1)

def BinarySum (a:String, b:String):List[Int] = {
val x1 =a.toList.reverse.map(_.toInt-48)
val y1 =b.toList.reverse.map(_.toInt-48)
  def sum(a1:List[Int], b1: List[Int], n:Int):List[Int]=
  (a1,b1) match {
    case (Nil, Nil) => List()

    case (Nil, y::ys) => if (n==0) y::sum(ys, Nil, 0)
    else if ((x+n)==2) 0::sum(ys, Nil, 1)
    else n::sum(ys, Nil, 0)

    case (x::xs, Nil) => if (n==0) x::sum(xs, Nil, 0)
    else if ((x+n)==2) 0::sum(xs, Nil, 1)
    else n::sum(xs, Nil, 0)

    case (x::xs, y::ys) => if ((x+y+n)==1) 1::sum(xs,ys,0)
    else if ((x+y+n)==2) 0::sum(xs,ys,1)
    else if ((x+y+n)==3) 1::sum(xs,ys,1)
    else  0::sum(xs,ys,0)
  }
  sum(x1, y1, 0).reverse
}
val answer2 = BinarySum(x,y).count(_==1)-BinarySum(x,y).count(_==0)

println(s"#2 - ${answer2}")

// #3

// your code goes here
def sum(n:Int):Int ={
  def sum_extended(x:Int, i:Int, sum:Int):Int={
    val list=(x).toBinaryString.toList
    if (i==n) sum
    else if (list==list.reverse) sum_extended(x+1, i+1, sum+x)
    else sum_extended(x+1, i, sum)}
  sum_extended(1, 0, 0)
}

println(s"#3 - ${sum(73)}")

// #4

// your code goes here
val list =List(-1, -1, -2, -2, 1, -5, 1, 0, 1, 14, -8, 4, 5, -11, 13, 5, 7, -10, -4, 3, -6, 8, 6, 2, -9, -1, -4, 0)

def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
  ls match {
    case Nil => Nil
    case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
  }

def combinations[A](n: Int, ls: List[A]): List[List[A]] =
  if (n == 0) List(Nil)
  else flatMapSublists(ls) { sl =>
    combinations(n - 1, sl.tail) map {sl.head :: _}
  }

val combo = combinations(3, list) filter (x=>(x.sum==0))  map (x=>x.sorted)
val prefinal = combo.distinct

println(s"#4 - ${prefinal.length}")

// #5
val rawfile = Source.fromURL("https://drive.google.com/uc?export=download&id=0Bw8apsd2PoTYb05lVk0tbzFlZzg")
val data = rawfile.mkString split "\\s+" map(_ toDouble)
val nums = data.toList

def Sum(xs: List[Double]) = (xs foldLeft 0.0)(_+_)

val answer5=Sum(nums).toString.filter(_.isDigit).take(10)

// your code goes here

println(s"#5 - ${answer5}")