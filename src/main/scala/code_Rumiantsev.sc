import scala.collection.immutable
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

println(s"#1 - ${encode(message, alph2, 7)}")

// #2

// your code goes here
def convert_it(list:List[Char], n:Int):Double = list match{
  case Nil => 0
  case  x::xs=>(x.toInt-48)*scala.math.pow(2,n)+convert_it(xs, n+1)
}
def convert_it2(n:Double):List[Char] = {if (n==0) List()
else (n%2).toChar::convert_it2(n/2)}

val x ="10001011101010101010000111110111011110101010101101110101010101010010000010110100101010101011011010100101011010101010101010101010101110101011000101101011110101010101010101010001010101010101101010101010101010101010101010111000001010101111010100111010101001011101010111111111101010101111111101010111110101001010101111110111101011010111111101011110101111111111111101111111111010101111101010101001111101010101010100100101010111101001010101001010101001010111110101010101010101011110101010010101001111101010100101111101010101001111111111101010111111111101001010111111110110101001111101010101111111010110100011111111111010101101011111110101010101110101010101010001110111101010101010101010101000001010110111111010101010010101011110101010000001010101000000000000101001111100000000000010010101010000001".toList.reverse
val y ="11100101000010101000001010010000010101011000110000110101000001010100000010000000010101100000110100100010111111111111111010010001010000001000000100000101011110101000000001010100000001010100101010111001010100000000000010101010101101010010101010101111001010000000000000001010010100111000010000000010100001010101000000110000001010101000000000000101001111100000000000010010101010000001".toList.reverse
val x1 = convert_it(x, 0)
val y1 = convert_it(y, 0)
val answer=(x1+y1)


println(s"#2 - ${/*answer #2*/}")

// #3

// your code goes here

println(s"#3 - ${/*answer #3*/}")

// #4

// your code goes here

println(s"#4 - ${/*answer #4*/}")

// #5

// your code goes here

println(s"#5 - ${/*answer #5*/}")