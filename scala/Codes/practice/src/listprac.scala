
object listprac extends App { 
 val list= List(1,2,3,4,5)
 val list2 = List(3,4,5,6)
  //It's aim to define the function in recursive way.
 def max[A<%Double](list:List[A]):A= list match {
   case Nil => throw new Exception("please input one variable")
   case x::Nil => x
   case x::xs => if (x<xs.head) max(xs) 
                 else max(x::xs.tail)
 }

def map[A,B](list:List[A],f:A=>B):List[B]=list match {
   case Nil => Nil
   case x::xs => f(x)::map(xs,f)
 }

def filter[A](list:List[A],f:A=>Boolean):List[A]={
   val acc =List[A]()
   list match {
   case Nil => acc
   case x::xs => if (f(x)) {acc:::List(x):::filter(xs,f)}
                 else acc:::filter(xs,f)
 }}
 
def union[A](list:List[A],other:List[A]):List[A]= {
  other match {
  case Nil => list
  case head::tail => union(list.filter(e=>e!=head):::List(head),tail)
}}
println(max(list))
println(map(list,(e:Int)=>e*2))
println(filter(list,(x:Int)=>x%2==0))
println(union(list,list2))
}

