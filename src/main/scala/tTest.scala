import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import math._

object SimpleApp {
  val SPARK_HOME = "/home/josh/software/spark-0.9.0-incubating"
  
  def readData(fName: String):Array[Map[String,Array[Double]]] = {
    val lines = scala.io.Source.fromFile(fName).mkString.split("\n")
    val header = lines(0).split("\t").tail
    val body = lines.tail.zipWithIndex.map( (args) => {
      val splitted = args._1.split("\t")
      val pepKey = splitted.head
      val pepVals = splitted.tail.map( (a) => a.toDouble )
      val pepValsMap = pepVals.zip(header).groupBy( x => x._2 ).map( kv => (kv._1,kv._2.map(v=>v._1)) )
      pepValsMap
    } )
    body
  }
  
  def mean[T]( ts: Iterable[T] )( implicit num: Numeric[T] ) = {
    num.toDouble( ts.sum ) / ts.size
  }
  
  def variance[T]( ts: Array[Double] ):Double = {
    val xbar = mean(ts)
    val deviations = ts.map( xi => {
      val dev = (xi - xbar)
      dev * dev
    } )
    val numerator = deviations.sum //sum((xi - xbar)^2)
    val denominator = ts.length - 1 //n - 1
    return numerator / denominator
  }
  
  def tTestWelch(x1:Array[Double], x2:Array[Double]):(Double,Double) = {
    val xbar1 = mean(x1)
    val xbar2 = mean(x2)
    val s1 = variance(x1)
    val s2 = variance(x2)
    val n1 = x1.length
    val n2 = x2.length
    val v1 = n1 - 1
    val v2 = n2 - 1
    val denomsquared = (s1/n1) + (s2/n2)
    val stat = (xbar1 - xbar2) / sqrt(denomsquared)
    val df = pow((s1/n1) + (s2/n2),2) / ( (pow(s1,4)/(pow(n1,2)*v1)) + (pow(s2,4)/(pow(n2,2)*v2)) )
    return (stat, df)
  }
  
  
  
  def main(args: Array[String]) {
    val logFile = s"/home/josh/CIM/Research/labdata/jaricher/healthtell/CRCvNorm.csv" // Should be some file on your system
    println("Reading Data")
    val data = readData(logFile)
    println("Spark Part")
    val sc = new SparkContext("local", "Simple App", SPARK_HOME)
    val rddata = sc.parallelize(data,2*8)
    val dataSums = rddata.map( attr => attr.map( grp => (grp._1,grp._2.reduce( (a,b) => a+b )) ) )
    val totalSum = dataSums.reduce( (a,b) => {
      val lst = a.toList ++ b.toList
      lst.groupBy(_._1).map( a => a._2.reduce( (a,b) => (a._1,a._2+b._2) ) )
    } )
    println(totalSum)
  }
}
