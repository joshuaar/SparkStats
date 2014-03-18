package com.protomapper.masks
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import math._
import scala.util.Random




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
  val nChunks = 16
  val sc = new SparkContext(s"local[${nChunks}]", "Simple App", SPARK_HOME)
  
  
  def generationCycle(population:RDD[MaskSet],nextPop:Int,mutationRate:Double,nFit:Int=4):RDD[MaskSet] = {
    val entropies = population.map(indiv => (indiv.getEntropy(),indiv))
    val ent2 = entropies.sortByKey(false)
    
    val fittest = ent2.take(nFit)
    println(s"Fittest: ${fittest(0)._1} Max: ${fittest(0)._2.getMaxEntropy()}")
    println(s"SecondFittest: ${fittest(1)._1} Max: ${fittest(0)._2.getMaxEntropy()}")
    val nChildren = (nextPop/(nFit/2)).toInt
    val fittestPar = sc.parallelize(fittest,nChunks)
    fittestPar.flatMap( a => {
      for(i <- 0 until nChildren) yield a._2.mutate(mutationRate) //mate a certain number of times for each pair
      } )
  }
  
  def getFittest(population:RDD[MaskSet]):MaskSet = {
    val entropies = population.map(indiv => (indiv.getEntropy(),indiv))
    val ent2 = entropies.sortByKey(false,nChunks)
    ent2.take(1)(0)._2
  }
  
  def main(args: Array[String]) {
    val nPeps = 100000
    val nMasks = 40
    val len = 12
    val popSize = 100
    val population = ((0 until popSize)).map( a => MaskSet(nPeps,nMasks,len) )
    var rddata = sc.parallelize(population,nChunks)
//    println(rddata.map( a => {
//      for(i <- 1 until 100000000) {
//        i+ 100
//      }
//      1
//    }).reduce((a,b) => a+b ))
    for(i <- 0 until 100) rddata = generationCycle(rddata,popSize,0.1,nFit=10)
  }
}
