package com.protomapper.masks
import scala.util.Random

trait Cuttable {
  type suitable
  def cut(cutPoint:Int):(suitable,suitable)
  def paste(otherCuttable: suitable, length: Int):suitable
}

trait Matable {
  type suitable
  def mate(partner:suitable):suitable
}

trait Entropable {
  def getEntropy():Double
}

class MaskSet(order:MaskOrder,holes:Array[MaskHoles]) extends Matable with Entropable {
    override type suitable = MaskSet
	def getGenetics():(MaskOrder, Array[MaskHoles]) = {
	  (order,holes)
	}
	def mate(partner:suitable):suitable = {
	  val (orderPartner,holesOther) = partner.getGenetics()
	  val newOrder = order.mate(orderPartner,order.length)
	  val newHoles = (holes,holesOther).zipped.map( (k1,k2) => k1.mate(k2,order.length))
	  new MaskSet(newOrder,newHoles)
	}
	override def getEntropy():Double = {
	  //This is SLLLOOOWWW
	  val peps = holes.map(i=>i.getPeptide(order))
	  MaskSet.peptideEntropy(peps)
	}
  
}
object MaskSet {
  def peptideEntropy(peps:Iterable[String]):Double = {
    //This is SLLLOOOOWWW
    val wins = peps.map(i=>window(i,5).toSet).reduce( (a,b) => a.union(b) )
    return wins.count( a => true )
  }
  
  def window(peptide:String,length:Int):Seq[String] = {
	if(length == 0)
	  return Seq[String]()
    for(i<-(0 until peptide.length-length+1).view) yield peptide.slice(i,i+length)
  }
  val alpha = "ACDEFGHIKLMNPQRSTVWY"
  val r = new Random()
  def apply(nPeps:Int,nMasks:Int) = {
    val maskOrder = (for(i <- (0 until nMasks)) yield alpha(r.nextInt(alpha.length)).toString).foldRight("")( (a,b)=>a+b )
    val maskHoles = (for(i <- (0 until nPeps)) yield genHoles(nMasks)).toArray
    new MaskSet(new MaskOrder(maskOrder),maskHoles)
  }
  def genHoles(nMasks:Int):MaskHoles = {
    return new MaskHoles((for(i <- 0 until nMasks) yield r.nextBoolean).toArray)
  }
}



class MaskOrder(ordering:String) extends Cuttable{
  override type suitable = MaskOrder
  def getOrdering():String = {
    return ordering
  }
  def length():Int = {
    getOrdering.length
  }
  def cut(cutPoint:Int):(suitable,suitable) = {
    (new MaskOrder(ordering.slice(0,cutPoint)),new MaskOrder(ordering.slice(cutPoint,ordering.length)))
  }
  def paste(otherMask:suitable, length:Int):suitable = {
    new MaskOrder(ordering++otherMask.getOrdering)
  }
  def mate(partner:MaskOrder, cutPoint:Int):MaskOrder = {
    val (orderA1,orderA2) = cut(cutPoint)
    val (orderB1,orderB2) = partner.cut(cutPoint)
    orderA1.paste(orderB2,length)
  }
}

class MaskHoles(maskMatrix: Array[Boolean]) extends Cuttable  {
  def getMaskMatrix():Array[Boolean] = {
    return maskMatrix
  }
  override type suitable = MaskHoles
  def cut(cutPoint:Int):(suitable,suitable) = {
    val mh1 = maskMatrix.slice(0,cutPoint) 
    val mh2 = maskMatrix.slice(cutPoint,maskMatrix.length)
    (new MaskHoles(mh1),new MaskHoles(mh2))
  }
  def getNBits():Int = {
    implicit def bool2int(b:Boolean) = if (b) 1 else 0
    getMaskMatrix.map(k=>bool2int(k)).sum
  }
  def paste(otherMask:MaskHoles, length:Int) = {
    var fromFront = true
    if(getNBits() >= otherMask.getNBits)
      fromFront = false
    var combined = MaskHoles.modifyBits(getMaskMatrix ++ otherMask.getMaskMatrix,length,fromFront)
    new MaskHoles(combined)
  }
  def getPeptide(ordering: MaskOrder):String = {
    val a = getMaskMatrix.zip(ordering.getOrdering)
    val b = a.filter( k => k._1 ).map( k => k._2.toString )
    b.foldRight("")( (k1,k2) => k1+k2)
  }
  def mate(partner:MaskHoles, cutPoint:Int):MaskHoles = {
    val (holesA1,holesA2) = cut(cutPoint)
    val (holesB1,holesB2) = partner.cut(cutPoint)
    holesA1.paste(holesB2,maskMatrix.length)
  }

}
object MaskHoles {
  def modifyBits(arr:Array[Boolean], nTarget:Int, fromFront:Boolean):Array[Boolean] = {//:Array[Boolean] = {
    var a = arr
    implicit def bool2int(b:Boolean) = if (b) 1 else 0
    var nBits = a.map(k=>bool2int(k)).sum
    require(nTarget <= a.length, "Cannot replace more bits than are in array")
    if(nBits > nTarget){ // more bits than needed
        var ix = 0
        if(!fromFront)
          ix = a.length - 1
        while(nBits > nTarget){
          if(a(ix)){
            a(ix) = false
            nBits -= 1
          }
          if(fromFront){ix += 1}
          else{ix -= 1} // change index
        }
    }
    else if(nBits < nTarget) {
      val r = new Random()
      while(nBits < nTarget) {
        var ix = r.nextInt(a.length) // choose a random integer
        if(!a(ix)){
          a(ix) = true
          nBits += 1
        }
      }
    }
    a
  }
  def mutate(arr:Array[Boolean]):Array[Boolean] = { //mutate a single bit in the sequence
    val r = new Random()
    val ix = r.nextInt(arr.length)
    val toflip = arr(ix)
    val replace = if (toflip) (false) else (true)
    arr(ix) = replace
    return arr
    }
}
