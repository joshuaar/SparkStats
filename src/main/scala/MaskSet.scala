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

@serializable
class MaskSet(order:MaskOrder,holes:Array[MaskHoles],length:Int) extends Matable with Entropable {
    override type suitable = MaskSet
    def length():Int = {
      return length
    }
	def getGenetics():(MaskOrder, Array[MaskHoles]) = {
	  (order,holes)
	}
	def mate(partner:suitable):suitable = {
	  val r = new Random()
	  val (orderPartner,holesOther) = partner.getGenetics()
	  val newOrder = order.mate(orderPartner,r.nextInt(order.length))
	  val newHoles = (holes,holesOther).zipped.map( (k1,k2) => k1.mate(k2,length()))
	  new MaskSet(newOrder,newHoles,length)
	}
	
	/**
	 * Mutate the mask order and holes
	 */
	def mutate(rate:Double):MaskSet = {
	  val newHoles = holes.map( a=> a.mutate(rate))
	  val newOrder = order.mutate(rate)
	  new MaskSet(newOrder,newHoles,length)
	}
	
	def getPeptides():Array[String] = {
	  holes.map(i=>i.getPeptide(order))
	}
	
	override def getEntropy():Double = {
	  //This is SLLLOOOWWW
	  val peps = getPeptides()
	  MaskSet.peptideEntropy(peps)
	}
	def getMaxEntropy():Double = {
	  math.min((length-5+1)*holes.length,math.pow(20,5))
	  
	}
  
}
object MaskSet {
  def peptideEntropy(peps:Iterable[String]):Double = {
    //This is SLLLOOOOWWW
    val wins = peps.view.flatMap(i=>window(i,5))
    val winCounts = scala.collection.mutable.Map[String,Int]()
    var finalCount = 0
    for(i <- wins){
      val k = winCounts += (i -> (winCounts.get(i) match {
        case Some(count) => count + 1
        case None => {
          finalCount +=1
          1
        }
      }))
    }
    return winCounts.count(a=>true)
  }
  
  def window(peptide:String,length:Int):Seq[String] = {
	if(length == 0)
	  return Seq[String]()
    for(i<-(0 until peptide.length-length+1).view) yield peptide.slice(i,i+length)
  }
  val alpha = "ACDEFGHIKLMNPQRSTVWY"
  val r = new Random()
  
  def apply(nPeps:Int,nMasks:Int,length:Int) = {
    val maskOrder = (for(i <- (0 until nMasks)) yield alpha(r.nextInt(alpha.length)).toString).foldRight("")( (a,b)=>a+b )
    val maskHoles = (for(i <- (0 until nPeps)) yield genHoles(nMasks,length)).toArray
    new MaskSet(new MaskOrder(maskOrder),maskHoles,length)
  }
  def genHoles(nMasks:Int,length:Int):MaskHoles = {
    val masks = (for(i <- 0 until nMasks) yield false).toArray
    val indicies = Random.shuffle((0 until nMasks).toList).slice(0,length)
    for(i <- indicies) {masks(i)=true}
    return new MaskHoles(masks)
  }
}


@serializable
class MaskOrder(order:String) extends Cuttable{
  var ordering = order
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
  def mutate(rate:Double):MaskOrder = {
    val r = new Random()
    val nMasks = ordering.length
    val ord = ordering.toArray
    for(i <- 0 until math.floor(rate*nMasks).toInt ){
    	val ix=r.nextInt(nMasks)
    	ord(ix) = MaskSet.alpha(r.nextInt(MaskSet.alpha.length))
    }
    new MaskOrder(ord.mkString)
  }
}

@serializable
class MaskHoles(mMatrix: Array[Boolean]) extends Cuttable  {
  var maskMatrix = mMatrix
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
  def mate(partner:MaskHoles, length:Int):MaskHoles = {
    val cutPoint = Random.nextInt(maskMatrix.length-1)
    val (holesA1,holesA2) = cut(cutPoint)
    val (holesB1,holesB2) = partner.cut(cutPoint)
    holesA1.paste(holesB2,length)
  }
  def mutate(rate:Double):MaskHoles = {
    require(rate <= 1, "Rate must be between 0 and 1")
    val r = new Random() // random number generator
    val matrix = getMaskMatrix().clone
    val indicies_true = Random.shuffle(matrix.zipWithIndex.filter( a => a._1 ).map(b=>b._2).toList)
    val indicies_false = Random.shuffle(matrix.zipWithIndex.filter( a => !a._1 ).map(b=>b._2).toList)
    val toChange = indicies_true.slice(0,(rate*indicies_true.length).toInt)
    val toChangetoTrue = indicies_false.slice(0,(rate*indicies_true.length).toInt)
    for(i <- toChange)
      matrix(i) = false
    for(j <- toChangetoTrue)
      matrix(j) = true
    new MaskHoles(matrix)
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
