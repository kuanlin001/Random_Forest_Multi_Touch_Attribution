/**
 * @author kuanlin
 */

class decisionTreeModel(val rootNodeData: Seq[unitData], val channel_list: Seq[String]) {
  private val rootNode = new treeNode(isRootNode = true, nodeData = rootNodeData)
  private val leafNodes = new scala.collection.mutable.ListBuffer[treeNode]()
  
  def grow_id3(maxDepth: Int = 5, minLeafSample: Int = 5) {
    DecisionTreeId3.growTree(rootNode, channel_list = channel_list, leafNodes_Collector = Some(leafNodes))
  }
  
  def getRootNode = rootNode
  def getLeafNodes = leafNodes.result()
}

class treeNode(
       var splitCandidate: String = "",
       var parentNode: Option[treeNode] = None,
       var exposedChildNode: Option[treeNode] = None,
       var unexposedChildNode: Option[treeNode] = None,
       var isLeafNode: Boolean = false,
       var isRootNode: Boolean = false,
       var info_gain: Double = Double.NaN,
       val nodeData: Seq[unitData],
       private var _nodeEntropy: Double = Double.NaN,
       private var _branchDepth: Int = -1)
 {
  def branchDepth(): Int = {
    if(_branchDepth == -1) {
      _branchDepth = 0
      var nd = this
      while(nd.parentNode != None) {
       _branchDepth += 1
       nd = nd.parentNode.get
      }
    }
    _branchDepth
  }
  
  def nodeEntropy() : Double = {
    if(_nodeEntropy.isNaN)
      _nodeEntropy = DecisionTreeId3.entropy(nodeData)
    _nodeEntropy
  }
  
  def sampleSize = nodeData.length
  def parentSplitCandidat = { if(this.isRootNode) "" else this.parentNode.get.splitCandidate }
  def conversionRatio = DecisionTreeId3.conversionRatio(nodeData)
  def conversionCount = this.nodeData.filter(_.isConverted).length
}

object DecisionTreeId3 {
  def growTree(baseNode: treeNode, maxDepth: Int = 5, minLeafSample: Int = 5, channel_list: Seq[String], leafNodes_Collector: Option[scala.collection.mutable.ListBuffer[treeNode]] = None) {
    if(baseNode.branchDepth() < maxDepth && baseNode.nodeData.length > minLeafSample && channel_list.length > 0 && baseNode.nodeEntropy() > 0) {
      val opt = ((channel_list.map(partitionEntropy(baseNode.nodeData, _)).zip(channel_list).sorted).toList)(0)
      val opt_channel = opt._2
      val opt_partitionEntropy = opt._1
      
      if(baseNode.nodeEntropy() > opt_partitionEntropy) {
        baseNode.splitCandidate = opt_channel
        baseNode.info_gain = baseNode.nodeEntropy() - opt_partitionEntropy
        val child1 = new treeNode(parentNode = Some(baseNode), nodeData = baseNode.nodeData.filter { _.channel_exposures(opt_channel) })
        val child2 = new treeNode(parentNode = Some(baseNode), nodeData = baseNode.nodeData.filter { !_.channel_exposures(opt_channel) })
        baseNode.exposedChildNode = Some(child1)
        baseNode.unexposedChildNode = Some(child2)
        
        val new_channel_list = channel_list.filter(_ != opt_channel)
        growTree(baseNode = child1, channel_list = new_channel_list, maxDepth = maxDepth, minLeafSample = minLeafSample, leafNodes_Collector = leafNodes_Collector)
        growTree(baseNode = child2, channel_list = new_channel_list, maxDepth = maxDepth, minLeafSample = minLeafSample, leafNodes_Collector = leafNodes_Collector)
      }
      else {
        baseNode.isLeafNode = true
        if(leafNodes_Collector != None)
          leafNodes_Collector.get += baseNode
      }
    }
    else {
      baseNode.isLeafNode = true
      if(leafNodes_Collector != None)
        leafNodes_Collector.get += baseNode
    }
  }
  
  def conversionRatio(data: Seq[unitData]): Double = {
    val converted_cnt = data.filter(_.isConverted).length
    val total_cnt = data.length
    
    converted_cnt.toDouble / total_cnt.toDouble
  }
  
  def entropy(data: Seq[unitData]): Double = {
    if(data.length == 0) return 0.0
    
    val p1 = conversionRatio(data)
    val p2 = 1.0 - p1
    var log2p1 = 0.0
    var log2p2 = 0.0
    if(p1 != 0) log2p1 = scala.math.log(p1)/scala.math.log(2)
    if(p2 != 0) log2p2 = scala.math.log(p2)/scala.math.log(2)
    
    -1.0*p1*log2p1 - p2*log2p2
  }
  
  def partitionEntropy(data: Seq[unitData], splitChannel: String): Double = {
    val split1 = data.filter(_.channel_exposures(splitChannel))
    val split2 = data.filter(!_.channel_exposures(splitChannel))
    
    split1.length.toDouble / data.length.toDouble * entropy(split1) + split2.length.toDouble / data.length.toDouble * entropy(split2)
  }
}
