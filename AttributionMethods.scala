/**
 * @author kuanlin
 */
object AttributionMethods {
  def decisionTreeHeuristic(channel_list: Seq[String], leafNodes: Seq[treeNode]): Map[String, Double] = {
    val channel_credits = new scala.collection.mutable.HashMap[String, Double]()
    for(c <- channel_list)
      channel_credits += c -> 0.0
      
    for(node <- leafNodes) {
      if(node.conversionCount > 0 && node.parentNode != None) {
       val credit_candidates = new scala.collection.mutable.ListBuffer[String]()
       var info_gains = new scala.collection.mutable.ListBuffer[Double]()
       
       var currNode = node
       while(!currNode.isRootNode && currNode.parentNode != None) {
         val parentNode = currNode.parentNode.get
         if(parentNode.exposedChildNode != None && parentNode.exposedChildNode.get == currNode) {
           if(parentNode.unexposedChildNode == None || parentNode.exposedChildNode.get.conversionRatio > parentNode.unexposedChildNode.get.conversionRatio) {
             credit_candidates += parentNode.splitCandidate
             info_gains += parentNode.info_gain
           }
         }
         currNode = parentNode
       }
       
       info_gains = info_gains.map(_ / info_gains.reduce(_+_))
       for(i <- 0 until credit_candidates.length)
         channel_credits(credit_candidates(i)) = channel_credits(credit_candidates(i)) + info_gains(i)*node.conversionCount
      }
    }
    
    val norm_const: Double = channel_credits.values.reduce(_+_)
    for(key <- channel_credits.keys)
      channel_credits(key) = channel_credits(key) / norm_const
      
    channel_credits.toMap
  }
}
