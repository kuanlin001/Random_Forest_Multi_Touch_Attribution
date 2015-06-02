/**
 * @author kuanlin
 */

import scala.io.Source

class unitData(val id: Int, val channel_exposures: Map[String, Boolean], val isConverted: Boolean)

object Program {
  def main(args: Array[String]){
    val dataFile = "C:\\Users\\kuanlin\\workspace\\Random_Forest_Multi_Touch_Attribution\\test_data.csv"
    val (channel_list, data_list) = loadSourceFile(dataFile)
    
    /* to verify data loading:
    for(dat <- data_list){
      println(dat.id + "," + channel_list.map(c => dat.channel_exposures(c)).mkString(",") + dat.isConverted)
    }
    */
    
    val model = new decisionTreeModel(data_list, channel_list)
    model.grow_id3(maxDepth=5, minLeafSample=5)
    val channel_credits = AttributionMethods.decisionTreeHeuristic(model.channel_list, model.getLeafNodes)
    
    /*
     *using bagging function to perform random forest.  this toy example dataset is too small to work with random forest. 
     *val channel_credits = bagging(channel_list, iter=10, data_sample_size=100, channel_sample_size=5)
     */
    
    for(key <- channel_credits.keys)
      println(key + " -> " + channel_credits(key))
  }
  
  
  def loadSourceFile(filePath: String): (Seq[String], Seq[unitData]) = {
    val datalist = new scala.collection.mutable.ListBuffer[unitData]()
    var channel_list: Seq[String] = Nil
    
    val src = Source.fromFile(filePath)
    try {
      for(line <- src.getLines()) {
        val lineArr = line.split(",") 
        if(!line.startsWith("user_id,")) {
          val user_id = lineArr(0).toInt
          val isConverted = (if(lineArr(lineArr.length-1)=="1") true else false)
          val channel_exposures = channel_list.zip(lineArr.slice(1, lineArr.length-1).map(x => if(x=="1") true else false)).toMap
          datalist += new unitData(user_id, channel_exposures, isConverted)
        }
        else {
          channel_list = lineArr.slice(1, lineArr.length-1)
        }
      }
    } 
    catch { case e: Exception => println(e.printStackTrace())}
    finally src.close()
    
    (channel_list, datalist.result)
  }
  
  def bagging(data_list: Seq[unitData], channel_list: Seq[String], iter: Int, data_sample_size: Int, channel_sample_size: Int): Map[String, Double] = {
    val aggregated_credits = new scala.collection.mutable.ListBuffer[Tuple2[String, Double]]()
    for(i <- 0 until iter) {
      val model = new decisionTreeModel(scala.util.Random.shuffle(data_list).slice(0, data_sample_size), scala.util.Random.shuffle(channel_list).slice(0, channel_sample_size))
      model.grow_id3(maxDepth=5, minLeafSample=5)
      val channel_credits = AttributionMethods.decisionTreeHeuristic(model.channel_list, model.getLeafNodes)
      for(key <- channel_credits.keys)
        aggregated_credits += Tuple2(key, channel_credits(key))
    }
    
    val aggregated_maps = aggregated_credits.groupBy(x => x._1)
    val average_credits = new scala.collection.mutable.HashMap[String, Double]()
    for(key <- aggregated_maps.keys)
      average_credits += key -> aggregated_maps(key).map(_._2).reduce(_+_)/aggregated_maps(key).length
    
    average_credits.toMap
  }
}
