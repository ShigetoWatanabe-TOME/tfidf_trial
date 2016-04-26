import scala.io.Source
import scala.math._
import java.io.PrintWriter
import scala.sys.process.Process

object TFIDF {

  class Docs{
    
    // train
    var bow    = List.empty[String]
    var pre_bow    = List.empty[String]
    var train_words = List.empty[ List[String] ]
    var tf:    Array[Array[Double]] = null
    var tmp_tf:    Array[Array[Double]] = null
    var df:    Array[Double] = null
    var idf:   Array[Double] = null
    var tfidf: Array[Array[Double]] = null 
    var short_tf:    Array[Array[Double]] = null
    var short_idf:   Array[Double] = null
    var short_tfidf: Array[Array[Double]] = null 
    var short_bow = List.empty[String]
    var threshold:Double = 0.0
    // test
    var test_tf:    Array[Double] = null
    var tmp_test_tf:    Array[Double] = null
    var test_tfidf: Array[Double] = null
    var short_test_tf:    Array[Double] = null
    var short_test_tfidf: Array[Double] = null 
    var test_words = List.empty[ String ]
    var labels:Array[String] =null

    def read_traindata(filename_bow:String, 
                       filename_idf:String, 
                       filename_tfidf:String,
                       threshold_N:Int ) {
      // sources
      val source_bow   = Source.fromFile(filename_bow)
      val source_idf   = Source.fromFile(filename_idf)
      val source_tfidf = Source.fromFile(filename_tfidf)
      // bow 1 row
      for( line <- source_bow.getLines )
        bow = line.stripLineEnd.split(",").toList // ok   
      // idf 1 row
      for( line <- source_idf.getLines ){
        val ss_idf = line.stripLineEnd.split(",")
        idf = ss_idf.map( x => x.toDouble)
      }
      // tfidf 
      var cnt:Int=0;var tfidf_length:Int=0
      for( (line,index) <- (source_tfidf.getLines).zipWithIndex ){
        val ss_tfidf = line.stripLineEnd.split(",")
        if(cnt==0){
          tfidf = Array.ofDim[Double]( ss_tfidf(0).toInt,  ss_tfidf(1).toInt); 
          labels = Array.ofDim[String]( ss_tfidf(0).toInt); cnt=1;
        }else{
          labels(index-1) = ss_tfidf.head; val ss_tfidf_tail = ss_tfidf.tail
          tfidf(index-1)  = ss_tfidf_tail.map( x => x.toDouble)
        }
      }

      // delete top-N idf words
      //println("start dimension reduction")
      val distinct_idf = (idf.distinct).sortBy(x => -x)
      threshold = distinct_idf(threshold_N)
      println(threshold)
      var short_dim = (idf.filter(n => n < threshold)).length // cnt
      short_idf    = Array.ofDim[Double]( short_dim) 
      short_tfidf = Array.ofDim[Double]( tfidf.length , short_dim) 
      short_bow = bow.zipWithIndex.collect{
        case(s,index) if idf(index) < threshold => s } 
      short_idf = idf.zipWithIndex.collect{
        case(s,index) if s < threshold => s } 
      for( (i,index) <- tfidf.zipWithIndex){
        short_tfidf(index) = i.zipWithIndex.collect{
          case(s,index2) if idf(index2) < threshold => s } 
      }

    }

    
    def create_tfidf_for_test(test_mecab_filename:String){
      // fileread
      var tmp_test_words = List.empty[String]
      val source_test = Source.fromFile(test_mecab_filename )
      for( line <- source_test.getLines ) {
        tmp_test_words = line.stripLineEnd.split(" ").toList.reverse ::: tmp_test_words
      } 
      source_test.close
      test_words = tmp_test_words.reverse; 
      //create tfidf
      test_tf = Array.ofDim[Double](    idf.length)     // normalized 
      tmp_test_tf = Array.ofDim[Double](idf.length) // not normalized
      test_tfidf = Array.ofDim[Double]( idf.length) // tf * idf

      //println(bow)
      for(i <- test_words)
        bow.collect{case s if s==i => tmp_test_tf( bow.indexOf(s) )+=1} 
      var sum:Double = tmp_test_tf.sum; 
      test_tf = tmp_test_tf.map(x => x/sum) // normalized 
      test_tfidf = test_tf.zipWithIndex.map{ 
        case (x, index) => x * idf(index)} // test-tfidf 
      
      // delete top-N idf words
      short_test_tfidf = Array.ofDim[Double](short_idf.length) 
      short_test_tfidf = test_tfidf.zipWithIndex.collect{
        case(s,index) if  idf(index) < threshold => s } 
    }

    def train_mecabfile(filename: Array[String]) {

      var tmp_pre_bow    = List.empty[String]
      var tmp_train_words = List.empty[ List[String] ]
      for(i <- filename){
        val source = Source.fromFile(i)
        for( line <- source.getLines ) {
            tmp_pre_bow = line.stripLineEnd.split(" ").toList.reverse ::: tmp_pre_bow
            tmp_train_words = line.stripLineEnd.split(" ").toList :: tmp_train_words
        } 
        source.close
      }
      train_words = tmp_train_words.reverse; 
      pre_bow = tmp_pre_bow.reverse;
    }

    def create_bow() { // side effect? 
      bow = pre_bow.distinct
      println(bow)
    }

    def create_tfidf() {
      // bow -> dic & init vec, words( tf, idf) 
      tf = Array.ofDim[Double]( train_words.length, bow.length) // normalized 
      tmp_tf = Array.ofDim[Double]( train_words.length, bow.length) // not normalized
      df    = Array.ofDim[Double]( bow.length) // frequency
      idf   = Array.ofDim[Double]( bow.length) // inverse frequency
      tfidf = Array.ofDim[Double]( train_words.length, bow.length) // tf * idf
      // calc tf
      for( (i,index) <- train_words.zipWithIndex){
        for( j <- i){
          bow.collect{case s if s==j => tmp_tf( index )( bow.indexOf(s) ) += 1} 
        }
      }
      // L1 normalization of tf
      for( (i,index) <- tmp_tf.zipWithIndex ){
        var sum:Double = i.sum; tf( index ) = i.map(x => x/sum) 
      }
      // calc df and idf
      for( i <- tf ){
        i.zipWithIndex.collect{ case (s, index) if s > 0.0 => df( index ) += 1 } 
      };idf = df.map(x => log10 ( train_words.length.toDouble / x ) )
      // calc tfidf
      for( (i,index1) <- tf.zipWithIndex){
        tfidf( index1 ) = i.zipWithIndex.map{ case (x, index2) => x * idf(index2) } 
      }
    }
    
    // compressed or not
    def save_traindata(dirname: Array[String]) {
      //
      val train_bow_filename : String =  "Train_bow.txt"    
      val train_idf_filename : String =  "Train_idf.txt"    
      val train_tfidf_filename : String =  "Train_tfidf.txt"    
      val output_train_bow =  new PrintWriter(train_bow_filename)
      val output_train_idf =  new PrintWriter(train_idf_filename)
      val output_train_tfidf =new PrintWriter(train_tfidf_filename)
      // write bow
      var bow_String = bow.mkString(",") 
      output_train_bow.write(bow_String)
	  output_train_bow.close
      // write idf
      var idf_String = idf.mkString(",") 
      output_train_idf.write(idf_String)
	  output_train_idf.close
      // write tfidf ( and tf )  
      var init_tfidf_String = tfidf.length + "," + bow.length + "\n"
      output_train_tfidf.write(init_tfidf_String)
      for( (i,index) <- dirname.zipWithIndex ){
        var tfidf_String = i + "," + tfidf(index).mkString(",") + "\n"
        output_train_tfidf.write(tfidf_String)
      }
	  output_train_tfidf.close
    }

    def get_cos(a: Array[Double], b: Array[Double]):Double={
      if(a.map( x => x*x ).sum == 0.0 || b.map( x => x*x ).sum == 0.0) return 0.0
      else return a.zip(b).map(x => x._1*x._2).sum / sqrt(a.map( x => x*x ).sum * b.map( x => x*x ).sum)
    
    }   
    
    def get_similarity() {
      var ev = Array.ofDim[Double]( tfidf.length  )
      var short_ev = Array.ofDim[Double]( tfidf.length  )
      for( (i,index) <- tfidf.zipWithIndex )
        ev(index) = get_cos( i, test_tfidf)     
      for( (i,index) <- short_tfidf.zipWithIndex )
        short_ev(index) = get_cos( i, short_test_tfidf)
      
      var sorted_labels  = labels.sortBy{ 
        (x) => -ev( labels.indexOf(x) ) // if labels is distinct 
      }
      var sorted_short_labels = labels.sortBy{ 
        (x) => -short_ev( labels.indexOf(x) ) // if labels is distinct 
      }

      var sorted_ev       = ev.sortBy{ x => -x }      
      var sorted_short_ev = short_ev.sortBy{ x => -x }      
      
      for( i <- 0 until ev.length){
        if( sorted_ev(i) != sorted_ev.last  )
          printf("%s,%f,",sorted_labels(i), sorted_ev(i))
        else
          printf("%s,%f",sorted_labels(i), sorted_ev(i))
      }
      printf("\n")
      for( i <- 0 until short_ev.length){
        if( sorted_short_ev(i) != sorted_short_ev.last )
          printf("%s,%f,",sorted_short_labels(i), sorted_short_ev(i))
        else
          printf("%s,%f", sorted_short_labels(i), sorted_short_ev(i))
      } 
      printf("\n")
    }


 


//-----------------------------------------

  }

}
