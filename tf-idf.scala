import scala.io.Source
import scala.math._
import java.io.PrintWriter
import scala.sys.process.Process


object TFIDF {

  class Docs{
    
    var bof    = List.empty[String]
    var pre_bof    = List.empty[String]
    var train_words = List.empty[ List[String] ]
    var test_words = List.empty[ List[String] ]
   
    //var words = List.empty[ [String] ]
    var tf:    Array[Array[Double]] = null
    var ntf:    Array[Array[Double]] = null

    var df:    Array[Double] = null
    var idf:   Array[Double] = null
    var tfidf: Array[Array[Double]] = null 
    var short_tf:    Array[Array[Double]] = null
    var short_idf:   Array[Double] = null
    var short_tfidf: Array[Array[Double]] = null 
 

   /* http://www.scala-lang.org/docu/files/ScalaByExample.pdf */
    def sort(a: Array[Double], b:Array[Double]) {
      def swap(i: Int, j: Int) {
        var t = a(i); a(i) = a(j); a(j) = t
        var u = b(i); b(i) = b(j); b(j) = u
      }

      def sort1(l: Int, r: Int) {
        val pivot = a((l + r) / 2)
        var i = l;
        var j = r
        while(i <= j) {
          while(a(i) < pivot) i += 1
          while(a(j) > pivot) j -= 1
          if(i <= j) {
            swap(i, j)
            i += 1
            j -= 1
          }
        }
        if(l < j) sort1(l, j)
        if(j < r) sort1(i, r)
      }
      sort1(0, a.length - 1)
    }


    def read_mecabfile(filename: Array[String]) {

      var tmp_pre_bof    = List.empty[String]
      var tmp_train_words = List.empty[ List[String] ]
      var tmp_test_words = List.empty[ List[String] ] // List[String] ?

      for(i <- filename){
        val source = Source.fromFile(i)
        for( line <- source.getLines ) {
          if(i!=filename.last){
            //println(line.stripLineEnd.split(" ").toList.reverse)
            tmp_pre_bof =  line.stripLineEnd.split(" ").toList.reverse ::: tmp_pre_bof
            tmp_train_words   =  line.stripLineEnd.split(" ").toList :: tmp_train_words
          }else{
            tmp_test_words =  line.stripLineEnd.split(" ").toList :: tmp_test_words
          }
        } 
        source.close
      }
      
      train_words = tmp_train_words.reverse; 
      test_words  = tmp_test_words.reverse; 
      pre_bof = tmp_pre_bof.reverse;
    }

    def create_BoF() {
      bof = pre_bof.distinct
      //println(bof)
    }


    def create_TFIDF() {
      // bof -> dic & init vec, words( tf, idf) 
      tf    = Array.ofDim[Double]( train_words.length, bof.length)
      ntf    = Array.ofDim[Double]( train_words.length, bof.length)
      df    = Array.ofDim[Double]( bof.length)
      idf   = Array.ofDim[Double]( bof.length)
      tfidf = Array.ofDim[Double]( train_words.length, bof.length)
      
      // create tf 
      for( i <- train_words){
        for( j <- i){
          bof.collect{case s if s==j => tf( train_words.indexOf(i) )( bof.indexOf(s) ) += 1} 
        }
      }
      // normalization of tf
      for( i <- tf ){
        var sum:Double = i.sum
        ntf( tf.indexOf(i) ) = i.map(x => x/sum) 
      }
      // calc df and idf
      for( i <- tf ){
        i.zipWithIndex.collect{ case (s, index) if s > 0.0 => df( index ) += 1 } 
      }
      idf = df.map(x => log10 ( train_words.length.toDouble / x ) )
      // calc tfidf
      for( i <- tf){
        tfidf( tf.indexOf(i) ) = i.zipWithIndex.map{ case (x, index) => x * idf(index) } 
      }

    }
    
    def get_cos(a: Array[Double], b: Array[Double]):Double={
      if(a.map( x => x*x ).sum == 0.0 || b.map( x => x*x ).sum == 0.0) return 0.0
      else return a.zip(b).map(x => x._1*x._2).sum / sqrt(a.map( x => x*x ).sum * b.map( x => x*x ).sum)
    
    }   
    
    def get_similarity(args: Array[String]) {

      var ev = Array.ofDim[Double]( train_words.length  )
      var id = Array.ofDim[Double]( train_words.length  )
      var sev = Array.ofDim[Double]( train_words.length  )
      var sid = Array.ofDim[Double]( train_words.length  )
      var ssev = Array.ofDim[Double]( train_words.length  )
      var ssid = Array.ofDim[Double]( train_words.length  )

      for(i <- 0 until train_words.length  ){
	    ev(i) = get_cos( tfidf(i), tfidf( train_words.length-1))
        id(i) = i
        sev(i) = get_cos( short_tfidf(i), short_tfidf( train_words.length-1))
        sid(i) = i
      }

      var sorted_id  = id.zipWithIndex.sortBy{ case (x,index) => ev( index ) }
      var sorted_sid = sid.zipWithIndex.sortBy{ case (x,index) => sev( index ) }
      var sortd_ev   = ev.sorted
      var sorted_sev = sev.sorted

      for(i <- id.length-1 to 0 by -1){
        printf("%s %f ", args( (id(i).toInt) ), ev(i))
      }
      print(";")

      for(i <- sid.length-1 to 0 by -1){
        printf("%s %f ", args( (sid(i).toInt) ), sev(i))
      }
      println()

    }


 


//-----------------------------------------

  }

}
