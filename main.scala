import MECAB._
import TFIDF._

object mynpl { 
  
  def main(args: Array[String]){
    
    //集めたデータが打ち込まれてるフォルダ名の列
    println( "Creating: wakati txt" )
    println( "Load directory" )
    for(i<-args)println(i)
    var files_for_tfidf =
      create_wakati(args)
    println( "Completed: wakati txt" )
    
    for(i<-files_for_tfidf.reverse)println(i)
    var docs= new Docs
    docs.read_mecabfile( (files_for_tfidf.reverse).toArray)
    docs.create_BoF()
    docs.create_TFIDF()
//    docs.get_similarity(args)
    

  }
}
