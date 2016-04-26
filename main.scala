import MECAB._
import TFIDF._

object mynpl { 
  
  def main(args: Array[String]){
    
    if( args(0) == "train" ){
      //集めたデータフォルダ名の列
      println( "Creating: train-wakati txt" )
      println( "Load directory/File" )
      var train_dir = args.tail 
      for(i<-train_dir)println(i)
      
      var files_for_tfidf =
        create_wakati(train_dir)
      println( "Completed: train-wakati txt" )
      
      for(i<-files_for_tfidf.reverse)println(i)
      var docs= new Docs
      docs.train_mecabfile( (files_for_tfidf.reverse).toArray)
      docs.create_bow()
      docs.create_tfidf()
      docs.save_traindata(train_dir)

    }else if( args(0) == "test" ){
      if( args.length==6 ){
        //trainデータ名の列
        val train_bow_filename   = args(1) 
        val train_idf_filename   = args(2) 
        val train_tfidf_filename = args(3)
        var test_filename = List.empty[String];
        test_filename = args(4) :: test_filename 
        for(i<-args)println(i)
        println()
        val threshold:Int = args(5).toInt
        
        var files_for_tfidf =
          create_wakati(test_filename.toArray)
        println( "Completed: test-wakati txt" )
        
        for(i<-files_for_tfidf.reverse)println(i)
        println()
        
        var docs = new Docs
        docs.read_traindata( train_bow_filename,
                            train_idf_filename,
                            train_tfidf_filename,
                            threshold ) // delete top-N idf 
        docs.create_tfidf_for_test( files_for_tfidf(0) )
        docs.get_similarity()
      }else{
        println("test: run test train_bow.txt train_idf.txt train_tfidf.txt test.txt N(delete top-N element for dimension reduction)")
      }
      

    }else{
      println("args(0) = trsain or test")
      println("train: run train train_dirname1 2 3 4 ...")
      println("test: run test train_bow.txt train_idf.txt train_tfidf.txt test.txt")
    }
//    docs.get_similarity(args)
    

  }
}
