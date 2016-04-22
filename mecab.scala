import scala.sys.process.Process
import scala.io.Source
import scala.sys.process.Process
import java.io.PrintWriter

object MECAB {

  def create_file_for_mecab( foldername: String,
                            doc_List: List[String]): String={

    // 学習のため全てのファイルをcatで１つにまとめる
    // テストデータ１個の場合にも対応可能
    val filename_for_mecab: String =  foldername + "-to-mecab.txt" 
    val out_for_mecab = new PrintWriter(filename_for_mecab)
    for(j <- doc_List){
      var command_cat: String =null
      // テストデータはカレントディレクトリにおけば"/"を含まなくても良い
      if( j.contains("/") ) {
        command_cat = "cat" + " " + j
      } else {
        command_cat = "cat" + " " + foldername + "/" + j
      }    
      var docs =  Process(command_cat).lineStream.toList; 
      var docs_String = docs.mkString("") ;
      var erase_parenthesis1 = 
        docs_String.replaceAllLiterally("(", "（") //半角カッコはダメ
      var erase_parenthesis2= 
        erase_parenthesis1.replaceAllLiterally(")", "）") //半角カッコはダメ
      out_for_mecab.write( erase_parenthesis2 + "。")
    }
	out_for_mecab.close

    filename_for_mecab

  }

  def create_wakati_txt( foldername: String, 
                        input_for_mecab: String): String = {
    
    // mecabを読んでTF-IDFを作成するためのwakatiファイルを作成
    // 名詞、動詞(、形容詞)を抽出し、その基本形を登録
    // 記号やあまり意味のない単語もこの段階で消しておく
    // -b 300000 is magic parameterrrrrrrrrrrrrrrrrrrr
    val command_mecab: String = "mecab -b 3000000 " + input_for_mecab
    var result_mecab = Process(command_mecab).lineStream.toList;
    var tmp_wakati_txt = List.empty[String] 
    for( line <- result_mecab ) {
      val s = line.replaceAllLiterally("\t", ",")  
      val ss = s.split(',')
      // analyze
      if(ss.length>1){
        if( ( ss(1) == "動詞" || ss(1) =="名詞" || ss(1) == "形容詞" ) && 
           ( ss(0) != "(" && ss(0)!=")" && ss(0) != "." && 
             ss(0) != "　" && ss(0) !="," && 
             ss(2) != "数" && ss(0) != "年" && 
            ss(0) != "月" &&  ss(0) != "日" && 
            ss(0) != ":" && ss(0) != ";" && 
             ss(0) != "@"  && ss(0) != "-" && 
            ss(0) != "/" && ss(0) != "%" && 
            ss(0) != "!"  && ss(0) != "?") ) {  /*名詞でも欲しくない奴*/ 
               if(ss(7)=="*") tmp_wakati_txt =  (" " + ss(0).trim()) :: tmp_wakati_txt
               else  tmp_wakati_txt = ( " " + ss(7).trim()) :: tmp_wakati_txt
             }
      }
    }
    
    var output_wakati_txt = (tmp_wakati_txt.reverse).mkString("")
    val wakati_filename : String =  foldername + "-wakati.txt"    
    val out_wakati = new PrintWriter(wakati_filename)
    out_wakati.write( output_wakati_txt.trim )
    out_wakati.close
    //inputs = inputs :+ wakati_filename
    return wakati_filename

  }


  def create_wakati( args: Array[String]) :List[String] ={

    var filename_for_tfidf    = List.empty[String]

    for(i <- args){
      // lsでフォルダ内のファイル名を全て取得
      val command_ls: String =  "ls" + " " + i 
      var doc_List = Process(command_ls).lineStream.toList
      
      // mecabに投げるためのファイルを作成
      val input_for_mecab: String = create_file_for_mecab(i,doc_List)

      // call mecab and create wakati-txt
      filename_for_tfidf = create_wakati_txt(i,input_for_mecab) :: filename_for_tfidf
    }
    return filename_for_tfidf
  }
 
}




