
/**
 * Created by IntelliJ IDEA.
 * User: rstradling
 * Date: Aug 17, 2009
 * Time: 9:02:51 AM
 * To change this template use File | Settings | File Templates.
 */
import collection.mutable.HashMap
import java.io._
import scala.util.parsing.combinator._
import scala.io.Source
import syntactical.StandardTokenParsers

/**
 * This class contains the column information for the table to create
 */
class Column(val name : String, val typ : String, val keyType : Option[String] ) {
  override def toString = "Column== Name=" + name + " type=" + typ + " keyType=" + keyType
}

/**
 * This class contains the parameters for the SQL DDL statements
 */
class Parameter(val name : String ) extends RegexParsers {
  def dataName: Parser[String] = """[a-zA-Z_]\w*""".r
  def dataType: Parser[String] = "INTEGER" | "INT" | "BIGINT" | "BIT" | "DECIMAL" | "INTEGER" | "MONEY" | "NUMERIC" |
                                   "SMALLINT" |
                                  "SMALLMONEY" | "TINYINT" | "FLOAT" | "DATE" | "DATETIME2" | "DATETIME" | "REAL" |
                                  "TIME" | "CHAR" | "TEXT" | "VARCHAR" | "NCHAR" | "NTEXT" | "NVARCHAR" | "BINARY" |
                                  "IMAGE" | "VARBINARY"      
  def Null : Parser[String] = "NULL" | "NOT NULL"
  def newKeyType : Parser[String] = "PRIMARY KEY" //| "UNIQUE"
  def parameters : Parser[Column] = dataName ~ dataType ~ opt(newKeyType) ^^
          {case nme ~ dt ~ keytyp =>  //println ("values = " + keytyp)
                                   var newName = nme.replace("type", "typeOf")
                                   new Column(newName, dt, keytyp)}

  /**
   * Returns a column instance if there was a successful parse
   */
  def DoMatch() = {
       //println ("name = " + name)
       parse(parameters, name) match {
        case Success(item, _) => item
        case Failure(msg, _) => throw new RuntimeException(msg)
        case Error(msg, _) => throw new RuntimeException(msg)
       }
  }
}

/**
 * The class that represents the CREATE TABLE
 */
class Instruction(val action : String, val actionOn : String, val item : String, val columns : List[Column]) {
  override def toString = "Action=" + action + " actionOn=" + actionOn + " item=" + item + " columns=" + columns +
                           " Columns Count= " + columns.size


}

class Alter(val action : String, val actionOn : String, val AddOrDrop : String, val restOfInfo : String) {
  override def toString = "Action=" + action + " actionOn=" + actionOn + " AddorDrop=" + AddOrDrop +
          " restOfInfo=" + restOfInfo
}

/**
 * Parses the sql ddl data
 */
class DDLParser extends RegexParsers {
  def create: Parser[String] = "CREATE"
  def table: Parser[String] = "TABLE"
  def alter: Parser[String] = "ALTER"
  def item: Parser[String] = """[a-zA-Z_]\w*""".r

  def IndexCreate: Parser[Any] = "CREATE INDEX" ~ """.*""".r
  def template : Parser[Any] = instr | IndexCreate
  def addDrop : Parser[String] = "ADD" | "DROP"
  def instr: Parser[Instruction] = create ~ table ~ item ~ ColumnInfo ~ ";" ^^
          {case create ~ table ~ itm ~ col ~ semi  => new Instruction(create, table, itm, col) }
  def alterInstr : Parser[Alter] = alter ~ table ~ item ~ addDrop ~ rest ~ ";" ^^
          {case alter ~ table ~ tblname ~ addDrp ~ moreInfo ~ semi => new Alter(alter, tblname, addDrp, moreInfo) }
                        
  def rest: Parser[String] = """.""".r
  def ColumnInfo : Parser[List[Column]] = "("~> repsep(ColumnData,",") <~")" ^^
          {case data => for (param <- data) yield ((new Parameter(param)).DoMatch()) }
  def ColumnData : Parser[String] = """(\w*\s*)*""".r
  def CapitalizeFirstLetter(word : String) = {
    val firstLetter = word.substring(0,1)
    val remainder = word.substring(1)
    firstLetter.toUpperCase() + remainder
  }

  def updateTable(s : Alter) : Unit = {
    val instruction = TableMap.tableMap(s.actionOn)
    ()
  }
  /**
   * Matches the input line to the parser above
   * <p>
   * Currently does nothing unless it is a CREATE TABLE construct. IGNORES Create INDEX construct, and errors
   * on anything else
   */
  def DoMatch(line : String, lineNum : Int) = {
    parse(template, line) match {
      case Success(item, _) => item match {
                                 case s : Instruction => TableMap.tableMap += s.item -> s
                                 case s : Alter => updateTable(s)
                                 case _  => () // Do nothing
                              }
      case Failure(msg, _) => val errorMsg = "Error " + msg + " at line number " + lineNum
                              throw new RuntimeException(errorMsg)
      case Error(msg, _) => val errorMsg = "Error " + msg + " at line number " + lineNum
                              throw new RuntimeException(errorMsg)
    }
  }
}

object TableMap {
  val tableMap = new HashMap[String, Instruction]
}


