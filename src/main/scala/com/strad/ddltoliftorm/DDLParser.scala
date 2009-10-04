
/**
 * Created by IntelliJ IDEA.
 * User: rstradling
 * Date: Aug 17, 2009
 * Time: 9:02:51 AM
 * To change this template use File | Settings | File Templates.
 */
package com.strad.ddltoliftorm

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

/**
 * Class handles the Alter table DDL syntax
 */
class Alter(val action : String, val actionOn : String, val AddOrDrop : String, val restOfInfo : String) {
  override def toString = "Action=" + action + " actionOn=" + actionOn + " AddorDrop=" + AddOrDrop +
          " restOfInfo=" + restOfInfo
}

/**
 * Parses the sql ddl data
 */
class DDLParser extends RegexParsers {
  // Holds the tables that have been read in thus far
  private val tableMap = new HashMap[String, Instruction]
  private def create: Parser[String] = "CREATE"
  private def table: Parser[String] = "TABLE"
  private def alter: Parser[String] = "ALTER"
  private def item: Parser[String] = """[a-zA-Z_]\w*""".r

  private def IndexCreate: Parser[Any] = "CREATE INDEX" ~ """.*""".r
  private def template : Parser[Any] = instr | IndexCreate
  private def addDrop : Parser[String] = "ADD" | "DROP"
  private def instr: Parser[Instruction] = create ~ table ~ item ~ ColumnInfo ~ ";" ^^
          {case create ~ table ~ itm ~ col ~ semi  => new Instruction(create, table, itm, col) }
  private def alterInstr : Parser[Alter] = alter ~ table ~ item ~ addDrop ~ rest ~ ";" ^^
          {case alter ~ table ~ tblname ~ addDrp ~ moreInfo ~ semi => new Alter(alter, tblname, addDrp, moreInfo) }
                        
  private def rest: Parser[String] = """.""".r
  private def ColumnInfo : Parser[List[Column]] = "("~> repsep(ColumnData,",") <~")" ^^
          {case data => for (param <- data) yield ((new Parameter(param)).DoMatch()) }
  private def ColumnData : Parser[String] = """(\w*\s*)*""".r
  private def CapitalizeFirstLetter(word : String) = {
    val firstLetter = word.substring(0,1)
    val remainder = word.substring(1)
    firstLetter.toUpperCase() + remainder
  }

  private def updateTable(s : Alter) : Unit = {
    val instruction = tableMap(s.actionOn)
    // Curently not supported
    assert(false)
    ()
  }
  /**
   * Matches the input line to the parser above
   * <p>
   * Currently does nothing unless it is a CREATE TABLE construct. IGNORES Create INDEX construct, and errors
   * on anything else
   */
  private def DoMatch(line : String, lineNum : Int) = {
    parse(template, line) match {
      case Success(item, _) => item match {
                                 case s : Instruction => tableMap += s.item -> s
                                 case s : Alter => updateTable(s)
                                 case _  => () // Do nothing
                              }
      case Failure(msg, _) => val errorMsg = "Error " + msg + " at line number " + lineNum
                              throw new RuntimeException(errorMsg)
      case Error(msg, _) => val errorMsg = "Error " + msg + " at line number " + lineNum
                              throw new RuntimeException(errorMsg)
    }
  }

  /**
   * processes the statements passed in as DDL
   * @param statements List of statements
   * @return List of instructions for the given statements
   */
  def processStatements(statements : List[String]) : List[Instruction] = {
    var lineNum = 0
    for (line <- statements) {
      DoMatch(line, lineNum)
      println("Processing line " + lineNum)
      lineNum = lineNum + 1
    }
    tableMap.map(_._2).toList

  }

  /**
   * Will process the given file for DDL statements
   * @param file The filename to open
   */
  def processFile(file : String) : List[Instruction] = {
    processStatements(Source.fromFile(file).getLines.toList)
  }
}

