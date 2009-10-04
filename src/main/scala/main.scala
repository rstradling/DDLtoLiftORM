import io.Source

/**
 * Created by IntelliJ IDEA.
 * User: rstradling
 * Date: Oct 4, 2009
 * Time: 11:43:20 AM
 * To change this template use File | Settings | File Templates.
 */

object main {
  def main(args: Array[String]) {
    val parser = new DDLParser
    var lineNum = 1;
    val path = args(1)
    val ddlwriter = args(2)
    val inputFile = args(0)

    println("Running the processor on file " + inputFile + " using DDLWriter " + ddlwriter + " and exporting to path " + path)
    for (line <- Source.fromFile(args(0)).getLines) {
      println("Processing line " + lineNum + " Info= " + parser.DoMatch(line, lineNum))
      lineNum = lineNum + 1
    }
    val instructions : List[Instruction] = TableMap.tableMap.map(_._2).toList
    val writer : IDDLWriter = DDLWriterFactory.createIDDLWriter(ddlwriter)
    writer.writeTables(instructions, path)
  }
}