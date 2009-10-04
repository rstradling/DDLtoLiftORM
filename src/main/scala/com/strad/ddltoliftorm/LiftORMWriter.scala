package com.strad.ddltoliftorm
import java.io.{FileOutputStream, PrintStream}

/**
 * Created by IntelliJ IDEA.
 * User: rstradling
 * Date: Oct 4, 2009
 * Time: 11:30:52 AM
 * To change this template use File | Settings | File Templates.
 */

/**
 * Implementation of the IDDLWriter to go from DDL to Lift's mapper ORM
 */
class LiftORMWriter extends IDDLWriter {
  override def writeTables(instructions : List[Instruction], pathToWriteTo : String) : Unit = {
    instructions.foreach(Writer.processItem(_, pathToWriteTo))
  }
}

object Writer {
  /**
   * Writes the column data to the print stream passed in representing
   * the class information
   *
   * @param columns The columns to process
   * @param p The print stream
   * @return  Unit
   */
  def WriteColumnDataToClass(columns : List[Column], p : PrintStream) = {
      def MapTypes(column : Column) = {
        column.typ match {
          case "INTEGER" => "MappedLong(this)"
          case "INT" => "MappedLong(this)"
          case "TEXT" => "MappedString(this, 100)"
          case "NUMERIC" => "MappedDouble(this)"
          case _ => throw new RuntimeException("Failure to parse name " + column.typ)
        }
      }
      for (column <- columns ) {
        column.keyType match {
          case None => p.println("  object " + column.name + " extends " + MapTypes(column))
          case Some(s) => () // Do nothing because we ignore the uniqe key
        }
      }
  }

  /**Writes the column data to the object class using the print stream passed in
   *
   * @param columns The columns to process
   * @param p The print stream to write to
   * @return Unit
   */
  def WriteColumnDataToObject(columns : List[Column], p : PrintStream) = {
    def WriteColumn(count : Int, name : String) = {
      if (count == 0) p.print(name)
      else p.print(", " + name)
    }
    p.print("  override def fieldOrder = List(")
    var counter = 0
    for (column <- columns) {
      column.keyType match {
        case None => WriteColumn(counter, column.name); counter=counter+1; ()
        case Some(s) => ()
      }
    }
    p.print(")")
    p.println("")
  }

  /**
   * Processes the instruction creating an Lift orm for it
   *
   * @param item The instruction to process
   * @return Unit
   */

  def processItem(item : Instruction, pathToWriteTo : String) = {
    // Create the file information
    val filename = pathToWriteTo + item.item + ".scala"
    val out = new FileOutputStream(filename)
    val p = new PrintStream(out)

    p.println("package com.eanc.careerlogic\n")
    p.println("import _root_.net.liftweb.mapper._\n")
    p.println("class " + item.item + " extends LongKeyedMapper[" + item.item + "] with IdPK {")
      p.println("  def getSingleton = " + item.item)
      WriteColumnDataToClass(item.columns, p)
    p.println("}")

    // Write out the object information
    p.println("object " + item.item + " extends " + item.item + " with LongKeyedMetaMapper[" + item.item + "] {")
      WriteColumnDataToObject(item.columns, p)
    p.println("}")

    p.close()

  }

}