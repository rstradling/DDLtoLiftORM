
/**
 * Created by IntelliJ IDEA.
 * User: rstradling
 * Date: Oct 4, 2009
 * Time: 11:27:55 AM
 * To change this template use File | Settings | File Templates.
 */
package com.strad.ddltoliftorm

/**
 * This is the factory class for creating DDLWriters.
 * Right now it only handles the string passed in LiftORM
 */
object DDLWriterFactory {
  def createIDDLWriter(name : String) : IDDLWriter = {
    name match {
      case "LiftORM" => new LiftORMWriter()
      case _ => error("Tried to use a IDDLwriter " + name + " that is not valid")
    }
  }
}