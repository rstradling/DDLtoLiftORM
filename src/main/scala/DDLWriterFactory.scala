
/**
 * Created by IntelliJ IDEA.
 * User: rstradling
 * Date: Oct 4, 2009
 * Time: 11:27:55 AM
 * To change this template use File | Settings | File Templates.
 */

object DDLWriterFactory {
  def createIDDLWriter(name : String) : IDDLWriter = {
    name match {
      case "LiftORM" => new LiftORMWriter()
      case _ => error("Tried to use a IDDLwriter " + name + " that is not valid")
    }
  }
}