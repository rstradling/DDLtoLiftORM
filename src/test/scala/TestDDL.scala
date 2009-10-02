import org.scalatest.junit.{JUnitRunner, JUnit3Suite}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers
import org.scalatest.Spec
import org.junit.runner.RunWith

/**
 * Created by IntelliJ IDEA.
 * User: rstradling
 * Date: Oct 2, 2009
 * Time: 12:03:30 AM
 * To change this template use File | Settings | File Templates.
 */
@RunWith(classOf[JUnitRunner])
class DDLTest extends Spec with ShouldMatchers {
  describe("A DDL Parser") {
    it("should be able to parse CREATE TABLE") {
      val x = "CREATE TABLE attributes (id INTEGER PRIMARY KEY, name TEXT, min NUMERIC, max NUMERIC, category INTEGER);"
      val parser = new DDLParser
      parser.DoMatch(x, 0)
      TableMap.tableMap.size should equal (1)
      for ((tableName : String, instruction : Instruction) <- TableMap.tableMap) {
         tableName should equal ("attributes")
      }
      

    }
  }
  
}


