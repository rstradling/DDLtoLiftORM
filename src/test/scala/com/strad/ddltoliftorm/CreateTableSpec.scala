package com.strad.ddltoliftorm

import org.scalatest.junit.{JUnitRunner, JUnit3Suite}
import org.scalatest.matchers.{MustMatchers}
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
class CreateTableSpec extends Spec with MustMatchers {
  describe("A DDL Parser") {
    it("must be able to parse CREATE TABLE and create the correct columns") {
      val statement = List("CREATE TABLE attributes (id INTEGER PRIMARY KEY, name TEXT, min NUMERIC, max NUMERIC, category INTEGER);")
      val parser = new DDLParser

      val tableList = parser.processStatements(statement)
      // Make sure we have an entry
      tableList.size must equal (1)
      // Make sure the entry's name is == attributes
      val entry : Option[Instruction] = tableList.find(_.item == "attributes")
      entry.isDefined must equal (true)
      val instruction : Instruction = entry.get
      instruction.action must equal ("CREATE")
      instruction.actionOn must equal ("TABLE")

      // Verify the columns exist
      instruction.columns.size must equal (5)
      val idOpt = instruction.columns.find(_.name == "id")
      idOpt must be ('defined)
      val nameOpt = instruction.columns.find(_.name == "name")
      nameOpt must be ('defined)
      val minOpt = instruction.columns.find(_.name == "min")
      minOpt must be ('defined)
      val maxOpt = instruction.columns.find(_.name == "max")
      maxOpt must be ('defined)
      val categoryOpt = instruction.columns.find(_.name == "category")
      categoryOpt must be ('defined)

      // Verify the column values
      val id : Column = idOpt.get
      id.keyType must equal (Some("PRIMARY KEY"))
      id.name must equal ("id")
      id.typ must equal("INTEGER")

      val name : Column = nameOpt.get
      name.keyType must equal (None)
      name.name must equal ("name")
      name.typ must equal ("TEXT")

      val min : Column = minOpt.get
      min.keyType must equal (None)
      min.name must equal ("min")
      min.typ must equal ("NUMERIC")

      val max : Column = maxOpt.get
      max.keyType must equal (None)
      max.name must equal ("max")
      max.typ must equal ("NUMERIC")

      val category : Column = categoryOpt.get
      category.keyType must equal (None)
      category.name must equal ("category")
      category.typ must equal ("INTEGER")
    }
    it("must error on trying to parse this CREATE TABLE statement") {
      val statement = List("CREATE TABLE")
      val parser = new DDLParser
      intercept[java.lang.RuntimeException] {
        parser.processStatements(statement)
      }
      ()
    }


  }
  
}


