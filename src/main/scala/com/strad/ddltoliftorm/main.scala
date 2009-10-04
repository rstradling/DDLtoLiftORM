package com.strad.ddltoliftorm

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
    val instructions = parser.processFile(args(0))

    val writer : IDDLWriter = DDLWriterFactory.createIDDLWriter(ddlwriter)
    writer.writeTables(instructions, path)

  }
}