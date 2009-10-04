
/**
 * Created by IntelliJ IDEA.
 * User: rstradling
 * Date: Oct 4, 2009
 * Time: 11:28:20 AM
 * To change this template use File | Settings | File Templates.
 */
package com.strad.ddltoliftorm

abstract class IDDLWriter {
  /**
   * @param tables A list of tables to process and write out
   * @param pathToWriteTo The path passed in from the command-line
   */
  def writeTables(tables : List[Instruction], pathToWriteTo : String) : Unit
}