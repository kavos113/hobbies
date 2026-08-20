import org.treesitter.TSParser
import org.treesitter.TSQuery
import org.treesitter.TSQueryCursor
import org.treesitter.TSQueryMatch
import org.treesitter.TreeSitterTypescript
import java.io.File

fun main() {
  val filename = "example.ts"

  val parser = TSParser()
  parser.language = TreeSitterTypescript()

  val code = File(filename).readText(
    Charsets.UTF_8)

  val tree = parser.parseString(null, code)
  val query = TSQuery(TreeSitterTypescript(), TS_QUERY)
  val cursor = TSQueryCursor()
  cursor.exec(query, tree.rootNode)
  val match = TSQueryMatch()
  val lines = code.split("\n")
  val formattedOutput = StringBuilder()
  var lastLine = -1
  while (cursor.nextMatch(match)) {
    val captures = match.captures.sortedBy { it.node.startPoint.row }
    captures.forEach { capture ->
      val node = capture.node
      val startLine = node.startPoint.row
      val endLine = node.endPoint.row

      if (lastLine != -1 && startLine > lastLine + 1) {
        formattedOutput.append("|----\n")
      }
      if (node.type.contains("identifier") && lines[startLine].isNotBlank()) {
        formattedOutput.append("|${lines[startLine]}\n")
      }

      lastLine = endLine
    }
  }

  if (formattedOutput.isNotEmpty()) {
    println(formattedOutput)
  }
}

const val TS_QUERY = """(function_signature
  name: (identifier) @name.definition.function) @definition.function

(method_signature
  name: (property_identifier) @name.definition.method) @definition.method

(abstract_method_signature
  name: (property_identifier) @name.definition.method) @definition.method

(abstract_class_declaration
  name: (type_identifier) @name.definition.class) @definition.class

(module
  name: (identifier) @name.definition.module) @definition.module

(interface_declaration
  name: (type_identifier) @name.definition.interface) @definition.interface

(function_declaration
  name: (identifier) @name.definition.function) @definition.function

(method_definition
  name: (property_identifier) @name.definition.method) @definition.method

(class_declaration
  name: (type_identifier) @name.definition.class) @definition.class

(type_alias_declaration
  name: (type_identifier) @name.definition.type) @definition.type

(enum_declaration
  name: (identifier) @name.definition.enum) @definition.enum"""

const val QUERY_2 = """
(export_statement 
  (class_declaration
    name: (type_identifier) @name.definition.class)) @definition.class

(class_declaration
  name: (type_identifier) @name.definition.class) @definition.class
"""

