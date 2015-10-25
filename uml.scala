import scala.xml._

val good = XML.loadFile("UseCaseDiagramOK.xmi")

val bad = XML.loadFile("UseCaseDiagramBad.xmi")

val ns = xml.getNamespace("xmi")

def packagedElementsByType(nodes: NodeSeq, elementType: String): NodeSeq = nodes \\ "packagedElement" filter (_ \@ ("{" + ns + "}type") == elementType)

def idAndNameFromNode(node: Node): (String, String) =
  (node \@ ("{" + ns + "}id"), node \@ "name")

def actors(nodes: NodeSeq) = packagedElementsByType(nodes, "uml:Actor") map idAndNameFromNode

def associations(nodes: NodeSeq) = {
  val as = packagedElementsByType(nodes, "uml:Association")
  val ownedEnds = as map (_ \\ "ownedEnd")
  ownedEnds map (o => (o(0) \@ "type", o(1) \@ "type"))
}

def unrelatedActors(nodes: NodeSeq) = {
  val as = actors(nodes)
  val bs = associations(nodes)
  as filter { case (aid, _) => ! (bs exists { case (bid, _) => aid == bid }) }
}  