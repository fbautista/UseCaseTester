import scala.xml._

val good = XML.loadFile("UseCaseDiagramOK.xmi")

val bad = XML.loadFile("UseCaseDiagramBad.xmi")

val two = XML.loadFile("two.xmi")

val ns = good.getNamespace("xmi")

def packagedElements(nodes: NodeSeq): NodeSeq = nodes \\ "packagedElement"

def packagedElementsByType(nodes: NodeSeq, elementType: String): NodeSeq = nodes \\ "packagedElement" filter (_ \@ ("{" + ns + "}type") == elementType)

def idAndNameFromNode(node: Node): (String, String) =
  (node \@ ("{" + ns + "}id"), node \@ "name")

def models(nodes: NodeSeq) = packagedElementsByType(nodes, "uml:Model") map idAndNameFromNode

def packages(nodes: NodeSeq) = packagedElementsByType(nodes, "uml:Package") map idAndNameFromNode

def useCases(nodes: NodeSeq) = packagedElementsByType(nodes, "uml:UseCase") map idAndNameFromNode

def actors(nodes: NodeSeq) = packagedElementsByType(nodes, "uml:Actor") map idAndNameFromNode

def associations(nodes: NodeSeq) = {
  val as = packagedElementsByType(nodes, "uml:Association")
  val ownedEnds = as map (_ \\ "ownedEnd")
  ownedEnds map (o => (o(0) \@ "type", o(1) \@ "type"))
}

def extendss(nodes: NodeSeq) = {
  val pe = packagedElements(nodes).filter(_ \"extend"\@ ("{" + ns + "}type") == "uml:Extend")
  pe map (o => ( o \@ ("{" + ns + "}id"), o \ "extend" \@ "extendedCase"))
}


/*
def extendss(nodes: NodeSeq) = {
  val pe = packagedElements(nodes)
  //pe map (o => ( o \@ ("{" + ns + "}id"), o \ "extend" \@ "extendedCase"))
  for(as <-pe){
    for(ex <-as\"extend"){
      val typo1= as \@ ("{" + ns + "}id")
      val typo2= ex \@ "extendedCase"

      println(typo1+" "+typo2)
    }

  }
}
*/

def includes(nodes: NodeSeq) = {
  val pe = packagedElements(nodes).filter(_ \"include"\@ ("{" + ns + "}type") == "uml:Include")
  pe map (o => ( o \@ ("{" + ns + "}id"), o \ "include" \@ "addition"))
}

def in(nodes: NodeSeq) = {
  val pe = packagedElements(nodes)
  pe map (o => (o \ "packagedElement"\@ ("{" + ns + "}id"), o \@ ("{" + ns + "}id")))

}


def unrelatedActors(nodes: NodeSeq) = {
  val as = actors(nodes)
  val bs = associations(nodes)
  as filter { case (aid, _) => ! (bs exists { case (bid, _) => aid == bid }) }
}

//def actorWithinBorder(nodes: NodeSer) = {
//  val as = actors(nodes)

//}
