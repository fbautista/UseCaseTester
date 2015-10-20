import scala.xml.XML
val xml = XML.loadFile("UseCaseDiagramOK.xmi")

val url = xml.getNamespace("xmi")

val pe = xml \\ "packagedElement" 

(pe) \\ "@{http://schema.omg.org/spec/XMI/2.1}id"

pe \\ ("@{"+url+"}id")

