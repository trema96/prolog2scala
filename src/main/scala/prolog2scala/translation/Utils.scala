package prolog2scala.translation

object Utils {
  def structNameToScala(name: String): String = name.substring(0,1).toUpperCase + name.substring(1)
  def varNameToScala(name: String): String = name.substring(0,1).toLowerCase + name.substring(1)
  def structToScalaName(structId: StructId, allStructs: Iterable[StructId]): String =
    structNameToScala(
      if (allStructs.count(_.name == structId.name) > 1) {
        structId.name + "_" + structId.arity
      } else {
        structId.name
      }
    )
}
