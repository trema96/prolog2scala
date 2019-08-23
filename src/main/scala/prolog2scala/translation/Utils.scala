package prolog2scala.translation

object Utils {
  /**
    * @param name a prolog struct name
    * @return the name for the translated struct
    */
  def structNameToScala(name: String): String = name.substring(0,1).toUpperCase + name.substring(1)
  /**
    * @param name a prolog variable name
    * @return the name for the translated variable
    */
  def varNameToScala(name: String): String = name.substring(0,1).toLowerCase + name.substring(1)

  /**
    * Decides a name for a struct, based also on the existence of other structs with the same name but different arity
    * @param structId struct name and arity
    * @param allStructs all known structs
    * @return a name for the struct to be used in the translated program
    */
  def structToScalaName(structId: StructId, allStructs: Iterable[StructId]): String =
    structNameToScala(
      if (allStructs.count(_.name == structId.name) > 1) {
        structId.name + "_" + structId.arity
      } else {
        structId.name
      }
    )
}
