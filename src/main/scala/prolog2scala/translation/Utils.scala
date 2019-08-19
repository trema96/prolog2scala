package prolog2scala.translation

object Utils {
  def structNameToScala(name: String): String = name.substring(0,1).toUpperCase + name.substring(1)
  def varNameToScala(name: String): String = name.substring(0,1).toLowerCase + name.substring(1)
}
