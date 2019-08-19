package prolog2scala.translation

class EquivalenceGroups[A] private(private val groups: Set[Set[A]]) {
  def union(equivalenceGroup: Set[A]): EquivalenceGroups[A] = {
    val (withOther, notWithOther) = groups.partition(_.exists(equivalenceGroup.contains))
    new EquivalenceGroups(notWithOther + (withOther.flatten ++ equivalenceGroup))
  }
  def join(other: EquivalenceGroups[A]): EquivalenceGroups[A] =
    other.groups.foldLeft(this)((eqGroups, newGroup) => eqGroups union newGroup)
  def joinMany(others: Iterable[EquivalenceGroups[A]]): EquivalenceGroups[A] =
    others.fold(this)(_ join _)
  def find(elem: A): A = {
    groups.find(_.contains(elem)).get.head
  }

  override def toString: String = "EquivalenceGroups-" + groups.toString()
}

object EquivalenceGroups {
  def empty[A]: EquivalenceGroups[A] = new EquivalenceGroups(Set.empty)
}