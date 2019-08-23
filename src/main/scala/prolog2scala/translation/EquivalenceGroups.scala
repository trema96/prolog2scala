package prolog2scala.translation

/**
  * Represent groups of equivalences between different elements
  * @tparam A the type of the elements
  */
class EquivalenceGroups[A] private(private val groups: Set[Set[A]]) {
  /**
    * Add a new group of equivalent elements
    * @param equivalenceGroup an equivalent elements group
    * @return the updated equivalence groups
    */
  def +(equivalenceGroup: Set[A]): EquivalenceGroups[A] = {
    val (withOther, notWithOther) = groups.partition(_.exists(equivalenceGroup.contains))
    new EquivalenceGroups(notWithOther + (withOther.flatten ++ equivalenceGroup))
  }

  /**
    * Add many equivalence groups to this
    * @param other equivalence groups to add
    * @return the updated equivalence groups
    */
  def ++(other: EquivalenceGroups[A]): EquivalenceGroups[A] =
    other.groups.foldLeft(this)((eqGroups, newGroup) => eqGroups + newGroup)
  /**
    * Add many equivalence groups to this
    * @param others equivalence groups to add
    * @return the updated equivalence groups
    */
  def ++(others: Iterable[EquivalenceGroups[A]]): EquivalenceGroups[A] =
    others.fold(this)(_ ++ _)

  /**
    * Finds the representative of the equivalence group of the provided element
    * @param elem an element of which you want to find the equivalence group
    * @return the element's group representative
    */
  def find(elem: A): A = {
    groups.find(_.contains(elem)).get.head
  }

  override def toString: String = "EquivalenceGroups-" + groups.toString()
}

object EquivalenceGroups {
  def empty[A]: EquivalenceGroups[A] = new EquivalenceGroups(Set.empty)
}