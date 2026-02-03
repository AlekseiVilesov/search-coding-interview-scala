package albums.challenge.models

case class Facet(value: String, count: Integer)

final case class PriceBucket(min: Float, maxExclusive: Option[Float]) {
  def label: String =
    maxExclusive match {
      case Some(max) => f"${min.toInt} - ${max.toInt}"
      case None      => f"${min.toInt}+"
    }

  def contains(p: Float): Boolean =
    maxExclusive match {
      case Some(max) => p >= min && p < max
      case None      => p >= min
    }
}
