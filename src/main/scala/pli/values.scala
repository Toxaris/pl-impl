package pli

/** Values at runtime.
  *
  * The representation of values of various dynamic types is
  * implemented by case classes extending this trait, see “Known
  * Subclasses” below for a list.
  */
trait Value

/** Numbers at runtime. */
case class NumericValue(toInt: Int) extends Value
