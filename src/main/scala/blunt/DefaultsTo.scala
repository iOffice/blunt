package blunt

trait DefaultsTo[Type, Default]

/*
 * This is some typelevel sorcery that allows an implicit
 * argument to have a default value, which is useful for
 * making all the operators default to equals.
 */
object DefaultsTo {
  implicit def defaultDefaultsTo[T]: DefaultsTo[T, T] = null
  implicit def fallback[T, D]: DefaultsTo[T, D] = null
}
