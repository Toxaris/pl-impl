package pli

import org.scalatest._

abstract class Spec extends FlatSpec with Matchers with Inside {
  implicit class WhenWord(val subject: String) {
    def when(context: String): String = subject + " (when " + context + ")"
  }
}
