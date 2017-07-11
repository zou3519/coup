package coup.state

object Character {
  sealed trait EnumVal
  case object Duke        extends EnumVal
  case object Captain     extends EnumVal
  case object Assassin    extends EnumVal
  case object Contessa    extends EnumVal
  case object Ambassador  extends EnumVal
  val characters = Seq(Duke, Captain, Assassin, Contessa, Ambassador)
}
