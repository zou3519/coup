package coup.core

import org.scalatest.{GivenWhenThen, Matchers}
import org.scalatest.Matchers
import org.scalatest.Matchers._

class CoupGameStateTest extends org.scalatest.FeatureSpec
  with GivenWhenThen
  with Matchers {

  def assertEquality(
    testGameState: CoupGameState,
    correctGameState: CoupGameState,
    testCourtDeck: Boolean = true,
    testDiscardPile: Boolean = true,
    testCoins: Boolean = true,
    testInfluences: Boolean = true,
    testCurrentPlay: Boolean = false,
    testPendingStages: Boolean = false,
    testAmbassadorDeck: Boolean = false
  ): Unit = {
    if (testCourtDeck)
      assert(testGameState.courtDeck == correctGameState.courtDeck)
    if (testDiscardPile)
      assert(testGameState.discardPile == correctGameState.discardPile)
    if (testCoins)
      assert(testGameState.coins == correctGameState.coins)
    if (testInfluences)
      assert(testGameState.influences == correctGameState.influences)
    if (testCurrentPlay)
      assert(testGameState.currentPlay == correctGameState.currentPlay)
    if (testPendingStages)
      assert(testGameState.pendingStages == correctGameState.pendingStages)
    if (testAmbassadorDeck)
      assert(testGameState.ambassadorDeck == correctGameState.ambassadorDeck)
  }

  def nextPlayer(player: PlayerT): PlayerT = (player + 1)%2

  feature("income") {
    scenario("first player incomes") {
      Given("a game state")
      val player = 0
      val newGameState = CoupGameState.init
      val oldGameState = newGameState.copy

      When("income happens")
      newGameState.applyAction(Income(player))

      Then("the player gets 1 coin")
      assert(newGameState.coins(player) == oldGameState.coins(player) + 1)

      And("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)

      And("everything else is unchanged")
      assert(
        newGameState.coins(nextPlayer(player)) == oldGameState.coins(nextPlayer(player)))
      assertEquality(newGameState, oldGameState, testCoins = false)
    }

    scenario("both players income") {
      Given("a game state")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.init
      val oldGameState = newGameState.copy

      When("both players income")
      newGameState.applyAction(Income(player))
      newGameState.applyAction(Income(otherPlayer))

      Then("both players get 1 coin")
      assert(newGameState.coins(player) == oldGameState.coins(player) + 1)
      assert(newGameState.coins(otherPlayer) == oldGameState.coins(otherPlayer) + 1)

      And("it is the first player's turn")
      assert(newGameState.pendingStages.head.player == player)
      assert(newGameState.currentPlay.isEmpty)

      And("everything else is unchanged")
      assertEquality(newGameState, oldGameState, testCoins = false)
    }
  }

  feature("coup") {
    scenario("coup") {
      Given("a game state where players have 7 coins each")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.init
      for (_ <- 1 to 5) {
        newGameState.applyAction(Income(player))
        newGameState.applyAction(Income(otherPlayer))
      }
      val oldGameState = newGameState.copy

      When("A coup happens [coup => lose influence]")
      newGameState.applyAction(Coup(player, otherPlayer))
      val lostInfluence = newGameState.influences(otherPlayer)(0)
      newGameState.applyAction(LoseInfluence(otherPlayer, lostInfluence))

      Then("the player pays 7 coins")
      assert((newGameState.coins(player) + 7) == oldGameState.coins(player))

      And("the other player loses a influence")
      newGameState.influences(otherPlayer) :+ lostInfluence should contain theSameElementsAs
        oldGameState.influences(otherPlayer)
      newGameState.discardPile(otherPlayer) should contain theSameElementsAs
        oldGameState.discardPile(otherPlayer) :+ lostInfluence

      And("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)
    }

    scenario("must coup") {
      Given("a game state")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.init

      When("Players have 10 coins each")
      for (_ <- 1 to 8) {
        newGameState.applyAction(Income(player))
        newGameState.applyAction(Income(otherPlayer))
      }

      Then("the player must coup")
      val legalActions = Rules.legalActions(newGameState.toPartialGameState(player))
      assert(legalActions.size == 1)
      assert(legalActions.head.isInstanceOf[Coup])
    }
  }

  feature("foreign aid") {
    scenario("foreign aid => do nothing") {
      Given("a game state")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.init
      val oldGameState = newGameState.copy

      When("foreign aid => do nothing")
      newGameState.applyAction(ForeignAid(player))
      newGameState.applyAction(DoNothing(otherPlayer))

      Then("the player gets 2 coins")
      assert(newGameState.coins(player) == oldGameState.coins(player) + 2)

      And("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)

      And("everything else is unchanged")
      assert(
        newGameState.coins(nextPlayer(player)) == oldGameState.coins(nextPlayer(player)))
      assertEquality(newGameState, oldGameState, testCoins = false)
    }

    scenario("foreign aid => block => do nothing") {
      Given("a game state")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.init
      val oldGameState = newGameState.copy

      When("foreign aid => block => do nothing")
      newGameState.applyAction(ForeignAid(player))
      newGameState.applyAction(Block(otherPlayer, player))
      newGameState.applyAction(DoNothing(player))

      Then("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)

      And("everything else is unchanged")
      assertEquality(newGameState, oldGameState)
    }

    scenario("foreign aid => block => challenge => prove influence") {
      // TODO need a way to specify a game state
    }

    scenario("foreign aid => block => challenge => lose influence") {
      // TODO
    }
  }


}
